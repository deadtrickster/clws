(in-package #:clws)

(defparameter *server-busy-message* (string-to-shareable-octets
                                     "HTTP/1.1 503 service unavailable

"
                                     :encoding :utf-8))

(defclass server ()
  ((name :initform nil :accessor server-name :initarg :name)
   (addr :initform +ipv4-loopback+ :initarg :addr :accessor server-addr)
   (port :initform nil :initarg :port :accessor server-port)
   (event-base :initform (make-instance 'iolib:event-base) :accessor server-event-base :initarg :event-base)
   (clients :initform (make-hash-table) :reader server-clients
            :documentation "Hash of client objects to them
            selves (just used as a set for now).")
   (resources :initform (make-hash-table :test 'equal) :reader server-resources
              :documentation "Collection of resource which handled by this server")
   (shutting-down-p :initform nil
                    :accessor shutting-down-p
                    :documentation "If t server enters shutting-down mode.
Stop accepting new connections and all incoming messages. After last outgoing message is written server killed.")
   (control-fd :initform nil :reader server-control-fd)
   (control-mailbox :initform nil :reader server-control-mailbox)
   (execute-in-server-lambda :initform nil :reader execute-in-server-lambda
                             :documentation "lambda grabbed from flet in run-server -> execute-in-server-thread"))
  (:documentation "A WebSockets server listens on a socket for
connections and has a bunch of client instances that it controls."))

(defmethod initialize-instance :after ((server server) &rest initargs &key name &allow-other-keys)
  (setf (server-name server) (format nil "WS Server \"~a\", ~a:~a" name (server-addr server) (server-port server))))

(defgeneric server-client-count (server)
  (:documentation "Returns number the server's clients."))

(defgeneric server-list-clients (server)
  (:documentation "Returns a list of the server's clients."))

(defgeneric server-client-class (server)
  (:documentation "Returns class designator for this server clients. Class should inherit clws:client."))

(defmethod server-client-count ((server server))
  (hash-table-count (server-clients server)))

(defmethod server-list-clients ((server server))
  (loop :for v :being the hash-values :of (server-clients server)
        :collect v))

(defmethod server-client-class ((server server))
  'client)

(defun make-listener-handler (server socket)
  (lambda (fd event exception)
     (declare (ignore fd event exception))
     (if (shutting-down-p server)
         ;; stop accepting new connections first
         (iolib:remove-fd-handlers (server-event-base server) (iolib:socket-os-fd socket))
         (let* ((client-socket (accept-connection socket :wait t))
                (client (when client-socket
                          (make-instance (server-client-class server)
                                         :server server
                                         :%host (remote-host client-socket)
                                         :host (address-to-string
                                                (remote-host client-socket))
                                         :port (remote-port client-socket)
                                         :server-hook (slot-value server 'execute-in-server-lambda)
                                         :socket client-socket))))
           (when client
             (log:debug "got client connection from ~s ~s~%" (client-host client)
                        (client-port client))
             (log:debug "client count = ~s/~s~%" (server-client-count server) *max-clients*)
             (cond
               ((and *max-clients* (> (server-client-count server) *max-clients*))
                ;; too many clients, send a server busy response and close connection
                (log:info "Too many clients ~s:~s, sending busy response" (server-client-count server) *max-clients*)
                (client-disconnect client :read t)
                (client-enqueue-write client *server-busy-message*)
                (client-enqueue-write client :close))
               (t
                ;; otherwise handle normally
                (setf (gethash client (server-clients server)) client)
                (add-reader-to-client client))))))))

(defun run-server% (server)
  (let ((event-base (server-event-base server))
        (control-fd (server-control-fd server))
        (control-mailbox (server-control-mailbox server)))
    (unwind-protect
         (iolib:with-open-socket (socket :connect :passive
                                         :address-family :internet
                                         :type :stream
                                         :ipv6 nil
                                         ;;:external-format '(unsigned-byte 8)
                                         ;; bind and listen as well
                                         :local-host (server-addr server)
                                         :local-port (server-port server)
                                         :backlog 5
                                         :reuse-address t
                                         #++ :no-delay)
           (let ((true-handler (make-listener-handler
                                server
                                socket)))
             (iolib:set-io-handler event-base
                                   control-fd
                                   :read (lambda (fd e ex)
                                           (declare (ignorable fd e ex))
                                           (eventfd.read control-fd)
                                           (log:debug "Got lambda to execute on server thread")
                                           (loop for m = (dequeue control-mailbox)
                                                 while m
                                                 do (funcall m))))
             (iolib:set-io-handler event-base
                                   (iolib:socket-os-fd socket)
                                   :read
                                   (lambda (&rest rest)
                                      (log:debug "There is something to read on fd ~A" (first rest))
                                      (apply true-handler rest)))
             (handler-case
                 (event-dispatch event-base)
               ;; ... handle errors
               ))
           (loop :for v :in (server-list-clients server)
                 :do
                    (log:debug "cleanup up dropping client ~s" v)
                    (client-enqueue-read v (list v :dropped))
                    (client-disconnect v :abort t))
           (close event-base)
           (eventfd.close control-fd)))))

(defun setup-execute-in-server-lambda (server)
  (let* ((control-mailbox (make-queue :name (concatenate 'string (server-name server) " server-control")))
         (control-fd (eventfd.new 0))  ;; TODO: add error checking here
         (execute-in-server-thread
           (lambda (thunk)
              ;; hook for waking up the server and telling it to run
              ;; some code, for things like enabling writers when
              ;; there is new data to write
              (enqueue thunk control-mailbox)
              (log:debug "Notifying server thread")
              (if *debug-on-server-errors*
                  (eventfd.notify-1 control-fd)
                  (ignore-errors
                   (eventfd.notify-1 control-fd))))))
    (setf (slot-value server 'execute-in-server-lambda) execute-in-server-thread
          (slot-value server 'control-mailbox) control-mailbox
          (slot-value server 'control-fd) control-fd)))

(defun run-server (server)
  "Starts given SERVER and blocks until the server stopped.
Intended to run in a dedicated thread (the current one),
dubbed the Server Thread.

Establishes a socket listener in the current thread.  This thread
handles all incoming connections, and because of this fact is able to
handle far more concurrent connections than it would be able to if it
spawned off a new thread for each connection.  As such, most of the
processing is done on the Server Thread, though most user functions
are thread-safe.

"
  (log:info "CLWS ~a" (server-name server))
  (setup-execute-in-server-lambda server)
  (log:info "Starting server resources")
  (run-server-resources server)
  (log:info "Starting global resources")
  (run-global-resources)
  (log:info "Starting server")
  (run-server% server)
  server)


(defun run-server-thread (server)
  "Calls RUN-SERVER with given SERVER in separate thread.
Return SERVER"
  (bt:make-thread
   (lambda ()
      (run-server server))
   :name (server-name server))
  server)

(defun careless-stop (server)
  (log:info "~a: Killing resources~%" (server-name server))
  (kill-all-server-resource-listeners server)
  (log:info "~a: Stopping Server Thread~%" (server-name server))
  (funcall (execute-in-server-lambda server) (lambda () (sb-thread:abort-thread))))

(defun stop-new-messages (server)
  (log:info "~a: Stop accepting new connections" (server-name server))
  (funcall (execute-in-server-lambda server) (lambda ()
                                                (setf (shutting-down-p server) t)
                                                (with-hash-table-iterator (next-client (server-clients server))
                                                  (loop
                                                    (multiple-value-bind (next? key client) (next-client)
                                                      (declare (ignorable key))
                                                      (unless next?
                                                        (return))
                                                            ;disable read
                                                      (setf (client-read-closed client) t)
                                                      (client-disable-handler client :read t)))))))

(defun stop-exsiting-clients (server close-code)  
  (log:info "~a: Stop reading new incoming messages and enqueue close frame for all clients" (server-name server))
  (funcall (execute-in-server-lambda server) (lambda ()
                                                (with-hash-table-iterator (next-client (server-clients server))
                                                  (loop
                                                    (multiple-value-bind (next? key client) (next-client)
                                                      (declare (ignorable key))
                                                      (unless next?
                                                        (return))
                                                      ;;enqueue close
                                                      (write-to-client-close client :code close-code))))))
  (log:info "~a: Waiting until all clients closed" (server-name server))
  (loop
    (when (= 0 (hash-table-count (server-clients server)))
      (return))))

(defun stop-server (server &key soft stop-new-messages-cb close-exsiting-clients-cb (close-code 3502))
  (if soft
      (progn
        (stop-new-messages server)
        (when stop-new-messages-cb
          (funcall stop-new-messages-cb server))
        (stop-exsiting-clients server close-code)
        (when close-exsiting-clients-cb
          (funcall close-exsiting-clients-cb server))
        (careless-stop server))
      (careless-stop server)))
