(in-package #:ws)

;; default buffer size for reading lines/frames
(defparameter *max-read-frame-size* 8192)
;; default max header size in octets (not used yet?)
(defparameter *max-header-size* 16384)
(defparameter *header-encoding* (babel:make-external-format :ascii
                                                            :eol-style :crlf))
;; max number of reads before reader gives up on assembling a line/frame
;; (temporary hack until fragments are stored more efficiently, to
;;  avoid wasting a bunch of space if we only get 1 octet per read or
;;  whatever)
;; currently allocating 2kio/read, so stores multiples of that
(defparameter *read-max-fragments* 8)

;; max number of frames that can be queued before the reader will start
;; throttling reads for clients using that queue
;; fixme: should this have a separate setting for when to reenable readers?
(defparameter *max-handler-read-backlog* 128)

(defparameter *policy-file* (make-domain-policy))
(defparameter *404-message* (babel:string-to-octets
                             "HTTP/1.1 404 Resource not found

"
                             :encoding :utf-8))

(defun lg (&rest args)
  (apply #'format t args))

(defun disable-readers-for-queue (queue)
  (loop for c in *clients*
     for q = (%client-queue queue)
     when (eq q queue)
     do (deactivate-read queue)))
(defun enable-readers-for-queue (queue)
  (loop for c in *clients*
     for q = (%client-queue queue)
     when (eq q queue)
     do (activate-read queue)))

(defun valid-resource-p (resource)
  ;; todo: see if there is a handler registered for the resource
  (when resource
    (gethash resource *resources*)))

(defun handle-connection-header (queue)
  (format t "parsing handshake: ~s~%"
          (sb-concurrency:list-mailbox-messages (queue-read-queue queue)))
  (let* ((resource nil)
         (headers nil)
         (resource-line (dequeue-read queue))
         (s1 (position #\space resource-line))
         (s2 (if s1 (position #\space resource-line :start (1+ s1)))))
    (format t "checking header...~%")
    (format t "s1,s2=~s ~s~%" s1 s2)
    (format t "GET: ~s =>~s~%" (subseq resource-line 0 s1)
            (string= "GET" (subseq resource-line 0 s1)))
    (format t "HTTP: ~s =>~s~%" (subseq resource-line s2)
            (string= " HTTP/1.1" (subseq resource-line s2)))
    (when (and s1 s2
               (string= "GET" (subseq resource-line 0 s1))
               (string= " HTTP/1.1" (subseq resource-line s2))
               (string= "Upgrade: WebSocket" (print (dequeue-read queue)))
               (string= "Connection: Upgrade" (print (dequeue-read queue))))
      (setf resource (subseq resource-line (1+ s1) s2)))

    (when resource
      ;; resources must be absolute path (start with /),
      ;; otherwise abort connection
      (unless (char= (aref resource 0) #\/)
        (return-from handle-connection-header
          (values :invalid-resource nil)))
      ;; if we don't recognize the resource, return 404
      (unless (valid-resource-p resource)
        (lg "unknown resource ~s~%" resource)
        (return-from handle-connection-header
          (values :404 nil)))
      ;; otherwise try to parse remaining headers
      (setf headers (make-hash-table :test 'equal))
      (loop for l = (dequeue-read queue)
         while l
         for c = (position #\: l)
         do
         ;; check for malformed headers
         ;; (spec draft allows either ignoring them, or aborting
         ;;  connection, so aborting for now)
           (unless c
             (lg "header line missing #\: \"~s\"~%" l)
             (return-from handle-connection-header
               (values :invalid-header nil)))
         ;; otherwise store the header into the hash
           (setf (gethash (subseq l 0 c) headers)
                 (subseq l (+ (if (and (< c (1- (length l)))
                                       (char= #\space (aref l (1+ c))))
                                  2 1)
                              c)))))
    (values (or resource :invalid-handshake) headers))
  )

;; some extra params for reader fsm, too lazy to add more to lambda-lists
(defvar *reader-fsm-client-data*)
(defparameter *reader-fsm*
  ;; mapping of state to lambda

  ;; - lambda handles a buffer, and returns new state + extra state
  ;;   info to pass to next call
  (alexandria:plist-hash-table
   (list
    ;; in order to support the .swf websocket emulation, we optionally
    ;; can send a policy file on the socket instead of making a ws
    ;; connection in that case, we read "<policy-file-request/>\0" and
    ;; respond with policy file followed by 0 octet and close the
    ;; socket
    :maybe-policy-file
    (lambda (buffer queue state)
      (declare (ignore queue state))
      (cond
        ((= (aref buffer 0) (char-code #\<))
         (values :policy-file (list :start 0)))
        ((= (aref buffer 0) (char-code #\G))
         (values :header (list :start 0)))
        (t
         (lg "bad header?, got ~s = ~s~%"
             buffer
             (babel:octets-to-string buffer :errorp nil))
         (values :abort nil))))

    ;; we might be getting a policy file request, so check for and
    ;; handle that case
    :policy-file
    (lambda (b queue state)
      (let* ((start (getf state :start))
             (p (position 0 b :start start))
             (junk nil))
        ;; fixme: optimize for the (usual) case where we get request as 1 pkt
        (store-partial-read queue b (list start p))
        (cond
          ((and p (string= (setf junk (extract-read-chunk-as-utf-8 queue))
                           "<policy-file-request/>"))
           (lg "got policy request = ~s~%" junk)
           ;; got a policy request, send response and close connection
           (enqueue-write queue *policy-file*)
           (enqueue-write queue :close)
           (values :close nil))
          (p
           (lg "got malformed policy request = ~s~%" junk)
           ;; got a whole 0 terminated chunk, but didn't match, kill connection
           (values :abort nil))
          ((> (queue-read-buffer-octets queue)
              (length "<policy-file-request/>" ))
           ;; got more octets than expected without a valid request,
           ;; kill connection
           (lg "got oversized policy request~%")
           (values :abort nil))
          ((> (length (queue-read-buffers queue)) *read-max-fragments*)
           ;; got too many tiny fragments, kill connection
           (lg "got overly fragmented policy request~%")
           (values :abort nil))
          (t
           (lg "got partial policy request?~%")
           ;; not enough octets to tell yet, keep reading...
           (values :policy-file nil)))))

    ;; reading header
    ;; (for now just accumulating lines until CRLFCRLF or too many
    ;;  bytes read probably should add a real parser at some point,
    ;;  and for policy-file as well)
    :header
    (lambda (b queue state)
      (let* ((start (getf state :start))
             (cr (position #x0d b :start start))
             (next (if (and cr (< (1+ cr) (length b))) (1+ cr))))
        (cond
          ((and cr (eql start cr))
           ;; got empty line, check for final LF and finish header
           (values :header-final-lf (if next (list :start next))))
          ((and cr)
           ;; got end of line, check for LF and extract a line
           (store-partial-read queue b (list start cr))
           (values :header-lf (if next (list :start next))))
          ((> (queue-read-buffer-octets queue)
              *max-header-size*)
           ;; header too big, kill connection...
           (lg "read too many octets without finishing header?~%")
           (values :abort nil))
          ((> (length (queue-read-buffers queue)) *read-max-fragments*)
           ;; got too many tiny fragments, kill connection
           (lg "got too many fragments reading header?~%")
           (values :abort nil))
          (t
           ;; not enough octets to tell yet, keep reading...
           (values :header nil)))))
    :header-lf
    (lambda (b queue state)
      (let* ((start (or (getf state :start) 0))
             (next (if (< (1+  start) (length b)) (1+ start))))
        (cond
          ((and (> (length b) start) (eql #x0a (aref b start)))
           ;; got CRLF pair, extract a string and add to queue,
           ;; then go back to reading header lines
           (enqueue-read queue (extract-read-chunk-as-utf-8 queue))
           (values :header (if next (list :start next))))
          (t ;; assuming 0 length packets won't happen for now...
           ;; no LF, kill connection
           (lg "got CR without LF?")
           (values :abort nil)))))
    :header-final-lf
    (lambda (b queue state)
      (let* ((start (or (getf state :start) 0))
             (next (if (< (1+ start) (length b)) (1+ start))))
        (cond
          ((and (> (length b) start) (eql #x0a (aref b start)))
           ;; got final CRLF pair, parse the header
           ;; then go to frame mode
           (multiple-value-bind (resource headers)
               (handle-connection-header queue)
             ;; fixme: probably should dispatch on type or something
             ;; so we can catch unexpected other symbols if
             ;; handle-connection-header is modified without matching
             ;; changes here
             (case resource
               (:404
                (enqueue-write queue *404-message*)
                (enqueue-write queue :close)
                (values :close nil))
               ((:invalid-handshake :invalid-header :invalid-resource)
                (lg "bad header ~s~%" resource)
                (values :abort nil))
               (t
                ;; todo: hook up handler
                ;; if header parsed OK, send handshake and start reading frames
                (let ((resource-handler (valid-resource-p resource)))
                  (format t "res=~s handler = ~s~%" resource resource-handler)
                  (multiple-value-bind (rqueue origin res-handler protocol)
                      (ws-accept-connection resource-handler resource
                                            headers
                                            *reader-fsm-client-data*)
                    ;; fixme: this isn't reliable... (need to swtch client-data to class or something soon anyway though)
                    ;; (not sure it is actually needed yet though)
                    ;(setf (getf *reader-fsm-client-data* :handler) res-handler)
                    (setf (queue-read-queue queue) rqueue)
                    (enqueue-write queue
                                   (make-handshake
                                    (or origin
                                        (gethash "Origin" headers)
                                        "http://127.0.0.1/")
                                    (format nil "~a~a" "ws://127.0.0.1:12345"
                                            resource)
                                    (or protocol
                                        (gethash "WebSocket-Protocol" headers)
                                        "test")))))
                (values :frame-00 (if next (list :start next)))
                )))
           #++(values :abort nil))
          (t ;; assuming 0 length packets won't happen for now...
           ;; no LF, kill connection
           (lg "got CR without LF on final header line?")
           (values :abort nil)))))


    ;; for now only handling 00.ff frames, not other text/binary frame types
    :frame-00
    (lambda (b queue state)
      (declare (ignore queue))
      (let* ((start (or (getf state :start) 0))
             (next (if (< start (1+ (length b))) (1+ start))))
        (cond
          ((and (> (length b) start) (eql #x00 (aref b start)))
           ;; got beginning of frame marker, look for end...
           (values :frame-ff (if next (list :start next))))
          (t
           ;; got empty packet (something broken?) or unsupported frame type
           ;; (or junk between frames)
           (lg "got bad frame marker, expected #x00? got ~s~%"
               (if (> (length b) start)
                   (aref b start)
                   "not enough octets"))
           (values :abort nil)))))
    :frame-ff
    (lambda (b queue state)
      (let* ((start (getf state :start))
             (ff (position #xff b :start start))
             (next (if (and ff (< (1+ ff) (length b))) (1+ ff))))
        (store-partial-read queue b (list start ff))
        (cond
          (ff
           ;; got end of frame marker, extract a frame, queue it, and
           ;; look for next frame
           (let ((f (extract-read-chunk-as-utf-8 queue)))
             (enqueue-read queue (list *reader-fsm-client-data* f))
             (lg "got frame, next=~s, ff=~s, len=~s~%  frame = ~s~%"
                 next ff (length b) f))
           (values :frame-00 (if next (list :start next))))
          ((> (queue-read-buffer-octets queue)
              *max-read-frame-size*)
           ;; frame too big, kill connection...
           (lg "read too many octets without finishing frame?")
           (values :abort nil))
          ((> (length (queue-read-buffers queue)) *read-max-fragments*)
           ;; got too many tiny fragments, kill connection
           (lg "got too many fragments without finishing frame?~%")
           (values :abort nil))
          (t
           ;; not enough octets to tell yet, keep reading...
           (values :frame-ff nil)))))

    :close
    (lambda (&rest r)
      (declare (ignore r))
      (error "reader kept reading on socket that should have been closed?"))
    :abort
    (lambda (&rest r)
      (declare (ignore r))
      (error "reader kept reading on socket that should have been aborted?")))))

(defun make-reader (socket name queue discon)
  (setf (queue-reader queue)
        (lambda (fd event exception)
          (declare (ignore fd event exception))
          (let* ((client-data (gethash name *clients*))
                 (octets (make-array 2048 :element-type '(unsigned-byte 8)
                                     :fill-pointer 2048))
                 (*reader-fsm-client-data* client-data))
            (handler-case
                (progn
                  (multiple-value-bind (_octets count)
                      ;; fixme: decide on good max read chunk size
                      (receive-from socket :buffer octets :end 2048)
                    (declare (ignore _octets))
                    (setf (fill-pointer octets) count)
                    (format t "(frame) read ~s octets...~%" count)
                    (when (zerop count)
                      (error 'end-of-file))
                    (format t "  ==  |~s|~%"
                            (babel:octets-to-string octets
                                                    :encoding :utf-8
                                                    :errorp nil))
                    (loop with next-state
                       with next-data
                       for state = (queue-read-state queue) then next-state
                       for data = nil then next-data ;(queue-read-state-data queue) then next-data
                       do
                       (format t "parsing packet in state ~s~%" state)
                       (setf (values next-state next-data)
                             (funcall (gethash state *reader-fsm*)
                                      octets queue data))
                       (format t "  -> state ~s~%" next-state)
                       (case next-state
                         ((:close :close-read)
                          (enqueue-read queue (list *reader-fsm-client-data* :eof))
                          (funcall discon :read t)
                          (loop-finish))
                         (:abort
                          (format t "aborting connection~%")
                          (funcall discon :abort t)
                          (loop-finish)))
                       (unless next-data
                         (loop-finish))
                       finally (setf (queue-read-state queue) next-state
                                        ;(queue-read-state-data queue) next-data
                                     ))))
              (end-of-file ()
                (enqueue-read queue (list *reader-fsm-client-data* :eof))
                (format t "closed connection ~s~%" name)
                ;; fixme: don't close write side of connection if we still have buffered data...
                (funcall discon :read t))
              ;; ... add error handlers
              )))))

