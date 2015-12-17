(in-package #:clws)

(defun match-resource-line (buffer parser)
  (next-reader-state
   buffer
   (slot-value parser 'first-line-parsed-predicate)
   (alexandria:named-lambda first-line-callback (buf)
     (declare (ignore buf))
     (let* ((http (slot-value parser 'http-request))
            (http-method
              (fast-http:http-method http))
            (resource
              (fast-http:http-resource http))
            (http-major-version
              (fast-http:http-major-version http))
            (http-minor-version
              (fast-http:http-minor-version http)))
       ;; validate http method
       (unless (eq http-method :get)
         (log:debug "got bad request method? ~s" http-method)
         (return-from first-line-callback
           (invalid-header buffer)))

       ;; validate http version
       (unless (and (= http-major-version 1)
                    (= http-minor-version 1))
         (log:debug "got bad http version? ~a.~a" http-major-version http-minor-version)
         (return-from first-line-callback
           (invalid-header buffer)))

       ;; validate resource line
       (multiple-value-bind (scheme authority host port path query fragment) (quri:parse-uri resource)
         (declare (ignore authority host port fragment))
         ;;validate scheme
         (unless (or
                  (and scheme
                       (or (equal scheme "ws")
                           (equal scheme "wss")))
                  (and path
                       (char= (char path 0) #\/)))
           (log:debug "got bad scheme ~s" scheme)
           (return-from first-line-callback
             (invalid-header buffer)))
         (log:debug "got request line ~s ? ~s" path query)
         (setf (client-resource-name buffer) path)
         (setf (client-query-string buffer) query)))
     (match-headers buffer parser))))

(defun match-headers (buffer parser)
  (next-reader-state
   buffer
   (slot-value parser 'finish-predicate)
   (lambda (buf)
     (declare (ignore buf))
     ;; old code called with-buffer-as-stream which called %get-chunks. %get-chunks resets buffer chunks buffer
     (setf (chunks buffer) (make-instance 'chunk-buffer))
     (let* ((http (slot-value parser 'http-request))
            (headers (fast-http:http-headers http)))
       (setf (client-connection-headers buffer) (ia-hash-table:alist-ia-hash-table
                                                 (alexandria:hash-table-alist headers)))
       (dispatch-protocols buffer)))))

(defun header-value (name headers)
  (gethash name headers))

(defclass http-request-parser ()
  ((http-request :initarg :http-request)
   (parser :initarg :parser)
   (first-line-parsed :initform nil)
   (finished :initform nil)
   (first-line-parsed-predicate)
   (finish-predicate)))

(defun make-http-request-parser ()
  (let* ((http-request-parser (make-instance 'http-request-parser))
         (http-request (fast-http:make-http-request))
         (parser (fast-http:make-parser http-request
                                        :first-line-callback
                                        (lambda ()
                                          (setf (slot-value http-request-parser 'first-line-parsed) t))
                                        :finish-callback
                                        (lambda ()
                                          (setf (slot-value http-request-parser 'finished) t)))))
    (setf (slot-value http-request-parser 'http-request) http-request
          (slot-value http-request-parser 'parser) parser
          (slot-value http-request-parser 'first-line-parsed-predicate)
          (lambda (buffer start end)
            (declare (ignore start))
            (funcall parser buffer :start 0 :end end)
            (slot-value http-request-parser 'first-line-parsed)
            end)
          (slot-value http-request-parser 'finish-predicate)
          (lambda (buffer start end)
            (funcall parser buffer :start start :end end)
            (slot-value http-request-parser 'finished)
            end))
    http-request-parser))
