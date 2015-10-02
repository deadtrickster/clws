(defpackage #:clws
  (:use #:cl #:iolib #:eventfd #:safe-queue)
  (:export
   ;; client
   #:client
   #:write-to-client-text
   #:write-to-client-binary
   #:write-to-clients-text
   #:write-to-clients-binary
   #:write-to-client-close
   #:client-host
   #:client-port
   #:client-resource-name
   #:client-query-string
   #:client-connection-headers
   #:client-websocket-version

   #:client-connection-rejected
   ;; resource
   #:ws-resource
   #:register-global-resource
   #:find-global-resource
   #:unregister-global-resource
   #:register-server-resource
   #:find-server-resource
   #:unregister-server-resource
   #:resource-received-text
   #:resource-received-binary
   #:resource-client-connected
   #:resource-client-disconnected
   #:run-resource-listener
   #:run-server-resources
   #:run-global-resources
   #:run-resource-listener-thread
   #:kill-resource-listener
   #:kill-all-global-resource-listeners
   #:kill-all-server-resource-listeners

   #:resource-accept-connection
   #:send-custom-message-to-resource
   #:send-custom-message-to-resource
   #:call-on-resource-thread
   ;; server
   #:server
   #:run-server
   #:run-server-thread
   #:stop-server
   #:*debug-on-server-errors*
   #:*debug-on-resource-errors*
   #:*protocol-76/00-support*
   #:*max-clients*
   #:*max-read-frame-size*
   #:origin-prefix
   #:any-origin
   #:origin-exact
   #:server-client-class))
   
(in-package :clws)

