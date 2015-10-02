(cl:in-package :cl-user)

(defpackage :clws-system
  (:use #:cl #:asdf))

(in-package :clws-system)

(defsystem :clws
  :depends-on ("safe-queue"
               "iolib"
               "fast-http"
               "quri"
               "eventfd"
               "ironclad"
               "chunga"     ; for o7 hanshake
               "cl-base64" ; for o7 hanshake
               "flexi-streams"
               "split-sequence"
               "ia-hash-table"
               "log4cl"
               "trivial-backtrace")
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "3b <https://github.com/3b>, Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :licence "MIT"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "config")
               (:file "buffer")
               (:file "protocol-common")
               (:file "protocol-00")
               (:file "protocol-7")
               (:file "protocol")
               (:file "http")
               (:file "client")
               (:file "server")
               (:file "resource"))
  :description "CLWS implement the WebSocket Protocol as described by
RFC6455[1] (as well as some older drafts implemented by recent
browsers [2][3][4][5]).  Only a WebSockets server implementation is
provided.

[1]http://tools.ietf.org/html/rfc6455
[2] http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
[3] http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08
[4] http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07
[5] http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-00")

