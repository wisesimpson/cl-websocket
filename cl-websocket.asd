;;;; cl-websocket.asd
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(asdf:defsystem #:cl-websocket
  :version "0.0.2"
  :author "Rob Blackwell"
  :description "Yet another WebSocket library for Common Lisp."
  :serial t
  :depends-on (#:ironclad
               #:cl-base64
	       #:cl-ppcre
	       #:babel)

  :components ((:file "cl-websocket")))



