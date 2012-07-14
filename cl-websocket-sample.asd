;;;; cl-websocket-sample.asd
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(asdf:defsystem #:cl-websocket-sample
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "Samples and demonstration of cl-websocket."
  :serial t
  :depends-on (#:cl-websocket
               #:usocket)

  :components ((:file "sample")))





