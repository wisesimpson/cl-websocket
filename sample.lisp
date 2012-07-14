;;;; sample.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(defpackage #:cl-websocket-sample
  (:use #:cl)
  (:export
   #:echo-server
   #:repl-server
   #:websocket-server))

(in-package #:cl-websocket-sample)

(defun read-all (stream)
  "Reads all available characters from a byte stream, returning them as a string."
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte stream))
       do (write-char char s)
       while (listen stream))))

(defun echo (stream)
  "Echoes requests on the websocket back to the sender."
  (loop (cl-websocket:send-frame stream (cl-websocket:receive-frame stream))))

(defun repl (stream)
  "Provides a simple read-eval-print loop over a websocket stream."
  (loop (cl-websocket:send-frame stream 
				 (format nil "~a"
					 (eval 
					  (read-from-string (cl-websocket:receive-frame stream)))))))

(defun websocket-server (handler &key (host usocket:*wildcard-host*) (port 8080))
  "A simplistic, single-threaded websocket server using usocket. Just
pass it a handler function, point your HTML5 browser, and it does the
rest."
  (usocket:socket-server host port  
			 #'(lambda (stream)
			     (trivial-utf-8:write-utf-8-bytes (cl-websocket:server-handshake (read-all stream)) stream)
			     (force-output stream)
			     (funcall handler stream))
			 nil

			 :element-type '(unsigned-byte 8)))

;; N.B. You'd probably want a multi-threaded or event driven stack for
;; production work, but this is after all, just a sample.

(defun echo-server ( &key (host usocket:*wildcard-host*) (port 8080))
  (websocket-server #'echo))

(defun repl-server ( &key (host usocket:*wildcard-host*) (port 8080))
  (websocket-server #'repl))

