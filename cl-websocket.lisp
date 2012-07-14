;;;; cl-websocket.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(defpackage #:cl-websocket
  (:use #:cl)
  (:export
   #:send-frame
   #:receive-frame
   #:server-handshake))

(in-package #:cl-websocket)

(defvar +crlf+ (coerce '(#\return #\linefeed) 'string))

;; Define a protocol for sending and receiving WebSocket frames.

(defgeneric send-frame (stream data)
  (:documentation "Sends data to a websocket using the appropriate framing."))

(defgeneric receive-frame (stream)
  (:documentation "Receives a frame from a websocket and returns the de-framed data."))

;; .. and a default implementation for arbitrary streams.

(defun write-uint16 (int stream)
  ""
  (write-byte (div int 256) stream)
  (write-byte (truncate int 256) stream))


(defmethod send-frame (stream (message string))
    (write-byte #x81 stream) ;; Final fragment, text frame.

    (if (<= (length message) 125)
	(write-byte (length message) stream)
	(progn
	  (write-byte 126 stream)
	  (write-uint16 (length message) stream)))
	  
    (trivial-utf-8:write-utf-8-bytes message stream)
    (force-output stream))

(defmethod receive-frame ((stream t))
  (let* ((b0 (read-byte stream))
	 (b1 (read-byte stream))
	 (fin (> (logand b0 #x80) 0))
	 (opcode (logand b0 #x0F))
	 (mask (> (logand b1 #x80) 0))
	 (len (logand b1 #x7F))
	 (payload (make-array len :initial-element '(unsigned-byte 8))))

    ;; A server MUST close the connection upon receiving a frame with
    ;; the MASK bit set to 0.
    (unless mask
      (close stream)
      (return-from receive-frame nil))

    ;; Sorry only short messages currently supported
    (when (> len 125)
      (close stream)
      (return-from receive-frame nil))

    ;; TODO Support longer message types

    (setf mask (list (read-byte stream) 
		     (read-byte stream) 
		     (read-byte stream) 
		     (read-byte stream)))

    (read-sequence payload stream)

    (dotimes (i len)
      (setf (aref payload i) (logxor (aref payload i) (nth (mod i 4)  mask))))
    
    (if (eq opcode #x01) ;; string?
      (trivial-utf-8:utf-8-bytes-to-string payload)
      payload)))

;; TODO: consider implementing binary frames, connection-close, ping
;; and pong.

(defun parse-headers (client-handshake)
  "Parses the websocket request client handshake (which looks like
HTTP) and extracts the headers as an alist of header names and
values."
  (let ((headers (car (cl-ppcre:split (concatenate 'string +crlf+ +crlf+) client-handshake))))
    (loop for string in (cl-ppcre:split +crlf+ headers)
       collect (cl-ppcre:split #\Space string :limit 2))))

(defun compute-acceptance (nonce)
  "Concatenates the client nonce with a magic string as per RFC6455,
signs it with SHA1 and returns the resulting signature base64
encoded."
  (let ((string-to-sign (concatenate 'string nonce "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
    (cl-base64:usb8-array-to-base64-string
     (ironclad:digest-sequence
      :sha1
      (ironclad:ascii-string-to-byte-array string-to-sign)))))

(defun server-handshake (client-handshake)
  "Constructs a server handshake corresponding to the given client
handshake as per RFC6455."
  (let* ((headers (parse-headers client-handshake))
	 (nonce (second (assoc "Sec-WebSocket-Key:" headers :test #'string=))))

    ;; Ignore Sec-WebSocket-Version, but we only support 13 at the time of writing.
    ;; Ignore Sec-WebSocket-Extensions.
    ;; Ignore Sec-WebSocket-Protocol.

    (concatenate 'string
		 "HTTP/1.1 101 Switching Protocols" +crlf+
		 "Upgrade: websocket" +crlf+
		 "Connection: Upgrade" +crlf+
		 "Sec-WebSocket-Accept: " (compute-acceptance nonce) +crlf+
		 +crlf+)))

