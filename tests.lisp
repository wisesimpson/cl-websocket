;;;; tests.lisp

(in-package #:cl-websocket)

(ql:quickload "lisp-unit")

(use-package :lisp-unit)

(define-test compute-acceptance
  (assert-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (compute-acceptance "dGhlIHNhbXBsZSBub25jZQ==")))

(run-all-tests)
