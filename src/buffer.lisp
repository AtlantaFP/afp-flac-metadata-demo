(defpackage :fmd.datastructures
  (:use :cl)
  (:local-nicknames (:a :alexandria))
  (:nicknames :fmd)
  (:export
   #:read-bit-sequence
   #:buffer
   #:buffer-bytes
   #:buffer-bits
   #:buffer-stream
   #:buffer-sequence
   #:with-buffer
   #:*buffer*))

(in-package :fmd.datastructures)

(defvar *buffer*)
(defun read-bit-sequence (bit-sequence buffer &key (start 0) end)
  (fast-io:fast-read-sequence bit-sequence buffer start end))

(defclass buffer ()
  ((bytes :initargs :bytes)
   (bits :initargs :bits)
   (stream :initargs :stream)
   (sequence :initargs :sequence)))

(defun buffer-bytes ()
  (slot-value *buffer* 'bytes))

(defun buffer-bits ()
  (slot-value *buffer* 'bits))

(defun buffer-stream ()
  (slot-value *buffer* 'stream))

(defun buffer-sequence ()
  (slot-value *buffer* 'sequence))

(defmacro with-buffer ((&key stream sequence) &body body)
  (a:with-gensyms (bytes bits)
    `(let* ((,bytes (fast-io:make-input-buffer :vector ,sequence
					       :stream ,stream))
	    (,bits (bitio:make-bitio ,bytes #'fast-io:fast-read-byte
				     #'read-bit-sequence))
	    (*buffer* (make-instance
		       'buffer
		       :bytes ,bytes
		       :bits ,bits
		       :stream (fast-io:input-buffer-stream ,bytes)
		       :sequence (fast-io:input-buffer-vector ,bytes))))
p       ,@body)))
