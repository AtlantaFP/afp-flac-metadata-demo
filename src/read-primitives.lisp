(defpackage fmd-primitives-readers
  (:use :cl :fmd)
  (:local-nicknames (:a :alexandria))
  (:export
   #:read-bytes
   #:read-bits
   #:read-uint-be
   #:read-uint-le
   #:read-string)
  (:nicknames :fmd-prim))

(in-package :fmd-primitives-readers)
  
(defun read-bytes (count)
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (bitio:read-bytes (buffer-bits) octet-vector :bits-per-byte 8)
    octet-vector))

(defun read-bits (count)
  (bitio:read-bits (buffer-bits) count))

;; document function
(defun read-uint-be (byte-count)
  (bitio:read-integer (buffer-bits)
		      :byte-endian :be
		      :num-bytes byte-count
		      :bits-per-byte 8
		      :unsignedp t))

(defun read-uint-le (byte-count)
  (bitio:read-integer (buffer-bits)
		      :byte-endian :le
		      :num-bytes byte-count
		      :bits-per-byte 8
		      :unsignedp t))

(defun read-string (&key byte-count (encoding :ascii) null-terminated-p)
  (flet ((get-string-length (byte-count null-terminated-p)
	   (let* ((sequence (fast-io:input-buffer-vector (buffer-bytes)))
		  (max-length (or byte-count (length sequence)))
		  (start (fast-io:buffer-position (buffer-bytes)))
		  (end (min (length sequence) (+ start max-length)))
		  (index (if null-terminated-p
			     (position 0 sequence :start start :end end)
			     end)))
	     (- index start))))
    (let ((octet-vector (fast-io:make-octet-vector (get-string-length byte-count null-terminated-p))))
      (fast-io:fast-read-sequence octet-vector (buffer-bytes))
      (when null-terminated-p
	(fast-io:fast-read-byte (buffer-bytes)))
      (babel:octets-to-string octet-vector :encoding encoding))))
