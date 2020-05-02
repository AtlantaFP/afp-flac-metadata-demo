(defpackage flac-metadata-demo
  (:use :cl :fmd :fmd-prim)
  (:local-nicknames (:a :alexandria)))
(in-package :flac-metadata-demo)

;; TODO: move functions into a read-primitives.lisp
	     
;;; FLAC parser
(defun read-flac-stream (stream &optional file-path)
  (with-buffer (:stream stream)
    (let ((flac-metadata (make-instance 'flac-metadata)))
      ;; parse stream into flac metadata
      flac-metadata)))

(defun read-flac-file (file-path)
  (with-open-file (strm file-path :element-type '(unsigned-byte 8))
    (read-flac-stream strm file-path)))



