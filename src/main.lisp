(in-package :flac-metadata-demo)

;;; FLAC parser
(defclass flac-metadata ()
  ((filepath :initarg :file-path :reader file-path)
   (parse-tree :initarg :parse-tree :accessor parse-tree)))

(defun read-flac-stream (stream &optional file-path)
  (with-buffer (:stream stream)
    (let ((flac-metadata (make-instance 'flac-metadata :file-path file-path)))
      ;; parse stream into flac metadata
      ;; each part of the flac stream will be its own class starting with
      ;; the stream.
      (setf (parse-tree flac-metadata) (parse-stream))
      )))

(defun read-flac-file (file-path)
  (with-open-file (strm file-path :element-type '(unsigned-byte 8))
    (read-flac-stream strm file-path)))



