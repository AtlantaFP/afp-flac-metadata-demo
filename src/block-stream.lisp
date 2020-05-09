(defpackage flac-metadata-demo
  (:use :cl :fmd :fmd-prim :fmd-primitives-readers)
  (:local-nicknames (:a :alexandria)))
(in-package :flac-metadata-demo)

(defclass block-stream ()
  ((marker :accessor stream-marker)
   (streaminfo :accessor stream-info)
   (metadata-blocks :initform () :accessor metadata)
   (frames :accessor frames)))

(defun parse-magic-string ()
  (with-buffer (:sequence (read-bytes 4))
    (let ((marker (read-string)))
      (if (string= marker "fLaC")
	  marker
	  (error "invalid flac file passed~%")))))

(defun parse-stream ()
  (let ((flac-data (make-instance 'block-stream)))
    (setf (stream-marker flac-data) (parse-magic-string))
    (destructuring-bind (streaminfo &rest metadata-blocks) (parse-metadata)
      (setf (stream-info flac-data) streaminfo
	    (metadata flac-data) metadata-blocks
	    (frames flac-data) nil))
    flac-data))




