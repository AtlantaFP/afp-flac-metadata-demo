(in-package :flac-metadata-demo)

(defclass metadata ()
  ((header :initarg :header :accessor metadata-header)
   (data :accessor metadata-block)))

(defclass metadata-header ()
  ((last :accessor last-metadata-block)
   (type :accessor block-type)
   (length :accessor block-length)))

(defun parse-metadata ()
  (let* ((header (make-instance 'metadata-header))
	 (metadata-block (make-instance 'metadata :header header)))
    (setf (last-metadata-block header) (read-bits 1)
	  (block-type header) (read-bits 7)
	  (block-length header) (read-uint-be 3))
    (list metadata-block)))
