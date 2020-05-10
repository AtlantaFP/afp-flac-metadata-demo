(in-package :flac-metadata-demo)

(defvar *metadata-block*)

(defclass metadata-block ()
  ((header :initarg :header :accessor metadata-header)
   (data :accessor metadata-block-data)))

(defclass metadata-header ()
  ((last :accessor last-metadata-block)
   (type :accessor block-type)
   (length :accessor block-length)))

(defun metadata-block-type ()
  (case (block-type (metadata-header *metadata-block*))
    (0 :streaminfo)
    (1 :padding)
    (2 :application)
    (3 :seektable)
    (4 :vorbis-comment)
    (5 :cuesheet)
    (6 :picture)
    (127 :invalid)
    (t :reserved)))

(defun parse-metadata-block ()
  (let* ((header (make-instance 'metadata-header))
	 (*metadata-block* (make-instance 'metadata-block
					:header header)))
    (setf (last-metadata-block header) (read-bits 1)
	  (block-type header) (read-bits 7)
	  (block-length header) (read-uint-be 3)
	  (metadata-block-data *metadata-block*) (parse-metadata (metadata-block-type)))
    (list *metadata-block*)))

(defgeneric parse-metadata (blk-type)
  (:documentation "generic function to parse different types of flac metadata"))

(defmacro define-metadata-block-data ((blk-type) (&rest slots) &body body)
  (a:with-gensyms (type metadata)
    (let ((class-name (a:symbolicate '#:metadata- blk-type)))
    `(progn
       (defclass ,class-name ()
	 ,slots)
       (defmethod parse-metadata ((,type (eql ,(a:make-keyword blk-type))))
	 (let ((,metadata (make-instance ',class-name)))
	   (with-slots ,slots ,metadata
	     (with-buffer (:sequence (read-bytes (block-length (metadata-header *metadata-block*))))
	       ,@body))
	   ,metadata))))))

(define-metadata-block-data (streaminfo)
    (min-block-size
     max-block-size
     min-frame-size
     max-frame-size
     sample-rate
     channel-count
     bits-per-sample
     sample-count
     md5)
  (setf min-block-size (read-uint-be 2)
	max-block-size (read-uint-be 2)
	min-frame-size (read-uint-be 3)
	max-frame-size (read-uint-be 3)
	sample-rate (read-bits 20)
	channel-count (read-bits 3)
	bits-per-sample (read-bits 5)
	sample-count (read-bits 36)
	md5 (read-bytes 16))) 
