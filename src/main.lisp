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
      flac-metadata)))

(defun read-flac-file (file-path)
  (with-open-file (strm file-path :element-type '(unsigned-byte 8))
    (read-flac-stream strm file-path)))

(defvar *current-metadata*)

(defun open-flac-file ()
  (let ((filename
	  (ltk:get-open-file :filetypes '(("FLaC" "*.flac")("All Files" "*")))))
    (setf *current-metadata* (read-flac-file filename))
    filename))

(defun main ()
  (ltk:with-ltk ()
    (let* ((top-frame (make-instance 'ltk:frame))
	   (button-frame (make-instance 'ltk:frame :master top-frame))
	   (open-flac-file-button (make-instance 'ltk:button
						 :master button-frame
						 :text "Open Flac File"
						 :command (lambda ()
							    (ltk:wm-title ltk:*tk*
									  (format nil "FLaC Metadata Viewer: ~A"
										  (open-flac-file))))))
	   (exit-button (make-instance 'ltk:button
				       :master button-frame
				       :text "Exit Application"
				       :command (lambda ()
						  (return-from main nil))))
	   (details-pane (make-instance 'ltk:paned-window
					:master top-frame))
	   (stream-info-section-title (make-instance 'ltk:label
						     :master details-pane
						     :text "FLac File Stream Header Information"))
	   (bits-per-sample-label (make-instance 'ltk:label
						 :master details-pane
						 :text "bits per sample:"))
	   (bps-info (make-instance 'ltk:text
				    :master details-pane
				    :name "bits-per-sample"))
	   (min-block-size-label (make-instance 'ltk:label
						:master details-pane
						:text "minimum block size:"))
	   (max-block-size-label (make-instance 'ltk:label
						:master details-pane
						:text "maximum block size:")))
      (ltk:wm-title ltk:*tk* "Flac Metadata Viewer")
      (ltk:pack top-frame)
      (ltk:pack button-frame)
      (ltk:pack open-flac-file-button :side :left :padx 30 :pady 5)
      (ltk:pack exit-button :side :left :padx 10)
      (ltk:configure button-frame :borderwidth 3)
      (ltk:pack details-pane)
      (ltk:pack stream-info-section-title :anchor :e :side :left)
      (ltk:pack bits-per-sample-label :anchor :s :side :bottom)
      (ltk:pack min-block-size-label :anchor :s :side :bottom)
      (ltk:pack max-block-size-label :anchor :s :side :bottom))))
