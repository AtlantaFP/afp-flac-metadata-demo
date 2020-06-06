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
    (unless (directory filename)
     (setf *current-metadata* (read-flac-file filename)))
    filename))

(defun main ()
  (ltk:with-ltk ()
    (let* ((button-frame (make-instance 'ltk:frame))
           (open-flac-file-button (make-instance 'ltk:button
                                                 :master button-frame
                                                 :text "Open Flac File"
                                                 :command (lambda ()
                                                            (let ((filename (open-flac-file)))
                                                              (when (probe-file filename)
                                                                (ltk:wm-title ltk:*tk*
                                                                              (format nil "FLaC Metadata Viewer: ~A"
                                                                                      filename)))))))
           (exit-button (make-instance 'ltk:button
                                       :master button-frame
                                       :text "Exit Application"
                                       :command (lambda ()
                                                  (return-from main nil))))
           (details-pane (make-instance 'ltk:frame))
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
           (min-block-size (make-instance 'ltk:text
                                          :master details-pane
                                          :name "min-block-size"))
           (max-block-size-label (make-instance 'ltk:label
                                                :master details-pane
                                                :text "maximum block size:"))
           (max-block-size (make-instance 'ltk:text
                                          :master details-pane
                                          :text "max-block-size")))
      (ltk:wm-title ltk:*tk* "Flac Metadata Viewer")
      (ltk:grid button-frame 0 0
                :rowspan 3
                :columnspan 3
                :sticky :nw
                )
      (ltk:grid open-flac-file-button 0 1 :sticky :w)
      (ltk:grid exit-button 0 2 :sticky :w)
      (ltk:grid details-pane 3 0
                :sticky :nw
                :rowspan 20
                :columnspan 150)
      (ltk:grid stream-info-section-title 3 0
                :ipadx 10
                :columnspan 20
                :sticky :w)
      (ltk:grid bits-per-sample-label 4 0
                :ipadx 10
                :columnspan 2
                :sticky :w)
      ;; (ltk:grid bps-info 4 3
      ;;           :ipadx 10
      ;;           :columnspan 1
      ;;           :sticky :w)
      (ltk:grid min-block-size-label 5 0
                :ipadx 10
                :columnspan 2
                :sticky :w)
      ;; (ltk:grid min-block-size 5 3
      ;;           :ipadx 10
      ;;           :columnspan 2
      ;;           :sticky :w)
      (ltk:grid max-block-size-label 6 0
                :ipadx 10
                :columnspan 2
                :sticky :w)
      ;; (ltk:grid max-block-size 6 3
      ;;           :ipadx 10
      ;;           :columnspan 2
      ;;           :sticky :w)
      )))
