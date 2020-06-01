(defsystem "flac-metadata-demo"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria" "fast-io" "bitio" "babel" "ltk")
  :components ((:module "src"
		:serial t
                :components
                ((:file "buffer")	       
		 (:file "read-primitives")
		 (:file "block-stream")
		 (:file "metadata")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "flac-metadata-demo/tests"))))

(defsystem "flac-metadata-demo/tests"
  :author ""
  :license ""
  :depends-on ("flac-metadata-demo"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for flac-metadata-demo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
