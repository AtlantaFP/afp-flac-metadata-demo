(defpackage flac-metadata-demo/tests/main
  (:use :cl
        :flac-metadata-demo
        :rove))
(in-package :flac-metadata-demo/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :flac-metadata-demo)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
