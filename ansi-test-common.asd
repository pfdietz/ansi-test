(cl:in-package #:asdf-user)

(defsystem :ansi-test-common
  :depends-on (:regression-test)
  :serial t
  :components
  :license "MIT"
  ((:file "packages")))
