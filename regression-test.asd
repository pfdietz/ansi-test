(cl:in-package #:asdf-user)

(defsystem :regression-test
  :serial t
  :components
  ((:file "rt-package")
   (:file "rt")))
