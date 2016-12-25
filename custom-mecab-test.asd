(in-package :cl-user)
(defpackage custom-mecab-test-asd
  (:use :cl :asdf))
(in-package :custom-mecab-test-asd)

(defsystem custom-mecab-test
  :author "carrotflakes"
  :license ""
  :depends-on (:custom-mecab
               :prove)
  :components ((:module "t"
                :components
                ((:file "custom-mecab"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
