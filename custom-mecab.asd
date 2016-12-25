(in-package :cl-user)
(defpackage custom-mecab-asd
  (:use :cl :asdf))
(in-package :custom-mecab-asd)

(defsystem custom-mecab
  :version "0.1"
  :author "carrotflakes"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql

               :cl-mecab
               :cl-csv)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op custom-mecab-test))))
