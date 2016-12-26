(in-package :cl-user)
(defpackage custom-mecab.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :*dict-directory*
           :*sentence-directory*
           :*mecab-dic-directory*
           :*ipadic-src-path*
           :*ipadic-path*
           :*mecab-dict-index-path*
           :appenv
           :developmentp
           :productionp))
(in-package :custom-mecab.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :custom-mecab))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defparameter *dict-directory* (merge-pathnames #P"dict/" *application-root*))
(defparameter *sentence-directory* (merge-pathnames #P"sentence/" *application-root*))
(defparameter *mecab-dic-directory* (merge-pathnames #P"mecab-dic/" *application-root*))

(defparameter *ipadic-src-path* #P"/usr/local/lib/mecab/dic/ipadic")
(defparameter *ipadic-path* #P"/usr/local/lib/mecab/dic/ipadic")
(defparameter *mecab-dict-index-path* #P"/usr/local/Cellar/mecab/0.996/libexec/mecab/mecab-dict-index")


(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  `(:error-log ,(merge-pathnames #P"log/error.log" *application-root*)))

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
