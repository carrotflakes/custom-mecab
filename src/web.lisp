(in-package :cl-user)
(defpackage custom-mecab.web
  (:use :cl
        :caveman2
        :custom-mecab.config
        :custom-mecab.view
        :custom-mecab.db
        :datafly
        :sxql)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :cl-csv
                :read-csv
                :write-csv)
  (:export :*web*))
(in-package :custom-mecab.web)

;; for @route annotation
(syntax:use-syntax :annot)


(defun dict-name-valid-p (name)
  (scan "^[a-zA-Z0-9_-]+$" name))

(defun build-dic (&optional name)
  (let ((dict-path (merge-pathnames (format nil "~a.csv" name) *dict-directory*))
        (user-dic-path (merge-pathnames (format nil "~a.dic" name) *mecab-dic-directory*)))

    (when (probe-file user-dic-path)
      (delete-file user-dic-path))

    (with-output-to-string (output)
      (sb-ext:run-program *mecab-dict-index-path*
                          (list "-d" (namestring *ipadic-src-path*)
                                "-u" (namestring user-dic-path)
                                "-f" "utf-8"
                                "-t" "utf-8"
                                (namestring dict-path))
                          :output output))))

(defun save-dict (name words sentences)
  (let ((path (merge-pathnames (format nil "~a.csv" name) *dict-directory*)))
    (with-open-file (stream
                     path
                     :direction :output
                     :if-exists :supersede)
      (loop
         for row in words
         do (format stream "~a~%" row))))
  (let ((path (merge-pathnames name *sentence-directory*)))
    (with-open-file (stream
                     path
                     :direction :output
                     :if-exists :supersede)
      (loop
         for sentence in sentences
         do (format stream "~a~%" sentence)))))

(defun load-dict (name)
  (list
   :|words|
   (let ((path (merge-pathnames (format nil "~a.csv" name) *dict-directory*)))
     (with-open-file (stream
                      path
                      :direction :input)
       (loop
          for row = (read-line stream nil)
          while row
          collect row)))
   :|sentences|
   (let ((path (merge-pathnames name *sentence-directory*)))
     (with-open-file (stream
                      path
                      :direction :input
                      :if-does-not-exist :create)
       (loop
          for row = (read-line stream nil)
          while row
          collect row)))))

(defun check-dict (name)
  (print (build-dic name))
  (let ((path (merge-pathnames name *sentence-directory*))
        (user-dic-path (merge-pathnames (format nil "~a.dic" name) *mecab-dic-directory*)))
    (with-open-file (stream
                     path
                     :direction :input
                     :if-does-not-exist :create)

      (cl-mecab:with-mecab* ((if (probe-file user-dic-path)
                                 (format nil "-d ~a -u ~a" *ipadic-path* user-dic-path)
                                 (format nil "-d ~a" *ipadic-path*)))
        (loop
           for row = (read-line stream nil)
           while row
           collect row;(intern row :keyword)
           collect (mapcar #'car (cl-mecab:parse* row)))))))


;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  '(render #P"index.html")
  #P"static/index.html")

(defroute "/features" ()
  #P"static/features.csv")

(defroute ("/dict/:name" :method :GET) (&key name)
  (or (handler-case
          (cond
            ((dict-name-valid-p name)
             (render-json (list* :|ok| t (load-dict name)))))
        (error (c)
          nil))
      (render-json '(:|ok| nil))))

(defroute ("/dict/:name" :method :PUT) (&key name _parsed)
  (cond
    ((dict-name-valid-p name)
     (save-dict name
                (cdr (assoc "words" _parsed :test #'string=))
                (cdr (assoc "sentences" _parsed :test #'string=)))
     "ok")
    (t
     "ng")))

(defroute ("/dict/:name/check" :method :GET) (&key name)
  (handler-case
      (render-json (check-dict name))
    (error (c)
      (print c)
      "failed")))

(defroute ("/mecab-dic/:name" :method :GET) (&key name)
  (let ((path (merge-pathnames name *mecab-dic-directory*)))
    (cond
      ((probe-file path)
       (setf (getf (response-headers *response*) :content-type) "application/octet-stream")
       path)
      (t
       "No file"))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
