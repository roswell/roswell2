(defpackage :roswell.quicklisp.extensions
  (:use :cl
        #+roswell2.init :roswell.init
        ))
(in-package :roswell.quicklisp.extensions)

(defun asd-p (file)
  (equal (pathname-type file) "asd"))

(defun load-asd (file)
  (asdf:load-asd file))

#+roswell2.init
(setf *load* (acons 'asd-p 'load-asd (remove 'asd-p *load* :key 'first)))

(defun ros-p (file)
  (or (equal (pathname-type file) "ros")
      (null (pathname-type file))))

(defun load-ros (file)
  (warn "loading ros is not implemented yet ~A" file))

#+roswell2.init
(setf *load* (acons 'ros-p 'load-ros (remove 'ros-p *load* :key 'first)))

(defun fetch-via-roswell (url file &key (follow-redirects t) quietly (maximum-redirects 10))
  "Request URL and write the body of the response to FILE."
  (declare (ignorable follow-redirects maximum-redirects quietly))
  (let ((ret (roswell (list "-v" "internal" "download"
                            (ql-http::urlstring (ql-http:url url))
                            (namestring file)) nil)))
    (values (make-instance 'ql-http::header :status (if (zerop ret) 200 400))
            (probe-file file))))

#+roswell2.init
(dolist (x '("https" "http"))
  (setf ql-http:*fetch-scheme-functions*
        (acons x 'fetch-via-roswell
               (remove x ql-http:*fetch-scheme-functions* :key 'first :test 'equal))))

(pushnew :quicklisp-support-https *features*)