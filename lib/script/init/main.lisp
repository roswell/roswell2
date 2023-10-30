(uiop:define-package :roswell2.script.init/main
  (:use :cl
        :roswell-bin/util)
        
  (:nicknames :roswell2.script.init))
(in-package :roswell2.script.init/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun sub-commands ())

(defun options ())

(defun handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (name (first args))
         (name (namestring (make-pathname :defaults name :type nil)))
         params)
    (map () (lambda (i)
              (setf name (remove i name)))
         "./\\")
    (setf params (loop for (i j) on (cdr args) by #'cddr
                       collect (intern i :keyword)
                       collect j))
    (let* ((date (get-universal-time))
           (path (make-pathname :defaults name :type "ros")))
      (handler-case
          (unless
              (prog1
                  (with-open-file (out path
                                       :direction :output
                                       :if-exists nil
                                       :if-does-not-exist :create)
                    (when out
                      (format out "~@{~A~%~}"
                              "#!/bin/sh"
                              "#|-*- mode:lisp -*-|#"
                              "#|"
                              "exec lisp script run -L sbcl -- -- $0 \"$@\"" "|#"
                              "(progn ;;init forms"
                              "  (ros:ensure-asdf)"
                              (let ((lib (getf params :|lib|)))
                                (format nil "  #+quicklisp(ql:quickload '(~A) :silent t)"
                                        (or lib "")))
                              "  )"
                              ""
                              (format nil "(defpackage :ros.script.~A.~A" name date)
                              "  (:use :cl))"
                              (format nil "(in-package :ros.script.~A.~A)" name date)
                              ""
                              "(defun main (&rest argv)"
                              "  (declare (ignorable argv)))"
                              ";;; vim: set ft=lisp lisp:")
                      (format t "~&Successfully generated: ~A~%" path)
                      t))
                (sb-posix:chmod path #o700))
            (format *error-output* "~&File already exists: ~A~%" path)
            (uiop:quit 1))
        (error (e)
          (format *error-output* "~&~A~%" e)
          (uiop:quit 1))))))
