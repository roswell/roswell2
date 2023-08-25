(defpackage :roswell2.run.sbcl/init
  (:use :cl)
  (:nicknames :roswell.init)
  (:shadow :load :eval)
  (:export :main :*load* :*impl-path*))
(in-package :roswell2.run.sbcl/init)

(defparameter *load* `((identity . cl:load)))
(defvar *impl-path* nil)

(defun load (file &rest rest)
  (let ((function (rest (find-if (lambda (x) (funcall (first x) file)) *load*))))
    (apply function file rest)))

(defun eval (arg &rest rest)
  (declare (ignorable rest))
  (loop with start = 0
        with end = (gensym)
        with exp
        do (multiple-value-setq (exp start)
             (read-from-string arg nil end :start start))
        until (eql exp end)
        do (cl:eval exp)))

(defun quit (arg &rest rest)
  (declare (ignorable rest))
  (sb-ext:quit :unix-status arg))

(defun main (args)
  (loop with package = (find-package :roswell2.run.sbcl/init)
        for elt in args
        do (apply (intern (string (first elt)) package) (rest elt))))

(push :roswell2.run.sbcl/init *features*)