(defpackage :roswell2.run.sbcl/init
  (:use :cl)
  (:nicknames :roswell.init :ros)
  (:shadow :load :eval)
  (:export :main :*load* :*impl-path* :*cache-path* :ensure-asdf))
(in-package :roswell2.run.sbcl/init)

(defparameter *load* `((identity . cl:load)))
(defvar *impl-path* nil)
(defvar *cache-path* nil)
(defvar *run-repl* nil)
(defvar *dump-file* nil)
(defvar *dump-option* nil)
(defvar *repl-function* nil)

(defun ensure-asdf (&key (version))
  (declare (ignore version))
  (require :asdf))

(defun asdf (&rest rest)
  (declare (ignorable rest))
  (ensure-asdf))

(defun quicklisp (path-or-t &rest rest)
  (declare (ignorable rest))
  (ensure-asdf)
  (flet ((re (&rest r) (cl:eval (read-from-string (apply 'format nil r)))))
    (let* ((ql-origin (merge-pathnames  "quicklisp/" *cache-path*))
           (setup (merge-pathnames "setup.lisp"
                                   (if (eql t path-or-t)
                                       ql-origin
                                       path-or-t))))
      (re "(push ~S asdf:*central-registry*)"
          (merge-pathnames  "quicklisp/" ql-origin))
      (unless (probe-file (ensure-directories-exist setup))
        (re "(uiop:copy-file ~S ~S)"
            (merge-pathnames "setup.lisp" ql-origin)
            setup))
      (cl:load setup))))

(defun dump (file &rest rest)
  (declare (ignorable rest))
  (setf *dump-file* file))

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

(defun quit (&optional (arg 0) &rest rest)
  (declare (ignorable rest))
  (sb-ext:quit :unix-status arg))

(defun repl (&rest rest)
  (declare (ignorable rest))
  (setf *run-repl* t))

(defun main (args)
  (loop with package = (find-package :roswell2.run.sbcl/init)
        for elt in args
        for sym = (intern (string (first elt)) package)
        do (apply sym (rest elt)))
  (when *dump-file*
    (let ((dump-file *dump-file*))
      (setf *dump-file* nil)
      (ensure-directories-exist dump-file)
      (apply 'sb-ext:save-lisp-and-die dump-file *dump-option*)))
  (when *run-repl*
    (sb-ext:enable-debugger)
    (if *repl-function*
        (funcall *repl-function*)
        (sb-impl::toplevel-repl nil))))

(push :roswell2.run.sbcl/init *features*)
