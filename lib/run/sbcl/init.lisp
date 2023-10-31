(defpackage :roswell2.run.sbcl/init
  (:use :cl)
  (:nicknames :roswell.init :ros :roswell :roswell.util)
  (:shadow :load :eval)
  (:export :main :*load* :*impl-path* :*cache-path* :*stage2-path* :roswell :ensure-asdf :quit
           :which :exec))
(in-package :roswell2.run.sbcl/init)

(defparameter *load* `((identity . cl:load)))
(defvar *impl-path* nil)
(defvar *cache-path* nil)
(defvar *stage2-path* nil)
(defvar *run-repl* nil)
(defvar *dump-file* nil)
(defvar *dump-option* nil)
(defvar *repl-function* nil)

(defun ensure-asdf (&key (version))
  (declare (ignore version))
  (require :asdf))

(defun roswell (args &optional (output :string) trim)
  (let* ((a0 *stage2-path*)
         (proc (sb-ext:run-program a0 args
                                   :output :stream
                                   :error t))
         (ret (if (equal output :string)
                  (let ((input (sb-ext:process-output proc)))
                    (with-output-to-string (o)
                      (let ((a (make-array 512 :initial-element nil)))
                        (loop for len = (read-sequence a input)
                              do (write-sequence a o :end len)
                              while (or (= len 512)
                                        (and (eql (sb-ext:process-status proc) :running)
                                             (sleep .1)))))))
                  (loop while (and (eql (sb-ext:process-status proc) :running)
                                   (sleep .1))
                        finally (return-from roswell (sb-ext:process-exit-code proc))))))
    (if trim
        (remove #\Newline (remove #\Return ret))
        ret)))

(defun re (&rest r)
  (cl:eval (read-from-string (apply 'format nil r))))

#+unix
(progn ;from swank
  (sb-alien:define-alien-routine ("execvp" %execvp) sb-alien:int
    (program sb-alien:c-string)
    (argv (* sb-alien:c-string)))
  (defun execvp (program args)
    "Replace current executable with another one."
    (let ((a-args (sb-alien:make-alien sb-alien:c-string
                                       (+ 1 (length args)))))
      (unwind-protect
           (progn
             (loop for index from 0 by 1
                   and item in (append args '(nil))
                   do (setf (sb-alien:deref a-args index)
                            item))
             (when (minusp
                    (%execvp program a-args))
               (let ((errno (sb-impl::get-errno)))
                 (case errno
                   (2 (error "No such file or directory: ~S" program))
                   (otherwise
                    (error "execvp(3) failed. (Code=~D)" errno))))))
        (sb-alien:free-alien a-args)))))

(defun run-program (args &key output)
  (ensure-asdf)
  (re "(uiop:run-program ~S :output ~S :error-output :interactive)"
      args
      (or output :interactive)))

(defun exec (args)
  "Launch executable"
  #+unix
  (execvp (first args) args)
  (re "(uiop:quit(run-program ~S)" args))

(defvar *strip-run-cmd-hash* (make-hash-table :test 'equal))
(defun strip-run-cmd (cmd &key cache)
  (ensure-asdf)
  (unless cache
    (remhash cmd *strip-run-cmd-hash*))
  (if (eql (gethash cmd *strip-run-cmd-hash* t) t)
      (setf (gethash cmd *strip-run-cmd-hash*)
            (re "(uiop:run-program ~S :output '(:string :stripped t) :ignore-error-status t)"
                cmd))
      (gethash cmd *strip-run-cmd-hash*)))

(defun which (cmd)
  "find out command's full path."
  (let* ((which-cmd #-win32(format nil "command -v ~S" cmd)
                    #+win32(format nil "cmd /c where ~S" cmd))
         (result (strip-run-cmd which-cmd)))
    (setf result (unless (zerop (length result))
                   result))
    result))

(defun asdf (&rest rest)
  (declare (ignorable rest))
  (ensure-asdf))

(defun quicklisp (path-or-t &rest rest)
  (declare (ignorable rest))
  (ensure-asdf)
  (unless (find :quicklisp *features*)
    (let* ((ql-origin (merge-pathnames  "quicklisp/" *cache-path*))
           (setup (merge-pathnames "setup.lisp"
                                   (if (eql t path-or-t)
                                       ql-origin
                                       path-or-t))))
      (unless (eql t path-or-t)
        (re "(push ~S asdf:*central-registry*)"
            (merge-pathnames  "quicklisp/" ql-origin)))
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

(push :roswell2.init *features*)
