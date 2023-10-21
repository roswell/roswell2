(uiop:define-package :roswell-bin/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell-bin/uname
        :roswell-bin/download
        :roswell-bin/build)
  (:export :main :setup))

(in-package :roswell-bin/main)

(defun internal (args)
  "functionality which is difficult to implement without ffi or external commands. stage-2 can invoke it for implement installers."
  (message :internal "message args = ~S" args)
  (let* ((arg (first args))
         (args (rest args)))
    (cond
      ((equal arg "rebuild")
       (build nil :force t)
       (uiop:quit 0))
      ((equal arg "head")
       (message :internal "not implementead head"))
      ((equal arg "uname")
       (uname args))
      ((equal arg "which")
       (which (first args)))
      ((equal arg "man")
       (message :internal "not implementead man"))
      ((equal arg "impl")
       (message :internal "not implementead impl"))
      ((equal arg "version")
       (message :internal "not implementead version")))))

(defun invoke-stage2 (args)
  (let* ((core (build nil))
         (invoke-list `(,(format nil "~A" core)
                        "--eval"
                        ,(format nil "(setf roswell-bin/util::*message-first-inovocation* ~A)"
                                 roswell-bin/util::*message-first-inovocation*)
                        ,@(unless (zerop roswell-bin/util:*verbose*)
                            (list (format nil "-~v@{v~}" roswell-bin/util:*verbose* nil)))
                        ,@args)))
    (message :main "args: ~S core: ~S" args core)
    (message :main "invoke-list ~S" invoke-list)
    (exec invoke-list)
    ;; in case
    (uiop:quit 1)))

(defun setup ()
  (sb-posix:unsetenv "P")
  (setf *stage1-commit* (uiop:read-file-line "lib/commit"))
  (lib-init)
  (uname-s)
  (uname-m))

(defun main ()
  (setup-uid :euid t)
  #+nil(print (list :uid (sb-posix:getuid) 
                    :gid (sb-posix:getgid)
                    :euid (sb-posix:geteuid)
                    :egid (sb-posix:getegid)))
  #+nil(print (uiop/image:raw-command-line-arguments))
  #+nil(print (list :sbcl-homedir sb-sys::*sbcl-homedir-pathname*))
  #+nil(print (list :homedir (user-homedir)))
  (setf *stage1-path* sb-ext:*runtime-pathname*)
  (loop for args on (rest (uiop/image:raw-command-line-arguments))
        for arg = (first args)
        do (cond ((equal arg "roswell-internal-use")
                  (uiop:quit (internal (rest args))))
                 ((equal arg "rebuild")
                  (build nil :force t)
                  (uiop:quit 0))
                 ((find arg '("--load" "-l") :test 'equal)
                  (load (second args))
                  (setf args (rest args)))
                 ((find arg '("--verbose" "-v") :test 'equal)
                  (incf roswell-bin/util:*verbose*))
                 ((find arg '("--eval" "-e") :test 'equal)
                  (eval (read-from-string (second args)))
                  (setf args (rest args)))
                 (t
                  (when (equal (car args) "--")
                    (setf args (cdr args)))
                  (invoke-stage2 args)))
        finally (invoke-stage2 args)))
