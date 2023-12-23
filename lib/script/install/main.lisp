(uiop:define-package :roswell2.script.install/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/config
        :roswell2/main
        :roswell2.cmd.script
        :roswell2.cmd.run)
  (:nicknames :roswell2.script.install))
(in-package :roswell2.script.install/main)

(defvar *build-hook* nil)
(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun sub-commands ())

(defun options ())

(defun process-exec-sep (line)
  (loop with context = :normal
        with result
        with buf
        for c across line
        do (push c buf)
        do (cond ((eql context :normal)
                  (and
                   (or (eql c #\Space)
                       (eql c #\Tab)
                       (eql c #\"))
                   (push (prog1
                             (coerce (nreverse (cdr buf)) 'string)
                           (setf buf nil
                                 context (cond ((find c '(#\Tab #\Space)) :space)
                                               ((find c '(#\")) :doubleq))))
                         result)))
                 ((eql context :space)
                  (and
                   (or (eql c #\")
                       (not (find c '(#\Tab #\Space))))
                   (setf buf (list (first buf))
                         context (cond ((find c '(#\")) :doubleq)
                                       (t :normal)
                                       ))))
                 ((eql context :doubleq)
                  (cond ((eql c #\")
                         (push (coerce (nreverse buf) 'string) result)
                         (setf buf nil
                               context :space))
                        ((eql c #\\)
                         (setf context :doubleqslash))))
                 ((eql context :doubleqslash)
                  (case c
                    (t (setf context :doubleq)))))
        finally (let ((str (coerce (nreverse buf) 'string)))
                  (return (nreverse (if (zerop (length str))
                                        result
                                        (cons str result)))))))
(defun process-exec-from-ros (sep)
  (loop with first
        with second
        for args on (cddr sep)
        for a = (first args)
        do (cond ((find a '("-v" "--verbose"
                            "-Q" "-A"
                            ) :test 'equal)
                  (push a first))
                 ((find a '("-L" "--lisp") :test 'equal)
                  (push "-L" first)
                  (pop args)
                  (push (cond ((equal "sbcl-bin" (first args)) "sbcl")
                              (t (first args))) first))
                 ((= (count #\= a) 1)
                  (let ((pos (position #\= a)))
                    (push (format nil "--~A" (subseq a 0 pos)) second)
                    (push (subseq a (1+ pos)) second)))
                 ((find a '("-m" "--image") :test 'equal) ;; ignore image
                  (pop args)))
        finally (return `("exec" "lisp" "script" "run" ,@(nreverse first) "--" ,@(nreverse second) "--" "$0" "\"$@\""))))

(defun process-exec (line)
  (let ((sep (process-exec-sep line)))
    (format nil "~{~A~^ ~}" 
            (cond ((equal (second sep) "ros")
                   (process-exec-from-ros sep))
                  (t sep)))))

#|
(process-exec "exec ros -v dynamic-space-size=8000 -L sbcl-bin -m arrival -Q -- $0 \"\\\"$@\"")
(process-exec "exec ros -Q -L sbcl-bin -- $0 \"$@\"")
|#

(defun register-script (to system)
  (message :register-script "register script file ~S system ~S" to system)
  (let ((file (file-namestring to))
        (config (load-config :where :user)))
    (let ((list (coerce (config `(,system  "scripts") config :if-does-not-exist nil) 'list)))
      (pushnew file list :test 'equal)
      (setf (config `(,system  "scripts") config :if-does-not-exist :create) list))
    (save-config :config config :where :user)))

(defun copy-ros (from to)
  (with-open-file (in from)
    (with-open-file (out to :direction :output :if-exists :supersede)
      (format out "~A~%" (read-line in)) ;; shebang
      (format out "~A~%" (read-line in)) ;; mode
      (format out "~A ~%" (read-line in)) ;; #|
      (format out "~A~%" (process-exec (read-line in))) ;; exec line
      ;; rest of file.
      (loop for line = (read-line in nil nil)
            while line
            do (format out "~A~%" line)))))

(defun install-file (from &key system)
  (let ((to (ensure-directories-exist
             (make-pathname
              :defaults (bin-dir)
              :name (pathname-name from)
              :type (unless (or (equalp (pathname-type from) "ros"))
                      (pathname-type from)))))
        (ros-p (equalp (pathname-type from) "ros")))
    (message :install-file "install file ~A ros-p:~A" to ros-p)
    (if ros-p
        (copy-ros from to)
        (uiop/stream:copy-file from to))
    (when system
      (register-script to system))
    (sb-posix:chmod to #o755)
    #+win32
    (when ros-p
      (copy-ros from (make-pathname :defaults to :type "ros")))))

(defun install-scripts-from-dir (dir &key system)
  (loop for file in (directory (merge-pathnames "roswell/*.*" dir))
        do (install-file file :system system)))

(defun handler (cmd)
  (message :script-install "script install ~S" (clingon:command-arguments cmd))
  (let ((args (clingon:command-arguments cmd)))
    (loop with system
          for arg in args
          for script = (make-pathname :defaults arg :type "ros")
          do (cond ((and (pathname-name script)
                         (uiop:file-exists-p script))
                    (install-file script))
                   ((setf system (ql-dist:find-system arg))
                    (ql-dist:ensure-installed system)
                    (let ((release (ql-dist:release system)))
                      (install-scripts-from-dir (ql-dist:base-directory release)
                                                :system (ql-dist:name system))))
                   ((setf system (asdf:find-system arg nil))
                    (let* ((asd (asdf:system-source-file system))
                           (dir (make-pathname :defaults asd :name nil :type nil)))
                      (install-scripts-from-dir dir
                                                :system (asdf:component-name system))))))))
