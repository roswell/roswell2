(uiop:define-package :roswell-bin/util
  (:use :cl :roswell-bin/config)
  (:export :message
           :strip-run-cmd
           :which
           :libdir
           :user-homedir
           :chdir
           :app-configdir
           :app-cachedir
           :setup-uid
           :subseq*
           :run-program
           :exec
           :*verbose*
           ))

(in-package :roswell-bin/util)

(defvar *message* 'message-func)
(defvar *message-first-inovocation* nil)
(defvar *verbose* 0)

(defun message-func (level fmt &rest params)
  (declare (ignorable level))
  (unless (zerop *verbose*)
    (format *error-output* "~&[~8,4f]"
            (float (/ (- (get-internal-run-time) *message-first-inovocation*)
                      internal-time-units-per-second)))
    (apply #'format *error-output* fmt params)
    (terpri *error-output*)))

(defun message (&rest r)
  (unless *message-first-inovocation*
    (setf *message-first-inovocation* (get-internal-run-time)))
  (apply *message* r)
  nil)

(defvar *strip-run-cmd-hash* (make-hash-table :test 'equal))

(defun strip-run-cmd (cmd &key cache)
  (unless cache
    (remhash cmd *strip-run-cmd-hash*))
  (if (eql (gethash cmd *strip-run-cmd-hash* t) t)
      (setf (gethash cmd *strip-run-cmd-hash*)
            (uiop:run-program
             cmd
             :output '(:string :stripped t)
             :ignore-error-status t))
      (gethash cmd *strip-run-cmd-hash*)))

(defun which (cmd)
  "find out command's full path."
  (let* ((which-cmd #-win32(format nil "command -v ~S" cmd)
                    #+win32(format nil "cmd /c where ~S" cmd))
         (result (strip-run-cmd which-cmd)))
    (message :which "which '~A' -> '~A'" cmd result)
    (setf result (unless (zerop (length result))
                   result))
    result))

(defun libdir ()
  (truename
   (merge-pathnames "../lib/roswell/"
                    (make-pathname :defaults *stage1-path* :name nil :type nil))))

(defun user-homedir ()
  "tweeked user-homedir-pathname"
  #-win32
  (let* ((user (uiop:getenv "SUDO_USER"))
         (uid (sb-posix:getuid))
         (pwd (sb-posix:getpwuid uid)))
    (when (and user (zerop uid))
      (setf pwd (sb-posix:getpwnam user)))
    (parse-namestring (format nil "~A/" (sb-posix:passwd-dir pwd))))
  #+win32
  (parse-native-namestring (sb-win32::get-folder-namestring sb-win32::csidl_profile)))

(defun app-configdir ()
  "Return directory which this applicatation freely read/write 
   to setup implementations and libraries"
  (let* ((env (uiop:getenv "XDG_CONFIG")) ;;expect $HOME/.config
         (result (or (when env
                       (parse-namestring (format nil "~A/~A/" env *project-name*)))
                     (merge-pathnames (format nil
                                              #-win32 ".config/~A/"
                                              #+win32 "config/~A/"
                                              *project-name*)
                                      (user-homedir)))))
    (message :app-configdir "app-configdir => ~A" result)
    result))

(defun app-cachedir ()
  "return directory which might be not important"
  (let* ((env (uiop:getenv "XDG_CACHE_HOME")) ;;expect $HOME/.cache
         (result (or (when env
                       (parse-namestring (format nil "~A/~A/" env *project-name*)))
                     (merge-pathnames (format nil ".cache/~A/" *project-name*)
                                      (user-homedir)))))
    (message :app-configdir "app-cachedir => ~A" result)
    result))

(defun chdir (dir)
  (message :chdir "chdir: ~S" dir)
  (uiop:chdir dir))

(defun setup-uid (&key euid)
  "Drop Privileges"
  #-(or win32 android)
  (when (zerop (sb-posix:getuid))
    (let* ((uid (ignore-errors (parse-integer (uiop:getenv "SUDO_UID"))))
           (gid (ignore-errors (parse-integer (uiop:getenv "SUDO_GID")))))
      (if euid
          (and
           (and gid (sb-posix:setegid gid))
           (and uid (sb-posix:seteuid uid)))
          (and
           (and gid (sb-posix:setgid gid))
           (and uid (sb-posix:setuid uid))))))
  t)
(defun subseq* (seq start &optional end)
  (let ((len (length seq)))
    (and end
         (minusp end)
         (setf end (+ len end)))
    (and start
         (minusp start)
         (setf start (+ len start)))
    (subseq seq (max 0 start) (and end (min end len)))))

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

(defun run-program (args)
  (uiop:run-program args
                    :output :interactive
                    :error-output :interactive))

(defun exec (args)
  "Launch executable"
  #+unix
  (execvp (first args) args)
  (uiop:quit (run-program args)))
