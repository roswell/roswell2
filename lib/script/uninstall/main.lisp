(uiop:define-package :roswell2.script.uninstall/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/config
        :roswell2/main
        :roswell2.cmd.script
        :roswell2.cmd.run)
  (:nicknames :roswell2.script.uninstall))
(in-package :roswell2.script.uninstall/main)

(defvar *build-hook* nil)
(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun sub-commands ())

(defun options ())

(defun handler (cmd)
  (message :script-uninstall "script uninstall ~S" (clingon:command-arguments cmd))
  (let* ((args (clingon:command-arguments cmd))
         (config (load-config :where :user))
         (hash ))
    (loop for arg in args
          for array = (config `(,arg "scripts") config)
          with flag
          do (when array
               (loop for x across (copy-seq array)
                     with result
                     do (ignore-errors 
                          (delete-file (merge-pathnames x (bin-dir)))
                          (push x result))
                     finally (setf (config `(,arg "scripts") config)
                                   (make-array (length result) :initial-contents (nreverse result))
                                   flag t)))
          finally (when flag
                    (when (= (hash-table-count (config `(,arg) config)) 1)
                      (remhash arg config))
                    (save-config :config config :where :user)))))
