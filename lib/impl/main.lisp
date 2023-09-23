(uiop:define-package :roswell2.cmd.impl/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run)
  (:nicknames :roswell2.cmd.impl)
  (:import-from :clingon))

(in-package :roswell2.cmd.impl/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ())

(defun sub-commands ()
  (sub-command-filter "roswell2.impl."))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (message :pin-handler "args-for impl handler ~S" args))
  (uiop:quit))
