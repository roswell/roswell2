(uiop:define-package :roswell2.cmd.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.cmd.install)
  (:import-from :clingon)
  (:export :sh
           ))
   
(in-package :roswell2.cmd.install/main)

(defvar *command-class* 'roswell2/clingon.extensions::install-command)

(defun options ()
  "Returns the options for the  command"
  (list
   ))

(defun sub-commands ()
  (sub-command-filter "roswell2.install."))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (message :main-handler "args-for install handler ~S" args)
    (cond ((null args)
           )
          (t
          )))
  (uiop:quit))

(defun sh ()
  (or (which "bash")
      "sh"))
