(uiop:define-package :roswell2.cmd.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.cmd.install)
  (:import-from :clingon))
   
(in-package :roswell2.cmd.install/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ()
  "Returns the options for the  command"
  (list))

(defun sub-commands ()
  )

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (message :main-handler "args-for install handler ~S" args)
    (cond ((null args)
           )
          (t
          )))
  (uiop:quit))
