(uiop:define-package :roswell2.cmd.script/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run)
  (:nicknames :roswell2.cmd.script)
  (:import-from :clingon)
  (:export :bin-dir))

(in-package :roswell2.cmd.script/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun bin-dir ()
  (merge-pathnames ".roswell/bin/" (user-homedir)))

(defun sub-commands ()
  (sub-command-filter "roswell2.script."))

(defun options ()
  )

(defun handler (cmd)
  "Handler for just evaluate options"
  )
