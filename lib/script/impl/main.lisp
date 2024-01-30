(uiop:define-package :roswell2.script.impl/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/config
        :roswell2/main
        :roswell2.cmd.script
        :roswell2.cmd.run)
  (:nicknames :roswell2.script.impl))
(in-package :roswell2.script.impl/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun sub-commands ())

(defun options ()
  (list
   (clingon:make-option
    :string
    :description "designate script name"
    :parameter "ALIAS"
    :short-name #\A
    :long-name "alias"
    :key :alias)))

(defun handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (impl (first args))
         (to (ensure-directories-exist
              (make-pathname
               :defaults (bin-dir)
               :name (or (clingon:getopt cmd :alias) impl)
               :type nil))))
    (message :script-impl "script impl ~S" args)
    (unless (clingon:command-arguments cmd)
      (clingon:run cmd '("--help")))
    (with-open-file (o to :direction :output :if-exists :supersede)
      (format o "~{~A~%~}"
              `("#!/bin/sh"
                ,(format nil "exec lisp run -L ~A --native -- \"$@\"" impl))))
    (sb-posix:chmod to #o755)))
