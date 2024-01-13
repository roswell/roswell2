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

(defun options ())

(defun handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (impl (first args))
         (to (ensure-directories-exist
              (make-pathname
               :defaults (bin-dir)
               :name impl
               :type nil))))
    (message :script-impl "script impl ~S" args)
    (with-open-file (o to :direction :output :if-exists :supersede)
      (format o "~{~A~%~}"
              `("#!/bin/sh"
                "#|-*- mode:lisp -*-|#"
                "#|"
                ,(format nil "exec lisp run -L ~A --native -- \"$@\"" impl)
                "|#")))))
