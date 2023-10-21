(uiop:define-package :roswell2.cmd.internal/main
  (:use :cl
        :roswell-bin/download)
  (:nicknames :roswell2.cmd.internal)
  (:import-from :clingon))

(in-package :roswell2.cmd.internal/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ())

(defun intern-if-it-looks-keyword (x)
  (if (eql #\: (ignore-errors (aref x 0)))
      ;; it's only care run on default sbcl so don't consider readtable-case.
      (intern (string-upcase x) :keyword)
      x))

(defun download-cmd (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (apply 'download-simple
           (mapcar 'intern-if-it-looks-keyword args))))

(defun tar-cmd (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (tar args)))

(defun sub-commands ()
  (list
   (make-instance
    'roswell2/clingon.extensions::command-without-version
    :name "download"
    :description "http/https client"
    :handler 'download-cmd)
   (make-instance
    'roswell2/clingon.extensions::command-without-version
    :name "tar"
    :description "extract archive"
    :handler 'tar-cmd)))

(defun handler (cmd)
  "Handler for just evaluate options"
  (unless (clingon:command-arguments cmd)
    (clingon:run cmd '("--help"))))
