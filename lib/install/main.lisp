(uiop:define-package :roswell2.cmd.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.cmd.install)
  (:import-from :clingon)
  (:export :sh
           :impl-path
           :impl-param
           :impl-param-name
           :impl-param-variant
           :impl-param-os
           :impl-param-arch
           :impl-param-base-uri
           :impl-param-version
           :impl-param-uri
           :impl-param-archive))
   
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

(defun impl-path (param)
  ;; "~/.cache/roswell/impl/sbcl/2.3.7/x86-64/linux/bin/"
  (merge-pathnames
   (format nil "impl/~A/~A/~A/~A/~A/"
           (impl-param-name param)
           (impl-param-version param)
           (impl-param-os param)
           (impl-param-arch param)
           (impl-param-variant param))
   (app-cachedir)))

(defclass impl-param ()
  ((impl
    :initarg :impl
    :initform nil
    :accessor impl-param-name)
   (variant
    :initarg :variant
    :initform nil
    :accessor impl-param-variant)
   (os
    :initarg :os
    :initform nil
    :accessor impl-param-os)
   (arch
    :initarg :arch
    :initform nil
    :accessor impl-param-arch)
   (base-uri
    :initarg :base-uri
    :initform nil
    :accessor impl-param-base-uri)
   (version
    :initarg :version
    :initform nil
    :accessor impl-param-version)
   (uri
    :initarg :uri
    :initform nil
    :accessor impl-param-uri)
   (archive
    :initarg :archive
    :initform nil
    :accessor impl-param-archive)))
