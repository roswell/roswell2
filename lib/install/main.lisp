(uiop:define-package :roswell2.cmd.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.cmd.install)
  (:import-from :clingon)
  (:export :sh
           :impl-path
           :install-param
           :install-param-impl
           :install-param-variant
           :install-param-os
           :install-param-arch
           :install-param-base-uri
           :install-param-version
           :install-param-uri
           :install-param-archive))
   
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
           (install-param-impl param)
           (install-param-version param)
           (install-param-os param)
           (install-param-arch param)
           (install-param-variant param))
   (app-cachedir)))

(defclass install-param ()
  ((impl
    :initarg :impl
    :initform nil
    :accessor install-param-impl)
   (variant
    :initarg :variant
    :initform nil
    :accessor install-param-variant)
   (os
    :initarg :os
    :initform nil
    :accessor install-param-os)
   (arch
    :initarg :arch
    :initform nil
    :accessor install-param-arch)
   (base-uri
    :initarg :base-uri
    :initform nil
    :accessor install-param-base-uri)
   (version
    :initarg :version
    :initform nil
    :accessor install-param-version)
   (uri
    :initarg :uri
    :initform nil
    :accessor install-param-uri)
   (archive
    :initarg :archive
    :initform nil
    :accessor install-param-archive)))
