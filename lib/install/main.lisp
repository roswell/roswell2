(uiop:define-package :roswell2.cmd.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.cmd.install)
  (:import-from :clingon)
  (:export :sh
           :option-base
           ))
   
(in-package :roswell2.cmd.install/main)

(defvar *command-class* 'roswell2/clingon.extensions::install-command)

(defun options ()
  "Returns the options for the  command"
  (list))

(defun option-base (&key variant-explanation
                         base-uri-explanation
                         arch-explanation
                         os-explanation)
  (list
   (clingon:make-option
    :string
    :description arch-explanation
    :parameter "ARCH"
    :long-name "arch"
    :key :arch)
   (clingon:make-option
    :string
    :description variant-explanation
    :parameter "VARIANT"
    :long-name "variant"
    :key :variant)
   (clingon:make-option
    :string
    :description os-explanation
    :parameter "OS"
    :long-name "os"
    :key :os)
   (clingon:make-option
    :string
    :description "set version for install"
    :parameter "VERSION"
    :long-name "version"
    :key :version)
   (clingon:make-option
    :string
    :description base-uri-explanation
    :parameter "URI"
    :long-name "base-uri"
    :key :base-uri)
   (clingon:make-option
    :string
    :description (format nil "set archive uri")
    :parameter "URI"
    :long-name "uri"
    :key :uri)
   (clingon:make-option
    :string
    :description (format nil "set local archive to install instad of downloading from The internet.")
    :parameter "archivefile"
    :long-name "archive"
    :key :archive)))

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
