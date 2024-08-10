(uiop:define-package :roswell2.impl.install/main
  (:use :cl
        :roswell-bin/util
        :roswell2/main)
  (:nicknames :roswell2.impl.install)
  (:import-from :clingon)
  (:export :sh
           :option-base
           :install
           :impl-set-version-param
           :impl-save-config
           :install-impl-param))
   
(in-package :roswell2.impl.install/main)

(defvar *command-class* 'roswell2/clingon.extensions::install-command)

(defun options ()
  "Returns the options for the  command"
  (list))

(defclass install-impl-param (impl-param)
  ((config-location
    :initarg :config-location
    :initform nil
    :reader impl-param-config-location)))

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
    :description (format nil "set local archive to install instead of downloading from The internet.")
    :parameter "archivefile"
    :long-name "archive"
    :key :archive)
   (clingon:make-option
    :string
    :description "edit config to use the implementation as default (user/local/none)"
    :long-name "config-location"
    :key :config-location)
   ))

(defmethod impl-set-version-param ((param impl-param)))

(defun sub-commands ()
  (sub-command-filter "roswell2.install."))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (message :main-handler "args-for install handler ~S" args)
    (cond ((null args)
           (clingon:run cmd '("--help")))
          (t)))
  (uiop:quit))

(defun sh ()
  (or (which "bash")
      "sh"))

(defun impl-save-config (param)
  (message :impl-save-config "impl-save-config param:~S" param)
  (let* ((where (slot-value param 'config-location))
         (variant (impl-param-variant* param))
         (version (impl-param-version param))
         (config (when where (load-config :where where)))
         (name (impl-param-name param)))
    (when where
      (unless (config `(,name "variant") config :if-does-not-exist nil)
        (setf (config `(,name "variant") config) variant))
      (unless (config `(,name "version") config :if-does-not-exist nil)
        (setf (config `(,name "version") config) version))
      (save-config :config config :where where))
    (setf (slot-value param 'config-location) nil)
    (with-open-file (o (merge-pathnames "roswell.sexp" (impl-path param))
                       :direction :output
                       :if-exists :supersede)
      (format o "~S~%" param))))

(defmethod install :after ((param install-impl-param))
  (message :install-after "install after ~S" param)
  (impl-save-config param))

(defmethod impl-set-param :after ((param install-impl-param) cmd)
  (message :impl-set-param "set config-location ~S" (clingon:getopt cmd :config-location))
  (setf (slot-value param 'config-location)
        (let ((place (clingon:getopt cmd :config-location)))
          (cond ((null place) :user)
                ((equalp place "user") :user)
                ((equalp place "local") :local)
                (t nil)))))
