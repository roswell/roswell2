(uiop:define-package :roswell2.cmd.run/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        )
  (:nicknames :roswell2.cmd.run)
  (:import-from :clingon)
  (:export :run 
           :distinguish
           :*forms* :impl-path
           :run-param-args
           :run-param-image))

(in-package :roswell2.cmd.run/main)

(defvar *forms* nil)
(defvar *command-class* 'roswell2/clingon.extensions::run-command)


(defun options ()
  ;;tbd consider make it extensible from sub modules.
  "Returns the options for the  command"
  (list
   (clingon:make-option
    :string
    :description "Run roswell with a lisp impl NAME."
    :parameter "IMPL"
    :short-name #\L
    :long-name "lisp"
    :key :lisp)
   (clingon:make-option
    :string
    :description (format nil "set arch. defualt:~A" (uname-m))
    :parameter "ARCH"
    :long-name "arch"
    :key :arch)
   (clingon:make-option
    :string
    :description "set variant"
    :parameter "VARIANT"
    :long-name "variant"
    :key :variant)
   (clingon:make-option
    :string
    :description (format nil "set os. default:~A" (uname-s))
    :parameter "OS"
    :long-name "os"
    :key :os)
   (clingon:make-option
    :string
    :description "set version"
    :parameter "VERSION"
    :long-name "version"
    :key :version)
   (clingon:make-option
    :option-filter
    :description "evaluate form"
    :parameter "FORM"
    :short-name #\e
    :long-name "eval"
    :filter (lambda (x option)
              (declare (ignore option))
              (push (list :eval x) *forms*)
              nil)
    :key :eval)
   (clingon:make-option
    :option-filter
    :description "load lisp FILE"
    :parameter "FILE"
    :short-name #\l
    :long-name "load"
    :filter (lambda (x option)
              (declare (ignore option))
              (push (list :load x) *forms*)
              nil)
    :key :load)
   (clingon:make-option
    :counter-filter
    :description "quit lisp here"
    :short-name #\q
    :long-name "quit"
    :filter (lambda (x option)
              (declare (ignore x option))
              (push (list :quit 0) *forms*)
              nil)
    :key :quit)
   (clingon:make-option
    :string
    :description "continue from Lisp image"
    :parameter "IMAGE"
    :short-name #\m
    :long-name "image"
    :key :image)))

(defun sub-commands ()
  )

(defclass run-param ()
  ((impl
    :initarg :impl
    :initform nil
    :accessor run-param-impl)
   (variant
    :initarg :variant
    :initform nil
    :accessor run-param-variant)
   (os
    :initarg :os
    :initform nil
    :accessor run-param-os)
   (arch
    :initarg :arch
    :initform nil
    :accessor run-param-arch)
   (version
    :initarg :version
    :initform nil
    :accessor run-param-version)
   (args
    :initarg :args
    :initform nil
    :accessor run-param-args)
   (image
    :initarg :image
    :initform nil
    :accessor run-param-image)))

(defun impl-path (param)
  (merge-pathnames
   (format nil "impl/~A/~A/~A/~A/~A/"
           (run-param-impl param)
           (run-param-version param)
           (run-param-os param)
           (run-param-arch param)
           (run-param-variant param))
   (app-cachedir)))

(defgeneric run (kind param config)
  (:documentation "run"))

(defgeneric distinguish (impl version)
  (:documentation "decide which kind of impl to be run"))

(defmethod distinguish (kind version)
  "default method for distinguish"
  nil)

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (setf *forms* (nreverse *forms*))
    (message :main-handler "args-for run handler ~S forms:~S name:~S lisp-global: ~S"
             args *forms*
             (clingon:command-name cmd)
             (clingon:getopt cmd :lisp-global))
    (let* ((config (or (load-config :where :local :default nil)
                       (load-config :where :global)))
           (global (clingon:getopt cmd :lisp-global))
           (gsplit (uiop:split-string global :separator '(#\/)))
           (impl  (or (clingon:getopt cmd :lisp)
                      (first gsplit)))
           (version (or (clingon:getopt cmd :version)
                        (second gsplit)
                        (config `(,impl "version") config)))
           (param (make-instance 
                   'run-param
                   :impl impl
                   :variant (or (clingon:getopt cmd :variant)
                                (config `(,impl "variant") config))
                   :os      (or (clingon:getopt cmd :os)      (uname-s))
                   :arch    (or (clingon:getopt cmd :arch)    (uname-m))
                   :version version
                   :args args)))
      (if impl
          (let ((sym (or
                      (distinguish (intern impl :keyword)
                                   (intern version :keyword))
                      (ignore-errors
                        (uiop:safe-read-from-string
                         (uiop:read-file-line
                          (merge-pathnames "roswell.class" (impl-path param))))))))
            (message :main-handler "just before run impl-path:~S sym:~S param:~S"
                     (impl-path param) sym param)
            (when sym
              (run sym param config)))
          (message :main-handler "impl-not found")))
    (uiop:quit)))
