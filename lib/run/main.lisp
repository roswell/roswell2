(uiop:define-package :roswell2.cmd.run/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.install/main
        )
  (:nicknames :roswell2.cmd.run)
  (:import-from :clingon)
  (:export :run 
           :distinguish
           :*forms* :impl-path))

(in-package :roswell2.cmd.run/main)

(defvar *forms* nil)
(defvar *command-class* 'roswell2/clingon.extensions::run-command)

(defun options ()
  "Returns the options for run command"
  (list
   (clingon:make-option
    :string
    :description "designate lisp impl to run."
    :parameter "IMPL"
    :short-name #\L
    :long-name "lisp"
    :category "Implementation designation options"
    :key :lisp)
   (clingon:make-option
    :string
    :description (format nil "set arch. defualt:~A" (uname-m))
    :parameter "ARCH"
    :long-name "arch"
    :category "Implementation designation options"
    :key :arch)
   (clingon:make-option
    :string
    :description "set variant"
    :parameter "VARIANT"
    :long-name "variant"
    :category "Implementation designation options"
    :key :variant)
   (clingon:make-option
    :string
    :description (format nil "set os. default:~A" (uname-s))
    :parameter "OS"
    :long-name "os"
    :category "Implementation designation options"
    :key :os)
   (clingon:make-option
    :string
    :description "set version"
    :parameter "VERSION"
    :long-name "version"
    :category "Implementation designation options"
    :key :version)
   (clingon:make-option
    :string
    :description "Run lisp with Quicklisp which home set to PATH"
    :parameter "PATH"
    :long-name "qlpath"
    :category "Quicklisp"
    :key :quicklisp-path)
   (clingon:make-option
    :boolean/true
    :description (format nil "Run lisp with Quicklisp home=~S" (ql:qmerge ""))
    :short-name #\Q
    :long-name "quicklisp"
    :category "Quicklisp"
    :key :quicklisp)
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
    :category "Runtime options"
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
    :category "Runtime options"
    :key :load)
   (clingon:make-option
    :option-filter
    :description "quit lisp here"
    :short-name #\q
    :long-name "quit"
    :filter (lambda (x option)
              (declare (ignore x option))
              (push (list :quit) *forms*)
              nil)
    :category "Runtime options"
    :key :quit)
   (clingon:make-option
    :option-filter
    :description "run repl after option processing"
    :long-name "repl"
    :filter (lambda (x option)
              (declare (ignore x option))
              (push (list :repl) *forms*)
              nil)
    :category "Runtime options"
    :key :repl)
   (clingon:make-option
    :option-filter
    :description "dump image after option processing"
    :parameter "FILE"
    :long-name "dump"
    :filter (lambda (x option)
              (declare (ignore option))
              (push (list :dump x) *forms*)
              nil)
    :category "Runtime options"
    :key :dump)
   (clingon:make-option
    :boolean/true
    :description "run lisp implementation without runtime option processing"
    :long-name "native"
    :category "Runtime options"
    :key :native)
   (clingon:make-option
    :string
    :description "continue from Lisp image"
    :parameter "IMAGE"
    :short-name #\m
    :long-name "image"
    :category "Runtime options"
    :key :image)))

(defvar *config* nil)

(defun sub-handler (cmd)
  (let* ((name (clingon.command:command-name cmd))
         (run (roswell2:command :roswell2.cmd.run
                                :name name)))
    (message :sub-handler "run sub-handler ~A config:~S cmd:~S forms:~S"
             (clingon:command-name cmd)
             (config `("pinned" ,name) *config*)
             cmd
             *forms*)
    (let* ((list (uiop:safe-read-from-string (config `("pinned" ,name) *config*)))
           (first (getf list :forms))
           (last (getf list :args))
           (mid (loop for (opt . val) in (nreverse *forms*)
                      append `(,(format nil "--~A" (string-downcase opt))
                               ,@val)))
           (*forms* nil))
      (clingon:run run `("-L" ,name
                         ,@(when (getf list :version)
                             (list "--version" (getf list :version)))
                         ,@(when (getf list :variant)
                             (list "--variant" (getf list :variant)))
                         ,@(when (getf list :os)
                             (list "--os" (getf list :os)))
                         ,@(when (getf list :arch)
                             (list "--arch" (getf list :arch)))
                         ,@(when (getf list :image)
                             (list "--image" (getf list :image)))
                         ,@first
                         ,@(or mid '("--repl"))
                         "--" ,@last)))))

(defun sub-commands ()
  (setf *config* (load-config :where :local))
  (let (result
        (hash (config '("pinned") *config*)))
    (message :run-sub-commands "run sub-commands: ~S"
             hash)
    (when hash
      (maphash (lambda (name y)
                 (push (make-instance
                        'roswell2/clingon.extensions::command-without-version
                        :name name
                        :description (format nil "launch ~A" name)
                        :options (loop for i in (options)
                                       unless (member (clingon.options:option-key i)
                                                      '(:lisp :arch :variant :os :version))
                                       collect i)
                        :handler 'sub-handler)
                       result))
               hash))
    result))

(defgeneric run (kind param config cmd)
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
    (message :main-handler "args-for run handler ~S forms:~S name:~S"
             args *forms*
             (clingon:command-name cmd))
    (let* ((config (load-config :where :global))
           (impl  (clingon:getopt cmd :lisp))
           (version (or (clingon:getopt cmd :version)
                        (and impl (config `(,impl "version") config :if-does-not-exist nil))))
           (param (make-impl-param
                   (intern (string-upcase impl) :keyword)
                   cmd
                   :version version
                   :args args)))
      (unless impl
        (clingon:run cmd '("--help")))
      (unless version
        (impl-set-version-param param))
      (message :main-handler "build param: ~S" param)
      (let ((sym (or
                  (distinguish (and impl (intern impl :keyword))
                               (and version (intern version :keyword)))
                  (ignore-errors
                    (let* ((path (merge-pathnames "roswell.sexp" (impl-path param)))
                           form)
                      (unless (uiop:file-exists-p path)
                        (message :main-handler "~S seems not exist... try install: ~S" path param)
                        (install param))
                      (setf form (uiop:read-file-form path))
                      (message :main-handler "read roswell.sexp: ~S" form)
                      (uiop:safe-read-from-string (getf form :run)))))))
        (message :main-handler "just before run impl-path:~S sym:~S param:~S"
                 (impl-path param) sym param)
        (if sym
            (run sym param config cmd))))
    (uiop:quit)))
