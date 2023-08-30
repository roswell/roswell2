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
           ))

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
    :string
    :description "continue from Lisp image"
    :parameter "IMAGE"
    :short-name #\m
    :long-name "image"
    :key :image)
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
    :option-filter
    :description "quit lisp here"
    :short-name #\q
    :long-name "quit"
    :filter (lambda (x option)
              (declare (ignore x option))
              (push (list :quit) *forms*)
              nil)
    :key :quit)
   (clingon:make-option
    :option-filter
    :description "run repl after option processing"
    :long-name "repl"
    :filter (lambda (x option)
              (declare (ignore x option))
              (push (list :repl) *forms*)
              nil)
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
    :key :dump)))

(defvar *config* nil)

(defun sub-handler (cmd)
  (let* ((name (clingon.command:command-name cmd))
         (run (roswell2:command :roswell2.cmd.run
                                :name name)))
    (message :sub-handler "sub-handler ~A config:~S cmd:~S forms:~S"
             (clingon:command-name cmd)
             (config `("pinned" ,name "args")
                     *config*)
             cmd
             *forms*)
    (let* ((list (coerce (config `("pinned" ,name "args")
                                 *config*) 'list))
           (pos (position "--" list :test 'equal))
           (first (subseq list 0 pos))
           (last (subseq list (1+ pos)))
           (mid (loop for (opt . val) in (nreverse *forms*)
                      append `(,(format nil "--~A" (string-downcase opt))
                               ,@val)))
           (*forms* nil))
      (clingon:run run `("-L" ,name
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
                                                      '(:lisp :arch :variant :os :version :image))
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
    (message :main-handler "args-for run handler ~S forms:~S name:~S lisp-global: ~S"
             args *forms*
             (clingon:command-name cmd)
             (clingon:getopt cmd :lisp-global))
    (let* ((config (load-config :where :global))
           (global (clingon:getopt cmd :lisp-global))
           (gsplit (uiop:split-string global :separator '(#\/)))
           (impl  (or (clingon:getopt cmd :lisp)
                      (first gsplit)))
           (version (or (clingon:getopt cmd :version)
                        (second gsplit)
                        (and impl (config `(,impl "version") config))))
           (variant (or (clingon:getopt cmd :variant)
                        (and impl
                             (or
                              (config `(,impl "variant") config :if-does-not-exist nil)
                              (symbol-value (read-from-string
                                             (format nil "roswell2.install.~A:*default-variant*" impl)))))))
           (param (make-instance
                   'impl-param
                   :impl impl
                   :variant variant
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
            (if sym
              (run sym param config cmd)
              ))
          (clingon:run cmd '("--help"))))
    (uiop:quit)))
