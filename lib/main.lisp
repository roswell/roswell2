(uiop:define-package :roswell2/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/clingon.extensions
        )
  (:nicknames :roswell2)
  (:import-from :clingon)
  (:import-from :cl-toml)
  (:export :impl-path
           :impl-archive-path
           :impl-param-variant*
           :impl-param-arch*
           :impl-param-os*
           :impl-param-class
           :make-impl-param
           :impl-set-run-param
           :impl-param
           :impl-param-kind
           :impl-param-name
           :impl-param-variant
           :impl-param-os
           :impl-param-arch
           :impl-param-base-uri
           :impl-param-version
           :impl-param-uri
           :impl-param-archive
           :impl-param-args
           :impl-param-native
           :impl-param-image
           :impl-param-quicklisp
           :impl-param-run
           :impl-param-forms
           :impl-param-wrap
           :setup
           :main
           :command
           :sub-command-filter
           :config
           :config-to-string
           :save-config
           :load-config))

(in-package :roswell2/main)

(defvar *command-class* 'roswell2/clingon.extensions::root-command)

(defclass impl-param ()
  ((kind
    :initarg :kind
    :initform nil
    :reader impl-param-kind)
   (name
    :initarg :name
    :initform nil
    :reader impl-param-name)
   (variant
    :initarg :variant
    :initform nil
    :reader impl-param-variant)
   (os
    :initarg :os
    :initform nil
    :reader impl-param-os)
   (arch
    :initarg :arch
    :initform nil
    :reader impl-param-arch)
   (version
    :initarg :version
    :initform nil
    :accessor impl-param-version)
   (base-uri
    :initarg :base-uri
    :initform nil
    :reader impl-param-base-uri)
   (uri
    :initarg :uri
    :initform nil
    :reader impl-param-uri)
   (archive
    :initarg :archive
    :initform nil
    :reader impl-param-archive)
   (args
    :initarg :args
    :initform nil
    :reader impl-param-args)
   (native
    :initarg :native
    :initform nil
    :reader impl-param-native)
   (quicklisp
    :initarg :quicklisp
    :initform nil
    :reader impl-param-quicklisp)
   (image
    :initarg :image
    :initform nil
    :reader impl-param-image)
   (run
    :initarg :run
    :initform nil
    :accessor impl-param-run)
   (forms
    :initarg :forms
    :initform nil
    :accessor impl-param-forms)
   (wrap
    :initarg :wrap
    :initform nil
    :accessor impl-param-wrap)))

(defmethod impl-path ((param impl-param))
  ;; "~/.cache/roswell/impl/sbcl/2.3.7/x86-64/linux/bin/"
  (merge-pathnames
   (format nil "impl/~A/~A/~A/~A/~A/"
           (impl-param-name param)
           (impl-param-version param)
           (impl-param-os* param)
           (impl-param-arch* param)
           (impl-param-variant* param))
   (app-cachedir)))

(defmethod impl-archive-path ((param impl-param))
  (or (and (impl-param-archive param)
           (uiop:file-exists-p (impl-param-archive param)))
      (ensure-directories-exist
       (merge-pathnames (format nil "archives/~A"
                                (concatenate 'string
                                             (impl-param-name param) ;; "sbcl"
                                             "-"
                                             (impl-param-version param) ;;"2.3.7"
                                             "-"
                                             (impl-param-arch* param) ;;"x86-64"
                                             "-"
                                             (impl-param-os* param) ;;"linux"
                                             "-"
                                             (impl-param-variant* param)
                                             "-binary.tar.bz2"))
                        (app-cachedir)))))

(defmethod impl-param-class (kind)
  (declare (ignorable kind))
  'impl-param)

(defmethod impl-set-run-param ((param impl-param)))

(defun make-impl-param (kind &key
                             cmd
                             name
                             args
                             version
                             run
                             forms
                             (image nil image-p)
                             (quicklisp nil quicklisp-p))
  (let* ((class (impl-param-class kind))
         (listp (listp cmd))
         (impl (flet ((elm (id)
                        (if listp
                            (getf cmd id)
                            (clingon:getopt cmd id))))
                 (make-instance
                  class
                  :kind kind
                  :name (or name (elm :lisp))
                  :variant (elm :variant)
                  :os      (elm :os)
                  :arch    (elm :arch)
                  :version (or version (elm :version))
                  :args args
                  :uri     (elm :uri)
                  :base-uri(elm :base-uri)
                  :native  (elm :native)
                  :wrap    (elm :wrap)
                  :quicklisp (if quicklisp-p
                                 quicklisp
                                 (or
                                  (and (elm :quicklisp-path)
                                       (or (uiop:directory-exists-p
                                            (ensure-directories-exist
                                             (pathname-directory (elm :quicklisp-path))))
                                           (message :make-impl-param "~S is not taken as quicklisp directory"
                                                    (elm :quicklisp-path))))
                                  (elm :quicklisp)))
                  :image (if image-p image (elm :image))
                  :forms forms
                  :run run))))
    (unless (impl-param-run impl)
      (impl-set-run-param impl))
    impl))

(defmethod print-object ((param impl-param) stream)
  (format stream "~S"
          (loop for c in (sb-mop:class-slots (class-of param))
                for val = (slot-value param (sb-mop:slot-definition-name c))
                when val
                append (list (first (sb-mop:slot-definition-initargs c))  val))))

(defmethod impl-param-variant* ((param impl-param))
  (let* ((impl (impl-param-name param))
         (variant (impl-param-variant param))
         (config (load-config :where :user))
         (default (ignore-errors (symbol-value (read-from-string
                                                (format nil "roswell2.install.~A:*default-variant*" impl))))))
    (or (and (equal variant "") default)
        variant
        (config `(,impl "variant") config :if-does-not-exist nil)
        default)))

(defmethod impl-param-arch* ((param impl-param))
  (or (impl-param-arch param)
      (uname-m)))

(defmethod impl-param-os* ((param impl-param))
  (or (impl-param-os param)
      (uname-s)))

(defun string-start-with-filter (str)
  (let ((len (length str)))
    (lambda (x)
      (and 
       (> (length x)
          len) 
       (string-equal x str :end1 len)))))

(defun setup (path core-path asds-path)
  ""
  (message :setup "roswell2 setup invoked. :stage-1 ~S~%" *stage1-path*)
  (message :setup "path: ~A core-path: ~A asds-path: ~A" path core-path asds-path)
  (mapc (lambda (x)
          (message :setup "load-asd[~A]" x)
          (asdf:load-asd x))
        (directory (format nil "~A*/**/*.asd" asds-path)))
  (setf uiop:*image-entry-point* (uiop:ensure-function "roswell2/main:main"))
  (loop for system-name in
           (remove-if-not
            (string-start-with-filter "roswell2.")
            (asdf:registered-systems))
        for system = (asdf:find-system system-name)
        do (message :setup "load-system[~A]" system-name)
           (ql:quickload system-name))
  (ql:quickload :roswell2/config.default)
  ;; tbd load system from dists.
  (let ((config-lisp (merge-pathnames "config.lisp" path)))
    (when (uiop:file-exists-p config-lisp)
      (message :setup "load[~A]" config-lisp)
      (ignore-errors (load config-lisp))))
  (message :setup "dump stage2")
  (setf *verbose* 0)
  (setf *stage2-commit*
        (with-open-file (in (merge-pathnames "commit" (ql:where-is-system "roswell2")))
          (read-line in)))
  (uiop:dump-image core-path :executable t))

(defvar *sub-command-filter* (make-hash-table :test 'equal))

(defun sub-command-filter (prefix)
  (cdr 
   (or (gethash prefix *sub-command-filter*)
       (let ((sub-commands
               (loop for system-name in (remove-if-not
                                         (string-start-with-filter prefix)
                                         (asdf:registered-systems))
                     for command = (command system-name)
                     do (message :sub-commands "sub command candidate for ~S  ~S ~A" prefix system-name command)
                     when command
                     collect command)))
         (setf (gethash prefix *sub-command-filter*) (cons t sub-commands))))))

(defun sub-commands ()
  (let ((command (sub-command-filter "roswell2.cmd.")))
    (message :sub-commands-main "command: ~S" command)
    command))

(defun options ()
  "Returns the options for the toplevel command"
  (list
   (clingon:make-option
    :option-filter
    :description "evaluate form for stage1"
    :parameter "FORM"
    :short-name #\e
    :long-name "eval"
    :filter (lambda (x option)
              (declare (ignore option))
              (eval (read-from-string x)) x)
    :key :eval)
   (clingon:make-option
    :option-filter
    :description "load lisp FILE for stage1"
    :parameter "FILE"
    :short-name #\l
    :long-name "load"
    :filter (lambda (x option)
              (declare (ignore option))
              ;; tbd
              (print (list :load x)) x)
    :key :load)
   (clingon:make-option
    :counter-filter
    :short-name #\v
    :long-name "verbose"
    :filter (lambda (x option)
              (declare (ignore option))
              (message :counter-filter "verbose level: ~A" x)
              (setf *verbose* x))
    :description "be quite noisy"
    :key :verbose)))

(defun handler (cmd)
  (message :main-handler "args for root handler ~S" (clingon:command-arguments cmd))
  (unless (clingon:command-arguments cmd)
    (clingon:run cmd '("--help")))
  (let ((args (clingon:command-arguments cmd)))
    (if (ignore-errors
          (uiop:file-exists-p (first args)))
        (clingon:run cmd`("script" "run" ,@args))))
  (uiop:quit))

(defun command (system &key
                       name)
  (let* ((package (or (find-package system)
                      (find-package (string-upcase system))))
         (system (asdf:find-system system))
         (name (or name (asdf:system-long-name system)))
         (class (or (let ((it (find-symbol (string '#:*command-class*) package)))
                      (and it 
                           (symbol-value it)))
                    'clingon.command:command)))
    (message :command "command: system:~S class:~S" system class)
    (when (and system
               name)
      (make-instance
       class
       :name name
       :description (asdf:system-description system)
       :version (asdf:component-version system)
       :authors `(,(asdf:system-author system))
       :license (asdf:system-licence system)
       :options (funcall (find-symbol (string '#:options) package))
       :handler (find-symbol (string '#:handler) package)
       :sub-commands (let ((it (find-symbol (string '#:sub-commands) package)))
                       (and it (funcall it)))))))

(defun main ()
  (setf *stage2-path* (first (uiop/image:raw-command-line-arguments)))
  (let* ((command (command "roswell2" :name (pathname-name *stage1-path*)))
         (args (cdr (uiop/image:raw-command-line-arguments))))
    (clingon:run command args)))

(defun toml-path (where)
  (let ((user-directory (app-configdir))
        (local-directory (uiop:getcwd)))
    (case where
      (:local (merge-pathnames ".roswell-config.toml" local-directory))
      (:user (merge-pathnames "config.toml" user-directory))
      (t (error "invalid where eparameter for config")))))

(defun load-config (&key
                    (where :local)
                    (default (make-hash-table :test 'equal)))
  (let ((path (toml-path where)))
    (values (if (uiop:file-exists-p path)
                (or (ignore-errors (cl-toml:parse-file path))
                    (message :impl-set-config "broken ~A" path))
                default)
            path)))

(defun save-config (&key 
                    config
                    (where :local))
  (let ((path (toml-path where)))
    (message :save-config "save-config path:~S" path)
    (with-open-file (o path :direction :output
                       :if-exists :supersede)
      (cl-toml:encode config o))))

(defun config-to-string (config)
  (with-output-to-string (o)
    (cl-toml:encode config o)))

(defun rhash (keys hash &key (result :value)
                             (if-does-not-exist :error)
                             (depth 0)
                             (debug))
  "Traverse hashtable"
  (when debug
    (format t "~S~%" (list 
                      :keys keys
                      :hash hash
                      :result result
                      :if-does-not-exist if-does-not-exist
                      :depth depth)))
  (let ((value (gethash (first keys) hash)))
    (if (rest keys)
        (if (hash-table-p value)
            (rhash (rest keys) value :result result :if-does-not-exist if-does-not-exist
                   :depth (1+ depth))
            (case if-does-not-exist
              ((nil) nil)
              (:error (error "rhash no key :~S" keys))
              (:create
               (setf value (make-hash-table :test 'equal)
                     (gethash (first keys) hash) value)
               (rhash (rest keys) value :result result :if-does-not-exist if-does-not-exist
                      :depth (1+ depth)))
              (t (error "invalid value for rhash if-does-not-exist"))))
        (case result
          (:value value)
          (:hash hash)
          (t result)))))

(defun config (keys
               config
               &key
               (result :value)
               (if-does-not-exist :error)
               debug)
  (when (and keys
             (hash-table-p config))
    (rhash keys config
           :result result
           :if-does-not-exist if-does-not-exist
           :debug debug)))

(defun (setf config) (val
                      keys
                      config
                      &key
                      if-does-not-exist
                      debug)
  (let ((hash (rhash keys config :result :hash :if-does-not-exist :create :debug debug)))
    (if val
        (setf (gethash (first (last keys)) hash) val)
        (progn
          (remhash (first (last keys)) hash)
          nil))))

#+nil
(let ((config (load-config :where :user)))
  (setf (config '("sbcl" "variant") config) "hoge")
  (setf (config '("sbcl" "version") config) "hoge2")
  
  (list (config '("sbcl" "variant") config)
        (config '("sbcl" "version") config)
        (config-to-string config)))
