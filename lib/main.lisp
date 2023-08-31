(uiop:define-package :roswell2/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell2/clingon.extensions)
  (:nicknames :roswell2)
  (:import-from :clingon)
  (:import-from :cl-toml)
  (:export :impl-path
           :impl-param
           :impl-param-name
           :impl-param-variant
           :impl-param-os
           :impl-param-arch
           :impl-param-base-uri
           :impl-param-version
           :impl-param-uri
           :impl-param-archive
           :impl-param-args
           :impl-param-image
           :impl-param-run
           :setup
           :main
           :command
           :sub-command-filter
           :config
           :save-config
           :load-config))

(in-package :roswell2/main)

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
  ((name
    :initarg :name
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
   (version
    :initarg :version
    :initform nil
    :accessor impl-param-version)
   (base-uri
    :initarg :base-uri
    :initform nil
    :accessor impl-param-base-uri)
   (uri
    :initarg :uri
    :initform nil
    :accessor impl-param-uri)
   (archive
    :initarg :archive
    :initform nil
    :accessor impl-param-archive)
   (args
    :initarg :args
    :initform nil
    :accessor impl-param-args)
   (image
    :initarg :image
    :initform nil
    :accessor impl-param-image)
   (run-type
    :initarg :run
    :initform nil
    :accessor impl-param-run)))

(defmethod print-object ((param impl-param) stream)
  (format stream "~S"
          `(,(type-of param)
            ,@(when (impl-param-name param) (list :name (impl-param-name param)))
            ,@(when (impl-param-variant param) (list :variant (impl-param-variant param)))
            ,@(when (impl-param-os param) (list :os (impl-param-os param)))
            ,@(when (impl-param-arch param) (list :arch (impl-param-arch param)))
            ,@(when (impl-param-version param) (list :version (impl-param-version param)))
            ,@(when (impl-param-base-uri param) (list :base-uri (impl-param-base-uri param)))
            ,@(when (impl-param-uri param) (list :uri (impl-param-uri param)))
            ,@(when (impl-param-archive param) (list :archive (impl-param-archive param)))
            ,@(when (impl-param-args param) (list :args (impl-param-args param)))
            ,@(when (impl-param-image param) (list :image (impl-param-image param)))
            ,@(when (impl-param-run param) (list :run (impl-param-run param))))))

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
  "Returns the options for the  command"
  (list
   (clingon:make-option
    :option-filter
    :description "evaluate form while building"
    :parameter "FORM"
    :short-name #\e
    :long-name "eval"
    :filter (lambda (x option)
              (declare (ignore option))
              (eval (read-from-string x)) x)
    :key :eval)
   (clingon:make-option
    :option-filter
    :description "load lisp FILE while building"
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
  "Handler for just evaluate options"
  (message :main-handler "args for root handler ~S" (clingon:command-arguments cmd))
  (unless (clingon:command-arguments cmd)
    (clingon:run cmd '("--help")))
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
  (let ((command (command "roswell2" :name (pathname-name *stage1-path*))))
    (clingon:run command
                 (cdr (uiop/image:raw-command-line-arguments)))))

(defun toml-path (where)
  (let ((global-directory (app-cachedir))
        (local-directory (uiop:getcwd)))
    (case where
      (:local (merge-pathnames ".roswell-config.toml" local-directory))
      (:global (merge-pathnames "config.toml" global-directory))
      (t (error "invalid where eparameter for config")))))

(defun load-config (&key
                    (where :local)
                    (default (make-hash-table :test 'equal)))
  (let ((path (toml-path where)))
    (if (uiop:file-exists-p path)
        (or (ignore-errors (cl-toml:parse-file path))
            (message :impl-set-config "broken ~A" path))
        default)))

(defun save-config (&key 
                    config
                    (where :local))
  (let ((path (toml-path where)))
    (message :save-config "save-config path:~S" path)
    (with-open-file (o path :direction :output
                       :if-exists :supersede)
      (cl-toml:encode config o))))

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
                      debug)
  (let ((hash (rhash keys config :result :hash :if-does-not-exist :create :debug debug)))
    (setf (gethash (first (last keys)) hash) val)))

#+nil
(let ((config (load-config :where :global)))
  (setf (config '("sbcl" "variant") config) "hoge")
  (setf (config '("sbcl" "version") config) "hoge2")
  
  (list (config '("sbcl" "variant") config)
        (config '("sbcl" "version") config)
        (with-output-to-string (o) (cl-toml:encode config o)))
  
  )
