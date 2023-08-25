(uiop:define-package :roswell2/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell2/clingon.extensions)
  (:nicknames :roswell2)
  (:import-from :clingon)
  (:import-from :cl-toml)
  (:export :setup
           :main
           :command
           :sub-command-filter
           :config
           :save-config
           :load-config))
(in-package :roswell2/main)

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
    :string
    :description "Run roswell with a lisp impl NAME[/VERSION]."
    :parameter "NAME[/VERSION]"
    :short-name #\L
    :long-name "lisp"
    :key :lisp-global)
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

(defun load-config (&key
                    (where :local)
                    (global-directory (app-cachedir))
                    (local-directory (uiop:getcwd))
                    (default (make-hash-table :test 'equal)))
  (let ((path (case where
                (:local (merge-pathnames ".roswell/config.toml" local-directory))
                (:global (merge-pathnames "config.toml" global-directory))
                (t (error "invalid where eparameter for config")))))
    (if (uiop:file-exists-p path)
        (or (ignore-errors (cl-toml:parse-file path))
            (message :impl-set-config "broken ~A" path))
        default)))


(defun save-config (&key 
                    config
                    (where :local)
                    (global-directory (app-cachedir))
                    (local-directory (uiop:getcwd)))
  (let ((path (case where
                (:local (merge-pathnames ".roswell/config.toml" local-directory))
                (:global (merge-pathnames "config.toml" global-directory))
                (t (error "invalid where parameter for config")))))
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
