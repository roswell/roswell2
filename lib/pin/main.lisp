(uiop:define-package :roswell2.cmd.pin/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run
        :roswell2.cmd.install/main)
  (:nicknames :roswell2.cmd.pin)
  (:import-from :clingon))

(in-package :roswell2.cmd.pin/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ())

(defun sub-handler (cmd)
  (message :pin-sub-hanlder  "sub-handler for pin ~S" cmd)
  (message :pin-sub-hanlder  "args ~S" (clingon:command-arguments cmd))
  (message :pin-sub-hanlder  "forms ~S" roswell2.cmd.run:*forms*)
  (let* ((gconfig (load-config :where :global))
         (config (load-config :where :local))
         (impl  (clingon.command:command-name cmd))
         (version (or (clingon:getopt cmd :version)
                      (and impl (config `(,impl "version") gconfig :if-does-not-exist nil))))
         (args (clingon:command-arguments cmd))
         (forms (reverse roswell2.cmd.run:*forms*))
         (param (make-impl-param
                 (intern (string-upcase impl) :keyword)
                 cmd
                 :name impl
                 :version version
                 :args args
                 :forms forms)))
    (unless version
      (impl-set-version-param param))
    (let ((path (merge-pathnames "roswell.sexp" (impl-path param))))
      (setf param (if (uiop:file-exists-p path)
                      (uiop:safe-read-file-form path)
                      (uiop:safe-read-from-string (format nil "~S" param)))))
    (setf (getf param :args) args
          (getf param :forms) (loop for (opt . val) in forms
                                    append `(,(format nil "--~A" (string-downcase opt))
                                             ,@val)))
    (message :pin-sub-hanlder "pin param:~S" param)
    (setf (config `("pinned" ,impl) config)
          (format nil "~S" param))
    (save-config :config config :where :local)
    (setf roswell2.cmd.run:*forms* nil)
    (let ((run (roswell2:command :roswell2.cmd.run)))
      (clingon:run run `(,impl "-e" "(format t \"~A ~A pinned~%\" (lisp-implementation-type) (lisp-implementation-version))")))))

(defun sub-options ()
  (loop with package = (find-package :roswell2.cmd.run)
        for i in (funcall (find-symbol (string '#:options) package))
        unless (or (member (clingon.options:option-key i)
                           '(:quit :image :lisp :repl :dump))
                   (member i clingon.command:*default-options*))
        collect i))

(defun sub-commands ()
  ;; copy list of ros run's implementations.
  (loop for base in (remove-if-not
                     (roswell2/main::string-start-with-filter "roswell2.run.")
                     (asdf:registered-systems))
        for system = (asdf:find-system base)
        for name = (asdf:system-long-name system)
        when name
        collect 
           (make-instance
            'roswell2/clingon.extensions::command-without-version
            :name name
            :description (format nil "set implementation parameter for 'ros run ~A'" name)
            :version (asdf:component-version system)
            :authors `(,(asdf:system-author system))
            :license (asdf:system-licence system)
            :options (sub-options)
            :handler 'sub-handler
            :sub-commands nil)))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((args (clingon:command-arguments cmd)))
    (message :pin-handler "args-for pin handler ~S" args))
  (uiop:quit))
