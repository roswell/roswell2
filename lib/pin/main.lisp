(uiop:define-package :roswell2.cmd.pin/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run)
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
         (global (clingon:getopt cmd :lisp-global))
         (gsplit (uiop:split-string global :separator '(#\/)))
         (impl  (clingon.command:command-name cmd))
         (version (or (clingon:getopt cmd :version)
                      (second gsplit)
                      (ignore-errors (config `(,impl "version") gconfig))))
         (variant (or (clingon:getopt cmd :variant)
                      (ignore-errors (config `(,impl "variant") gconfig))))
         (os (or (clingon:getopt cmd :os)
                 (uname-s)))
         (arch (or (clingon:getopt cmd :arch)
                   (uname-m)))
         (args (clingon:command-arguments cmd)))
    (setf (config `("pinned" ,impl "args") config)
          `(,@(when version (list "--version" version))
            ,@(when variant (list "--variant" variant))
            ,@(when os (list "--os" os))
            ,@(when arch (list "--arch" arch))
            ,@(loop for (cmd . val) in (nreverse roswell2.cmd.run:*forms*)
                    append `(,(format nil "--~A" (string-downcase cmd))
                             ,@val))
            "--" ,@args))
    (save-config :config config :where :local)
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
