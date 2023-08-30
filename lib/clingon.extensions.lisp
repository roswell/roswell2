(uiop:define-package :roswell2/clingon.extensions
  (:use :cl
        :roswell-bin/util)
  (:import-from :clingon))
(in-package :roswell2/clingon.extensions)


;;extend options
(defclass option-filter (clingon:option)
  ((filter
    :initarg :filter
    :initform #'identity
    :reader option-filter))
  (:documentation "An option which collects values into a list"))

(defmethod clingon:derive-option-value :around ((option option-filter) arg &key)
  (funcall (option-filter option) (call-next-method) option))

(defmethod clingon:make-option ((kind (eql :option-filter)) &rest rest)
  (apply #'make-instance 'option-filter rest))

(defclass option-counter-filter (clingon:option-counter option-filter)
  ())

(defmethod clingon:make-option ((kind (eql :counter-filter)) &rest rest)
  (apply #'make-instance 'option-counter-filter rest))


;;extend commands

(defclass command-without-version (clingon:command) ())

(defmethod initialize-instance :after ((command command-without-version) &key)
  (setf (clingon:command-options command)
        (remove clingon.command:*default-version-flag*
                (clingon:command-options command)
                :test 'equal)))

(defclass command-accept-slash-sepalated-subcommand (clingon:command)
  ())

(defmethod clingon.command:find-sub-command ((command command-accept-slash-sepalated-subcommand)
                                             name)
  "Returns the sub-command with the given name or alias"
  (let* ((pos (position #\/ name))
         (val (and pos (subseq name (1+ pos))))
         (name (subseq name 0 pos)))
    (find-if (lambda (sub-command)
               (let ((result
                       (or (string= name (clingon:command-name sub-command))
                           (member name (clingon:command-aliases sub-command) :test #'string=))))
                 (when (and pos result)
                   (setf (clingon:command-args-to-parse command)
                         `(,(first (clingon:command-args-to-parse command))
                           "--version"
                           ,val
                           ,@(rest (clingon:command-args-to-parse command)))))
                 result))
             (clingon:command-sub-commands command))))

(defclass stop-parse-when-free-argument-comes-command (clingon:command)
  ())

(defmethod clingon.command:find-sub-command ((command stop-parse-when-free-argument-comes-command)
                                             name)
  "stop parsing if get non option"
  (cond
    ((or (clingon:short-option-p name)
         (clingon:long-option-p name))
     nil)
    ((find-if (lambda (sub-command)
                (or (string= name (clingon.command:command-name sub-command))
                    (member name (clingon.command:command-aliases sub-command) :test #'string=)))
              (clingon.command:command-sub-commands command)))
    (t ;;stop evaluating. if not option.
     (setf (clingon:command-args-to-parse command)
           `("*" ,@(if (equal "--" name) nil (list "--"))
             ,@(clingon:command-args-to-parse command)))
     (make-instance
      'clingon.command:command
      :name name
      :handler (clingon.command:command-handler command)
      :parent command))))

(defclass install-command (command-without-version
                           command-accept-slash-sepalated-subcommand)
  ())

(defclass run-command (command-without-version
                       stop-parse-when-free-argument-comes-command)
  ())
