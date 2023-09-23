(uiop:define-package :roswell2.cmd.config/main
  (:use :cl)
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run
        :roswell2.impl.install)
  (:nicknames :roswell2.cmd.config)
  (:import-from :clingon))

(in-package :roswell2.cmd.config/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ()
  (list
   (clingon:make-option
    :boolean/true
    :persistent t
    :description "use user config file"
    :long-name "user"
    :category "Config file location"
    :key :user)
   (clingon:make-option
    :boolean/true
    :persistent t
    :description "use local config file"
    :long-name "local"
    :category "Config file location"
    :key :local)))

(defun sub-commands ()
  (list
   (make-instance
    'roswell2/clingon.extensions::command-without-version
    :name "show"
    :description "show config"
    :handler 'show)))

(defun config-file (cmd)
  (let ((count (count-if (complement #'null)  '(:local :user)
                         :key (lambda (x) (clingon:getopt cmd x)))))
    (when (> count 1)
      (format *error-output* "too much location options~%")
      (return-from config-file nil))
    (load-config :where 
                 (cond ((clingon:getopt cmd :user)
                        :user)
                       ((clingon:getopt cmd :local)
                        :local)
                       (t :user)))))

(defun handler (cmd)
  (multiple-value-bind (config path) 
      (config-file cmd)
    (let ((orig-result (with-output-to-string (o) (cl-toml:encode config o))))
      (loop for arg in (clingon:command-arguments cmd)
            for split = (uiop:split-string arg :separator '(#\=))
            for left = (first split)
            for right = (second split)
            do (and left right
                    (setf (config (uiop:split-string left :separator '(#\.))
                                  config)
                          (if (equal right "")
                              nil
                              right))))
      (let ((new-result (with-output-to-string (o) (cl-toml:encode config o))))
        (unless (equal orig-result new-result)
          (with-open-file (o path :direction :output :if-exists :supersede)
            (format o "~A~%" new-result)))))
    (unless (clingon:command-arguments cmd)
      (clingon:run cmd '("--help")))))

(defun show (cmd)
  (multiple-value-bind (config path) 
      (config-file cmd)
    (message :config-show "config: ~A path: ~A" config path)
    (when path
      (if (uiop:file-exists-p path)
          (with-open-file (in path)
            (format t "~A" (uiop:read-file-string in)))
          (progn
            (format *error-output* "~A not exists.~%" path))))))
