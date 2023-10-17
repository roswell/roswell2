(uiop:define-package :roswell2.cmd.version/main
  (:use :cl
        :roswell-bin/config
        :roswell-bin/download
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2/main
        :roswell2.cmd.run)
  (:nicknames :roswell2.cmd.version)
  (:import-from :clingon))

(in-package :roswell2.cmd.version/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun options ()
  `(    
    ,(clingon:make-option
      :string
      :description "Format the output"
      :parameter "string"
      :short-name #\f
      :long-name "format"
      :key :format)))

(defun version-info ()
  (let ((info (make-hash-table :test 'equal)))
    (setf (config (list "roswell2" "version") info)
          (asdf:component-version (asdf:find-system "roswell2"))
          (config (list "roswell2" "path") info)
          (namestring (ql:where-is-system "roswell2"))
          (config (list "roswell2" "stage1-bin") info) (namestring *stage1-path*)
          (config (list "roswell2" "stage1-commit") info) *stage1-commit*
          (config (list "roswell2" "stage2-bin") info) (namestring *stage2-path*)
          (config (list "roswell2" "stage2-commit") info) *stage2-commit*
          (config (list "sbcl" "version") info) (lisp-implementation-version)
          (config (list "sbcl" "variant") info) roswell2.install.sbcl:*default-variant*
          (config (list "quicklisp" "client") info)
          (handler-bind
              ((simple-warning #'muffle-warning))
            (ql:client-version))
          (config (list "quicklisp" "dist") info) (ql:dist-version "quicklisp"))
    (loop for elt in (sort (remove-if (lambda (x)
                                        (or (find #\/ x)
                                            (find #\. x)))
                                      (asdf:registered-systems)) #'string<)
          do (setf (config (list "registered-systems" elt) info)
                   (asdf:component-version (asdf:find-system elt))))
    #+nil
    (setf (config (list "sbcl" "features") info)
          (let* ((pos (position :package-local-nicknames *features* :test 'equal)))
            (nreverse (loop for i in (subseq *features* (1+ pos))
                            collect (format nil "~(~A~)" i)))))
    (loop for (lib sublib version) in (lib-info)
          do (setf (config (list lib sublib) info) version))
    info))

(defun handler (cmd)
  "Handler for just evaluate options"
  (format t "~A" (config-to-string (version-info)))
  (uiop:quit))
