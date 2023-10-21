(uiop:define-package :roswell-bin/build
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell-bin/uname
        :roswell-bin/install-quicklisp)
  (:export :build))
(in-package :roswell-bin/build)

(defun build (args &key (cache-path (app-cachedir))
                        (config-path (app-configdir))
                        force)
  (message :build "build stage2 args => ~A path => ~A" args cache-path)
  (ensure-directories-exist config-path)
  (install-quicklisp :path cache-path :ql-path "quicklisp/")
  (message :build "build install quicklisp ~S done" config-path)
  (let ((asds-path (libdir))
        (core-path (core-path cache-path)))
    (if (or (not (uiop:file-exists-p (ensure-directories-exist core-path)))
            force)
      (let ((invoke-list (list *stage1-path*
                               "--eval" (format nil "(setf roswell-bin/util::*message-first-inovocation* ~A roswell-bin/util:*verbose* ~A)"
                                                roswell-bin/util::*message-first-inovocation*
                                                roswell-bin/util:*verbose*)
                               "--load" (uiop:native-namestring (merge-pathnames "quicklisp/setup.lisp" cache-path))
                               "--eval" (format nil "(mapc (lambda (x) (asdf:load-asd x)) (directory \"~A*.asd\"))" asds-path)
                               "--eval" "(let ((*standard-output* *error-output*)(*trace-output* *error-output*)) (ql:quickload :roswell2))"
                               "--eval" (format nil "(let ((*standard-output* *error-output*)(*trace-output* *error-output*)) (roswell2/main:setup ~S ~S ~S))" config-path core-path asds-path))))
        (message :build "build stage2 with ~S" invoke-list)
        (uiop:run-program invoke-list
                          :output :interactive
                          :error-output :interactive))
      (message :build "build stage2 core found ~S" core-path))
    (uiop:file-exists-p core-path)))
