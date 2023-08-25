(uiop:define-package :roswell2.run.sbcl/main
  (:use :cl
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2.cmd.run/main)
  (:nicknames :roswell2.run.sbcl))
(in-package :roswell2.run.sbcl/main)

(defun init.lisp-path ()
  (merge-pathnames 
   "init.lisp"
   (asdf:system-source-directory
    (asdf:find-system :roswell2.run.sbcl))))

(defmethod run ((kind (eql :roswell2.sbcl))
                param config)
  "run sbcl installed on cachedir"
  (message :run "check params ~S"
           (list :param param
                 :config config
                 :args (run-param-args param)
                 ))
  (let* ((impl-path (impl-path param))
         (args (run-param-args param))
         (sbcl-home (merge-pathnames "lib/sbcl/" impl-path))
         (pos (position "--" args :test 'equal))
         (runtime-options (when pos (subseq args (1+ pos))))
         (args (if pos (subseq args 0 pos) args))
         ret
         help
         (image (run-param-image param)))
    (setf (uiop:getenv "SBCL_HOME") (uiop:native-namestring sbcl-home))
    (push (uiop:native-namestring (merge-pathnames (format nil "bin/sbcl~A" (exeext)) impl-path)) ret)
    (loop while runtime-options
          do (push (pop runtime-options) ret))
    
    (push "--core" ret)
    (push (if image
              image
              (uiop:native-namestring (merge-pathnames "sbcl.core" sbcl-home)))
          ret)
    (when (zerop *verbose*)
      (push "--noinform" ret))
    (push "--no-sysinit" ret)
    (push "--no-userinit" ret)
    (push "--eval" ret)
    (push (format nil "(progn #-roswell2.run.sbcl/init (cl:load ~S))" (init.lisp-path)) ret)
    (push "--eval" ret)
    (push (format nil "(roswell2.run.sbcl/init:main '~S)"
                  (append `((:eval ,(format nil "(setf roswell.init:*impl-path* ~S)" impl-path)))
                          roswell2.cmd.run:*forms*))
          ret)
    (setf ret (nreverse ret))
    (message :run-sbcl "ret:~S" ret)
    (exec ret)))
;;; https://thinca.hatenablog.com/entry/20100210/1265813598
