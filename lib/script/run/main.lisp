(uiop:define-package :roswell2.script.run/main
  (:use :cl
        :roswell2/main
        :roswell-bin/util
        :roswell2.cmd.script
        :roswell2.cmd.run)
  (:nicknames :roswell2.script.run))
(in-package :roswell2.script.run/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)

(defun sub-commands ())

(defun options ()
  `(,@(loop with package = (find-package :roswell2.cmd.run)
            for i in (funcall (find-symbol (string '#:options) package))
            unless (or (member (clingon.options:option-key i)
                               '(:quit :image :lisp :repl :dump :native :quicklisp-path :quicklisp))
                       (member i clingon.command:*default-options*))
            collect i)
    ,(clingon:make-option
      :boolean/true
      :description "use quicklisp"
      :short-name #\Q
      :long-name "quicklisp"
      :category "Quicklisp"
      :key :quicklisp)
    ,(clingon:make-option
      :string
      :description "Take image name it will be ignored"
      :parameter "IMAGE"
      :short-name #\m
      :long-name "image"
      :category "dummy options"
      :key :image)
    ,(clingon:make-option
      :string
      :description "Take impl name it will be ignored for now."
      :parameter "lisp"
      :short-name #\L
      :long-name "lisp"
      :category "dummy options"
      :key :lisp)
    ,(clingon:make-option
      :counter-filter
      :short-name #\v
      :long-name "verbose"
      :hidden nil
      :filter (lambda (x option)
                (declare (ignore option))
                (message :counter-filter "verbose level: ~A" x)
                (setf *verbose* x))
      :description "be quite noisy"
      :key :verbose)))

(defun parse-script (file)
  (let (md5sum package seq pos pos2)
    (with-open-file (in file)
      (read-line in);; read shebang
      (setf pos (file-position in))
      (message :script-handler "ignore shebang pos:~S" pos)
      (with-standard-io-syntax
        (let ((*read-suppress* t))
          (read in)))
      (setf pos2 (file-position in))
      (message :script-handler "pos2:~S" pos2)
      (setf seq (make-string (- pos2 pos)))
      (file-position in pos)
      (read-sequence seq in)
      (message :script-handler "seq:~S" seq)
      (setf md5sum (format nil "~(~{~2,'0X~}~)"
                           (coerce (sb-md5:md5sum-string seq) 'list)))
      (loop with eof = '#:eof
            for read = (read in nil eof)
            for is-package = (ignore-errors (string-equal "in-package" (first read)))
            do (when is-package
                 (setf package (second read)))
            until (or (eql read eof)
                      is-package))
      (values package md5sum seq))))

(defun handler (cmd)
  (let* ((config (load-config :where :user))
         (args (clingon:command-arguments cmd))
         (impl (or (clingon:getopt cmd :lisp)
                   "sbcl"))
         (version (or (clingon:getopt cmd :version)
                      (and impl (config `(,impl "version") config :if-does-not-exist nil))))
         (pos (position "--" args :test 'equal))
         (native-args (when pos (cons "--" (subseq args 0 pos))))
         (args (if pos
                   (subseq args (1+ pos))
                   args))
         (param (make-impl-param
                 (intern (string-upcase impl) :keyword)
                 :cmd cmd
                 :name impl
                 :version version
                 :image nil
                 :quicklisp nil)))
    (let ((script (uiop:file-exists-p (first args)))
          (impl-path (impl-path param))
          (bin-dir (bin-dir :native t))
          md5 package)
      (message :script-handler "args-for script handler ~S" args)
      (message :script-handler "cmd for script handler ~S" cmd)
      (message :script-handler "param for script handler ~S" param)
      (message :script-handler "fileexist: ~S" script)
      (unless args
        (clingon:run cmd '("--help")))
      (multiple-value-setq (package md5) (parse-script script))
      (message :script-handler "script parsed package ~S md5 ~S" package md5)
      (setf image
            (make-pathname :name (format nil "~A-~A" (pathname-name script) md5)
                           :type "core"
                           :defaults (translate-pathname 
                                      script
                                      "/**/*.*" (merge-pathnames "core/**/*.*" impl-path)))
            ql (if (and (equal (pathname-directory script)
                               (pathname-directory bin-dir))
                        (equal (pathname-device script)
                               (pathname-device bin-dir)))
                   t
                   (namestring
                    (merge-pathnames
                     (format nil "~A/" (pathname-name script))
                     (make-pathname
                      :name nil
                      :type nil
                      :defaults (translate-pathname
                                 script
                                 "/**/*.*" (merge-pathnames "quicklisp/**/*.*" impl-path)))))))
      (message :script-handler "image-path: ~S" image)
      (message :script-handler "ql-path: ~S" ql)
      (message :script-handler "forms: ~S" *forms*)
      (unless (uiop:file-exists-p image)
        (let (*forms*
              (dump-param (make-impl-param
                           (impl-param-kind param)
                           :args native-args
                           :cmd cmd
                           :name (impl-param-name param)
                           :version (impl-param-version param)
                           :image nil
                           :quicklisp ql)))
          (push (list :eval (format nil "(with-open-file (in ~S) (read-line in) (eval (read in)))" script)) *forms*)
          (push (list :dump image) *forms*)
          (run-impl :forms *forms* :param dump-param :exec 'run-program)))
      (let (*forms*
            (run-param (make-impl-param
                        (impl-param-kind param)
                        :args native-args
                        :cmd cmd
                        :name (impl-param-name param)
                        :version (impl-param-version param)
                        :image (namestring image)
                        :quicklisp ql)))
        (push (list :eval (format nil "(progn #-roswell2.cmd.script (cl:load ~S))"
                                  (truename (merge-pathnames
                                             "ros-loader.lisp"
                                             (asdf:system-source-directory
                                              (asdf:find-system :roswell2.cmd.script)))))) *forms*)
        (push (list :eval "(roswell2.cmd.script/ros-loader:ignore-shebang)") *forms*)
        (push (list :load script) *forms*)
        (push (list :eval (format nil "(apply (let ((*package* (find-package '~S))) (read-from-string \"main\")) '~S)" package (cdr args))) *forms*)
        (run-impl :forms (nreverse *forms*) :param run-param))))
  (uiop:quit))
