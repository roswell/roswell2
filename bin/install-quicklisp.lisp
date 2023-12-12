(uiop:define-package :roswell-bin/install-quicklisp
  (:use :cl
        :roswell-bin/config
        :roswell-bin/util
        :roswell-bin/download
        :roswell-bin/archive)
  (:export :install-quicklisp))
(in-package :roswell-bin/install-quicklisp)

(defvar *quicklisp-client-version-uri*
  "https://raw.githubusercontent.com/quicklisp/quicklisp-client/master/quicklisp/version.txt")

(defvar *quicklisp-client-archive-uri*
  "https://github.com/quicklisp/quicklisp-client/archive/refs/tags/version-~A.tar.gz")

(defun quicklisp-client-version (&key (uri *quicklisp-client-version-uri*)
                                      (path (app-cachedir)))
  "=> \"2021-02-13\""
  (let ((file-path (ensure-directories-exist (merge-pathnames "tmp/quicklisp-client.version" path))))
    (download-simple uri file-path)
    (uiop:read-file-line file-path)))

(defun quicklisp-client-archive (&key version
                                      (cache-path (app-cachedir)))
  (let* ((version (or version (quicklisp-client-version :path cache-path)))
         (archive-uri (format nil *quicklisp-client-archive-uri* version))
         (file-path (ensure-directories-exist (merge-pathnames (format nil "archives/qlcli-~A.tgz" version) cache-path))))
    (unless (uiop:file-exists-p file-path)
      (download-simple archive-uri file-path))
    (values file-path version)))

(defun install-quicklisp (&key
                          version
                          (dist-url "http://beta.quicklisp.org/dist/quicklisp.txt")
                          (path (app-cachedir))
                          (ql-path "quicklisp/"))
  (let ((ql-path (merge-pathnames ql-path (ensure-directories-exist path)))
        (libpath (libdir)))
    (if (uiop:directory-exists-p ql-path)
        (message :install-quicklisp "~S found" ql-path)
      (multiple-value-bind (file-path version)
          (quicklisp-client-archive :version version)
        (tar
         (list "-xf" file-path
               "-C" (uiop:native-namestring path)))
        (let ((orig-dir (first (directory (format nil "~A*~A" (uiop:native-namestring path) version)))))
          (message :quicklisp-client-archive "rename '~A' => '~A'" orig-dir ql-path)
          (rename-file
           orig-dir
           ql-path))
        (message :quicklisp-client-archive "set dist dir for '~A' = '~A'" ql-path dist-url)
        (uiop:run-program (list *stage1-path*
                                "--eval" "(defpackage :quicklisp-quickstart)"
                                "--eval" (format nil "(defvar quicklisp-quickstart::*quickstart-parameters* (list :initial-dist-url ~S))" dist-url)
                                "--eval" (format nil "(let ((*standard-output* *error-output*)(*trace-output* *error-output*))(load (uiop:native-namestring (merge-pathnames \"setup.lisp\" ~S))))"
                                                 ql-path)
                                "--eval" "(uiop:quit)")
                          :ignore-error-status t
                          :output :interactive
                          :error-output :interactive)))
    ;; copy extension
    (let* ((extension "roswell.quicklisp.extensions")
           (loader (ensure-directories-exist (merge-pathnames "local-init/roswell2.lisp" ql-path)))
           (lisp (format nil "~A.lisp" extension))
           (asd (format nil "~A.asd" extension)))
      ;; shouldn't automatically load.
      #+()
      (unless (uiop:file-exists-p loader)
        (with-open-file (o loader :direction :output)
          (format o "#+roswell2.init(asdf:load-system ~S)~%" extension)))
      (loop for file in (list lisp asd)
            for src = (merge-pathnames file libpath)
            for dest = (merge-pathnames (format nil "quicklisp/~A" file) ql-path)
            do (when (or (not (uiop:file-exists-p dest))
                         (> (file-write-date src)
                            (file-write-date dest)))
                 (uiop:copy-file  src dest))))))
