(uiop:define-package :roswell2.install.sbcl/main
  (:use :cl
        :roswell-bin/archive
        :roswell-bin/download
        :roswell-bin/util
        :roswell-bin/uname
        :roswell2.cmd.install
        :roswell2/main)
  (:nicknames :roswell2.install.sbcl)
  (:import-from :clingon)
  (:export
   :*base-uri*
   :*default-variant*))

(in-package :roswell2.install.sbcl/main)

(defvar *command-class* 'roswell2/clingon.extensions::command-without-version)
(defvar *base-uri* "https://github.com/roswell/sbcl_bin/releases/download/")
(defvar *vanilla* "bin")
(defvar *default-variant* *vanilla*)
(defvar *param-class* 'install-param)

(defun options ()
  "Returns the options for the  command"
  (list
   (clingon:make-option
    :string
    :description "set version for install"
    :parameter "VERSION"
    :long-name "version"
    :key :version)
   (clingon:make-option
    :string
    :description (format nil "set arch for install. defualt:~A" (uname-m))
    :parameter "ARCH"
    :long-name "arch"
    :key :arch)
   (clingon:make-option
    :string
    :description (format nil "set variant for install. default:~A https://github.com/roswell/sbcl_bin/blob/master/table.md" *default-variant*)
    :parameter "VARIANT"
    :long-name "variant"
    :key :variant)
   (clingon:make-option
    :string
    :description (format nil "set os for install. default:~A" (uname-s))
    :parameter "OS"
    :long-name "os"
    :key :os)
   (clingon:make-option
    :string
    :description (format nil "set base-uri  default:~A" *base-uri*)
    :parameter "URI"
    :long-name "base-uri"
    :key :base-uri)
   (clingon:make-option
    :string
    :description (format nil "set archive uri")
    :parameter "URI"
    :long-name "uri"
    :key :uri)))

(defun impl-tsv-uri (param)
  (format nil  "~Afiles/sbcl-bin_uri.tsv" (install-param-base-uri param)))

(defun impl-set-version-param (param)
  (let* ((tsv-uri  (impl-tsv-uri param))
         (tsv-path (merge-pathnames (format nil "tmp/~A" (file-namestring tsv-uri)) (app-cachedir))))
    (ensure-directories-exist tsv-path)
    (message :impl-set-version-param "No ~A version specified. Downloading ~A to see the available versions..."
             (install-param-impl param)
             (file-namestring tsv-uri))
    (let ((code (download-simple tsv-uri tsv-path)))
      (unless (zerop code)
        (message :impl-set-version-param "Download failed (Code=~A)" code)
        (uiop:quit 1)))
    (loop for line in (cdr (uiop:read-file-lines tsv-path))
          for split = (uiop:split-string line)
          for system = (first split)
          for arch = (second split)
          for version = (third split)
          for variant = (fourth split)
          for uri = (fifth split)
          do (when (and (if (equal (install-param-variant param) *vanilla*)
                            (equal variant "")
                            (equal (install-param-variant param) variant))
                        (equal (install-param-os param) system)
                        (equal (install-param-arch param) arch))
               (setf (install-param-version param) version
                     (install-param-uri param)     uri)
               (message :impl-set-version-param "Installing ~A/~A..."
                        (install-param-impl param)
                        (install-param-version param))
               (return-from impl-set-version-param param)))
    (uiop:quit 1)))

(defun impl-already-installedp (param)
  ;;return executable-paths with multple-values
  (values (uiop:file-exists-p
           (merge-pathnames (format nil "bin/sbcl")
                            (impl-path param)))))

(defun impl-download (param)
  (unless (install-param-uri param)
    (setf (install-param-uri param)
          (concatenate 'string
                       (install-param-base-uri param)
                       ;;"https://github.com/roswell/sbcl_bin/releases/download/"
                       (install-param-version param) ;;"2.3.7"
                       "/"
                       (install-param-impl param) ;; "sbcl"
                       "-"
                       (install-param-version param) ;;"2.3.7"
                       "-"
                       (install-param-arch param) ;;"x86-64"
                       "-"
                       (install-param-os param) ;;"linux"
                       (if (equal (install-param-variant param)
                                  *vanilla*)
                           "-"
                           (format nil "-~A-" (install-param-variant param)))
                       "binary.tar.bz2")))
  (setf (install-param-archive param) 
        (ensure-directories-exist (merge-pathnames (format nil "archives/~A"
                                                           (file-namestring (install-param-uri param)))
                                                   (app-cachedir))))
  (message :impl-download "Downlaad ~A/~A..."
           (install-param-impl param)
           (install-param-version param))
  (message :impl-download "URI: ~A" (install-param-uri param))
  (message :impl-download "PATH: ~A" (install-param-archive param))
  (let ((code (download-simple (install-param-uri param) (install-param-archive param))))
    (unless (zerop code)
      (message :impl-download "Download failed (Code=~A)" code)
      (uiop:quit 1)))
  param)

(defun impl-expand (param)
  (let ((archive (uiop:native-namestring (install-param-archive param)))
        (dist-path (uiop:native-namestring 
                    (ensure-directories-exist (merge-pathnames "src/" (app-cachedir))))))
    (message :impl-expand "Extracting ~A to ~A" archive dist-path);
    (tar
     (list "-xf" archive
           "-C" dist-path)))
  param)

(defun find-gnumake ()
  ;;tbd
  t)

(defun impl-install (param)
  (let* ((impl-path  
           (namestring (impl-path param)))
         (expand-path
           (namestring
            (merge-pathnames
             (concatenate 
              'string
              "src/"
              (install-param-impl param)
              "-" (install-param-version param)
              "-"  (install-param-arch param)
              "-"  (install-param-os param)
              (or (and (equal *vanilla* (install-param-variant param))
                       "/")
                  (format nil "-~A/" (install-param-variant param))))
             (app-cachedir))))
         (sbcl-home (namestring (merge-pathnames "lib/sbcl/" impl-path))))
    (message :impl-install "Building ~A/~A(~A)..."
             (install-param-impl param)
             (install-param-version param)
             (install-param-variant param))
    (chdir expand-path)
    (setf (uiop:getenv "SBCL_HOME") (subseq* sbcl-home 0 -1)
          (uiop:getenv "INSTALL_ROOT") (subseq* impl-path 0 -1))
    (unless (find-gnumake)
      (message :impl-install "'make' command not available.")
      (uiop:quit 1))
    (uiop:run-program  
     (format nil "~A install.sh" (sh))
     :output :interactive
     :error-output :interactive)
    (message :impl-install "install Done.")))

#+linux
(defun impl-patchelf (param)
  (let* ((patchelf (which "patchelf"))
         (ls (which "ls")) ;; shouldn't fail on normal environment.
         (interpreter))
    (when patchelf
      (setf interpreter (strip-run-cmd (format nil "patchelf --print-interpreter ~A" ls)))
      (loop for i in (multiple-value-list (impl-already-installedp param))
            do (message :impl-patchelf "patchelf ~A ~A"
                        (namestring i) interpreter)
               (strip-run-cmd (format nil "patchelf --set-interpreter ~A ~A"
                                      interpreter
                                      (namestring i)))))))

(defun impl-set-config (param)
  (let* ((variant (install-param-variant param))
         (version (install-param-version param))
         (config (load-config :where :global)))
    (setf (config '("sbcl" "variant") config) variant)
    (setf (config '("sbcl" "version") config) version)
    (save-config :config config :where :global)
    (with-open-file (o (merge-pathnames "roswell.class" (impl-path param))
                       :direction :output
                       :if-exists :supersede)
      (format o ":roswell2.sbcl~%"))))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((param (make-instance
                *param-class*
                :impl "sbcl"
                :variant (or (and (equal (clingon:getopt cmd :variant) "")
                                  *default-variant*)
                             (clingon:getopt cmd :variant)
                             *default-variant*)
                :os  (or (clingon:getopt cmd :os)          (uname-s))
                :arch    (or (clingon:getopt cmd :arch)    (uname-m))
                :base-uri(or (clingon:getopt cmd :base-uri)*base-uri*)
                :version (clingon:getopt cmd :version)
                :uri (clingon:getopt cmd :uri))))
    (message :main-handler "args-for install ~A  ~S"
             (install-param-impl param)
             (clingon:command-arguments cmd))
    (message :main-handler "version: ~S" (install-param-version param))
    (unless (install-param-version param)
      (impl-set-version-param param))
    (when (impl-already-installedp param)
      (progn
        (message :main-handler "~A/~A(~A) is already installed."
                 (install-param-impl param)
                 (install-param-version param)
                 (install-param-variant param))
        (uiop:quit 1)))
    (impl-download param)
    (impl-expand param)
    (impl-install param)
    #+linux
    (impl-patchelf param)
    (impl-set-config param)))
    
