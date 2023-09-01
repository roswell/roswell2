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

(defun options ()
  "Returns the options for the  command"
  (option-base
   :variant-explanation (format nil "set variant for install. default:~A https://github.com/roswell/sbcl_bin/blob/master/table.md" *default-variant*)
   :base-uri-explanation (format nil "set base-uri  default:~A" *base-uri*)
   :arch-explanation (format nil "set arch for install. defualt:~A" (uname-m))
   :os-explanation (format nil "set os for install. default:~A" (uname-s))))

(defun impl-tsv-uri (param)
  (format nil "~Afiles/sbcl-bin_uri.tsv" (or (impl-param-base-uri param) *base-uri*)))

(defun impl-set-version-param (param)
  (let* ((tsv-uri  (impl-tsv-uri param))
         (tsv-path (merge-pathnames (format nil "tmp/~A" (file-namestring tsv-uri)) (app-cachedir))))
    (ensure-directories-exist tsv-path)
    (message :impl-set-version-param "No ~A version specified. Downloading ~A to see the available versions..."
             (impl-param-name param)
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
          for param-variant = (impl-param-variant param)
          do (when (and (equal variant (if (equal param-variant *vanilla*)
                                           ""
                                           param-variant))
                        (equal (impl-param-os param) system)
                        (equal (impl-param-arch param) arch))
               (setf (impl-param-version param) version)
               (setf (impl-param-uri param)     uri)
               (message :impl-set-version-param "Installing ~A/~A..."
                        (impl-param-name param)
                        (impl-param-version param))
               (return-from impl-set-version-param param)))
    (uiop:quit 1)))

(defun impl-already-installedp (param)
  ;;return executable-paths with multple-values
  (values (uiop:file-exists-p
           (merge-pathnames (format nil "bin/sbcl")
                            (impl-path param)))))

(defun impl-download (param)
  (unless (impl-param-uri param)
    (setf (impl-param-uri param)
          (concatenate 'string
                       (or (impl-param-base-uri param) *base-uri*)
                       ;;"https://github.com/roswell/sbcl_bin/releases/download/"
                       (impl-param-version param) ;;"2.3.7"
                       "/"
                       (impl-param-name param) ;; "sbcl"
                       "-"
                       (impl-param-version param) ;;"2.3.7"
                       "-"
                       (impl-param-arch param) ;;"x86-64"
                       "-"
                       (impl-param-os param) ;;"linux"
                       (if (equal (impl-param-variant param)
                                  *vanilla*)
                           "-"
                           (format nil "-~A-" (impl-param-variant param)))
                       "binary.tar.bz2")))
  (message :impl-download "Downlaad ~A/~A..."
           (impl-param-name param)
           (impl-param-version param))
  (message :impl-download "URI: ~A" (impl-param-uri param))
  (let ((archive (impl-archive-path param)))
    (message :impl-download "PATH: ~A" archive)
    (if (uiop:file-exists-p archive)
        (message :impl-download "PATH: ~A already exist. skip downloading." archive)
        (let ((code (download-simple (impl-param-uri param) archive)))
          (unless (zerop code)
            (message :impl-download "Download failed (Code=~A)" code)
            (uiop:quit 1))))
    param))

(defun impl-expand (param)
  (let ((archive (uiop:native-namestring (impl-archive-path param)))
        (dist-path (uiop:native-namestring 
                    (ensure-directories-exist (merge-pathnames "src/" (app-cachedir))))))
    (message :impl-expand "Extracting ~A to ~A" archive dist-path)
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
              (impl-param-name param)
              "-" (impl-param-version param)
              "-"  (impl-param-arch param)
              "-"  (impl-param-os param)
              (or (and (equal *vanilla* (impl-param-variant param))
                       "/")
                  (format nil "-~A/" (impl-param-variant param))))
             (app-cachedir))))
         (sbcl-home (namestring (merge-pathnames "lib/sbcl/" impl-path))))
    (message :impl-install "Building ~A/~A(~A)..."
             (impl-param-name param)
             (impl-param-version param)
             (impl-param-variant param))
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
  (let* ((variant (impl-param-variant param))
         (version (impl-param-version param))
         (config (load-config :where :global)))
    (setf (config '("sbcl" "variant") config) variant)
    (setf (config '("sbcl" "version") config) version)
    (save-config :config config :where :global)
    (with-open-file (o (merge-pathnames "roswell.sexp" (impl-path param))
                       :direction :output
                       :if-exists :supersede)
      (format o "~S~%" param))))

(defun handler (cmd)
  "Handler for just evaluate options"
  (let ((param (make-instance
                'impl-param
                :name "sbcl"
                :variant (or (and (equal (clingon:getopt cmd :variant) "")
                                  *default-variant*)
                             (clingon:getopt cmd :variant)
                             *default-variant*)
                :os  (or (clingon:getopt cmd :os)          (uname-s))
                :arch    (or (clingon:getopt cmd :arch)    (uname-m))
                :base-uri(clingon:getopt cmd :base-uri)
                :version (clingon:getopt cmd :version)
                :uri (clingon:getopt cmd :uri)
                :run ":roswell2.sbcl")))
    (message :main-handler "args-for install ~A  ~S"
             (impl-param-name param)
             (clingon:command-arguments cmd))
    (message :main-handler "version: ~S" (impl-param-version param))
    (unless (impl-param-version param)
      (impl-set-version-param param))
    (when (impl-already-installedp param)
      (progn
        (message :main-handler "~A/~A(~A) is already installed."
                 (impl-param-name param)
                 (impl-param-version param)
                 (impl-param-variant param))
        (uiop:quit 1)))
    (impl-download param)
    (impl-expand param)
    (impl-install param)
    #+linux
    (impl-patchelf param)
    (impl-set-config param)))
