(uiop:define-package :roswell-bin/uname
  (:use :cl
        :roswell-bin/util)
  (:export :uname-s
           :uname-m
           :exeext
           :core-path
           :uname))
(in-package :roswell-bin/uname)

(defun uname-s ()
  #-win32
  (let ((s (strip-run-cmd "uname -s" :cache t)))
    (cond ((equal s "SunOS") "solaris")
          ((equal s "DragonFly") "DFlyBSD")
          ((and (equal s "Linux")
                (equal (strip-run-cmd "uname -m" :cache t)
                       "aarch64")
                (equal (strip-run-cmd "uname -o" :cache t)
                       "Android"))
           ;; termux?
           "android")
          (t (string-downcase s)))))

(defun uname-m ()
  (let ((m (strip-run-cmd "uname -m" :cache t)))
    (cond ((equal m "i86pc")
           ;; solaris
           (if (equal (strip-run-cmd "isainfo -k" :cache t)
                      "amd64")
               "x86-64"
               "x86"))
          ((or (equal m "i686")
               (equal m "i386"))
           "x86")
          ((equal m "amd64")
           "x86-64")
          ((equal m "aarch64")
           "arm64")
          ((or (equal m "armv6l")
               (equal m "armv7l"))
           (let ((result (strip-run-cmd "readelf -A /proc/self/exe |grep Tag_ABI_VFP_args|wc -l" :cache t)))
             (if (equal "0" result)
                 "armhf"
                 "armel")))
          ((or (equal m "armv5tejl")
               (equal m "armel"))
           t)
          (t
           (substitute #\- #\_ m)))))

(defun exeext ()
  #-win32"")

(defun core-path (base-path)
  (merge-pathnames (format nil "core/~A/~A/roswell~A"
                           (uname-m)                           
                           (uname-s)
                           (exeext)) base-path))

(defun uname (args)
  (cond ((or (equal (first args) "-s")
             (null (first args)))
         (format t "~A~%" (uname-s)))
        ((equal (first args) "-m")
         (format t "~A~%" (uname-m)))
        (t
         (error "uname error ~S" args))))
