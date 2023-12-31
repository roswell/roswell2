(uiop:define-package :roswell-bin/download
  (:use :cl
        :roswell-bin/util)
  (:export :simple-fetch
           :lib-info :lib-init))
(in-package :roswell-bin/download)

#-win32
(cffi:defcallback write-data :size ((ptr :pointer) (size :size)
                                                   (nmemb :size) (stream :pointer))
  (let (#+nil(data-size (* size nmemb)))
    (cl-curl/functions::fwrite ptr size nmemb stream)))

#-win32
(cffi:defcallback header-callback :size ((buffer :pointer) (size :size)
                                                           (nmemb :size) (stream :pointer))
  (declare (ignorable stream buffer))
  (* size nmemb))

(defun simple-fetch (uri path &key &allow-other-keys)
  (message :simple-fetch "download uri ~A path ~A" uri path)
  #-win32
  (let* ((part (format nil "~A.part" path))
         (bodyfile (cl-curl/functions::fopen part "wb"))
         res)
    (unless (cffi:null-pointer-p bodyfile)
      (let ((curl (cl-curl:curl-easy-init)))
        (unless (cffi:null-pointer-p curl)
          (unwind-protect
               (progn
                 (cl-curl:curl-easy-setopt curl :url uri)
                 (cl-curl:curl-easy-setopt curl :followlocation 1)
                 (cl-curl:curl-easy-setopt curl :writefunction (cffi:callback write-data))
                 (cl-curl:curl-easy-setopt curl :headerfunction  (cffi:callback header-callback))
                 (cl-curl:curl-easy-setopt curl :writedata bodyfile)
                 (setf res (cl-curl:curl-easy-perform curl)))
            (cl-curl:curl-easy-cleanup curl)
            (cl-curl/functions::fclose bodyfile)
            (uiop:rename-file-overwriting-target part path))
          (unless (zerop res)
            (return-from simple-fetch 2))))))
  0)

(defun lib-info ()
  #-win32
  (loop for elt in (uiop:split-string (cl-curl:curl-version) :separator '(#\Space))
        for split = (uiop:split-string (string-trim "()" elt) :separator '(#\/))
        collect (cons "libcurl" split)))

(defun lib-init ()
  #-(and linux)
  (cl-curl:init)
  )
