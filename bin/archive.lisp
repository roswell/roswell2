(uiop:define-package :roswell-bin/archive
  (:use :cl
        :roswell-bin/util)
  (:export :tar))
(in-package :roswell-bin/archive)

(defun extract-command-str (flags filename do-extract outputpath type)
  #-win32
  (cond ((or (equal type "gzip")
             (equal type "bzip2")
             (equal type "xz"))
         (let ((arc (or (which "gtar")
                        (which "tar"))))
           (and arc
                (format nil "~A -dc ~A | ~A -~A~A~A"
                        type
                        filename
                        arc
                        (if do-extract "x" "t")
                        (if flags "p" "f - -C ")
                        outputpath))))
        ((equal type "7za")
          (ensure-directories-exist outputpath)
          (format nil "7za ~A -o~A ~A"
                  (if do-extract "x" "t")
                  outputpath
                  filename))
        (t nil)))

(defun extract (&key filename do-extract flags outputpath)
  (let ((ext (pathname-type filename))
        (type "gzip"))
    (setf type (cond ((or (equal "tbz2" ext)
                          (equal "bz2" ext))
                      "bzip2")
                     ((equal "xz" ext)
                      "xz")
                     ((equal "7z" ext)
                      "7za")
                     ((equal "cab" ext)
                      "cab")
                     (t "gzip")))
    (message :extract "extract type=~A" type)
    (let ((str (extract-command-str flags filename do-extract outputpath type)))
      (message :extract "extract cmd=~A" str)
      (when str
        (uiop:run-program str)))))

(defun tar (args)
  (let (filename
        outputpath
        flags
        (mode #\x)
        (verbose 0))
    (loop for argv on args
          while (eql (aref (car argv) 0) #\-)
          do (loop for p on (rest (coerce (car argv) 'list))
                   for opt = (first p)
                   do (case opt
                        (#\f
                         (setf filename
                               (if (rest p)
                                   (coerce (rest p) 'string)
                                   (second argv))
                               argv (if (rest p) 
                                        argv
                                        (rest argv))
                               p nil))
                        (#\C
                         (setf outputpath
                               (if (rest p)
                                   (coerce (rest p) 'string)
                                   (second argv))
                               argv (if (rest p) 
                                        argv
                                        (rest argv))
                               p nil))
                        (#\p
                         (setf flags t))
                        ((#\t #\x)
                         (setf mode opt))
                        (#\v
                         (setf verbose (1+ (* verbose 2)))))))
    (extract :filename filename :do-extract (eql mode #\x) :flags flags :outputpath outputpath)
    0))
