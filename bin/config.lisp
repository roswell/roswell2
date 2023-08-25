(uiop:define-package :roswell-bin/config
  (:use :cl)
  (:export :*project-name* :*stage1-path* :*stage2-path*))
(in-package :roswell-bin/config)

(defparameter *project-name* "roswell")
(defvar *stage1-path* nil)
(defvar *stage2-path* nil)
