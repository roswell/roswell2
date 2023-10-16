(uiop:define-package :roswell-bin/config
  (:use :cl)
  (:export :*project-name* :*stage1-path* :*stage1-commit* :*stage2-path* :*stage2-commit*))
(in-package :roswell-bin/config)

(defparameter *project-name* "roswell")
(defvar *stage1-path* nil)
(defvar *stage1-commit* nil)
(defvar *stage2-path* nil)
(defvar *stage2-commit* nil)
