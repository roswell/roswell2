(uiop:define-package :roswell2/config.default
  (:use :cl))
(in-package :roswell2/config.default)

#+linux
(when (uiop:file-exists-p "/etc/alpine-release")
  (setf roswell2.install.sbcl:*default-variant* "musl"))
