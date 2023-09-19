(defpackage :roswell2.cmd.script/ros-loader
  (:use :cl)
  (:export :ignore-shebang))
(in-package :roswell2.cmd.script/ros-loader)

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character infix-parameter))
  (loop for x = (read-char stream nil nil)
        until (or (not x) (eq x #\newline)))
  (values))

(defun ignore-shebang ()
  (set-dispatch-macro-character #\# #\! #'shebang-reader))

(push :roswell2.cmd.script *features*)
