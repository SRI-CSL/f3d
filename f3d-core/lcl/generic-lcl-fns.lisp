(in-package :lcl)


(defun environment-variable (name) (getenv name))

(defun setenv (name value)
  (setf (environment-variable name) value))

(defun (setf getenv) (name value)
  (setf (environment-variable name) value))

