(defpackage :utils
  (:use :common-lisp)
  (:export concat-lines join-strings join-strings-stream))
(in-package :utils)

(defun concat-lines (list-of-strings)
  (with-output-to-string (s)
      (dolist (str list-of-strings)
	(write-string str s)
	(write-char #\newline s))))

(defun join-strings-stream (s seperator strings)
  (if (cdr strings)
      (progn
	(write-string (car strings) s)
	(write-char seperator s)
	(join-strings-stream s seperator (cdr strings)))
    (write-string (car strings) s)))

(defun join-strings(seperator strings)
  (if strings
      (with-output-to-string (s)
	(join-strings-stream s  seperator strings))
      ""))
