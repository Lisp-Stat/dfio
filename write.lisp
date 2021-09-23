;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;; Write a data frame to a form that can be read by the Lisp reader

;;; Examples:
;; (load #P"LS:DATASETS;mtcars")
;; (save mtcars #P"LS:DATASETS;mtcars2.lisp")

;;; TODO Set these up to work with defaults: ".lisp" as the filename
;;; extension and the *default-pathname-defaults* for the directory
;;; See `savevar` function

(defmacro write-df (df stream)
  "Write DF to STREAM in a format suitable for reading back in with the Lisp reader"
  `(progn (format ,stream ";;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LS-USER -*-")
	  (format ,stream "~%(define-data-frame ~A (alist-df '" (symbol-name ',df))
	  (prin1 (as-alist ,df) ,stream)
	  (format ,stream ")")
	  (print (documentation ',df 'variable) ,stream)
	  (format ,stream ")")))

(defmacro save (df pathspec)
  "Save DF in the file named by PATHSPEC"
  `(with-open-file (s
		   ,pathspec
		   :direction :output
		   :if-exists :supersede)
    (write-df ,df s)))
