;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;; Write a data frame to a form that can be read by the Lisp reader

;;; Examples:
;; (load #P"LS:DATA;mtcars")
;; (save mtcars #P"LS:DATA;mtcars2.lisp")

;;; TODO Set these up to work with defaults: ".lisp" as the filename
;;; extension and the *default-pathname-defaults* for the directory
;;; See `savevar` function

;;; TODO Make these generic functions so that the user can subclass or
;;; use :after methods to save custom properties or data frame
;;; subclasses

(defun write-df (df &optional (stream *standard-output*))
  "Write DF to STREAM in a format suitable for reading back in with the Lisp reader"
  (let ((*package* (find-package (string-upcase (symbol-name df)))))
  (format stream ";;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LS-USER -*-")

  ;; Write data frame
  (format stream "~%(defdf ~A (alist-df '" (symbol-name df))
  (prin1 (as-alist (symbol-value df)) stream)
  (format stream ")")
  (print (documentation df 'variable) stream)
  (format stream ")~2%")

  ;; Write standard properties
  (write-properties (symbol-value df) :type  stream)
  (write-properties (symbol-value df) :label stream)
  (write-properties (symbol-value df) :unit  stream)))

;; TODO Implement automatic adding of suffix
(defun save (df pathspec &optional (suffix ".lisp"))
  "Save DF in the file named by PATHSPEC"
  (declare (ignore suffix))		;Remove and implement
  (with-open-file (s
		   pathspec
		   :direction :output
		   :if-exists :supersede)
     (write-df df s)))

(defun write-properties (df property &optional (stream *standard-output*))
  "Write the variable PROPERTY strings to stream so they can be read back in when LOADed from a lisp file.  By convention, the name of the function that sets the property is the same as the property.
Example (write-property mtcars :label)"
  (format stream "(set-properties ~A :~A '(" (df:name df) property)
  (loop for key across (keys df)
	for value = (get key property)
	do
    	   (typecase value
	     (string (format stream "~&  :~A \"~A\"" key value))
	     (symbol (format stream "~&  :~A ~A"     key value))))
  (format stream "))~2%"))
