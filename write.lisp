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

(defun write-df (data-symbol &optional (stream *standard-output*))
  "Write DATA-SYMBOL to STREAM in a format suitable for reading back in with the Lisp reader"
  (check-type data-symbol symbol "a SYMBOL naming a data frame")
  (let ((*package* (find-package (string-upcase (symbol-name data-symbol)))))
  (format stream ";;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LS-USER -*-")

  ;; Write data frame
  (format stream "~%(defdf ~A (alist-df '" (symbol-name data-symbol))
  (prin1 (as-alist (symbol-value data-symbol)) stream)
  (format stream ")")
  (print (documentation data-symbol 'variable) stream)
  (format stream ")~2%")

  ;; Write standard properties
  (write-properties data-symbol :type  stream)
  (write-properties data-symbol :label stream)
  (write-properties data-symbol :unit  stream)))

(defun save (data-symbol pathspec &optional (suffix ".lisp"))
  "Save DF in the file named by PATHSPEC.  By default, a suffix of .lisp is added."
  (with-open-file (s
		   (uiop:add-pathname-suffix pathspec nil :type suffix)
		   :direction :output
		   :if-exists :supersede)
     (write-df data-symbol s)))

(defun write-properties (data-symbol property &optional (stream *standard-output*))
  "Write the variable PROPERTY strings to stream so they can be read back in when LOADed from a lisp file.  By convention, the name of the function that sets the property is the same as the property.
Example (write-property mtcars :label)"
  (let* ((name (symbol-name data-symbol))
	 (df   (symbol-value data-symbol))
	 (pkg  (find-package name)))
    (when pkg
      (format stream "(set-properties '~A :~A '(" name property)
      (loop for x across (keys df)
	    for key = (find-symbol (string-upcase (symbol-name x)) pkg)
	    for value = (get key property)
	    do
	       (when value
    		 (typecase value
		   (string (format stream "~&  :~A \"~A\"" key value))
		   (symbol (if (eql property :type)
			       (format stream "~&  :~A :~A" key value)
			       (format stream "~&  :~A ~A"     key value))))))
      (format stream "))~2%"))))
