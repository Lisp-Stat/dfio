;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;; JSON/Vega-lite serialisation of data-frame
;;; Export list at the bottom of the file.

(defun json-to-data-columns (stream-or-string)
  "Read a JSON array and accumulate the values in DATA-COLUMNs, return a list of columns.  Rows are checked to have the same number of elements.  The second value is a list of column names."
  (let (data-columns column-keys)
    (loop for row in (yason:parse stream-or-string)
	  do (if data-columns
		 (assert (alexandria:length= data-columns (alexandria:hash-table-values row)))
		 (setf data-columns (loop repeat (length (alexandria:hash-table-keys row)) collect (data-column))))
	  do (if (not column-keys)
		 (setf column-keys (alexandria:hash-table-keys row)))
	  do (mapc #'data-column-add
		   data-columns
		   (mapcar #'princ-to-string
			   (alexandria:hash-table-values row)))) ;Sigh JSON->number->string to reuse data-column-add
    (values data-columns (map 'list #'string-to-symbol column-keys))))

(defun vl-to-df (stream-or-string)
  "Read a stream of Vega-Lite data into DATA-FRAME
Useful when working with Vega-Lite data sets from external sources."
  (let+ (((&values data-columns column-keys) (json-to-data-columns stream-or-string)))
    (data-frame:alist-df
     (mapcar (lambda (column-key data-column)
               (cons column-key (data-column-vector data-column)))
             column-keys data-columns))))

(defun df-to-vl (df &optional (stream *standard-output*))
  "Encode a DATA-FRAME as a JSON array
Used to write Vega-Lite data to disk or network locations.  This is usually done when working with a third party tool where the data is not embedded within the Vega-Lite plot specification."
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)) ; See issue #1
    (yason:with-output (stream :indent t)
      (yason:with-array ()
	(loop for i below (aops:nrow df)
	      do (yason:encode-plist (nu:as-plist (select:select df i t)) stream))))))

(defun df-to-alist (df)
  "Convert DF, a data-frame, to a Vega style alist matrix
This is useful when working with a JSON encoder that will take a lisp alist and output a Vega-Lite plot specification."
  (loop for i below (aops:nrow df)
	with lst = nil
	do (loop for k across (df:keys df)
		 collecting (cons (symbol-name k) (select:select df i k)) into row
		 finally (push row lst))
	finally (return (coerce lst 'vector))))

(export 'vl-to-df)
(export 'df-to-vl)
(export 'df-to-alist)
