;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;; JSON/Vega data

;; (dex:get "https://cdn.jsdelivr.net/npm/vega-datasets@2.2.0/data/cars.json" :want-stream t)
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
			   (alexandria:hash-table-values row)))) ; Sigh JSON->number->string to reuse data-column-add
    (values data-columns (map 'list #'string-to-keyword column-keys))))

(defun json-to-data-frame (stream-or-string)
  "Read stream into DATA-FRAME"
  (let-plus:let+ (((&values data-columns column-keys) (json-to-data-columns stream-or-string)))
    (data-frame:alist-df
     (mapcar (lambda (column-key data-column)
               (cons column-key (data-column-vector data-column)))
             column-keys data-columns))))


;;; Encode a data frame
(setf yason:*symbol-key-encoder* 'YASON:ENCODE-SYMBOL-AS-LOWERCASE) ; See issue #1

(defun data-frame-to-json (df &key (stream nil))
  "Encode a DATA-FRAME as a json array"
  (yason:with-output (stream :indent t)
    (yason:with-array ()
      (loop for i below (aops:nrow df)
	    do (yason:encode-plist (nu:as-plist (select:select df i t)))))))

