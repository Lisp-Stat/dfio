;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

(defun csv-to-data-columns (stream-or-string skip-first-row? &key map-alist)
  "Read a CSV file (or stream, or string), accumulate the values in DATA-COLUMNs, return a list of these.  Rows are checked to have the same number of elements.

When SKIP-FIRST-ROW?, the first row is read separately and returned as the second value (list of strings), otherwise it is considered data like all other rows."
  (let (data-columns
        (first-row skip-first-row?))
    (with-csv-input-stream (s stream-or-string)
      (loop for row = (fare-csv:read-csv-line s) while row do
	(progn
	  (if data-columns
              (assert (length= data-columns row))
              (setf data-columns (loop repeat (length row) collect (data-column :map-alist map-alist))))
	  (if first-row
              (mapc #'data-column-add data-columns row)
              (setf first-row row)))
	    ) ;loop
      )
    (values data-columns (unless skip-first-row? first-row))))

(defun read-csv (stream-or-string
                 &key
		   (skip-first-row? nil)
                   (column-keys-or-function #'string-to-symbol)
		   (package nil)
		   (map-alist '((""   . :missing)
                                ("NA" . :na))))
  "Read a CSV file, stream, or string into a DATA-FRAME, which is returned.
When SKIP-FIRST-ROW?, the first row is read separately and COLUMN-KEYS-OR-FUNCTION is used to form column keys.
When COLUMN-KEYS-OR-FUNCTION is a sequence, it is used for column keys, regardless of the value of SKIP-FIRST-ROW?.
PACKAGE indicates the package to intern column names into.

MAP-ALIST maps values during the import. This is useful if you want special mappings for missing, though the mechanism is general."
  (let+ (((&values data-columns first-row)
          (csv-to-data-columns stream-or-string skip-first-row? :map-alist map-alist))
	 (*package* (cond
		      ((not package) *package*)
		      ((find-package (string-upcase package)) (find-package (string-upcase package)))
		      (t (make-package (string-upcase package)))))
         (column-keys (cond
                        ((and first-row (functionp column-keys-or-function))
			 (mapcar column-keys-or-function first-row))
                        ((typep column-keys-or-function 'sequence)
                         (assert (length= data-columns column-keys-or-function) ()
                                 "The length of column keys ~A does not match the number of columns ~A."
                                 column-keys-or-function (length data-columns))
			 column-keys-or-function)
                        (t (error "Could not generate column keys.")))))
    (data-frame:alist-df
     (mapcar (lambda (column-key data-column)
               (cons column-key (data-column-vector data-column)))
             column-keys data-columns))))

(defun write-csv (df stream
                  &key
		    (add-first-row nil)
		    ((:separator separator) fare-csv:*separator*)
                    ((:quote quote) fare-csv:*quote*)
                    ((:eol eol) fare-csv:+LF+))
  "Write DF to STRING-OR-STREAM in CSV format. STRING-OR-STREAM can be a STREAM, a STRING or a file PATHSPEC.

Keywords:
    string-or-stream: stream to write to. Default: nil, returning a string
    add-first-row:    add column names as the first row
    separator: separator to use when reading or writing CSV files. A character. By default, a comma: #\,
    quote:     quote character to use when reading or writing CSV files. A character. By default, a double-quote: #\"
    eol:       line ending to use when writing CSV files. A string. By default, +CRLF+ as specified by creativyst.

Notes:
    The :newline keyword requires a sequence, so use :newline '(#\newline)"
  (let ((rows (if add-first-row
		  (list* (coerce (df:keys df) 'list)
			 (df::2d-array-to-list (aops:as-array df)))
		  (df::2d-array-to-list (aops:as-array df))))
	(fare-csv:*separator* separator)
        (fare-csv:*quote*     quote)
	(fare-csv:*eol*       eol))
    (with-csv-output-stream (s stream)
      (fare-csv:write-csv-lines rows s)
      (unless stream
	(get-output-stream-string s)))))
