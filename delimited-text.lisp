;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;;
;;; CSV
;;;

(defun csv-to-data-columns (stream-or-string skip-first-row? &key map-alist)
  "Read a CSV file (or stream, or string), accumulate the values in DATA-COLUMNs, return a list of these.  Rows are checked to have the same number of elements.

When SKIP-FIRST-ROW?, the first row is read separately and returned as the second value (list of strings), otherwise it is considered data like all other rows."
  (let (data-columns
        (first-row skip-first-row?))
    (cl-csv:do-csv (row stream-or-string)
      (if data-columns
          (assert (length= data-columns row))
          (setf data-columns (loop repeat (length row) collect (data-column :map-alist map-alist))))
      (if first-row
          (mapc #'data-column-add data-columns row)
          (setf first-row row)))
    (values data-columns (unless skip-first-row? first-row))))

(defun string-to-keyword (string)
  "Map string to a keyword.

The current implementation replaces #\. and #\space with a #\-, and upcases all other characters."

  ;; Tamas:date-unknown: QUESTION: should the result depend on the readtable?
  ;; SN:20210416: I suspect Tamas may have been thinking about readtable-case
  ;; in order to preserve the original case of the column names
  (make-keyword (map 'string
                     (lambda (character)
                       (case character
                         ((#\. #\space) #\-)
                         (otherwise (char-upcase character))))
                     string)))

(defun string-to-symbol (string)
  "Map STRING to a symbol in PACKAGE, replacing #\. and #\space with a #\-, and upcasing all other characters. Exports symbol."
  (let* ((sym (cond ((string= string "")
		     (warn "Missing column name was filled in")
		     (gentemp "X"))
		    (t (intern
			(map 'string
			     (lambda (character)
			       (case character
				 ((#\. #\space) #\-)
				 (otherwise (char-upcase character))))
			     string))))))
    (export sym)
    sym))

(defun read-csv (stream-or-string
                 &key
		   (skip-first-row? nil)
                   (column-keys-or-function #'string-to-symbol)
		   (package nil)
		   (map-alist '(("" . :na) ;could be anything, e.g. :missing, nil
                                ("NA" . :na))))
  "Read a CSV file (or stream, or string) into a DATA-FRAME, which is returned.

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

(defun write-csv (df
                  &key
		    stream
		    (add-first-row nil)
		    ((:separator separator) cl-csv:*separator*)
		    ((:quote quote) cl-csv:*quote*)
		    ((:escape quote-escape) cl-csv:*quote-escape*)
		    ((:newline write-newline) cl-csv::*write-newline*)
		    ((:always-quote always-quote) cl-csv::*always-quote*))
  "Write a data-frame to a stream.

Keywords:
    stream: stream to write to. Default: nil.
      nil - writes the rows to a string and returns it
      an open stream
      a pathname (overwrites if the file exists)
    quote: quoting character. Defaults to *quote*
    escape: escaping character. Defaults to *quote-escape*
    newline: newline character. Defaults to *write-newline*
    always-quote: Defaults to *always-quote*
    add-first-row: Add column names as the first

Notes:
    The :newline keyword requires a sequence, so use :newline '(#\newline) or use cl-interpol"
  (let ((rows (if add-first-row
		  (list* (coerce (df:keys df) 'list)
			 (df::2d-array-to-list (aops:as-array df)))
		  (df::2d-array-to-list (aops:as-array df)))))
    (cl-csv:write-csv rows
		      :stream stream
		      :separator separator
		      :quote  quote
		      :escape quote-escape
		      :newline write-newline
		      :always-quote always-quote)))

