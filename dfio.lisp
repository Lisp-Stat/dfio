;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; (c) 2021 Symbolics Pte. Ltd. All rights reserved.

(uiop::define-package #:dfio
  (:nicknames #:do)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-csv
        #:let-plus
        #:dfio.data-column)
  (:export
   #:string-to-keyword
   #:csv-to-data-frame))

(in-package #:dfio)

(defun csv-to-data-columns (stream-or-string skip-first-row?)
  "Read a CSV file (or stream, or string), accumulate the values in DATA-COLUMNs, return a list of these.  Rows are checked to have the same number of elements.

When SKIP-FIRST-ROW?, the first row is read separately and returned as the second value (list of strings), otherwise it is considered data like all other rows."
  (let (data-columns
        (first-row skip-first-row?))
    (do-csv (row stream-or-string)
      (if data-columns
          (assert (length= data-columns row))
          (setf data-columns (loop repeat (length row) collect (data-column))))
      (if first-row
          (mapc #'data-column-add data-columns row)
          (setf first-row row)))
    (values data-columns (unless skip-first-row? first-row))))

(defun string-to-keyword (string)
  "Map string to a keyword.

This is the default for constructing column keys for CSV files.

The current implementation replaces #\. and #\space with a #\-, and upcases all other characters."
  ;; QUESTION: should the result depend on the readtable? Papp:date-unknown

  ;; ANSWER: Probably. Symbols would be interned in the current
  ;; readtable package, *package*. This is likely the case when
  ;; setting up a data project. All your data-frame could will be in a
  ;; seperate package. Alternatively, specify a short package name
  ;; specifically for data objects. Export them too.
  ;; If in CL-USER, then use KEYWORD package. SN:20210112
  (make-keyword (map 'string
                     (lambda (character)
                       (case character
                         ((#\. #\space) #\-)
                         (otherwise (char-upcase character))))
                     string)))

(defun csv-to-data-frame (stream-or-string
                          &key (skip-first-row? nil)
                               (column-keys-or-function #'string-to-keyword))
  "Read a CSV file (or stream, or string) into a DATA-FRAME, which is returned.

When SKIP-FIRST-ROW?, the first row is read separately and COLUMN-KEYS-OR-FUNCTION is used to form column keys.

When COLUMN-KEYS-OR-FUNCTION is a sequence, it is used for column keys, regardless of the value of SKIP-FIRST-ROW?."
  (let+ (((&values data-columns first-row)
          (csv-to-data-columns stream-or-string skip-first-row?))
         (column-keys (cond
                        ((and first-row (functionp column-keys-or-function))
                         (mapcar column-keys-or-function first-row))
                        ((typep column-keys-or-function 'sequence)
                         (assert (length= data-columns column-keys-or-function) ()
                                 "The length of column keys ~A does not match the number of columns ~A."
                                 column-keys-or-function (length data-columns)))
                        (t (error "Could not generate column keys.")))))
    (data-frame:alist-df
     (mapcar (lambda (column-key data-column)
               (cons column-key (data-column-vector data-column)))
             column-keys data-columns))))
