;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO.STRING-TABLE -*-
;;; (c) 2021 Symbolics Pte. Ltd. All rights reserved.

(cl:in-package #:dfio.string-table)

(defstruct (string-table (:constructor string-table))
  "A table of distinct strings, optionally mapping each one to a value."
  (table (make-hash-table :test #'equalp) :type hash-table))

(define-condition string-table-not-found (error)
  ()
  (:documentation "String not found in table."))

(define-condition string-table-duplicate (error)
  ()
  (:documentation "String is already in the table."))

(defun string-table-count (string-table)
  "Number of distinct strings in the table."
  (hash-table-count (string-table-table string-table)))

(defun string-table-strings (string-table)
  "List of strings in STRING-TABLE."
  (hash-table-keys (string-table-table string-table)))

(defmethod print-object ((string-table string-table) stream)
  (let+ (((&accessors-r/o (strings string-table-strings)
                          (count string-table-count)) string-table))
    (print-unreadable-object (string-table stream :type t)
      (format stream "with ~D strings" count)
      (loop for string in strings
            do (format stream "    ~S" string)))))

(declaim (inline string-table-get (setf string-table-get)))

(defun string-table-get (string-table string)
  "Synonym for GETHASH, used internally."
  (gethash string (string-table-table string-table)))

(defun (setf string-table-get) (value string-table string)
  "Synonym for (SETF GETHASH), used internally, checks that STRING is a string."
  (check-type string string)
  (setf (gethash string (string-table-table string-table)) value))

(defun string-table-lookup (string-table string)
  "Return the value corresponding to STRING in STRING-TABLE, or raise the STRING-TABLE-NOT-FOUND error."
  (let+ (((&values value present?) (string-table-get string-table string)))
    (assert present? () 'string-table-not-found)
    value))

(defun string-table-add (string-table string &optional (value string))
  "Add STRING mapped to VALUE to STRING-TABLE, raising STRING-TABLE-DUPLICATE if STRING is already in the table.  Return VALUE. "
  (let+ (((&values &ign present?) (string-table-get string-table string)))
    (assert (not present?) () 'string-table-duplicate)
    (setf (string-table-get string-table string) value)))

(defun string-table-intern (string-table string &optional (new-value string))
  "If STRING is already in STRING-TABLE, return its value, otherwise add it and return NEW-VALUE.  When used with the default argument for NEW-VALUE, EQUAL strings are always mapped to values that are EQ."
  (let+ (((&values value present?) (string-table-get string-table string)))
    (if present?
        value
        (setf (string-table-get string-table string) new-value))))
