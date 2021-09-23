;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

;;; Conversions to/from systems with other conventions
;;; See Postmodern for a more extensive set of conversion utilities if these don't do what you want.
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
  "Map STRING to a symbol in PACKAGE, replacing #\., #\_ and #\space with a #\-, and upcasing all other characters. Exports symbol."
  (let* ((sym (cond ((string= string "")
		     (warn "Missing column name was filled in")
		     (gentemp "X"))
		    (t (intern
			(map 'string
			     (lambda (character)
			       (case character
				 ((#\. #\_ #\space) #\-)
				 (otherwise (char-upcase character))))
			     string))))))
    (export sym)
    sym))

(defun symbol-name-to-pathname (string)
  "Map the symbol-name of S to something that can be part of a logical-pathname"
  (map 'string
       (lambda (character)
	 (case character
	   ((#\. #\_ #\space) #\-)
	   (otherwise (char-upcase character))))
	   string))


;;; I/O stream simplification

;;; Make it so a STRING, STREAM or FILE can be passed to a function that requires a STREAM
;;; Parts of this originally came from cl-csv.

(defparameter *default-external-format* :default
  "External format used for opening files")

(deftype str-strm-file ()
  '(or null stream pathname)
  "A string, stream or a file path")

(defun %in-stream (stream-or-string)
  (typecase stream-or-string
    (string (make-string-input-stream stream-or-string))
    (stream stream-or-string)
    (pathname (values (open stream-or-string :external-format *default-external-format*)
                      t))))

(defmacro with-csv-input-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%in-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))

(defmacro with-csv-output-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%out-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))

(defun %out-stream (stream-or-string)
  "creates a stream from the given thing, trying to DWIM"
  (etypecase stream-or-string
    (null (make-string-output-stream))
    (stream stream-or-string)
    (pathname
     (values
      (open stream-or-string :direction :output :if-exists :supersede)
      t))))

