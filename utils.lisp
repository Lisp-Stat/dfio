;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:dfio)

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


