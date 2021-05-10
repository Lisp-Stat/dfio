;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO.DECIMAL -*-
;;; (c) 2021 Symbolics Pte. Ltd. All rights reserved.

(cl:in-package #:dfio.decimal)

;;; conditions

(define-condition parse-rational-error (error)
  ((string :initform :string)
   (message :initform :message))
  (:report (lambda (condition stream)
             (let+ (((&slots-r/o string message) condition))
               (format stream "Could not parse ~A as a real number: ~A."
                       string message))))
  (:documentation "Error used by parse-rational and parse-real."))

(defun gobble-positive-integer (string start end)
  "If (SUBSEQ STRING START END) starts with a nonnegative integer (ie a sequence of digits 0-9), return the integer and position at which it ends as two values.

Otherwise, return NIL and 0.

START < END has to hold, END cannot be NIL.  Consequences are undefined when START >= END."
  (let+ ((result 0)
         ((&flet done (index)
            (return-from gobble-positive-integer
              (if (eql index 0)
                  (values nil 0)
                  (values result index))))))
    (loop for index from start below end
          do (aif (digit-char-p (char string index))
                  (setf result (+ it (* result 10)))
                  (done index)))
    (done end)))

(defun gobble-sign (string start)
  "Return (values SIGNUM INDEX), where SIGNUM is -1 or 1 depending on whether (CHAR STRING START) was a sign, and INDEX is the index of the subsequent character (START or START+1)."
  (case (char string start)
    (#\+ (values 1 (1+ start)))
    (#\- (values -1 (1+ start)))
    (t (values 1 start))))

(defparameter +exponent-chars+ "defslDEFSL"
  "Default exponent characters.")

(defun parse-rational (string &key (start 0) (end nil)
                                   (exponent-chars +exponent-chars+))
  "Parse a decimal rational in (subseq string start end) of the form [sign][whole][.[fraction]][exponent] where

sign  ::= + | - | empty
whole ::= digit*
fraction ::= digit*
exponent ::= exponent-char[sign]digit+

with the restriction that WHOLE and FRACTION cannot be empty at the same time.  EXPONENT-CHAR is a string and contains the valid exponent chars.

Whitespace is NOT trimmed, and leads to an error.  In case of a parsing failure, PARSE-RATIONAL-ERROR is used.

Return (values NUMBER DECIMAL-DOT? EXPONENT-CHAR).  NUMBER is a RATIONAL, DECIMAL-DOT? is T when a decimal dot is present, otherwise NIL, EXPONENT-CHAR contains the exponent character, NIL if not present.

Numbers of the form .112 and 112. are valid syntax, representing 0.112 and 112.0, respectively.

Examples:

  (parse-rational \"7\")    => (values 7 NIL NIL)
  (parse-rational \"7.\")   => (values 7 T NIL)
  (parse-rational \"0.7\")  => (values 7/10 T NIL)
  (parse-rational \".7\")   => (values 7/10 T NIL)
  (parse-rational \"7.e2\") => (values 700 T #\e)
  (parse-rational \".7d1\") => (values 7 T #\d)
"
  (let+ ((end (or end (length string)))
         ((&macrolet when-end (&body body)
            `(when (= start end)
               ,@body)))
         sign
         whole
         decimal-dot?
         fraction fraction-digits
         exponent-char exponent-sign exponent
         ((&flet report-error (message)
            (error 'parse-rational-error :string string
                                         :message message)))
         ((&flet pow10 (power)
            (check-type power integer)
            (expt 10 power)))
         ((&flet make-real ()
            (let ((mantissa
                    (* sign
                       (cond
                         ((and whole fraction)
                          (+ whole (* fraction (pow10 (- fraction-digits)))))
                         (whole whole)
                         (fraction (* fraction (pow10 (- fraction-digits))))
                         (t (report-error "no digits around decimal dot"))))))
              (return-from parse-rational
                (values (if exponent
                            (* mantissa (pow10 (* exponent-sign exponent)))
                            mantissa)
                        decimal-dot?
                        exponent-char))))))
    (unless (< start end)
      (report-error "empty string"))
    ;; sign
    (setf (values sign start) (gobble-sign string start))
    (when-end (report-error "sign without digits"))
    ;; whole
    (setf (values whole start) (gobble-positive-integer string start end))
    (when-end (make-real))
    ;; fraction
    (when (char= (char string start) #\.)
      (setf decimal-dot? t)
      (incf start)
      (when-end (make-real))
      (let+ (((&values fraction% end%)
              (gobble-positive-integer string start end)))
        (when fraction%
          (setf fraction fraction%
                fraction-digits (- (or end% (length string)) start)
                start end%)))
      (when-end (make-real)))
    ;; exponent
    (let ((maybe-exponent-char (char string start)))
      (when (find maybe-exponent-char exponent-chars)
        (setf exponent-char maybe-exponent-char)
        (incf start)
        (when-end (report-error "no characters after exponent character"))
        (setf (values exponent-sign start) (gobble-sign string start))
        (when-end (report-error "no characters after exponent character or its sign"))
        (setf (values exponent start) (gobble-positive-integer string start end))
        (unless exponent
          (report-error "missing exponent"))
        (when-end (make-real))))
    (when-end (make-real))
    (error 'parse-rational-error :string string :message "trailing junk")))

(defun parse-real (string &key (start 0) (end nil)
                               (s-float 'short-float)
                               (f-float 'single-float)
                               (d-float 'double-float)
                               (l-float 'long-float)
                               (e-float *read-default-float-format*))
  "Wrapper for PARSE-RATIONAL, converting non-integers to floats.  The float type is determined by the -float arguments for each exponent character.  Integers are not converted to floats.  Return a single value, type of (or integer float).

See PARSE-RATIONAL for accepted formats, errors, etc."
  (let+ (((&values real decimal-dot? exponent-char)
          (parse-rational string :start start
                                 :end end
                                 :exponent-chars +exponent-chars+)))
    (if exponent-char
        (coerce real
                (ecase (char-downcase exponent-char)
                  (#\s s-float)
                  (#\f f-float)
                  (#\d d-float)
                  (#\l l-float)
                  (#\e e-float)))
        (if decimal-dot?
            (coerce real e-float)
            real))))
