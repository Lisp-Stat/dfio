;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: DFIO-TESTS -*-
;;; (c) 2021 Symbolics Pte. Ltd. All rights reserved.

(cl:defpackage #:dfio-tests
  (:use #:cl
        #:alexandria
        #:clunit
        #:dfio.decimal
        #:dfio.string-table
        #:dfio.data-column
        #:dfio
        #:let-plus)
  (:import-from #:nu #:as-plist)
  (:export #:run))

(in-package #:dfio-tests)



;;; interface

(defsuite dfio-tests ())

(defparameter *iterations* 10000
  "Number of iterations for random tests.")

(defun run (&optional interactive?)
  (let ((*iterations* (if interactive?
                          100
                          *iterations*)))
    (run-suite 'dfio-tests :use-debugger interactive?)))

;;; decimals

(defsuite decimal-tests (dfio-tests))

(defun random-sign ()
  "Return a random sign (+,-) or an empty string."
  (ecase (random 3)
    (0 "")
    (1 "+")
    (2 "-")))

(defun random-signless (digits)
  "Random sequence of digits, representing an integer.  DIGITS suggests a total length for nonzero digits, but it is random, padded with a random number of zeroes.  For testing.  When digits is NIL, an empty string is returned."
  (if digits
      (progn
        (check-type digits (integer 1))
        (format nil "~v,'0D"
                (random (* 2 digits))
                (random (expt 10 digits))))
      ""))

(defun random-integer-string (digits)
  "Return (cons VALUE STRING) such that (PARSE-REAL STRING) is expected to return VALUE, which is an integer.

See RANDOM-SIGNLESS for the semantics of DIGITS, the result of (RANDOM-SIGN) is prepended."
  (let ((string (concatenate 'string (random-sign) (random-signless digits))))
    (cons (parse-integer string) string)))

(defun random-float-string (&key (exponent-char #\d)
                                     (whole-digits 6)
                                     (fraction-digits 6)
                                     (exponent-digits 2))
  "Return (cons VALUE STRING) such that (PARSE-REAL STRING) is expected to give VALUE.  Useful for testing.

STRING represents a number, randomly generated according to the following rules:

  - Whole and fractional parts are generated using RANDOM-SIGNLESS called with WHOLE-DIGITS and FRACTION-DIGITS, prepended by a random sign.

  - EXPONENT-DIGITS has similar semantics.  EXPONENT-CHAR is used before the exponent (if applicable)."
  (assert (or whole-digits fraction-digits) ()
          "No digits around the decimal dot.")
  (let+ (((&flet cat (&rest strings)
            (apply #'concatenate 'string strings)))
         (whole-sign (random-sign))
         (whole (random-signless whole-digits))
         (fraction (random-signless fraction-digits))
         (exponent (if exponent-digits
                       (format nil "~C~A~A"
                               exponent-char
                               (random-sign)
                               (random-signless exponent-digits))
                       ""))
         (string (cat whole-sign whole "." fraction exponent))
         (value (read-from-string
                 (concatenate 'string
                              (cat whole-sign
                                   (if whole-digits
                                       whole
                                       "0"))
                              "."
                              (if fraction-digits
                                  fraction
                                  "0")
                              exponent))))
    (cons value string)))

(defmacro random-parse-test (form)
  "Evaluates FORM repeatedly, using the resulting (cons VALUE STRING) to test PARSE-REAL."
  `(loop repeat *iterations*
         do (let+ (((value . string) ,form))
              (assert-eql value (parse-real string)
                value string))))

(deftest parse-real-test (decimal-tests)
  (random-parse-test (random-float-string)))

(deftest parse-real-test-noexp (decimal-tests)
  (random-parse-test (random-float-string :exponent-digits nil)))

(deftest parse-real-test-nowhole (decimal-tests)
  (random-parse-test (random-float-string :whole-digits nil)))

(deftest parse-real-test-nofrac (decimal-tests)
  (random-parse-test (random-float-string :fraction-digits nil)))

(deftest parse-real-test-integer (decimal-tests)
  (random-parse-test (random-integer-string 5)))

(deftest parse-rational-errors (decimal-tests)
  (assert-condition parse-rational-error (parse-rational ""))     ; empty
  (assert-condition parse-rational-error (parse-rational "junk")) ; junk
  (assert-condition parse-rational-error (parse-rational "1..2"))
  (assert-condition parse-rational-error (parse-rational "  12  "))
  (assert-condition parse-rational-error (parse-rational "1.-2")))



;;; string-table

(defsuite string-table-tests (dfio-tests))

(deftest string-table-basic-test (string-table-tests)
  (let ((st (string-table)))
    (assert-equal 0 (string-table-count st))
    (assert-condition string-table-not-found (string-table-lookup st "foo"))
    (assert-equal 0 (string-table-count st))
    (string-table-add st "bar")
    (assert-equal 1 (string-table-count st))
    (assert-condition string-table-duplicate (string-table-add st "bar"))))

(deftest string-table-intern-test (string-table-tests)
  (let+ ((st (string-table))
         (strings '("foo" "bar" "foo" "baz" "bar"))
         (interned-strings (mapcar (curry #'string-table-intern st) strings))
         ((foo1 bar1 foo2 &ign bar2) interned-strings))
    (assert-eql 3 (string-table-count st))
    (assert-eq foo1 foo2)
    (assert-eq bar1 bar2)
    (assert-equal strings interned-strings)))

(deftest string-table-map-test (string-table-tests)
  (let+ ((st (string-table))
         (alist '(("foo" . foo) ("bar" . bar) ("baz" . baz))))
    (mapc (lambda+ ((string . symbol))
            (assert-eq symbol (string-table-add st string symbol)))
          alist)
    (assert-equal 3 (string-table-count st))
    (mapc (lambda+ ((string . symbol))
            (assert-eq symbol (string-table-lookup st string)))
          alist)))



;;; data-column

(defsuite data-column-tests (dfio-tests))

(deftest data-column-basic-test (data-column-tests)
  (let* ((e-float 'double-float)
         (dc (data-column :map-alist '(("" . missing)
                                       ("NA" . not-available))
                          ))
         (strings #("male" "female" "male" "male" "female"
                    "112.7" "99" "28" "1e2" "1e-2"
                    "" "NA" "NA" "" ""))
         (added-elements (map 'vector (curry #'data-column-add dc)
                              strings)))
    (assert-equalp #("male" "female" "male" "male" "female"
                     112.7d0 99d0 28d0 100d0 0.01d0
                     missing not-available not-available missing missing)
        (data-column-vector dc))
    (assert-equalp added-elements (data-column-vector dc))
    (let+ (((e0 e1 e2 e3 e4 &rest &ign)
            (coerce (data-column-vector dc) 'list)))
      (assert-eq e0 e2)
      (assert-eq e0 e3)
      (assert-eq e1 e4))))



;;; data-frame

(defsuite csv-tests (dfio-tests))

(deftest csv-reading-basic (csv-tests)
  (let ((df (read-csv
             "Index,Gender,Age
0,\"Male\",30
1,\"Female\",31
2,Male,\"32\"
"
	     :column-keys-or-function #'string-to-keyword
	     )))
    (assert-equalp '(:index #(0 1 2)
                     :gender #("Male" "Female" "Male")
                     :age #(30 31 32))
		   (nu:as-plist df))))

(deftest csv-reading-with-row-names (csv-tests)
  (let ((df (read-csv
             "Not,the,original
0,\"Male\",30
1,\"Female\",31
2,Male,\"32\"
" :column-keys-or-function '(index gender age))))
    (assert-equalp '(index #(0 1 2)
                     gender #("Male" "Female" "Male")
                     age #(30 31 32))
        (nu:as-plist df))))

(deftest csv-writing-basic (csv-tests)
  (let ((df (read-csv
             "Index,Gender,Age
0,\"Male\",30
1,\"Female\",31
2,Male,\"32\"
")))
    (assert-equalp
	"INDEX,GENDER,AGE
0,Male,30
1,Female,31
2,Male,32
"
	(remove #\ (write-csv df :add-first-row t))))) ; remove CR if on windows

