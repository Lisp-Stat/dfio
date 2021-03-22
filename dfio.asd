;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:dfio
  :version     (:read-file-form "version.sexp")
  :description "Common Lisp library for reading data from text files (eg CSV)."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     :MS-PL
  :depends-on (#:alexandria
               #:anaphora
               #:cl-csv
               #:data-frame
               #:let-plus)
  :serial t
  :components ((:file "decimal")
               (:file "string-table")
               (:file "data-column")
               (:file "dfio"))
  :in-order-to ((test-op (test-op "dfio/tests"))))

(asdf:defsystem #:dfio/tests
  :description "Unit tests for DFIO."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :depends-on (#:dfio
	       #:dfio/json
               #:clunit)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
		    (uiop:symbol-call :clunit :run-suite
				      (uiop:find-symbol* :dfio-tests
							 :dfio-tests))))
