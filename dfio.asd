;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "dfio"
  :version     "1.0.1"
  :licence     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Data frame I/O"
  :description "Common Lisp library for reading and writing data-frames"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  ;:homepage    "https://lisp-stat.dev/..."
  :source-control (:git "https://github.com/Lisp-Stat/dfio.git")
  :bug-tracker "https://github.com/Lisp-Stat/dfio/issues"

  :depends-on ("alexandria"
               "anaphora"
               "data-frame"
	       #-genera "dexador"
               "fare-csv"
               "let-plus")
  :serial t
  :components ((:file "pkgdcl")
	       (:file "decimal")
               (:file "string-table")
               (:file "data-column")
	       (:file "utils")
	       (:file "write")
               (:file "delimited-text"))
  :in-order-to ((test-op (test-op "dfio/tests"))))

(defsystem "dfio/tests"
  :version     "1.0.0"
  :description "Unit tests for DFIO."
  :author      "Steve Nunez <steve@symbolics.tech>"
  :depends-on ("dfio"
               "clunit2")
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :dfio-tests
						 :dfio-tests)))))
