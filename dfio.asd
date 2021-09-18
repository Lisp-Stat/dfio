;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "dfio"
  :version     (:read-file-form "version.sexp")
  :description "Common Lisp library for reading and writing data-frames"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     :MS-PL
  :depends-on ("alexandria"
               "anaphora"
               #-genera "cl-csv"
               "data-frame"
               "let-plus")
  :serial t
  :components ((:file "pkgdcl")
	       (:file "decimal")
               (:file "string-table")
               (:file "data-column")
	       (:file "utils")
	       (:file "write")
               #-genera (:file "delimited-text")
	       )
  :in-order-to ((test-op (test-op "dfio/tests"))))

(defsystem "dfio/json"
  :description "Serialise/deserialise data-frames to various JSON formats"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("dfio"
	       "yason")
  :serial t
  :components ((:file "vega-lite")))

(defsystem "dfio/tests"
  :description "Unit tests for DFIO."
  :author      "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :depends-on ("dfio"
               "clunit2")
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
		    (symbol-call :clunit :run-suite
				 (find-symbol* :dfio-tests
					       :dfio-tests))))
