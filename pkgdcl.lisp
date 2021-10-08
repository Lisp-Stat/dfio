;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:dfio.decimal
  (:use #:cl #:anaphora #:let-plus)
  (:export
   #:parse-rational-error
   #:parse-rational
   #:parse-real))

(uiop:define-package #:dfio.string-table
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:string-table
   #:string-table-not-found
   #:string-table-duplicate
   #:string-table-count
   #:string-table-strings
   #:string-table-lookup
   #:string-table-add
   #:string-table-intern))

(uiop:define-package #:dfio.data-column
  (:use #:cl
        #:anaphora
        #:dfio.decimal
        #:dfio.string-table
        #:let-plus)
  (:export
   #:data-column
   #:data-column-add
   #:data-column-counts
   #:data-column-vector))

(uiop:define-package #:dfio
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
	#:data-frame
        #:dfio.data-column)
  (:import-from #:num-utils
		#:as-alist)
  (:export

   ;; Utility functions
   #:string-to-keyword
   #:string-to-symbol
   #:symbol-name-to-pathname

   ;; Text delimited format
   #:read-csv
   #:write-csv

   ;; Lisp format
   #:write-df
   #:write-properties
   #:save


   ;; JSON (Vega-Lite) format
   #:read-vl				;Read vega-lite data into df
   #:df-to-vl				;Write df data into vega-lite array
   #:df-to-alist))			;Write df data to alist for further encoding via JSON library



