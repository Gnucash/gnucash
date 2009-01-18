;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports))
(use-modules (ice-9 slib))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.

(export gnc:register-report-create)
(export gnc:register-report-hook)

(require 'hash-table)

(define gnc:*register-report-hash* (make-hash-table 23))

;; Keep a hash-table of records, keyed off the account type.  Each
;; record contains a function pointer for that account-type with split
;; or without split.  If no function is found, then run the 'default'
;; function

(define acct-type-info (make-record-type "AcctTypeInfo" '(split non-split)))

(define make-acct-type-private
  (record-constructor acct-type-info '(split non-split)))

(define (make-acct-type)
  (make-acct-type-private #f #f))

(define get-split
  (record-accessor acct-type-info 'split))

(define set-split
  (record-modifier acct-type-info 'split))

(define get-non-split
  (record-accessor acct-type-info 'non-split))

(define set-non-split
  (record-modifier acct-type-info 'non-split))

(define (gnc:register-report-hook acct-type split? create-fcn)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))

    (if (not type-info)
	(set! type-info (make-acct-type)))

    (if split?
	(set-split type-info create-fcn)
	(set-non-split type-info create-fcn))

    (hash-set! gnc:*register-report-hash* acct-type type-info)))

(define (lookup-register-report acct-type split)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))
    (gnc:debug "acct-type: " acct-type)
    (gnc:debug "ref: " type-info)
    (gnc:debug "hash: " gnc:*register-report-hash*)
    (gnc:debug "split: " split)
    (if type-info
	(if (and split (not (null? split)))
	    (begin (gnc:debug "get-split...") (get-split type-info))
	    (begin (gnc:debug "get-non-split...") (get-non-split type-info)))
	#f)))

(use-modules (gnucash report account-piecharts))
(use-modules (gnucash report account-summary))
(use-modules (gnucash report advanced-portfolio))
(use-modules (gnucash report average-balance))
(use-modules (gnucash report balance-sheet))
(use-modules (gnucash report equity-statement))
(use-modules (gnucash report general-journal))
(use-modules (gnucash report general-ledger))
(use-modules (gnucash report cash-flow))
(use-modules (gnucash report budget))
(use-modules (gnucash report budget-balance-sheet))
(use-modules (gnucash report budget-barchart))
(use-modules (gnucash report budget-flow))
(use-modules (gnucash report budget-income-statement))
(use-modules (gnucash report category-barchart))
(use-modules (gnucash report daily-reports))
(use-modules (gnucash report net-barchart))
(use-modules (gnucash report income-statement))
(use-modules (gnucash report portfolio))
(use-modules (gnucash report price-scatter))
(use-modules (gnucash report register))
(use-modules (gnucash report trial-balance))
(use-modules (gnucash report transaction))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

(define (gnc:register-report-create account split query journal? double?
				    title debit-string credit-string)
  (let* ((acct-type (xaccAccountGetType account))
	 (create-fcn (lookup-register-report acct-type split)))
    (gnc:debug "create-fcn: " create-fcn)
    (if create-fcn
	(create-fcn account split query journal? double? title
		    debit-string credit-string)
	(gnc:register-report-create-internal #f query journal? double? title
					     debit-string credit-string))))
