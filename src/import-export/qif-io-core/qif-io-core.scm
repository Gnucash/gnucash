;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io-core.scm
;;  top-level module for QIF i/o code
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash import-export qif-io-core))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 regex))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/engine" 0)
(gnc:module-load "gnucash/app-utils" 0)

(load-from-path "qif-parse.scm")
(load-from-path "qif-format-check.scm")
(load-from-path "qif-file.scm")
(load-from-path "qif-objects.scm")
(load-from-path "qif-record-xform.scm")
(load-from-path "qif-bank-xtn-import.scm")
(load-from-path "qif-invst-xtn-import.scm")
(load-from-path "qif-acct-table.scm")

;; qif-parse.scm
(export qif-io:parse-category)
(export qif-io:parse-year)
(export qif-io:parse-acct-type)
(export qif-io:parse-bang-field)
(export qif-io:parse-action-field)
(export qif-io:parse-cleared-field)
(export qif-io:check-date-format)
(export qif-io:parse-date/format)
(export qif-io:check-number-format)
(export qif-io:check-multi-number-format)
(export qif-io:parse-number/format)

;; qif-format-check.scm
(export qif-io:setup-data-formats)
(export qif-io:check-possible-formats)

;; qif-file.scm
(export qif-io:read-file)
(export qif-io:write-file)
(export qif-io:read-record)
(export qif-io:write-record)

;; qif-objects.scm
(export qif-io:make-file)
(export qif-io:make-empty-file)
(export qif-io:file-bank-xtns)
(export qif-io:file-invst-xtns)
(export qif-io:file-bank-xtn-format)
(export qif-io:file-invst-xtn-format)
(export qif-io:file-set-bank-xtn-format!)
(export qif-io:file-set-invst-xtn-format!)
(export qif-io:file-xtns-need-acct?)
(export qif-io:file-set-default-src-acct!)

(export qif-io:make-empty-acct-table)
(export qif-io:acct-table-accounts)
(export qif-io:acct-table-categories)
(export qif-io:acct-table-securities)
(export qif-io:acct-table-brokerage-accts)

;; qif-record-xform.scm
(export qif-io:record->bank-xtn)
(export qif-io:record->invst-xtn)
(export qif-io:record->account)
(export qif-io:record->category)
(export qif-io:record->class)
(export qif-io:record->security)

(export qif-io:bank-xtn->record)
(export qif-io:invst-xtn->record)
(export qif-io:account->record)
(export qif-io:category->record)
(export qif-io:class->record)
(export qif-io:security->record)

;; qif-bank-xtn-import.scm
(export qif-io:bank-xtn-import)
(export qif-io:invst-xtn-import)

;; acct-table.scm 
(export qif-io:acct-table-lookup)
(export qif-io:acct-table-insert!)
(export qif-io:acct-table-make-gnc-acct-tree)

;; from main
(export simple-format)
