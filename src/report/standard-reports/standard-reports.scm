;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports))

(export gnc:register-report-create)

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash report account-piecharts))
(use-modules (gnucash report account-summary))
(use-modules (gnucash report average-balance))
(use-modules (gnucash report balance-sheet))
(use-modules (gnucash report category-barchart))
(use-modules (gnucash report net-barchart))
(use-modules (gnucash report pnl))
(use-modules (gnucash report portfolio))
(use-modules (gnucash report price-scatter))
(use-modules (gnucash report payables))
(use-modules (gnucash report receivables))
(use-modules (gnucash report register))
(use-modules (gnucash report transaction))

(define gnc:register-report-create gnc:register-report-create-internal)
