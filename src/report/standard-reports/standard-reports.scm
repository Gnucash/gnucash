;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports))

(export gnc:show-register-report)
(export gnc:print-register-report)
(export gnc:show-invoice-report)

(use-modules (gnucash report account-piecharts))
(use-modules (gnucash report account-summary))
(use-modules (gnucash report average-balance))
(use-modules (gnucash report balance-sheet))
(use-modules (gnucash report category-barchart))
(use-modules (gnucash report net-barchart))
(use-modules (gnucash report payables))
(use-modules (gnucash report pnl))
(use-modules (gnucash report portfolio))
(use-modules (gnucash report price-scatter))
(use-modules (gnucash report register))
(use-modules (gnucash report transaction))

(define (gnc:show-register-report . rest)
  (apply gnc:apply-register-report
         (cons gnc:report-window (cons #f rest))))

(define (gnc:print-register-report . rest)
  (apply gnc:apply-register-report
         (cons gnc:print-report (cons #f rest))))

(define (gnc:show-invoice-report . rest)
  (apply gnc:apply-register-report
         (cons gnc:report-window (cons #t rest))))
