;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  business-reports.scm
;;  load the business report definitions
;;
;;  Copyright (c) 2002 Derek Atkins <derek@ihtfp.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report business-reports))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/standard-reports" 0)

(export gnc:invoice-report-create)

(use-modules (gnucash report invoice))
(use-modules (gnucash report owner-report))

(define gnc:invoice-report-create gnc:invoice-report-create-internal)

(export gnc:owner-report-create)
