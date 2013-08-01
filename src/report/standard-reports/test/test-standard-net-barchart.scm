(debug-set! stack 50000)
;(use-modules (gnucash report new-reports reports-2))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash report report-system test test-extras))

(use-modules (gnucash report standard-reports test test-generic-net-barchart))
(use-modules (gnucash report standard-reports net-barchart))

(define (run-test)
  (run-net-asset-income-test net-worth-barchart-uuid income-expense-barchart-uuid))

