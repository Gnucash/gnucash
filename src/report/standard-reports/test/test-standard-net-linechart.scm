(debug-set! stack 50000)
;(use-modules (gnucash report new-reports reports-2))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash report report-system test test-extras))

(use-modules (gnucash report standard-reports test test-generic-net-linechart))
(use-modules (gnucash report standard-reports net-linechart))

(define (run-test)
  (run-net-asset-test net-worth-linechart-uuid))

