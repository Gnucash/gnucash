(debug-set! stack 50000)
(use-modules (ice-9 format))
(use-modules (ice-9 streams))
(use-modules (srfi srfi-1))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash printf))
(use-modules (gnucash report report-system))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash report standard-reports net-barchart))

(use-modules (gnucash report report-system test test-extras))

(use-modules (gnucash report standard-reports test test-generic-category-report))
(use-modules (gnucash report standard-reports category-barchart))

(define (run-test)
  (run-category-income-expense-test category-barchart-income-uuid category-barchart-expense-uuid)
  (run-category-asset-liability-test category-barchart-asset-uuid category-barchart-liability-uuid))
