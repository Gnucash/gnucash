(debug-set! stack 50000)
;(use-modules (gnucash report new-reports reports-2))

(use-modules (gnucash gnc-module))

;; Guile 2 needs to load external modules at compile time
;; otherwise the N_ syntax-rule won't be found at compile time
;; causing the test to fail
;; That's what the wrapper below is meant for:
(cond-expand
   (guile-2
    (define-syntax-rule (begin-for-syntax form ...)
      (eval-when (load compile eval expand) (begin form ...))))
   (else
    (define begin-for-syntax begin)))

(begin-for-syntax (gnc:module-load "gnucash/report/report-system" 0))
(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash report report-system test test-extras))

(use-modules (gnucash report standard-reports test test-generic-net-barchart))
(use-modules (gnucash report standard-reports net-barchart))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (run-net-asset-income-test net-worth-barchart-uuid income-expense-barchart-uuid))

