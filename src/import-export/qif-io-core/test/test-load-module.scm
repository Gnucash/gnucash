
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(define (run-test)
  (gnc:module-load "gnucash/qif-io/core" 0))

