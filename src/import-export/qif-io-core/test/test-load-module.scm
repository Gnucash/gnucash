
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(define (run-test)
  (if (gnc:module-load "gnucash/qif-io/core" 0)
    (begin 
      (display "ok\n")
      (exit 0))
    (begin 
      (display "failed\n")
      (exit -1))))
