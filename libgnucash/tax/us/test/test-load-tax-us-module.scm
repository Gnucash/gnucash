(display "  testing US tax info module load ... ")
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(if (gnc:module-load "gnucash/tax/us" 0)
    (begin 
      (display "ok\n")
      (exit 0))
    (begin 
      (display "failed\n")
      (exit -1)))
