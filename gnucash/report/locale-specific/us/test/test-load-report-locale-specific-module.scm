(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(display "  testing US locale-specific report module load ... ")
(gnc:module-load "gnucash/app-file" 0)

(display "  (done with precursor) ... ")
(if (gnc:module-load "gnucash/report/locale-specific/us" 0)
    (begin 
      (display "ok\n")
      (exit 0))
    (begin 
      (display "failed\n")
      (exit -1)))
