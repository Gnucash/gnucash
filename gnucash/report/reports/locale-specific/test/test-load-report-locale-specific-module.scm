(setenv "GNC_UNINSTALLED" "1")
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(display "  testing US locale-specific report module load ... ")

(display "  (done with precursor) ... ")
(if (gnc:module-load "gnucash/tax/us" 0)
    (begin 
      (display "ok\n")
      (exit 0))
    (begin 
      (display "failed\n")
      (exit -1)))
