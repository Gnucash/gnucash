(setenv "GNC_UNINSTALLED" "1")
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(display "  testing gnome-utils module load ... ")
(if (gnc:module-load "gnucash/gnome-utils" 0)

   (begin 
      (display "ok\n")
      (exit 0))
    (begin 
      (display "failed\n")
      (exit 1)))

