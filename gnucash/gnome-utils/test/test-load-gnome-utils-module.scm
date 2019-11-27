(define exit-code 0)
(setenv "GNC_UNINSTALLED" "1")

(display "  testing gnome-utils module load ... ")
(use-modules (gnucash gnome-utils))

(if (defined? 'gnc:make-menu)
    (display "Procedure gnc:make-menu found\n")
    (begin
      (display "Failed - procedure gnc:make-menu not found\n")
      (set! exit-code -1)))

(if (defined? 'gnc:set-ui-status)
    (display "Procedure gnc:set-ui-status found\n")
    (begin
      (display "Failed - procedure gnc:set-ui-status not found\n")
      (set! exit-code -1)))

(exit exit-code)
