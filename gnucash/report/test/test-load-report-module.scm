(define exit-code 0)
(setenv "GNC_UNINSTALLED" "1")

(display "  testing report module load ... ")
(use-modules (gnucash report))

(if (defined? 'gnc:define-report)
    (display "Procedure gnc:define-report found\n")
    (begin
      (display "Failed - procedure gnc:define-report not found\n")
      (set! exit-code -1)))

(if (defined? 'gnc:make-html-chart)
    (display "Procedure gnc:make-html-chart found\n")
    (begin
      (display "Failed - procedure gnc:make-html-chart not found\n")
      (set! exit-code -1)))

(exit exit-code)
