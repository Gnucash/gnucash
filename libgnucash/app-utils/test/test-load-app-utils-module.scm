(define exit-code 0)
(setenv "GNC_UNINSTALLED" "1")

(use-modules (gnucash app-utils))

(cond
    ((defined? 'gnc:apply-with-error-handling)
        (display "Procedure gnc:apply-with-error-handling found\n"))
    (else
        (display "Failed - procedure gnc:apply-with-error-handling not found\n")
        (set! exit-code -1)))

(cond
    ((defined? 'gnc-default-currency)
        (display "Procedure gnc-default-currency found\n"))
    (else
        (display "Failed - procedure gnc-default-currency not found\n")
        (set! exit-code -1)))

(exit exit-code)
