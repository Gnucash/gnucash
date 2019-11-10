(define exit-code 0)
(setenv "GNC_UNINSTALLED" "1")
(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(gnc:module-begin-syntax (define loaded-module (gnc:module-load "gnucash/app-utils" 0)))
(if loaded-module
    (display "Module gnucash/app-utils loaded successfully\n")
    (begin
      (display "Failed - module gnucash/app-utils not loaded successfully\n")
      (set! exit-code -1)))

(if (procedure? gnc:apply-with-error-handling)
    (display "Procedure gnc:apply-with-error-handling found\n")
    (begin
      (display "Failed - procedure gnc:apply-with-error-handling not found\n")
      (set! exit-code -1)))

(if (procedure? gnc-default-currency)
    (display "Procedure gnc-default-currency found\n")
    (begin
      (display "Failed - procedure gnc-default-currency not found\n")
      (set! exit-code -1)))

(exit exit-code)
