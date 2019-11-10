(define exit-code 0)
(setenv "GNC_UNINSTALLED" "1")
(use-modules (gnucash core-utils))

(if (macro? (module-ref (current-module) 'N_))
    (display "Macro N_ defined\n")
    (begin
      (display "Failed - macro N_ not defined\n")
      (set! exit-code -1)))

(if (string=? (N_ "foobar") "foobar")
    (display "Macro N_ works properly\n")
    (begin
      (display "Failed - macro N_ doesn't work\n")
      (set! exit-code -1)))

(exit exit-code)
