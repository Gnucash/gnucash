
(define-module (gnucash import-export binary-import))
(use-modules (g-wrapped gw-binary-import))
(use-modules (gnucash app-utils))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

(define (import-commodities session)
  (let ((book-url (gnc:session-get-url session)))
    (gnc:import-legacy-commodities book-url)))

(gnc:hook-add-dangler gnc:*book-opened-hook* import-commodities)
