
(define-module (gnucash import-export binary-import))
(use-modules (g-wrapped gw-binary-import))
(use-modules (gnucash app-utils))

(gnc:hook-add-dangler gnc:*book-opened-hook* gnc:import-legacy-commodities)
