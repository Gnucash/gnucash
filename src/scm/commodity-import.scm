;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  commodity-import.scm
;;;  file-io hooks to convert old-style currency strings to 
;;;  real gnucash commodities.
;;;
;;;  Bill Gribble <grib@billgribble.com> 11 Aug 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "commodity-import.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  import-old-currencies
;;  If there are old currencies in the account group, start the 
;;  import wizard. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (import-old-currencies from-filename) 
  (if (gnc:commodity-table-has-namespace  (gnc:engine-commodities)
                                          "GNC_LEGACY_CURRENCIES")
      (gnc:import-legacy-commodities from-filename)))

(gnc:hook-add-dangler gnc:*book-opened-hook* import-old-currencies)
