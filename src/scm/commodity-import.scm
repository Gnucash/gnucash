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
  (display "importing old currencies:\n")
  (if (gnc:commodity-table-has-namespace 
       (gnc:engine-commodities)
       "GNC_LEGACY_CURRENCIES")
      (begin 
        (display "Legacy namespace exists.\n")
        (let ((commodities 
               (gnc:commodity-table-get-commodities 
                (gnc:engine-commodities)
                "GNC_LEGACY_CURRENCIES")))
          (for-each 
           (lambda (commodity)
             (write (gnc:commodity-get-printname commodity))
             (newline))
           commodities))
        (display "Loading conversion dialog...\n")
        (gnc:commodity-import-legacy-druid from-filename))
      (display "No legacy namespace found.\n")))

(gnc:hook-add-dangler gnc:*file-opened-hook* import-old-currencies)

