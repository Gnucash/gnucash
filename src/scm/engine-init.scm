;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  engine-init.scm
;;;  make sure the engine is initialized at gnucash startup
;;;
;;;  Bill Gribble <grib@billgribble.com> 4 Aug 2000
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize commodity tables in the engine
(gnc:load-iso-4217-currencies)
(gnc:setup-default-namespaces)
