;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-import.scm
;;;  virtual loader for QIF import facility 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import.scm")

(gnc:depend "simple-obj.scm")
(gnc:depend "qif-objects.scm")       ;; class definitions 
(gnc:depend "qif-parse.scm")         ;; string-to-value, date parsing
(gnc:depend "qif-utils.scm")         
(gnc:depend "qif-file.scm")          ;; actual file reading 
(gnc:depend "qif-dialog-utils.scm")  ;; build displays for dialog 
(gnc:depend "qif-guess-map.scm")     ;; build QIF->gnc acct mappings
(gnc:depend "qif-to-gnc.scm")        ;; conv QIF xtns/acct to GNC xtns/acct 


