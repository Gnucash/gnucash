;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-import.scm
;;;  virtual loader for QIF import facility 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-import.scm")

(gnc:depend "qif-import/simple-obj.scm")
(gnc:depend "qif-import/qif-objects.scm")      ;; class definitions 
(gnc:depend "qif-import/qif-parse.scm")        ;; string-to-value, date parsing
(gnc:depend "qif-import/qif-utils.scm")         
(gnc:depend "qif-import/qif-file.scm")         ;; actual file reading 
(gnc:depend "qif-import/qif-dialog-utils.scm") ;; build displays for dialog 
(gnc:depend "qif-import/qif-guess-map.scm")    ;; build QIF->gnc acct mappings
(gnc:depend "qif-import/qif-to-gnc.scm")       ;; conv QIF xtns/acct to GNC 
(gnc:depend "qif-import/qif-merge-groups.scm") ;; merge into user's acct  


