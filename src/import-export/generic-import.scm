;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  generic-import.scm
;;  
;;
;;  Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Register Preferences
;(gnc:register-configuration-option
; (gnc:make-simple-boolean-option
;  (N_ "Transaction Matcher") (N_ "Enable SKIP transaction action")
;  "b" (N_ "Enable the SKIP action in the transaction matcher.  If enabled, a transaction whose best match's score is in the yellow zone (above the Auto-ADD threshold but below the Auto-CLEAR threshold) will be SKIPed by default.")
;  #t))

; Disable for 1.8 release until implemented

;(gnc:register-configuration-option
; (gnc:make-simple-boolean-option
;  (N_ "Transaction Matcher") (N_ "Enable EDIT match action")
;  "b" (N_ "Enable the EDIT action in the transaction matcher.  NOT YET SUPPORTED")
;  #f))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "Transaction Matcher") (N_ "Match display threshold")
  "g" (N_ "The minimum score a potential match must have to be displayed in the match list.")
  1.0 ;; default
  0.0 ;; lower bound
  6.0 ;; upper bound
  0.0 ;; number of decimals
  1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "Transaction Matcher") (N_ "Auto-ADD threshold")
  "g" (N_ "A transaction whose best match's score is in the red zone (above the display threshold but below or equal to the Auto-ADD threshold) will be ADDed by default.")
  3.0 ;; default
  1.0 ;; lower bound
  6.0 ;; upper bound
  0.0 ;; number of decimals
  1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "Transaction Matcher") (N_ "Auto-CLEAR threshold")
  "g" (N_ "A transaction whose best match's score is in the green zone (above or equal to the Auto-CLEAR threshold) will be CLEARed by default.")
  6.0 ;; default
  1.0 ;; lower bound
  10.0 ;; upper bound
  0.0 ;; number of decimals
  1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "Transaction Matcher") (N_ "Commercial ATM fees threshold")
  "g" (N_ "In some places commercial ATMs (not belonging to a financial institution) are installed in places like convienience store.  These ATM add its fee directly to the amount instead of showing up as a separate transaction or in your monthly banking fees.  For example, you withdraw 100$, and you are charged 101,50$ plus Interac fees.  If you manually entered that 100$, the amounts won't match.  You should set this to whatever is the maximum such fee in your area (in units of your local currency), so the transaction will be recognised as a match.")
  2.00 ;; default
  0.0 ;; lower bound
  1000.0 ;; upper bound
  2.0 ;; number of decimals
  0.01 ;; step size
  ))