;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  generic-import.scm
;;  
;;
;;  Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Register Preferences
(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Transaction Matcher") (N_ "Enable SKIP transaction action")
  "b" (N_ "Enable the SKIP action in the transaction matcher.  If enabled, a transaction whose best match's score is in the yellow zone (above the Auto-ADD threshold but below the Auto-CLEAR threshold) will be SKIPed by default.")
  #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Transaction Matcher") (N_ "Enable REPLACE match action")
  "b" (N_ "Enable the REPLACE action in the transaction matcher.  If the REPLACE action is selected, the downloaded transaction's data will replace the selected match")
  #f))

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
  "g" (N_ "A transaction whose best match's score is in the red zone (above the display treshold but below or equal to the Auto-ADD treshold) will be ADDed by default.")
  2.0 ;; default
  1.0 ;; lower bound
  6.0 ;; upper bound
  0.0 ;; number of decimals
  1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "Transaction Matcher") (N_ "Auto-CLEAR threshold")
  "g" (N_ "A transaction whose best match's score is in the green zone (above or equal to the Auto-CLEAR threshold) will be CLEARed by default.")
  5.0 ;; default
  1.0 ;; lower bound
  6.0 ;; upper bound
  0.0 ;; number of decimals
  1.0 ;; step size
  ))
