;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  hbci.scm
;;  load the HBCI support code
;;
;;  Copyright (c) 2002 Christian <Stimming@tuhh.de>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Register Preferences
(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Online Banking & Importing") (N_ "HBCI Remember PIN in memory")
  "b" (N_ "Remember the PIN for HBCI in memory during a session")
  #f))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "hbci_close_on_finish" 1))

;(gnc:register-configuration-option
; (gnc:make-simple-boolean-option
;  (N_ "Online Banking & Importing") (N_ "HBCI Use generic import matcher")
;  "b" (N_ "Use the transaction matcher from the generic import infrastructure instead of the HBCI specific one")
;  #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "_+Advanced") (N_ "HBCI Verbose Debug Messages")
  "m" (N_ "Activate verbose debug messages for HBCI Online Banking.") #f))
