;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  hbci.scm
;;  load the HBCI support code
;;
;;  Copyright (c) 2002 Christian <Stimming@tuhh.de>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-hbci-menu-items)
;  (gnc:add-extension 
;   (gnc:make-menu (N_ "HBCI") (list gnc:window-name-main "_Tools" "")))

  (gnc:add-extension
   (gnc:make-menu-item (N_ "HBCI Setup")
		       (N_ "HBCI Setup")
		       (list gnc:window-name-main "_Tools" "")
		       (lambda ()
			 (gnc:hbci-initial-setup))))


;  (gnc:add-extension
;   (gnc:make-menu "Online" (list gnc:window-name-register "Actions" "")))

;  (gnc:add-extension
;   (gnc:make-menu-item (N_ "Get Balance")
;		       (N_ "Get Balance")
;		       (list gnc:window-name-register "Actions" "Online" "")
;		       (lambda ()
;			 (gnc:hbci-initial-setup))))
)

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-hbci-menu-items)

;; Register Preferences
(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Online Banking & Importing") (N_ "HBCI Remember PIN in memory")
  "b" (N_ "Remember the PIN for HBCI in memory during a session")
  #f))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "hbci_close_on_finish" 1))
