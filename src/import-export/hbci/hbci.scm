;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  hbci.scm
;;  load the HBCI support code
;;
;;  Copyright (c) 2002 Christian <Stimming@tuhh.de>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-hbci-menu-items)
  (gnc:add-extension 
   (gnc:make-menu (N_ "HBCI") (list gnc:window-name-main (N_ "_Tools") "")))

  (gnc:add-extension
   (gnc:make-menu-item (N_ "HBCI Setup")
		       (N_ "HBCI Setup")
		       (list gnc:window-name-main (N_ "_Tools") (N_ "HBCI") "")
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
;  (gnc:add-extension
;   (gnc:make-menu-item (N_ "Get Transactions")
;		       (N_ "Get Transactions")
;		       (list gnc:window-name-register "Actions" "Online" "")
;		       (lambda ()
;			 (gnc:hbci-initial-setup))))
)
;  (gnc:add-extension
;   (gnc:make-menu-item (N_ "Final HBCI Setup")
;		       (N_ "Finish the HBCI Setup")
;		       (list gnc:window-name-main "Tools" "HBCI" "")
;		       (lambda ()
;			 (gnc:hbci-finish-setup)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-hbci-menu-items)

