;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  hbci.scm
;;  load the HBCI support code
;;
;;  Copyright (c) 2002 Christian <Stimming@tuhh.de>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-hbci-menu-items)
  (gnc:add-extension 
   (gnc:make-menu "HBCI" (list gnc:window-name-main "_Tools" "")))


  (gnc:add-extension
   (gnc:make-menu-item (N_ "HBCI Setup")
		       (N_ "HBCI Setup")
		       (list gnc:window-name-main "Tools" "HBCI" "")
		       (lambda ()
			 (gnc:hbci-initial-setup))))
)
;  (gnc:add-extension
;   (gnc:make-menu-item (N_ "Final HBCI Setup")
;		       (N_ "Finish the HBCI Setup")
;		       (list gnc:window-name-main "Tools" "HBCI" "")
;		       (lambda ()
;			 (gnc:hbci-finish-setup)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-hbci-menu-items)

