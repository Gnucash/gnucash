;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  hbci.scm
;;  load the HBCI support code
;;
;;  Copyright (c) 2002 Christian <Stimming@tuhh.de>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-hbci-menu-items)
  (gnc:add-extension 
   (gnc:make-menu "HBCI" (list "_Tools" "")))


  (gnc:add-extension
   (gnc:make-menu-item (N_ "Initial HBCI Setup")
		       (N_ "Initial HBCI Setup")
		       (list "_Tools" "HBCI" "")
		       (lambda ()
			 (gnc:hbci-initial-setup)))))
  ;;(gnc:add-extension
  ;; (gnc:make-menu-item (N_ "Finish HBCI Setup")
  ;;	       (N_ "Finish HBCI Setup")
  ;;	       (list "Tools" "HBCI" "")
  ;;	       (lambda ()
  ;;		 (gnc:hbci-finish-setup)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-hbci-menu-items)

