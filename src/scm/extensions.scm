
(gnc:support "extensions.scm")

(define (gnc:extensions-menu-setup win)
  ;; Should take window as a parameter?
  
  (gnc:debug "Setting up extensions menu " win "\n")
  
  (gnc:extensions-menu-add-item "Export data as text (Danger: Unfinished)"
                                "Export data as text."
                                (lambda ()
                                  (gnc:main-win-export-data-as-text win)))

  (gnc:extensions-menu-add-item "QIF File Import (Danger: Unfinished)"
                                "Import QIF File - Scripted in Guile."
                                (lambda () 
				  (gnc:extensions-qif-import win))))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:extensions-menu-setup)
