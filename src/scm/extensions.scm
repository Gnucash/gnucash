
(define (gnc:extensions-menu-test-func)
  (gnc:debug "Extension called from scheme.\n"))

(define (gnc:extensions-menu-setup win)
  ;; Should take window as a parameter?
  
  (gnc:debug "Setting up extensions menu " win "\n")
  
  (gnc:extensions-menu-add-item "Export data as text."
                                "Export data as text hint."
                                (lambda ()
                                  (gnc:main-win-export-data-as-text win)))

  (gnc:extensions-menu-add-item "Scheme test2"
                                 "Scheme test2 hint"
                                 gnc:extensions-menu-test-func))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:extensions-menu-setup)
