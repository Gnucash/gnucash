
(define (gnc:extensions-menu-test-func)
  (gnc:debug "Extension called from scheme.\n"))

(define (gnc:extensions-menu-setup win)
  ;; Should take window as a parameter?
  
  (gnc:debug "Setting up extensions menu " win "\n")
  
  (gnc:extensions-menu-add-item "Export data as text"
                                "Export data as text hint"
                                (lambda ()
                                  (gnc:main-win-export-data-as-text win)))

  (gnc:extensions-menu-add-item "Test error dialog"
                                "Test error dialog hint"
                                (lambda ()
                                  (gnc:error-message-dialog
                                   "Some error didn't occur.")))

  (gnc:extensions-menu-add-item "QIF Import"
                                "Import QIF hint"
                                (lambda () 
				  (gnc:extensions-qif-import win)))

  (gnc:extensions-menu-add-item
   "Test verify dialog"
   "Test verify dialog hint"
   (lambda ()
     (let ((result (gnc:verify-dialog "Would you like to play a game?")))
       (if result
           (gnc:error-message-dialog "You said yes.")
           (gnc:error-message-dialog "You said no.")))))

  (gnc:extensions-menu-add-item
   "Test query dialog"
   "Test query dialog hint"
   (lambda ()
     (let ((result (gnc:query-dialog
                    "Would you like to play a game?"
                    'yes
                    #t #f #t #t)))
       (case result
         ((#t) (gnc:message-dialog "You said yes."))
         ((#f) (gnc:message-dialog "You said no."))
         ((cancel) (gnc:message-dialog "You said cancel."))))))

  (gnc:extensions-menu-add-item "Simple extension test"
                                "Simple extension test hint"
                                 gnc:extensions-menu-test-func))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:extensions-menu-setup)
