
(define (gnc:extensions-menu-test-func)
  (gnc:debug "Extension called from scheme.\n"))

(define (gnc:extensions-menu-setup win)
  ;; Should take window as a parameter?
  
  (gnc:debug "Setting up extensions menu " win "\n")
  
  (gnc:extensions-menu-add-item
   "Test Account creation"
   "Creates three accounts and adds them to the top group"
   (lambda ()
     (gnc:test-creation)))

  (gnc:extensions-menu-add-item "Export data as text"
                                "Export data as text hint"
                                (lambda ()
                                  (gnc:main-win-export-data-as-text win)))

  (gnc:extensions-menu-add-item "Test error dialog"
                                "Test error dialog hint"
                                (lambda ()
                                  (gnc:error-dialog
                                   "Some error didn't occur.")))

  (gnc:extensions-menu-add-item "QIF Import"
                                "Import QIF hint"
                                (lambda () 
				  (gnc:extensions-qif-import win)))

  (gnc:extensions-menu-add-item
   "Test choose item from list dialog"
   "Test choose item from list dialog"
   (lambda ()
     (let ((result (gnc:choose-item-from-list-dialog
                    "Choose item from list test dialog"
                    (list
                     (cons "Item 1"
                           (lambda ()
                             (display "Item 1 selected") (newline)
                             #f))
                     (cons "Item 2"
                           (lambda ()
                             (display "Item 2 selected") (newline)
                             #f))
                     (cons "Item 3 (and close dialog)"
                           (lambda ()
                             (display "Item 3 selected -- close") (newline)
                             'some-interesting-result))))))
                           
       (cond
        ((eq? result #f)
         (gnc:error-dialog
          "Fatal error in choose item from list dialog."))
        ((eq? result 'cancel)
         (gnc:error-dialog "Choose item from list dialog canceled."))
        (else
         (gnc:error-dialog
          (call-with-output-string (lambda (string-port)
                                     (display "Choose item result: "
					      string-port)
                                     (write result string-port)))))))))
  
  (gnc:extensions-menu-add-item
   "Test verify dialog"
   "Test verify dialog hint"
   (lambda ()
     (let ((result (gnc:verify-dialog "Would you like to play a game?" #t)))
       (if result
           (gnc:info-dialog "You said yes.")
           (gnc:info-dialog "You said no.")))))

  (gnc:extensions-menu-add-item
   "Test info dialog"
   "Test info dialog hint"
   (lambda () (gnc:info-dialog "This is information.")))

  (gnc:extensions-menu-add-item "Simple extension test"
                                "Simple extension test hint"
                                 gnc:extensions-menu-test-func))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:extensions-menu-setup)
