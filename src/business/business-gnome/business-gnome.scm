(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)

(define (add-business-extensions)

  (define gnc:extensions-temp-book #f)

  (define (gnc:extensions-get-book)
    (if gnc:extensions-temp-book
        gnc:extensions-temp-book
        (begin
          (set! gnc:extensions-temp-book (gnc:get-current-book))
          (gnc:business-create-book gnc:extensions-temp-book)
          gnc:extensions-temp-book)))

  (define gnc:extensions-last-order #f)
  (define gnc:extensions-owner #f)

  (define new-job-item
    (gnc:make-menu-item (N_ "Test New Job Dialog")
			(N_ "Test New Job Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-new #f (gnc:extensions-get-book)
				       #f))))

  (define select-job-item
    (gnc:make-menu-item (N_ "Test Job Selection Dialog")
			(N_ "Test Job Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-select #f (gnc:extensions-get-book)
                                          #f #f))))

  (define new-vendor-item
    (gnc:make-menu-item (N_ "Test New Vendor Dialog")
			(N_ "Test New Vendor Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:vendor-new #f (gnc:extensions-get-book)))))


  (define select-vendor-item
    (gnc:make-menu-item (N_ "Test Vendor Selection Dialog")
			(N_ "Test Vendor Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:vendor-select (gnc:extensions-get-book)
                                             #f #f))))

  (define new-employee-item
    (gnc:make-menu-item (N_ "Test New Employee Dialog")
			(N_ "Test New Employee Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-new #f (gnc:extensions-get-book)))))


  (define select-employee-item
    (gnc:make-menu-item (N_ "Test Employee Selection Dialog")
			(N_ "Test Employee Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-select (gnc:extensions-get-book)
                                               #f #f))))

  (define new-order-item
    (gnc:make-menu-item (N_ "Test New Order Dialog")
			(N_ "Test New Order Dialog")
			(list "Extensions" "")
			(lambda ()
			  (set! gnc:extensions-last-order
				(gnc:order-new #f gnc:extensions-owner
					       (gnc:extensions-get-book))))))

  (define edit-order-item
    (gnc:make-menu-item (N_ "Test Edit/View Order Dialog")
			(N_ "Test Edit/View Order Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:order-edit #f gnc:extensions-last-order))))


  (gnc:add-extension edit-order-item)
  (gnc:add-extension new-order-item)
  (gnc:add-extension select-employee-item)
  (gnc:add-extension new-employee-item)
  (gnc:add-extension select-vendor-item)
  (gnc:add-extension new-vendor-item)
  (gnc:add-extension select-job-item)
  (gnc:add-extension new-job-item))

(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-extensions)
