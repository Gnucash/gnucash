(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)

(define (add-customer-extensions)
  (let ((last-cust (gnc:owner-create)))

    (define customer-menu
      (gnc:make-menu (N_ "Customers")
		     (list "Extensions" "")))

    (define new-customer-item
      (gnc:make-menu-item (N_ "New Customer")
			  (N_ "New Customer")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((cust (gnc:customer-new
					 #f (gnc:get-current-book))))
			      (if (not (null? cust))
				  (gnc:owner-init-customer last-cust cust))))))

    (define select-customer-item
      (gnc:make-menu-item (N_ "Customer Selection")
			  (N_ "Customer Selection")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((cust (gnc:customer-select
					 (gnc:get-current-book)
					 (gnc:owner-get-customer
					  last-cust)
					 #f)))
			      (if (not (null? cust))
				  (gnc:owner-init-customer last-cust cust))))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:order-new #f last-cust
					   (gnc:get-current-book)))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:invoice-new #f last-cust
					     (gnc:get-current-book)))))

    (gnc:owner-init-customer last-cust #f)

    (gnc:add-extension customer-menu)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension select-customer-item)
    (gnc:add-extension new-customer-item)
    ))

(define (add-vendor-extensions)
  (let ((last-vendor (gnc:owner-create)))

    (define vendor-menu
      (gnc:make-menu (N_ "Vendors")
		     (list "Extensions" "")))

    (define new-vendor-item
      (gnc:make-menu-item (N_ "New Vendor")
			  (N_ "New Vendor")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((vendor (gnc:vendor-new 
					   #f
					   (gnc:get-current-book))))
			      if (not (null? vendor))
			      (gnc:owner-init-vendor last-vendor vendor)))))
  

    (define select-vendor-item
      (gnc:make-menu-item (N_ "Vendor Selection")
			  (N_ "Vendor Selection")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((vendor (gnc:vendor-select
					   (gnc:get-current-book)
					   (gnc:owner-get-vendor
					    last-vendor)
					   #f)))
			      (if (not (null? vendor))
				  (gnc:owner-init-vendor last-vendor vendor))))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:order-new #f last-vendor
					   (gnc:get-current-book)))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:invoice-new #f last-vendor
					     (gnc:get-current-book)))))

    (gnc:owner-init-vendor last-vendor #f)

    (gnc:add-extension vendor-menu)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension select-vendor-item)
    (gnc:add-extension new-vendor-item)
    ))

(define (add-business-extensions)

  (define gnc:extensions-last-order #f)
  (define gnc:extensions-last-invoice #f)

  (define new-job-item
    (gnc:make-menu-item (N_ "Test New Job Dialog")
			(N_ "Test New Job Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-new #f (gnc:get-current-book) #f))))

  (define select-job-item
    (gnc:make-menu-item (N_ "Test Job Selection Dialog")
			(N_ "Test Job Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-select #f (gnc:get-current-book)
                                          #f #f))))

  (define new-employee-item
    (gnc:make-menu-item (N_ "Test New Employee Dialog")
			(N_ "Test New Employee Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-new #f (gnc:get-current-book)))))


  (define select-employee-item
    (gnc:make-menu-item (N_ "Test Employee Selection Dialog")
			(N_ "Test Employee Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-select (gnc:get-current-book)
                                               #f #f))))

  (define edit-order-item
    (gnc:make-menu-item (N_ "Test Edit/View Order Dialog")
			(N_ "Test Edit/View Order Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:order-edit #f gnc:extensions-last-order))))

  (define edit-invoice-item
    (gnc:make-menu-item (N_ "Test Edit/View Invoice Dialog")
			(N_ "Test Edit/View Invoice Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:invoice-edit #f gnc:extensions-last-invoice))))


;;  (gnc:add-extension edit-invoice-item)
;;  (gnc:add-extension edit-order-item)
  (gnc:add-extension select-employee-item)
  (gnc:add-extension new-employee-item)
  (gnc:add-extension select-job-item)
  (gnc:add-extension new-job-item)

  (add-vendor-extensions)
  (add-customer-extensions)
)

(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-extensions)
