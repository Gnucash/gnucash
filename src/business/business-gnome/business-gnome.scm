(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)
(gnc:module-load "gnucash/gnome-search" 0)

(define (add-customer-extensions)
  (let ((last-cust (gnc:owner-create))
	(last-order #f)
	(last-invoice #f))

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
      (gnc:make-menu-item (N_ "Find Customer")
			  (N_ "Find Customer")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((cust (gnc:customer-find
					 #f
					 (gnc:owner-get-customer
					  last-cust)
					 (gnc:get-current-book))))
			      (if (not (null? cust))
				  (gnc:owner-init-customer last-cust cust))))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((order (gnc:order-new
					  #f last-cust
					  (gnc:get-current-book))))
			      (if (not (null? order))
				  (set! last-order order))))))

    (define find-order-item
      (gnc:make-menu-item (N_ "Find Order")
			  (N_ "Find Order")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((order (gnc:order-find
					  #f last-order last-cust
					  (gnc:get-current-book))))
			      (if (not (null? order))
				  (set! last-order order))))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((invoice (gnc:invoice-new
					    #f last-cust
					    (gnc:get-current-book))))
			      (if (not (null? invoice))
				  (set! last-invoice invoice))))))

    (define find-invoice-item
      (gnc:make-menu-item (N_ "Find Invoice")
			  (N_ "Find Invoice")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (let ((invoice (gnc:invoice-find
					    #f last-invoice last-cust
					    (gnc:get-current-book))))
			      (if (not (null? invoice))
				  (set! last-invoice invoice))))))

    (gnc:owner-init-customer last-cust #f)

    (gnc:add-extension customer-menu)
    (gnc:add-extension find-invoice-item)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension find-order-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension select-customer-item)
    (gnc:add-extension new-customer-item)
    ))

(define (add-vendor-extensions)
  (let ((last-vendor (gnc:owner-create))
	(last-order #f)
	(last-invoice #f))

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
      (gnc:make-menu-item (N_ "Find Vendor")
			  (N_ "Find Vendor")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((vendor (gnc:vendor-find
					   #f
					   (gnc:owner-get-vendor
					    last-vendor)
					   (gnc:get-current-book))))
			      (if (not (null? vendor))
				  (gnc:owner-init-vendor last-vendor vendor))))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((order (gnc:order-new
					  #f last-vendor
					  (gnc:get-current-book))))
			      (if (not (null? order))
				  (set! last-order order))))))

    (define find-order-item
      (gnc:make-menu-item (N_ "Find Order")
			  (N_ "Find Order")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((order (gnc:order-find
					  #f last-order last-vendor
					  (gnc:get-current-book))))
			      (if (not (null? order))
				  (set! last-order order))))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((invoice (gnc:invoice-new
					    #f last-vendor
					    (gnc:get-current-book))))
			      (if (not (null? invoice))
				  (set! last-invoice invoice))))))

    (define find-invoice-item
      (gnc:make-menu-item (N_ "Find Invoice")
			  (N_ "Find Invoice")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (let ((invoice (gnc:invoice-find
					    #f last-invoice last-vendor
					    (gnc:get-current-book))))
			      (if (not (null? invoice))
				  (set! last-invoice invoice))))))

    (gnc:owner-init-vendor last-vendor #f)

    (gnc:add-extension vendor-menu)
    (gnc:add-extension find-invoice-item)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension find-order-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension select-vendor-item)
    (gnc:add-extension new-vendor-item)
    ))

(define (add-employee-extensions)
  (let ((last-employee #f))

    (define employee-menu
      (gnc:make-menu (N_ "Employees")
		     (list "Extensions" "")))

    (define new-employee-item
      (gnc:make-menu-item (N_ "New Employee")
			  (N_ "New Employee")
			  (list "Extensions" "Employees" "")
			  (lambda ()
			    (let ((employee (gnc:employee-new
					 #f (gnc:get-current-book))))
			      (if (not (null? employee))
				  (set! last-employee employee))))))

    (define select-employee-item
      (gnc:make-menu-item (N_ "Find Employee")
			  (N_ "Find Employee")
			  (list "Extensions" "Employees" "")
			  (lambda ()
			    (let ((employee (gnc:employee-find
					 #f
					 last-employee
					 (gnc:get-current-book))))
			      (if (not (null? employee))
				  (set! last-employee employee))))))

    (gnc:add-extension employee-menu)
    (gnc:add-extension select-employee-item)
    (gnc:add-extension new-employee-item)
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

  (define test-search
    (gnc:make-menu-item (N_ "Test Search Dialog")
			(N_ "Test Search Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:search-dialog-test))))


  (gnc:add-extension test-search)
  (gnc:add-extension select-job-item)
  (gnc:add-extension new-job-item)

  (add-employee-extensions)
  (add-vendor-extensions)
  (add-customer-extensions)
)

(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-extensions)
