(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)
(gnc:module-load "gnucash/gnome-search" 0)

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
			    (gnc:customer-new #f (gnc:get-current-book)))))

    (define find-customer-item
      (gnc:make-menu-item (N_ "Find Customer")
			  (N_ "Find Customer")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:customer-find #f (gnc:owner-get-customer
						   last-cust)
					       (gnc:get-current-book)))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:order-new #f last-cust
					   (gnc:get-current-book)))))

    (define find-order-item
      (gnc:make-menu-item (N_ "Find Order")
			  (N_ "Find Order")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:order-find #f #f last-cust
					    (gnc:get-current-book)))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:invoice-new #f last-cust
					     (gnc:get-current-book)))))

    (define find-invoice-item
      (gnc:make-menu-item (N_ "Find Invoice")
			  (N_ "Find Invoice")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:invoice-find #f #f last-cust
					      (gnc:get-current-book)))))

    (define new-job-item
      (gnc:make-menu-item (N_ "New Job")
			  (N_ "New Job")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:job-new #f last-cust
					 (gnc:get-current-book)))))

    (define find-job-item
      (gnc:make-menu-item (N_ "Find Job")
			  (N_ "Find Job")
			  (list "Extensions" "Customers" "")
			  (lambda ()
			    (gnc:job-find #f #f last-cust
					  (gnc:get-current-book)))))


    (gnc:owner-init-customer last-cust #f)

    (gnc:add-extension customer-menu)
    (gnc:add-extension find-job-item)
    (gnc:add-extension new-job-item)
    (gnc:add-extension find-invoice-item)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension find-order-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension find-customer-item)
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
			    (gnc:vendor-new #f (gnc:get-current-book)))))
  

    (define find-vendor-item
      (gnc:make-menu-item (N_ "Find Vendor")
			  (N_ "Find Vendor")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:vendor-find #f (gnc:owner-get-vendor
						 last-vendor)
					     (gnc:get-current-book)))))

    (define new-order-item
      (gnc:make-menu-item (N_ "New Order")
			  (N_ "New Order")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:order-new #f last-vendor
					   (gnc:get-current-book)))))

    (define find-order-item
      (gnc:make-menu-item (N_ "Find Order")
			  (N_ "Find Order")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:order-find #f #f last-vendor
					    (gnc:get-current-book)))))

    (define new-invoice-item
      (gnc:make-menu-item (N_ "New Invoice")
			  (N_ "New Invoice")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:invoice-new #f last-vendor
					     (gnc:get-current-book)))))

    (define find-invoice-item
      (gnc:make-menu-item (N_ "Find Invoice")
			  (N_ "Find Invoice")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:invoice-find #f #f last-vendor
					      (gnc:get-current-book)))))

    (define new-job-item
      (gnc:make-menu-item (N_ "New Job")
			  (N_ "New Job")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:job-new #f last-vendor
					 (gnc:get-current-book)))))

    (define find-job-item
      (gnc:make-menu-item (N_ "Find Job")
			  (N_ "Find Job")
			  (list "Extensions" "Vendors" "")
			  (lambda ()
			    (gnc:job-find #f #f last-vendor
					  (gnc:get-current-book)))))


    (gnc:owner-init-vendor last-vendor #f)

    (gnc:add-extension vendor-menu)
    (gnc:add-extension find-job-item)
    (gnc:add-extension new-job-item)
    (gnc:add-extension find-invoice-item)
    (gnc:add-extension new-invoice-item)
    (gnc:add-extension find-order-item)
    (gnc:add-extension new-order-item)
    (gnc:add-extension find-vendor-item)
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
			    (gnc:employee-new #f (gnc:get-current-book)))))

    (define find-employee-item
      (gnc:make-menu-item (N_ "Find Employee")
			  (N_ "Find Employee")
			  (list "Extensions" "Employees" "")
			  (lambda ()
			    (gnc:employee-find #f last-employee
					       (gnc:get-current-book)))))

    (gnc:add-extension employee-menu)
    (gnc:add-extension find-employee-item)
    (gnc:add-extension new-employee-item)
    ))

(define (add-business-extensions)

  (define test-search
    (gnc:make-menu-item (N_ "Test Search Dialog")
			(N_ "Test Search Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:search-dialog-test))))


  (gnc:add-extension test-search)

  (add-employee-extensions)
  (add-vendor-extensions)
  (add-customer-extensions)
)

(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-extensions)
