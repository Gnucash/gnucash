(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)
(gnc:module-load "gnucash/gnome-search" 0)
(gnc:module-load "gnucash/business-core-file" 0)

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
			    (gnc:customer-find (gnc:owner-get-customer
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
			    (gnc:order-find #f last-cust
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
			    (gnc:invoice-find #f last-cust
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
			    (gnc:job-find #f last-cust
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
			    (gnc:vendor-find (gnc:owner-get-vendor
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
			    (gnc:order-find #f last-vendor
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
			    (gnc:invoice-find #f last-vendor
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
			    (gnc:job-find #f last-vendor
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
			    (gnc:employee-find last-employee
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


  (define reload-invoice
    (gnc:make-menu-item (N_ "Reload invoice report")
			(N_ "Reload invoice report")
			(list "Extensions" "")
			(lambda ()
			  (let ((m (current-module)))
			    (load-from-path "gnucash/report/invoice.scm")
			    (set-current-module m)))))

  (define init-data
    (gnc:make-menu-item (N_ "Initialize Test Data")
			(N_ "Initialize Test Data")
			(list "Extensions" "")
			(lambda ()
			  (let* ((book (gnc:get-current-book))
				 (customer (gnc:customer-create book))
				 (address (gnc:customer-get-addr customer))
				 (invoice (gnc:invoice-create book))
				 (owner (gnc:owner-create))
				 (job (gnc:job-create book))
				 (order (gnc:order-create book))
				 (group (gnc:book-get-group book))
				 (inc-acct (gnc:malloc-account book))
				 (bank-acct (gnc:malloc-account book))
				 (tax-acct (gnc:malloc-account book))
				 (ar-acct (gnc:malloc-account book)))

			    ;; Create Customer
			    (gnc:customer-set-id customer "000001")
			    (gnc:customer-set-name customer "Test Customer")
			    (gnc:customer-set-commodity customer
							(gnc:default-currency))
			    (gnc:address-set-name address "Contact Person")
			    (gnc:address-set-addr1 address
						   "20 Customer Lane")
			    (gnc:address-set-addr2 address "Customer M/S")
			    (gnc:address-set-addr3 address "Addr3, XXX  12345")
			    
			    ;; Create the Owner
			    (gnc:owner-init-customer owner customer)
			    
			    ;; Create the Invoice
			    (gnc:invoice-set-id invoice "000012")
			    (gnc:invoice-set-owner invoice owner)
			    (gnc:invoice-set-terms invoice "Net-30")
			    (gnc:invoice-set-date-opened
			     invoice (cons (current-time) 0))
			    (gnc:invoice-set-common-commodity
			     invoice (gnc:default-currency))

			    ;; Create the Job
			    (gnc:job-set-id job "000025")
			    (gnc:job-set-name job "Test Job")
			    (gnc:job-set-reference job "Customer's ref#")
			    (gnc:job-set-owner job owner)

			    ;; MODIFY THE OWNER
			    (gnc:owner-init-job owner job)

			    ;; Create the Order
			    (gnc:order-set-id order "001342")
			    (gnc:order-set-owner order owner)
			    (gnc:order-set-reference order "Customer's ref#")
			    (gnc:order-set-date-opened
			     order (cons (current-time) 0))
			    
			    ;; Create the A/R account
			    (gnc:account-set-type ar-acct 'receivable)
			    (gnc:account-set-name ar-acct "A/R")
			    (gnc:account-set-commodity ar-acct
						       (gnc:default-currency))
			    (gnc:group-insert-account group ar-acct)

			    ;; Create the Income account
			    (gnc:account-set-type inc-acct 'income)
			    (gnc:account-set-name inc-acct "Income")
			    (gnc:account-set-commodity inc-acct
						       (gnc:default-currency))
			    (gnc:group-insert-account group inc-acct)

			    ;; Create the Bank account
			    (gnc:account-set-type bank-acct 'bank)
			    (gnc:account-set-name bank-acct "Bank")
			    (gnc:account-set-commodity bank-acct
						       (gnc:default-currency))
			    (gnc:group-insert-account group bank-acct)

			    ;; Create the Tax account
			    (gnc:account-set-type tax-acct 'liability)
			    (gnc:account-set-name tax-acct "Tax-Holding")
			    (gnc:account-set-commodity tax-acct
						       (gnc:default-currency))
			    (gnc:group-insert-account group tax-acct)

			    ;; Launch the order editor
			    (gnc:order-edit order)

			    ;; Launch the invoice editor
			    (gnc:invoice-edit invoice)
			    ))))


  (gnc:add-extension init-data)
  (gnc:add-extension reload-invoice)
  (gnc:add-extension test-search)

  (add-employee-extensions)
  (add-vendor-extensions)
  (add-customer-extensions)
)

(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-extensions)
