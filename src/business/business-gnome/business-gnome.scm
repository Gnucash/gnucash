(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)
(gnc:module-load "gnucash/business-utils" 0)
(gnc:module-load "gnucash/gnome-search" 0)
(gnc:module-load "gnucash/business-core-file" 0)
(gnc:module-load "gnucash/dialog-tax-table" 0)

(gnc:module-load "gnucash/report/report-gnome" 0)

(use-modules (gnucash report business-reports))
(use-modules (gnucash main))		;for gnc:debug

(define main-window gnc:window-name-main)
(define top-level (N_ "_Business"))
(define new-label (N_ "New"))
(define find-label (N_ "Find"))

(define ui-started #f)

(define (remind-bills-due session)
  (define (option-value name)
    (gnc:option-value (gnc:lookup-global-option gnc:*business-label* name)))

  (let ((check-bills? (option-value "Notify Bills Due?")))
    (if (and session check-bills?)
	(let* ((book (gnc:session-get-book session))
	       (days (option-value "Bills Due Days")))
	  (gnc:invoice-show-bills-due book days)))))

(define (business-report-function)
  (gnc:add-extension
   (gnc:make-menu gnc:menuname-business-reports
		  (list "Main" gnc:menuname-reports gnc:menuname-income-expense))))



(define (add-business-test)

  (define test-search
    (gnc:make-menu-item (N_ "Test Search Dialog")
			(N_ "Test Search Dialog")
			(list main-window "Extensions" "")
			(lambda ()
			  (gnc:search-dialog-test))))


  (define reload-invoice
    (gnc:make-menu-item (N_ "Reload invoice report")
			(N_ "Reload invoice report scheme file")
			(list main-window "Extensions" "")
			(lambda ()
			  (let ((m (current-module)))
			    (load-from-path "gnucash/report/invoice.scm")
			    (set-current-module m)))))

  (define reload-owner
    (gnc:make-menu-item (N_ "Reload owner report")
			(N_ "Reload owner report scheme file")
			(list main-window "Extensions" "")
			(lambda ()
			  (let ((m (current-module)))
			    (load-from-path "gnucash/report/owner-report.scm")
			    (set-current-module m)))))

  (define reload-receivable
    (gnc:make-menu-item (N_ "Reload receivable report")
			(N_ "Reload receivable report scheme file")
			(list main-window "Extensions" "")
			(lambda ()
			  (let ((m (current-module)))
			    (load-from-path "gnucash/report/aging.scm")
			    (set-current-module m)
			    (load-from-path "gnucash/report/receivables.scm")
			    (set-current-module m)))))

  (define init-data
    (gnc:make-menu-item (N_ "Initialize Test Data")
			(N_ "Initialize Test Data")
			(list main-window "Extensions" "")
			(lambda ()
			  (let* ((book (gnc:get-current-book))
				 (customer (gnc:customer-create book))
				 (address (gnc:customer-get-addr customer))
				 (invoice (gnc:invoice-create book))
				 (owner (gnc:owner-create))
				 (job (gnc:job-create book))
				 (group (gnc:book-get-group book))
				 (inc-acct (gnc:malloc-account book))
				 (bank-acct (gnc:malloc-account book))
				 (tax-acct (gnc:malloc-account book))
				 (ar-acct (gnc:malloc-account book)))

			    ;; Create Customer
			    (gnc:customer-set-id customer "000001")
			    (gnc:customer-set-name customer "Test Customer")
			    (gnc:customer-set-currency customer
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
			    (gnc:invoice-set-date-opened
			     invoice (cons (current-time) 0))
			    (gnc:invoice-set-currency
			     invoice (gnc:default-currency))

			    ;; Create the Job
			    (gnc:job-set-id job "000025")
			    (gnc:job-set-name job "Test Job")
			    (gnc:job-set-reference job "Customer's ref#")
			    (gnc:job-set-owner job owner)

			    ;; MODIFY THE OWNER
			    (gnc:owner-init-job owner job)

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

			    ;; Launch the invoice editor
			    (gnc:invoice-edit invoice)
			    ))))

  
  (gnc:add-extension init-data)
  (gnc:add-extension reload-receivable)
  (gnc:add-extension reload-invoice)
  (gnc:add-extension reload-owner)
  (gnc:add-extension test-search)
)

(define (business-book-opened session)
  (remind-bills-due session))

(define (business-ui-started)
  (set! ui-started #t)
  (remind-bills-due (gnc:get-current-session)))

(gnc:hook-add-dangler gnc:*report-hook* business-report-function)
;(gnc:hook-add-dangler gnc:*ui-post-startup-hook* business-ui-started)
(gnc:hook-add-dangler gnc:*book-opened-hook* business-book-opened)
(gnc:hook-add-dangler gnc:*add-extension-hook* add-business-test)
