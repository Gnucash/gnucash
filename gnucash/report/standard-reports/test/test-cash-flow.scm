(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports cash-flow))
(use-modules (gnucash report report-system))

(define (run-test)
  (and (test test-one-tx-in-cash-flow)
       (test test-one-tx-skip-cash-flow)
       (test test-both-way-cash-flow)))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
	(list "Asset" 
	      (list "Bank")
	      (list "Wallet"))
	(list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))))

(define (NDayDelta t64 n)
  (let* ((day-secs (* 60 60 24 n)) ; n days in seconds is n times 60 sec/min * 60 min/h * 24 h/day
         (new-secs (- t64 day-secs)))
    new-secs))

(define (to-report-currency curr amt date) amt)

(define (exchange-fn mon comm) mon)

(define (test-one-tx-in-cash-flow)
  (let* ((env (create-test-env))
	 (account-alist (env-create-account-structure-alist env structure))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist)))
	 (today (gnc-localtime (current-time)))
         (to-date-t64 (gnc-dmy2time64-end (tm:mday today) (+ 1 (tm:mon today)) (+ 1900 (tm:year today))))
         (from-date-t64 (NDayDelta to-date-t64 1))
	 (report-currency (gnc-default-report-currency))
	 )
    (env-create-transaction env to-date-t64 bank-account expense-account 100/1)
    (let ((result (cash-flow-calc-money-in-out (list (cons 'accounts (list bank-account))
						     (cons 'to-date-t64 to-date-t64)
						     (cons 'from-date-t64 from-date-t64)
						     (cons 'report-currency report-currency)
						     (cons 'include-trading-accounts #f)
						     (cons 'to-report-currency to-report-currency)))))
      (let* ((money-in-collector (cdr (assq 'money-in-collector result)))
	     (money-out-collector (cdr (assq 'money-out-collector result)))
	     (money-in-alist (cdr (assq 'money-in-alist result)))
	     (money-out-alist (cdr (assq 'money-out-alist result)))
	     (expense-acc-in-collector (cadr (assoc expense-account money-in-alist))))
	(and (or (null? money-out-alist)
                 (begin (format #t "The money-out-alist is not null.~%") #f))
	     (or (equal? 10000/100
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity expense-acc-in-collector
									   report-currency exchange-fn)))
                 (begin (format #t "Failed expense-acc-in-collector ~g expected 100.00~%" (gnc:gnc-monetary-amount (gnc:sum-collector-commodity expense-acc-in-collector
									   report-currency exchange-fn))) #f))
	     (or (equal? 10000/100
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-in-collector
									   report-currency exchange-fn)))
                 (begin (format #t "Failed money-in-collector ~g expected 100.00~%" (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-in-collector
									   report-currency exchange-fn))) #f))
	     (or (equal? 0/1
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-out-collector
									   report-currency exchange-fn)))
                 (begin (format #t "Failed sum-collector-commodity ~g expected 100.00~%" (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-out-collector
                                                                                                                                               report-currency exchange-fn))) #f))
             (begin (format #t "test-one-tx-in-cash-flow success~%") #t)
	     )))))

(define (test-one-tx-skip-cash-flow)
  (let* ((env (create-test-env))
	 (account-alist (env-create-account-structure-alist env structure))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist)))
	 (today (gnc-localtime (current-time)))
         (to-date-t64 (gnc-dmy2time64-end (tm:mday today) (+ 1 (tm:mon today)) (+ 1900 (tm:year today))))
         (from-date-t64 (NDayDelta to-date-t64 1))
	 (report-currency (gnc-default-report-currency))
	 )
    (env-create-transaction env to-date-t64 bank-account wallet-account 100/1)
    (let ((result (cash-flow-calc-money-in-out (list (cons 'accounts (list wallet-account bank-account))
						     (cons 'to-date-t64 to-date-t64)
						     (cons 'from-date-t64 from-date-t64)
						     (cons 'report-currency report-currency)
						     (cons 'include-trading-accounts #f)
						     (cons 'to-report-currency to-report-currency)))))
      (let* ((money-in-collector (cdr (assq 'money-in-collector result)))
	     (money-out-collector (cdr (assq 'money-out-collector result)))
	     (money-in-alist (cdr (assq 'money-in-alist result)))
	     (money-out-alist (cdr (assq 'money-out-alist result))))
	(and (null? money-in-alist)
	     (null? money-out-alist)
	     (equal? 0/1
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-in-collector
									   report-currency exchange-fn)))
	     (equal? 0/1
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-out-collector
									   report-currency exchange-fn)))
             (begin (format #t "test-one-tx-skip-cash-flow success~%") #t)
             )))))

(define (test-both-way-cash-flow)
  (let* ((env (create-test-env))
	 (account-alist (env-create-account-structure-alist env structure))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist)))
	 (today (gnc-localtime (current-time)))
         (to-date-t64 (gnc-dmy2time64-end (tm:mday today) (+ 1 (tm:mon today)) (+ 1900 (tm:year today))))
         (from-date-t64 (NDayDelta to-date-t64 1))
	 (report-currency (gnc-default-report-currency))
	 )
    (env-create-transaction env to-date-t64 bank-account expense-account 100/1)
    (env-create-transaction env to-date-t64 expense-account bank-account 50/1)
    (let ((result (cash-flow-calc-money-in-out (list (cons 'accounts (list wallet-account bank-account))
						     (cons 'to-date-t64 to-date-t64)
						     (cons 'from-date-t64 from-date-t64)
						     (cons 'report-currency report-currency)
						     (cons 'include-trading-accounts #f)
						     (cons 'to-report-currency to-report-currency)))))
      (let* ((money-in-collector (cdr (assq 'money-in-collector result)))
	     (money-out-collector (cdr (assq 'money-out-collector result)))
	     (money-in-alist (cdr (assq 'money-in-alist result)))
	     (money-out-alist (cdr (assq 'money-out-alist result)))
	     (expense-acc-in-collector (cadr (assoc expense-account money-in-alist)))
	     (expense-acc-out-collector (cadr (assoc expense-account money-out-alist)))
	     (expenses-in-total (gnc:gnc-monetary-amount (gnc:sum-collector-commodity expense-acc-in-collector
										      report-currency
										      exchange-fn)))
	     (expenses-out-total (gnc:gnc-monetary-amount (gnc:sum-collector-commodity expense-acc-out-collector
										       report-currency
										       exchange-fn))))
	(and (equal? 10000/100 expenses-in-total)
	     (equal? 5000/100 expenses-out-total)
	     (equal? 10000/100
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-in-collector
									   report-currency exchange-fn)))
	     (equal? 5000/100
		     (gnc:gnc-monetary-amount (gnc:sum-collector-commodity money-out-collector
									   report-currency exchange-fn)))
             (begin (format #t "test-both-way-cash-flow success~%") #t)
             )))))
