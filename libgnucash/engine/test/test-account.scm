(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine))

(use-modules (gnucash engine test test-extras))

(define (run-test)
  (test test-account-same?)
  (test test-account-in-list?)
  (test test-account-in-alist?)
  (test test-account-list-predicate))

(define (test-account-same?)
  (let* ((env (create-test-env))
	 (account-alist (env-create-test-accounts env))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist))))
    (and (account-same? bank-account bank-account)
	 (not (account-same? bank-account expense-account)))))

(define (test-account-in-alist?)
  (let* ((env (create-test-env))
	 (account-alist (env-create-test-accounts env))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist))))
    (let ((alist (list (cons bank-account "Bank") (cons expense-account "Expenses"))))
      (and (account-in-alist bank-account alist)
	   (account-in-alist expense-account alist)
	   (not (account-in-alist wallet-account alist))))))

(define (test-account-in-list?)
  (test-account-list-predicate-generic
   (lambda (accounts) (lambda (account) (account-in-list? account accounts)))))

(define (test-account-list-predicate)
  (test-account-list-predicate-generic account-in-list-pred))

(define (test-account-list-predicate-generic predicate)
  (let* ((env (create-test-env))
	 (account-alist (env-create-test-accounts env))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (other-account (cdr (assoc "Other" account-alist)))
	 (bank-or-wallet? (predicate (list bank-account wallet-account))))
    (and (bank-or-wallet? bank-account)
	 (bank-or-wallet? wallet-account)
	 (not (bank-or-wallet? other-account)))))
