(use-modules (gnucash gnc-module))
(use-modules (srfi srfi-1))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash app-utils))

(define (run-test)
  (test test-split-in-list?))

(define (test-split-in-list?)
  ;; this test suite tests deprecated functions.
  (let* ((env (create-test-env))
	 (today (current-time))
	 (account-alist (env-create-test-accounts env))
	 (bank-account (cdr (assoc "Bank" account-alist)))
	 (expense-account (cdr (assoc "Expenses" account-alist)))
	 (wallet-account (cdr (assoc "Wallet" account-alist)))
	 (tx1 (env-create-transaction env today bank-account wallet-account 20/1))
	 (tx2 (env-create-transaction env today bank-account expense-account 10/1))
	 (splits-tx1 (xaccTransGetSplitList tx1))
	 (splits-tx2 (xaccTransGetSplitList tx2)))
    (and (split-in-list? (first splits-tx1) splits-tx1)
	 (split-in-list? (second splits-tx1) splits-tx1)
	 (not (split-in-list? (first splits-tx1) splits-tx2))
	 (not (split-in-list? (second splits-tx1) splits-tx2))
	 (not (split-in-list? (first splits-tx1) '())))))
