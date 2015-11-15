(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))


(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report report-system))

(define (run-test)
  (test-account-get-trans-type-splits-interval))

(define (NDayDelta n)
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt n)
    ddt))

(define (test-account-get-trans-type-splits-interval)
  (let ((env (create-test-env))
	(end-date (gnc:date->timepair (localtime (current-time)))))
    (let* ((accounts (env-create-account-structure-alist env (list "Assets"
								   (list (cons 'type ACCT-TYPE-ASSET))
								   (list "Bank Account")
								   (list "Wallet"))))
	   (bank-account (cdr (assoc "Bank Account" accounts)))
	   (wallet (cdr (assoc "Wallet" accounts))))

      (env-create-daily-transactions env (decdate end-date (NDayDelta 10)) end-date bank-account wallet)

      (let ((splits (gnc:account-get-trans-type-splits-interval (list bank-account wallet)
							      ACCT-TYPE-ASSET
							      (decdate end-date (NDayDelta 5))
							      end-date)))
	;; 8 is the right number (4 days, two splits per tx)
	(and (equal? 8 (length splits)))))))
