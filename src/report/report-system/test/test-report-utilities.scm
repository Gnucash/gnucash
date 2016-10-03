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
	(end-date-tp (gnc:date->timepair (localtime (current-time)))))
    (let* ((accounts (env-create-account-structure-alist env (list "Assets"
								   (list (cons 'type ACCT-TYPE-ASSET))
								   (list "Bank Account")
								   (list "Wallet"))))
	   (bank-account (cdr (assoc "Bank Account" accounts)))
	   (wallet (cdr (assoc "Wallet" accounts)))
	   (start-date-tp (decdate end-date-tp (NDayDelta 10)))
	   (q-start-date-tp (decdate end-date-tp (NDayDelta 5)))
	   (q-start-date (gnc:timepair->date q-start-date-tp))
	   (q-end-date (gnc:timepair->date end-date-tp)))

      (env-create-daily-transactions env start-date-tp end-date-tp bank-account wallet)

      ; Ensure the query interval is as inclusive as possible to deal with timezone differences
      (set-tm:hour q-end-date 23)
      (set-tm:min  q-end-date 59)
      (set-tm:sec  q-end-date 59)
      (set-tm:hour q-start-date 00)
      (set-tm:min  q-start-date 00)
      (set-tm:sec  q-start-date 01)

      (let ((splits (gnc:account-get-trans-type-splits-interval (list bank-account wallet)
							      ACCT-TYPE-ASSET
							      (gnc:date->timepair q-start-date)
							      (gnc:date->timepair q-end-date))))
	;; 10 is the right number (5 days, two splits per tx)
	(and (equal? 10 (length splits)))))))
