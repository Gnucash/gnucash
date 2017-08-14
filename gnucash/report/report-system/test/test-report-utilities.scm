(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))


(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report report-system))

(define (run-test)
  (test-account-get-trans-type-splits-interval))

(define (NDayDelta tp n)
  (let* ((day-secs (* 60 60 24 n)) ; n days in seconds is n times 60 sec/min * 60 min/h * 24 h/day
         (new-secs (- (car tp) day-secs))
         (new-tp (cons new-secs 0)))
    new-tp))

(define (test-account-get-trans-type-splits-interval)
  (let* ((env (create-test-env))
         (ts-now (gnc-localtime (current-time)))
         (test-day (tm:mday ts-now))
         (test-month (+ 1 (tm:mon ts-now)))
         (test-year (+ 1900 (tm:year ts-now)))
         (end-date-tp (gnc-dmy2timespec-neutral test-day test-month test-year))
         (start-date-tp (NDayDelta end-date-tp 10))
         (q-end-date-tp (gnc-dmy2timespec-end test-day test-month test-year))
         (q-start-date-tp (gnc-dmy2timespec test-day test-month test-year))
         (q-start-date-tp (NDayDelta q-start-date-tp 5)))

    (let* ((accounts (env-create-account-structure-alist env (list "Assets"
								   (list (cons 'type ACCT-TYPE-ASSET))
								   (list "Bank Account")
								   (list "Wallet"))))
	   (bank-account (cdr (assoc "Bank Account" accounts)))
	   (wallet (cdr (assoc "Wallet" accounts))))

      (env-create-daily-transactions env start-date-tp end-date-tp bank-account wallet)

      (let ((splits (gnc:account-get-trans-type-splits-interval (list bank-account wallet)
							      ACCT-TYPE-ASSET
							      q-start-date-tp q-end-date-tp)))
	;; 10 is the right number (5 days, two splits per tx)
	(and (equal? 10 (length splits)))))))
