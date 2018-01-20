(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))


(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report report-system))

(define (run-test)
  (test-account-get-trans-type-splits-interval))

(define (NDayDelta t64 n)
  (let* ((day-secs (* 60 60 24 n)) ; n days in seconds is n times 60 sec/min * 60 min/h * 24 h/day
         (new-secs (- t64 day-secs)))
    new-secs))

(define (test-account-get-trans-type-splits-interval)
  (let* ((env (create-test-env))
         (ts-now (gnc-localtime (current-time)))
         (test-day (tm:mday ts-now))
         (test-month (+ 1 (tm:mon ts-now)))
         (test-year (+ 1900 (tm:year ts-now)))
         (end-date (gnc-dmy2time64-neutral test-day test-month test-year))
         (start-date (NDayDelta end-date 10))
         (q-end-date (gnc-dmy2time64-end test-day test-month test-year))
         (q-start-date (gnc-dmy2time64 test-day test-month test-year))
         (q-start-date (NDayDelta q-start-date 5)))

    (let* ((accounts (env-create-account-structure-alist env (list "Assets"
								   (list (cons 'type ACCT-TYPE-ASSET))
								   (list "Bank Account")
								   (list "Wallet"))))
	   (bank-account (cdr (assoc "Bank Account" accounts)))
	   (wallet (cdr (assoc "Wallet" accounts))))

      (env-create-daily-transactions env start-date end-date bank-account wallet)
      (format #t "Created transactions for each day from ~a to ~a~%" (gnc-ctime start-date) (gnc-ctime end-date))
      (let ((splits (gnc:account-get-trans-type-splits-interval (list bank-account wallet)
							      ACCT-TYPE-ASSET
							      q-start-date q-end-date)))
	;; 10 is the right number (5 days, two splits per tx)
	(or (equal? 10 (length splits)) (begin (format #t "Fail, ~d splits, expected 10~%" (length splits)) #f))))))
