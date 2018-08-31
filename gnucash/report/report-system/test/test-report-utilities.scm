(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))

(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report report-system))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "report-utilities")
  (test-account-get-trans-type-splits-interval)
  (test-list-ref-safe)
  (test-end "report-utilities")
  )

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
	(test-equal "length splits = 10"
          10
          (length splits))))))

(define (teardown)
  (gnc-clear-current-session))

(define (test-list-ref-safe)
  (test-begin "list-ref-safe")
  (let ((lst '(1 2)))
    (test-equal "list-ref-safe normal"
      1
      (list-ref-safe lst 0))
    (test-equal "list-ref-safe out of bounds"
      #f
      (list-ref-safe lst 3)))
  (test-end "list-ref-safe"))
