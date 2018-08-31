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
  (test-list-set-safe)
  (test-gnc:monetary->string)
  (test-commodity-collector)
  (test-end "report-utilities"))

(define (NDayDelta t64 n)
  (let* ((day-secs (* 60 60 24 n)) ; n days in seconds is n times 60 sec/min * 60 min/h * 24 h/day
         (new-secs (- t64 day-secs)))
    new-secs))

(define (collector->list coll)
  ;; input:  collector
  ;; output: list of strings e.g. '("$25.00" "-£15.00")
  (map gnc:monetary->string (coll 'format gnc:make-gnc-monetary #f)))

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

(define (test-list-set-safe)
  (test-begin "list-set-safe")
  (let ((lst (list 1 2)))
    (list-set-safe! lst 1 3)
    (test-equal "list-set-safe normal"
      '(1 3)
      lst)
    (list-set-safe! lst 5 1)
    (test-equal "list-set-safe out-of-bounds"
      '(1 3 #f #f #f 1)
      lst))
  (test-end "list-set-safe"))

(define (test-gnc:monetary->string)
  (test-group-with-cleanup "gnc:monetary->string"
    (let* ((book (gnc-get-current-book))
           (comm-table (gnc-commodity-table-get-table book))
           (monetary (gnc:make-gnc-monetary
                      (gnc-commodity-table-lookup comm-table "CURRENCY" "USD")
                      100)))
      (test-equal "gnc:monetary->string"
        "$100.00"
        (gnc:monetary->string monetary)))
    (teardown)))

(define (test-commodity-collector)
  (test-group-with-cleanup "test-commodity-collector"
    (let* ((book (gnc-get-current-book))
           (comm-table (gnc-commodity-table-get-table book))
           (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
           (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
           (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
           (coll-A (gnc:make-commodity-collector))
           (coll-B (gnc:make-commodity-collector)))

      (test-equal "commodity-collector empty"
        '()
        (collector->list coll-A))

      (coll-A 'add USD 25)
      (test-equal "coll-A 'add USD25"
        '("$25.00")
        (collector->list coll-A))

      (coll-A 'add USD 25)
      (test-equal "coll-A 'add USD25"
        '("$50.00")
        (collector->list coll-A))

      (coll-A 'add GBP 20)
      (test-equal "coll-A 'add GBP20"
        '("£20.00" "$50.00")
        (collector->list coll-A))

      (coll-A 'reset #f #f)
      (test-equal "coll-A 'reset"
        '()
        (collector->list coll-A))

      (coll-A 'add USD 25)
      (coll-B 'add GBP 20)
      (test-equal "coll-B 'add GBP20"
        '("£20.00")
        (collector->list coll-B))

      (coll-A 'merge coll-B #f)
      (test-equal "coll-A 'merge coll-B"
        '("£20.00" "$25.00")
        (collector->list coll-A))

      (coll-A 'reset #f #f)
      (coll-A 'add USD 25)
      (coll-A 'minusmerge coll-B #f)
      (test-equal "coll-A 'minusmerge coll-B"
        '("-£20.00" "$25.00")
        (collector->list coll-A))

      (test-equal "coll-A 'getpair USD"
        (list USD 25)
        (coll-A 'getpair USD #f))

      (test-equal "coll-A 'getmonetary USD"
        (gnc:make-gnc-monetary USD 25)
        (coll-A 'getmonetary USD #f))

      (test-equal "gnc:commodity-collector-get-negated"
        '("-$25.00" "£20.00")
        (collector->list
         (gnc:commodity-collector-get-negated coll-A)))

      (test-equal "gnc:commodity-collectorlist-get-merged"
        '("$25.00" "£0.00")
        (collector->list
         (gnc:commodity-collectorlist-get-merged (list coll-A coll-B))))

      (test-equal "gnc-commodity-collector-allzero? #f"
        #f
        (gnc-commodity-collector-allzero? coll-A))

      ;; coll-A has -GBP20 and USD25 for now, bring bal to 0 each
      (coll-A 'add GBP 20)
      (coll-A 'add USD -25)
      (test-equal "gnc-commodity-collector-allzero? #t"
        #t
        (gnc-commodity-collector-allzero? coll-A)))
    (teardown)))
