(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard acb-tool))
(use-modules (gnucash report stylesheets plain))
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; This is implementation testing for both the ACB Tool
(define uuid "acb-tool")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-acbs.scm")
  (null-test)
  (acb-tests)
  (test-end "test-acbs.scm"))

(define (options->sxml uuid options test-title)
  (gnc:options->sxml uuid options "test-acb" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options uuid)))
    (test-assert "null-test"
      (options->sxml uuid options "null-test"))))

(define (mnemonic->commodity sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

(define (new-commodity name ns sym scu)
  (let* ((book (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (new-comm (gnc-commodity-new book name ns sym "" scu)))
    (gnc-commodity-table-insert comm-table new-comm)
    new-comm))

(define (create-acb-test-data)
  (define book (gnc-get-current-book))
  (define env (create-test-env))
  (define USD (mnemonic->commodity "USD"))
  (define CAD (mnemonic->commodity "CAD"))
  (define SPY (new-commodity "SPY" "NYSE" "SPY" 10000))
  (define structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                       (cons 'commodity CAD))
          (list "Asset"
                (list "Broker" (list (cons 'commodity CAD))
                      (list "CAD Cash" (list (cons 'commodity CAD)))
                      (list "USD Cash" (list (cons 'commodity USD)))
                      (list "SPY" (list (cons 'commodity SPY)
                                        (cons 'type ACCT-TYPE-STOCK))))
                (list "Current Assets"
                      (list "Checking Account")))
          (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
                (list "USD CapGain" (list (cons 'commodity USD)))
                (list "USD Interest" (list (cons 'commodity USD))))
          (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE))
                (list "USD Commissions" (list (cons 'commodity USD))))
          (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))
                (list "Opening Balances")
                (list "Opening Balances USD" (list (cons 'commodity USD))))))
  (define account-alist (env-create-account-structure-alist env structure))
  (define (get-acct name)
    (or (assoc-ref account-alist name) (error "no account" name)))
  (define usd-cash (get-acct "USD Cash"))
  (define usd-comm (get-acct "USD Commissions"))
  (define inc-capg (get-acct "USD CapGain"))
  (define spy (get-acct "SPY"))

  (env-transfer env 01 05 2020
                (get-acct "Opening Balances")
                (get-acct "Checking Account")
                200000 #:description "opening cash")

  (env-transfer env 06 05 2020
                (get-acct "Checking Account")
                (get-acct "CAD Cash")
                100000 #:description "fund trading account")

  (env-transfer-foreign env 10 05 2020
                        (get-acct "CAD Cash")
                        usd-cash
                        100000 85000
                        #:description "Convert CAD to USD")

  (env-create-multisplit-transaction
   env 01 07 2019
   (list (vector usd-cash  -2000995/100 -2000995/100 "")
         (vector spy 20000 100 "Buy")
         (vector spy 995/100 0 "Fee"))
   #:description "Buy SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 11 12 2019
   (list (vector usd-cash -1600995/100 -1600995/100 "")
         (vector spy 16000 50 "Buy")
         (vector spy 995/100 0 "Fee")
         )
   #:description "Buy SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 18 03 2020
   (list (vector usd-comm 995/100 995/100 "Fee")
         (vector usd-cash 1199005/100 1199005/100 "")
         (vector inc-capg 600995/100 600995/100 "")
         (vector spy -12000 -75 "Sell")
         (vector spy -600995/100 0 "")  ;gross profit/loss
         )
   #:description "Sell SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 01 04 2020
   (list (vector usd-cash -4200995/100 -4200995/100 "")
         (vector spy 42000 250 "Buy")
         (vector spy 995/100 0 "Fee")
         )
   #:description "Buy SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 16 04 2020
   (list (vector usd-cash 2500 -2500)
         (vector spy -2500 0 "Buy")
         )
   #:description "Return of Capital"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 02 05 2020
   (list (vector usd-cash -47500 -47500 "")
         (vector spy 47500 125 "Buy")
         (vector spy 0 0 "Fee")
         )
   #:description "Buy spy"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 11 05 2020
   (list (vector spy 0 450 "Buy"))
   #:description "stock split"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 21 05 2020
   (list (vector usd-comm 995/100 995/100 "Fee")
         (vector usd-cash 2149005/100 2149005/100 "")
         (vector inc-capg 574702/100 574702/100 "")
         (vector spy -21500 -135 "Sell")
         (vector spy -574702/100 0 "")  ;gross profit/loss
         )
   #:description "Sell SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 03 06 2020
   (list (vector usd-cash -21000 -21000 "")
         (vector spy 21000 150 "Buy")
         (vector spy 0 0 "Fee")
         )
   #:description "Buy spy"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 10 06 2020
   (list (vector usd-comm 995/100 995/100 "Fee")
         (vector usd-cash 12809005/100 12809005/100 "")
         (vector inc-capg  1783309/100  1783309/100 "")
         (vector spy -128100 -915 "Sell")
         (vector spy -1783309/100 0 "")  ;gross profit/loss
         )
   #:description "Sell SPY"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 10 06 2020
   (list (vector spy 995/100 0 "Fee")
         (vector usd-cash 1189005/100 1189005/100 "")
         (vector spy -11900 -85 "Sell")
         )
   #:description "Sell SPY Short"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 15 06 2020
   (list (vector spy 995/100 0 "Fee")
         (vector usd-cash 1104005/100 1104005/100 "")
         (vector spy -11050 -65 "Sell")
         )
   #:description "Sell SPY Short"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 18 06 2020
   (list (vector spy 5000 50 "Sell")
         (vector usd-comm 995/100 995/100 "Fee")
         (vector spy 264337/100 0 "")  ;gross profit/loss
         (vector usd-cash -500995/100 -500995/100 "")
         (vector inc-capg -264337/100 -264337/100 "")
         )
   #:description "Buy SPY Close Short"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 20 06 2020
   (list (vector spy 8000 100 "Sell")
         (vector usd-comm 498/100 498/100 "Fee")
         (vector spy 728673/100 0 "")  ;gross profit/loss
         (vector usd-cash -800498/100 -800498/100 "")
         (vector inc-capg -728673/100 -728673/100 "")
         )
   #:description "Buy SPY Close Short"
   #:currency CAD)

  (env-create-multisplit-transaction
   env 21 06 2020
   (list (vector usd-cash -800498/100 -800498/100 "")
         (vector spy 8000 100 "Buy")
         (vector spy 498/100 0 "Fee")
         )
   #:description "Buy SPY"
   #:currency CAD)

  account-alist)

(define (acb-tests)
  (test-group-with-cleanup "acb-tests"
    (let* ((account-alist (create-acb-test-data))
           (options (gnc:make-report-options uuid)))
      ;; (set-option! options "General" "Price Source" 'pricedb-latest)
      (set-option! options "General" "Report's currency" (mnemonic->commodity "CAD"))
      (set-option! options "General" "Proceeds Account"
                   (assoc-ref account-alist "USD Cash"))

      ;; (gnc:dump-all-transactions)

      (let ((sxml (options->sxml uuid options "latest")))
        (test-equal "BUY 100 SPY"
          '("07/01/19" "Buy SPY" "100 SPY" "100 SPY" "Open Long" "CAD"
            "C$1.0000" "C$20,000.00" "C$9.95" "C$20,000.00" "C$9.95"
            "C$20,009.95" "C$0.00" "C$0.00" "C$0.00")
          (sxml->table-row-col sxml 1 1 #f))

        (test-equal "BUY 50 SPY"
          '("12/11/19" "Buy SPY" "50 SPY" "150 SPY" "Buy" "CAD" "C$1.0000"
            "C$16,000.00" "C$9.95" "C$16,000.00" "C$9.95" "C$200.10"
            "C$36,019.90" "C$0.00" "C$0.00" "C$0.00")
          (sxml->table-row-col sxml 1 2 #f))

        (test-equal "Sell 75 SPY"
          '("03/18/20" "Sell SPY" "-75 SPY" "75 SPY" "Sell" "CAD" "C$1.0000"
            "C$12,000.00" "C$9.95" "C$12,000.00" "C$9.95" "C$240.13"
            "C$18,009.95" "C$18,009.95" "-C$6,019.90" "-C$6,009.95"
            "C$11,990.05" "-C$6,009.95" "-C$6,019.90" "-C$6,019.90")
          (sxml->table-row-col sxml 1 3 #f))

        (test-equal "BUY 250 SPY"
          '("04/01/20" "Buy SPY" "250 SPY" "325 SPY" "Buy" "CAD" "C$1.0000"
            "C$42,000.00" "C$9.95" "C$42,000.00" "C$9.95" "C$240.13"
            "C$60,019.90" "-C$6,009.95" "-C$6,019.90" "-C$6,019.90")
          (sxml->table-row-col sxml 1 4 #f))

        (test-equal "Return Capital $2500"
          '("04/16/20" "Return of Capital" "0 SPY" "325 SPY" "Return Capital"
            "CAD" "C$1.0000" "-C$2,500.00" "-C$2,500.00" "C$184.68"
            "C$57,519.90" "-C$6,009.95" "-C$6,019.90" "-C$6,019.90")
          (sxml->table-row-col sxml 1 5 #f))

        (test-equal "BUY 125 SPY"
          '("05/02/20" "Buy spy" "125 SPY" "450 SPY" "Buy" "CAD" "C$1.0000"
            "C$47,500.00" "C$0.00" "C$47,500.00" "C$0.00" "C$176.98"
            "C$105,019.90" "-C$6,009.95" "-C$6,019.90" "-C$6,019.90")
          (sxml->table-row-col sxml 1 6 #f))

        (test-equal "2:1 split"
          '("05/11/20" "stock split" "450 SPY" "900 SPY" "2:1 Split"
            "CAD" "C$1.0000" "C$233.38" "C$105,019.90" "-C$6,009.95"
            "-C$6,019.90" "-C$6,019.90")
          (sxml->table-row-col sxml 1 7 #f))

        (test-equal "sell 135 SPY"
          '("05/21/20" "Sell SPY" "-135 SPY" "765 SPY" "Sell" "CAD" "C$1.0000"
            "C$21,500.00" "C$9.95" "C$21,500.00" "C$9.95" "C$116.69"
            "C$15,752.98" "C$89,266.92" "C$5,737.06" "C$5,747.02"
            "C$21,490.05" "-C$262.94" "-C$282.84" "-C$282.84")
          (sxml->table-row-col sxml 1 8 #f))

        (test-equal "BUY 150 SPY"
          '("06/03/20" "Buy spy" "150 SPY" "915 SPY" "Buy" "CAD" "C$1.0000"
            "C$21,000.00" "C$0.00" "C$21,000.00" "C$0.00" "C$116.69"
            "C$110,266.92" "-C$262.94" "-C$282.84" "-C$282.84")
          (sxml->table-row-col sxml 1 9 #f))

        (test-equal "sell 915 SPY close long"
          '("06/10/20" "Sell SPY" "-915 SPY" "0 SPY" "Close Long" "CAD"
            "C$1.0000" "C$128,100.00" "C$9.95" "C$128,100.00" "C$9.95"
            "C$120.51" "C$110,266.92" "C$0.00" "C$17,823.14" "C$17,833.08"
            "C$128,090.05" "C$17,570.15" "C$17,540.30" "C$17,540.30")
          (sxml->table-row-col sxml 1 10 #f))

        (test-equal "short-sell 85 SPY"
          '("06/10/20" "Sell SPY Short" "-85 SPY" "-85 SPY" "Open Short"
            "CAD" "C$1.0000" "-C$11,900.00" "C$9.95" "-C$11,900.00" "C$9.95"
            "-C$11,890.05" "C$17,570.15" "C$17,540.30" "C$17,540.30")
          (sxml->table-row-col sxml 1 11 #f))

        (test-equal "short-sell 65 SPY"
          '("06/15/20" "Sell SPY Short" "-65 SPY" "-150 SPY" "Short Sell"
            "CAD" "C$1.0000" "-C$11,050.00" "C$9.95" "-C$11,050.00" "C$9.95"
            "C$139.88" "-C$22,930.10" "C$17,570.15" "C$17,540.30" "C$17,540.30")
          (sxml->table-row-col sxml 1 12 #f))

        (test-equal "buy 50 SPY short"
          '("06/18/20" "Buy SPY Close Short" "50 SPY" "-100 SPY" "Short Buy"
            "CAD" "C$1.0000" "-C$5,000.00" "C$9.95" "-C$5,000.00" "C$9.95"
            "C$152.87" "-C$7,643.37" "-C$15,286.73" "C$2,633.42" "C$2,643.37"
            "-C$5,009.95" "C$20,213.52" "C$20,173.72" "C$20,173.72")
          (sxml->table-row-col sxml 1 13 #f))

        (test-equal "BUY 100 SPY close short"
          '("06/20/20" "Buy SPY Close Short" "100 SPY" "0 SPY" "Close Short"
            "CAD" "C$1.0000" "-C$8,000.00" "C$4.98" "-C$8,000.00" "C$4.98"
            "C$152.87" "-C$15,286.73" "C$0.00" "C$7,281.75" "C$7,286.73"
            "-C$8,004.98" "C$27,500.25" "C$27,455.47" "C$27,455.47")
          (sxml->table-row-col sxml 1 14 #f))

        (test-equal "BUY 100 SPY"
          '("06/21/20" "Buy SPY" "100 SPY" "100 SPY" "Open Long" "CAD"
            "C$1.0000" "C$8,000.00" "C$4.98" "C$8,000.00" "C$4.98"
            "C$8,004.98" "C$27,500.25" "C$27,455.47" "C$27,455.47")
          (sxml->table-row-col sxml 1 15 #f))))
    (gnc-clear-current-session)))


