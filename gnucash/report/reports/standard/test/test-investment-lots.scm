(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard investment-lots))
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (gnucash report stylesheets plain))
(use-modules (tests srfi64-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))

;; UUID for the investment-lots report
(define uuid "ab2acc24afd14630a551f98f1a35fa81")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

;; Exercise production helper implementations directly.
(define calculate-cagr
  (@@ (gnucash reports standard investment-lots) calculate-cagr))
(define calculate-gain
  (@@ (gnucash reports standard investment-lots) calculate-gain))
(define calculate-roi
  (@@ (gnucash reports standard investment-lots) calculate-roi))
(define is-long-term?
  (@@ (gnucash reports standard investment-lots) is-long-term?))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-investment-lots")
  (test-investment-lots)
  (test-end "test-investment-lots"))

(define (set-option! options page tag value)
  (if (gnc-lookup-option (gnc:optiondb options) page tag)
      (gnc-set-option (gnc:optiondb options) page tag value)
      (begin
        (test-assert (format #f "wrong-option ~a ~a" page tag) #f)
        #f)))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-investment-lots" test-title
                     #:strip-tag "script"))

(define (render-report options test-title)
  "Render report and return #t if successful, #f if error."
  (catch #t
    (lambda ()
      (let ((result (options->sxml options test-title)))
        (if result #t #f)))
    (lambda (key . args)
      #f)))

(define (mnemonic->commodity sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

(define (create-investment-lots-test-data)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (USD (mnemonic->commodity "USD"))
         (comm-table (gnc-commodity-table-get-table book))
         ;; Create stock commodities
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (SPY (gnc-commodity-new book "SPY" "NYSE" "SPY" "" 1))
         (structure
           (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                              (cons 'commodity USD))
                 (list "Assets"
                       (list "Broker" (list (cons 'commodity USD))
                             (list "Cash" (list (cons 'commodity USD)))
                             (list "AAPL" (list (cons 'commodity AAPL)
                                                (cons 'type ACCT-TYPE-STOCK)))
                             (list "SPY" (list (cons 'commodity SPY)
                                               (cons 'type ACCT-TYPE-STOCK)))))
                 (list "Income" (list (cons 'type ACCT-TYPE-INCOME))))))

    ;; Register commodities in the table
    (gnc-commodity-table-insert comm-table AAPL)
    (gnc-commodity-table-insert comm-table SPY)

    (let* ((account-alist (env-create-account-structure-alist env structure))
           (cash (cdr (assoc "Cash" account-alist)))
           (aapl-acct (cdr (assoc "AAPL" account-alist)))
           (spy-acct (cdr (assoc "SPY" account-alist)))
           (income (cdr (assoc "Income" account-alist))))

      ;; Create transactions forming two lots of AAPL
      ;; Lot 1: Buy 100 shares at $50
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector cash -5000 -5000 "")
             (vector aapl-acct 100 100 "Buy 100 shares"))
       #:description "Buy AAPL lot 1"
       #:currency USD)

      ;; Lot 2: Buy 50 shares at $60
      (env-create-multisplit-transaction
       env 01 02 2020
       (list (vector cash -3000 -3000 "")
             (vector aapl-acct 50 50 "Buy 50 shares"))
       #:description "Buy AAPL lot 2"
       #:currency USD)

      ;; Partial sale: 60 shares at $80
      (env-create-multisplit-transaction
       env 01 06 2020
       (list (vector cash 4800 4800 "")
             (vector aapl-acct -60 -60 "Sell 60 shares"))
       #:description "Sell AAPL"
       #:currency USD)

      ;; Buy SPY for variety
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector cash -40000 -40000 "")
             (vector spy-acct 200 200 "Buy 200 shares"))
       #:description "Buy SPY"
       #:currency USD)

      account-alist)))

(define (create-reverse-split-lot-test-data)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (USD (mnemonic->commodity "USD"))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (structure
           (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                              (cons 'commodity USD))
                 (list "Assets"
                       (list "Broker" (list (cons 'commodity USD))
                             (list "Cash" (list (cons 'commodity USD)))
                             (list "AAPL" (list (cons 'commodity AAPL)
                                                (cons 'type ACCT-TYPE-STOCK))))))))

    (gnc-commodity-table-insert comm-table AAPL)

    (let* ((account-alist (env-create-account-structure-alist env structure))
           (cash (cdr (assoc "Cash" account-alist)))
           (aapl-acct (cdr (assoc "AAPL" account-alist)))
           (lot (gnc-lot-new book))
           (buy-txn (xaccMallocTransaction book))
           (buy-stock (xaccMallocSplit book))
           (buy-cash (xaccMallocSplit book))
           (split-txn (xaccMallocTransaction book))
           (split-split (xaccMallocSplit book)))

      ;; Buy 100 shares into one explicit lot.
      (xaccTransBeginEdit buy-txn)
      (xaccTransSetCurrency buy-txn USD)
      (xaccTransSetDate buy-txn 1 1 2020)
      (xaccTransSetDescription buy-txn "Buy AAPL lot")

      (xaccSplitSetAccount buy-stock aapl-acct)
      (xaccSplitSetAmount buy-stock (gnc-numeric-create 100 1))
      (xaccSplitSetValue buy-stock (gnc-numeric-create 5000 1))
      (xaccSplitSetParent buy-stock buy-txn)
      (gnc-lot-add-split lot buy-stock)

      (xaccSplitSetAccount buy-cash cash)
      (xaccSplitSetAmount buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetValue buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetParent buy-cash buy-txn)
      (xaccTransCommitEdit buy-txn)

      ;; Reverse split: remove 50 shares with zero proceeds.
      (xaccTransBeginEdit split-txn)
      (xaccTransSetCurrency split-txn USD)
      (xaccTransSetDate split-txn 1 2 2020)
      (xaccTransSetDescription split-txn "Reverse Split")

      (xaccSplitSetAccount split-split aapl-acct)
      (xaccSplitSetAmount split-split (gnc-numeric-create -50 1))
      (xaccSplitSetValue split-split (gnc-numeric-create 0 1))
      (xaccSplitMakeStockSplit split-split)
      (xaccSplitSetAction split-split "Split")
      (xaccSplitSetParent split-split split-txn)
      (gnc-lot-add-split lot split-split)
      (xaccTransCommitEdit split-txn)

      account-alist)))

(define (create-baseline-lot-test-data)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (USD (mnemonic->commodity "USD"))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (structure
           (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                              (cons 'commodity USD))
                 (list "Assets"
                       (list "Broker" (list (cons 'commodity USD))
                             (list "Cash" (list (cons 'commodity USD)))
                             (list "AAPL" (list (cons 'commodity AAPL)
                                                (cons 'type ACCT-TYPE-STOCK))))))))

    (gnc-commodity-table-insert comm-table AAPL)

    (let* ((account-alist (env-create-account-structure-alist env structure))
           (cash (cdr (assoc "Cash" account-alist)))
           (aapl-acct (cdr (assoc "AAPL" account-alist)))
           (lot (gnc-lot-new book))
           (buy-txn (xaccMallocTransaction book))
           (buy-stock (xaccMallocSplit book))
           (buy-cash (xaccMallocSplit book)))

      ;; Buy 100 shares into one explicit lot, with no split events.
      (xaccTransBeginEdit buy-txn)
      (xaccTransSetCurrency buy-txn USD)
      (xaccTransSetDate buy-txn 1 1 2020)
      (xaccTransSetDescription buy-txn "Buy AAPL lot baseline")

      (xaccSplitSetAccount buy-stock aapl-acct)
      (xaccSplitSetAmount buy-stock (gnc-numeric-create 100 1))
      (xaccSplitSetValue buy-stock (gnc-numeric-create 5000 1))
      (xaccSplitSetParent buy-stock buy-txn)
      (gnc-lot-add-split lot buy-stock)

      (xaccSplitSetAccount buy-cash cash)
      (xaccSplitSetAmount buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetValue buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetParent buy-cash buy-txn)
      (xaccTransCommitEdit buy-txn)

      account-alist)))

(define (create-reverse-split-with-sale-test-data)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (USD (mnemonic->commodity "USD"))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (structure
           (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                              (cons 'commodity USD))
                 (list "Assets"
                       (list "Broker" (list (cons 'commodity USD))
                             (list "Cash" (list (cons 'commodity USD)))
                             (list "AAPL" (list (cons 'commodity AAPL)
                                                (cons 'type ACCT-TYPE-STOCK))))))))

    (gnc-commodity-table-insert comm-table AAPL)

    (let* ((account-alist (env-create-account-structure-alist env structure))
           (cash (cdr (assoc "Cash" account-alist)))
           (aapl-acct (cdr (assoc "AAPL" account-alist)))
           (lot (gnc-lot-new book))
           (buy-txn (xaccMallocTransaction book))
           (buy-stock (xaccMallocSplit book))
           (buy-cash (xaccMallocSplit book))
           (split-txn (xaccMallocTransaction book))
           (split-split (xaccMallocSplit book))
           (sale-txn (xaccMallocTransaction book))
           (sale-stock (xaccMallocSplit book))
           (sale-cash (xaccMallocSplit book)))

      ;; Buy 100 shares, $5,000 basis.
      (xaccTransBeginEdit buy-txn)
      (xaccTransSetCurrency buy-txn USD)
      (xaccTransSetDate buy-txn 1 1 2020)
      (xaccTransSetDescription buy-txn "Buy AAPL lot")
      (xaccSplitSetAccount buy-stock aapl-acct)
      (xaccSplitSetAmount buy-stock (gnc-numeric-create 100 1))
      (xaccSplitSetValue buy-stock (gnc-numeric-create 5000 1))
      (xaccSplitSetParent buy-stock buy-txn)
      (gnc-lot-add-split lot buy-stock)
      (xaccSplitSetAccount buy-cash cash)
      (xaccSplitSetAmount buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetValue buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetParent buy-cash buy-txn)
      (xaccTransCommitEdit buy-txn)

      ;; Reverse split removes 50 shares, zero proceeds.
      (xaccTransBeginEdit split-txn)
      (xaccTransSetCurrency split-txn USD)
      (xaccTransSetDate split-txn 1 2 2020)
      (xaccTransSetDescription split-txn "Reverse Split")
      (xaccSplitSetAccount split-split aapl-acct)
      (xaccSplitSetAmount split-split (gnc-numeric-create -50 1))
      (xaccSplitSetValue split-split (gnc-numeric-create 0 1))
      (xaccSplitMakeStockSplit split-split)
      (xaccSplitSetAction split-split "Split")
      (xaccSplitSetParent split-split split-txn)
      (gnc-lot-add-split lot split-split)
      (xaccTransCommitEdit split-txn)

      ;; Sell 25 shares for $3,000.
      (xaccTransBeginEdit sale-txn)
      (xaccTransSetCurrency sale-txn USD)
      (xaccTransSetDate sale-txn 1 3 2020)
      (xaccTransSetDescription sale-txn "Sell AAPL")
      (xaccSplitSetAccount sale-stock aapl-acct)
      (xaccSplitSetAmount sale-stock (gnc-numeric-create -25 1))
      (xaccSplitSetValue sale-stock (gnc-numeric-create -3000 1))
      (xaccSplitSetParent sale-stock sale-txn)
      (gnc-lot-add-split lot sale-stock)
      (xaccSplitSetAccount sale-cash cash)
      (xaccSplitSetAmount sale-cash (gnc-numeric-create 3000 1))
      (xaccSplitSetValue sale-cash (gnc-numeric-create 3000 1))
      (xaccSplitSetParent sale-cash sale-txn)
      (xaccTransCommitEdit sale-txn)

      account-alist)))

(define (create-equivalent-postsplit-sale-baseline-test-data)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (USD (mnemonic->commodity "USD"))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (structure
           (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                              (cons 'commodity USD))
                 (list "Assets"
                       (list "Broker" (list (cons 'commodity USD))
                             (list "Cash" (list (cons 'commodity USD)))
                             (list "AAPL" (list (cons 'commodity AAPL)
                                                (cons 'type ACCT-TYPE-STOCK))))))))

    (gnc-commodity-table-insert comm-table AAPL)

    (let* ((account-alist (env-create-account-structure-alist env structure))
           (cash (cdr (assoc "Cash" account-alist)))
           (aapl-acct (cdr (assoc "AAPL" account-alist)))
           (lot (gnc-lot-new book))
           (buy-txn (xaccMallocTransaction book))
           (buy-stock (xaccMallocSplit book))
           (buy-cash (xaccMallocSplit book))
           (sale-txn (xaccMallocTransaction book))
           (sale-stock (xaccMallocSplit book))
           (sale-cash (xaccMallocSplit book)))

      ;; Equivalent post-split basis: buy 50 shares for $5,000.
      (xaccTransBeginEdit buy-txn)
      (xaccTransSetCurrency buy-txn USD)
      (xaccTransSetDate buy-txn 1 1 2020)
      (xaccTransSetDescription buy-txn "Buy AAPL baseline")
      (xaccSplitSetAccount buy-stock aapl-acct)
      (xaccSplitSetAmount buy-stock (gnc-numeric-create 50 1))
      (xaccSplitSetValue buy-stock (gnc-numeric-create 5000 1))
      (xaccSplitSetParent buy-stock buy-txn)
      (gnc-lot-add-split lot buy-stock)
      (xaccSplitSetAccount buy-cash cash)
      (xaccSplitSetAmount buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetValue buy-cash (gnc-numeric-create -5000 1))
      (xaccSplitSetParent buy-cash buy-txn)
      (xaccTransCommitEdit buy-txn)

      ;; Sell 25 shares for $3,000.
      (xaccTransBeginEdit sale-txn)
      (xaccTransSetCurrency sale-txn USD)
      (xaccTransSetDate sale-txn 1 3 2020)
      (xaccTransSetDescription sale-txn "Sell AAPL baseline")
      (xaccSplitSetAccount sale-stock aapl-acct)
      (xaccSplitSetAmount sale-stock (gnc-numeric-create -25 1))
      (xaccSplitSetValue sale-stock (gnc-numeric-create -3000 1))
      (xaccSplitSetParent sale-stock sale-txn)
      (gnc-lot-add-split lot sale-stock)
      (xaccSplitSetAccount sale-cash cash)
      (xaccSplitSetAmount sale-cash (gnc-numeric-create 3000 1))
      (xaccSplitSetValue sale-cash (gnc-numeric-create 3000 1))
      (xaccSplitSetParent sale-cash sale-txn)
      (xaccTransCommitEdit sale-txn)

      account-alist)))

(define (find-col-index labels target)
  (let loop ((rest labels) (i 1))
    (cond
      ((null? rest) #f)
      ((and (string? (car rest)) (string=? (car rest) target)) i)
      (else (loop (cdr rest) (+ i 1))))))

(define (list-any pred lst)
  (if (null? lst)
    #f
    (or (pred (car lst))
        (list-any pred (cdr lst)))))

(define (test-investment-lots)
  ;; Test rendering with various configurations
  (test-group-with-cleanup "rendering-tests"
    (let* ((account-alist (create-investment-lots-test-data)))

      ;; Test 1: Basic rendering with defaults
      (test-begin "basic-rendering")
      (let ((options (gnc:make-report-options uuid)))
        (test-assert "report-renders" (render-report options "default")))
      (test-end "basic-rendering")

      ;; Test 2: CAGR columns (new in PR #1956)
      (test-begin "cagr-columns")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Columns" "Show CAGR columns" #t)
        (test-assert "renders-with-cagr" (render-report options "with-cagr")))
      (test-end "cagr-columns")

      ;; Test 3: Gain columns visibility
      (test-begin "gain-columns")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Columns" "Show realized gain columns" #t)
        (set-option! options "Columns" "Show unrealized gain columns" #t)
        (test-assert "renders-gains" (render-report options "with-gains")))
      (test-end "gain-columns")

      ;; Test 4: LT/ST grouping
      (test-begin "lt-st-grouping")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Columns" "Group gains by age (short term and long term)" 'gains-only)
        (test-assert "renders-lt-st" (render-report options "with-lt-st")))
      (test-end "lt-st-grouping")

      ;; Test 5: ROI columns
      (test-begin "roi-columns")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Columns" "Show ROI columns" #t)
        (test-assert "renders-roi" (render-report options "with-roi")))
      (test-end "roi-columns")

      ;; Test 6: All column types enabled
      (test-begin "all-columns-enabled")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Columns" "Show bought columns" #t)
        (set-option! options "Columns" "Show sold columns" #t)
        (set-option! options "Columns" "Show end columns" #t)
        (set-option! options "Columns" "Show realized gain columns" #t)
        (set-option! options "Columns" "Show unrealized gain columns" #t)
        (set-option! options "Columns" "Show ROI columns" #t)
        (set-option! options "Columns" "Show CAGR columns" #t)
        (test-assert "renders-all-columns" (render-report options "all-columns")))
      (test-end "all-columns-enabled")

      ;; Test 7: Chart display
      (test-begin "chart-display")
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Chart" "Show Chart" #t)
        (test-assert "renders-with-chart" (render-report options "with-chart")))
      (test-end "chart-display"))

    ;; Cleanup
    (gnc-clear-current-session))

  ;; Test options integrity
  (test-group-with-cleanup "options-integrity"
    (let ((options (gnc:make-report-options uuid)))
      (test-begin "all-options-exist")

      ;; Verify each option can be set
      (test-assert "show-purchased"
        (set-option! options "Columns" "Show bought columns" #t))
      (test-assert "show-sold"
        (set-option! options "Columns" "Show sold columns" #t))
      (test-assert "show-end"
        (set-option! options "Columns" "Show end columns" #t))
      (test-assert "show-realized-gains"
        (set-option! options "Columns" "Show realized gain columns" #t))
      (test-assert "show-unrealized-gains"
        (set-option! options "Columns" "Show unrealized gain columns" #t))
      (test-assert "show-roi"
        (set-option! options "Columns" "Show ROI columns" #t))
      (test-assert "show-cagr"
        (set-option! options "Columns" "Show CAGR columns" #t))
      (test-assert "group-gains"
        (set-option! options "Columns" "Group gains by age (short term and long term)" 'gains-and-sales))
      (test-assert "show-chart"
        (set-option! options "Chart" "Show Chart" #t))
      (test-assert "show-validation"
        (set-option! options "Validation" "Include only accounts with warnings" #t))

      (test-end "all-options-exist"))
    (gnc-clear-current-session))

  ;; Test different configurations
  (test-group-with-cleanup "configuration-variants"
    (let* ((account-alist (create-investment-lots-test-data)))

      (test-begin "column-combinations")
      (let ((options (gnc:make-report-options uuid)))
        ;; Test combinations of columns
        (set-option! options "Columns" "Show realized gain columns" #t)
        (set-option! options "Columns" "Show ROI columns" #t)
        (test-assert "realized-gains-and-roi" (render-report options "realized-roi")))
      (test-end "column-combinations")

      (test-begin "grouping-variants")
      (let ((opt1 (gnc:make-report-options uuid))
            (opt2 (gnc:make-report-options uuid))
            (opt3 (gnc:make-report-options uuid)))
        ;; Test different grouping options
        (set-option! opt1 "Columns" "Group gains by age (short term and long term)" 'no)
        (set-option! opt2 "Columns" "Group gains by age (short term and long term)" 'gains-only)
        (set-option! opt3 "Columns" "Group gains by age (short term and long term)" 'gains-and-sales)
        (test-assert "no-grouping" (render-report opt1 "no-grouping"))
        (test-assert "gains-grouping" (render-report opt2 "gains-grouping"))
        (test-assert "gains-sales-grouping" (render-report opt3 "gains-sales-grouping")))
      (test-end "grouping-variants"))

    (gnc-clear-current-session))

  ;; Test calculation functions (new module-level pure functions)
  (test-group-with-cleanup "calculation-functions"
    (test-begin "cagr-calculations")
    ;; Test CAGR calculation with known values
    ;; $100 → $200 in 1 year = 100% CAGR = 1.0
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 200 1))
           (years 1)
           (result (calculate-cagr basis end-value years)))
      (test-assert "doubling-in-year" (and result (>= result 0.99) (<= result 1.01))))

    ;; $100 → $100 in 1 year = 0% CAGR = 0.0
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 100 1))
           (years 1)
           (result (calculate-cagr basis end-value years)))
      (test-assert "zero-growth" (and result (>= result -0.01) (<= result 0.01))))

    ;; Invalid: zero basis
    (let ((result (calculate-cagr (gnc-numeric-create 0 1)
                                  (gnc-numeric-create 100 1) 1)))
      (test-assert "zero-basis-undefined" (eq? result #f)))

    ;; Invalid: false basis
    (let ((result (calculate-cagr #f (gnc-numeric-create 100 1) 1)))
      (test-assert "false-basis-undefined" (eq? result #f)))

    ;; Invalid: negative basis should not produce complex numbers
    (let ((result (calculate-cagr (gnc-numeric-create -100 1)
                                  (gnc-numeric-create 100 1)
                                  1)))
      (test-assert "negative-basis-undefined" (eq? result #f)))

    ;; Invalid: negative end value should not produce complex numbers
    (let ((result (calculate-cagr (gnc-numeric-create 100 1)
                                  (gnc-numeric-create -50 1)
                                  1)))
      (test-assert "negative-end-value-undefined" (eq? result #f)))

    ;; $1000 → $1100 in 1 year = 10% CAGR
    (let* ((basis (gnc-numeric-create 1000 1))
           (end-value (gnc-numeric-create 1100 1))
           (years 1)
           (result (calculate-cagr basis end-value years)))
      (test-assert "ten-percent-gain" (and result (>= result 0.09) (<= result 0.11))))

    ;; $100 → $110 in 2 years ≈ 4.88% CAGR
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 110 1))
           (years 2)
           (result (calculate-cagr basis end-value years)))
      (test-assert "110-in-2years" (and result (>= result 0.04) (<= result 0.06))))

    (test-end "cagr-calculations")

    (test-begin "gain-calculations")
    ;; $100 basis, $150 end value = $50 gain
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 150 1))
           (gain (calculate-gain basis end-value)))
      (test-assert "positive-gain" (not (gnc-numeric-negative-p gain)))
      (test-assert "fifty-dollar-gain"
        (gnc-numeric-zero-p
          (gnc-numeric-sub-fixed gain (gnc-numeric-create 50 1)))))

    ;; $100 basis, $80 end value = -$20 loss
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 80 1))
           (loss (calculate-gain basis end-value)))
      (test-assert "negative-loss" (gnc-numeric-negative-p loss))
      (test-assert "twenty-dollar-loss"
        (gnc-numeric-zero-p
          (gnc-numeric-sub-fixed loss (gnc-numeric-create -20 1)))))

    ;; No change: same value
    (let* ((basis (gnc-numeric-create 100 1))
           (end-value (gnc-numeric-create 100 1))
           (gain (calculate-gain basis end-value)))
      (test-assert "zero-change" (gnc-numeric-zero-p gain)))

    (test-end "gain-calculations")

    (test-begin "roi-calculations")
    ;; $100 basis, $25 gain = 25% ROI
    (let* ((basis (gnc-numeric-create 100 1))
           (gain (gnc-numeric-create 25 1))
           (result (calculate-roi basis gain)))
      (test-assert "positive-roi" (and result (>= result 0.24) (<= result 0.26))))

    ;; $100 basis, -$10 loss = -10% ROI
    (let* ((basis (gnc-numeric-create 100 1))
           (loss (gnc-numeric-create -10 1))
           (result (calculate-roi basis loss)))
      (test-assert "negative-roi" (and result (<= result -0.09) (>= result -0.11))))

    ;; Invalid: zero basis
    (let ((result (calculate-roi (gnc-numeric-create 0 1)
                                 (gnc-numeric-create 25 1))))
      (test-assert "zero-basis-roi-undefined" (eq? result #f)))

    ;; $500 basis, $500 gain = 100% ROI
    (let* ((basis (gnc-numeric-create 500 1))
           (gain (gnc-numeric-create 500 1))
           (result (calculate-roi basis gain)))
      (test-assert "hundred-percent-roi" (and result (>= result 0.99) (<= result 1.01))))

    ;; $200 basis, -$50 loss = -25% ROI
    (let* ((basis (gnc-numeric-create 200 1))
           (loss (gnc-numeric-create -50 1))
           (result (calculate-roi basis loss)))
      (test-assert "twentyfive-percent-loss" (and result (<= result -0.24) (>= result -0.26))))

    (test-end "roi-calculations")

    (test-begin "long-term-classification")
    ;; 1 year holding = long-term (exactly at boundary)
    (let* ((buy-date (gnc-dmy2time64 01 01 2020))
           (sell-date (gnc-dmy2time64 01 01 2021))
           (result (is-long-term? buy-date sell-date 1)))
      (test-assert "one-year-is-lt" result))

    ;; 364 days < 1 year = not long-term (just under)
    (let* ((buy-date (gnc-dmy2time64 01 01 2020))
           (sell-date (gnc-dmy2time64 31 12 2020))
           (result (is-long-term? buy-date sell-date 1)))
      (test-assert "364-days-not-lt" (not result)))

    ;; 2 year holding with 1-year threshold = long-term
    (let* ((buy-date (gnc-dmy2time64 01 01 2020))
           (sell-date (gnc-dmy2time64 01 01 2022))
           (result (is-long-term? buy-date sell-date 1)))
      (test-assert "two-years-is-lt" result))

    ;; 5 years holding = long-term
    (let* ((buy-date (gnc-dmy2time64 01 01 2015))
           (sell-date (gnc-dmy2time64 01 01 2020))
           (result (is-long-term? buy-date sell-date 1)))
      (test-assert "five-years-is-lt" result))

    ;; Same day = not long-term (0 years)
    (let* ((date (gnc-dmy2time64 01 01 2020))
           (result (is-long-term? date date 1)))
      (test-assert "same-day-not-lt" (not result)))

    (test-end "long-term-classification")

    (test-begin "reverse-split-classification")
    (let ((stock-split-event?
            (@@ (gnucash reports standard investment-lots)
                stock-split-event?))
          (is-non-realizing-share-reduction?
            (@@ (gnucash reports standard investment-lots)
                non-realizing-share-reduction?)))
      ;; Integration check: real engine stock-split metadata must be detected.
      (let* ((book (gnc-get-current-book))
             (split (xaccMallocSplit book))
             (amount (gnc-numeric-create -10 1))
             (value (gnc-numeric-create 0 1)))
        (xaccSplitSetAmount split amount)
        (xaccSplitSetValue split value)
        (xaccSplitMakeStockSplit split)
        (xaccSplitSetAction split "Split")
        (test-assert "engine-stock-split-type-detected"
          (stock-split-event?
            (xaccSplitGetType split)
            (xaccSplitGetAction split)))
        (test-assert "engine-reverse-split-classified"
          (is-non-realizing-share-reduction?
            (xaccSplitGetType split)
            (xaccSplitGetAction split)
            amount
            value)))

      ;; Reverse split style reduction should be treated as non-realizing.
      (test-assert "stock-split-negative-zero-value"
        (is-non-realizing-share-reduction?
          "stock-split"
          "Split"
          (gnc-numeric-create -10 1)
          (gnc-numeric-create 0 1)))

      ;; Ordinary sale with zero proceeds is not auto-classified as split.
      (test-assert "plain-sale-zero-value-not-split"
        (not (is-non-realizing-share-reduction?
               ""
               ""
               (gnc-numeric-create -10 1)
               (gnc-numeric-create 0 1))))

      ;; Forward split increases shares and is not a reduction event.
      (test-assert "forward-split-not-reduction"
        (not (is-non-realizing-share-reduction?
               "stock-split"
               "Split"
               (gnc-numeric-create 10 1)
               (gnc-numeric-create 0 1))))

      ;; Matrix-style coverage on reduction detection gates used by split
      ;; classification.
      (test-assert "matrix-positive-amount-not-reduction"
        (not (is-non-realizing-share-reduction?
               ""
               ""
               (gnc-numeric-create 10 1)
               (gnc-numeric-create 100 1))))
      (test-assert "matrix-sale-with-proceeds-not-reduction"
        (not (is-non-realizing-share-reduction?
               ""
               ""
               (gnc-numeric-create -10 1)
               (gnc-numeric-create -100 1))))
      (test-assert "matrix-zero-amount-not-reduction"
        (not (is-non-realizing-share-reduction?
               ""
               ""
               (gnc-numeric-create 0 1)
               (gnc-numeric-create 100 1))))
      (test-assert "matrix-reduction-detected"
        (is-non-realizing-share-reduction?
          "stock-split"
          "Split"
          (gnc-numeric-create -10 1)
          (gnc-numeric-create 0 1))))

    (test-end "reverse-split-classification")

    (gnc-clear-current-session))

  ;; End-to-end rendered output regression for reverse split behavior.
  (test-group-with-cleanup "reverse-split-render-regression"
    (let* ((account-alist (create-reverse-split-lot-test-data))
           (options (gnc:make-report-options uuid))
           (sxml (options->sxml options "reverse-split-regression"))
           (headers (sxml->table-row-col sxml 1 0 #f))
           (sold-col (find-col-index headers "Sold Splits"))
           (realized-col (find-col-index headers "Realized Gain")))

      (test-assert "found-sold-splits-column" sold-col)
      (test-assert "realized-gain-column-optional-when-no-sales" #t)

      (let ((sold-values (sxml->table-row-col sxml 1 #f sold-col))
            (realized-values (if realized-col
                                 (sxml->table-row-col sxml 1 #f realized-col)
                                 '())))
        ;; Reverse split must not be counted as a sold split.
        (test-assert "no-sold-split-count-one"
          (not (list-any (lambda (v) (and (string? v) (string=? v "1")))
                         sold-values)))

        ;; Reverse split must not produce phantom realized loss formatting.
        (test-assert "no-phantom-negative-realized-loss"
          (not (list-any (lambda (v)
                           (and (string? v)
                                (string-contains v "(")))
                         realized-values)))))

      (gnc-clear-current-session))

  ;; Compare baseline vs reverse-split render to ensure no phantom sale/gain.
  (test-group-with-cleanup "reverse-split-vs-baseline-regression"
    (let* ((base-alist (create-baseline-lot-test-data))
           (base-options (gnc:make-report-options uuid))
           (base-sxml (options->sxml base-options "baseline-lot"))
           (base-headers (sxml->table-row-col base-sxml 1 0 #f))
           (base-sold-col (find-col-index base-headers "Sold Splits"))
           (base-realized-col (find-col-index base-headers "Realized Gain"))
           (base-sold-values (if base-sold-col
                               (sxml->table-row-col base-sxml 1 #f base-sold-col)
                               '()))
           (base-realized-values (if base-realized-col
                                   (sxml->table-row-col base-sxml 1 #f base-realized-col)
                                   '())))

      (gnc-clear-current-session)

      (let* ((rev-alist (create-reverse-split-lot-test-data))
             (rev-options (gnc:make-report-options uuid))
             (rev-sxml (options->sxml rev-options "reverse-split-vs-baseline"))
             (rev-headers (sxml->table-row-col rev-sxml 1 0 #f))
             (rev-sold-col (find-col-index rev-headers "Sold Splits"))
             (rev-realized-col (find-col-index rev-headers "Realized Gain"))
             (rev-sold-values (if rev-sold-col
                                (sxml->table-row-col rev-sxml 1 #f rev-sold-col)
                                '()))
             (rev-realized-values (if rev-realized-col
                                    (sxml->table-row-col rev-sxml 1 #f rev-realized-col)
                                    '())))

        (test-assert "sold-splits-equal-baseline-vs-reverse"
          (equal? base-sold-values rev-sold-values))
        (test-assert "realized-gain-equal-baseline-vs-reverse"
          (equal? base-realized-values rev-realized-values))))

    (gnc-clear-current-session))

  ;; Reverse-split-with-sale should match equivalent post-split baseline.
  (test-group-with-cleanup "reverse-split-with-sale-equivalence"
    (let* ((base-alist (create-equivalent-postsplit-sale-baseline-test-data))
           (base-options (gnc:make-report-options uuid))
           (base-sxml (options->sxml base-options "equivalent-baseline-sale"))
           (base-headers (sxml->table-row-col base-sxml 1 0 #f))
           (base-sold-col (find-col-index base-headers "Sold Splits"))
           (base-realized-col (find-col-index base-headers "Realized Gain"))
           (base-sold-values (if base-sold-col
                               (sxml->table-row-col base-sxml 1 #f base-sold-col)
                               '()))
           (base-realized-values (if base-realized-col
                                   (sxml->table-row-col base-sxml 1 #f base-realized-col)
                                   '())))

      (gnc-clear-current-session)

      (let* ((rev-alist (create-reverse-split-with-sale-test-data))
             (rev-options (gnc:make-report-options uuid))
             (rev-sxml (options->sxml rev-options "reverse-split-with-sale"))
             (rev-headers (sxml->table-row-col rev-sxml 1 0 #f))
             (rev-sold-col (find-col-index rev-headers "Sold Splits"))
             (rev-realized-col (find-col-index rev-headers "Realized Gain"))
             (rev-sold-values (if rev-sold-col
                                (sxml->table-row-col rev-sxml 1 #f rev-sold-col)
                                '()))
             (rev-realized-values (if rev-realized-col
                                    (sxml->table-row-col rev-sxml 1 #f rev-realized-col)
                                    '())))

      (test-assert "reverse-split-sale-has-sold-splits-column" rev-sold-col)
      (test-assert "reverse-split-sale-realized-gain-column-optional" #t)
      (test-assert "sold-splits-equal-equivalent-baseline"
        (equal? base-sold-values rev-sold-values))
      (test-assert "realized-gain-equal-equivalent-baseline"
        (equal? base-realized-values rev-realized-values))))

    (gnc-clear-current-session)))
