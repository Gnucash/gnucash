(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; This is implementation testing for balsheet-pnl report.

(define uuid-list
  (list (cons 'pnl "0e94fd0277ba11e8825d43e27232c9d4")
        (cons 'balsheet "065d5d5a77ba11e8b31e83ada73c5eea")))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "balsheet-pnl.scm")
  (null-test 'pnl)
  (null-test 'balsheet)
  (balsheet-pnl-tests)
  (test-end "balsheet-pnl.scm"))

(define (options->sxml uuid options test-title)
  (gnc:options->sxml uuid options "test-pnl" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (symbol->commodity symbol)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   symbol))

(gnc-commodity-set-user-symbol (symbol->commodity "GBP") "#")

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ROOT))
        (list "Asset" (list (cons 'type ACCT-TYPE-ASSET))
              (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE)))
              (list "Bank-GBP" (list (cons 'commodity (symbol->commodity "GBP"))))
              (list "Bank"
                    (list "Bank-1"
                          (list "Bank-1-1"
                                (list "Bank-1-1-1")
                                (list "Bank-1-1-2"))
                          (list "Bank-1-2")
                          (list "Bank-1-3"))))
        (list "Liability" (list (cons 'type ACCT-TYPE-PAYABLE))
              (list "Bank1"
                    (list "Loan1")
                    (list "Loan2"))
              (list "CreditCard")
              (list "A/Payable"))
        (list "GST" (list (cons 'type ACCT-TYPE-ASSET))
              (list "BAS")
              (list "GST Sales" (list (cons 'type ACCT-TYPE-LIABILITY)))
              (list "GST Purchases"))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Income-GBP" (list (cons 'type ACCT-TYPE-INCOME)
                                 (cons 'commodity (symbol->commodity "GBP"))))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))))

(define (null-test variant)
  ;; This null-test tests for the presence of report.
  (let* ((uuid (assq-ref uuid-list variant))
         (options (gnc:make-report-options uuid)))
    (test-assert "null-test" (options->sxml uuid options "null-test"))))

(define (balsheet-pnl-tests)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank1 (cdr (assoc "Bank" account-alist)))
         (bank2 (cdr (assoc "Bank-1" account-alist)))
         (bank3 (cdr (assoc "Bank-1-1" account-alist)))
         (bank4 (cdr (assoc "Bank-1-1-1" account-alist)))
         (bank5 (cdr (assoc "Bank-1-1-2" account-alist)))
         (bank6 (cdr (assoc "A/Receivable" account-alist)))
         (bank-gbp (cdr (assoc "Bank-GBP" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (income-gbp (cdr (assoc "Income-GBP" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (creditcard (cdr (assoc "CreditCard" account-alist)))
         (payable (cdr (assoc "A/Payable" account-alist)))
         (receivable (cdr (assoc "A/Receivable" account-alist)))
         (YEAR (- (gnc:time64-get-year (gnc:get-today)) 5)))

    (define (default-testing-options uuid)
      (let ((options (gnc:make-report-options uuid)))
        ;; (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        ;; (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
        ;; (set-option! options "Display" "Hierarchical subtotals" #t)
        (set-option! options "General" "Disable indenting for export?" #t)
        options))

    (define* (create-txn DD MM YY DESC list-of-splits #:optional txn-type)
      (let ((txn (xaccMallocTransaction (gnc-get-current-book))))
        (xaccTransBeginEdit txn)
        (xaccTransSetDescription txn DESC)
        (xaccTransSetCurrency txn (gnc-default-report-currency))
        (xaccTransSetDate txn DD MM YY)
        (for-each
         (lambda (tfr)
           (let ((split (xaccMallocSplit (gnc-get-current-book))))
             (xaccSplitSetParent split txn)
             (xaccSplitSetAccount split (cdr tfr))
             (xaccSplitSetValue split (car tfr))
             (xaccSplitSetAmount split (car tfr))))
         list-of-splits)
        (if txn-type
            (xaccTransSetTxnType txn txn-type))
        (xaccTransCommitEdit txn)
        txn))

    (create-txn 1 3 YEAR "receive #100 in GBP"
                (list (cons -100 income-gbp)
                      (cons  100 bank-gbp)))

    (gnc-pricedb-create (gnc-default-report-currency) (symbol->commodity "GBP")
                        (gnc-dmy2time64 1 3 YEAR) 125/100)

    (gnc-pricedb-create (gnc-default-report-currency) (symbol->commodity "GBP")
                        (gnc:get-today) 130/100)

    (create-txn 1 3 YEAR "multiplebank income"
                (list (cons -1111110 income)
                      (cons 1000000 bank1)
                      (cons 100000 bank2)
                      (cons 10000 bank3)
                      (cons 1000 bank4)
                      (cons 100 bank5)
                      (cons 10 bank6)))

    ;; Finally we can begin testing
    (test-begin "balsheet tests")
    (let* ((uuid (assq-ref uuid-list 'balsheet))
           (options (default-testing-options uuid)))
      (let ((sxml (options->sxml uuid options "balsheet-default")))
        (test-equal "balsheet default options. net worth is #100 $1,111,110.00"
          '("#100.00" "$1,111,110.00")
          (sxml->table-row-col sxml 1 49 2))
        (test-equal "balsheet default amounts column are as expected"
          '("$0.00" "$1,000,000.00" "$100,000.00" "$10,000.00" "$1,000.00"
            "$100.00" "$11,100.00" "$0.00" "$0.00" "$111,100.00" "$1,111,100.00" "#100.00"
            "$10.00" "#100.00" "$1,111,110.00" "$0.00" "$0.00" "$0.00" "$0.00" "#100.00"
            "$1,111,110.00" "$0.00" "$0.00" "$0.00" "$0.00" "$0.00" "$0.00" "$0.00"
            "$0.00" "$0.00" "$0.00" "$0.00" "$0.00" "#100.00" "$1,111,110.00")
          (cdr (sxml->table-row-col sxml 1 #f 2))))

      (set-option! options "Display" "Include accounts with zero total balances" #f)
      (set-option! options "Display" "Omit zero balance figures" #t)
      (set-option! options "Display" "Hierarchical subtotals" #f)
      (let ((sxml (options->sxml uuid options "balsheet-swap-display-options")))
        (test-equal "balsheet swap display options. amounts are as expected."
          '("#100.00" "$1,111,110.00" "$1,111,100.00" "$111,100.00" "$11,100.00"
            "$1,000.00" "$100.00" "#100.00" "$10.00" "#100.00" "$1,111,110.00" "$0.00"
            "$0.00" "#100.00" "$1,111,110.00")
          (cdr (sxml->table-row-col sxml 1 #f 2))))

      (set-option! options "Commodities" "Convert to common currency" #t)
      (set-option! options "Commodities" "Report's currency" (gnc-default-report-currency))
      (set-option! options "Commodities" "Price Source" 'nearest)
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 1 YEAR)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 1 3 YEAR)))
      (let ((sxml (options->sxml uuid options "balsheet-common-currency nearest")))
        (test-equal "balsheet common-currency. price-source=nearest. net worth is $1,111,235.00"
          '("$1,111,235.00")
          (sxml->table-row-col sxml 1 22 3))
        (test-equal "balsheet common-currency. nearest. amounts are as expected."
          '("$1,111,235.00" "$1,111,100.00" "$111,100.00" "$11,100.00" "$1,000.00" "$100.00"
            "#100.00" "$125.00" "$10.00" "$1,111,235.00" "$0.00" "$0.00" "$1,111,235.00")
          (cdr (sxml->table-row-col sxml 1 #f 3))))

      (set-option! options "Commodities" "Price Source" 'latest)
      (let ((sxml (options->sxml uuid options "balsheet-common-currency latest")))
        (test-equal "balsheet common-currency. price-source=latest. net worth is $1,111,240.00"
          '("$1,111,240.00")
          (sxml->table-row-col sxml 1 22 3))
        (test-equal "balsheet common-currency. latest. amounts are as expected."
          '("$1,111,240.00" "$1,111,100.00" "$111,100.00" "$11,100.00" "$1,000.00" "$100.00"
            "#100.00" "$130.00" "$10.00" "$1,111,240.00" "$0.00" "$0.00" "$1,111,240.00")
          (cdr (sxml->table-row-col sxml 1 #f 3))))
      )

    (test-end "balsheet tests")

    ;; Finally we can begin testing
    (test-begin "pnl tests")
    (let* ((uuid (assq-ref uuid-list 'pnl))
           (options (default-testing-options uuid)))
      (let ((sxml (options->sxml uuid options "pnl-default")))
        (test-equal "pnl default options. profit is #0.00, $0.00"
          '("#0.00" "$0.00")
          (sxml->table-row-col sxml 1 13 2)))

      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 1 YEAR)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 1 3 YEAR)))
      (let ((sxml (options->sxml uuid options "pnl-basic")))
        (test-equal "pnl basic options. profit is -#100.00 -$1,111,110.00"
          '("-#100.00" "-$1,111,110.00")
          (sxml->table-row-col sxml 1 13 2)))

      (set-option! options "Display" "Include accounts with zero total balances" #f)
      (set-option! options "Display" "Omit zero balance figures" #t)
      (set-option! options "Display" "Hierarchical subtotals" #f)
      (let ((sxml (options->sxml uuid options "pnl-swap-display-options")))
        (test-equal "pnl swap display options. amounts are as expected."
          '("-$1,111,110.00" "-#100.00" "-#100.00"
            "-$1,111,110.00" "$0.00" "-#100.00" "-$1,111,110.00")
          (cddr (sxml->table-row-col sxml 1 #f 2))))

      (set-option! options "Commodities" "Convert to common currency" #t)
      (set-option! options "Commodities" "Report's currency" (gnc-default-report-currency))
      (set-option! options "Commodities" "Price Source" 'midperiod)
      (set-option! options "Commodities" "Show Foreign Currencies" #f)
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 1 YEAR)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 1 3 YEAR)))
      (let ((sxml (options->sxml uuid options "pnl-common-currency midperiod")))
        (test-equal "pnl common-currency. price-source=midperiod. net worth is $1,111,235.00"
          '("-$1,111,235.00")
          (sxml->table-row-col sxml 1 12 2))
        (test-equal "pnl common-currency. amounts are as expected."
          '("-$1,111,110.00" "-$125.00" "-$1,111,235.00" "$0.00" "-$1,111,235.00")
          (cddr (sxml->table-row-col sxml 1 #f 2)))
        )

      (set-option! options "Commodities" "Price Source" 'latest)
      (set-option! options "Commodities" "Show Foreign Currencies" #t)
      (let ((sxml (options->sxml uuid options "pnl-common-currency latest")))
        (test-equal "pnl common-currency. price-source=latest. net worth is $1,111,240.00"
          '("-$1,111,240.00")
          (sxml->table-row-col sxml 1 12 2))
        (test-equal "pnl common-currency. amounts are as expected."
          '("-$1,111,110.00" "-#100.00" "-$130.00" "-$1,111,240.00" "$0.00" "-$1,111,240.00")
          (cddr (sxml->table-row-col sxml 1 #f 2))))
      )
    (test-end "pnl tests")

    ))
