(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard balance-sheet))
(use-modules (gnucash reports standard income-statement))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; This is implementation testing for Balance Sheet and Profit&Loss.

(define balance-sheet-uuid "c4173ac99b2b448289bf4d11c731af13")
(define pnl-uuid "0b81a3bdfd504aff849ec2e8630524bc")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "balsheet and profit&loss")
  (null-test)
  (balsheet-pnl-tests)
  (test-end "balsheet and profit&loss"))

(define (options->sxml uuid options test-title)
  (gnc:options->sxml uuid options "test-balsheet-pnl" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (mnemonic->commodity sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

(define USD (gnc-default-report-currency)) ;default currency should be USD because LC_ALL="C"
(define GBP (mnemonic->commodity "GBP"))
(define FUNDS (gnc-commodity-new (gnc-get-current-book)
                                 "Funds"            ;fullname
                                 "FUNDS"            ;namespace
                                 "FUNDS"            ;mnemonic
                                 "FUNDS"            ;cusip
                                 1000               ;fraction
                                 ))
(gnc-commodity-set-user-symbol GBP "#")

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                     (cons 'commodity USD))
        (list "Asset"
              (list "Bank1"
                    (list "Savings")
                    (list "Bonds")
                    (list "Empty")
                    (list "Current"))
              (list "House")
              (list "ForeignBank" (list (cons 'commodity GBP))
                    (list "ForeignSavings"))
              (list "Broker"
                    (list "Funds" (list (cons 'type ACCT-TYPE-STOCK)
                                        (cons 'commodity FUNDS)))))
        (list "Liability" (list (cons 'type ACCT-TYPE-LIABILITY))
              (list "Bank2"
                    (list "Loan")
                    (list "CreditCard")))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
              (list "Income-GBP" (list (cons 'commodity GBP))))))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((balance-sheet-options (gnc:make-report-options balance-sheet-uuid)))
    (test-assert "null-test" (options->sxml balance-sheet-uuid balance-sheet-options "null-test"))))

(define (balsheet-pnl-tests)
  ;; This function will perform implementation testing on the transaction report.
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank1savings (cdr (assoc "Savings" account-alist)))
         (bank1bonds (cdr (assoc "Bonds" account-alist)))
         (bank1current (cdr (assoc "Current" account-alist)))
         (house (cdr (assoc "House" account-alist)))
         (foreignsavings (cdr (assoc "ForeignSavings" account-alist)))
         (broker (cdr (assoc "Broker" account-alist)))
         (brokerfunds (cdr (assoc "Funds" account-alist)))
         (bank2loan (cdr (assoc "Loan" account-alist)))
         (bank2creditcard (cdr (assoc "CreditCard" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (income-GBP (cdr (assoc "Income-GBP" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (default-balsheet-testing-options)
      (let ((balance-sheet-options (gnc:make-report-options balance-sheet-uuid)))
        (set-option! balance-sheet-options "General" "Balance Sheet Date" (cons 'absolute (gnc-dmy2time64 1 1 1971)))
        (set-option! balance-sheet-options "Accounts" "Levels of Subaccounts" 'all)
        (set-option! balance-sheet-options "Commodities" "Show Exchange Rates" #t)
        balance-sheet-options))

    (define (default-pnl-testing-options)
      (let ((pnl-options (gnc:make-report-options pnl-uuid)))
        (set-option! pnl-options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 1 1980)))
        (set-option! pnl-options "General" "End Date" (cons 'absolute (gnc-dmy2time64 1 1 1981)))
        (set-option! pnl-options "Accounts" "Levels of Subaccounts" 'all)
        (set-option! pnl-options "Commodities" "Show Exchange Rates" #t)
        pnl-options))

    ;; $100 in Savings account
    (env-transfer env 01 01 1970 equity bank1savings 100)
    ;; $2000 in Bonds
    (env-transfer env 01 01 1970 equity bank1bonds 2000)
    ;; $500 in Current Acc
    (env-transfer env 01 01 1970 equity bank1current 9000)
    ;; $100,000 in house
    (env-transfer env 01 01 1970 equity house 100000)

    ;; pre-existing GBPs
    (env-transfer-foreign env 01 01 1970 bank1current foreignsavings  130  100 #:description "buy 100GBP at $1.30")
    (env-transfer-foreign env 01 02 1970 bank1current foreignsavings  140  100 #:description "buy 100GBP at $1.40")
    (env-transfer-foreign env 15 02 1970 bank1current foreignsavings -142 -100 #:description "sell 100GBP at $1.42")
    (env-transfer-foreign env 01 03 1970 bank1current foreignsavings  150  100 #:description "buy 100GBP at $1.50")
    (env-transfer-foreign env 01 04 1970 bank1current foreignsavings  155  100 #:description "buy 100GBP at $1.55")
    (env-transfer-foreign env 15 04 1970 bank1current foreignsavings -157 -100 #:description "sell 100GBP at $1.57")
    (env-transfer-foreign env 01 05 1970 bank1current foreignsavings -160 -100 #:description "sell 100GBP at $1.60")
    (env-transfer-foreign env 01 06 1970 bank1current foreignsavings  155  100 #:description "buy 100GBP at $1.55")
    (env-transfer-foreign env 01 07 1970 bank1current foreignsavings -145 -100 #:description "sell 100GBP at $1.45")
    (env-transfer-foreign env 01 08 1970 bank1current foreignsavings  165  100 #:description "buy 100GBP at $1.65")

    ;; broker has $2000
    (env-transfer env 01 01 1970 equity broker 2000)

    ;; existing FUNDs = 200 USD on 01/01/1970
    (env-transfer-foreign env 01 01 1970 bank1current brokerfunds  2000  10 #:description "buy 10FUND at $200")
    (env-transfer-foreign env 01 02 1970 bank1current brokerfunds  2100  10 #:description "buy 10FUND at $210")
    (env-transfer-foreign env 01 03 1970 bank1current brokerfunds  2250  10 #:description "buy 10FUND at $225")
    (env-transfer-foreign env 01 04 1970 bank1current brokerfunds  2440  10 #:description "buy 10FUND at $244")
    (env-transfer-foreign env 01 05 1970 bank1current brokerfunds -2640 -10 #:description "sell 10FUND at $264")
    (env-transfer-foreign env 01 06 1970 bank1current brokerfunds -2550 -10 #:description "sell 10FUND at $255")
    (env-transfer-foreign env 01 07 1970 bank1current brokerfunds  2500  10 #:description "buy 10FUND at $250")

    ;; $9000 loan
    (env-transfer env 01 01 1970 equity bank2loan -9000)

    ;; $500 on creditcard debt
    (env-transfer env 01 01 1970 equity bank2creditcard -500)

    ;; further prices into pricedb
    ;; GBP = 1.50 to 1.90 USD
    (gnc-pricedb-create USD GBP (gnc-dmy2time64 1 1 1971) 15/10)
    (gnc-pricedb-create USD GBP (gnc-dmy2time64 1 1 1972) 16/10)
    (gnc-pricedb-create USD GBP (gnc-dmy2time64 1 1 1973) 17/10)
    (gnc-pricedb-create USD GBP (gnc-dmy2time64 1 1 1974) 18/10)
    (gnc-pricedb-create USD GBP (gnc-dmy2time64 1 1 1975) 19/10)
    ;; FUND = 300 to 500 USD
    (gnc-pricedb-create USD FUNDS (gnc-dmy2time64 1 1 1971) 300)
    (gnc-pricedb-create USD FUNDS (gnc-dmy2time64 1 1 1972) 350)
    (gnc-pricedb-create USD FUNDS (gnc-dmy2time64 1 1 1973) 400)
    (gnc-pricedb-create USD FUNDS (gnc-dmy2time64 1 1 1974) 450)
    (gnc-pricedb-create USD FUNDS (gnc-dmy2time64 1 1 1975) 500)

    ;; a couple INCOME transactions, a decade later
    (env-transfer env 01 01 1980 income bank1current 250)
    (env-transfer env 01 01 1980 income-GBP foreignsavings 500)
    (env-transfer-foreign env 01 02 1980 income-GBP bank1current 100 170 #:description "earn 100GBP into $170")

    ;; Finally we can begin testing balsheet
    (display "\n\n balsheet tests\n\n")
    (let* ((balance-sheet-options (default-balsheet-testing-options))
           (sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-default")))
      (test-equal "total assets = $116,009"
        (list "$116,009.00")
        (sxml->table-row-col sxml 1 15 6))
      (test-equal "total liabilities = $9,500.00"
        (list "$9,500.00")
        (sxml->table-row-col sxml 1 23 6))
      (test-equal "total equity  = $106,509.00"
        (list "$106,509.00")
        (sxml->table-row-col sxml 1 28 6))

      (set-option! balance-sheet-options "Commodities" "Price Source" 'weighted-average)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-weighted-average")))
        (test-equal "weighted average assets = $114,071.66"
          (list "$114,071.66")
          (sxml->table-row-col sxml 1 15 6)))

      (set-option! balance-sheet-options "Commodities" "Price Source" 'average-cost)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-average-cost")))
        (test-equal "average-cost assets = $113,100"
          (list "$113,100.00")
          (sxml->table-row-col sxml 1 15 6)))

      (set-option! balance-sheet-options "Commodities" "Price Source" 'pricedb-nearest)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-pricedb-nearest")))
        (test-equal "pricedb-nearest assets = $116,009"
          (list "$116,009.00")
          (sxml->table-row-col sxml 1 15 6)))

      (set-option! balance-sheet-options "Commodities" "Price Source" 'pricedb-latest)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-pricedb-latest")))
        (test-equal "pricedb-latest assets = $122,049"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 15 6)))

      ;; set multilevel subtotal style
      ;; verifies amount in EVERY line of the report.
      (set-option! balance-sheet-options "Display" "Parent account balances" 'immediate-bal)
      (set-option! balance-sheet-options "Display" "Parent account subtotals" 't)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-multilevel")))
        (test-equal "multilevel. root = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 3 5))
        (test-equal "multilevel. assets = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 4 4))
        (test-equal "multilevel. bank1 = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 5 3))
        (test-equal "multilevel. bonds = $2,000.00"
          (list "$2,000.00")
          (sxml->table-row-col sxml 1 6 3))
        (test-equal "multilevel. current = $2609.00"
          (list "$2,609.00")
          (sxml->table-row-col sxml 1 7 3))
        (test-equal "multilevel. empty = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 8 3))
        (test-equal "multilevel. savings = $100.00"
          (list "$100.00")
          (sxml->table-row-col sxml 1 9 3))
        (test-equal "multilevel. total bank1 = $4709"
          (list "$4,709.00")
          (sxml->table-row-col sxml 1 10 4))
        (test-equal "multilevel. broker = $2,000.00"
          (list "$2,000.00")
          (sxml->table-row-col sxml 1 11 3))
        (test-equal "multilevel. funds = $15,000.00"
          (list "30 FUNDS" "$15,000.00" "$15,000.00")
          (sxml->table-row-col sxml 1 12 3))
        (test-equal "multilevel. total broker = $17,000.00"
          (list "$17,000.00")
          (sxml->table-row-col sxml 1 13 4))
        (test-equal "multilevel. foreign = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 14 3))
        (test-equal "multilevel. foreignsavings = #200.00 = $340"
          (list "#200.00" "$340.00" "$340.00")
          (sxml->table-row-col sxml 1 15 3))
        (test-equal "multilevel. total foreign = $340"
          (list "$340.00")
          (sxml->table-row-col sxml 1 16 4))
        (test-equal "multilevel. house = $100,000"
          (list "$100,000.00")
          (sxml->table-row-col sxml 1 17 4))
        (test-equal "multilevel. total asset = $122,049"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 18 5))
        (test-equal "multilevel. total root = $122,049"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 19 6))
        (test-equal "multilevel. total assets = $122,049"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 20 6)))

      ;; set recursive-subtotal subtotal style
      (set-option! balance-sheet-options "Display" "Parent account balances" 'recursive-bal)
      (set-option! balance-sheet-options "Display" "Parent account subtotals" 'f)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-recursive")))
        (test-equal "recursive. root = $760+15000+104600"
          (list "#200.00" "$340.00" "30 FUNDS" "$15,000.00" "$106,709.00" "$106,709.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "recursive. assets = $760+15000+104600"
          (list "#200.00" "$340.00" "30 FUNDS" "$15,000.00" "$106,709.00" "$106,709.00")
          (sxml->table-row-col sxml 1 4 5))
        (test-equal "recursive. bank1 = $4,709.00"
          (list "$4,709.00")
          (sxml->table-row-col sxml 1 5 4))
        (test-equal "recursive. bonds = $2,000.00"
          (list "$2,000.00")
          (sxml->table-row-col sxml 1 6 3))
        (test-equal "recursive. current = $2609.00"
          (list "$2,609.00")
          (sxml->table-row-col sxml 1 7 3))
        (test-equal "recursive. empty = $0.00"
          (list "$0.00")
          (sxml->table-row-col sxml 1 8 3))
        (test-equal "recursive. savings = $100.00"
          (list "$100.00")
          (sxml->table-row-col sxml 1 9 3))
        (test-equal "recursive. broker = $15000+2000.00"
          (list "30 FUNDS" "$15,000.00" "$2,000.00" "$2,000.00")
          (sxml->table-row-col sxml 1 10 4))
        (test-equal "recursive. funds = $15,000.00"
          (list "30 FUNDS" "$15,000.00" "$15,000.00")
          (sxml->table-row-col sxml 1 11 3))
        (test-equal "recursive. foreign = $340.00"
          (list "#200.00" "$340.00")
          (sxml->table-row-col sxml 1 12 4))
        (test-equal "recursive. foreignsavings = #200.00 = $340"
          (list "#200.00" "$340.00" "$340.00")
          (sxml->table-row-col sxml 1 13 3))
        (test-equal "recursive. house = $100,000"
          (list "$100,000.00")
          (sxml->table-row-col sxml 1 14 4))
        (test-equal "recursive. total assets = $122,049.00"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 15 6)))

      (set-option! balance-sheet-options "Commodities" "Show Foreign Currencies" #f)
      (set-option! balance-sheet-options "Commodities" "Show Exchange Rates" #f)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-disable show-fcur show-rates")))
        (test-equal "show-fcur disabled"
          (list "$122,049.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "show-rates disabled"
          '()
          (sxml->table-row-col sxml 2 #f #f)))

      (set-option! balance-sheet-options "Commodities" "Show Foreign Currencies" #t)
      (set-option! balance-sheet-options "Commodities" "Show Exchange Rates" #t)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-enable show-fcur show-rates")))
        (test-equal "show-fcur enabled"
          (list "#200.00" "$340.00" "30 FUNDS" "$15,000.00" "$106,709.00" "$106,709.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "show-rates enabled"
          (list "1 FUNDS" "$500.00" "#1.00" "$1.70")
          (sxml->table-row-col sxml 2 #f #f)))

      ;;make-multilevel
      (set-option! balance-sheet-options "Display" "Parent account balances" 'immediate-bal)
      (set-option! balance-sheet-options "Display" "Parent account subtotals" 't)

      (set-option! balance-sheet-options "Display" "Omit zero balance figures" #t)
      (set-option! balance-sheet-options "Display" "Include accounts with zero total balances" #f)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-incl-zb-accts=#f omit-zb-bals=#t")))
        (test-equal "omit-zb-bals=#t"
          '()
          (sxml->table-row-col sxml 1 3 5))
        (test-equal "incl-zb-accts=#f"
          '("Savings" "$100.00")        ;i.e.skips "Empty" account with $0.00
          (sxml->table-row-col sxml 1 8 #f)))

      (set-option! balance-sheet-options "Display" "Omit zero balance figures" #f)
      (set-option! balance-sheet-options "Display" "Include accounts with zero total balances" #t)
      (let ((sxml (options->sxml balance-sheet-uuid balance-sheet-options "balsheet-incl-zb-accts=#t omit-zb-bals=#f")))
        (test-equal "omit-zb-bals=#f"
          (list "$0.00")
          (sxml->table-row-col sxml 1 3 5))
        (test-equal "incl-zb-accts=#t"
          '("Empty" "$0.00")
          (sxml->table-row-col sxml 1 8 #f)))
      )

    (display "\n\n pnl tests\n\n")
    (let* ((pnl-options (default-pnl-testing-options))
           (sxml (options->sxml pnl-uuid pnl-options "pnl-default")))
      (test-equal "total revenue  = $1,270.00"
        (list "$1,270.00" "$1,270.00")
        (sxml->table-row-col sxml 1 5 6))
      (test-equal "total expenses  = $0.00"
        (list "$0.00" "$0.00")
        (sxml->table-row-col sxml 1 3 6))

      (set-option! pnl-options "Commodities" "Price Source" 'weighted-average)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-weighted-average")))
        (test-equal "weighted average revenue = $1160.36"
          (list "$1,160.36" "$1,160.36")
          (sxml->table-row-col sxml 1 5 6)))

      (set-option! pnl-options "Commodities" "Price Source" 'average-cost)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-average-cost")))
        (test-equal "average-cost revenue = $976"
          (list "$976.00" "$976.00")
          (sxml->table-row-col sxml 1 5 6)))

      (set-option! pnl-options "Commodities" "Price Source" 'pricedb-nearest)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-pricedb-nearest")))
        (test-equal "pricedb-nearest revenue = $1270"
          (list "$1,270.00" "$1,270.00")
          (sxml->table-row-col sxml 1 5 6)))

      (set-option! pnl-options "Commodities" "Price Source" 'pricedb-latest)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-pricedb-latest")))
        (test-equal "pricedb-latest revenue = $1270"
          (list "$1,270.00" "$1,270.00")
          (sxml->table-row-col sxml 1 5 6)))

      ;; set multilevel subtotal style
      ;; verifies amount in EVERY line of the report.
      (set-option! pnl-options "Display" "Parent account balances" 'immediate-bal)
      (set-option! pnl-options "Display" "Parent account subtotals" 't)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-multilevel")))
        (test-equal "multilevel. income = -$250.00"
          (list "-$250.00" "-$250.00")
          (sxml->table-row-col sxml 1 3 5))
        (test-equal "multilevel. income-GBP = $0.00"
          (list "-#600.00" "-$1,020.00" "-#600.00" "-$1,020.00")
          (sxml->table-row-col sxml 1 4 5))
        (test-equal "multilevel. total income = -$1,270.00"
          (list "-$1,270.00" "-$1,270.00")
          (sxml->table-row-col sxml 1 5 6))
        (test-equal "multilevel. total revenue = $1,270.00"
          (list "$1,270.00" "$1,270.00")
          (sxml->table-row-col sxml 1 6 6))
        (test-equal "multilevel. expenses = $0.00"
          (list "$0.00" "$0.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "multilevel. net-income = $1,270"
          (list "$1,270.00" "$1,270.00")
          (sxml->table-row-col sxml 1 4 6)))

      ;; set recursive-subtotal subtotal style
      (set-option! pnl-options "Display" "Parent account balances" 'recursive-bal)
      (set-option! pnl-options "Display" "Parent account subtotals" 'f)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-recursive")))
        (test-equal "recursive. income = $1020+250"
          (list "-#600.00" "-$1,020.00" "-$250.00" "-$250.00" "$0.00" "-#600.00" "-$1,020.00" "-$250.00" "-$250.00" "$0.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "recursive. income-gbp = $1020"
          (list "-#600.00" "-$1,020.00" "-#600.00" "-$1,020.00")
          (sxml->table-row-col sxml 1 4 5))
        (test-equal "recursive. total revenue = $1270"
          (list "$1,270.00" "$1,270.00")
          (sxml->table-row-col sxml 1 5 6)))

      (set-option! pnl-options "Commodities" "Show Foreign Currencies" #f)
      (set-option! pnl-options "Commodities" "Show Exchange Rates" #f)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-disable show-fcur show-rates")))
        (test-equal "show-fcur disabled"
          (list "-$1,270.00" "$0.00" "-$1,270.00" "$0.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "show-rates disabled"
          '()
          (sxml->table-row-col sxml 2 #f #f)))

      (set-option! pnl-options "Commodities" "Show Foreign Currencies" #t)
      (set-option! pnl-options "Commodities" "Show Exchange Rates" #t)
      (let ((sxml (options->sxml pnl-uuid pnl-options "pnl-enable show-fcur show-rates")))
        (test-equal "show-fcur enabled"
          (list "-#600.00" "-$1,020.00" "-$250.00" "-$250.00" "$0.00" "-#600.00" "-$1,020.00" "-$250.00" "-$250.00" "$0.00")
          (sxml->table-row-col sxml 1 3 6))
        (test-equal "show-rates enabled"
          (list "#1.00" "$1.70")
          (sxml->table-row-col sxml 2 #f #f)))

      ;;make-multilevel
      (set-option! pnl-options "Display" "Parent account balances" 'immediate-bal)
      (set-option! pnl-options "Display" "Parent account subtotals" 't)
      )))
