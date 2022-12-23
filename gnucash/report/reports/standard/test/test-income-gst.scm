(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard income-gst-statement))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))


;; This is implementation testing for Income & GST report. This
;; delegates to the Transaction Report, therefore, only the
;; GSTR-specific options will be individually tested. Foreign-currency
;; conversions will NOT be tested, because they require pricedb entries.

;; see transaction.scm for explanatory notes and hints.

;; copied from income-gst-statement.scm
(define rpt-uuid "5bf27f249a0d11e7abc4cec278b6b50a")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "income-gst-statement.scm")
  (null-test)
  (test-group-with-cleanup "default GST report"
    (gstr-tests)
    (teardown))
  (test-group-with-cleanup "UK-VAT report"
    (uk-vat-tests)
    (teardown))
  (test-end "income-gst-statement.scm"))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml rpt-uuid options "test-gstr" test-title))

(define (set-option! options section name value)
  (if (gnc-lookup-option options section name)
      (gnc-set-option options section name value)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))

(define* (create-txn d m y desc splits #:optional txn-type)
  (let* ((splits (map (lambda (s) (vector (cdr s) (car s) (car s))) splits))
         (txn (env-create-multisplit-transaction #f d m y splits #:description desc)))
    (when txn-type (xaccTransSetTxnType txn txn-type))
    txn))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options rpt-uuid)))
    (test-assert "null-test" (options->sxml options "null-test"))))

(define (gstr-tests)
  (define structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
          (list "GST"
                (list "GST on Purchases")
                (list "GST on Sales" (list (cons 'type ACCT-TYPE-LIABILITY)))
                (list "Reduced GST on Sales" (list (cons 'type ACCT-TYPE-LIABILITY))))
          (list "Asset"
                (list "Bank")
                (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE))))
          (list "Liability" (list (cons 'type ACCT-TYPE-PAYABLE))
                (list "CreditCard")
                (list "A/Payable"))
          (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
          (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))))
  ;; This function will perform implementation testing on the GST report.
  (let* ((env (create-test-env))
         (book (gnc-get-current-book))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (creditcard (cdr (assoc "CreditCard" account-alist)))
         (payable (cdr (assoc "A/Payable" account-alist)))
         (receivable (cdr (assoc "A/Receivable" account-alist)))
         (gst-sales (cdr (assoc "GST on Sales" account-alist)))
         (reduced-gst-sales (cdr (assoc "Reduced GST on Sales" account-alist)))
         (gst-purch (cdr (assoc "GST on Purchases" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (default-testing-options)
      (let ((options (gnc:make-report-options rpt-uuid)))
        (set-option! options "Accounts" "Sales" (list income))
        (set-option! options "Accounts" "Purchases" (list expense))
        (set-option! options "Accounts" "Tax Accounts"
                     (list gst-sales reduced-gst-sales gst-purch))
        (set-option! options "General" "Add options summary" 'always)
        (set-option! options "General" "Table for Exporting" #t)
        (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
        (set-option! options "Display" "Account Name" #t)
        (set-option! options "Display" "Other Account Name" #f)
        (set-option! options "Display" "Amount" 'single)
        options))

    ;; This will make all accounts use default currency (I think depends on locale)
    (for-each
     (lambda(pair)
       (xaccAccountSetCommodity (cdr pair) (gnc-default-report-currency)))
     account-alist)

    (create-txn 1 1 YEAR "invoice charge $100, no GST"
                (list (cons -100 income)
                      (cons  100 receivable))
                TXN-TYPE-INVOICE)

    (create-txn 2 1 YEAR "invoice charge $200+$20GST"
                (list (cons -200 income)
                      (cons  -20 gst-sales)
                      (cons  220 receivable))
                TXN-TYPE-INVOICE)

    (create-txn 3 1 YEAR "receive $320 for invoices from bank"
                (list (cons -320 receivable)
                      (cons  320 bank))
                TXN-TYPE-PAYMENT)

    (create-txn 4 1 YEAR "cash sales $300+$15GST5%"
                (list (cons -300 income)
                      (cons  -15 reduced-gst-sales)
                      (cons  315 bank)))

    (create-txn 5 1 YEAR "cash spend $50, no GST"
                (list (cons -50 bank)
                      (cons  50 expense)))

    (create-txn 6 1 YEAR "purchase on credit $80+$8GST"
                (list (cons -88 payable)
                      (cons  80 expense)
                      (cons   8 gst-purch))
                TXN-TYPE-INVOICE)

    (create-txn 7 1 YEAR "hybrid paycheck. earn $400+$20, less $110+$10"
                (list (cons  310 bank)
                      (cons -400 income)
                      (cons  -20 reduced-gst-sales)
                      (cons  100 expense)
                      (cons   10 gst-purch)))

    (create-txn 8 1 YEAR "pay bill from 6-january for $88 using creditcard"
                (list (cons  88 payable)
                      (cons -88 creditcard))
                TXN-TYPE-PAYMENT)

    (create-txn 2 2 YEAR "link"
                (list (cons -77 income)
                      (cons  77 income))
                TXN-TYPE-LINK)

    (create-txn 3 2 YEAR "payment"
                (list (cons -22 income)
                      (cons  22 income))
                TXN-TYPE-PAYMENT)

    (xaccTransSetIsClosingTxn
     (create-txn 3 2 YEAR "closing"
                 (list (cons -33 income)
                       (cons  33 income)))
     #t)

    ;; Finally we can begin testing
    (test-begin "display options")

    (let ((options (default-testing-options)))
      (set-option! options "Display" "Num" #f)
      (set-option! options "Display" "Memo" #f)
      (set-option! options "Display" "Account Name" #f)
      (set-option! options "Sorting" "Primary Subtotal" 'date)
      (set-option! options "Sorting" "Secondary Subtotal" 'account-name)
      (let ((sxml (options->sxml options "initial setup")))
        (test-equal "totals are as expected"
          '("Grand Total" "$1,055.00" "$1,000.00" "$55.00" "$248.00" "$230.00" "$18.00")
          (sxml->table-row-col sxml 1 -1 #f))

        (test-equal "tax on sales as expected"
          '("$20.00" "$15.00" "$20.00" "$55.00")
          (sxml->table-row-col sxml 1 #f 5))

        (test-equal "tax on purchases as expected"
          '("$8.00" "$10.00" "$18.00")
          (sxml->table-row-col sxml 1 #f 8)))

      (set-option! options "Format" "Individual tax columns" #t)
      (set-option! options "Format" "Individual purchases columns" #t)
      (set-option! options "Format" "Individual sales columns" #t)
      (set-option! options "Format" "Gross Balance" #t)
      (set-option! options "Format" "Net Balance" #t)
      (set-option! options "Format" "Tax payable" #t)
      (let ((sxml (options->sxml options "display options enabled")))
        (test-equal "all display columns enabled"
          '("Grand Total" "$1,055.00" "$1,000.00" "$20.00" "$35.00" "$248.00" "$230.00" "$18.00" "$807.00" "$770.00" "$37.00")
          (sxml->table-row-col sxml 1 -1 #f))))

    (test-end "display options")))

(define (uk-vat-tests)
  (define structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
          (list "VAT"
                (list "Input"
                      (list "Purchases VAT"))
                (list "Output" (list (cons 'type ACCT-TYPE-LIABILITY))
                      (list "EU Purchases VAT")
                      (list "Sales VAT")))
          (list "Asset"
                (list "Bank")
                (list "Capital Assets"))
          (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
                (list "Sales non-EU")
                (list "Sales EU Goods")
                (list "Sales EU Services"))
          (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE))
                (list "Professional Fees")
                (list "EU Reverse VAT Expenses"))))
  ;; This function will perform implementation testing on the VAT report.
  (let* ((env (create-test-env))
         (book (gnc-get-current-book))
         (account-alist (env-create-account-structure-alist env structure))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (get-acct a)
      (or (assoc-ref account-alist a) (error "invalid account:" a)))
    (define (default-testing-options)
      (let ((options (gnc:make-report-options rpt-uuid)))
        (set-option! options "Accounts" "Sales"
                     (gnc-accounts-and-all-descendants
                      (list (get-acct "Income"))))
        (set-option! options "Accounts" "Purchases"
                     (gnc-accounts-and-all-descendants
                      (list (get-acct "Expenses"))))
        (set-option! options "Accounts" "Tax Accounts"
                     (list (get-acct "Purchases VAT")
                           (get-acct "EU Purchases VAT")
                           (get-acct "Sales VAT")))
        (set-option! options "General" "Add options summary" 'always)
        (set-option! options "General" "Table for Exporting" #t)
        (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
        options))

    (xaccAccountSetDescription (get-acct "Sales EU Goods") "*EUGOODS*")
    (xaccAccountSetDescription (get-acct "EU Reverse VAT Expenses") "*EUGOODS*")
    (xaccAccountSetDescription (get-acct "EU Purchases VAT") "*EUVAT*")

    (create-txn 01 01 YEAR "$1000 sales + $200 VAT"
                (list
                 (cons  1200 (get-acct "Bank"))
                 (cons  -200 (get-acct "Sales VAT"))
                 (cons -1000 (get-acct "Income"))))

    (create-txn 02 01 YEAR "$100 sales + $20 VAT"
                (list
                 (cons   120 (get-acct "Bank"))
                 (cons   -20 (get-acct "Sales VAT"))
                 (cons  -100 (get-acct "Income"))))

    (create-txn 03 01 YEAR "refund for $50 sales + $10 VAT"
                (list
                 (cons  -120 (get-acct "Bank"))
                 (cons    20 (get-acct "Sales VAT"))
                 (cons   100 (get-acct "Income"))))

    (create-txn 04 01 YEAR "reduced VAT sales $200 + $20 VAT"
                (list
                 (cons   220 (get-acct "Bank"))
                 (cons   -20 (get-acct "Sales VAT"))
                 (cons  -200 (get-acct "Income"))))

    (create-txn 05 01 YEAR "Sale of Goods to EU $100"
                (list
                 (cons  100 (get-acct "Bank"))
                 (cons -100 (get-acct "Sales EU Goods"))))

    (create-txn 07 01 YEAR "UK Accountant Services"
                (list
                 (cons  -54 (get-acct "Bank"))
                 (cons   45 (get-acct "Professional Fees"))
                 (cons    9 (get-acct "Purchases VAT"))))

    (create-txn 08 01 YEAR "VAT-free sales, $0 vat-sales"
                (list
                 (cons   50 (get-acct "Bank"))
                 (cons    0 (get-acct "Sales VAT"))
                 (cons  -50 (get-acct "Sales non-EU"))))

    (create-txn 09 01 YEAR "Widget Inserter bought from EU"
                (list
                 (cons -150 (get-acct "Bank"))
                 (cons  -30 (get-acct "EU Purchases VAT"))
                 (cons  150 (get-acct "EU Reverse VAT Expenses"))
                 (cons   30 (get-acct "Purchases VAT"))))

    (create-txn 10 01 YEAR "Services to EU customer"
                (list
                 (cons  125 (get-acct "Bank"))
                 (cons -125 (get-acct "Sales EU Services"))))

    (create-txn 11 01 YEAR "Consumables from EU Supplier"
                (list
                 (cons  -50 (get-acct "Bank"))
                 (cons   50 (get-acct "EU Reverse VAT Expenses"))
                 (cons   10 (get-acct "Purchases VAT"))
                 (cons  -10 (get-acct "EU Purchases VAT"))))

    (create-txn 12 01 YEAR "Laptop bought in UK"
                (list
                 (cons -360 (get-acct "Bank"))
                 (cons  300 (get-acct "Expenses"))
                 (cons   60 (get-acct "Purchases VAT"))))

    (let ((options (default-testing-options)))
      (set-option! options "Format" "Report Format" 'default)
      (let ((sxml (options->sxml options "ukvat-default-format")))
        (test-equal "ukvat-default-format"
          '("Grand Total" "$1,735.00" "$1,475.00" "$260.00"
            "$654.00" "$545.00" "$109.00")
          (sxml->table-row-col sxml 1 -1 #f)))

      (set-option! options "Format" "Report Format" 'uk-vat)
      (let ((sxml (options->sxml options "ukvat-return-format")))
        (test-equal "ukvat-return-format"
          '("Grand Total" "$220.00" "$40.00" "$260.00" "$109.00"
            "$151.00" "$1,475.00" "$545.00" "$100.00" "$200.00")
          (sxml->table-row-col sxml 1 -1 #f)))

      (set-option! options "Format" "Report Format" 'au-bas)
      (let ((sxml (options->sxml options "aubas-return-format")))
        (test-equal "aubas-return-format"
          '("Grand Total" "$1,735.00" "$260.00" "$109.00")
          (sxml->table-row-col sxml 1 -1 #f))))))
