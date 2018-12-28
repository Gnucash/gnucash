(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports income-gst-statement))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
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
  (gstr-tests)
  (test-end "income-gst-statement.scm"))

(define (options->sxml options test-title)
  (gnc:options->sxml rpt-uuid options "test-gstr" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

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
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        ))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options rpt-uuid)))
    (test-assert "null-test" (options->sxml options "null-test"))))

(define (gstr-tests)
  ;; This function will perform implementation testing on the transaction report.
  (let* ((env (create-test-env))
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
      ;; To ease testing of transaction report, we will set default
      ;; options for generating reports. We will elable extra columns
      ;; for Exporting, disable generation of informational text, and
      ;; disable indenting. These options will be tested separately as
      ;; the first test group. By default, we'll select the modern dates.
      (let ((options (gnc:make-report-options rpt-uuid)))
        (set-option! options "Accounts" "Accounts" (list income expense payable receivable))
        (set-option! options "Accounts" "Tax Accounts" (list gst-sales
                                                             reduced-gst-sales
                                                             gst-purch))
        (set-option! options "General" "Add options summary" 'always)
        (set-option! options "General" "Table for Exporting" #t)
        (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
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
          '("$20.00" "$20.00" "$20.00" "$20.00" "$15.00" "$15.00" "$55.00")
          (sxml->table-row-col sxml 1 #f 6))

        (test-equal "tax on purchases as expected"
          '("$8.00" "$10.00" "$18.00" "$18.00")
          (sxml->table-row-col sxml 1 #f 9)))

      (set-option! options "Display" "Individual tax columns" #t)
      (set-option! options "Display" "Individual purchases columns" #t)
      (set-option! options "Display" "Individual sales columns" #t)
      (set-option! options "Display" "Gross Balance" #t)
      (set-option! options "Display" "Net Balance" #t)
      (set-option! options "Display" "Tax payable" #t)
      (let ((sxml (options->sxml options "display options enabled")))
        (test-equal "all display columns enabled"
          '("Grand Total" "$1,055.00" "$1,000.00" "$20.00" "$35.00" "$248.00" "$230.00" "$18.00" "$807.00" "$770.00" "$37.00")
          (sxml->table-row-col sxml 1 -1 #f))))

    (test-end "display options")))
