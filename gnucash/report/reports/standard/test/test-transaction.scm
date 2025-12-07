(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard transaction))
(use-modules (gnucash reports standard reconcile-report))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; Guide to the test-transaction.scm

;; This test-transaction will implement regression testing for most
;; features from transaction.scm as of March 2018. It requires SRFI-64
;; (present in guile-2.0.10 or later), SXML, and VM.  SRFI-64 and SXML
;; are mandatory and has tremendously eased creation of tests. The VM
;; modules are only required to perform coverage analysis of this test
;; suite.
;;
;; By default the (run-test) entry point will run (run-test-proper)
;; which sets the SRFI-64 test runner, and initiates the proper test suite
;; in (null-test) and (trep-tests). Please note the tests will all call
;; (options->sxml) which in turn generates the transaction report, and
;; dumps the output at /tmp/test-trep-*.html for review.

;; copied from transaction.scm
(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")
(define reconcile-uuid "e45218c6d76f11e7b5ef0800277ef320")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "transaction.scm")
  (null-test)
  (trep-tests)
  (csv-tests)
  (reconcile-tests)
  ;; (test-end) must be run as the last function, it will
  ;; return #f if any of the tests have failed.
  (test-end "transaction.scm"))

;;
;; CANDIDATES FOR INCLUSION IN TEST-EXTRAS.SCM
;;
(define (logger . items)
  (define (strify item)
    (format #f "~s" item))
  (format #t "LOGGER: ~a\n" (string-join (map strify items) " "))
  items)

(define (str->num str)
  (string->number
   (string-filter
    (lambda (c) (or (char-numeric? c)
                    (memv c '(#\- #\.))))
    str)))

(define (options->sxml options test-title)
  ;; options object -> sxml tree
  ;;
  ;; This function abstracts the whole transaction report renderer.
  ;; It also catches XML parsing errors, dumping the options changed.
  ;;
  ;; It also dumps the render into /tmp/test-trep-XX.html where XX is the test title
  (qof-date-format-set QOF-DATE-FORMAT-ISO)
  (gnc:options->sxml trep-uuid options "test-trep" test-title))

(define (get-row-col sxml row col)
  (sxml->table-row-col sxml 1 row col))

(define (set-option! options section name value)
  (if (gnc-lookup-option options section name)
      (gnc-set-option options section name value)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))
(define (opt-val options section name)
  (if (gnc-lookup-option options section name)
      (gnc-optiondb-lookup-value options section name)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank")
              (list "GBP Bank")
              (list "USD Bank")
              (list "Wallet"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Income-GBP" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Liabilities" (list (cons 'type ACCT-TYPE-LIABILITY)))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))
        ))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options trep-uuid)))
    (test-assert "null-test" (options->sxml options "null-test"))))

(define (trep-tests)
  ;; This function will perform implementation testing on the transaction report.
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (gbp-bank (cdr (assoc "GBP Bank" account-alist)))
         (usd-bank (cdr (assoc "USD Bank" account-alist)))
         (wallet (cdr (assoc "Wallet" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (gbp-income (cdr (assoc "Income-GBP" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (liability (cdr (assoc "Liabilities" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today)))
         (foreign1 (gnc-commodity-table-lookup
                    (gnc-commodity-table-get-table (gnc-account-get-book bank))
                    (gnc-commodity-get-namespace (xaccAccountGetCommodity bank))
                    "USD"))
         (foreign2 (gnc-commodity-table-lookup
                    (gnc-commodity-table-get-table (gnc-account-get-book bank))
                    (gnc-commodity-get-namespace (xaccAccountGetCommodity bank))
                    "GBP")))

    (define (default-testing-options)
      ;; To ease testing of transaction report, we will set default
      ;; options for generating reports. We will elable extra columns
      ;; for Exporting, disable generation of informational text, and
      ;; disable indenting. These options will be tested separately as
      ;; the first test group. By default, we'll select the modern dates.
      (let ((options (gnc:make-report-options trep-uuid)))
        (set-option! options "Accounts" "Accounts" (list bank))
        (set-option! options "Sorting" "Add indenting columns" #f)
        (set-option! options "General" "Add options summary" 'always)
        (set-option! options "General" "Table for Exporting" #t)
        (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
        (set-option! options "Display" "Account Name" #t)
        (set-option! options "Display" "Other Account Name" #f)
        (set-option! options "Display" "Amount" 'single)
        options))

    ;; to test Sorting/Show Account Description
    (xaccAccountSetDescription income "Salaries etc")
    (xaccAccountSetCode expense "EXP-001")

    ;; This will make all accounts use default currency (I think depends on locale)
    (for-each
     (lambda(pair)
       (xaccAccountSetCommodity (cdr pair) (gnc-default-report-currency)))
     account-alist)

    ;; Here we set foreign currencies

    (gnc-commodity-set-user-symbol foreign2 "#")

    (with-account
     gbp-bank
     (lambda ()
       (xaccAccountSetCode gbp-bank "01-GBP")
       (xaccAccountSetCommodity gbp-bank foreign2)))

    (with-account
     gbp-income
     (lambda ()
       (xaccAccountSetCode gbp-income "01-GBP")
       (xaccAccountSetCommodity gbp-income foreign2)))

    (with-account
     usd-bank
     (lambda ()
       (xaccAccountSetCode usd-bank "02-USD")
       (xaccAccountSetCommodity usd-bank foreign1)))

    ;; We will fill the transaction report with crafted dummy
    ;; transactions for producing repeatable reports.

    ;; old transactions for testing absolute dates, sorting, and
    ;; filtering options.
    (env-transfer env 01 01 1970 bank expense       5   #:description "desc-1" #:num "trn1" #:memo "memo-3")
    (env-transfer env 31 12 1969 income bank       10   #:description "desc-2" #:num "trn2" #:void-reason "void" #:notes "notes3")
    (env-transfer env 31 12 1969 income bank       29   #:description "desc-3" #:num "trn3"
                  #:reconcile (cons #\c (gnc-dmy2time64 01 03 1970)))
    (env-transfer env 01 02 1970 bank expense      15   #:description "desc-4" #:num "trn4" #:notes "notes2" #:memo "memo-1")
    (env-transfer env 10 01 1970 liability expense 10   #:description "desc-5" #:num "trn5" #:void-reason "any")
    (env-transfer env 10 01 1970 liability expense 11   #:description "desc-6" #:num "trn6" #:notes "notes1")
    (env-transfer env 10 02 1970 bank liability     8   #:description "desc-7" #:num "trn7" #:notes "notes1" #:memo "memo-2"
                  #:reconcile (cons #\y (gnc-dmy2time64 01 03 1970)))

    ;; a single 3-split transaction dated 14/02/1971
    ;; $100 from bank
    ;;  $80 to expenses
    ;;  $20 to wallet
    (env-create-multisplit-transaction
     env 14 02 1971
     (list (vector bank  -100 -100)
           (vector expense 80   80)
           (vector wallet  20   20))
     #:description "$100bank -> $80expenses + $20wallet"
     #:notes "multisplit")

    ;; A single closing transaction
    (let ((closing-txn (env-transfer env 31 12 1999 expense equity 111 #:description "Closing")))
      (xaccTransSetIsClosingTxn closing-txn #t))

    ;; A couple of transactions which involve foreign currency
    ;; conversions. We'll set the currencies to GBP and USD.
    (env-transfer-foreign env 15 01 2000 gbp-bank usd-bank 10 14 #:description "GBP 10 to USD 14")
    (env-transfer-foreign env 15 02 2000 usd-bank gbp-bank  9  6 #:description "USD 9 to GBP 6")

    ;; new transactions for testing relative dates. every month in the
    ;; year will have 2 income transactions for $103 and $109 dated on
    ;; the 3rd and 9th respectively, and 1 expense transaction for $22
    ;; on the 15th. We will assume that the test suite will always be
    ;; run in modern times, otherwise these transactions will be mixed
    ;; up with the old transactions above. The year end net bank balance
    ;; should be (* 12 (+ 103 109 -22)) = $2280.
    ;; there will also be a #51 income monthly, tested at end of file
    (for-each (lambda (m)
                (env-transfer env 08 (1+ m) YEAR gbp-income gbp-bank 51 #:description "#51 income")
                (env-transfer env 03 (1+ m) YEAR income bank  103 #:description "$103 income")
                (env-transfer env 15 (1+ m) YEAR bank expense  22 #:description "$22 expense")
                (env-transfer env 09 (1+ m) YEAR income bank  109 #:description "$109 income"))
              (iota 12))

    ;; (for-each (lambda (s)
    ;;             (format #t "~a '~a' ~a ~a from/to ~a\n"
    ;;                     (qof-print-date (xaccTransGetDate (xaccSplitGetParent s)))
    ;;                     (xaccTransGetDescription (xaccSplitGetParent s))
    ;;                     (gnc-commodity-get-mnemonic (xaccAccountGetCommodity (xaccSplitGetAccount s)))
    ;;                     (xaccSplitGetAmount s)
    ;;                     (xaccAccountGetName (xaccSplitGetAccount (xaccSplitGetOtherSplit s)))
    ;;                     ))
    ;;           (xaccAccountGetSplits bank))

    ;; Finally we can begin testing
    (test-begin "general options")

    (let* ((options (default-testing-options))
           (sxml (options->sxml options "general options"))
           (default-headers '("Date" "Num" "Description" "Memo/Notes" "Account" "Amount")))
      (test-equal "default headers"
        default-headers
        (get-row-col sxml 0 #f))
      (test-equal "grand total present"
        '("Grand Total")
        (get-row-col sxml -1 1))
      (test-equal "grand total is $2,280.00"
        '("$2,280.00")
        (get-row-col sxml -1 -1)))

    ;; dual columns
    (let ((options (default-testing-options)))
      (set-option! options "Sorting" "Add indenting columns" #t)
      (set-option! options "General" "Table for Exporting" #f)
      (set-option! options "Currency" "Common Currency" #t)
      (set-option! options "Currency" "Show original currency amount" #t)
      (set-option! options "General" "Add options summary" 'never)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let ((sxml (options->sxml options "test basic column headers, and original currency")))
        (test-equal "default headers, indented, includes common-currency"
          '("Date" "Num" "Description" "Memo/Notes" "Account" "Amount (USD)" "Amount")
          (get-row-col sxml 0 #f))
        (test-equal "grand total present, no blank cells, and is $2,280 in both common-currency and original-currency"
          '("Grand Total" "$2,280.00" "$2,280.00")
          (get-row-col sxml -1 #f))
        (test-equal "account total present, and is $2,280 in original-currency"
          '("$2,280.00")
          (get-row-col sxml -3 -1))
        (test-equal "month total present, and is $190 in original-currency"
          '("$190.00")
          (get-row-col sxml 6 -1))
        )

      ;; dual columns with all running totals
      (set-option! options "Display" "Running Totals" 'all)
      (let ((sxml (options->sxml options "test column headers with original currency and running totals")))
        (test-equal "default headers with all running totals, indented, includes common-currency"
          '("Date" "Num" "Description" "Memo/Notes" "Account" "Amount (USD)"
            "Running Secondary Subtotal (USD)" "Running Primary Subtotal (USD)" "Running Grand Total (USD)"
            "Amount" "Running Secondary Subtotal" "Running Primary Subtotal" "Running Grand Total")
          (get-row-col sxml 0 #f))))

    (test-end "general options")

    (test-begin "accounts selectors and filtering")

    (let ((options (default-testing-options)))
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))

      ;; Filter Account Name Filters
      (set-option! options "Filter" "Account Name Filter" "Expenses")
      (let ((sxml (options->sxml options "accounts filter expenses")))
        (test-equal "account name filter to 'expenses', sum = $31.00"
          '("$31.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Account Name Filter" "Expen.es")
      (let ((sxml (options->sxml options "accounts filter expen.es")))
        (test-equal "account name filter to 'expen.es', blank report"
          '()
          (get-row-col sxml #f #f)))

      (set-option! options "Filter" "Use regular expressions for account name filter" #t)
      (let ((sxml (options->sxml options "accounts filter expen.es regex")))
        (test-equal "account name filter to 'expen.es' and switch on regex filter, sum = $31.00"
          '("$31.00")
          (get-row-col sxml -1 -1)))

      ;; Filter Account Name Filters
      (set-option! options "Filter" "Account Name Filter excludes matched strings"
                   #t)
      (let ((sxml (options->sxml options "accounts filter exclude expen.es regex")))
        (test-equal "account name filter to 'expen.es, regex, negated', sum = -$31.00"
          '("-$31.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Use regular expressions for account name filter"
                   #f)
      (let ((sxml (options->sxml options "accounts filter exclude expen.es")))
        (test-equal "account name filter to 'expen.es, negated', sum = $0.00"
          '("$0.00")
          (get-row-col sxml -1 -1)))

      ;; Test Transaction Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (set-option! options "Filter" "Transaction Filter" "desc-3")
      (let ((sxml (options->sxml options "transaction filter to ponies")))
        (test-equal "transaction filter in bank to 'desc-3', sum = $29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Transaction Filter" "not.s?")
      (let ((sxml (options->sxml options "transaction filter not.s?")))
        (test-equal "transaction filter in bank to 'not.s?', blank report"
          '()
          (get-row-col sxml #f #f)))

      (set-option! options "Filter" "Use regular expressions for transaction filter" #t)
      (let ((sxml (options->sxml options "transaction filter not.s? regex")))
        (test-equal "transaction filter in bank to 'not.s?' and switch regex, sum = -$23.00"
          '("-$23.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Transaction Filter excludes matched strings" #t)
      (let ((sxml (options->sxml options "negate transaction filter not.s?")))
        (test-equal "transaction filter in bank to 'not.s?' and switch regex, sum = -$23.00"
          '("$24.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Transaction Filter excludes matched strings" #f)
      (set-option! options "Filter" "Use regular expressions for transaction filter" #f)
      (set-option! options "Filter" "Transaction Filter is case insensitive" #t)
      (set-option! options "Filter" "Transaction Filter" "dEsC-3")
      (let ((sxml (options->sxml options "transaction filter insensitive")))
        (test-equal "transaction filter case insensitive"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Use regular expressions for transaction filter" #t)
      (set-option! options "Filter" "Transaction Filter" "NoT.S?")
      (let ((sxml (options->sxml options "transaction filter regex case insensitive")))
        (test-equal "transaction filter regex case insensitive"
          '("-$23.00")
          (get-row-col sxml -1 -1)))

      ;; Test Reconcile Status Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Filter" "Reconciled Status" 'unreconciled)
      (let ((sxml (options->sxml options "unreconciled")))
        (test-equal "filter unreconciled only, sum = -$20.00"
          '("-$20.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Reconciled Status" 'cleared)
      (let ((sxml (options->sxml options "cleared")))
        (test-equal "filter cleared only, sum = $29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Reconciled Status" 'reconciled)
      (let ((sxml (options->sxml options "reconciled")))
        (test-equal "filter reconciled only, sum = -$8.00"
          '("-$8.00")
          (get-row-col sxml -1 -1)))

      ;; Test Accounts Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Accounts" "Filter By…" (list income))
      (set-option! options "Accounts" "Filter Type" 'include)
      (let ((sxml (options->sxml options "including bank-income accts only")))
        (test-equal "filter includes bank-income, sum = -$29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Accounts" "Filter Type" 'exclude)
      (let ((sxml (options->sxml options "bank exclude bank-income accts")))
        (test-equal "filter excludes bank-income, sum = -$28.00"
          '("-$28.00")
          (get-row-col sxml -1 -1)))

      ;; Test Void Transaction Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Filter" "Void Transactions" 'void-only)
      (let ((sxml (options->sxml options "void only")))
        (test-equal "filter void-transactions only, sum = -$10.00"
          '("$10.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Void Transactions" 'both)
      (let ((sxml (options->sxml options "both void and non-void")))
        (test-equal "filter void-transactions only, sum = $11.00"
          '("$11.00")
          (get-row-col sxml -1 -1)))

      ;; Test Closing-Txn Filters
      (set! options (default-testing-options))
      (set-option! options "Accounts" "Accounts" (list expense))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1911)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 2012)))      
      (set-option! options "Filter" "Closing transactions" 'exclude-closing)
      (let ((sxml (options->sxml options "filter closing - exclude closing txns ")))
        (test-equal "filter exclude closing. bal = $111"
          '("$111.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Closing transactions" 'closing-only)
      (let ((sxml (options->sxml options "filter closing - include closing only")))
        (test-equal "filter closing only. bal = -$111"
          '("-$111.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Closing transactions" 'include-both)
      (let ((sxml (options->sxml options "filter closing - include both")))
        (test-equal "filter include both. bal = $0"
          '("$0.00")
          (get-row-col sxml -1 -1)))
      )

    (test-end "accounts selectors and filtering")

    (test-begin "display options")

    (let ((options (default-testing-options)))
      ;; Disable most Display/* columns
      (for-each
       (lambda (name)
         (set-option! options "Display" name #f))
       (list "Date" "Reconciled Date" "Num" "Description" "Memo" "Notes"
             "Account Name" "Other Account Name" "Shares" "Price" "Account Balance"
             "Grand Total"))
      (let ((sxml (options->sxml options "all columns off")))
        (test-assert "all display columns off, except amount and subtotals are enabled, there should be 2 columns"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             2)))

      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'none)
      (let ((sxml (options->sxml options "only amounts")))
        (test-assert "all display columns off, and no subtotals, but amount enabled, there should be 1 column"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             1)))

      (set-option! options "Display" "Enable Links" #f)
      (let ((sxml (options->sxml options "disable hyperlinks")))
        (test-assert "no anchor when disabling hyperlinks"
          (zero? (length ((sxpath '(// a // *text*)) sxml)))))

      (set-option! options "Display" "Enable Links" #t)
      (let ((sxml (options->sxml options "enable hyperlinks")))
        (test-assert "anchors exist when enabling hyperlinks"
          (positive? (length ((sxpath '(// a // *text*)) sxml)))))

      (set-option! options "Display" "Amount" 'none)
      (let ((sxml (options->sxml options "no columns")))
        (test-assert "all display columns off, without amount nor subtotals, there should be 0 column"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             0)))

      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'weekly)
      (set-option! options "Sorting" "Secondary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'weekly)
      (let ((sxml (options->sxml options "subtotals only")))
        (test-assert "all display columns including amount are disabled, but subtotals are enabled, there should be 1 column"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             1)))

      ;; Reset to test with full columns
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (set-option! options "Sorting" "Primary Key" 'reconciled-status)
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (for-each
       (lambda (name)
         (set-option! options "Display" name #t))
       (list "Date" "Reconciled Date" "Num" "Description" "Memo" "Notes"
             "Account Name" "Other Account Name" "Shares" "Price" "Account Balance"
             "Grand Total" "Use Full Other Account Name" "Use Full Account Name"))
      (set-option! options "Display" "Running Totals" 'grand)
      (let* ((sxml (options->sxml options "all columns on")))
        (test-equal "all display columns on, displays correct columns"
          (list "Date" "Reconciled Date" "Num" "Description" "Memo/Notes" "Account"
                "Transfer from/to" "Shares" "Price" "Amount" "Account Balance" "Running Grand Total")
          (get-row-col sxml 0 #f))
        (test-assert "reconciled dates must be 01/03/70 or whitespace"
          (and-map
           (lambda (reconcile-date-string)
             (or (string=? (string-trim-both reconcile-date-string)
                           (qof-print-date (gnc-dmy2time64 01 03 1970)))
                 (string-null? (string-trim-both reconcile-date-string))))
           (get-row-col sxml #f 2)))
        (test-equal "reconciled status subtotal"
          (list "Total For Unreconciled" "$0.00")
          (get-row-col sxml -3 #f))
        )

      ;; Customized test for multi-line, with default display cols.
      ;; This option should return a single transaction with 2 splits
      ;; in 2 lines.
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 13 02 1971)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 15 02 1971)))
      (set-option! options "Display" "Detail Level" 'multi-line)
      (set-option! options "Display" "Grand Total" #f)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'none)
      (let* ((sxml (options->sxml options "multiline")))
        (test-assert "multi line transaction with 1st split have same memo"
          (apply string=? (get-row-col sxml #f 4)))

        ;; the following sxpath will retrieve all text from <a href> of the
        ;; last <td> in the table. This retrieves the amounts.
        (test-equal "multi-line amounts must total to zero"
          0.0
          (apply + (map str->num ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml)))))

      ;; Remove expense multisplit, transaction is not shown
      (set-option! options "Accounts" "Filter By…" (list expense))
      (set-option! options "Accounts" "Filter Type" 'exclude)
      (let* ((sxml (options->sxml options "multiline, filtered out")))
        (test-equal "multi-line has been excluded"
          '()
          (get-row-col sxml #f #f)))

      ;; Testing for original currency amount display as well as
      ;; dual-column subtotals
      (set! options (default-testing-options))
      (set-option! options "Accounts" "Accounts" (list usd-bank gbp-bank))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 2000)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 2000)))
      (set-option! options "Currency" "Common Currency" #t)
      (set-option! options "Currency" "Show original currency amount" #t)
      (let* ((sxml (options->sxml options "single column, with original currency headers")))
        (test-equal "single amount column, with original currency headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account"
                "Amount (USD)" "Amount")
          (get-row-col sxml 0 #f)))

      (set-option! options "Display" "Amount" 'double)
      (set-option! options "Display" "Account Name" #t)
      (set-option! options "Display" "Account Code" #t)
      (set-option! options "Display" "Other Account Name" #t)
      (set-option! options "Display" "Other Account Code" #t)
      (let* ((sxml (options->sxml options "dual column")))
        (test-equal "dual amount headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Transfer from/to"
                "Debit (USD)" "Credit (USD)" "Debit" "Credit")
          (get-row-col sxml 0 #f))
        (test-equal "Account Name and Code displayed"
          (list "01-GBP Root.Asset.GBP Bank")
          (get-row-col sxml 2 5))
        (test-equal "Other Account Name and Code displayed"
          (list "01-GBP GBP Bank")
          (get-row-col sxml 7 6))
        (test-equal "GBP original currency totals = #4"
          (list 4.0)
          (map str->num (get-row-col sxml 5 10)))
        (test-assert "USD original currency totals = $5 (tests pricedb)"
          (equal?
           (list 5.0)
           (map str->num (get-row-col sxml 4 8))
           (map str->num (get-row-col sxml 9 7))
           (map str->num (get-row-col sxml 9 9))))
        (test-equal "USD grand totals are correct (tests pricedb)"
          (list "Grand Total" "$0.00" "$5.00")
          (get-row-col sxml 11 #f)))

      ;; This test group will test sign reversal strategy. We will
      ;; display all transactions in the 1969-1970 series, sorted by
      ;; account name, then by description. This will ensure
      ;; consistent order and we can query each amount individually.
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "Display" "Sign Reverses" 'none)
      (set-option! options "Filter" "Void Transactions" 'both)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Secondary Key" 'description)
      (set-option! options "Sorting" "Secondary Subtotal" #f)
      (let* ((sxml (options->sxml options "sign-reversal is none, correct signs of amounts?")))
        (test-equal "sign-reversal is none, correct signs of amounts"
          '(#f #t #t #f #f #t #t #t #t #f #f #f #f #t)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      (set-option! options "Display" "Sign Reverses" 'income-expense)
      (let* ((sxml (options->sxml options "sign-reversal is income-expense, correct signs of amounts?")))
        (test-equal "sign-reversal is income-expense, correct signs of amounts"
          '(#f #t #t #f #f #f #f #f #f #t #t #f #f #t)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      (set-option! options "Display" "Sign Reverses" 'credit-accounts)
      (let* ((sxml (options->sxml options "sign-reversal is credit-accounts, correct signs of amounts?")))
        (test-equal "sign-reversal is credit-accounts, correct signs of amounts"
          '(#f #t #t #f #f #t #t #t #t #t #t #t #t #f)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      ;; test debit/credit dual columns
      (set! options (default-testing-options))
      (set-option! options "Display" "Amount" 'double)
      (set-option! options "Currency" "Common Currency" #t)
      (set-option! options "Currency" "Show original currency amount" #t)
      (set-option! options "Sorting" "Primary Key" 'date)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (let* ((sxml (options->sxml options "dual columns")))
        (test-equal "dual amount column, with original currency headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account"
                "Debit (USD)" "Credit (USD)" "Debit" "Credit")
          (get-row-col sxml 0 #f))
        (test-equal "dual amount column, grand totals available"
          (list "Grand Total" "$2,280.00" "$2,280.00")
          (get-row-col sxml -1 #f))
        (test-equal "dual amount column, first transaction correct"
          (list "$103 income" "Root.Asset.Bank" "$103.00" "$103.00")
          (cdr (get-row-col sxml 1 #f))))

      ;; test account balance displayed as running balance
      (set! options (default-testing-options))
      (set-option! options "Display" "Account Balance" #t)
      (let* ((sxml (options->sxml options "running balance, default options")))
        (test-equal "running balance, default sort"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Amount" "Running Balance")
          (get-row-col sxml 0 #f))
        (test-equal "running balance, showing Balance b/f"
          (list "Bank: Balance b/f" "-$99.00")
          (get-row-col sxml 1 #f)))

      (set-option! options "Sorting" "Primary Key" 'account-code)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (let* ((sxml (options->sxml options "running balance, primary sort by account code, secondary sort by date")))
        (test-equal "running balance, secondary sort by date"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Amount" "Running Balance")
          (get-row-col sxml 0 #f))
        (test-equal "running balance, showing Balance b/f"
          (list "Bank: Balance b/f" "-$99.00")
          (get-row-col sxml 1 #f)))

      ;; test running grand total
      (set! options (default-testing-options))
      (set-option! options "Display" "Running Totals" 'grand)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (let* ((sxml (options->sxml options "running grand total"))
             (cs (char-set-adjoin! (char-set-copy char-set:digit) #\-)))
        (test-equal "Last running grand total equals grand total"
          (get-row-col sxml -3 -1)
          (get-row-col sxml -1 -2))
        (test-equal "Row 9 running grand total matches row 8 running grand total + row 9 amount"
          (+ (string->number (string-filter cs (car (get-row-col sxml 8 -1))))
             (string->number (string-filter cs (car (get-row-col sxml 9 -2)))))
          (string->number (string-filter cs (car (get-row-col sxml 9 -1))))))
      )

    (test-end "display options")

    (test-begin "sorting options")

    (let ((options (default-testing-options)))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      ;;(set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "Accounts" "Accounts" (list bank))
      (set-option! options "Display" "Grand Total" #f)
      (set-option! options "Display" "Other Account Name" #t)
      (set-option! options "Filter" "Void Transactions" 'both)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)

      (set-option! options "Sorting" "Primary Key" 'date)
      (let* ((sxml (options->sxml options "sorting=date")))
        (test-equal "dates are sorted"
          '("1969-12-31" "1969-12-31" "1970-01-01" "1970-02-01" "1970-02-10")
          (get-row-col sxml #f 1)))

      (set-option! options "Sorting" "Primary Key" 'number)
      (let* ((sxml (options->sxml options "sorting=number")))
        (test-equal "sort by number"
          '("trn1" "trn2" "trn3" "trn4" "trn7")
          (get-row-col sxml #f 2)))

      (set-option! options "Sorting" "Primary Key" 'reconciled-status)
      (let* ((sxml (options->sxml options "sorting=reconciled-status")))
        (test-equal "sort by reconciled status"
          '("desc-2" "desc-7" "desc-3" "desc-1" "desc-4")
          (get-row-col sxml #f 3)))

      (set-option! options "Sorting" "Primary Key" 'memo)
      (let* ((sxml (options->sxml options "sorting=memo")))
        (test-equal "sort by memo"
          '("notes3" "memo-1" "memo-2" "memo-3")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Primary Key" 'account-name)
      (let* ((sxml (options->sxml options "sorting=account-name")))
        (test-assert "account names are sorted"
          (sorted? (get-row-col sxml #f 5) string<?)))

      (set-option! options "Sorting" "Primary Key" 'corresponding-acc-name)
      (let* ((sxml (options->sxml options "sorting=corresponding-acc-name")))
        (test-equal "sort by corresponding-acc-name"
          '("Expenses" "Expenses" "Income" "Income" "Liabilities")
          (get-row-col sxml #f 6)))

      (set-option! options "Sorting" "Primary Key" 'notes)
      (let* ((sxml (options->sxml options "sorting=trans-notes")))
        (test-equal "sort by transaction notes"
          '("memo-3" "memo-2" "memo-1" "notes3")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Primary Key" 'amount)
      (let* ((sxml (options->sxml options "sorting=amount")))
        (test-equal "sort by amount"
          '("-$15.00" "-$8.00" "-$5.00" "$10.00" "$29.00")
          ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml)))

      (set! options (default-testing-options))
      (set-option! options "Sorting" "Add indenting columns" #t)
      (set-option! options "Currency" "Show original currency amount" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (set-option! options "Display" "Grand Total" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'quarterly)
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (let* ((sxml (options->sxml options "sorting=account-name, date-quarterly, subtotals only")))
        (test-equal "sorting=account-name, date-quarterly, subtotals only"
          '("$570.00" "$570.00" "$570.00" "$570.00" "$2,280.00" "$2,280.00")
          (get-row-col sxml #f -1)))

      (set! options (default-testing-options))
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "Display" "Grand Total" #t)
      (set-option! options "Display" "Amount" 'double)
      (set-option! options "Currency" "Show original currency amount" #t)
      (set-option! options "General" "Table for Exporting" #f)
      (set-option! options "Sorting" "Add indenting columns" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'quarterly)
      (set-option! options "Sorting" "Show Informal Debit/Credit Headers" #t)
      (set-option! options "Sorting" "Show Account Description" #f)
      (set-option! options "Sorting" "Show Account Code" #f)
      (let* ((sxml (options->sxml options "sorting=date, friendly headers")))
        (test-equal "expense acc friendly headers"
          '("Expenses" "Expense" "Rebate")
          (get-row-col sxml 69 #f))
        (test-equal "income acc friendly headers"
          '("Income" "Charge" "Income")
          (get-row-col sxml 91 #f)))

      (set-option! options "Sorting" "Show Account Description" #t)
      (set-option! options "Sorting" "Show Account Code" #t)
      (let* ((sxml (options->sxml options "sorting=date, friendly headers with acct desc/code")))
        (test-equal "expense acc friendly headers"
          '("EXP-001 Expenses" "Expense" "Rebate")
          (get-row-col sxml 69 #f))
        (test-equal "income acc friendly headers"
          '("Income: Salaries etc" "Charge" "Income")
          (get-row-col sxml 91 #f)))

      (set-option! options "Accounts" "Accounts" (list bank))
      (set-option! options "Display" "Grand Total" #f)
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (let* ((sxml (options->sxml options "sorting=date quarterly")))
        (test-equal "quarterly subtotals are correct"
          '("$570.00" "$570.00" "$570.00" "$570.00")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let* ((sxml (options->sxml options "sorting=date monthly")))
        (test-equal "monthly subtotals are correct"
          '("$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'yearly)
      (let* ((sxml (options->sxml options "sorting=date yearly")))
        (test-equal "yearly subtotals are correct"
          '("$2,280.00")
          (get-row-col sxml #f 4)))

      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 31 12 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1972)))
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #f)
      (set-option! options "Filter" "Void Transactions" 'both)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'daily)
      (let* ((sxml (options->sxml options "sorting=date")))
        (test-equal "daily subtotals are correct"
          '("$39.00")
          (get-row-col sxml 5 4)))

      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'weekly)
      (let* ((sxml (options->sxml options "sorting=date weekly")))
        (test-equal "weekly subtotals are correct (1)"
          '("$34.00" "$89.00")
          (get-row-col sxml #f 4))
        (test-equal "weekly subtotals are correct (2)"
          '("$15.00" "$8.00" "$100.00")
          (get-row-col sxml #f 5)))

      ;; test running subtotals
      (set! options (default-testing-options))
      (set-option! options "Display" "Grand Total" #f)
      (set-option! options "Display" "Running Totals" 'sub)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let* ((sxml (options->sxml options "running subtotals"))
             (cs (char-set-adjoin! (char-set-copy char-set:digit) #\-)))
        (test-equal "Running subtotal columns are present"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Amount"
                "Running Secondary Subtotal" "Running Primary Subtotal")
          (get-row-col sxml 0 #f))
        (test-equal "Last running primary subtotal equals primary subtotal"
          (get-row-col sxml -3 -1)
          (get-row-col sxml -1 -3))
        (test-equal "Row 10 running primary subtotal matches row 9 running primary subtotal + row 10 amount"
          (+ (string->number (string-filter cs (car (get-row-col sxml 9 -1))))
             (string->number (string-filter cs (car (get-row-col sxml 10 -3)))))
          (string->number (string-filter cs (car (get-row-col sxml 10 -1)))))
        (test-equal "In first secondary group, last running secondary subtotal equals secondary subtotal"
          (get-row-col sxml 5 -2)
          (get-row-col sxml 6 -3))
        (test-equal "Row 10 running secondary subtotal matches row 9 running secondary subtotal + row 10 amount"
          (+ (string->number (string-filter cs (car (get-row-col sxml 9 -2))))
             (string->number (string-filter cs (car (get-row-col sxml 10 -3)))))
          (string->number (string-filter cs (car (get-row-col sxml 10 -2))))))
      ;; test all running totals shown
      (set-option! options "Display" "Running Totals" 'all)
      (let* ((sxml (options->sxml options "all running totals"))
             (cs (char-set-adjoin! (char-set-copy char-set:digit) #\-)))
        (test-equal "All running total columns are present"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Amount"
                "Running Secondary Subtotal" "Running Primary Subtotal" "Running Grand Total")
          (get-row-col sxml 0 #f)))
      ;; test no running totals shown
      (set-option! options "Display" "Running Totals" 'none)
      (let* ((sxml (options->sxml options "no running totals"))
             (cs (char-set-adjoin! (char-set-copy char-set:digit) #\-)))
        (test-equal "No running total columns are present"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Amount")
          (get-row-col sxml 0 #f)))

      ;; test that only columns with subtotals are displayed when
      ;; "Show subtotals only (hide transactional data)" is selected
      (set! options (default-testing-options))
      (for-each
      (lambda (name)
        (set-option! options "Display" name #t))
      (list "Date" "Reconciled Date" "Num" "Description" "Memo" "Notes"
            "Account Name" "Other Account Name" "Shares" "Price" "Account Balance"
            "Grand Total" "Use Full Other Account Name" "Use Full Account Name"))
      (set-option! options "Display" "Running Totals" 'all)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (set-option! options "Currency" "Common Currency" #t)
      (set-option! options "Currency" "Show original currency amount" #t)
      (let* ((sxml (options->sxml options "show subtotals only, single column")))
        (test-equal "all display columns on with single amount; show subtotals only, so only amount columns are shown"
          (list "Amount (USD)" "Amount")
          (get-row-col sxml 0 #f)))

      (set-option! options "Display" "Amount" 'double)
      (let* ((sxml (options->sxml options "show subtotals only, dual column")))
        (test-equal "all display columns on with dual amount; show subtotals only, so only debit/credit columns are shown"
          (list "Debit (USD)" "Credit (USD)" "Debit" "Credit")
          (get-row-col sxml 0 #f)))
    )

    (test-end "sorting options")

    (test-begin "subtotal table")

    (let ((options (default-testing-options)))
      (set-option! options "Accounts" "Accounts" (list bank gbp-bank gbp-income income expense))
      (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
      (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
      (set-option! options "Display" "Subtotal Table" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let ((sxml (options->sxml options "subtotal table")))
        (test-equal "summary bank-row is correct"
          (list "Bank" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00"
                "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$2,280.00" "$190.00")
          (get-row-col sxml 1 #f))
        (test-equal "summary gbp bank-row is correct"
          (list "GBP Bank" "#51.00" "#51.00" "#51.00" "#51.00" "#51.00" "#51.00"
                "#51.00" "#51.00" "#51.00" "#51.00" "#51.00" "#51.00" "#612.00" "#51.00")
          (get-row-col sxml 2 #f))
        (test-equal "summary expense-row is correct"
          (list "Expenses" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00"
                "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$264.00" "$22.00")
          (get-row-col sxml 3 #f))
        (test-equal "summary income-row is correct"
          (list "Income" "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00"
                "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00"
                "-$212.00" "-$2,544.00" "-$212.00")
          (get-row-col sxml 4 #f))
        (test-equal "summary gbp income-row is correct"
          (list "Income-GBP" "-#51.00" "-#51.00" "-#51.00" "-#51.00" "-#51.00" "-#51.00"
                "-#51.00" "-#51.00" "-#51.00" "-#51.00" "-#51.00" "-#51.00" "-#612.00" "-#51.00")
          (get-row-col sxml 5 #f))
        (test-equal "summary gbp total-row is correct"
          (list "Grand Total" "#0.00" "#0.00")
          (get-row-col sxml 6 #f))
        (test-equal "summary total-row is correct"
          (list "$0.00" "$0.00")
          (get-row-col sxml 7 #f)))

      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (let ((sxml (options->sxml options "sparse subtotal table")))
        (test-equal "sparse summary-table - row 1"
          (list "Bank" "$29.00" "-$5.00" "-$23.00" "$1.00" "$0.33")
          (get-row-col sxml 1 #f))
        (test-equal "sparse summary-table - row 2"
          (list "Expenses" "$16.00" "$15.00" "$31.00" "$10.33")
          (get-row-col sxml 2 #f))
        (test-equal "sparse summary-table - row 3"
          (list "Income" "-$29.00" "-$29.00" "-$9.67")
          (get-row-col sxml 3 #f))
        (test-equal "sparse summary-table - row 4"
          (list "Grand Total" "$3.00" "$1.00")
          (get-row-col sxml 4 #f))
        (test-equal "sparse summary-table - col 1"
          (list "Bank" "Expenses" "Income" "Grand Total")
          (get-row-col sxml #f 1))
        (test-equal "sparse summary-table - col 2"
          (list "$29.00" "-$29.00")
          (get-row-col sxml #f 2))
        (test-equal "sparse summary-table - col 3"
          (list "-$5.00" "$16.00")
          (get-row-col sxml #f 3))
        (test-equal "sparse summary-table - col 4"
          (list "-$23.00" "$15.00")
          (get-row-col sxml #f 4))
        (test-equal "sparse summary-table - col 5"
          (list "$1.00" "$31.00" "-$29.00" "$3.00")
          (get-row-col sxml #f 5))
        (test-equal "sparse summary-table - col 6 average"
          (list "$0.33" "$10.33" "-$9.67" "$1.00")
          (get-row-col sxml #f 6))))
    (test-end "subtotal table")

    (test-begin "invoice-column")
    (let* ((invoices (create-test-invoice-data))
           (options (default-testing-options)))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 9 1980)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 7 9 1980)))
      (set-option! options "Display" "Invoice" #t)
      (set-option! options "Accounts" "Accounts"
                   (list
                    (gnc-account-lookup-by-full-name bank "Root.Asset.Bank")
                    (gnc-account-lookup-by-full-name bank "Root.A/Receivable")
                    (gnc-account-lookup-by-full-name bank "Root.A/Payable")
                    (gnc-account-lookup-by-full-name bank "Root.Income")))
      (let ((sxml (options->sxml options "show invoice")))
        (test-equal "retrieve invoice IDs from trep"
                    '("0003" "0004" "0006" "0007" "0001" "0002" "0005")
                    (get-row-col sxml #f 5))))
    (test-end "invoice-column")

    (test-begin "csv-export")
    (let ((options (default-testing-options)))
      (set-option! options "Accounts" "Accounts"
                   (list bank usd-bank gbp-bank gbp-income income expense))
      (set-option! options "General" "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date"
                   (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (set-option! options "Display" "Subtotal Table" #t)
      (set-option! options "Currency" "Common Currency" #t)
      (set-option! options "Currency" "Report's currency" foreign2)
      (set-option! options "Currency" "Show original currency amount" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)

      (let* ((template (gnc:find-report-template trep-uuid))
             (report-id (gnc:make-report trep-uuid options))
             (report (gnc-report-find report-id))
             (renderer (gnc:report-template-renderer template))
             (document (renderer report #:export-type 'csv)))
        (test-assert "csv output has no export error"
          (not (gnc:html-document-export-error document)))
        (test-equal "csv output is valid"
          "\"from\",\"1969-01-01\"\n\"to\",\"1970-12-31\"\n\"Amount (GBP)\",2.15\n\"Amount\",3.0"
          (gnc:html-document-export-string document))))
    (test-end "csv-export")))

(define (csv-tests)
  (test-begin "csv tests")
  (test-equal "gnc:lists->csv empty"
    ""
    (gnc:lists->csv '(())))
  (test-equal "gnc:lists->csv simple"
    "\"from\",\"01/01/2010\""
    (gnc:lists->csv
     '(("from" "01/01/2010"))))
  (test-equal "gnc:lists->csv complex"
    "\"from\",\"01/01/2010\",,,
\"to\",\"31/12/2010\",,,
\"total\",23500.0,30000.0,3.5714285714285716,sym"
    (gnc:lists->csv
     '(("from" "01/01/2010")
       ("to" "31/12/2010")
       ("total" 23500 30000 25/7 sym))))
  (test-error "gnc:lists->csv improper list" #t
    (gnc:lists->csv
     '(("from" "01/01/2010")
       ("to" "31/12/2010")
       ("total" 23500 30000 25/7 . sym))))
  (test-end "csv tests"))

(define (reconcile-tests)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (liability (cdr (assoc "Liabilities" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today)))
         )

    (define (options->sxml options test-title)
      (gnc:options->sxml reconcile-uuid options "test-reconcile" test-title))

    (define (default-testing-options)
      (let ((options (gnc:make-report-options reconcile-uuid)))
        (set-option! options "Accounts" "Accounts" (list bank liability))
        options))

    ;; old transactions for testing reconcile date options
    (env-transfer env 01 01 1970 bank expense       5   #:description "desc-1" #:num "trn1" #:memo "memo-3")
    (env-transfer env 31 12 1969 income bank       10   #:description "desc-2" #:num "trn2" #:void-reason "void" #:notes "notes3")
    (env-transfer env 31 12 1969 income bank       29   #:description "desc-3" #:num "trn3"
                  #:reconcile (cons #\c (gnc-dmy2time64 01 03 1970)))
    (env-transfer env 01 02 1970 bank expense      15   #:description "desc-4" #:num "trn4" #:notes "notes2" #:memo "memo-1")
    (env-transfer env 10 01 1970 liability expense 10   #:description "desc-5" #:num "trn5" #:void-reason "any")
    (env-transfer env 10 01 1970 liability expense 11   #:description "desc-6" #:num "trn6" #:notes "notes1")
    (env-transfer env 10 02 1970 bank expense       8   #:description "desc-7" #:num "trn7" #:notes "notes1" #:memo "memo-2"
                  #:reconcile (cons #\y (gnc-dmy2time64 01 03 1970)))


    (let* ((options (default-testing-options)))
      (test-assert "reconcile-report basic run"
        (options->sxml options "null test"))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 03 1970)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 03 1970)))
      (let ((sxml (options->sxml options "filter reconcile date")))
        (test-equal "test reconciled amounts = $8"
          (list "Total For Reconciled" "$8.00")
          (get-row-col sxml 3 #f))
        (test-equal "test cleared amounts = $29"
          (list "Total For Cleared" "$29.00")
          (get-row-col sxml 6 #f))
        (test-equal "test unreconciled amounts = $31"
          (list "Total For Unreconciled" "$31.00")
          (get-row-col sxml 11 #f))
        sxml)
      )))
