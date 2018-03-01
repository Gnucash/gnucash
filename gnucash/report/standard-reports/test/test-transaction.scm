(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports transaction))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage)
             (system vm vm))

;; Guide to the test-transaction.scm

;; This test-transation will implement regression testing for most
;; features from transction.scm as of March 2018. It requires SRFI-64
;; (present in guile-2.0.10 or later), SXML, and VM.  SRFI-64 and SXML
;; are mandatory and has tremendously eased creation of tests. The VM
;; modules are only required to perform coverage analysis of this test
;; suite.
;;
;; By default the (run-test) entry point will run (run-test-proper)
;; which sets the SRFI-64 test runner, and initiates the proper test suite
;; in (null-test) and (trep-tests). Please note the tests will all call
;; (options->sxml) which in turn generates the transaction report, and
;; dumps the output at /tmp/out-XX.html for review.

;; For coverage analysis, please amend (run-test) (if #f ...) to (if
;; #t ...)  and this will run (coverage-test) instead, which will
;; generate the coverage report in /tmp/lcov.info -- the latter can be
;; converted to an html report using genhtml from
;; http://ltp.sourceforge.net/coverage/lcov.php

;; Please note the with-code-coverage has changed guile-2.0 to 2.2
;; which does not require specifying the VM; I have no experience
;; running in guile 2.2 and cannot confirm syntax.

;; copied from transaction.scm
(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (test-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (1+ num-passed)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~s\n -> obtained: ~s\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (1+ num-failed)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source:~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)
        (zero? num-failed)))
    runner))

(define (run-test)
  (if #f
      (coverage-test)
      (run-test-proper)))

(define (coverage-test)
  (define %test-vm (make-vm))
  (add-to-load-path "/home/chris/sources/gnucash/gnucash/report/standard-reports")
  (call-with-values
      (lambda()
        (with-code-coverage %test-vm run-test-proper))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test-proper)
  (test-runner-factory test-runner)
  (test-begin "transaction.scm")
  (null-test)
  (trep-tests)
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

(define counter
  (let ((count 0))
    (lambda ()
      (set! count (1+ count))
      count)))

(define (options->sxml options test-title)
  ;; options object -> sxml tree
  ;;
  ;; This function abstracts the whole transaction report renderer.
  ;; It also catches XML parsing errors, dumping the options changed.
  ;;
  ;; It also dumps the render into /tmp/out-N.html where N is a counter
  (let* ((template (gnc:find-report-template trep-uuid))
         (report (constructor trep-uuid "bar" options #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template))
         (document (renderer report)))
    (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
    (if test-title
        (gnc:html-document-set-title! document test-title))
    (let* ((filename (format #f "/tmp/out-~a.html" (counter)))
           (render (gnc:html-document-render document))
           (outfile (open-file filename "w")))
      (display render outfile)
      (close-output-port outfile)
      (catch 'parser-error
        (lambda () (xml->sxml render))
        (lambda (k . args)
          (test-assert k #f)            ; XML parse error doesn't cause a crash but logs as a failure
          (format #t "see render output at ~a\n~a" filename (gnc:render-options-changed options #t)))))))

(define (get-row-col sxml row col)
  ;; sxml, row & col (numbers or #f) -> list-of-string
  ;;
  ;; from an SXML table tree with tr/th/td elements, retrieve row/col
  ;; if row  =  0 retrieve <tr><th> elements
  ;; if row  = #f retrieve whole <td> col, excludes <th> cols
  ;; if col  = #f retrieve whole <tr> row
  ;; if both = #f retrieve all text elements
  ;;
  ;; NOTE: This will retrieve cells from the first table in the tree.
  ;; If there are multiple tables, I recommend that the tree is first
  ;; pruned to the desired table via e.g. '(// (table 2)) then sent as
  ;; argument to this function.
  (let ((xpath (cond
                ((not (or row col))             '(// (table 1) // tr // *text*))
                ((not row)                      `(// (table 1) // tr // (td ,col) // *text*))
                ((and (equal? row 0) (not col)) '(// (table 1) // tr // th // *text*))
                ((not col)                      `(// (table 1) // (tr ,row) // td // *text*))
                ((equal? row 0)                 `(// (table 1) // tr // (th ,col) // *text*))
                (else                           `(// (table 1) // (tr ,row) // (td ,col) // *text*)))))
    ((sxpath xpath) sxml)))
;;
;; END CANDIDATES
;;

(define constructor (record-constructor <report>))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))
(define (opt-val options section name)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-value option)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank")
              (list "GBP Bank")
              (list "USD Bank")
              (list "Wallet"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Liabilities" (list (cons 'type ACCT-TYPE-LIABILITY)))
        ))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options trep-uuid)))
    (test-assert "null-test" (options->sxml options "null-test")))) ;out-1.html

(define (trep-tests)
  ;; This function will perform implementation testing on the transaction report.
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (gbp-bank (cdr (assoc "GBP Bank" account-alist)))
         (usd-bank (cdr (assoc "USD Bank" account-alist)))
         (wallet (cdr (assoc "Wallet" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (liability (cdr (assoc "Liabilities" account-alist)))
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
        options))

    ;; This will make all accounts use default currency (I think depends on locale)
    (for-each
     (lambda(pair)
       (xaccAccountSetCommodity (cdr pair) (gnc-default-report-currency)))
     account-alist)

    ;; Here we set foreign banks' currencies
    (with-account
     gbp-bank
     (lambda ()
       (xaccAccountSetCode gbp-bank "01-GBP")
       (xaccAccountSetCommodity gbp-bank foreign2)))

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
    (let ((txn (xaccMallocTransaction (gnc-get-current-book)))
          (split-1 (xaccMallocSplit  (gnc-get-current-book)))
          (split-2 (xaccMallocSplit  (gnc-get-current-book)))
          (split-3 (xaccMallocSplit  (gnc-get-current-book))))
      (xaccTransBeginEdit txn)
      (xaccTransSetDescription txn "$100bank -> $80expenses + $20wallet")
      (xaccTransSetCurrency txn (xaccAccountGetCommodity bank))
      (xaccTransSetDate txn 14 02 1971)
      (xaccSplitSetParent split-1 txn)
      (xaccSplitSetParent split-2 txn)
      (xaccSplitSetParent split-3 txn)
      (xaccSplitSetAccount split-1 bank)
      (xaccSplitSetAccount split-2 expense)
      (xaccSplitSetAccount split-3 wallet)
      (xaccSplitSetValue split-1 -100)
      (xaccSplitSetValue split-2 80)
      (xaccSplitSetValue split-3 20)
      (xaccSplitSetAmount split-1 -100)
      (xaccSplitSetAmount split-2 80)
      (xaccSplitSetAmount split-3 20)
      (xaccTransSetNotes txn "multisplit")
      (xaccTransCommitEdit txn))

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
    (for-each (lambda (m)
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
    ;;           (xaccAccountGetSplitList bank))

    ;; Finally we can begin testing
    (test-begin "general options")

    (let* ((options (default-testing-options))
           (sxml (options->sxml options "general options")) ;out-2.html
           (default-headers '("Date" "Num" "Description" "Memo/Notes" "Account" "Amount")))
      (test-equal "default headers"
        default-headers
        (get-row-col sxml 0 #f))
      (test-equal "last row has same number of cols as header"
        (length default-headers)
        (length (get-row-col sxml -1 #f)))
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
      (set-option! options "General" "Common Currency" #t)
      (set-option! options "General" "Show original currency amount" #t)
      (set-option! options "General" "Add options summary" 'never)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let ((sxml (options->sxml options "test basic column headers, and original currency"))) ;out-3.html
        (test-equal "default headers, indented, includes common-currency"
          '(" " " " "Date" "Num" "Description" "Memo/Notes" "Account" "Amount" "USD" "Amount")
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
        ))

    (test-end "general options")

    (test-begin "accounts selectors and filtering")

    (let ((options (default-testing-options)))
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))

      ;; Filter Account Name Filters
      (set-option! options "Filter" "Account Name Filter" "Expenses")
      (let ((sxml (options->sxml options "accounts filter expenses"))) ;out-4.html
        (test-equal "account name filter to 'expenses', sum = $31.00"
          '("$31.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Account Name Filter" "Expen.es")
      (let ((sxml (options->sxml options "accounts filter expen.es"))) ;out-5.html
        (test-equal "account name filter to 'expen.es', blank report"
          '()
          (get-row-col sxml #f #f)))

      (set-option! options "Filter" "Use regular expressions for account name filter" #t)
      (let ((sxml (options->sxml options "accounts filter expen.es regex"))) ;out-6.html
        (test-equal "account name filter to 'expen.es' and switch on regex filter, sum = $31.00"
          '("$31.00")
          (get-row-col sxml -1 -1)))

      ;; Test Transaction Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (set-option! options "Filter" "Transaction Filter" "desc-3")
      (let ((sxml (options->sxml options "transaction filter to ponies"))) ;out-7.html
        (test-equal "transaction filter in bank to 'desc-3', sum = $29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Transaction Filter" "not.s?")
      (let ((sxml (options->sxml options "transaction filter not.s?"))) ;out-8.html
        (test-equal "transaction filter in bank to 'not.s?', blank report"
          '()
          (get-row-col sxml #f #f)))

      (set-option! options "Filter" "Use regular expressions for transaction filter" #t)
      (let ((sxml (options->sxml options "transaction filter not.s? regex"))) ;out-9.html
        (test-equal "transaction filter in bank to 'not.s?' and switch regex, sum = -$23.00"
          '("-$23.00")
          (get-row-col sxml -1 -1)))

      ;; Test Reconcile Status Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Filter" "Reconcile Status" 'unreconciled)
      (let ((sxml (options->sxml options "unreconciled"))) ;out-10.html
        (test-equal "filter unreconciled only, sum = -$20.00"
          '("-$20.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Reconcile Status" 'cleared)
      (let ((sxml (options->sxml options "cleared"))) ;out-11.html
        (test-equal "filter cleared only, sum = $29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Reconcile Status" 'reconciled)
      (let ((sxml (options->sxml options "reconciled"))) ;out-12.html
        (test-equal "filter reconciled only, sum = -$8.00"
          '("-$8.00")
          (get-row-col sxml -1 -1)))

      ;; Test Accounts Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Accounts" "Filter By..." (list income))
      (set-option! options "Accounts" "Filter Type" 'include)
      (let ((sxml (options->sxml options "including bank-income accts only"))) ;out-13.html
        (test-equal "filter includes bank-income, sum = -$29.00"
          '("$29.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Accounts" "Filter Type" 'exclude)
      (let ((sxml (options->sxml options "bank exclude bank-income accts"))) ;out-14.html
        (test-equal "filter excludes bank-income, sum = -$28.00"
          '("-$28.00")
          (get-row-col sxml -1 -1)))

      ;; Test Void Transaction Filters
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))      
      (set-option! options "Filter" "Void Transactions" 'void-only)
      (let ((sxml (options->sxml options "void only"))) ;out-15.html
        (test-equal "filter void-transactions only, sum = -$10.00"
          '("$10.00")
          (get-row-col sxml -1 -1)))

      (set-option! options "Filter" "Void Transactions" 'both)
      (let ((sxml (options->sxml options "both void and non-void"))) ;out-16.html
        (test-equal "filter void-transactions only, sum = $11.00"
          '("$11.00")
          (get-row-col sxml -1 -1))))

    (test-end "accounts selectors and filtering")

    (test-begin "display options")

    (let ((options (default-testing-options)))
      ;; Disable most Display/* columns
      (for-each
       (lambda (name)
         (set-option! options "Display" name #f))
       (list "Date" "Reconciled Date" "Num" "Description" "Memo" "Notes"
             "Account Name" "Other Account Name" "Shares" "Price" "Running Balance"
             "Totals"))
      (let ((sxml (options->sxml options "all columns off"))) ;out-17.html
        (test-assert "all display columns off, except amount and subtotals are enabled, there should be 2 columns"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             2)))

      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'none)
      (let ((sxml (options->sxml options "only amounts"))) ;out-18.html
        (test-assert "all display columns off, and no subtotals, but amount enabled, there should be 1 column"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             1)))

      (set-option! options "Display" "Amount" 'none)
      (let ((sxml (options->sxml options "no columns"))) ;out-19.html
        (test-assert "all display columns off, without amount nor subtotals, there should be 0 column"
          (= (length ((sxpath '(// (table 1) // (tr 1) // th)) sxml))
             (length ((sxpath '(// (table 1) // (tr 4) // td)) sxml))
             (length ((sxpath '(// (table 1) // (tr -1) // td)) sxml))
             0)))

      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'weekly)
      (set-option! options "Sorting" "Secondary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'weekly)
      (let ((sxml (options->sxml options "subtotals only"))) ;out-20.html
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
             "Account Name" "Other Account Name" "Shares" "Price" "Running Balance"
             "Totals" "Use Full Other Account Name" "Use Full Account Name"))
      (let* ((sxml (options->sxml options "all columns on"))) ;out-21.html
        (test-equal "all display columns on, displays correct columns"
          (list "Date" "Reconciled Date" "Num" "Description" "Memo/Notes" "Account"
                "Transfer from/to" "Shares" "Price" "Amount" "Running Balance")
          (get-row-col sxml 0 #f))
        (test-assert "reconciled dates must be 01/03/70 or whitespace"
          (and-map
           (lambda (reconcile-date-string)
             (or (string=? (string-trim-both reconcile-date-string)
                           (qof-print-date (gnc-dmy2time64 01 03 1970)))
                 (string-null? (string-trim-both reconcile-date-string))))
           (get-row-col sxml #f 2)))
        (test-equal "reconciled status subtotal"
          (list "Total For Unreconciled" " " " " " " " " " " " " " " " " "$0.00" " ")
          (get-row-col sxml -3 #f))
        )

      ;; Customized test for multi-line, with default display cols.
      ;; This option should return a single transaction with 2 splits
      ;; in 2 lines.
      (set! options (default-testing-options))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 13 02 1971)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 15 02 1971)))
      (set-option! options "Display" "Detail Level" 'multi-line)
      (set-option! options "Display" "Totals" #f)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'none)
      (let* ((sxml (options->sxml options "multiline"))) ;out-22.html
        (test-assert "multi line transaction with 1st split have same memo"
          (apply string=? (get-row-col sxml #f 4)))

        ;; the following sxpath will retrieve all text from <a href> of the
        ;; last <td> in the table. This retrieves the amounts.
        (test-equal "multi-line amounts must total to zero"
          0.0
          (apply + (map str->num ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml)))))

      ;; Remove expense multisplit, transaction is not shown
      (set-option! options "Accounts" "Filter By..." (list expense))
      (set-option! options "Accounts" "Filter Type" 'exclude)
      (let* ((sxml (options->sxml options "multiline, filtered out"))) ;out-23.html
        (test-equal "multi-line has been excluded"
          '()
          (get-row-col sxml #f #f)))

      ;; Testing for original currency amount display as well as
      ;; dual-column subtotals
      (set! options (default-testing-options))
      (set-option! options "Accounts" "Accounts" (list usd-bank gbp-bank))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 2000)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 2000)))
      (set-option! options "General" "Common Currency" #t)
      (set-option! options "General" "Show original currency amount" #t)
      (let* ((sxml (options->sxml options "single column, with original currency headers"))) ;out-24.html
        (test-equal "single amount column, with original currency headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account"
                "Amount" "USD" "Amount")
          (get-row-col sxml 0 #f)))

      (set-option! options "Display" "Amount" 'double)
      (set-option! options "Display" "Account Name" #t)
      (set-option! options "Display" "Account Code" #t)
      (set-option! options "Display" "Other Account Name" #t)
      (set-option! options "Display" "Other Account Code" #t)
      (let* ((sxml (options->sxml options "dual column"))) ;out-25.html
        ;; Note. It's difficult to test converted monetary
        ;; amounts. Although I've set transfers from USD/GBP, the
        ;; transfers do not update the pricedb automatically,
        ;; therefore converted amounts are displayed as $0. We are not
        ;; testing the pricedb so it does not seem fair to test its
        ;; output here too.
        (test-equal "dual amount headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account" "Transfer from/to"
                "Debit" "USD" "Credit" "USD" "Debit" "Credit")
          (get-row-col sxml 0 #f))
        (test-equal "Account Name and Code displayed"
          (list "01-GBP Root.Asset.GBP Bank")
          (get-row-col sxml 2 5))
        (test-equal "Other Account Name and Code displayed"
          (list "01-GBP GBP Bank")
          (get-row-col sxml 7 6))
        (test-equal "GBP original currency totals = £4"
          (list 4.0)
          (map str->num (get-row-col sxml 5 10)))
        (test-assert "USD original currency totals = $5"
          (equal?
           (list 5.0)
           (map str->num (get-row-col sxml 9 7))
           (map str->num (get-row-col sxml 9 9))))
        )

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
      (let* ((sxml (options->sxml options "sign-reversal is none, correct signs of amounts?"))) ;out-26.html
        (test-equal "sign-reversal is none, correct signs of amounts"
          '(#f #t #t #f #f #t #t #t #t #f #f #f #f #t)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      (set-option! options "Display" "Sign Reverses" 'income-expense)
      (let* ((sxml (options->sxml options "sign-reversal is income-expense, correct signs of amounts?"))) ;out-27.html
        (test-equal "sign-reversal is income-expense, correct signs of amounts"
          '(#f #t #t #f #f #f #f #f #f #t #t #f #f #t)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      (set-option! options "Display" "Sign Reverses" 'credit-accounts)
      (let* ((sxml (options->sxml options "sign-reversal is credit-accounts, correct signs of amounts?"))) ;out-28.html
        (test-equal "sign-reversal is credit-accounts, correct signs of amounts"
          '(#f #t #t #f #f #t #t #t #t #t #t #t #t #f)
          (map (lambda (s) (not (string-contains s "-")))
               ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml))))

      ;; test debit/credit dual columns
      (set! options (default-testing-options))
      (set-option! options "Display" "Amount" 'double)
      (set-option! options "General" "Common Currency" #t)
      (set-option! options "General" "Show original currency amount" #t)
      (set-option! options "Sorting" "Primary Key" 'date)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (let* ((sxml (options->sxml options "dual columns"))) ;out-29.html
        (test-equal "dual amount column, with original currency headers"
          (list "Date" "Num" "Description" "Memo/Notes" "Account"
                "Debit" "USD" "Credit" "USD" "Debit" "Credit")
          (get-row-col sxml 0 #f))
        (test-equal "dual amount column, grand totals available"
          (list "Grand Total" " " " " " " " " "$2,280.00" "$2,280.00")
          (get-row-col sxml -1 #f))
        (test-equal "dual amount column, first transaction correct"
          (list "01/03/18" "$103 income" "Root.Asset.Bank" "\n" "$103.00" " " "\n" "$103.00" " ")
          (get-row-col sxml 1 #f)))
      )

    (test-end "display options")

    (test-begin "sorting options")

    (let ((options (default-testing-options)))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      ;;(set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "Accounts" "Accounts" (list bank))
      (set-option! options "Display" "Totals" #f)
      (set-option! options "Display" "Other Account Name" #t)
      (set-option! options "Filter" "Void Transactions" 'both)
      (set-option! options "Sorting" "Primary Subtotal" #f)
      (set-option! options "Sorting" "Primary Subtotal for Date Key" 'none)
      (set-option! options "Sorting" "Secondary Key" 'none)
      (set-option! options "Sorting" "Secondary Subtotal" #f)

      (set-option! options "Sorting" "Primary Key" 'date)
      (let* ((sxml (options->sxml options "sorting=date"))) ;out-30.html
        (test-equal "dates are sorted"
          '("12/31/69" "12/31/69" "01/01/70" "02/01/70" "02/10/70")
          (get-row-col sxml #f 1)))

      (set-option! options "Sorting" "Primary Key" 'number)
      (let* ((sxml (options->sxml options "sorting=number"))) ;out-31.html
        (test-equal "sort by number"
          '("trn1" "trn2" "trn3" "trn4" "trn7")
          (get-row-col sxml #f 2)))

      (set-option! options "Sorting" "Primary Key" 'reconciled-status)
      (let* ((sxml (options->sxml options "sorting=reconciled-status"))) ;out-32.html
        (test-equal "sort by reconciled status"
          '("desc-2" "desc-7" "desc-3" "desc-1" "desc-4")
          (get-row-col sxml #f 3)))

      (set-option! options "Sorting" "Primary Key" 'memo)
      (let* ((sxml (options->sxml options "sorting=memo"))) ;out-33.html
        (test-equal "sort by memo"
          '("notes3" "memo-1" "memo-2" "memo-3")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Primary Key" 'account-name)
      (let* ((sxml (options->sxml options "sorting=account-name"))) ;out-34.html
        (test-assert "account names are sorted"
          (sorted? (get-row-col sxml #f 5) string<?)))

      (set-option! options "Sorting" "Primary Key" 'corresponding-acc-name)
      (let* ((sxml (options->sxml options "sorting=corresponding-acc-name"))) ;out-35.html
        (test-equal "sort by corresponding-acc-name"
          '("Expenses" "Expenses" "Income" "Income" "Liabilities")
          (get-row-col sxml #f 6)))

      (set-option! options "Sorting" "Primary Key" 'amount)
      (let* ((sxml (options->sxml options "sorting=amount"))) ;out-36.html
        (test-equal "sort by amount"
          '("-$15.00" "-$8.00" "-$5.00" "$10.00" "$29.00")
          ((sxpath '(// (table 1) // tr // (td -1) // a // *text*)) sxml)))

      (set! options (default-testing-options))
      (set-option! options "Sorting" "Add indenting columns" #t)
      (set-option! options "General" "Show original currency amount" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (set-option! options "Display" "Totals" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'quarterly)
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (let* ((sxml (options->sxml options "sorting=account-name, date-quarterly, subtotals only"))) ;out-37.html
        (test-equal "sorting=account-name, date-quarterly, subtotals only"
          '("$570.00" "$570.00" "$570.00" "$570.00" "$2,280.00" "$2,280.00")
          (get-row-col sxml #f -1)))

      (set! options (default-testing-options))
      (set-option! options "Accounts" "Accounts" (gnc-account-get-descendants (gnc-account-get-root bank)))
      (set-option! options "Display" "Totals" #t)
      (set-option! options "Display" "Amount" 'double)
      (set-option! options "General" "Show original currency amount" #t)
      (set-option! options "General" "Table for Exporting" #f)
      (set-option! options "Sorting" "Add indenting columns" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'quarterly)
      (set-option! options "Sorting" "Show Informal Debit/Credit Headers" #t)
      (set-option! options "Sorting" "Show Account Description" #t)
      (let* ((sxml (options->sxml options "sorting=date"))) ;out-38.html
        (test-equal "expense acc friendly headers"
          '("\n" "Expenses" "Expense" "Rebate")
          (get-row-col sxml 47 #f))
        (test-equal "income acc friendly headers"
          '("\n" "Income" "Charge" "Income")
          (get-row-col sxml 69 #f)))

      (set-option! options "Accounts" "Accounts" (list bank))
      (set-option! options "Display" "Totals" #f)
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (let* ((sxml (options->sxml options "sorting=date quarterly"))) ;out-39.html
        (test-equal "quarterly subtotals are correct"
          '("$570.00" "$570.00" "$570.00" "$570.00")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let* ((sxml (options->sxml options "sorting=date monthly"))) ;out-40.html
        (test-equal "monthly subtotals are correct"
          '("$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00")
          (get-row-col sxml #f 4)))

      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'yearly)
      (let* ((sxml (options->sxml options "sorting=date yearly"))) ;out-41.html
        (test-equal "yearly subtotals are correct"
          '("$2,280.00")
          (get-row-col sxml #f 4)))

      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 31 12 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1972)))
      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #f)
      (set-option! options "Filter" "Void Transactions" 'both)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'daily)
      (let* ((sxml (options->sxml options "sorting=date"))) ;out-42.html
        (test-equal "daily subtotals are correct"
          '("$39.00")
          (get-row-col sxml 5 4)))

      (set-option! options "Sorting" "Show subtotals only (hide transactional data)" #t)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'weekly)
      (let* ((sxml (options->sxml options "sorting=date weekly"))) ;out-43.html
        (test-equal "weekly subtotals are correct (1)"
          '("$34.00" "$89.00")
          (get-row-col sxml #f 4))
        (test-equal "weekly subtotals are correct (2)"
          '("$15.00" "$8.00" "$100.00")
          (get-row-col sxml #f 5))))

    (test-end "sorting options")

    (test-begin "subtotal table")

    (let ((options (default-testing-options)))
      (set-option! options "Accounts" "Accounts" (list bank income expense))
      (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
      (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
      (set-option! options "Display" "Subtotal Table" #t)
      (set-option! options "Sorting" "Primary Key" 'account-name)
      (set-option! options "Sorting" "Primary Subtotal" #t)
      (set-option! options "Sorting" "Secondary Key" 'date)
      (set-option! options "Sorting" "Secondary Subtotal for Date Key" 'monthly)
      (let ((sxml (options->sxml options "subtotal table"))) ;out-44.html
        (test-equal "summary bank-row is correct"
          (list "Bank" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00"
                "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$190.00" "$2,280.00")
          (get-row-col sxml 1 #f))
        (test-equal "summary expense-row is correct"
          (list "Expenses" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00"
                "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$22.00" "$264.00")
          (get-row-col sxml 2 #f))
        (test-equal "summary income-row is correct"
          (list "Income" "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00"
                "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00" "-$212.00"
                "-$212.00" "-$2,544.00")
          (get-row-col sxml 3 #f))
        (test-equal "summary total-row is correct"
          (list "Grand Total" "$0.00")
          (get-row-col sxml 4 #f)))

      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 1969)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 31 12 1970)))
      (let ((sxml (options->sxml options "sparse subtotal table"))) ;out-45.html
        (test-equal "sparse summary-table - row 1"
          (list "Bank" "$29.00" "-$5.00" "-$23.00" "$1.00")
          (get-row-col sxml 1 #f))
        (test-equal "sparse summary-table - row 2"
          (list "Expenses" "$16.00" "$15.00" "$31.00")
          (get-row-col sxml 2 #f))
        (test-equal "sparse summary-table - row 3"
          (list "Income" "-$29.00" "-$29.00")
          (get-row-col sxml 3 #f))
        (test-equal "sparse summary-table - row 4"
          (list "Grand Total" "$3.00")
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
          (get-row-col sxml #f 5))))
    (test-end "subtotal table")
    ))
