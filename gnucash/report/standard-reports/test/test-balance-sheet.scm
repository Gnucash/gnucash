(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports balance-sheet))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; This is implementation testing for Balance Sheet.

(define balance-sheet-uuid "c4173ac99b2b448289bf4d11c731af13")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "balsheet.scm")
  (null-test)
  (balsheet-tests)
  (test-end "balsheet.scm"))

(define (options->sxml options test-title)
  (gnc:options->sxml balance-sheet-uuid options "test-balsheet" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank"
                    (list "Bank-Sub"))
              (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE))))
        (list "Liability" (list (cons 'type ACCT-TYPE-PAYABLE))
              (list "CreditCard")
              (list "A/Payable"))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))))

(define (null-test)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options balance-sheet-uuid)))
    (test-assert "null-test" (options->sxml options "null-test"))))

(define (balsheet-tests)
  ;; This function will perform implementation testing on the transaction report.
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (banksub (cdr (assoc "Bank-Sub" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (creditcard (cdr (assoc "CreditCard" account-alist)))
         (payable (cdr (assoc "A/Payable" account-alist)))
         (receivable (cdr (assoc "A/Receivable" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (default-testing-options)
      ;; To ease testing of transaction report, we will set default
      ;; options for generating reports. We will elable extra columns
      ;; for Exporting, disable generation of informational text, and
      ;; disable indenting. These options will be tested separately as
      ;; the first test group. By default, we'll select the modern dates.
      (let ((options (gnc:make-report-options balance-sheet-uuid)))
        (set-option! options "General" "Balance Sheet Date" (cons 'relative 'end-cal-year))
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

    (create-txn 1 1 YEAR "invoice charge $100"
                (list (cons -100 income)
                      (cons  100 receivable))
                TXN-TYPE-INVOICE)

    (create-txn 1 2 YEAR "receive part-payment $98"
                (list (cons -98 receivable)
                      (cons  98 bank))
                TXN-TYPE-PAYMENT)

    (create-txn 1 3 YEAR "receive bill $55"
                (list (cons  55 expense)
                      (cons -55 payable))
                TXN-TYPE-INVOICE)

    (create-txn 1 4 YEAR "part-pay bill $50 using creditcard"
                (list (cons  50 payable)
                      (cons -50 creditcard))
                TXN-TYPE-PAYMENT)

    (create-txn 1 5 YEAR "part-pay creditcard from bank"
                (list (cons  47 creditcard)
                      (cons -47 banksub)))

    ;; Finally we can begin testing
    (test-begin "display options")

    (let* ((options (default-testing-options))
           (sxml (options->sxml options "default")))

      (test-equal "total assets = $53.00"
        (list "Total Assets" "$53.00")
        (sxml->table-row-col sxml 1 7 #f))

      (test-equal "total liabilities = $8.00"
        (list "Total Liabilities" "$8.00")
        (sxml->table-row-col sxml 1 14 #f))

      (test-equal "total equity  = $45.00"
        (list "Total Equity" "$45.00")
        (sxml->table-row-col sxml 1 19 #f))

      )
    (test-end "display options")))
