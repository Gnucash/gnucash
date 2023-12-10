(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports))
(use-modules (gnucash report stylesheets plain))
(use-modules (gnucash report))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

(define uuid-list
  (list (cons 'customer-new "c146317be32e4948a561ec7fc89d15c1")))

(setlocale LC_ALL "C")

(define (teardown)
  (gnc-clear-current-session))

(define (run-test)
  (let ((saved-format (qof-date-format-get)))
    (qof-date-format-set QOF-DATE-FORMAT-ISO)
    (test-runner-factory gnc:test-runner)
    (test-begin "test-owner-report")
    (test-group-with-cleanup "test-owner-report"
      (owner-tests)
      (teardown))
    (qof-date-format-set saved-format)
    (test-end "test-owner-report")))

(define (sxml-get-row-col sxml row col)
  (sxml->table-row-col sxml 3 row col))

(define (set-option! options section name value)
  (if (gnc-lookup-option (gnc:optiondb options) section name)
      (gnc-set-option (gnc:optiondb options) section name value)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))

(define (get-currency sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                     (cons 'commodity (get-currency "USD")))
        (list "Asset"
              (list "Bank-GBP" (list (cons 'commodity (get-currency "GBP"))))
              (list "Bank-EUR" (list (cons 'commodity (get-currency "EUR"))))
              (list "Bank-USD"))
        (list "VAT"
              (list "VAT-on-Purchases")
              (list "VAT-on-Sales" (list (cons 'type ACCT-TYPE-LIABILITY))))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
              (list "Income-USD")
              (list "Income-GBP" (list (cons 'commodity (get-currency "GBP"))))
              (list "Income-EUR" (list (cons 'commodity (get-currency "EUR")))))
        (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE))
              (list "AR-USD")
              (list "AR-GBP" (list (cons 'commodity (get-currency "GBP"))))
              (list "AR-EUR" (list (cons 'commodity (get-currency "EUR")))))
        (list "A/Payable" (list (cons 'type ACCT-TYPE-PAYABLE))
              (list "AP-USD")
              (list "AP-GBP" (list (cons 'commodity (get-currency "GBP"))))
              (list "AP-EUR" (list (cons 'commodity (get-currency "EUR")))))))

(define (owner-tests)
  ;; This function will perform implementation testing on the customer report.
  (define (options->sxml variant options test-title)
    (define uuid (cdr (assq variant uuid-list)))
    ;; (format #t "[~a] Options:\n~a"
    ;;         test-title
    ;;         (gnc:html-render-options-changed options #t))
    (gnc:options->sxml uuid options (format #f "test-~a" variant) test-title))
  (define (options->invoice inv)
    (let* ((inv-uuid "5123a759ceb9483abf2182d01c140e8d") ;invoice
           (inv-options (gnc:make-report-options inv-uuid)))
      (set-option! inv-options "General" "Invoice Number" inv)
      (gnc:options->sxml inv-uuid inv-options "test" "test-invoice")))

  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (get-acct (lambda (name)
                     (or (assoc-ref account-alist name)
                     (error "invalid account name" name))))
         (YEAR (gnc:time64-get-year (gnc:get-today)))

         (cust-1 (let ((cust-1 (gncCustomerCreate (gnc-get-current-book))))
                   (gncCustomerSetID cust-1 "cust-1-id")
                   (gncCustomerSetName cust-1 "cust-1-name")
                   (gncCustomerSetNotes cust-1 "cust-1-notes")
                   (gncCustomerSetCurrency cust-1 (get-currency "USD"))
                   (gncCustomerSetTaxIncluded cust-1 1) ;1 = GNC-TAXINCLUDED-YES
                   cust-1))

         (owner-1 (let ((owner-1 (gncOwnerNew)))
                    (gncOwnerInitCustomer owner-1 cust-1)
                    owner-1))

         ;; inv-1 is generated for a customer
         (inv-1 (let ((inv-1 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-1 owner-1)
                  (gncInvoiceSetNotes inv-1 "inv-1-notes")
                  (gncInvoiceSetBillingID inv-1 "inv-1-billing-id")
                  (gncInvoiceSetCurrency inv-1 (get-currency "USD"))
                  inv-1))

         (job-1 (let ((job-1 (gncJobCreate (gnc-get-current-book))))
                  (gncJobSetID job-1 "job-1-id")
                  (gncJobSetName job-1 "job-1-name")
                  (gncJobSetOwner job-1 owner-1)
                  job-1))
         (owner-2 (let ((owner-2 (gncOwnerNew)))
                    (gncOwnerInitJob owner-2 job-1)
                    owner-2))
         ;; inv-2 is generated from a customer's job
         (inv-2 (let ((inv-2 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-2 owner-2)
                  (gncInvoiceSetNotes inv-2 "inv-2-notes")
                  (gncInvoiceSetCurrency inv-2 (get-currency "USD"))
                  inv-2))

         (entry (lambda (amt)
                  (let ((entry (gncEntryCreate (gnc-get-current-book))))
                    (gncEntrySetDateGDate entry (time64-to-gdate (current-time)))
                    (gncEntrySetDescription entry "entry-desc")
                    (gncEntrySetAction entry "entry-action")
                    (gncEntrySetNotes entry "entry-notes")
                    (gncEntrySetInvAccount entry (get-acct "Income-USD"))
                    (gncEntrySetDocQuantity entry 1 #f)
                    (gncEntrySetInvPrice entry amt)
                    entry)))

         ;; entry-1  1 widgets of $6 = $6
         (entry-1 (entry 6))

         ;; entry-2  3 widgets of EUR4 = EUR12
         (entry-2 (let ((entry-2 (gncEntryCreate (gnc-get-current-book))))
                    (gncEntrySetDateGDate entry-2 (time64-to-gdate (current-time)))
                    (gncEntrySetDescription entry-2 "entry-2-desc")
                    (gncEntrySetAction entry-2 "entry-2-action")
                    (gncEntrySetNotes entry-2 "entry-2-notes")
                    (gncEntrySetInvAccount entry-2 (get-acct "Income-EUR"))
                    (gncEntrySetDocQuantity entry-2 3 #f)
                    (gncEntrySetInvPrice entry-2 4)
                    entry-2))

         ;; entry-3  5 widgets of GBP7 = GBP35
         (entry-3 (let ((entry-3 (gncEntryCreate (gnc-get-current-book))))
                    (gncEntrySetDateGDate entry-3 (time64-to-gdate (current-time)))
                    (gncEntrySetDescription entry-3 "entry-3-desc")
                    (gncEntrySetAction entry-3 "entry-3-action")
                    (gncEntrySetNotes entry-3 "entry-3-notes")
                    (gncEntrySetInvAccount entry-3 (get-acct "Income-GBP"))
                    (gncEntrySetDocQuantity entry-3 5 #f)
                    (gncEntrySetInvPrice entry-3 7)
                    entry-3))

         (standard-vat-sales-tt
          (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
            (gncTaxTableIncRef tt)
            (gncTaxTableSetName tt "10% vat on sales")
            (let ((entry (gncTaxTableEntryCreate)))
              (gncTaxTableEntrySetAccount entry (get-acct "VAT-on-Sales"))
              (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
              (gncTaxTableEntrySetAmount entry 10)
              (gncTaxTableAddEntry tt entry))
            tt))

         (standard-vat-purchases-tt
          (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
            (gncTaxTableIncRef tt)
            (gncTaxTableSetName tt "10% vat on purchases")
            (let ((entry (gncTaxTableEntryCreate)))
              (gncTaxTableEntrySetAccount entry (get-acct "VAT-on-Purchases"))
              (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
              (gncTaxTableEntrySetAmount entry 10)
              (gncTaxTableAddEntry tt entry))
            tt)))

    (define* (default-testing-options variant owner account)
      ;; owner-report will run from 1.1.1980 to 1.7.1980
      (let ((options (gnc:make-report-options
                      (assq-ref uuid-list variant))))
        (set-option! options "General"
                     (case variant
                       ((customer) "Customer")
                       ((customer-new) "Customer")
                       ((job) "Job"))
                     owner)
        (set-option! options "General" "From"
                     (cons 'absolute (gnc-dmy2time64 1 1 1980)))
        (set-option! options "General" "To"
                     (cons 'absolute (gnc-dmy2time64 1 7 1980)))
        (cond
         ((eq? variant 'customer-new)
          (set-option! options "Display Columns" "Links" 'detailed))
         (else
          (set-option! options "General" "Account" account)))
        options))

    ;; inv-1 $6, due 18.7.1980 after report-date i.e. "current"
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 27/4))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 13 05 1980) ;posted
                               (gnc-dmy2time64 18 07 1980) ;due
                               "inv current $6.75" #t #f))

    ;; inv-1-copy due 18.6.1980, <30days before report date
    ;; amount due $12
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 4))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 13 04 1980) ;posted
                               (gnc-dmy2time64 18 06 1980) ;due
                               "inv <30days $4.00" #t #f))

    ;; inv-1-copy due 18.5.1980, 30-60days before report date
    ;; amount due $6
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 17/2))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 13 03 1980) ;posted
                               (gnc-dmy2time64 18 05 1980) ;due
                               "inv 30-60 $8.50" #t #f))

    ;; inv-1-copy due 18.4.1980, 60-90days before report date
    ;; amount due $6
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 15/2))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 13 02 1980) ;posted
                               (gnc-dmy2time64 18 04 1980) ;due
                               "inv 60-90 $7.50" #t #f))

    ;; inv-1-copy due 18.3.1980, >90days before report date
    ;; amount due $11.50, drip-payments
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 23/2))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 13 01 1980) ;posted
                               (gnc-dmy2time64 18 03 1980) ;due
                               "inv >90 $11.50" #t #f)
      (gncInvoiceApplyPayment
       inv-1-copy '() (get-acct "Bank-USD") 3/2 1
       (gnc-dmy2time64 18 03 1980)
       "inv >90 payment" "pay only $1.50")
      (gncInvoiceApplyPayment
       inv-1-copy '() (get-acct "Bank-USD") 2 1
       (gnc-dmy2time64 20 03 1980)
       "inv >90 payment" "pay only $2.00"))

    ;; inv-1-copy due 18.3.1980, >90days before report date
    ;; amount due $11.50, drip-payments
    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry 200))
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 18 04 1980) ;posted
                               (gnc-dmy2time64 18 04 1980) ;due
                               "inv $200" #t #f)
      (gncInvoiceApplyPayment
       inv-1-copy '() (get-acct "Bank-USD") 200 1
       (gnc-dmy2time64 19 04 1980)
       "inv $200" "fully paid"))

    (let ((inv-1-copy (gncInvoiceCopy inv-1)))
      (gncInvoiceAddEntry inv-1-copy (entry -3))
      (gncInvoiceSetIsCreditNote inv-1-copy #t)
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 22 06 1980) ;posted
                               (gnc-dmy2time64 22 06 1980) ;due
                               "inv $3 CN" #t #f))

    (display "new-owner-report tests:\n")
    (test-begin "new-customer-report")
    (let* ((options (default-testing-options 'customer-new
                      owner-1 (get-acct "AR-USD")))
           (sxml (options->sxml 'customer-new options "new-customer-report basic")))
      (test-equal "inv-descriptions"
        '("inv >90 $11.50" "-$2.00" "inv 60-90 $7.50" "inv 30-60 $8.50"
          "inv >90 payment" "inv >90 payment" "inv <30days $4.00"
          "inv $200" "inv $200" "inv current $6.75" "inv $3 CN"
          "$31.75" "$7.50")
        ((sxpath `(// (table 3) // tr (td 5) // *text*))
         sxml))
      (test-equal "credit-amounts"
        '("$11.50" "-$2.00" "$7.50" "$8.50" "$4.00" "$200.00" "$6.75" "$8.00")
        ((sxpath `(// (table 3) // tr (td 6) // *text*))
         sxml))
      (test-equal "debit-amounts"
        '("$1.50" "$2.00" "$200.00" "$3.00" "$31.75")
        ((sxpath `(// (table 3) // tr (td 7) // *text*))
         sxml))
      (test-equal "balance-amounts"
        '("$11.50" "$19.00" "$27.50" "$26.00" "$24.00" "$28.00"
          "$228.00" "$28.00" "$34.75" "$31.75")
        ((sxpath `(// (table 3) // tr (td 8) // *text*))
         sxml))
      (test-equal "positive-link-amounts"
        '("-$1.50" "-$2.00" "$8.00" "$7.50" "$8.50" "$11.50" "$11.50"
          "$4.00" "-$200.00" "$200.00" "$6.75")
        ((sxpath `(// (table 3) // tr
                      (td -1 (@ (equal? (class "number-cell")))) //
                      *text*))
         sxml))
      (test-equal "negative-link-amounts"
        '("-$3.00")
        ((sxpath `(// (table 3) // tr
                      (td -1 (@ (equal? (class "number-cell neg")))) //
                      *text*))
         sxml))
      ;; from the report, find the 3rd table, last row, find embedded
      ;; table, retrieve tr contents
      (test-equal "aging-table"
        '("$0.00" "$6.75" "$1.00" "$8.50" "$7.50" "$8.00" "$31.75")
        ((sxpath `(// (table 3) // (tr -1) // table // tbody // tr // *text*))
         sxml))

      (test-equal "dr/cr headers"
        '("Date" "Due Date" "Reference" "Type" "Description"
          "Debits" "Credits" "Balance" "Date" "Reference" "Type"
          "Description" "Partial Amount" "Amount")
        ((sxpath `(// (table 3) // thead // (tr 2) // *text*))
         sxml))
      )
    (test-end "new-customer-report")))
