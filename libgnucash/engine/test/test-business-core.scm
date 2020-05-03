(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (gnucash gnc-module))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash engine test test-extras))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))
(use-modules (gnucash utilities))
(use-modules (gnucash business-core))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-business-core")
  (core-tests)
  (test-end "test-business-core"))

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

(define (core-tests)
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
      (gncInvoiceAddEntry inv-1-copy (entry -5/2))
      (gncInvoiceSetIsCreditNote inv-1-copy #t)
      (gncInvoicePostToAccount inv-1-copy
                               (get-acct "AR-USD")         ;post-to acc
                               (gnc-dmy2time64 22 06 1980) ;posted
                               (gnc-dmy2time64 22 06 1980) ;due
                               "inv $2.50 CN" #t #f))

    (test-equal "gnc:owner-get-name-dep"
      "cust-1-name"
      (gnc:owner-get-name-dep owner-1))

    (test-equal "gnc:owner-get-address-dep"
      ""
      (gnc:owner-get-address-dep owner-1))

    (test-equal "gnc:owner-get-name-and-address-dep"
      "cust-1-name\n"
      (gnc:owner-get-name-and-address-dep owner-1))

    (test-equal "gnc:owner-get-owner-id"
      "cust-1-id"
      (gnc:owner-get-owner-id owner-1))

    ;; a non-business transaction
    (env-transfer env 01 01 1990
                  (get-acct "Income-GBP") (get-acct "Bank-GBP") 10)

    (let ((new-owner (gncOwnerNew)))

      (test-equal "new-owner is initially empty"
        ""
        (gncOwnerGetName new-owner))

      (test-equal "gnc:owner-from-split (from AR) return"
        "cust-1-name"
        (gncOwnerGetName
         (gnc:owner-from-split
          (last (xaccAccountGetSplitList (get-acct "AR-USD")))
          new-owner)))

      (test-equal "gnc:owner-from-split (from AR) mutated"
        "cust-1-name"
        (gncOwnerGetName new-owner))

      (set! new-owner (gncOwnerNew))
      (test-equal "gnc:owner-from-split (from inc-acct) return"
        "cust-1-name"
        (gncOwnerGetName
         (gnc:owner-from-split
          (last (xaccAccountGetSplitList (get-acct "Income-USD")))
          new-owner)))

      (test-equal "gnc:owner-from-split (from inc-acct) mutated"
        "cust-1-name"
        (gncOwnerGetName new-owner))

      (set! new-owner (gncOwnerNew))
      (test-equal "gnc:owner-from-split (from payment txn) return"
        "cust-1-name"
        (gncOwnerGetName
         (gnc:owner-from-split
          (last (xaccAccountGetSplitList (get-acct "Bank-USD")))
          new-owner)))

      (test-equal "gnc:owner-from-split (from payment txn) mutated"
        "cust-1-name"
        (gncOwnerGetName new-owner))

      (set! new-owner 'reset)
      (test-equal "gnc:owner-from-split (non-business split) return"
        ""
        (gncOwnerGetName
         (gnc:owner-from-split
          (last (xaccAccountGetSplitList (get-acct "Bank-GBP")))
          new-owner)))

      (test-equal "gnc:owner-from-split (non-business split) mutated"
        'reset
        new-owner))

    ))
