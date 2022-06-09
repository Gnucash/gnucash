(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (tests test-engine-extras))
(use-modules (gnucash reports standard invoice))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (gnucash report))
(use-modules (gnucash reports))
(use-modules (tests test-report-extras))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage))
(use-modules (system vm vm))

(define uuid "c146317be32e4948a561ec7fc89d15c1")

(setlocale LC_ALL "C")

(define (run-test)
  (if #f
      (coverage-test run-test-proper)
      (run-test-proper)))

(define (coverage-test tester)
  (let* ((currfile (dirname (current-filename)))
         (path (string-take currfile (string-rindex currfile #\/))))
    (add-to-load-path path))
  (call-with-values
      (lambda()
        (with-code-coverage tester))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test-proper)
  (let ((saved-format (qof-date-format-get)))
    (qof-date-format-set QOF-DATE-FORMAT-ISO)
    (test-runner-factory gnc:test-runner)
    (test-begin "test-new-owner-report")
    (owner-tests)
    (qof-date-format-set saved-format)
    (test-end "test-new-owner-report")))

(define (sxml-get-row-col sxml row col)
  (sxml->table-row-col sxml 3 row col))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (get-currency sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
		     (cons 'commodity (get-currency "EUR"))
                     (cons 'commodity (get-currency "USD")))
	(list "Asset"
	      (list "Bank-EUR"))
        (list "Asset"
              (list "Bank-USD"))
        (list "VAT"
              (list "VAT-on-Purchases")
              (list "VAT-on-Sales" (list (cons 'type ACCT-TYPE-LIABILITY))))
	(list "Income" (list (cons 'type ACCT-TYPE-INCOME))
	      (list "Income-EUR"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
              (list "Income-USD"))
	(list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE))
	      (list "AR-EUR"))
        (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE))
              (list "AR-USD"))
	(list "A/Payable" (list (cons 'type ACCT-TYPE-PAYABLE))
	      (list "AP-EUR")))
        (list "A/Payable" (list (cons 'type ACCT-TYPE-PAYABLE))
              (list "AP-USD"))))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-new-owner-report" test-title))

(define (owner-tests)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (get-acct (lambda (name)
                     (or (assoc-ref account-alist name)
                         (error "invalid account name" name))))
	 (AR (get-acct "AR-EUR"))
         (AR (get-acct "AR-USD"))
	 (Bank (get-acct "Bank-EUR"))
         (Bank (get-acct "Bank-USD"))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define owner-1
      (let* ((coowner-1 (gncCoOwnerCreate (gnc-get-current-book)))
             (owner-1 (gncOwnerNew)))
	(gncCoOwnerSetID coowner-1 "coowner-1-id")
	(gncCoOwnerSetName coowner-1 "coowner-1-name")
	(gncCoOwnerSetNotes coowner-1 "coowner-1-notes")
	(gncCoOwnerSetCurrency coowner-1 (get-currency "EUR"))
	(gncCoOwnerSetTaxIncluded coowner-1 1)
	(gncOwnerInitCoOwner owner-1 coowner-1)
	owner-1))

    (define owner-2
      (let* ((cust-1 (gncCustomerCreate (gnc-get-current-book)))
	     (owner-2 (gncOwnerNew)))
        (gncCustomerSetID cust-1 "cust-1-id")
        (gncCustomerSetName cust-1 "cust-1-name")
        (gncCustomerSetNotes cust-1 "cust-1-notes")
        (gncCustomerSetCurrency cust-1 (get-currency "USD"))
        (gncCustomerSetTaxIncluded cust-1 1)
        (gncOwnerInitCustomer owner-1 cust-1)
        owner-1))

    ;; inv-1 is generated for a coowner
    (define inv-1
      (let ((inv-1 (gncInvoiceCreate (gnc-get-current-book))))
        (gncInvoiceSetOwner inv-1 owner-1)
        (gncInvoiceSetNotes inv-1 "inv-1-notes")
        (gncInvoiceSetBillingID inv-1 "inv-1-billing-id")
	(gncInvoiceSetCurrency inv-1 (get-currency "EUR"))
        inv-1))

    (define (make-entry amt)
      (let ((entry (gncEntryCreate (gnc-get-current-book))))
        (gncEntrySetDateGDate entry (time64-to-gdate (current-time)))
        (gncEntrySetDescription entry "entry-desc")
        (gncEntrySetAction entry "entry-action")
        (gncEntrySetNotes entry "entry-notes")
	(gncEntrySetInvAccount entry (get-acct "Income-EUR"))
        (gncEntrySetDocQuantity entry 1 #f)
        (gncEntrySetInvPrice entry amt)
        entry))

    (define (add-invoice date amount desc)
      (let ((inv (gncInvoiceCopy inv-1)))
        (gncInvoiceAddEntry inv (make-entry amount))
        (gncInvoicePostToAccount inv
				 (get-acct "AR-EUR")         ;post-to acc
                                 date                        ;posted-date
                                 date                        ;due-date
                                 desc                        ;desc
                                 #t                          ;acc-splits?
                                 #f)                         ;autopay
        inv))

    (define (create-split-for-lot account amount lot)
      (let ((split (xaccMallocSplit (gnc-get-current-book))))
        (xaccSplitSetAccount split account)
        (xaccSplitSetAmount split amount)
        (xaccSplitSetValue split amount)
        (xaccSplitSetAction split "")
        (when lot
          (gnc-lot-set-title lot "")
          (gnc-lot-add-split lot split))
        split))

    (define (create-multisplit DD MM YY desc type splits)
      (let* ((book (gnc-get-current-book))
             (txn (xaccMallocTransaction book)))
        (xaccTransBeginEdit txn)
        (xaccTransSetDescription txn desc)
        (xaccTransSetCurrency txn (xaccAccountGetCommodity
                                   (xaccSplitGetAccount (car splits))))
        (xaccTransSetDate txn DD MM YY)
        (xaccTransSetTxnType txn type)
        (for-each (lambda (s) (xaccSplitSetParent s txn)) splits)
        (xaccTransCommitEdit txn)
        txn))

    (define (testing-options-coowner owner account)
      ;; owner-report will run from 1.1.2010 to 1.7.2010
      (let ((options (gnc:make-report-options uuid)))
	(set-option! options "General" "CoOwner" owner)
	(set-option! options "General" "From"
		     (cons 'absolute (gnc-dmy2time64 1 1 2010)))
	(set-option! options "General" "To"
		     (cons 'absolute (gnc-dmy2time64 1 7 2011)))
	(set-option! options "Display Columns" "Transaction Links" 'detailed)
	options))

    ;; inv €10,55
    (add-invoice (gnc-dmy2time64 13 05 2010) 27/4 "€10.55")

    ;; inv $50.99, 2 payments
    (let ((inv (add-invoice (gnc-dmy2time64 13 01 2010) 23/2 "$50.99")))
      (gncInvoiceApplyPayment inv '() Bank 3/2 1
			      (gnc-dmy2time64 18 03 2010)
			      "inv >90 payment" "pay only $3.50")

      (gncInvoiceApplyPayment inv '() Bank 2 1
			      (gnc-dmy2time64 20 03 2010)
			      "inv >90 payment" "pay only $5.00"))

    ;; CN €3.00
    (let ((new-cn (add-invoice (gnc-dmy2time64 22 06 2010) -3 "CN")))
      (gncInvoiceSetIsCreditNote new-cn #t))

    (display "new-owner-report tests:\n")
    (test-begin "new-owner-report")
    (let* ((options (testing-options-coowner owner-1 (get-acct "AR-EUR")))
	   (sxml (options->sxml options "new-owner-report basic")))
      (test-equal "line 1"
	'("2010-01-13" "2010-01-13" "Invoice" "€50,99" "€50,99" "2010-03-18"
	  "Payment" "inv >90 payment" "€50,99" "pay only €3,50" "-€3,50" "-€3,50")
	((sxpath `(html body (table 3) tbody (tr 1) // *text*)) sxml))
      (test-equal "line 2"
	'("2010-03-20" "Payment" "inv >90 payment" "pay only €5.00" "-€5.00" "-€5.00")
	((sxpath `(// (table 3) // tbody // (tr 2) // *text*)) sxml))
      )
    (test-end "new-owner-report")))

    ;; inv-2 is generated for a customer
    (define inv-2
      (let ((inv-2 (gncInvoiceCreate (gnc-get-current-book))))
	(gncInvoiceSetOwner inv-2 owner-2)
	(gncInvoiceSetNotes inv-2 "inv-2-notes")
	(gncInvoiceSetBillingID inv-2 "inv-2-billing-id")
	(gncInvoiceSetCurrency inv-2 (get-currency "USD"))
	inv-2))

    (define (make-entry amt)
      (let ((entry (gncEntryCreate (gnc-get-current-book))))
	(gncEntrySetDateGDate entry (time64-to-gdate (current-time)))
	(gncEntrySetDescription entry "entry-desc")
	(gncEntrySetAction entry "entry-action")
	(gncEntrySetNotes entry "entry-notes")
	(gncEntrySetInvAccount entry (get-acct "Income-USD"))
	(gncEntrySetDocQuantity entry 1 #f)
	(gncEntrySetInvPrice entry amt)
	entry))

    (define (add-invoice date amount desc)
      (let ((inv (gncInvoiceCopy inv-2)))
	(gncInvoiceAddEntry inv (make-entry amount))
	(gncInvoicePostToAccount inv
				 (get-acct "AR-USD")         ;post-to acc
				 date                        ;posted-date
				 date                        ;due-date
				 desc                        ;desc
				 #t                          ;acc-splits?
				 #f)                         ;autopay
	inv))

    (define (testing-options-customer owner account)
      ;; owner-report will run from 1.1.1980 to 1.7.1980
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "General" "Customer" owner)
        (set-option! options "General" "From"
                     (cons 'absolute (gnc-dmy2time64 1 1 1980)))
        (set-option! options "General" "To"
                     (cons 'absolute (gnc-dmy2time64 1 7 1981)))
        (set-option! options "Display Columns" "Transaction Links" 'detailed)
        options))

    ;; inv $6.75
    (add-invoice (gnc-dmy2time64 13 05 1980) 27/4 "$6.75")

    ;; inv $11.50, 2 payments
    (let ((inv (add-invoice (gnc-dmy2time64 13 01 1980) 23/2 "$11.50")))
      (gncInvoiceApplyPayment inv '() Bank 3/2 1
                              (gnc-dmy2time64 18 03 1980)
                              "inv >90 payment" "pay only $1.50")

      (gncInvoiceApplyPayment inv '() Bank 2 1
                              (gnc-dmy2time64 20 03 1980)
                              "inv >90 payment" "pay only $2.00"))

    ;; CN $3.00
    (let ((new-cn (add-invoice (gnc-dmy2time64 22 06 1980) -3 "CN")))
      (gncInvoiceSetIsCreditNote new-cn #t))

    ;; inv $28, CN $27, Bank $1
    (let* ((inv (add-invoice (gnc-dmy2time64 24 06 1980) 28 "$28.00"))
           (CN (add-invoice (gnc-dmy2time64 25 06 1980) -27 "$27.00"))
           (inv-lot (gncInvoiceGetPostedLot inv))
           (CN-lot (gncInvoiceGetPostedLot CN)))
      (gncInvoiceSetIsCreditNote CN #t)

      (create-multisplit
       26 06 1980 "payment" TXN-TYPE-PAYMENT
       (list (create-split-for-lot AR -1 inv-lot)
             (create-split-for-lot Bank 1 #f)))

      (create-multisplit
       26 06 1980 "payment" TXN-TYPE-LINK
       (list (create-split-for-lot AR -27 inv-lot)
             (create-split-for-lot AR 27 CN-lot))))

    ;; refund $120 to partially repay
    (let ((lot1 (gnc-lot-new (gnc-get-current-book)))
          (lot2 (gnc-lot-new (gnc-get-current-book))))

      (gncOwnerAttachToLot owner-1 lot1)
      (gncOwnerAttachToLot owner-1 lot2)

      (create-multisplit
       28 06 1980 "payment" TXN-TYPE-PAYMENT
       (list (create-split-for-lot AR  -120 lot1)
             (create-split-for-lot AR   -40 lot2)
             (create-split-for-lot Bank 160 #f)))

      (create-multisplit
       29 06 1980 "payment" TXN-TYPE-PAYMENT
       (list (create-split-for-lot AR    120 lot1)
             (create-split-for-lot Bank -120 #f)))

      (create-multisplit
       30 06 1980 "payment" TXN-TYPE-PAYMENT
       (list (create-split-for-lot AR    50 lot2)
             (create-split-for-lot Bank -50 #f))))

    (display "new-owner-report tests:\n")
    (test-begin "new-customer-report")
    (let* ((options (testing-options-customer owner-2 (get-acct "AR-USD")))
           (sxml (options->sxml options "new-customer-report basic")))
      (test-equal "line 1"
        '("1980-01-13" "1980-01-13" "Invoice" "$11.50" "$11.50" "1980-03-18"
          "Payment" "inv >90 payment" "$11.50" "pay only $1.50" "-$1.50" "-$1.50")
        ((sxpath `(html body (table 3) tbody (tr 1) // *text*)) sxml))
      (test-equal "line 2"
        '("1980-03-20" "Payment" "inv >90 payment" "pay only $2.00" "-$2.00" "-$2.00")
        ((sxpath `(// (table 3) // tbody // (tr 2) // *text*)) sxml))
      (test-equal "line 3"
        '("UNPAID" "$8.00")
        ((sxpath `(// (table 3) // (tr 3) // *text*)) sxml))
      (test-equal "line 4"
        '("1980-03-18" "Payment" "inv >90 payment" "$10.00" "1980-01-13"
          "Invoice" "inv >90 payment" "pay only $1.50" "$1.50" "$1.50" "$11.50")
        ((sxpath `(// (table 3) // (tr 4) // *text*)) sxml))
      (test-equal "line 5"
        '("1980-03-20" "Payment" "inv >90 payment" "$8.00" "1980-01-13"
          "Invoice" "inv >90 payment" "pay only $2.00" "$2.00" "$2.00" "$11.50")
        ((sxpath `(// (table 3) // (tr 5) // *text*)) sxml))
      (test-equal "line 6"
        '("1980-05-13" "1980-05-13" "Invoice" "$6.75" "$14.75" "UNPAID"
          "$6.75" "$6.75")
        ((sxpath `(// (table 3) // (tr 6) // *text*)) sxml))
      (test-equal "line 7"
        '("1980-06-22" "1980-06-22" "Credit Note" "CN" "$11.75" "UNPAID"
          "-$3.00" "$3.00")
        ((sxpath `(// (table 3) // (tr 7) // *text*)) sxml))
      (test-equal "line 8"
        '("1980-06-24" "1980-06-24" "Invoice" "$28.00" "$39.75" "1980-06-26"
          "Payment" "$28.00" "-$1.00" "-$1.00")
        ((sxpath `(// (table 3) // (tr 8) // *text*)) sxml))
      (test-equal "line 9"
        ' ("1980-06-25" "Credit Note" "$27.00" "-$27.00" "-$27.00")
        ((sxpath `(// (table 3) // (tr 9) // *text*)) sxml))
      (test-equal "line 10"
        '("1980-06-25" "1980-06-25" "Credit Note" "$27.00" "$12.75"
          "1980-06-24" "Invoice" "$28.00" "$27.00" "$27.00" "$28.00")
        ((sxpath `(// (table 3) // (tr 10) // *text*)) sxml))
      (test-equal "line 11"
        '("1980-06-26" "Payment" "$11.75" "1980-06-24" "Invoice" "$1.00"
          "$1.00" "$28.00")
        ((sxpath `(// (table 3) // (tr 11) // *text*)) sxml))

      ;; tests for refund $120 to partially repay
      (test-equal "line 12 refund $120 to partially repay"
        '("1980-06-28" "Payment" "-$148.25" "1980-06-30" "Refund"
          "$160.00" "-$40.00" "$50.00")
        ((sxpath `(// (table 3) // (tr 12) // *text*)) sxml))
      (test-equal "line 13 refund $120 to partially repay"
        '("1980-06-29" "Refund" "-$120.00" "$120.00")
        ((sxpath `(// (table 3) // (tr 13) // *text*)) sxml))
      (test-equal "line 14 refund $120 to partially repay"
        '("1980-06-29" "Refund" "-$28.25" "1980-06-28"
          "Payment" "$120.00" "$120.00" "-$120.00")
        ((sxpath `(// (table 3) // (tr 14) // *text*)) sxml))
      (test-equal "line 15 refund $120 to partially repay"
        '("1980-06-30" "Refund" "$21.75" "1980-06-28" "Payment"
          "* " "$50.00" "$40.00" "-$40.00")
        ((sxpath `(// (table 3) // (tr 15) // *text*)) sxml))
      (test-equal "line 16 refund $120 to partially repay"
        '("Pre-Payment" "$10.00")
        ((sxpath `(// (table 3) // (tr 16) // *text*)) sxml))
      (test-equal "line 17 refund $120 to partially repay"
        '("Period Totals" "$216.25" "$194.50" "$21.75"
          "* Amounts denoted thus are derived from, and do not match the transaction.")
        ((sxpath `(// (table 3) // (tr 17) // *text*)) sxml))

      )
    (test-end "new-customer-report")))
