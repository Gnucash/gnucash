(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report invoice))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage))
(use-modules (system vm vm))

(define uuid-list
  (list (cons 'invoice "5123a759ceb9483abf2182d01c140e8d")
        (cons 'fancy-invoice "3ce293441e894423a2425d7a22dd1ac6")
        (cons 'easy-invoice "67112f318bef4fc496bdc27d106bbda4")))

(setlocale LC_ALL "C")

(define (run-test)
  (if #f
      (coverage-test run-test-proper)
      (run-test-proper)))

(define (coverage-test tester)
  (add-to-load-path "/home/chris/sources/gnucash/gnucash/report/business-reports")
  (call-with-values
      (lambda()
        (with-code-coverage tester))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test-proper)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-invoice.scm")
  (inv-tests 'invoice)
  (inv-tests 'easy-invoice)
  (inv-tests 'fancy-invoice)
  (test-end "test-invoice.scm"))

(define (sxml-main-get-row-col sxml row col)
  (sxml->table-row-col sxml 3 row col))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                     (cons 'commodity (gnc-default-report-currency)))
        (list "Asset"
              (list "Bank"))
        (list "VAT"
              (list "VAT-on-Purchases")
              (list "VAT-on-Sales" (list (cons 'type ACCT-TYPE-LIABILITY))))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE)))))

(define (inv-tests variant)
  ;; This function will perform implementation testing on the printable invoice.
  (define uuid (cdr (assq variant uuid-list)))
  (define (options->sxml options test-title)
    (gnc:options->sxml uuid options (format #f "test-~a" variant) test-title))

  (format #t "\n\n**** starting tests for variant ~a ****\n\n" variant)

  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (vat-sales (cdr (assoc "VAT-on-Sales" account-alist)))
         (vat-purchases (cdr (assoc "VAT-on-Purchases" account-alist)))
         (receivable (cdr (assoc "A/Receivable" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today)))

         (cust-1 (let ((cust-1 (gncCustomerCreate (gnc-get-current-book))))
                   (gncCustomerSetID cust-1 "cust-1-id")
                   (gncCustomerSetName cust-1 "cust-1-name")
                   (gncCustomerSetNotes cust-1 "cust-1-notes")
                   (gncCustomerSetCurrency cust-1 (gnc-default-report-currency))
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
                  inv-2))

         (vend-1 (let ((vend-1 (gncVendorCreate (gnc-get-current-book))))
                   (gncVendorSetID vend-1 "vend-1-id")
                   (gncVendorSetName vend-1 "vend-1-name")
                   (gncVendorSetNotes vend-1 "vend-1-notes")
                   (gncVendorSetCurrency vend-1 (gnc-default-report-currency))
                   (gncVendorSetTaxIncluded vend-1 1) ;1 = GNC-TAXINCLUDED-YES
                   vend-1))

         (owner-3 (let ((owner-3 (gncOwnerNew)))
                    (gncOwnerInitVendor owner-3 vend-1)
                    owner-3))

         ;; inv-3 is generated from a vendor
         (inv-3 (let ((inv-3 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-3 owner-3)
                  (gncInvoiceSetNotes inv-3 "inv-3-notes")
                  inv-3))

         (emp-1 (let ((emp-1 (gncEmployeeCreate (gnc-get-current-book))))
                  (gncEmployeeSetID emp-1 "emp-1-id")
                  (gncEmployeeSetCurrency emp-1 (gnc-default-report-currency))
                  (gncEmployeeSetName emp-1 "emp-1-name")
                  emp-1))

         (owner-4 (let ((owner-4 (gncOwnerNew)))
                    (gncOwnerInitEmployee owner-4 emp-1)
                    owner-4))

         ;; inv-4 is generated for an employee
         (inv-4 (let ((inv-4 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-4 owner-4)
                  (gncInvoiceSetNotes inv-4 "inv-4-notes")
                  inv-4))

         ;; inv-5 cust-credit-note
         (inv-5 (let ((inv-5 (gncInvoiceCopy inv-1)))
                  (gncInvoiceSetIsCreditNote inv-5 #t)
                  inv-5))

         ;; inv-6 vend-credit-note
         (inv-6 (let ((inv-6 (gncInvoiceCopy inv-3)))
                  (gncInvoiceSetIsCreditNote inv-6 #t)
                  inv-6))

         ;; inv-7 emp-credit-note
         (inv-7 (let ((inv-7 (gncInvoiceCopy inv-4)))
                  (gncInvoiceSetIsCreditNote inv-7 #t)
                  inv-7))

         (standard-vat-sales-tt (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
                                  (gncTaxTableIncRef tt)
                                  (gncTaxTableSetName tt "10% vat on sales")
                                  (let ((entry (gncTaxTableEntryCreate)))
                                    (gncTaxTableEntrySetAccount entry vat-sales)
                                    (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
                                    (gncTaxTableEntrySetAmount entry 10)
                                    (gncTaxTableAddEntry tt entry))
                                  tt))

         (standard-vat-purchases-tt (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
                                      (gncTaxTableIncRef tt)
                                      (gncTaxTableSetName tt "10% vat on purchases")
                                      (let ((entry (gncTaxTableEntryCreate)))
                                        (gncTaxTableEntrySetAccount entry vat-purchases)
                                        (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
                                        (gncTaxTableEntrySetAmount entry 10)
                                        (gncTaxTableAddEntry tt entry))
                                      tt)))

    (define* (default-testing-options inv #:optional (setting #t))
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "General" "Invoice Number" inv)
        (for-each
         (lambda (disp-col-name)
           (set-option! options "Display Columns" disp-col-name setting))
         (case variant
           ((invoice fancy-invoice)
            '("Date" "Description" "Action" "Quantity" "Price" "Discount"
              "Taxable" "Tax Amount" "Total"))
           ((easy-invoice)
            '("Date" "Description" "Charge Type" "Quantity"
              "Price" "Discount" "Taxable" "Tax Amount" "Total"))))
        (for-each
         (lambda (disp-col-name)
           (set-option! options "Display" disp-col-name setting))
         (case variant
           ((invoice)
            '("Individual Taxes" "Totals" "References" "Billing Terms"
              "Billing ID" "Invoice Notes" "Payments" "Job Details"))
           ((fancy-invoice)
            '("Individual Taxes" "Totals" "References" "Billing Terms"
              "Billing ID" "Invoice Notes" "Payments"))
           ((easy-invoice)
            '("My Company" "My Company ID" "Due Date"
              "Individual Taxes" "Totals" "Subtotal" "References"
              "Billing Terms" "Billing ID" "Invoice Notes"
              "Payments"))))
        options))

    ;; entry-1  2 widgets of $3 = $6
    (let ((entry-1 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-1 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-1 "entry-1-desc")
      (gncEntrySetAction entry-1 "entry-1-action")
      (gncEntrySetNotes entry-1 "entry-1-notes")
      (gncEntrySetInvAccount entry-1 income)
      (gncEntrySetDocQuantity entry-1 2 #f)
      (gncEntrySetInvPrice entry-1 3)
      (gncInvoiceAddEntry inv-1 entry-1))

    (test-begin "inv-1 simple entry")
    (let* ((options (default-testing-options inv-1))
           (sxml (options->sxml options "inv-1 simple entry")))
      (test-equal "inv-1 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-1 simple entry details are correct"
        '("entry-1-desc" "entry-1-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-1 cust-name is correct"
          '("cust-1-name")
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-1-billing-id is in invoice body"
        (member
         (case variant
           ((invoice fancy-invoice) "Reference:\xa0inv-1-billing-id")
           ((easy-invoice) "Billing ID:\xa0inv-1-billing-id"))
         ((sxpath '(// body // *text*)) sxml)))
      (test-assert "inv-1 inv-notes is in invoice body"
        (member
         "inv-1-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-1 simple entry")

    (test-begin "inv-1 simple entry, sparse options")
    (let* ((options (default-testing-options inv-1 #f))
           (sxml (options->sxml options "inv-1 simple entry sparse")))
      (test-equal "inv-1 sparse simple entry headers are correct"
        (case variant
          ((invoice) '("Net Price" "Tax" "Total Price" "Amount Due"))
          ((fancy-invoice) '("Net Price" "Tax" "Total\xa0Price" "Amount\xa0Due"))
          ((easy-invoice) '("Tax" "Total Price" "Amount Due")))
        (sxml-main-get-row-col sxml #f 1))
      (test-equal "inv-1 sparse simple entry amounts are correct"
        (case variant
          ((invoice fancy-invoice) '("$6.00" "$0.00" "$6.00" "$6.00"))
          ((easy-invoice) '("$0.00" "$6.00" "$6.00")))
        (sxml-main-get-row-col sxml #f -1)))
    (test-end "inv-1 simple entry, sparse options")

    (test-begin "inv-2")
    (let ((entry-2 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-2 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-2 "entry-2-desc")
      (gncEntrySetAction entry-2 "entry-2-action")
      (gncEntrySetNotes entry-2 "entry-2-notes")
      (gncEntrySetInvAccount entry-2 income)
      (gncEntrySetInvTaxable entry-2 #f)
      (gncEntrySetDocQuantity entry-2 5 #f)
      (gncEntrySetInvPrice entry-2 11)
      (gncEntrySetInvDiscount entry-2 10)
      (gncInvoiceAddEntry inv-1 entry-2))
    ;; entry-inv-2  2 widgets of $3 = $6
    (let ((entry-inv-2 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-2 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-2 "entry-inv-2-desc")
      (gncEntrySetAction entry-inv-2 "entry-inv-2-action")
      (gncEntrySetNotes entry-inv-2 "entry-inv-2-notes")
      (gncEntrySetInvAccount entry-inv-2 income)
      (gncEntrySetDocQuantity entry-inv-2 2 #f)
      (gncEntrySetInvPrice entry-inv-2 3)
      (gncInvoiceAddEntry inv-2 entry-inv-2))
    (let* ((options (default-testing-options inv-2))
           (sxml (options->sxml options "inv-2 simple entry")))
      (test-equal "inv-2 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-2 simple entry details are correct"
        '("entry-inv-2-desc" "entry-inv-2-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-2 cust-name is correct"
          '("cust-1-name")
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-2 inv-notes is in invoice body"
        (member
         "inv-2-notes"
         ((sxpath '(// body // *text*)) sxml)))
      (when (eq? variant 'invoice)
        (test-assert "inv-2 jobnumber is in invoice body"
          (member
           "Job number:\xa0job-1-id"
           ((sxpath '(// body // *text*)) sxml)))
        (test-assert "inv-2 jobname is in invoice body"
          (member
           "Job name:\xa0job-1-name"
           ((sxpath '(// body // *text*)) sxml))))
      )
    (test-end "inv-2")

    (test-begin "inv-3")
    ;; entry-inv-3  2 widgets of $3 = $6
    (let ((entry-inv-3 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-3 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-3 "entry-inv-3-desc")
      (gncEntrySetAction entry-inv-3 "entry-inv-3-action")
      (gncEntrySetNotes entry-inv-3 "entry-inv-3-notes")
      (gncEntrySetInvAccount entry-inv-3 income)
      (gncEntrySetDocQuantity entry-inv-3 2 #f)
      (gncEntrySetBillPrice entry-inv-3 3)
      (gncInvoiceAddEntry inv-3 entry-inv-3))
    (let* ((options (default-testing-options inv-3))
           (sxml (options->sxml options "inv-3 simple entry")))
      (test-equal "inv-3 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-3 simple entry details are correct"
        '("entry-inv-3-desc" "entry-inv-3-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-3 vend-name is correct"
          '("vend-1-name")
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-3 inv-notes is in invoice body"
        (member
         "inv-3-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-3")


    (test-begin "inv-4")
    ;; entry-inv-4  2 widgets of $3 = $6
    (let ((entry-inv-4 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-4 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-4 "entry-inv-4-desc")
      (gncEntrySetAction entry-inv-4 "entry-inv-4-action")
      (gncEntrySetNotes entry-inv-4 "entry-inv-4-notes")
      (gncEntrySetInvAccount entry-inv-4 income)
      (gncEntrySetDocQuantity entry-inv-4 2 #f)
      (gncEntrySetBillPrice entry-inv-4 3)
      (gncInvoiceAddEntry inv-4 entry-inv-4))
    (let* ((options (default-testing-options inv-4))
           (sxml (options->sxml options "inv-4 simple entry")))
      (test-equal "inv-4 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml->table-row-col sxml 3 #f -1))
      (test-equal "inv-4 simple entry details are correct"
        '("entry-inv-4-desc" "entry-inv-4-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml->table-row-col sxml 3 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-4 vend-name is correct"
          '("emp-1-name" "emp-1-name")    ;FIXME: why is this duplicated????
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-4 inv-notes is in invoice body"
        (member
         "inv-4-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-4")

    (test-begin "inv-5 simple entry")
    ;; entry-5  2 widgets of $3 = $6
    (let ((entry-5 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-5 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-5 "entry-5-desc")
      (gncEntrySetAction entry-5 "entry-5-action")
      (gncEntrySetNotes entry-5 "entry-5-notes")
      (gncEntrySetInvAccount entry-5 income)
      (gncEntrySetDocQuantity entry-5 2 #t)
      (gncEntrySetInvPrice entry-5 3)
      (gncInvoiceAddEntry inv-5 entry-5))
    (let* ((options (default-testing-options inv-5))
           (sxml (options->sxml options "inv-5 simple entry")))
      (test-equal "inv-5 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-5 simple entry details are correct"
        '("entry-5-desc" "entry-5-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-5 cust-name is correct"
          '("cust-1-name")
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml))))
    (test-end "inv-5 simple entry")

    (test-begin "inv-6")
    (let ((entry-inv-6 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-6 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-6 "entry-inv-6-desc")
      (gncEntrySetAction entry-inv-6 "entry-inv-6-action")
      (gncEntrySetNotes entry-inv-6 "entry-inv-6-notes")
      (gncEntrySetInvAccount entry-inv-6 income)
      (gncEntrySetDocQuantity entry-inv-6 2 #t)
      (gncEntrySetBillPrice entry-inv-6 3)
      (gncInvoiceAddEntry inv-6 entry-inv-6))
    (let* ((options (default-testing-options inv-6))
           (sxml (options->sxml options "inv-6 simple entry")))
      (test-equal "inv-6 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-6 simple entry details are correct"
        '("entry-inv-6-desc" "entry-inv-6-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-6 vend-name is correct"
          '("vend-1-name")
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-6 inv-3-notes is in invoice body"
        (member
         "inv-3-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-6")

    (test-begin "inv-7")
    ;; entry-inv-7  2 widgets of $3 = $6
    (let ((entry-inv-7 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-7 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-7 "entry-inv-7-desc")
      (gncEntrySetAction entry-inv-7 "entry-inv-7-action")
      (gncEntrySetNotes entry-inv-7 "entry-inv-7-notes")
      (gncEntrySetInvAccount entry-inv-7 income)
      (gncEntrySetDocQuantity entry-inv-7 2 #t)
      (gncEntrySetBillPrice entry-inv-7 3)
      (gncInvoiceAddEntry inv-7 entry-inv-7))
    (let* ((options (default-testing-options inv-7))
           (sxml (options->sxml options "inv-7 simple entry")))
      (test-equal "inv-7 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-main-get-row-col sxml #f -1))
      (test-equal "inv-7 simple entry details are correct"
        '("entry-inv-7-desc" "entry-inv-7-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-main-get-row-col sxml 1 #f)))
      (unless (eq? variant 'fancy-invoice)
        (test-equal "inv-7 vend-name is correct"
          '("emp-1-name" "emp-1-name")    ;FIXME: why is this duplicated????
          ((sxpath '(// (table 2) // tbody // tr // td // *text*))
           sxml)))
      (test-assert "inv-7 inv-4-notes is in invoice body"
        (member
         "inv-4-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-7")

    (test-begin "combinations of gncEntry options")
    (let* ((inv-8 (gncInvoiceCreate (gnc-get-current-book)))
           (taxrate 109/10)
           (discount 7/2)
           (unitprice 777/4)
           (quantity 11)
           (combo-vat-sales-tt (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
                                 (gncTaxTableIncRef tt)
                                 (gncTaxTableSetName tt (format #f "~a% vat on sales" taxrate))
                                 (let ((entry (gncTaxTableEntryCreate)))
                                   (gncTaxTableEntrySetAccount entry vat-sales)
                                   (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
                                   (gncTaxTableEntrySetAmount entry taxrate)
                                   (gncTaxTableAddEntry tt entry))
                                 tt))
           (order (let ((order (gncOrderCreate (gnc-get-current-book))))
                    (gncOrderSetID order "order-id")
                    (gncOrderSetOwner order owner-1)
                    (gncOrderSetReference order "order-ref")
                    (gncOrderSetActive order #t)
                    order))
           (billterm (let ((term (gncBillTermCreate (gnc-get-current-book))))
                       (gncBillTermSetName term "billterm-name")
                       (gncBillTermSetDescription term "billterm-desc")
                       (gncBillTermSetType term 1) ;1 = GNC-TERM-TYPE-DAYS
                       (gncBillTermSetDueDays term 8)
                       term)))
      (gncInvoiceSetOwner inv-8 owner-1)
      (gncInvoiceSetCurrency inv-8 (gnc-default-report-currency))
      (gncInvoiceSetTerms inv-8 billterm)
      (for-each
       (lambda (combo)
         (let* ((each-entry (gncEntryCreate (gnc-get-current-book)))
                (taxable? (= (vector-ref combo 0) 1))
                (tax-included? (= (vector-ref combo 1) 1))
                (discount-type (vector-ref combo 2))
                (discount-how (vector-ref combo 3))
                (desc (format #f "taxable=~a tax-included=~a discount-type=~a discount-how=~a"
                              (if taxable? "Y" "N")
                              (if tax-included? "Y" "N")
                              (gncAmountTypeToString discount-type)
                              (gncEntryDiscountHowToString discount-how))))
           (gncEntrySetDateGDate each-entry (time64-to-gdate (current-time)))
           (gncEntrySetDescription each-entry desc)
           (gncEntrySetAction each-entry "action")
           (gncEntrySetInvAccount each-entry income)
           (gncEntrySetDocQuantity each-entry quantity #f)
           (gncEntrySetInvPrice each-entry unitprice)
           (gncEntrySetInvDiscount each-entry discount)
           (gncEntrySetInvDiscountType each-entry discount-type)
           (gncEntrySetInvDiscountHow each-entry discount-how)
           (gncEntrySetInvTaxable each-entry taxable?)
           (gncEntrySetInvTaxIncluded each-entry tax-included?)
           (gncEntrySetInvTaxTable each-entry combo-vat-sales-tt)
           ;; FIXME: Note: The following function hides a subtle
           ;; bug. It aims to retrieve & dump the entry description
           ;; and amount. Unfortunately the (gncEntryGetDocValue)
           ;; function will subtly modify the entry amounts by a
           ;; fraction; this means that the subsequent invoice payment
           ;; will not make the invoice amount completely zero. If the
           ;; following statement is uncommented, the invoice
           ;; generated will not change, however, the test will fail
           ;; because the (gncInvoiceIsPaid) final test will fail.

           ;; (format #t "inv-8: adding ~a to invoice, entry amount is ~a\n"
           ;;         desc
           ;;         (exact->inexact (gncEntryGetDocValue each-entry #f #t #f)))
           (gncOrderAddEntry order each-entry)
           (gncInvoiceAddEntry inv-8 each-entry)))
       (list
        ;; the following list specifies combinations to test gncEntry options
        ;; thanks to rgmerk and to jenny for idea how to generate list of options
        ;; (vector Taxable?(1=#t) Tax-included?(1=#t) DiscountType DiscountHow)
        (vector 1 2 1 1)
        (vector 2 1 2 2)
        (vector 1 1 2 3)
        (vector 2 2 1 3)
        (vector 2 1 1 1)
        (vector 1 2 2 2)
        (vector 1 2 1 2)
        (vector 1 1 2 1)))
      (gncInvoiceSetNotes inv-8 (format #f "tax=~a%, discount=~a, qty=~a, price=~a" taxrate discount quantity unitprice))

      (gncInvoicePostToAccount inv-8 receivable (current-time)
                               (current-time) "trans-posting-memo"
                               #t #f)

      (gncInvoiceApplyPayment inv-8 '() bank 1747918/100 1
                              (current-time) "trans-payment-memo-1" "trans-payment-num-1")
      (let* ((options (default-testing-options inv-8))
             (sxml (options->sxml options "inv-8 combinatorics")))
        (test-assert "inv-8 billterm-desc is in invoice body"
          (member
           "Terms:\xa0billterm-desc"
           ((sxpath '(// body // *text*)) sxml)))
        (test-assert "inv-8 gncOrder reference is in invoice body"
          (member
           "REF:\xa0order-ref"
           ((sxpath '(// body // *text*)) sxml)))
        (case variant
          ((invoice)
           (test-equal "inv-8 invoice date is in invoice body"
             '("Invoice Date:\xa0")
             (sxml->table-row-col sxml 2 1 1))
           (test-equal "inv-8 due date is in invoice body"
             '("Due Date:\xa0")
             (sxml->table-row-col sxml 2 2 1)))
          ((easy-invoice)
           (test-equal "inv-8 invoice date is in invoice body"
             '("Date:\xa0")
             (sxml->table-row-col sxml 3 1 1))
           (test-equal "inv-8 invoice date is in invoice body"
             '("Due:\xa0")
             (sxml->table-row-col sxml 3 2 1))))
        (test-equal "inv-8 combo amounts are correct"
          '("$2,133.25" "$2,061.96" "$2,133.25" "$2,061.96" "$2,133.25" "$2,133.25"
            "$1,851.95" "$1,859.30" "$16,368.17" "$1,111.01" "$17,479.18"
            "-$17,479.18" "$0.00")
          (if (eq? variant 'fancy-invoice)
              (sxml->table-row-col sxml 3 #f -1)
              (sxml->table-row-col sxml 4 #f -1)))
        (test-assert "inv-8 is fully paid up!"
          (gncInvoiceIsPaid inv-8))))
    (test-end "combinations of gncEntry options")))
