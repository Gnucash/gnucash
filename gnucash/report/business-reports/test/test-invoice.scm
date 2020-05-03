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
  (test-runner-factory gnc:test-runner)
  (test-begin "test-invoice.scm")
  (inv-tests 'invoice)
  (inv-tests 'easy-invoice)
  (inv-tests 'fancy-invoice)
  (test-end "test-invoice.scm"))

(define (sxml-get-row-col classname sxml row col)
  (sxml->table-row-col
   ((sxpath `(// (div (@ (equal? (class ,classname))))))
    sxml)
   1 row col))

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
         (invoices (create-test-invoice-data))
         (inv-1 (vector-ref invoices 0))
         (inv-2 (vector-ref invoices 1))
         (inv-3 (vector-ref invoices 2))
         (inv-4 (vector-ref invoices 3))
         (inv-5 (vector-ref invoices 4))
         (inv-6 (vector-ref invoices 5))
         (inv-7 (vector-ref invoices 6))
         (inv-8 (vector-ref invoices 7)))

    (define* (default-testing-options inv #:optional (setting #t))
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "General" "Invoice Number" inv)
        (for-each
         (lambda (disp-col-name)
           (set-option! options "Display Columns" disp-col-name setting))
         '("Date" "Description" "Action" "Quantity" "Price" "Discount"
           "Taxable" "Tax Amount" "Total"))
        (for-each
         (lambda (disp-col-name)
           (set-option! options "Display" disp-col-name setting))
         '("Due Date" "Use Detailed Tax Summary" "Totals" "Subtotal"
           "References" "Billing Terms" "Billing ID" "Invoice Notes"
           "Payments" "Job Details"))
        options))

    (test-begin "inv-1 simple entry")
    (let* ((options (default-testing-options inv-1))
           (sxml (options->sxml options "inv-1 simple entry")))
      (test-equal "inv-1 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-1 simple entry details are correct"
        '("entry-1-desc" "entry-1-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-1 cust-name is correct"
        '("cust-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-1-billing-id is in invoice body"
        (member
         "inv-1-billing-id"
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
        '("Tax" "Total Price" "Amount Due")
        (sxml-get-row-col "entries-table" sxml #f 1))
      (test-equal "inv-1 sparse simple entry amounts are correct"
        '("$0.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1)))
    (test-end "inv-1 simple entry, sparse options")

    (test-begin "inv-2")
    (let* ((options (default-testing-options inv-2))
           (sxml (options->sxml options "inv-2 simple entry")))
      (test-equal "inv-2 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-2 simple entry details are correct"
        '("entry-inv-2-desc" "entry-inv-2-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-2 cust-name is correct"
        '("cust-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-2 inv-notes is in invoice body"
        (member
         "inv-2-notes"
         ((sxpath '(// body // *text*)) sxml)))
      (test-assert "inv-2 jobnumber is in invoice body"
        (member
         "job-1-id"
         ((sxpath '(// body // *text*)) sxml)))
      (test-assert "inv-2 jobname is in invoice body"
        (member
         "job-1-name"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-2")

    (test-begin "inv-3")
    (let* ((options (default-testing-options inv-3))
           (sxml (options->sxml options "inv-3 simple entry")))
      (test-equal "inv-3 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-3 simple entry details are correct"
        '("entry-inv-3-desc" "entry-inv-3-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-3 vend-name is correct"
        '("vend-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-3 inv-notes is in invoice body"
        (member
         "inv-3-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-3")

    (test-begin "inv-4")
    (let* ((options (default-testing-options inv-4))
           (sxml (options->sxml options "inv-4 simple entry")))
      (test-equal "inv-4 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-4 simple entry details are correct"
        '("entry-inv-4-desc" "entry-inv-4-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-4 vend-name is correct"
        '("emp-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-4 inv-notes is in invoice body"
        (member
         "inv-4-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-4")

    (test-begin "inv-5 simple entry")
    (let* ((options (default-testing-options inv-5))
           (sxml (options->sxml options "inv-5 simple entry")))
      (test-equal "inv-5 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-5 simple entry details are correct"
        '("entry-5-desc" "entry-5-action" "2.00" "$3.00" "0.00 %" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-5 cust-name is correct"
        '("cust-1-name")
        (sxml-get-row-col "client-table" sxml 1 1)))
    (test-end "inv-5 simple entry")

    (test-begin "inv-6")
    (let* ((options (default-testing-options inv-6))
           (sxml (options->sxml options "inv-6 simple entry")))
      (test-equal "inv-6 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-6 simple entry details are correct"
        '("entry-inv-6-desc" "entry-inv-6-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-6 vend-name is correct"
        '("vend-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-6 inv-3-notes is in invoice body"
        (member
         "inv-3-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-6")

    (test-begin "inv-7")
    (let* ((options (default-testing-options inv-7))
           (sxml (options->sxml options "inv-7 simple entry")))
      (test-equal "inv-7 simple entry amounts are correct"
        '("$6.00" "$6.00" "$6.00" "$6.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-equal "inv-7 simple entry details are correct"
        '("entry-inv-7-desc" "entry-inv-7-action" "2.00" "$3.00" "T" "$0.00" "$6.00")
        (cdr (sxml-get-row-col "entries-table" sxml 1 #f)))
      (test-equal "inv-7 vend-name is correct"
        '("emp-1-name")
        (sxml-get-row-col "client-table" sxml 1 1))
      (test-assert "inv-7 inv-4-notes is in invoice body"
        (member
         "inv-4-notes"
         ((sxpath '(// body // *text*)) sxml))))
    (test-end "inv-7")

    (test-begin "combinations of gncEntry options")
    (let* ((options (default-testing-options inv-8))
           (sxml (options->sxml options "inv-8 combinatorics")))
      (test-assert "inv-8 billterm-desc is in invoice body"
        (member
         "billterm-desc"
         ((sxpath '(// body // *text*)) sxml)))
      (test-assert "inv-8 gncOrder reference is in invoice body"
        (member
         "REF order-ref"
         ((sxpath '(// body // *text*)) sxml)))
      (test-equal "inv-8 invoice date is in invoice body"
        '("Date:")
        (sxml-get-row-col "invoice-details-table" sxml 1 1))
      (test-equal "inv-8 due date is in invoice body"
        '("Due Date:")
        (sxml-get-row-col "invoice-details-table" sxml 2 1))
      (test-equal "inv-8 combo amounts are correct"
        '("$2,133.25" "$2,061.96" "$2,133.25" "$2,061.96" "$2,133.25" "$2,133.25"
          "$1,851.95" "$1,859.30" "$16,368.17" "$1,111.01" "$17,479.18"
          "-$17,479.18" "$0.00")
        (sxml-get-row-col "entries-table" sxml #f -1))
      (test-assert "inv-8 is fully paid up!"
        (gncInvoiceIsPaid inv-8)))
    (test-end "combinations of gncEntry options")))
