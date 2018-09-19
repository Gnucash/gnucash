(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports net-charts))
(use-modules (gnucash report standard-reports account-piecharts))
(use-modules (gnucash report standard-reports cashflow-barchart))
(use-modules (gnucash report standard-reports daily-reports))
(use-modules (gnucash report standard-reports price-scatter))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage))
(use-modules (system vm vm))

(define variant-alist
  (list
   (cons 'liability-piechart "3fe6dce77da24c66bdc8f8efdea7f9ac")
   (cons 'stock-piechart "e9418ff64f2c11e5b61d1c7508d793ed")
   (cons 'asset-piechart "5c7fd8a1fe9a4cd38884ff54214aa88a")
   (cons 'expense-piechart "9bf1892805cb4336be6320fe48ce5446")
   (cons 'income-piechart "e1bd09b8a1dd49dd85760db9d82b045c")
   (cons 'cashflow-barchart "5426e4d987f6444387fe70880e5b28a0")
   (cons 'category-barchart-income "44f81bee049b4b3ea908f8dac9a9474e")
   (cons 'category-barchart-expense "b1f15b2052c149df93e698fe85a81ea6")
   (cons 'category-barchart-asset "e9cf815f79db44bcb637d0295093ae3d")
   (cons 'category-barchart-liability "faf410e8f8da481fbc09e4763da40bcc")
   (cons 'daily-income "5e2d129f28d14df881c3e47e3053f604")
   (cons 'daily-expense "dde49fed4ca940959ae7d01b72742530")
   (cons 'price-scatterplot "1d241609fd4644caad765c95be20ff4c")
   (cons 'net-worth-barchart "cbba1696c8c24744848062c7f1cf4a72")
   (cons 'net-worth-linechart "d8b63264186b11e19038001558291366")
   (cons 'income-expense-barchart "80769921e87943adade887b9835a7685")
   (cons 'income-expense-linechart "e533c998186b11e1b2e2001558291366")))

(define (variant->uuid variant)
  (cdr (assq variant variant-alist)))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "net-charts.scm")
  (for-each null-test (map car variant-alist))
  (for-each test-chart (map car variant-alist))
  (test-end "net-charts.scm"))

(define (options->render variant options test-title)
  ;; options object -> string
  ;; It also dumps the render into /tmp/test-net-charts-XX.html where XX is the test title
  (gnc:options->render variant options "test-net-charts-~a" test-title))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank1")
              (list "Bank2")
              (list "Bank3")
              (list "Bank"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Liability" (list (cons 'type ACCT-TYPE-LIABILITY)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))

(define (null-test variant)
  ;; This null-test tests for the presence of report.
  (let* ((uuid (variant->uuid variant))
         (options (gnc:make-report-options uuid)))
    (test-assert (format #f "null-test: ~a" variant)
      (options->render uuid options "null-test"))))

(define (test-chart variant)
  (test-group-with-cleanup (format #f "test variant ~a" variant)
    (test-chart-variant variant)
    (gnc-clear-current-session)))

(define (test-net-chart-variant variant)
  (define (set-option! options section name value)
    (let ((option (gnc:lookup-option options section name)))
      (if option
          (gnc:option-set-value option value)
          (test-assert (format #f "[~a] wrong-option ~a ~a" variant section name) #f))))
  (let* ((uuid (variant->uuid variant))
         (inc-exp? (memq variant '(income-expense-barchart income-expense-linechart)))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank1 (cdr (assoc "Bank1" account-alist)))
         (bank2 (cdr (assoc "Bank2" account-alist)))
         (bank3 (cdr (assoc "Bank3" account-alist)))
         (liability (cdr (assoc "Liability" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist))))

    (env-transfer env 12 01 1970 income bank1 10)
    (env-transfer env 18 01 1970 income bank1 15)
    (env-transfer env 03 03 1970 income bank1 200)

    (env-transfer env 18 01 1970 income bank2 50)
    (env-transfer env 18 02 1970 income bank2 50)

    (env-transfer env 05 05 1969 income bank3 25)
    (env-transfer env 05 01 1970 income bank3 25)

    ;; one closing txn which should be ignored by the inc-exp charts
    (let ((txn (env-transfer env 03 01 1970 equity income 25)))
      (xaccTransSetIsClosingTxn txn #t))

    (let* ((options (gnc:make-report-options (variant->uuid variant))))
      (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 1 1 1970)))
      (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 15 4 1970)))
      (set-option! options "Accounts" "Accounts" (list income bank1 bank2 bank3))
      (set-option! options "General" "Step Size" 'MonthDelta)
      (set-option! options "Display" "Show table" #t)
      (format #t "\n\ntesting net-chart variant:~a\n" variant)
      (let ((sxml (gnc:options->sxml uuid options (format #f "test-net-charts ~a 3 months" variant)
                                     "test-table" #:strip-tag "script")))
        (unless inc-exp?
          (test-equal "first row"
            '("Date" "Assets" "Liabilities" "Net Worth")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "first data row"
            '("01/01/70" "$25.00" "$0.00" "$25.00")
            (sxml->table-row-col sxml 1 1 #f))
          (test-equal "last data row"
            '("04/15/70" "$375.00" "$0.00" "$375.00")
            (sxml->table-row-col sxml 1 -1 #f)))

        (when inc-exp?
          (test-equal "first row"
            '("Date" "Income" "Expense" "Net Profit")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "first data row"
            '("01/01/70" "$100.00" "$0.00" "$100.00")
            (sxml->table-row-col sxml 1 1 #f))
          (test-equal "last data row"
            '("04/01/70" "$0.00" "$0.00" "$0.00")
            (sxml->table-row-col sxml 1 -1 #f)))))))

(define (test-chart-variant variant)
  (define (set-option! options section name value)
    (let ((option (gnc:lookup-option options section name)))
      (if option
          (gnc:option-set-value option value)
          (test-assert (format #f "[~a] wrong-option ~a ~a" variant section name) #f))))
  (let* ((uuid (variant->uuid variant))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (liability (cdr (assoc "Liability" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (default-testing-options)
      (let ((options (gnc:make-report-options (variant->uuid variant))))

        (unless (memq variant '(liability-piechart asset-piechart stock-piechart))
          (set-option! options "General" "Start Date" '(relative . start-cal-year)))

        (set-option! options "General" "End Date" '(relative . end-cal-year))

        (unless (eq? variant 'price-scatterplot)
          (set-option! options "Accounts" "Accounts" (list bank liability)))

        options))

    (env-transfer env 01 01 YEAR equity bank   3)
    (env-transfer env 11 01 YEAR bank expense  8)
    (env-transfer env 11 02 YEAR income bank   29)
    (env-transfer env 21 02 YEAR income bank   10 #:void-reason "void")
    (env-transfer env 22 02 YEAR liability expense 27)
    (env-transfer env 01 03 YEAR bank expense  15)
    (env-transfer env 10 05 YEAR bank expense  10 #:void-reason "any")
    (env-transfer env 10 07 YEAR expense bank  11)
    (env-transfer env 10 09 YEAR income bank    8)

    (let* ((options (default-testing-options)))
      (test-assert (format #f "basic report exists: ~a" variant)
        (options->render uuid options (format #f "test-null ~a default options" variant))))

    ;; test net worth barchart amounts
    (when (or (eq? variant 'net-worth-barchart)
              (eq? variant 'income-expense-barchart))
      ;; create 100 daily transactions from 1/1/70.  this is meant to
      ;; test chart date ranges.  day 0 = $0, day 1 = $1, etc
      (let loop ((date (gnc-dmy2time64 1 1 1970)) (idx 0))
        (when (<= idx 100)
          (env-create-transaction env date bank income idx)
          (loop (incdate date DayDelta) (1+ idx))))
      (when (eq? variant 'net-worth-barchart)
        (let* ((options (default-testing-options)))
        (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 15 1 1970)))
        (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 15 3 1970)))
        (set-option! options "General" "Step Size" 'DayDelta)
        (set-option! options "Display" "Show table" #t)
        (let ((sxml (gnc:options->sxml uuid options (format #f "test-net-charts ~a 2 months" variant)
                                         "test-table" #:strip-tag "script")))
          (test-equal "net-worth-barchart: first row"
            '("Date" "Assets" "Liabilities" "Net Worth")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "net-worth-barchart: first data row"
            '("01/15/70" "$105.00" "$0.00" "$105.00")
            (sxml->table-row-col sxml 1 1 #f))
          (test-equal "net-worth-barchart: last data row"
            '("03/15/70" "$2,701.00" "$0.00" "$2,701.00")
            (sxml->table-row-col sxml 1 -1 #f)))))

      (when (eq? variant 'income-expense-barchart)
        (let* ((options (default-testing-options)))
        (set-option! options "General" "Start Date" (cons 'absolute (gnc-dmy2time64 15 1 1970)))
        (set-option! options "General" "End Date" (cons 'absolute (gnc-dmy2time64 15 3 1970)))
        (set-option! options "General" "Step Size" 'DayDelta)
        (set-option! options "Display" "Show table" #t)
        (set-option! options "Accounts" "Accounts" (list income expense))
        (let ((sxml (gnc:options->sxml uuid options (format #f "test-net-charts ~a 2 years" variant)
                                         "test-table" #:strip-tag "script")))
          (test-equal "income-expense-barchart: first row"
            '("Date" "Income" "Expense" "Net Profit")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "income-expense: first data row"
            '("01/15/70" "$14.00" "$0.00" "$14.00")
            (sxml->table-row-col sxml 1 1 #f))
          (test-equal "income-expense: last data row"
            '("03/15/70" "$73.00" "$0.00" "$73.00")
            (sxml->table-row-col sxml 1 -1 #f))))
      ))

    (case variant
      ((liability-piechart stock-piechart asset-piechart expense-piechart income-piechart)
       'piechart-tests)

      ((cashflow-barchart)
       'cashflow-barchart-test)

      ((category-barchart-income category-barchart-expense category-barchart-asset category-barchart-liability)
       'category-barchart-tests)

      ((daily-income daily-expense)
       'daily-tests)

      ((net-worth-barchart income-expense-barchart net-worth-linechart income-expense-linechart)
       (test-net-chart-variant variant)))))

