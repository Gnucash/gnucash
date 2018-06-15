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
  (for-each (lambda (variant)
              (null-test variant))
            (map car variant-alist))
  (for-each (lambda (variant)
              (net-charts-test variant))
            (map car variant-alist))
  (test-end "net-charts.scm"))

(define (options->render variant options test-title)
  ;; options object -> string
  ;; It also dumps the render into /tmp/test-net-charts-XX.html where XX is the test title
  (gnc:options->render variant options "test-net-charts-~a" test-title))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
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

(define (net-charts-test variant)
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
        (options->render uuid options (format #f "net-charts-test ~a default options" variant))))

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
       'net-charts-tests))))
