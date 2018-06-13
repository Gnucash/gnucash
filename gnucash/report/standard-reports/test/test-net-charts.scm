(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report standard-reports net-barchart))
(use-modules (gnucash report standard-reports net-linechart))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage))
(use-modules (system vm vm))

;; copied from net-barchart.scm and net-linechart.scm
(define (variant->uuid variant)
  (case variant
    ((net-worth-barchart) "cbba1696c8c24744848062c7f1cf4a72")
    ((income-expense-barchart) "80769921e87943adade887b9835a7685")))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "net-charts.scm")
  (null-test 'net-worth-barchart)
  (null-test 'income-expense-barchart)
  (net-charts-test 'net-worth-barchart)
  (net-charts-test 'income-expense-barchart)
  (test-end "net-charts.scm"))

(define (options->sxml variant options test-title)
  ;; options object -> sxml tree
  ;; It also dumps the render into /tmp/test-net-charts-XX.html where XX is the test title
  (gnc:options->sxml variant options "test-net-charts-~a" test-title))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (null-test variant)
  ;; This null-test tests for the presence of report.
  (let* ((uuid (variant->uuid variant))
         (options (gnc:make-report-options uuid)))
    (test-assert (format #f "~a null-test" variant)
      (options->sxml uuid options "null-test"))))

(define (net-charts-test variant)
  (let* ((uuid (variant->uuid variant))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    (define (default-testing-options)
      (let ((options (gnc:make-report-options (variant->uuid variant))))
        (set-option! options "Accounts" "Accounts" (list bank))
        (set-option! options "General" "Start Date" (cons 'relative 'start-cal-year))
        (set-option! options "General" "End Date" (cons 'relative 'end-cal-year))
        options))

    (env-transfer env 01 01 YEAR bank expense       5   #:description "desc-1" #:num "trn1" #:memo "memo-3")
    (env-transfer env 21 02 YEAR income bank       10   #:description "desc-2" #:num "trn2" #:void-reason "void" #:notes "notes3")
    (env-transfer env 11 02 YEAR income bank       29   #:description "desc-3" #:num "trn3"
                  #:reconcile (cons #\c (gnc-dmy2time64 01 03 YEAR)))
    (env-transfer env 01 02 YEAR bank expense      15   #:description "desc-4" #:num "trn4" #:notes "notes2" #:memo "memo-1")
    (env-transfer env 10 03 YEAR bank expense      10   #:description "desc-5" #:num "trn5" #:void-reason "any")
    (env-transfer env 10 03 YEAR expense bank      11   #:description "desc-6" #:num "trn6" #:notes "notes1")
    (env-transfer env 10 04 YEAR income bank        8   #:description "desc-7" #:num "trn7" #:notes "notes1" #:memo "memo-2"
                  #:reconcile (cons #\y (gnc-dmy2time64 01 03 YEAR)))

    (let* ((options (default-testing-options)))
      (test-assert (format #f "~a basic report exists" variant)
        (options->sxml uuid options (format #f "net-charts-test ~a default options" variant))))))

