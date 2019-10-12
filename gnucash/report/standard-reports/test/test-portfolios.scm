(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports portfolio))
(use-modules (gnucash report standard-reports advanced-portfolio))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))
(use-modules (system vm coverage))
(use-modules (system vm vm))

;; This is implementation testing for both the Portfolio and the
;; Advanced Portfolio Report.

(define portfolio-uuid "4a6b82e8678c4f3d9e85d9f09634ca89")
(define advanced-uuid "21d7cfc59fc74f22887596ebde7e462d")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (if #f
      (coverage-test)
      (run-test-proper)))

(define (coverage-test)
  (let ((currfile (dirname (current-filename))))
    (add-to-load-path (string-take currfile (string-rindex currfile #\/))))
  (call-with-values
      (lambda () (with-code-coverage run-test-proper))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test-proper)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-portfolios.scm")
  (null-test "portfolio" portfolio-uuid)
  (null-test "advanced-portfolio" advanced-uuid)
  (portfolio-tests)
  (advanced-tests)
  (test-end "test-portfolios.scm"))

(define (options->sxml uuid options test-title)
  (gnc:options->sxml uuid options "test-apr" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (teardown)
  (gnc-clear-current-session))

(define (null-test variant uuid)
  ;; This null-test tests for the presence of report.
  (let ((options (gnc:make-report-options uuid)))
    (test-assert (format #f "null-test ~a" variant)
      (options->sxml uuid options "null-test"))))

(define (portfolio-tests)
  (test-group-with-cleanup "portfolio-tests"
    (let* ((account-alist (create-stock-test-data))
           (options (gnc:make-report-options portfolio-uuid)))
      (set-option! options "General" "Price Source" 'pricedb-latest)
      (let ((sxml (options->sxml portfolio-uuid options "latest")))
        (test-equal "portfolio: pricedb-latest"
          '("AAPL" "AAPL" "NASDAQ" "42.00" "$6.00" "$252.00")
          (sxml->table-row-col sxml 1 1 #f)))

      (set-option! options "General" "Price Source" 'pricedb-nearest)
      (set-option! options "General" "Date" (cons 'absolute (gnc-dmy2time64 1 3 1980)))
      (let ((sxml (options->sxml portfolio-uuid options "nearest")))
        (test-equal "portfolio: pricedb-nearest"
          '("AAPL" "AAPL" "NASDAQ" "2.00" "$200.00" "$400.00")
          (sxml->table-row-col sxml 1 1 #f)))

      (set-option! options "General" "Price Source" 'average-cost)
      (set-option! options "General" "Date" (cons 'absolute (gnc-dmy2time64 1 9 1980)))
      (let ((sxml (options->sxml portfolio-uuid options "average-cost")))
        (test-equal "portfolio: average-cost"
          '("AAPL" "AAPL" "NASDAQ" "1.00" "$200.00" "$200.00")
          (sxml->table-row-col sxml 1 1 #f)))

      (set-option! options "General" "Price Source" 'weighted-average)
      (let ((sxml (options->sxml portfolio-uuid options "'weighted-average")))
        (test-equal "portfolio: weighted-average"
          '("AAPL" "AAPL" "NASDAQ" "1.00" "$233.33" "$233 + 1/3")
          (sxml->table-row-col sxml 1 1 #f))))
    (teardown)))

(define (advanced-tests)
  (test-group-with-cleanup "advanced-portfolio-tests"
    (let ((account-alist (create-stock-test-data))
          (options (gnc:make-report-options advanced-uuid)))
      (let ((sxml (options->sxml advanced-uuid options "basic average")))
        (test-equal "advanced: average basis"
          '("AAPL" "AAPL" "NASDAQ" "42.00" "$6.00" "$484.88" "$252.00" "$800.00"
            "$553.00" "$227.88" "-$232.88" "-$5.00" "-0.63%" "$4.00"
            "$10.00" "-$1.00" "-0.13%")
          (sxml->table-row-col sxml 1 1 #f)))

      (set-option! options "General" "Basis calculation method" 'fifo-basis)
      (let ((sxml (options->sxml advanced-uuid options "basic fifo")))
        (test-equal "advanced: fifo basis"
          '("AAPL" "AAPL" "NASDAQ" "42.00" "$6.00" "$543.94" "$252.00" "$800.00"
            "$553.00" "$286.94" "-$291.94" "-$5.00" "-0.63%" "$4.00" "$10.00"
            "-$1.00" "-0.13%")
          (sxml->table-row-col sxml 1 1 #f)))

      (set-option! options "General" "Basis calculation method" 'filo-basis)
      (let ((sxml (options->sxml advanced-uuid options "basic filo")))
        (test-equal "advanced: filo basis"
          '("AAPL" "AAPL" "NASDAQ" "42.00" "$6.00" "$400.00" "$252.00" "$800.00"
            "$553.00" "$143.00" "-$148.00" "-$5.00" "-0.63%" "$4.00" "$10.00"
            "-$1.00" "-0.13%")
          (sxml->table-row-col sxml 1 1 #f))))
    (teardown)))
