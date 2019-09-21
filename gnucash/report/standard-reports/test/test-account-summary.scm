(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports account-summary))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

(define accsum-uuid "3298541c236b494998b236dfad6ad752")
(define fsts-uuid "47f45d7d6d57b68518481c1fc8d4e4ba")

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-setup)
  (test-begin "accsum-and-fsts")
  (accsum-tests)
  (test-end "accsum-and-fsts"))

(define (test-setup)
  (define (mnemonic->commodity sym)
    (gnc-commodity-table-lookup
     (gnc-commodity-table-get-table (gnc-get-current-book))
     (gnc-commodity-get-namespace (gnc-default-report-currency))
     sym))
  (define GBP (mnemonic->commodity "GBP"))
  (gnc-commodity-set-user-symbol GBP "#"))

(define (options->sxml uuid options test-title)
  (gnc:options->sxml uuid options "test-accsum" test-title))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value)
        (test-assert (format #f "wrong-option ~a ~a" section name) #f))))

(define (accsum-tests)
  (let* ((account-alist (create-test-data))
         (income (assoc-ref "Income" account-alist)))

    (define (default-testing-options uuid)
      (gnc:make-report-options uuid))


    (test-begin "account-summary")
    (let* ((options (default-testing-options accsum-uuid))
           (sxml (options->sxml accsum-uuid options "accsum")))
      (test-equal "accsum col 1"
        '("#608.00" "-#612.00" "#608.00" "-#612.00" "#608.00" "-#612.00")
        (sxml->table-row-col sxml 1 #f 1))
      (test-equal "accsum col 2"
        '("Root" "Asset" "Bank" "GBP Bank" "Wallet"
          "Liabilities" "Income" "Income-GBP" "Expenses" "Equity")
        (sxml->table-row-col sxml 1 #f 2))
      (test-equal "accsum col 3"
        '("$2,186.00" "#608.00" "$912.00" "$912.00" "$20.00"
          "-$918.00" "$912.00" "-$918.00" "$912.00" "-$918.00")
        (sxml->table-row-col sxml 1 #f 3)))
    (test-end "account-summary")

    (test-begin "fsts")
    (let* ((options (default-testing-options fsts-uuid))
           (sxml (options->sxml fsts-uuid options "fsts")))
      (test-equal "fsts col 1"
        '()
        (sxml->table-row-col sxml 1 #f 1))
      (test-equal "fsts col 2"
        '("Root" "Asset" "Bank" "GBP Bank" "Wallet"
          "Liabilities" "Income" "Income-GBP" "Expenses" "Equity")
        (sxml->table-row-col sxml 1 #f 2))
      (test-equal "fsts col 3"
        '("$0.00" "$0.00" "$0.00")
        (sxml->table-row-col sxml 1 #f 3)))
    (test-end "fsts")))
