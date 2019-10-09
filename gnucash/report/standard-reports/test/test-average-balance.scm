(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports average-balance))
(use-modules (gnucash report report-system))
(use-modules (gnucash report standard-reports budget))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine))
(use-modules (sw_engine))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define uuid "d5adcc61c62e4b8684dd8907448d7900") ;average-balance

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-average-balance")
  (test-average-balance)
  (test-end "test-average-balance"))

(define (set-option! options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-average-balance"
                     test-title #:strip-tag "script"))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Bank")
        (list "Another Bank")
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))))

(define (get-row-col sxml row col)
  (sxml->table-row-col sxml 1 row col))

(define (test-average-balance)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (options (gnc:make-report-options uuid))
         (bank (cdr (assoc "Bank" account-alist)))
         (bank2 (cdr (assoc "Another Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist))))

    (define (default-testing-options)
      (let ((options (gnc:make-report-options uuid)))
        (set-option! options "Accounts" "Accounts" (list bank bank2))
        (set-option! options "Display" "Show table" #t)
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 1979)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 30 06 1979)))
        options))

    (env-transfer env 15 02 1979 income bank 100)
    (env-transfer env 16 04 1979 income bank 100)

    (let* ((options (default-testing-options))
           (sxml (options->sxml options "default")))
      (test-equal "averages"
        '("$0.00" "$50.00" "$100.00" "$150.00" "$200.00" "$200.00")
        (get-row-col sxml #f 3))
      (test-equal "maximums"
        '("$0.00" "$100.00" "$100.00" "$200.00" "$200.00" "$200.00")
        (get-row-col sxml #f 4))
      (test-equal "minimums"
        '("$0.00" "$0.00" "$100.00" "$100.00" "$200.00" "$200.00")
        (get-row-col sxml #f 5))
      (test-equal "net"
        '("$0.00" "$100.00" "$0.00" "$100.00" "$0.00" "$0.00")
        (get-row-col sxml #f 8)))

    (env-transfer env 15 03 1979 bank bank2 25)
    (let* ((options (default-testing-options))
           (sxml (options->sxml options "include-internal")))
      (test-equal "gains-include-internal"
        '("$0.00" "$100.00" "$25.00" "$100.00" "$0.00" "$0.00")
        (get-row-col sxml #f 6))
      (test-equal "loss-include-internal"
        '("$0.00" "$0.00" "$25.00" "$0.00" "$0.00" "$0.00")
        (get-row-col sxml #f 7)))

    (let* ((options (default-testing-options)))
      (set-option! options "Accounts" "Exclude transactions between selected accounts" #t)
      (let ((sxml (options->sxml options "exclude-internal")))
        (test-equal "gain-exclude-internal"
          '("$0.00" "$100.00" "$0.00" "$100.00" "$0.00" "$0.00")
          (get-row-col sxml #f 6))
        (test-equal "loss-exclude-internal"
          '("$0.00" "$0.00" "$0.00" "$0.00" "$0.00" "$0.00")
          (get-row-col sxml #f 7))))
    (teardown)))
