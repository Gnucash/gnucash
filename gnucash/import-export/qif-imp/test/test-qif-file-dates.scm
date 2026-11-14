(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-file-dates")
  (test-ambiguous-date-reparse)
  (test-ambiguous-prices-resolved-by-transactions)
  (test-incompatible-price-transaction-dates)
  (test-end "test-qif-file-dates"))

(define (transaction-dates qif-file)
  (map qif-xtn:date (qif-file:xtns qif-file)))

(define (transaction-date-strings qif-file)
  (map qif-xtn:datestr (qif-file:xtns qif-file)))

(define (price-dates qif-file)
  (map qif-price:date (reverse (qif-file:prices qif-file))))

(define (price-date-strings qif-file)
  (map qif-price:datestr (reverse (qif-file:prices qif-file))))

(define (test-ambiguous-date-reparse)
  (let* ((qif-file (make-qif-file))
         (read-result
          (qif-file:read-file qif-file "ambiguous_dates_sample.qif"
                              (make-ticker-map) #f))
         (raw-dates
          '("10/11/2026" "09/11/2026" "08/11/2026"))
         (raw-price-dates
          '("10/11/2026" "09/11/2026" "08/11/2026")))

    ;; Stage 1: read QIF records, retaining the date text as strings.
    (test-equal "read fixture successfully" '() read-result)
    (test-equal "raw date strings are retained after reading" raw-dates
      (transaction-date-strings qif-file))
    (test-equal "parsed dates are unset after reading" '(#f #f #f)
      (transaction-dates qif-file))
    (test-equal "price raw dates are retained" raw-price-dates
      (price-date-strings qif-file))
    (test-equal "price dates are unset before parsing" '(#f #f #f)
      (price-dates qif-file))

    ;; Stage 2: initial parse detects both day-month-year and month-day-year.
    (let* ((parse-result (qif-file:parse-fields qif-file #f))
           (parse-errors
            (and (pair? parse-result) (cdr parse-result)))
           (date-formats
            (and parse-errors
                 (qif-file:parse-fields-results parse-errors 'date))))
      (test-equal "ambiguous date formats reported" '(m-d-y d-m-y)
        date-formats)
      (test-equal "ambiguous dates remain strings after initial parse"
        raw-dates (transaction-date-strings qif-file))
      (test-equal "ambiguous dates remain unparsed"
        '(#f #f #f) (transaction-dates qif-file))
      (test-equal "ambiguous price dates remain unparsed"
        '(#f #f #f) (price-dates qif-file))
      (test-equal "ambiguous price date strings remain intact"
        raw-price-dates (price-date-strings qif-file)))

    ;; Stage 3a: selecting day-month-year fills the parsed date fields.
    (test-equal "day-month-year reparse succeeds" #t
      (qif-file:reparse-dates qif-file 'd-m-y))
    (test-equal "dates after day-month-year selection"
      '((10 11 2026) (9 11 2026) (8 11 2026))
      (transaction-dates qif-file))
    (test-equal "raw dates survive first reparse" raw-dates
      (transaction-date-strings qif-file))
    (test-equal "price dates after day-month-year selection"
      '((10 11 2026) (9 11 2026) (8 11 2026)) (price-dates qif-file))
    (test-equal "raw price dates survive first reparse"
      raw-price-dates (price-date-strings qif-file))

    ;; Stage 3b: changing the choice must reinterpret the original strings.
    (test-equal "changing to month-day-year reparses successfully" #t
      (qif-file:reparse-dates qif-file 'm-d-y))
    (test-equal "dates after changing to month-day-year"
      '((11 10 2026) (11 9 2026) (11 8 2026))
      (transaction-dates qif-file))
    (test-equal "raw dates survive changed reparse" raw-dates
      (transaction-date-strings qif-file))
    (test-equal "price dates after changing to month-day-year"
      '((11 10 2026) (11 9 2026) (11 8 2026)) (price-dates qif-file))
    (test-equal "raw price dates survive changed reparse"
      raw-price-dates (price-date-strings qif-file))))

(define (test-ambiguous-prices-resolved-by-transactions)
  (let* ((qif-file (make-qif-file))
         (read-result
          (qif-file:read-file
           qif-file "ambiguous_prices_resolved_by_transactions.qif"
           (make-ticker-map) #f))
         (parse-result (qif-file:parse-fields qif-file #f)))
    (test-equal "read price ambiguity fixture successfully" '() read-result)
    (test-equal "transaction dates resolve price ambiguity on first parse"
      '() parse-result)
    (test-equal "resolved format is retained on the file" 'd-m-y
      (qif-file:date-format qif-file))
    (test-equal "transaction dates use the resolved day-month-year format"
      '((13 11 2026) (14 11 2026) (15 11 2026))
      (transaction-dates qif-file))
    (test-equal "ambiguous price dates use the transaction-resolved format"
      '((10 11 2026) (9 11 2026) (8 11 2026))
      (price-dates qif-file))
    (test-equal "resolved transaction date text is preserved"
      '("13/11/2026" "14/11/2026" "15/11/2026")
      (transaction-date-strings qif-file))
    (test-equal "resolved price date text is preserved"
      '("10/11/2026" "09/11/2026" "08/11/2026")
      (price-date-strings qif-file))))

(define (test-incompatible-price-transaction-dates)
  (let* ((qif-file (make-qif-file))
         (read-result
          (qif-file:read-file
           qif-file "incompatible_price_transaction_dates.qif"
           (make-ticker-map) #f))
         (parse-result (qif-file:parse-fields qif-file #f))
         (parse-errors (and (pair? parse-result) (cdr parse-result)))
         (date-error
          (and parse-errors
               (qif-file:parse-fields-results parse-errors 'date))))
    (test-equal "read incompatible-date fixture successfully" '()
      read-result)
    (test-assert "incompatible date formats fail parsing"
      (and (pair? parse-result) (eq? (car parse-result) #f)))
    (test-equal "incompatible date formats report the date error"
      (G_ "Unrecognized or inconsistent format.") date-error)
    (test-equal "no file date format is retained after conflict" #f
      (qif-file:date-format qif-file))))
