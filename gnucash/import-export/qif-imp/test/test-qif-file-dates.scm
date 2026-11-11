(use-modules (gnucash app-utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-file-dates")
  (test-ambiguous-date-reparse)
  (test-end "test-qif-file-dates"))

(define (transaction-dates qif-file)
  (map qif-xtn:date (qif-file:xtns qif-file)))

(define (test-ambiguous-date-reparse)
  (let* ((qif-file (make-qif-file))
         (read-result
          (qif-file:read-file qif-file "ambiguous_dates_sample.qif"
                              (make-ticker-map) #f))
         (raw-dates
          '("10/11/2026" "09/11/2026" "08/11/2026")))
    ;; Stage 1: read QIF records, retaining the date text as strings.
    (test-equal "read fixture successfully" '() read-result)
    (test-equal "dates are strings after reading" raw-dates
      (transaction-dates qif-file))

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
        raw-dates (transaction-dates qif-file)))

    ;; Stage 3a: selecting day-month-year parses and replaces the strings.
    (test-equal "day-month-year reparse succeeds" #t
      (qif-file:reparse-dates qif-file 'd-m-y))
    (test-equal "dates after day-month-year selection"
      '((10 11 2026) (9 11 2026) (8 11 2026))
      (transaction-dates qif-file))

    ;; Stage 3b: changing the choice must reinterpret the original strings.
    (test-equal "changing to month-day-year reparses successfully" #t
      (qif-file:reparse-dates qif-file 'm-d-y))
    (test-equal "dates after changing to month-day-year"
      '((11 10 2026) (11 9 2026) (11 8 2026))
      (transaction-dates qif-file))))
