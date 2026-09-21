(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-date-formats")
  (test-year-first-separators)
  (test-unambiguous-year-first)
  (test-price-date-selection)
  (test-multirecord-iso-dates)
  (test-inconsistent-per-record-formats)
  (test-year-leading-alternative-selection)
  (test-end "test-qif-date-formats"))

(define all-date-formats '(m-d-y d-m-y y-m-d y-d-m))

(define (read-date-fixture name)
  (let ((file (make-qif-file)))
    (test-equal (string-append name ": read-file succeeds")
      '()
      (qif-file:read-file
       file (string-append (getenv "QIF_DATE_FIXTURES") "/" name)
       (make-ticker-map) #f))
    file))

;; This is the same extraction performed by assistant-qif-import.c.
;; A parse failure is a test failure, not an alternative candidate list.
(define (picker-formats results)
  (and (pair? results) (eq? (car results) #t)
       (qif-file:parse-fields-results (cdr results) 'date)))

(define (transaction-dates file)
  (map qif-xtn:date (qif-file:xtns file)))

(define (price-dates file)
  (map qif-price:date (qif-file:prices file)))

(define (test-year-first-separators)
  (for-each
   (lambda (separator)
     (let* ((name (string-append "ymd-" separator ".qif"))
            (file (read-date-fixture name))
            (raw-date (car (transaction-dates file))))
       (test-group name
         (test-equal "detector returns year-first candidates"
           '(y-m-d y-d-m)
           (qif-parse:check-date-format raw-date all-date-formats))
         ;; Reparse passes a singleton candidate list. Compact dates take
         ;; a different detector branch, so test that contract directly.
         (test-equal "detector accepts a singleton YMD candidate list"
           '(y-m-d)
           (qif-parse:check-date-format raw-date '(y-m-d)))
         (test-equal "detector accepts a singleton YDM candidate list"
           '(y-d-m)
           (qif-parse:check-date-format raw-date '(y-d-m)))
         (let ((results (qif-file:parse-fields file #f)))
           (test-equal "parse-fields preserves year-first warning"
             '(#t (date y-m-d y-d-m)) results)
           (test-equal "picker receives year-first, not year-last, candidates"
             '(y-m-d y-d-m) (picker-formats results)))
         (test-equal "ambiguous transaction is still raw"
           (list raw-date) (transaction-dates file))
         (test-assert "selecting YMD succeeds"
           (qif-file:reparse-dates file 'y-m-d))
         (test-equal "YMD selection parses transaction as January 2"
           '((2 1 2024)) (transaction-dates file))
         ;; Detection alone does not exercise this conversion.
         (test-equal "YDM conversion parses the same text as February 1"
           '(1 2 2024)
           (qif-parse:parse-date/format raw-date 'y-d-m))
         ;; Exercise the other picker choice on a fresh, unresolved file.
         (let ((other-file (read-date-fixture name)))
           (qif-file:parse-fields other-file #f)
           (test-assert "selecting YDM succeeds"
             (qif-file:reparse-dates other-file 'y-d-m))
           (test-equal "YDM selection parses transaction as February 1"
             '((1 2 2024)) (transaction-dates other-file))))))
   '("slash" "hyphen" "dot" "apostrophe" "compact")))

(define (test-unambiguous-year-first)
  (for-each
   (lambda (example)
     (let* ((name (car example))
            (file (read-date-fixture name)))
       (test-group name
         (test-equal "unambiguous interpretation needs no picker"
           '() (qif-file:parse-fields file #f))
         (test-equal "transaction date is parsed"
           (list (cadr example)) (transaction-dates file)))))
   '(("ymd-day13.qif" (13 1 2024))
     ("ydm-day13.qif" (13 1 2024))
     ;; Two candidate formats, but only one interpretation.
     ("year-first-equal-parts.qif" (3 3 2024)))))

(define (test-price-date-selection)
  ;; A real year-first ambiguity is never offered to the user: the price and
  ;; transaction warnings share the 'date key, add-error prepends, and
  ;; parse-fields-results returns only the first match, so the later-recorded
  ;; year-last transaction warning shadows the year-first price warning.
  (let* ((name "mixed-price-ymd-transaction-mdy.qif")
         (file (read-date-fixture name)))
    (test-group "year-first ambiguity shadowed out of the picker: mixed-price-ymd-transaction-mdy.qif"
      (test-equal "price detector returns year-first candidates"
        '(y-m-d y-d-m)
        (qif-parse:check-date-format (car (price-dates file)) all-date-formats))
      (test-equal "transaction detector returns year-last candidates"
        '(m-d-y d-m-y)
        (qif-parse:check-date-format (car (transaction-dates file)) all-date-formats))
      (let ((results (qif-file:parse-fields file #f)))
        (test-equal "both warnings survive, transaction warning first"
          '(#t (date m-d-y d-m-y) (date y-m-d y-d-m)) results)
        (test-equal "picker receives only the year-last candidates"
          '(m-d-y d-m-y) (picker-formats results))
        ;; The user is only ever asked to choose between MDY and DMY, so the
        ;; offered list cannot express any reading of the price date.
        (test-assert "some offered candidate can interpret the ambiguous price date"
          (any (lambda (format)
                 (qif-parse:parse-date/format (car (price-dates file)) format))
               (picker-formats results))))
      (test-assert "selecting MDY reports success"
        (qif-file:reparse-dates file 'm-d-y))
      (test-equal "transaction is parsed as February 3"
        '((3 2 2024)) (transaction-dates file))
      ;; Do not guess which price format the user wanted. Completion must
      ;; at least not leave a price date as an unparsed string.
      (test-assert "date-page completion leaves no unresolved price date"
        (every list? (price-dates file)))))

  ;; No conflicting format policy is needed here: both groups use YMD.
  (let* ((name "price-and-transaction-ymd.qif")
         (file (read-date-fixture name)))
    (test-group name
      (test-equal "picker receives year-first candidates for both groups"
        '(y-m-d y-d-m) (picker-formats (qif-file:parse-fields file #f)))
      (test-assert "selecting YMD reports success"
        (qif-file:reparse-dates file 'y-m-d))
      (test-equal "transaction is parsed"
        '((2 1 2024)) (transaction-dates file))
      (test-equal "price is also parsed"
        '((2 1 2024)) (price-dates file))))

  ;; Only prices remain ambiguous. The resolved transaction must survive
  ;; selecting a format for the unresolved dates.
  (let* ((name "price-ymd-transaction-unambiguous.qif")
         (file (read-date-fixture name)))
    (test-group name
      (test-equal "picker receives the price's year-first candidates"
        '(y-m-d y-d-m) (picker-formats (qif-file:parse-fields file #f)))
      (test-equal "transaction was already resolved"
        '((13 1 2024)) (transaction-dates file))
      (test-assert "selecting YMD for unresolved prices succeeds"
        (qif-file:reparse-dates file 'y-m-d))
      (test-equal "resolved transaction is preserved"
        '((13 1 2024)) (transaction-dates file))
      (test-equal "price is parsed"
        '((2 1 2024)) (price-dates file)))))

(define (test-multirecord-iso-dates)
  ;; Expected dates are day/month/year, in file order. The reader preserves
  ;; transaction order but stores prices in reverse file order.
  (for-each
   (lambda (example)
     (let* ((name (list-ref example 0))
            (needs-choice? (list-ref example 1))
            (expected-transactions (list-ref example 2))
            (expected-prices (list-ref example 3))
            (file (read-date-fixture name))
            (raw-transactions (transaction-dates file))
            (raw-prices (price-dates file)))
       (test-group name
         (test-equal "all four transactions were read"
           4 (length raw-transactions))
         (test-equal "all price records were read"
           (length expected-prices) (length raw-prices))
         (let ((results (qif-file:parse-fields file #f)))
           (if needs-choice?
               (begin
                 (test-equal "ambiguous ISO records supply year-first choices"
                   '(y-m-d y-d-m) (picker-formats results))
                 (test-equal "all ambiguous transaction dates remain raw"
                   raw-transactions (transaction-dates file))
                 (test-equal "all ambiguous price dates remain raw"
                   raw-prices (price-dates file))
                 (test-assert "YMD selection succeeds for the entire file"
                   (qif-file:reparse-dates file 'y-m-d)))
               (test-equal "whole-file interpretation requires no selection"
                 '() results)))
         (test-equal "every transaction has the intended ISO date"
           expected-transactions (transaction-dates file))
         (test-equal "every price has the intended ISO date"
           (reverse expected-prices) (price-dates file)))))
   '(("iso-multi-day31-first.qif" #f
      ((31 12 2022) (2 1 2023) (3 2 2023) (11 12 2023)) ())
     ("iso-multi-day31-last.qif" #f
      ((2 1 2023) (3 2 2023) (11 12 2023) (31 12 2022)) ())
     ("iso-multi-days-at-most12.qif" #t
      ((11 12 2022) (2 1 2023) (3 2 2023) (5 4 2024)) ())
     ("iso-multi-equal-month-day.qif" #f
      ((12 12 2022) (1 1 2023) (2 2 2023) (3 3 2024)) ())
     ("iso-multi-prices-ambiguous.qif" #t
      ((11 12 2022) (2 1 2023) (3 2 2023) (5 4 2024))
      ((11 12 2022) (2 1 2023) (3 2 2023) (5 4 2024)))
     ("iso-multi-prices-day31.qif" #f
      ((2 1 2023) (3 2 2023) (11 12 2023) (31 12 2022))
      ((2 1 2023) (3 2 2023) (11 12 2023) (31 12 2022))))))

;; check-and-parse-field intersects the candidate formats across every record
;; in a group, so two individually unambiguous dates in incompatible formats
;; empty the list and take the "Unrecognized or inconsistent format." path.
;; That path is not gated by the ambiguity policy: it is a hard failure, not
;; the #t warning the rest of this suite exercises.
(define (test-inconsistent-per-record-formats)
  (let* ((name "inconsistent-per-record-formats.qif")
         (file (read-date-fixture name))
         (raw-transactions (transaction-dates file)))
    (test-group name
      (test-equal "both records were read"
        2 (length raw-transactions))
      (test-equal "first record is only valid as YMD"
        '(y-m-d)
        (qif-parse:check-date-format (car raw-transactions) all-date-formats))
      (test-equal "second record is only valid as MDY"
        '(m-d-y)
        (qif-parse:check-date-format (cadr raw-transactions) all-date-formats))
      (let ((results (qif-file:parse-fields file #f)))
        (test-assert "parse-fields reports a hard failure, not a warning"
          (eq? (car results) #f))
        (test-assert "the failure carries a date entry"
          (string? (qif-file:parse-fields-results (cdr results) 'date)))
        ;; The picker must never be offered a candidate list here: no single
        ;; format can interpret both records.
        (test-equal "no candidates reach the picker"
          #f (picker-formats results)))
      (test-equal "neither transaction date was parsed under either format"
        raw-transactions (transaction-dates file))
      (test-assert "every transaction date is still a raw string"
        (every string? (transaction-dates file))))))

;; The "no day over 12" files offer YMD and YDM. test-multirecord-iso-dates
;; already covers selecting YMD; these groups select the other offered
;; candidate on freshly read files, so that the two together show that neither
;; year-leading choice completes a correct import of the same file.
(define (test-year-leading-alternative-selection)
  ;; Every date here is also a valid YDM calendar date, so the conversion has
  ;; no calendar reason to refuse the selection.
  (let* ((name "iso-multi-days-at-most12.qif")
         (file (read-date-fixture name))
         (raw-transactions (transaction-dates file)))
    (test-group "YDM selection on transactions only: iso-multi-days-at-most12.qif"
      (test-equal "picker offers both year-leading candidates"
        '(y-m-d y-d-m) (picker-formats (qif-file:parse-fields file #f)))
      (test-assert "selecting YDM reports success for the entire file"
        (qif-file:reparse-dates file 'y-d-m))
      (test-equal "YDM selection parses every transaction as day-before-month"
        '((12 11 2022) (1 2 2023) (2 3 2023) (4 5 2024))
        (transaction-dates file))
      (test-equal "refused YDM selection leaves every transaction date raw"
        raw-transactions (transaction-dates file))))

  (let* ((name "iso-multi-prices-ambiguous.qif")
         (file (read-date-fixture name))
         (raw-transactions (transaction-dates file))
         (raw-prices (price-dates file)))
    (test-group "YDM selection with prices: iso-multi-prices-ambiguous.qif"
      (test-equal "picker offers both year-leading candidates"
        '(y-m-d y-d-m) (picker-formats (qif-file:parse-fields file #f)))
      (test-assert "selecting YDM reports success for the entire file"
        (qif-file:reparse-dates file 'y-d-m))
      (test-equal "YDM selection parses every transaction as day-before-month"
        '((12 11 2022) (1 2 2023) (2 3 2023) (4 5 2024))
        (transaction-dates file))
      (test-equal "YDM selection parses every price as day-before-month"
        (reverse '((12 11 2022) (1 2 2023) (2 3 2023) (4 5 2024)))
        (price-dates file))
      (test-equal "refused YDM selection leaves every transaction date raw"
        raw-transactions (transaction-dates file))
      (test-equal "refused YDM selection leaves every price date raw"
        raw-prices (price-dates file)))))
