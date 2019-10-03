(use-modules (ice-9 textual-ports))
(use-modules (ice-9 popen))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))
(use-modules (gnucash report view-column))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report taxinvoice))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-98))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; NOTE: This file will attempt to run most reports and set their
;; options. First, the reports are run on empty-book, then on a book
;; with sample transactions and invoices.

;; SIMPLE stress tests by default will run tests as many times as the
;; maximum number of multichoice. if the option with most choices is a
;; price-source with the 4 possibilities, average-cost,
;; weighted-average, pricedb-nearest, pricedb-latest;
;; simple-stress-test will run it 4 times using each price-source. Other
;; options with fewer options are cycled e.g. multichoice 'simple
;; 'detailed will be run with 'simple 'detailed 'simple 'detailed
;; while the price-source gets more exhaustively tested. The report is
;; only run to verify it does not crash. No testing of report output
;; is actually done.
;;
;; PAIRWISE testing will improve test coverage. From the above
;; example, if the stress test runs: average-cost + simple,
;; weighted-average + detailed, pricedb-nearest + simple,
;; pricedb-latest + detailed. No testing of average-cost + detailed is
;; performed. PAIRWISE testing ensures pairs are tested adequately and
;; uses an external tool jenny to generate combinations. The full-path
;; to jenny must be specified in the COMBINATORICS environment
;; variable. The n-tuple may be modified -- see the global variable
;; N-TUPLE. The jenny.c is copied in the "borrowed" folder in GnuCash
;; source.  Source: http://burtleburtle.net/bob/math/jenny.html
;;
;; e.g. COMBINATORICS=/home/user/jenny/jenny ninja check

;; the following is the N-tuple
(define N-TUPLE 2)

(define optionslist '())

(define-record-type :combo
  (make-combo section name combos)
  combo?
  (section get-section)
  (name get-name)
  (combos get-combos))

(define (generate-optionslist)
  (gnc:report-templates-for-each
   (lambda (report-id template)
     (let* ((options-generator (gnc:report-template-options-generator template))
            (options (options-generator))
            (report-options-tested '()))
       (gnc:options-for-each
        (lambda (option)
          (when (memq (gnc:option-type option)
                      '(multichoice boolean))
            (set! report-options-tested
              (cons (make-combo
                     (gnc:option-section option)
                     (gnc:option-name option)
                     (case (gnc:option-type option)
                       ((multichoice) (map (lambda (d) (vector-ref d 0))
                                           (gnc:option-data option)))
                       ((boolean) (list #t #f))))
                    report-options-tested))))
        options)
       (set! optionslist
         (cons (list (cons 'report-id report-id)
                     (cons 'report-name (gnc:report-template-name template))
                     (cons 'options report-options-tested))
               optionslist))))))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "stress options")
  (generate-optionslist)
  (tests)
  (gnc:dump-book)
  (gnc:dump-invoices)
  (test-end "stress options"))

(define jennypath
  (get-environment-variable "COMBINATORICS"))

(define jenny-exists?
  ;; this is a simple test for presence of jenny - will check
  ;; COMBINATORICS env exists, and running it produces exit-code of
  ;; zero, and tests the first few letters of its output.
  (and (string? jennypath)
       (zero? (system jennypath))
       (string=? (string-take (get-string-all (open-input-pipe jennypath)) 6)
                 "jenny:")))

(define (set-option! options section name value)
  (let ((option (gnc:lookup-option options section name)))
    (if option
        (gnc:option-set-value option value))))

(define (mnemonic->commodity sym)
  (gnc-commodity-table-lookup
   (gnc-commodity-table-get-table (gnc-get-current-book))
   (gnc-commodity-get-namespace (gnc-default-report-currency))
   sym))

;; code snippet to run report uuid, with options object
(define (try-run-report uuid options option-summary)
  (define (try proc . args) (gnc:apply-with-error-handling proc args))
  (let* ((res (try gnc:options->render uuid options "stress-test" "test"))
         (captured-error (cadr res)))
    (cond
     (captured-error
      (format #t "[fail]... \noptions-list are:\n~abacktrace:\n~a\n"
              (gnc:html-render-options-changed options #t)
              captured-error)
      (test-assert "logging test failure..." #f))
     (else
      (format #t "[pass] ~a\n" (string-join option-summary ","))))))

(define (simple-stress-test report-name uuid report-options)
  (let ((options (gnc:make-report-options uuid)))
    (test-assert (format #f "basic test ~a" report-name)
      (gnc:options->render uuid options (string-append "stress-" report-name) "test"))
    (format #t "Testing SIMPLE combinations for:\n~a" report-name)
    (for-each
     (lambda (option)
       (format #t ",~a/~a"
               (get-section option)
               (get-name option)))
     report-options)
    (newline)
    (for-each
     (lambda (idx)
       (when (gnc:lookup-option options "General" "Start Date")
         (set-option! options "General" "Start Date"
                      (cons 'absolute (gnc-dmy2time64 1 12 1969))))
       (when (gnc:lookup-option options "General" "End Date")
         (set-option! options "General" "End Date"
                      (cons 'absolute (gnc-dmy2time64 1 1 1972))))
       (let loop ((report-options report-options)
                  (option-summary '()))
         (if (null? report-options)
             (try-run-report uuid options option-summary)
             (let* ((option (car report-options))
                    (section (get-section option))
                    (name (get-name option))
                    (value (list-ref (get-combos option)
                                     (modulo idx (length (get-combos option))))))
               (set-option! options section name value)
               (loop (cdr report-options)
                     (cons (cond
                            ((boolean? value) (if value "t" "f"))
                            (else (object->string value)))
                           option-summary))))))
     (iota (apply max (cons 0 (map (lambda (opt) (length (get-combos opt)))
                                   report-options)))))))

(define (combinatorial-stress-test report-name uuid report-options)
  (let* ((options (gnc:make-report-options uuid))
         (render #f))

    (test-assert (format #f "basic test ~a" report-name)
      (set! render
        (gnc:options->render
         uuid options (string-append "stress-" report-name) "test")))

    (cond
     (render
      (format #t "Testing n-tuple combinatorics for:\n~a" report-name)
      (for-each
       (lambda (option)
         (format #t ",~a/~a"
                 (get-section option)
                 (get-name option)))
       report-options)
      (newline)
      (when (gnc:lookup-option options "General" "Start Date")
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 1 12 1969))))
      (when (gnc:lookup-option options "General" "End Date")
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 1 1 1972))))
      ;; generate combinatorics
      (let* ((option-lengths (map (lambda (report-option)
                                    (length (get-combos report-option)))
                                  report-options))
             (jennyargs (string-join (map number->string option-lengths) " "))
             (n-tuple (min N-TUPLE (length report-options)))
             (cmdline (format #f "~a -n~a ~a" jennypath n-tuple jennyargs))
             (jennyout (get-string-all (open-input-pipe cmdline)))
             (test-cases (string-split jennyout #\newline)))
        (for-each
         (lambda (case)
           (unless (string-null? case)
             (let* ((choices-str (string-filter char-alphabetic? case))
                    (choices-alpha (map char->integer (string->list choices-str)))
                    (choices (map (lambda (n)
                                    ;; a-z -> 0-25, and A-Z -> 26-51
                                    (- n (if (> n 96) 97 39)))
                                  choices-alpha)))
               (let loop ((option-idx (1- (length report-options)))
                          (option-summary '()))
                 (if (negative? option-idx)
                     (try-run-report uuid options option-summary)
                     (let* ((option (list-ref report-options option-idx))
                            (section (get-section option))
                            (name (get-name option))
                            (value (list-ref (get-combos option)
                                             (list-ref choices option-idx))))
                       (set-option! options section name value)
                       (loop (1- option-idx)
                             (cons (cond
                                    ((boolean? value) (if value "t" "f"))
                                    (else (object->string value)))
                                   option-summary))))))))
         test-cases)))

     (else
      (display "...aborted due to basic test failure")))))

(define test
  ;; what strategy are we using here? simple stress test (ie tests as
  ;; many times as the maximum number of options) or combinatorial
  ;; tests (using jenny)
  (if jenny-exists?
      combinatorial-stress-test
      simple-stress-test))

(define (run-tests prefix)
  (for-each
   (lambda (option-set)
     (let ((report-name (assq-ref option-set 'report-name))
           (report-guid (assq-ref option-set 'report-id))
           (report-options (assq-ref option-set 'options)))
       (if (member report-name
                   ;; these reports seem to cause problems when running...
                   '(
                     ;; eguile-based reports
                     "Tax Invoice"
                     "Receipt"
                     "Australian Tax Invoice"
                     "Balance Sheet (eguile)"
                     ))
           (format #t "\nSkipping ~a ~a...\n" report-name prefix)
           (begin
             (format #t "\nTesting ~a ~a...\n" report-name prefix)
             (test report-name report-guid report-options)))))
   optionslist))

(define (tests)
  (run-tests "with empty book")
  (let ((env (create-test-env))
        (account-alist (create-test-data)))
    (gnc:create-budget-and-transactions env account-alist))
  (create-test-invoice-data)
  (run-tests "on a populated book"))
