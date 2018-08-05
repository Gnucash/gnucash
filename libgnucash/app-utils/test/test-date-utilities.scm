(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-date-utilities.scm")
  (test-weeknum-calculator)
  (test-date-get-quarter-string)
  (test-end "test-date-utilities.scm"))

(define (create-datevec l)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now (list-ref l 5))
    (set-tm:min now (list-ref l 4))
    (set-tm:hour now (list-ref l 3))
    (set-tm:mday now (list-ref l 2))
    (set-tm:mon now (1- (list-ref l 1)))
    (set-tm:year now (- (list-ref l 0) 1900))
    (set-tm:isdst now -1)
    now))

(define (create-time64 l)
  (let ((now (create-datevec l)))
    (gnc-mktime now)))

(define (weeknums-equal? pair-of-dates)
  (let ((d1 (car pair-of-dates))
        (d2 (cdr pair-of-dates)))
    (equal? (gnc:date-to-week (create-time64 d1))
            (gnc:date-to-week (create-time64 d2)))))

(define (test-weeknum-calculator)
  (test-assert "weeknums 1/1/70early = 1/1/70late"
    (weeknums-equal? (cons '(1970 1 1 0 0 0)
                           '(1970 1 1 23 59 59))))

  (test-assert "weeknums 31/12/69early = 31/12/69late"
    (weeknums-equal? (cons '(1969 12 31 0 0 0)
                           '(1969 12 31 23 59 59))))

  (test-assert "weeknums 31/12/69 = 1/1/70"
    (weeknums-equal? (cons '(1969 12 31 0 0 0)
                           '(1970 1 1 0 0 1))))

  (test-assert "weeknums 1/1/01early = 01/01/01 late"
    (weeknums-equal? (cons '(2001 1 1 0 0 0)
                           '(2001 1 1 23 59 59))))

  (test-assert "weeknums 1/1/70 != 10/1/70"
    (not (weeknums-equal? (cons '(1970 1 1 0 0 0)
                                '(1970 1 10 0 0 1)))))

  (test-assert "weeknum 28/12/69 != 5/1/70"
    (not (weeknums-equal? (cons '(1969 12 28 0 0 1)
                                '(1970 1 5 0 0 1))))))

(define (test-date-get-quarter-string)
  (test-equal "14/02/2001 = Q1"
    "Q1"
    (gnc:date-get-quarter-string (create-datevec '(2001 2 14 11 42 23))))

  (test-equal "23/04/2013 = Q2"
    "Q2"
    (gnc:date-get-quarter-string (create-datevec '(2013 4 23 18 11 49))))

  (test-equal "11/09/1997 = Q3"
    "Q3"
    (gnc:date-get-quarter-string (create-datevec '(1997 9 11 08 14 21)))))
