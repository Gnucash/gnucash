;; -*-scheme-*-
;; dateutils.scm
;; date utility functions.  mainly used by budget
;; Bryan Larsen (blarsen@ada-works.com)

(gnc:support "dateutils.scm")

;; get stuff from localtime date vector
(define (gnc:date-get-year datevec)
  (vector-ref datevec 5))
(define (gnc:date-get-month-day datevec)
  (vector-ref datevec 3))
;; get month with january==1
(define (gnc:date-get-month datevec)
  (+ (vector-ref datevec 4) 1))
(define (gnc:date-get-week-day datevec)
  (vector-ref datevec 6))
(define (gnc:date-get-year-day datevec)
  (vector-ref datevec 7))

;; is leap year?
(define (gnc:leap-year? year)
  (if (= (remainder year 4) 0)
      (if (= (remainder year 100) 0)
	  (if (= (remainder year 400) 0) #t #f)
	  #t)
      #f))

;; number of days in year
(define (gnc:days-in-year year)
  (if (gnc:leap-year? year) 366 365))

;; number of days in month
(define (gnc:days-in-month month year)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (gnc:leap-year? year) 29 28))))

;; convert a date in seconds since 1970 into # of years since 1970 as
;; a fraction.
(define (gnc:date-to-year-fraction caltime)
  (let ((lt (localtime caltime)))
    (+ (- (gnc:date-get-year lt) 1970)
       (/ (gnc:date-get-year-day lt) (* 1.0 (gnc:days-in-year 
					     (gnc:date-get-year lt)))))))

;; convert a date in seconds since 1970 into # of months since 1970
(define (gnc:date-to-month-fraction caltime)
  (let ((lt (localtime caltime)))
    (+ (* 12 (- (gnc:date-get-year lt) 1970.0))
       (/ (- (gnc:date-get-month-day lt) 1.0) (gnc:days-in-month 
					       (gnc:date-get-month lt)
					       (gnc:date-get-year lt))))))

;; convert a date in seconds since 1970 into # of weeks since Jan 4, 1970
;; ignoring leap-seconds
(define (gnc:date-to-week-fraction caltime)
  (/ (- (/ (/ caltime 3600.0) 24) 3) 7))

;; convert a date in seconds since 1970 into # of days since Jan 1, 1970
;; ignoring leap-seconds
(define (gnc:date-to-day-fraction caltime)
  (/ (/ caltime 3600.0) 24))

;; convert a date to a defined fraction
(define (gnc:date-to-N-fraction caltime type)
  (case type
    ((gnc:budget-day) (gnc:date-to-day-fraction caltime))
    ((gnc:budget-week) (gnc:date-to-week-fraction caltime))
    ((gnc:budget-month) (gnc:date-to-month-fraction caltime))
    ((gnc:budget-year) (gnc:date-to-year-fraction caltime))
    (else (gnc:debug "undefined period type in budget!") #f)))

;; describe a time type
(define (gnc:date-describe-type type)
  (case type
    ((gnc:budget-day) "days")
    ((gnc:budget-week) "weeks")
    ((gnc:budget-month) "months")
    ((gnc:budget-year) "years")))

