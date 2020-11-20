;; date-utilities.scm -- date utility functions.
;; Bryan Larsen (blarsen@ada-works.com)
;; Revised by Christopher Browne
;; Improvement to financial year support by Yves-Eric Martin
;;
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org


(use-modules (gnucash core-utils))
(use-modules (srfi srfi-9))

;; get stuff from localtime date vector
(define (gnc:date-get-year datevec)
  (+ 1900 (tm:year datevec)))
(define (gnc:date-get-quarter datevec)
  (+ (quotient (tm:mon datevec) 3) 1))
(define (gnc:date-get-month-day datevec)
  (tm:mday datevec))
;; get month with january==1
(define (gnc:date-get-month datevec)
  (+ (tm:mon datevec) 1))
(define (gnc:date-get-week-day datevec)
  (+ (tm:wday datevec) 1))
;; jan 1 == 1
(define (gnc:date-get-week datevec)
  (gnc:date-to-week (gnc:time64-start-day-time
                     (gnc-mktime datevec))))

(define (gnc:date-get-year-day datevec)
  (+ (tm:yday datevec) 1))

(define (gnc:time64-get-year t64)
  (gnc:date-get-year (gnc-localtime t64)))

(define (gnc:time64-get-quarter t64)
  (gnc:date-get-quarter (gnc-localtime t64)))

(define (gnc:time64-get-month-day t64)
  (gnc:date-get-month-day (gnc-localtime t64)))

(define (gnc:time64-get-month t64)
  (gnc:date-get-month (gnc-localtime t64)))

(define (gnc:time64-get-week-day t64)
  (gnc:date-get-week-day (gnc-localtime t64)))

(define (gnc:time64-get-week t64)
  (gnc:date-get-week (gnc-localtime t64)))

(define (gnc:time64-get-year-day t64)
  (gnc:date-get-year-day (gnc-localtime t64)))

(define (gnc:date-get-year-string datevec)
  (gnc-print-time64 (gnc-mktime datevec) "%Y"))

(define (gnc:date-get-quarter-string datevec)
  (format #f "Q~d" (gnc:date-get-quarter datevec)))

(define (gnc:date-get-quarter-year-string datevec)
  (string-append 
   (gnc:date-get-quarter-string datevec) 
   " " 
   (gnc:date-get-year-string datevec)))

(define (gnc:date-get-month-string datevec)
  (gnc-print-time64 (gnc-mktime datevec) "%B"))

(define (gnc:date-get-month-year-string datevec)
  (gnc-print-time64 (gnc-mktime datevec) "%B %Y"))

(define (gnc:date-get-week-year-string datevec)
  (let* ((beginweekt64 (* (gnc:time64-get-week (gnc-mktime datevec)) 7 86400))
         (begin-string (qof-print-date (+ beginweekt64 (* 3 86400))))
         (end-string (qof-print-date (+ beginweekt64 (* 9 86400)))))
    (format #f (G_ "~a to ~a") begin-string end-string)))

;; is leap year?
(define (gnc:leap-year? year)
  (or (and (zero? (remainder year 4))
           (not (zero? (remainder year 100))))
      (zero? (remainder year 400))))

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
  (let ((lt (gnc-localtime caltime)))
    (+ (- (gnc:date-get-year lt) 1970)
       (/ (- (gnc:date-get-year-day lt) 1)
          (gnc:days-in-year (gnc:date-get-year lt))))))

;; return the number of years (in floating point format) between two dates.
(define (gnc:date-year-delta caltime1 caltime2)
  (let* ((lt1 (gnc-localtime caltime1))
	 (lt2 (gnc-localtime caltime2))
	 (day1 (gnc:date-get-year-day lt1))
	 (day2 (gnc:date-get-year-day lt2))
	 (year1 (gnc:date-get-year lt1))
	 (year2 (gnc:date-get-year lt2))
	 (dayadj1 (if (and (not (gnc:leap-year? year1))
			   (>= day1 59))
		      (+ day1 1)
		      day1))
	 (dayadj2 (if (and (not (gnc:leap-year? year2))
			   (>= day2 59))
		      (+ day2 1)
		      day2)))
    (+ (- (gnc:date-get-year lt2) (gnc:date-get-year lt1))
       (/ (- dayadj2 dayadj1) 
	  366.0))))

;; convert a date in seconds since 1970 into # of 1/2 years since 1970 as
;; a fraction (doubling date-to-year-fraction)
(define (gnc:date-to-halfyear-fraction caltime)
  (* (gnc:date-to-year-fraction caltime) 2))

;; convert a date in seconds since 1970 into # of quarters since 1970
;; (assuming quarter = 3 months and using 1/3 of date-to-month-fraction)
(define (gnc:date-to-quarter-fraction caltime)
  (/ (gnc:date-to-month-fraction caltime) 3))

;; convert a date in seconds since 1970 into # of months since 1970
(define (gnc:date-to-month-fraction caltime)
  (let ((lt (gnc-localtime caltime)))
    (+ (* 12 (- (gnc:date-get-year lt) 1970))
       (gnc:date-get-month lt) -1
       (/ (- (gnc:date-get-month-day lt) 1)
          (gnc:days-in-month
	   (gnc:date-get-month lt)
	   (gnc:date-get-year lt))))))

(define (gnc:date-to-twoweek-fraction caltime)
  (/ (gnc:date-to-week-fraction caltime) 2))

;; which dow does the week start? 1=Sunday, 2=Monday etc
(define weekstart
  (let ((dow (gnc-start-of-week)))
    (cond
     ((zero? dow) (gnc:warn "cannot determine start of week. using Sunday") 1)
     (else dow))))

(define (gnc:date-to-week-fraction caltime)
  (/ (- (/ caltime 86400) 1 weekstart) 7))

(define (gnc:date-to-week caltime)
  (floor (gnc:date-to-week-fraction caltime)))

;; convert a date in seconds since 1970 into # of days since Feb 28, 1970
;; ignoring leap-seconds
(define (gnc:date-to-day-fraction caltime)
  (- (/ (/ caltime 3600.0) 24) 59))

;; Returns the function that converts a date into a fraction of
;; {year,month,week,day} according to the given symbol, or #f if the
;; symbol was unknown
(define (gnc:date-get-fraction-func interval)
  (case interval
    ((YearDelta) gnc:date-to-year-fraction)
    ((HalfYearDelta) gnc:date-to-halfyear-fraction)
    ((QuarterDelta) gnc:date-to-quarter-fraction)
    ((MonthDelta) gnc:date-to-month-fraction)
    ((TwoWeekDelta) gnc:date-to-twoweek-fraction)
    ((WeekDelta) gnc:date-to-week-fraction)
    ((DayDelta) gnc:date-to-day-fraction)
    (else #f)))

;; Modify a date
(define (moddate op adate delta)
  (let ((newtm (gnc-localtime adate)))
    (begin
      (set-tm:sec newtm (op (tm:sec newtm) (tm:sec delta)))
      (set-tm:min newtm (op (tm:min newtm) (tm:min delta)))
      (set-tm:hour newtm (op (tm:hour newtm) (tm:hour delta)))
      (set-tm:mday newtm (op (tm:mday newtm) (tm:mday delta)))
      (set-tm:mon newtm (op (tm:mon newtm) (tm:mon delta)))
      (set-tm:year newtm (op (tm:year newtm) (tm:year delta)))
      (set-tm:isdst newtm -1)
      (gnc-mktime newtm))))

;; Add or subtract time from a date
(define (decdate adate delta) (moddate - adate delta ))
(define (incdate adate delta) (moddate + adate delta ))

;; returns #t if adding 1 to mday causes a month change.
(define (end-month? date)
  (let ((nextdate (gnc-localtime date)))
    (set-tm:mday nextdate (1+ (tm:mday nextdate)))
    (not (= (tm:mon (gnc-localtime (gnc-mktime nextdate)))
            (tm:mon (gnc-localtime date))))))

(define (incdate-months date nmonths)
  (let* ((new-date (gnc-localtime date))
         (newmonth (+ (tm:mon new-date) nmonths))
         (new-month-proper (floor-remainder newmonth 12))
         (new-year-proper (+ (tm:year new-date) (floor-quotient newmonth 12))))
    (set-tm:year new-date new-year-proper)
    (set-tm:mon new-date new-month-proper)
    (let loop ((new-mday (tm:mday new-date)))
      (set-tm:mday new-date new-mday)
      (let ((res (gnc-mktime new-date)))
        (cond
         ;; next date causes a date slip. reduce mday.
         ((not (= new-month-proper (tm:mon (gnc-localtime res))))
          (loop (1- new-mday)))
         ;; orig date is month-end. ensure all dates are month-ends.
         ((and (end-month? date) (not (end-month? res)))
          (loop (1+ new-mday)))
         (else res))))))

;; Build a list of time intervals.
;;
;; Note that the last interval will be shorter than <incr> if
;; (<curd>-<endd>) is not an integer multiple of <incr>. If you don't
;; want that you'll have to write another function.
(define (gnc:make-date-interval-list startdate enddate incr)
  (define month-delta
    (assv-ref MonthDeltas incr))
  (define (make-interval from to)
    (list from (if (< to enddate) (decdate to SecDelta) enddate)))
  (let loop ((result '())
             (date startdate)
             (idx 0))
    (cond
     ((>= date enddate)
      (reverse result))
     (month-delta
      (let* ((curr (incdate-months startdate (* month-delta idx)))
             (next (incdate-months startdate (* month-delta (1+ idx)))))
        (loop (cons (make-interval curr next) result)
              next
              (1+ idx))))
     (else
      (let ((next (incdate date incr)))
        (loop (cons (make-interval date next) result)
              next
              (1+ idx)))))))

;; Build a list of times.  The dates are evenly spaced with the
;; stepsize 'incr'. If the difference of 'startdate' and 'enddate' is
;; not an integer multiple of 'incr', 'enddate' will be added as the
;; last element of the list, thus making the last interval smaller
;; than 'incr'.
(define (gnc:make-date-list startdate enddate incr)
  (define month-delta
    (assv-ref MonthDeltas incr))
  (let loop ((result '())
             (date startdate)
             (idx 0))
    (cond
     ((>= date enddate)
      (reverse (cons enddate result)))
     (month-delta
      (let* ((curr (incdate-months startdate (* month-delta idx)))
             (next (incdate-months startdate (* month-delta (1+ idx)))))
        (loop (cons curr result)
              next
              (1+ idx))))
     (else
      (loop (cons date result)
            (incdate date incr)
            (1+ idx))))))

; A reference zero date - the Beginning Of The Epoch
; Note: use of eval is evil... by making this a generator function, 
; each delta function gets its own instance of Zero Date
(define (make-zdate) 
  (let ((zd (gnc-localtime (current-time))))
    (set-tm:hour zd 0)
    (set-tm:min zd 0)
    (set-tm:sec zd 0)
    (set-tm:mday zd 0)
    (set-tm:mon zd 0)
    (set-tm:year zd 0)
    (set-tm:yday zd 0)
    (set-tm:wday zd 0)
    (set-tm:isdst zd 0)
    zd))


(define SecDelta 
  (let ((ddt (make-zdate)))
    (set-tm:sec ddt 1)
    ddt))

(define DayDelta
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt 1)
    ddt))

(define WeekDelta 
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt 7)
    ddt))

(define TwoWeekDelta
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt 14)
    ddt))

(define MonthDelta
  (let ((ddt (make-zdate)))
    (set-tm:mon ddt 1)
    ddt))

(define QuarterDelta
  (let ((ddt (make-zdate)))
    (set-tm:mon ddt 3)
    ddt))

(define HalfYearDelta
  (let ((ddt (make-zdate)))
    (set-tm:mon ddt 6)
    ddt))

(define YearDelta 
  (let ((ddt (make-zdate)))
    (set-tm:year ddt 1)
    ddt))


(define ThirtyDayDelta
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt 30)
    ddt))

(define NinetyDayDelta
  (let ((ddt (make-zdate)))
    (set-tm:mday ddt 90)
    ddt))

(define MonthDeltas
  (list
   (cons MonthDelta 1)
   (cons QuarterDelta 3)
   (cons HalfYearDelta 6)
   (cons YearDelta 12)))

;; if you add any more FooDeltas, add to this list!!!

(define deltalist
  (list (cons 'SecDelta SecDelta)
	(cons 'DayDelta DayDelta)
	(cons 'WeekDelta WeekDelta)
	(cons 'TwoWeekDelta TwoWeekDelta)
	(cons 'MonthDelta MonthDelta)
	(cons 'QuarterDelta QuarterDelta)
	(cons 'HalfYearDelta HalfYearDelta)
	(cons 'YearDelta YearDelta)
	(cons 'ThirtyDayDelta ThirtyDayDelta)
	(cons 'NinetyDayDelta NinetyDayDelta)))

(define (gnc:deltasym-to-delta ds)
  (let ((retval (assq ds deltalist)))
    (if (pair? retval)
	(cdr retval)
	#f)))

;; given a time64 time on a certain day (local time)
;; converts it to be midday that day.

(define (gnc:time64-start-day-time t64)
  (let ((bdt (gnc-localtime t64)))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 0)
    (set-tm:isdst bdt -1)
    (gnc-mktime bdt)))

(define (gnc:time64-end-day-time t64)
  (let ((bdt (gnc-localtime t64)))
    (set-tm:sec bdt 59)
    (set-tm:min bdt 59)
    (set-tm:hour bdt 23)
    (set-tm:isdst bdt -1)
    (gnc-mktime bdt)))

(define (gnc:time64-previous-day t64)
  (decdate t64 DayDelta))

(define (gnc:time64-next-day t64)
  (incdate t64 DayDelta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; relative-date functions start here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type :reldates
  (make-reldate symbol string desc fn)
  gnc:reldate?
  (symbol gnc:reldate-get-symbol)
  (string gnc:reldate-get-string)
  (desc gnc:reldate-get-desc)
  (fn gnc:reldate-get-fn))

;; the globally available hash of reldates (hash-key = reldate
;; symbols, hash-value = a vector, reldate data).
(define gnc:relative-date-hash (make-hash-table))

(define (gnc:get-absolute-from-relative-date date-symbol)
  ;; used in options.scm
  (let ((rel-date-data (hash-ref gnc:relative-date-hash date-symbol)))
    (if rel-date-data
        ((gnc:reldate-get-fn rel-date-data))
        (let* ((msg (G_ "Tried to look up an undefined date symbol \
'~a'. This report was probably saved by a later version of GnuCash. \
Defaulting to today."))
               (conmsg (format #f msg date-symbol))
               (uimsg (format #f (G_ msg) date-symbol)))
          (gnc:gui-warn conmsg uimsg)
          (current-time)))))

(define (gnc:get-relative-date-string date-symbol)
  ;; used in options.scm
  (let ((rel-date-info (hash-ref gnc:relative-date-hash date-symbol)))
    (gnc:reldate-get-string rel-date-info)))

(define (gnc:get-relative-date-desc date-symbol)
  ;; used in options.scm
  (let ((rel-date-info (hash-ref gnc:relative-date-hash date-symbol)))
    (gnc:reldate-get-desc rel-date-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end relative-date functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:get-start-cal-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now 0)
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-cal-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (set-tm:mday now 31)
    (set-tm:mon now 11)
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-start-prev-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now 0)
    (set-tm:year now (- (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-prev-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (set-tm:mday now 31)
    (set-tm:mon now 11)
    (set-tm:year now (- (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-start-next-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now 0)
    (set-tm:year now (+ (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-next-year)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (set-tm:mday now 31)
    (set-tm:mon now 11)
    (set-tm:year now (+ (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-start-accounting-period)
  (gnc-accounting-period-fiscal-start))

(define (gnc:get-end-accounting-period)
  (gnc-accounting-period-fiscal-end))

(define (gnc:get-start-this-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-this-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59) 
    (set-tm:hour now 23)
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1) 
					(+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))
    
(define (gnc:get-start-prev-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (if (= (tm:mon now) 0)
	(begin 
	  (set-tm:mon now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:mon now (- (tm:mon now) 1)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-prev-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59) 
    (set-tm:hour now 23)
    (if (= (tm:mon now) 0)
	(begin
	  (set-tm:mon now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:mon now (- (tm:mon now) 1)))
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1) 
					(+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))
    
(define (gnc:get-start-next-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (if (= (tm:mon now) 11)
	(begin 
	  (set-tm:mon now 0)
	  (set-tm:year now (+ (tm:year now) 1)))
	(set-tm:mon now (+ (tm:mon now) 1)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-next-month)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59) 
    (set-tm:hour now 23)
    (if (= (tm:mon now) 11)
	(begin
	  (set-tm:mon now 0)
	  (set-tm:year now (+ (tm:year now) 1)))
	(set-tm:mon now (+ (tm:mon now) 1)))
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1) 
					(+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))
    
(define (gnc:get-start-current-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now (- (tm:mon now) (modulo (tm:mon now) 3)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-current-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (set-tm:mon now (+ (tm:mon now) 
		       (- 2 (modulo (tm:mon now) 3))))
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1)
                                        (+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-start-prev-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now (- (tm:mon now) (modulo (tm:mon now) 3)))
    (if (= (tm:mon now) 0)
	(begin
	  (set-tm:mon now 9)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:mon now (- (tm:mon now) 3)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-prev-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (if (< (tm:mon now) 3)
	(begin
	  (set-tm:mon now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:mon now (- (tm:mon now) 
			     (+ 1 (modulo (tm:mon now) 3)))))
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1)
                                        (+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-start-next-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (if (> (tm:mon now) 8)
	(begin
	  (set-tm:mon now 0)
	  (set-tm:year now (+ (tm:year now) 1)))
        (set-tm:mon now (+ (tm:mon now) (- 3 (modulo (tm:mon now) 3)))))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-end-next-quarter)
  (let ((now (gnc-localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (if (> (tm:mon now) 8)
	(begin
	  (set-tm:mon now 2)
	  (set-tm:year now (+ (tm:year now) 1)))
	(set-tm:mon now (+ (tm:mon now) 
			     (+ 1 (modulo (tm:mon now) 3)))))
    (set-tm:mday now (gnc:days-in-month (+ (tm:mon now) 1)
                                        (+ (tm:year now) 1900)))
    (set-tm:isdst now -1)
    (gnc-mktime now)))

(define (gnc:get-today)
  (current-time))

(define (gnc:get-one-month-ago)
  (let ((now (gnc-localtime (current-time))))
    (if (= (tm:mon now) 0)
	(begin
	  (set-tm:mon now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:mon now (- (tm:mon now) 1)))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-three-months-ago)
  (let ((now (gnc-localtime (current-time))))
    (if (< (tm:mon now) 3)
	(begin
	  (set:tm-mon now (+ (tm:mon now) 12))
	  (set:tm-year now  (- (tm:year now) 1))))
    (set:tm-mon now (- (tm:mon now) 3))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-six-months-ago)
  (let ((now (gnc-localtime (current-time))))
    (if (< (tm:mon now) 6)
	(begin
	  (set:tm-mon now (+ (tm:mon now) 12))
	  (set:tm-year now  (- (tm:year now) 1))))
    (set:tm-mon now (- (tm:mon now) 6))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-one-year-ago)
  (let ((now (gnc-localtime (current-time))))
    (set:tm-year now (- (tm:year now) 1))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-one-month-ahead)
  (let ((now (gnc-localtime (current-time))))
    (if (= (tm:mon now) 11)
	(begin
	  (set-tm:mon now 0)
	  (set-tm:year now (+ (tm:year now) 1)))
	(set-tm:mon now (+ (tm:mon now) 1)))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-three-months-ahead)
  (let ((now (gnc-localtime (current-time))))
    (if (> (tm:mon now) 8)
	(begin
	  (set:tm-mon now (- (tm:mon now) 9))
	  (set:tm-year now  (+ (tm:year now) 1))
          (set:tm-mon now (+ (tm:mon now) 3))))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-six-months-ahead)
  (let ((now (gnc-localtime (current-time))))
    (if (> (tm:mon now) 5)
	(begin
	  (set:tm-mon now (- (tm:mon now) 6))
	  (set:tm-year now  (+ (tm:year now) 1))
          (set:tm-mon now (+ (tm:mon now) 6))))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(define (gnc:get-one-year-ahead)
  (let ((now (gnc-localtime (current-time))))
    (set:tm-year now (+ (tm:year now) 1))
    (let ((month-days (gnc:days-in-month (+ (tm:mon now) 1)
                                         (+ (tm:year now) 1900))))
      (if (< month-days (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc-mktime now))))

(for-each
 (lambda (reldate)
   (hashq-set! gnc:relative-date-hash
               (gnc:reldate-get-symbol reldate)
               reldate))

 (list
  (make-reldate 'start-cal-year
                (G_ "Start of this year")
                (G_ "First day of the current calendar year.")
                gnc:get-start-cal-year)

  (make-reldate 'end-cal-year
                (G_ "End of this year")
                (G_ "Last day of the current calendar year.")
                gnc:get-end-cal-year)

  (make-reldate 'start-prev-year
                (G_ "Start of previous year")
                (G_ "First day of the previous calendar year.")
                gnc:get-start-prev-year)

  (make-reldate 'start-next-year
                (G_ "Start of next year")
                (G_ "First day of the next calendar year.")
                gnc:get-start-next-year)

  (make-reldate 'end-prev-year
                (G_ "End of previous year")
                (G_ "Last day of the previous calendar year.")
                gnc:get-end-prev-year)

  (make-reldate 'end-next-year
                (G_ "End of next year")
                (G_ "Last day of the next calendar year.")
                gnc:get-end-next-year)

  (make-reldate 'start-accounting-period
                (G_ "Start of accounting period")
                (G_ "First day of the accounting period, as set in the global preferences.")
                gnc:get-start-accounting-period)

  (make-reldate 'end-accounting-period
                (G_ "End of accounting period")
                (G_ "Last day of the accounting period, as set in the global preferences.")
                gnc:get-end-accounting-period)

  (make-reldate 'start-this-month
                (G_ "Start of this month")
                (G_ "First day of the current month.")
                gnc:get-start-this-month)

  (make-reldate 'end-this-month
                (G_ "End of this month")
                (G_ "Last day of the current month.")
                gnc:get-end-this-month)

  (make-reldate 'start-prev-month
                (G_ "Start of previous month")
                (G_ "First day of the previous month.")
                gnc:get-start-prev-month)

  (make-reldate 'end-prev-month
                (G_ "End of previous month")
                (G_ "Last day of previous month.")
                gnc:get-end-prev-month)

  (make-reldate 'start-next-month
                (G_ "Start of next month")
                (G_ "First day of the next month.")
                gnc:get-start-next-month)

  (make-reldate 'end-next-month
                (G_ "End of next month")
                (G_ "Last day of next month.")
                gnc:get-end-next-month)

  (make-reldate 'start-current-quarter
                (G_ "Start of current quarter")
                (G_ "First day of the current quarterly accounting period.")
                gnc:get-start-current-quarter)

  (make-reldate 'end-current-quarter
                (G_ "End of current quarter")
                (G_ "Last day of the current quarterly accounting period.")
                gnc:get-end-current-quarter)

  (make-reldate 'start-prev-quarter
                (G_ "Start of previous quarter")
                (G_ "First day of the previous quarterly accounting period.")
                gnc:get-start-prev-quarter)

  (make-reldate 'end-prev-quarter
                (G_ "End of previous quarter")
                (G_ "Last day of previous quarterly accounting period.")
                gnc:get-end-prev-quarter)

  (make-reldate 'start-next-quarter
                (G_ "Start of next quarter")
                (G_ "First day of the next quarterly accounting period.")
                gnc:get-start-next-quarter)

  (make-reldate 'end-next-quarter
                (G_ "End of next quarter")
                (G_ "Last day of next quarterly accounting period.")
                gnc:get-end-next-quarter)

  (make-reldate 'today
                (G_ "Today")
                (G_ "The current date.")
                gnc:get-today)

  (make-reldate 'one-month-ago
                (G_ "One Month Ago")
                (G_ "One Month Ago.")
                gnc:get-one-month-ago)

  (make-reldate 'three-months-ago
                (G_ "Three Months Ago")
                (G_ "Three Months Ago.")
                gnc:get-three-months-ago)

  (make-reldate 'six-months-ago
                (G_ "Six Months Ago")
                (G_ "Six Months Ago.")
                gnc:get-three-months-ago)

  (make-reldate 'one-year-ago
                (G_ "One Year Ago")
                (G_ "One Year Ago.")
                gnc:get-one-year-ago)

  (make-reldate 'one-month-ahead
                (G_ "One Month Ahead")
                (G_ "One Month Ahead.")
                gnc:get-one-month-ahead)

  (make-reldate 'three-months-ahead
                (G_ "Three Months Ahead")
                (G_ "Three Months Ahead.")
                gnc:get-three-months-ahead)

  (make-reldate 'six-months-ahead
                (G_ "Six Months Ahead")
                (G_ "Six Months Ahead.")
                gnc:get-three-months-ahead)

  (make-reldate 'one-year-ahead
                (G_ "One Year Ahead")
                (G_ "One Year Ahead.")
                gnc:get-one-year-ahead)))
