;; date-utilities.scm -- date utility functions.
;; Bryan Larsen (blarsen@ada-works.com)
;; Revised by Christopher Browne
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(gnc:support "date-utilities.scm")
(gnc:depend "srfi/srfi-19.scm")

(define gnc:reldate-list '())

(define (gnc:timepair->secs tp)
  (inexact->exact 
   (+ (car tp)
      (/ (cdr tp) 1000000000))))

(define (gnc:secs->timepair secs)
  (cons secs 0))

(define (gnc:timepair->date tp)
  (localtime (gnc:timepair->secs tp)))

;; get stuff from localtime date vector
(define (gnc:date-get-year datevec)
  (+ 1900 (tm:year datevec)))
(define (gnc:date-get-month-day datevec)
  (tm:mday datevec))
;; get month with january==1
(define (gnc:date-get-month datevec)
  (+ (tm:mon datevec) 1))
(define (gnc:date-get-week-day datevec)
  (+ (tm:wday datevec) 1))
;; jan 1 == 1

(define (gnc:date-get-year-day datevec)
  (+ (tm:yday datevec) 1))

(define (gnc:timepair-get-year tp)
  (gnc:date-get-year (gnc:timepair->date tp)))

(define (gnc:timepair-get-month-day tp)
  (gnc:date-get-month (gnc:timepair->date tp)))

(define (gnc:timepair-get-month tp)
  (gnc:date-get-month (gnc:timepair->date tp)))

(define (gnc:timepair-get-week-day tp)
  (gnc:date-get-week-day (gnc:timepair->date tp)))

(define (gnc:timepair-get-year-day tp)
  (gnc:date-get-year-day (gnc:timepair->date tp)))

(define (gnc:date-get-month-string datevec)
  (strftime "%B" datevec))

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
       (/ (- (gnc:date-get-year-day lt) 1.0)
	  (* 1.0 (gnc:days-in-year (gnc:date-get-year lt)))))))

;; return the number of years (in floating point format) between two dates.
(define (gnc:date-year-delta caltime1 caltime2)
  (let* ((lt1 (localtime caltime1))
	 (lt2 (localtime caltime2))
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

;; convert a date in seconds since 1970 into # of months since 1970
(define (gnc:date-to-month-fraction caltime)
  (let ((lt (localtime caltime)))
    (+ (* 12 (- (gnc:date-get-year lt) 1970.0))
       (gnc:date-get-month lt) -1
       (/ (- (gnc:date-get-month-day lt) 1.0) (gnc:days-in-month 
					       (gnc:date-get-month lt)
					       (gnc:date-get-year lt))))))

;; convert a date in seconds since 1970 into # of weeks since Jan 4, 1970
;; ignoring leap-seconds
(define (gnc:date-to-week-fraction caltime)
  (/ (- (/ (/ caltime 3600.0) 24) 3) 7))

;; convert a date in seconds since 1970 into # of days since Feb 28, 1970
;; ignoring leap-seconds
(define (gnc:date-to-day-fraction caltime)
  (- (/ (/ caltime 3600.0) 24) 59))

;; Modify a date
(define (moddate op adate delta)
  (let ((newtm (gnc:timepair->date adate)))
    (begin
      (set-tm:sec newtm (op (tm:sec newtm) (tm:sec delta)))
      (set-tm:min newtm (op (tm:min newtm) (tm:min delta)))
      (set-tm:hour newtm (op (tm:hour newtm) (tm:hour delta)))
      (set-tm:mday newtm (op (tm:mday newtm) (tm:mday delta)))
      (set-tm:mon newtm (op (tm:mon newtm) (tm:mon delta)))
      (set-tm:year newtm (op (tm:year newtm) (tm:year delta)))
      (set-tm:isdst newtm -1)
       (let ((time (car (mktime newtm))))
	 (cons time 0)))))

;; Add or subtract time from a date
(define (decdate adate delta)(moddate - adate delta ))
(define (incdate adate delta)(moddate + adate delta ))

;; Time comparison, true if t2 is later than t1
(define (gnc:timepair-later t1 t2)
  (cond ((< (car t1) (car t2)) #t)
        ((= (car t1) (car t2)) (< (cdr t2) (cdr t2)))
        (else #f)))

(define gnc:timepair-lt gnc:timepair-later)

(define (gnc:timepair-earlier t1 t2)
  (gnc:timepair-later t2 t1))

;; t1 <= t2
(define (gnc:timepair-le t1 t2)
  (cond ((< (car t1) (car t2)) #t)
        ((= (car t1) (car t2)) (<= (cdr t2) (cdr t2)))
        (else #f)))

(define (gnc:timepair-ge t1 t2)
  (gnc:timepair-le t2 t1))

(define (gnc:timepair-eq t1 t2)
  (and (= (car t1) (car t2)) (= (cdr t1) (cdr t2))))

;; date-granularity comparison functions.

(define (gnc:timepair-earlier-date t1 t2)
  (gnc:timepair-earlier (gnc:timepair-canonical-day-time t1)
			(gnc:timepair-canonical-day-time t2)))

(define (gnc:timepair-later-date t1 t2)
  (gnc:timepair-earlier-date t2 t1))

(define (gnc:timepair-le-date t1 t2)
  (gnc:timepair-le (gnc:timepair-canonical-day-time t1)
		   (gnc:timepair-canonical-day-time t2)))

(define (gnc:timepair-ge-date t1 t2)
  (gnc:timepair-le t2 t1))

(define (gnc:timepair-eq-date t1 t2)
  (gnc:timepair-eq (gnc:timepair-canonical-day-time t1)
		   (gnc:timepair-canonical-day-time t2)))

;; Build a list of time intervals. 
;;
;; Note that the last interval will be shorter than <incr> if
;; (<curd>-<endd>) is not an integer multiple of <incr>. If you don't
;; want that you'll have to write another function.
(define (gnc:dateloop curd endd incr) 
  (cond ((gnc:timepair-later curd endd)
	 (let ((nextd (incdate curd incr)))
	   (cond ((gnc:timepair-later nextd endd)
		  (cons (list curd (decdate nextd SecDelta) '())
			(gnc:dateloop nextd endd incr)))
		  (else (cons (list curd endd '()) '())))))
	(else '())))

; A reference zero date - the Beginning Of The Epoch
; Note: use of eval is evil... by making this a generator function, 
; each delta function gets its own instance of Zero Date
(define (make-zdate) 
  (let ((zd (localtime 0)))
    (set-tm:hour zd 0)
    (set-tm:min zd 0)
    (set-tm:sec zd 0)
    (set-tm:mday zd 0)
    (set-tm:mon zd 0)
    (set-tm:year zd 0)
    (set-tm:yday zd 0)
    (set-tm:wday zd 0)
    (set-tm:isdst zd -1)
    zd))

(define SecDelta 
  (let ((ddt (make-zdate)))
    (set-tm:sec ddt 1)
    ddt))

(define YearDelta 
  (let ((ddt (make-zdate)))
    (set-tm:year ddt 1)
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

;; Find difference in seconds time 1 and time2
(define (gnc:timepair-delta t1 t2)
  (- (gnc:timepair->secs t2) (gnc:timepair->secs t1)))

;; find float difference between times 
(define (gnc:time-elapsed t1 t2)
  (+ (- (car t2)
        (car t1))
     (/ (- (cdr t2)
           (cdr t1)) 1000000.0)))

;; timepair manipulation functions
;; hack alert  - these should probably be put somewhere else
;; and be implemented PROPERLY rather than hackily
;;; Added from transaction-report.scm

(define (gnc:timepair-to-datestring tp)
  (let ((bdtime (gnc:timepair->date tp)))
    (strftime "%x" bdtime)))

;; given a timepair contains any time on a certain day (local time)
;; converts it to be midday that day.

(define (gnc:timepair-canonical-day-time tp)
  (let ((bdt (gnc:timepair->date tp)))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 12)
    (set-tm:isdst bdt -1)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))

(define (gnc:timepair-start-day-time tp)
  (let ((bdt (gnc:timepair->date tp)))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 0)
    (set-tm:isdst bdt -1)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))

(define (gnc:timepair-end-day-time tp)
  (let ((bdt (gnc:timepair->date tp)))
    (set-tm:sec bdt 59)
    (set-tm:min bdt 59)
    (set-tm:hour bdt 23)
    (set-tm:isdst bdt -1)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))

(define (gnc:timepair-previous-day tp)
  (decdate tp DayDelta))

(define (gnc:reldate-get-symbol x) (vector-ref x 0))
(define (gnc:reldate-get-string x) (vector-ref x 1))
(define (gnc:reldate-get-desc x) (vector-ref x 2))
(define (gnc:reldate-get-fn x) (vector-ref x 3))

(define (gnc:make-reldate-hash hash reldate-list)
  (map (lambda (reldate) (hash-set! 
			  hash 
			  (gnc:reldate-get-symbol reldate)
			  reldate))
       reldate-list))

(define gnc:reldate-string-db (gnc:make-string-database))

(define gnc:relative-date-values '())

(define gnc:relative-date-hash (make-hash-table 23))

(define (gnc:get-absolute-from-relative-date date-symbol)
  (let ((rel-date-data (hash-ref gnc:relative-date-hash date-symbol)))
    (if rel-date-data
       ((gnc:reldate-get-fn rel-date-data))
       (gnc:error "Tried to look up an undefined date symbol"))))

(define (gnc:get-relative-date-strings date-symbol)
  (let ((rel-date-info (hash-ref gnc:relative-date-hash date-symbol)))
    
    (cons (gnc:reldate-get-string rel-date-info)
	  (gnc:relate-get-desc rel-date-info))))

(define (gnc:get-relative-date-string date-symbol)
  (let ((rel-date-info (hash-ref gnc:relative-date-hash date-symbol)))
    (gnc:reldate-get-string rel-date-info)))

(define (gnc:get-relative-date-desc date-symbol)
  (let ((rel-date-info (hash-ref gnc:relative-date-hash date-symbol)))
    (gnc:reldate-get-desc rel-date-info)))

(define (gnc:get-start-cal-year)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now 0)
    (set-tm:isdst now -1)
    (gnc:secs->timepair (car (mktime now)))))

(define (gnc:get-start-prev-year)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:mon now 0)
    (set-tm:year now (- (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc:secs->timepair (car (mktime now)))))

(define (gnc:get-end-prev-year)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (set-tm:mday now 31)
    (set-tm:mon now 11)
    (set-tm:year now (- (tm:year now) 1))
    (set-tm:isdst now -1)
    (gnc:secs->timepair (car (mktime now)))))

;; FIXME:: Replace with option when it becomes available
(define (gnc:get-start-cur-fin-year)
  (let ((now (localtime (current-time))))
    (if (< (tm:mon now) 6)
	(begin
	  (set-tm:sec now 0)
	  (set-tm:min now 0)
	  (set-tm:hour now 0)
	  (set-tm:mday now 1)
	  (set-tm:mon now 6)
	  (set-tm:year now (- (tm:year now) 1))
          (set-tm:isdst now -1)
	  (gnc:secs->timepair (car (mktime now))))
	(begin
	  (set-tm:sec now 0)
	  (set-tm:min now 0)
	  (set-tm:hour now 0)
	  (set-tm:mday now 1)
	  (set-tm:mon now 6)
          (set-tm:isdst now -1)
	  (gnc:secs->timepair (car (mktime now)))))))

(define (gnc:get-start-prev-fin-year)
  (let ((now (localtime (current-time))))
    (if (< (tm:mon now) 6)
	(begin
	  (set-tm:sec now 0)
	  (set-tm:min now 0)
	  (set-tm:hour now 0)
	  (set-tm:mday now 1)
	  (set-tm:mon now 6)
	  (set-tm:year now (- (tm:year now) 2))
          (set-tm:isdst now -1)
	  (cons (car (mktime now)) 0))
	(begin
	  (set-tm:sec now 0)
	  (set-tm:min now 0)
	  (set-tm:hour now 0)
	  (set-tm:mday now 1)
	  (set-tm:mon now 6)
	  (set-tm:year now (- (tm:year now) 2))
          (set-tm:isdst now -1)
	  (cons (car (mktime now)) 0)))))

(define (gnc:get-end-prev-fin-year)
  (let ((now (localtime (current-time))))
    (if (< (tm:mon now) 6)
	(begin
	  (set-tm:sec now 59)
	  (set-tm:min now 59)
	  (set-tm:hour now 23)
	  (set-tm:mday now 30)
	  (set-tm:mon now 5)
          (set-tm:isdst now -1)
	  (cons (car (mktime now)) 0))
	(begin
	  (set-tm:sec now 59)
	  (set-tm:min now 59)
	  (set-tm:hour now 23)
	  (set-tm:mday now 30)
	  (set-tm:mon now 5)
	  (set-tm:year now (- (tm:year now) 1))
          (set-tm:isdst now -1)
	  (cons (car (mktime now)) 0)))))

(define (gnc:get-start-this-month)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:isdst now -1)
    (cons (car (mktime now)) 0)))

(define (gnc:get-start-prev-month)
  (let ((now (localtime (current-time))))
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
    (cons (car (mktime now)) 0)))

(define (gnc:get-end-prev-month)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59) 
    (set-tm:hour now 23)
    (if (= (tm:month now 0))
	(begin
	  (set-tm:month now 11)
	  (set-tm:year (- (tm:year now) 1)))
	(set-tm:month now (- (tm:month now) 1)))
    (set-tm:mday (gnc:days-in-month (+ (tm:month now) 1)) (+ (tm:year) 1900))
    (set-tm:isdst now -1)
    (cons (car (mktime now)) 0)))
    
(define (gnc:get-start-current-quarter)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:month now (- (tm:month now) (mod (tm:month now) 3)))
    (set-tm:isdst now -1)
    (cons (car (mktime now)) 0)))

(define (gnc:get-start-prev-quarter)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 0)
    (set-tm:min now 0)
    (set-tm:hour now 0)
    (set-tm:mday now 1)
    (set-tm:month now (- (tm:month now)  (mod (tm:month now) 3)))
    (if (= (tm:month now) 0)
	(begin
	  (set-tm:month now 9)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:month now (- (tm-month now) 3)))
    (set-tm:isdst now -1)
    (cons (car (mktime now) 0))))

(define (gnc:get-end-prev-quarter)
  (let ((now (localtime (current-time))))
    (set-tm:sec now 59)
    (set-tm:min now 59)
    (set-tm:hour now 23)
    (if (< (tm:month now) 3)
	(begin
	  (set-tm:month now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:month now (- (tm:month now) 
			     (3 + (mod (tm:month now) 3)))))
    (set-tm:mday now (gnc:days-in-month (+ (tm:month now) 1)
                                        (+ (tm:year) 1900)))
    (set-tm:isdst now -1)
    (gnc:secs->timepair (car (mktime now)))))

(define (gnc:get-today)
  (cons (current-time) 0))

(define (gnc:get-one-month-ago)
  (let ((now (localtime (current-time))))
    (if (= (tm:month now) 0)
	(begin
	  (set-tm:month now 11)
	  (set-tm:year now (- (tm:year now) 1)))
	(set-tm:month now (- (tm:month now) 1)))
    (let ((month-length (gnc:days-in-month (+ (tm:month now) 1)
                                           (+ (tm:year now) 1900))))
      (if (> month-length (tm:mday now))
	  (set-tm:mday now month-length))
      (set-tm:isdst now -1)
     (gnc:secs->timepair (car (mktime now))))))

(define (gnc:get-three-months-ago)
  (let ((now (localtime (current-time))))
    (if (< (tm:month now) 3)
	(begin
	  (set:tm-month now (+ (tm:month now) 12))
	  (set:tm-year now  (- (tm:year now) 1))))
    (set:tm-month now (- (tm:month now) 3))
    (let ((month-days) (gnc:days-in-month (+ (tm:month now) 1)
                                          (+ (tm:year now) 1900)))
      (if (> (month-days) (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc:secs->timepair (car (mktime now))))))

(define (gnc:get-six-months-ago)
  (let ((now (localtime (current-time))))
    (if (< (tm:month now) 6)
	(begin
	  (set:tm-month now (+ (tm:month now) 12))
	  (set:tm-year now  (- (tm:year now) 1))))
    (set:tm-month now (- (tm:month now) 6))
    (let ((month-days) (gnc:days-in-month (+ (tm:month now) 1)
                                          (+ (tm:year now) 1900)))
      (if (> (month-days) (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc:secs->timepair (car (mktime now))))))

(define (gnc:get-one-year-ago)
  (let ((now (localtime (current-time))))
    (set:tm-year now (- (tm:year now) 1))
    (let ((month-days) (gnc:days-in-month (+ (tm:month now) 1)
                                          (+ (tm:year now) 1900)))
      (if (> (month-days) (tm:mday now))
	  (set-tm:mday now month-days))
      (set-tm:isdst now -1)
      (gnc:secs->timepair (car (mktime now))))))

(define (gnc:reldate-initialize)
  (begin
    (gnc:reldate-string-db 
     'store 'start-cal-year-string 
     (N_ "Current Year Start"))
    (gnc:reldate-string-db 
     'store 'start-cal-year-desc 
     (N_ "Start of the current calendar year"))
    (gnc:reldate-string-db 
     'store 'start-prev-year-string 
     (N_ "Previous Year Start"))
    (gnc:reldate-string-db 
     'store 'start-prev-year-desc 
     (N_ "Beginning of the previous calendar year"))
    (gnc:reldate-string-db 
     'store 'end-prev-year-string 
     (N_ "Previous Year End"))
    (gnc:reldate-string-db 
     'store 'end-prev-year-desc 
     (N_ "End of the Previous Year"))
    (gnc:reldate-string-db 
     'store 'start-cur-fin-year-string 
     (N_ "Current Financial Year Start"))
    (gnc:reldate-string-db 
     'store 'start-cur-fin-year-desc 
     (N_ "Start of the current financial year/accounting period"))
    (gnc:reldate-string-db 
     'store 'start-prev-fin-year-string 
     (N_ "Previous Financial Year Start"))
    (gnc:reldate-string-db 
     'store 'start-prev-fin-year-desc 
     (N_ "The start of the previous financial year/accounting period"))
    (gnc:reldate-string-db 
     'store 'end-prev-fin-year-string 
     (N_ "End Previous Financial Year"))
    (gnc:reldate-string-db 
     'store 'end-prev-fin-year-desc 
     (N_ "End of the previous Financial year/Accounting Period"))
    (gnc:reldate-string-db 
     'store 'start-this-month-string 
     (N_ "Start of this month"))
    (gnc:reldate-string-db 
     'store 'start-this-month-desc 
     (N_ "Start of the current month"))
    (gnc:reldate-string-db 
     'store 'start-prev-month-string 
     (N_ "Start of previous month"))
    (gnc:reldate-string-db 
     'store 'start-prev-month-desc
     (N_ "The beginning of the previous month"))
    (gnc:reldate-string-db 
     'store 'end-prev-month-string 
     (N_ "End of previous month"))
    (gnc:reldate-string-db 
     'store 'end-prev-month-desc
     (N_ "Last day of previous month"))
    (gnc:reldate-string-db 
     'store 'start-current-quarter-string 
     (N_ "Start of current quarter"))
    (gnc:reldate-string-db 
     'store 'start-current-quarter-desc
     (N_ "The start of the latest quarterly accounting period"))
    (gnc:reldate-string-db 
     'store 'start-prev-quarter-string 
     (N_ "Start of previous quarter"))
    (gnc:reldate-string-db 
     'store 'start-prev-quarter-desc
     (N_ "The start of the previous quarterly accounting period"))
    (gnc:reldate-string-db 
     'store 'end-prev-quarter-string 
     (N_ "End of previous quarter"))
    (gnc:reldate-string-db 
     'store 'end-prev-quarter-desc 
     (N_ "End of previous quarterly accounting period"))
    (gnc:reldate-string-db 
     'store 'today-string 
     (N_ "Today"))
    (gnc:reldate-string-db 
     'store 'today-desc (N_ "The current date"))
    (gnc:reldate-string-db 
     'store 'one-month-ago-string 
     (N_ "One Month Ago"))
    (gnc:reldate-string-db 
     'store 'one-month-ago-desc (N_ "One Month Ago"))
    (gnc:reldate-string-db 
     'store 'one-week-ago-string 
     (N_ "One Week Ago"))
    (gnc:reldate-string-db 
     'store 'one-week-ago-desc (N_ "One Week Ago"))
    (gnc:reldate-string-db 
     'store 'three-months-ago-string 
     (N_ "Three Months Ago"))
    (gnc:reldate-string-db 
     'store 'three-months-ago-desc (N_ "Three Months Ago"))
    (gnc:reldate-string-db 
     'store 'six-months-ago-string 
     (N_ "Six Months Ago"))
    (gnc:reldate-string-db 
     'store 'six-months-ago-desc (N_ "Six Months Ago"))
    (gnc:reldate-string-db 
     'store 'one-year-ago-string (N_ "One Year Ago"))
    (gnc:reldate-string-db 
     'store 'one-year-ago-desc (N_ "One Year Ago")) 

    (set! gnc:relative-date-values 
	  (list 
	   (vector 'start-cal-year 
		   (gnc:reldate-string-db 'lookup 'start-cal-year-string)
		   (gnc:reldate-string-db 'lookup 'start-cal-year-desc)
		   gnc:get-start-cal-year)
	   (vector 'start-prev-year
		   (gnc:reldate-string-db 'lookup 'start-prev-year-string)
		   (gnc:reldate-string-db 'lookup 'start-prev-year-desc)
		   gnc:get-start-prev-year)
	   (vector 'end-prev-year
		   (gnc:reldate-string-db 'lookup 'end-prev-year-string)
		   (gnc:reldate-string-db 'lookup 'end-prev-year-desc)
		   gnc:get-end-prev-year)
	   (vector 'start-cur-fin-year
		   (gnc:reldate-string-db 'lookup 'start-cur-fin-year-string)
		   (gnc:reldate-string-db 'lookup 'start-cur-fin-year-desc)
		   gnc:get-start-cur-fin-year)
	   (vector 'start-prev-fin-year
		   (gnc:reldate-string-db 'lookup 'start-prev-fin-year-string)
		   (gnc:reldate-string-db 'lookup 'start-prev-fin-year-desc)
		   gnc:get-start-prev-fin-year)
	   (vector 'end-prev-fin-year
		   (gnc:reldate-string-db 'lookup 'end-prev-fin-year-string)
		   (gnc:reldate-string-db 'lookup 'end-prev-fin-year-desc)
		   gnc:get-end-prev-fin-year)
	   (vector 'start-this-month
		   (gnc:reldate-string-db 'lookup 'start-this-month-string)
		   (gnc:reldate-string-db 'lookup 'start-this-month-desc)
		   gnc:get-start-this-month)
	   (vector 'start-prev-month
		   (gnc:reldate-string-db 'lookup 'start-prev-month-string)
		   (gnc:reldate-string-db 'lookup 'start-prev-month-desc)
		   gnc:get-start-prev-month)
	   (vector 'end-prev-month
		   (gnc:reldate-string-db 'lookup 'end-prev-month-string)
		   (gnc:reldate-string-db 'lookup 'end-prev-month-desc)
		   gnc:get-end-prev-month)
	   (vector 'start-current-quarter
		   (gnc:reldate-string-db 'lookup 'start-current-quarter-string)
		   (gnc:reldate-string-db 'lookup 'start-current-quarter-desc)
		   gnc:get-start-current-quarter)
	   (vector 'start-prev-quarter
		   (gnc:reldate-string-db 'lookup 'start-prev-quarter-string)
		   (gnc:reldate-string-db 'lookup 'start-prev-quarter-desc)
		   gnc:get-start-prev-quarter)
	   (vector 'end-prev-quarter
		   (gnc:reldate-string-db 'lookup 'end-prev-quarter-string)
		   (gnc:reldate-string-db 'lookup 'end-prev-quarter-desc)
		   gnc:get-end-prev-quarter)
	   (vector 'today
		   (gnc:reldate-string-db 'lookup 'end-prev-quarter-string)
		   (gnc:reldate-string-db 'lookup 'end-prev-quarter-desc)
		   gnc:get-today)
	   (vector 'one-month-ago
		   (gnc:reldate-string-db 'lookup 'one-month-ago-string)
		   (gnc:reldate-string-db 'lookup 'one-month-ago-desc)
		   gnc:get-one-month-ago)
	   (vector 'three-months-ago
		   (gnc:reldate-string-db 'lookup 'three-months-ago-string)
		   (gnc:reldate-string-db 'lookup 'three-months-ago-desc)
		   gnc:get-three-months-ago)
	   (vector 'six-months-ago
		   (gnc:reldate-string-db 'lookup 'six-months-ago-string)
		   (gnc:reldate-string-db 'lookup 'six-months-ago-desc)
		   gnc:get-three-months-ago)
	   (vector 'one-year-ago
		   (gnc:reldate-string-db 'lookup 'one-year-ago-string)
		   (gnc:reldate-string-db 'lookup 'one-year-ago-desc)
		   gnc:get-one-year-ago)))


    (gnc:make-reldate-hash gnc:relative-date-hash gnc:relative-date-values)
    (set! gnc:reldate-list
          (map (lambda (x) (vector-ref x 0)) gnc:relative-date-values))))

;; Startup
(let ((hook (gnc:hook-lookup 'startup-hook)))
  (gnc:hook-add-dangler hook gnc:reldate-initialize))
