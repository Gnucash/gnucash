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

(gnc:support "dateutils.scm")
(gnc:depend "srfi/srfi-19.scm")

;; get stuff from localtime date vector
(define (gnc:date-get-year datevec)
  (+ 1900 (vector-ref datevec 5)))
(define (gnc:date-get-month-day datevec)
  (vector-ref datevec 3))
;; get month with january==1
(define (gnc:date-get-month datevec)
  (+ (vector-ref datevec 4) 1))
(define (gnc:date-get-week-day datevec)
  (+ (vector-ref datevec 6) 1))
;; jan 1 == 1
(define (gnc:date-get-year-day datevec)
  (+ (vector-ref datevec 7) 1))

;; fixme: internationalize
(define (gnc:date-get-month-string datevec)
  (case (gnc:date-get-month datevec)
    ((1) "January")
    ((2) "February")
    ((3) "March")
    ((4) "April")
    ((5) "May")
    ((6) "June")
    ((7) "July")
    ((8) "August")
    ((9) "September")
    ((10) "October")
    ((11) "November")
    ((12) "December")))

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
  (let ((newtm (localtime (car adate))))
    (begin
      (set-tm:sec newtm (op (tm:sec newtm) (tm:sec delta)))
      (set-tm:min newtm (op (tm:min newtm) (tm:min delta)))
      (set-tm:hour newtm (op (tm:hour newtm) (tm:hour delta)))
      (set-tm:mday newtm (op (tm:mday newtm) (tm:mday delta)))
      (set-tm:mon newtm (op (tm:mon newtm) (tm:mon delta)))
      (set-tm:year newtm (op (tm:year newtm) (tm:year delta)))

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

;; t1 <= t2
(define (gnc:timepair-le t1 t2)
  (cond ((< (car t1) (car t2)) #t)
        ((= (car t1) (car t2)) (<= (cdr t2) (cdr t2)))
        (else #f)))

(define (gnc:timepair-eq t1 t2)
  (and (= (car t1) (car t2)) (= (cdr t1) (cdr t2))))

;; Build a list of time intervals 
(define (dateloop curd endd incr) 
  (cond ((gnc:timepair-later curd endd)
	 (let ((nextd (incdate curd incr)))
	 (cons (list curd (decdate nextd SecDelta) '())
	       (dateloop nextd endd incr))))
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

(define (gnc:timepair->secs tp)
  (inexact->exact
   (+ (car tp)
      (/ (cdr tp) 1000000000))))

;; Find difference in seconds time 1 and time2
(define (gnc:timepair-delta t1 t2)
  (- (gnc:timepair->secs t2) (gnc:timepair->secs t1)))

;; timepair manipulation functions
;; hack alert  - these should probably be put somewhere else
;; and be implemented PROPERLY rather than hackily
;;; Added from transaction-report.scm

(define (gnc:timepair-to-datestring tp)
  (let ((bdtime (localtime (car tp))))
    (strftime "%x" bdtime)))

;; given a timepair contains any time on a certain day (local time)
;; converts it to be midday that day.

(define (gnc:timepair-canonical-day-time tp)
  (let ((bdt (localtime (gnc:timepair->secs tp))))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 12)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))

(define (gnc:timepair-start-day-time tp)
  (let ((bdt (localtime (gnc:timepair->secs tp))))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 0)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))

(define (gnc:timepair-end-day-time tp)
  (let ((bdt (localtime (gnc:timepair->secs tp))))
    (set-tm:sec bdt 59)
    (set-tm:min bdt 59)
    (set-tm:hour bdt 23)
    (let ((newtime (car (mktime bdt))))
      (cons newtime 0))))
