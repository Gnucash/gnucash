(gnc:support "srfi/srfi-19.scm")   ;;; For purposes of GnuCash...
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 2000 Neodesic Corporation; 
;;              All rights reserved.
;;
;; File:        common-time.scm
;; Created:     January 2000
;; Author:      Will Fitzgerald
;; 
;; Description: Implementation of Common Time Data Types and Procedures
;;              
;;
;; MzScheme specific, and depends on the following procedures:
;; 
;; current-seconds
;; seconds->date
;; the date record and associated accessors
;; current-process-milliseconds)
;; current-milliseconds
;; 
;; The following procedures were useful:
;; 
;; error
;; open-output-string
;; get-output-string
;;
;; I attempted to minimize non-public procedures and constants,
;; but the following remain:
;;
;; JDN-1900-01-01
;; DOW-1900-01-01
;; normalize-year 
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

;; MzScheme specific; we can define this only as a parameter.
;(define current-time-zone (make-parameter -5))
(define current-time-zone (lambda () -5))

;; This is based on the algorithm described in
;; http://www.tondering.dk/claus/calendar.html
;; 

(define (encode-julian-day-number day month year)
  (if (< year 1)
      (error "can only convert Common Era (i.e., AD) dates.")
      (let* ((a (quotient (- 14 month) 12))
             (y (- (+ year 4800) a))
             (m (- (+ month (* 12 a)) 3)))
        (+ day
           (quotient (+ (* 153 m) 2) 5)
           (* 365 y)
           (quotient y 4)
           (- (quotient y 100))
           (quotient y 400)
           (- 32045)))))

(define (decode-julian-day-number jdn)
  (if (< jdn 1721426)
      (error "can only convert Common Era (i.e., AD) dates.")
      (let* ((a (+ jdn 32044))
             (b (quotient (+ (* 4 a) 3) 146097))
             (c (- a (quotient (* 146097 b) 4)))
             (d (quotient (+ (* 4 c) 3) 1461))
             (e (- c (quotient (* 1461 d) 4)))
             (m (quotient (+ (* 5 e) 2) 153)))
        (values				; date month year
         (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
         (+ m 3 (* -12 (quotient m 10)))
         (+ (* 100 b) d -4800 (quotient m 10))))))

(define JDN-1900-01-01 (encode-julian-day-number 1 1 1900)) ; non-public
(define DOW-1900-01-01 6) 

;;; Code for MzScheme
; (define (get-encoded-time)
;   (let ((date (seconds->date (current-seconds))))
;     (values
;      (date-second date)
;      (date-minute date)
;      (date-hour date)
;      (date-day date)
;      (date-month date)
;      (date-year date)
;      (date-week-day date)
;      (date-dst? date)
;      (current-time-zone))))
;;; (22 33 17 13 3 2000 1 #f)

(define (get-encoded-time)   ;;; For Guile
  (let 
      ((now (localtime (current-time))))
    (values (tm:sec now)
	    (tm:min now) 
	    (tm:hour now)
	    (tm:mday now)
	    (tm:mon now)
	    (+ 1900 (tm:year now))
	    (tm:wday now)
	    (> (tm:isdst now) 0)
	    (/ (tm:gmtoff now) 3600))))

(define (get-julian-day-number)
  (call-with-values
   get-encoded-time
   (lambda (second minute hour day month year week-day dst? time-zone)
     (encode-julian-day-number day month year))))

;; number of seconds since midnight, January 1, 1900, UTC
(define (get-universal-time) 
  (call-with-values
   get-encoded-time
   (lambda (second minute hour day month year week-day dst? time-zone)
     (let ((jdn-now (encode-julian-day-number day month year)))
       (+ (* (- jdn-now JDN-1900-01-01) 86400)
	  (* hour 3600)
	  (* minute 60)
	  second
	  (if dst? -3600 0)
	  (* time-zone 3600))))))

(define (encode-universal-time second minute hour date month year . time-zone)
  (if (< year 1900)
      (error "Universal time is not defined for year:" year)
      (call-with-values
       (lambda ()
	 (get-encoded-time))
       (lambda (csecond cminute chour cday cmonth 
			cyear cweek-day cdst? ctime-zone)
	 (let ((dst? (if (pair? time-zone) #f cdst?))
	       (time-zone (if (pair? time-zone) (car time-zone) ctime-zone))
	       (jdn (encode-julian-day-number date month 
					      (normalize-year year cyear))))
	   (+ (* (- jdn JDN-1900-01-01) 86400)
	      (* hour 3600)
	      (* minute 60)
	      second
	      (if dst? -3600 0)
	      (* time-zone 3600)))))))

;; handles negative numbers fine--just like MCL!
(define (decode-universal-time ut-time . time-zone)
  (call-with-values
   (lambda ()
     (get-encoded-time))
   (lambda (csecond cminute chour cday cmonth 
		    cyear cweek-day cdst? ctime-zone)
     (let 
	 ((dst? (if (pair? time-zone) #f cdst?))
	  (time-zone (if (pair? time-zone) (car time-zone) ctime-zone)))
       (let* 
	   ((adj-ut-time (+ ut-time (if dst? 3600 0) 
			    (if time-zone (- (* time-zone 3600)) 0)))
	    (jdn (+ (quotient adj-ut-time 86400) JDN-1900-01-01)))
	 (call-with-values
	  (lambda ()
	    (decode-julian-day-number jdn))
	  (lambda (day month year)
	    (let ((second (modulo adj-ut-time 86400)))
	      (let ((hour (quotient second 3600))
		    (second (modulo second 3600)))
		(let ((minute (quotient second 60))
		      (second (modulo second 60)))
		  (values second minute hour day month year 
			  (modulo (+ jdn DOW-1900-01-01 1) 7) dst? 
			  time-zone)))))))))))

;;; MzScheme require these:
;(define internal-time-units-per-second 1000)
;(define get-internal-run-time current-process-milliseconds)
;(define get-internal-real-time current-milliseconds)

; Guile has all three values as built-ins (or, at least, equivalents
; thereof...

(define (output-time-zone time-zone port)
  (if (= time-zone 0)
      (display #\Z port)
      (begin
	(if (< time-zone 0) 
	    (begin 
	      (display #\- port)
	      (set! time-zone 
		    (- time-zone)))
	    (display #\+ port))
	(let 
	    ((tdz-raw-minutes (inexact->exact (* time-zone 60))))
	  (let
	      ((tdz-hours (quotient tdz-raw-minutes 60))
	       (tdz-minutes (remainder tdz-raw-minutes 60)))
	    (display-2-digits tdz-hours port)
	    (display #\: port)
	    (display-2-digits tdz-minutes port))))))

(define (display-2-digits value port)
  (display (quotient value 10) port)
  (display (modulo value 10) port))

(define (display-4-digits value port)
  (display (quotient value 1000) port)
  (display (quotient (modulo value 1000) 100) port)
  (display-2-digits (modulo value 100) port))

(define (erriso field index string)
  (let ((msg (string-append "Invalid ISO date string ["
			    field "]")))
    (error msg index string)))

(define (erriso2 field something index string)
  (let ((msg (string-append "Invalid ISO date string ["
			    field "]")))
    (error msg something index string)))

(define (char-to-digit ch i)
  (cond
   ((char=? ch #\0) 0)
   ((char=? ch #\1) 1)
   ((char=? ch #\2) 2)
   ((char=? ch #\3) 3)
   ((char=? ch #\4) 4)
   ((char=? ch #\5) 5)
   ((char=? ch #\6) 6)
   ((char=? ch #\7) 7)
   ((char=? ch #\8) 8)
   ((char=? ch #\9) 9)
   (else (error "Non-integer character" ch i))))

;; non-public procedure
(define (normalize-year year nowyear)
  (if (< year 100)
      (if (< year 50)
          (+ nowyear year)
          (+ (- nowyear 100) year))
      year))

(define (universal-time->string universal-time . time-zone)
  (call-with-values 
   (lambda ()
     (apply decode-universal-time universal-time time-zone))
   (lambda (second minute hour day month year day-of-week dst? time-zone)
     (let 
	 ((str 
	   (call-with-output-string 
	    (lambda (str)
	      (display-4-digits year str)
	      (display #\- str)
	      (display-2-digits month str)
	      (display #\- str)
	      (display-2-digits day str)
	      (display #\T str)
	      (display-2-digits hour str)
	      (display #\: str)
	      (display-2-digits minute str)
	      (display #\: str)
	      (display-2-digits second str)
	      (output-time-zone time-zone str)))))
	   (display str)
	   (newline)
	   str))))

(define (universal-time->date-string universal-time . time-zone)
  (call-with-values 
   (lambda ()
     (apply decode-universal-time universal-time time-zone))
   (lambda (second minute hour day month year day-of-week dst? time-zone)
     (let ((str (open-output-string)))
       (display-4-digits year str)
       ;;
       (display #\- str)
       (display-2-digits month str)
       (display #\- str)
       (display-2-digits day str)
       (get-output-string str)))))

(define (universal-time->time-string universal-time . time-zone)
  (call-with-values 
   (lambda ()
     (apply decode-universal-time universal-time time-zone))
   (lambda (second minute hour day month year day-of-week dst? time-zone)
     (let ((str (open-output-string)))
       (display-2-digits hour str)
       (display #\: str)
       (display-2-digits minute str)
       (display #\: str)
       (display-2-digits second str)
       (get-output-string str)))))

         
(define (string->universal-time string)
  (let ((cstring (string-copy string)))
    ;; remove non-numerics
    (do ((index 0 (+ index 1)))
        ((>= index (string-length cstring)))
      (let ((ch (string-ref cstring index)))
        (if (not (or (char-numeric? ch)
                     (char=? ch #\.)))
            (string-set! cstring index #\Space))))
    (let ((nums (read-from-string-all cstring)))
      (let ((second (sixth nums))
            (minute (fifth nums))
            (hour   (fourth nums))
            (date   (third nums))
            (month  (second nums))
            (year   (first nums))
            (time-zone (seventh nums)))
        (encode-universal-time second minute hour date 
			       month year time-zone)))))

(define (string->universal-time string)
  (let ((index 0)
	(len (string-length string))
	(char->int 
	 (lambda (i)
	   (let ((ch (string-ref string i)))
	     (char-to-digit ch i)))))
    (let
	((accumulate-int
	  (lambda ()
	    (if (or (>= index len)
		    (not (char-numeric? (string-ref string index))))
		#f
		(do ((acc (char->int index) (+ (* acc 10) (char->int index))))
		    ((or (>= (+ index 1) len)
			 (not (char-numeric? (string-ref string (+ index 1)))))
		     (begin (set! index (+ index 1)) 
			    acc))
		  (set! index (+ index 1))))))
	 (accumulate-frac
	  (lambda ()
	    (if (or (>= index len)
		    (not (char-numeric? (string-ref string index))))
		#f
		(do ((acc (/ (char->int index) 10) 
			  (+ acc (/ (char->int index) dix)))
		     (dix 100 (* dix 10)))
		    ((or (>= (+ index 1) len)
			 (not (char-numeric? 
			       (string-ref string (+ index 1)))))
		     (begin 
		       (set! index (+ index 1)) 
		       acc))
		  (set! index (+ index 1)))))))
      (call-with-values
       get-encoded-time
       (lambda (csecond cminute chour cday cmonth cyear
			cweek-day cdst? ctime-zone)                 
	 (let ((second #f) (minute #f) (hour #f) (day #f) 
	       (month #f) (year #f) (tzhour #f) (tzminute #f))
	   (set! year (accumulate-int))
	   (if (eq? year #f) 
	       (erriso "year" index string))
	   (set! year (normalize-year year cyear))
	   (set! index (+ index 1))
	   (set! month (accumulate-int))
	   (if (eq? month #f)
	       (erriso "month" index string))
	   (set! index (+ index 1))
	   (set! day (accumulate-int))
	   (if (eq? day #f) 
	       (erriso "day" index string))
	   (set! index (+ index 1))
	   (set! hour (accumulate-int))
	   (if (eq? hour #f) 
	       (erriso "hour" index string))
	   (set! index (+ index 1))
	   (set! minute (accumulate-int))
	   (if (eq? minute #f) 
	       (erriso "minute" index string))
	   (set! index (+ index 1))
	   (set! second (accumulate-int))
	   (if (eq? second #f) 
	       (erriso "second" index string)
	   ;; if fractional seconds, we round.
	   (if (and (< index (string-length string))
		    (char=? (string-ref string index)
			    #\.))
	       (let ((frac #f))
		 (set! index (+ index 1))
		 (set! frac (accumulate-frac))
		 (if (eq? frac #f) 
		     (erriso "fractional second" index string)
		     (if (>= frac 1/2)
			 (set! second (+ second 1))))))
	   ;; now, check for time zone
	   (if (and (< index len)
		    (or (char=? (string-ref string index) #\Z)
			(char=? (string-ref string index) #\z)))
	       (encode-universal-time second minute hour day month year 0)
	       (if (>= index len)
		   (encode-universal-time second minute hour day month year)  
		   ;; we have a time zone
		   (let ((pm-char (string-ref string index)))
		     (set! index (+ index 1)) ; skip over +/-
		     (set! tzhour (accumulate-int))
		     (if (and (eq? tzhour #f) (= index len)) ; must be the end
			 (encode-universal-time second minute hour day month year)
			 (if (eq? tzhour #f) 
			     (erriso "time zone hour" index string)
			     (begin
			       (set! index (+ index 1))
			       (set! tzminute (accumulate-int))
			       (if (eq? tzminute #f) 
				   (erriso "time zone minute" index string))
			       (if (char=? pm-char #\-)
				   (encode-universal-time 
				    second minute
				    hour day month 
				    year (- (+ tzhour 
					       (/ tzminute 60))))
				   (if (char=? pm-char #\+)
				       (encode-universal-time 
					second minute hour day month year
					(+ tzhour (/ tzminute 60)))
				       (erriso2 "time zone +/-"
					      pm-char index 
					      string))))))))))))))))
  

(define (julian-day-number->string jdn)
  (call-with-values 
   (lambda ()
     (decode-julian-day-number jdn))
   (lambda (day month year)
     (let ((str (open-output-string)))
       (display-4-digits year str)
       (display #\- str)
       (display-2-digits month str)
       (display #\- str)
       (display-2-digits day str)
       (get-output-string str)))))

(define (string->julian-day-number string)
  (let
      ((accumulate-integer
	(lambda (string index)
	  (if (or (>= index (string-length string))
		  (not (char-numeric? (string-ref string index))))
	      (values #f index)
	      (let ((char->int 
		     (lambda (i)
		       (let ((ch (string-ref string i)))
			 (char-to-digit ch i)))))
		(do ((acc (char->int index) (+ (* acc 10) (char->int i)))
		     (i   (+ index 1)       (+ i 1)))
		    ((or (>= i (string-length string))
			 (not (char-numeric? (string-ref string i))))
		     (values acc i))))))))
    (call-with-values
     get-encoded-time
     (lambda (csecond cminute chour cday cmonth cyear 
		      cweek-day cdst? ctime-zone)                   
       (let* ((index 0))
	 (call-with-values 
	  (lambda ()
	    (accumulate-integer string index))
	  (lambda (year index)
	    (if (eq? year #f)
		(erriso "year" index string)
		(let ((year (normalize-year year cyear)))
		  (call-with-values 
		   (lambda ()
		     (accumulate-integer string (+ index 1)))
		   (lambda (month index)
		     (if (eq? month #f)
			 (erriso "month" index string)
			 (call-with-values 
			  (lambda ()
			    (accumulate-integer string (+ index 1)))
			  (lambda (day index)
			    (if (eq? day #f)
				(erriso "day" index string)
				(encode-julian-day-number 
				 day month year))))))))))))))))
