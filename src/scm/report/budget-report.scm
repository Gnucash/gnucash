;; -*-scheme-*-
;; budget-report.scm
;; Report on budget
;; uses some functions from transaction-report
;; Bryan Larsen (blarsen@ada-works.com)

;; situations I want to handle
;; lunch M-F
;; xmas gifts & birthday gifts in same budget line
;; car repairs
;; car fuel-ups
;; paychecks & rent payments

(require 'sort)
;(require 'time)
(gnc:depend "report-utilities.scm")

;; time values
(define gnc:budget-day 1)
(define gnc:budget-week 2)
(define gnc:budget-month 3)
(define gnc:budget-year 4)

;; define the budget itself.  For prototype, define inline.
;; the budget is a vector of vectors.  the vectors contain:
;; 0 - description: a string describing the budget line
;; 1 - amount:
;; 2 - accounts: the list of accounts that this line spans
;;            (in colon delimited format)
;; 3 - period: the time span of the budget line in #4
;; 4 - period-type:
;; 5 - triggers:  as yet undefined
(define gnc:budget 
  #(#("lunch" 8 ("Food:Lunch") 1 gnc:budget-day)
    #("junk food" 0.50 ("Food:Junk") 1 gnc:budget-day)))

(define (gnc:budget-get-description budget-line)
  (vector-ref budget-line 0))

(define (gnc:budget-get-amount budget-line)
  (vector-ref budget-line 1))

(define (gnc:budget-get-accounts budget-line)
  (vector-ref budget-line 2))

(define (gnc:budget-get-period budget-line)
  (vector-ref budget-line 3))

(define (gnc:budget-get-period-type budget-line)
  (vector-ref budget-line 4))

;; budget report:  a vector with indexes corresponding to the budget
;; 0 - actual:  the amount spend / recieved
;; 1 - desired: the budgeted amount
;; 2 - periods: the number of periods for the line in the report

(define (gnc:budget-report-get-actual brep-line)
  (vector-ref brep-line 0))

(define (gnc:budget-report-get-desired brep-line)
  (vector-ref brep-line 1))

(define (gnc:budget-report-get-periods brep-line)
  (vector-ref brep-line 2))

;; accumulate the actual amounts for the budget given a split and
;; a budget.  returns the budget-report vector.  The split is a 2 item list: 
;; account name and value
;; obsolete
(define (gnc:budget-report-accumulate-actual sub-split budget budget-report)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length budget)) budget-report)
    (let ((budget-line (vector-ref budget i))
	  (budget-report-line (vector-ref budget-report i))
	  (name (car sub-split))
	  (value (cadr sub-split)))
      (for-each 
       (lambda (budget-account-name)
	 (if (string-ci=? name budget-account-name)
	     (begin
	       (vector-set! budget-report-line 0	
			    (+ (gnc:budget-report-get-actual budget-report-line)
			       value)))))
       (gnc:budget-get-accounts budget-line)))))

;; add a value to the budget accumulator
(define (gnc:budget-accumulate-actual value budget-report-line)
  (vector-set! budget-report-line 0 
	       (+ (gnc:budget-report-get-actual budget-report-line)
		  value))
  budget-report-line)



;; convert budget-report to an html table
(define (gnc:budget-report-to-html budget budget-report)
  (let ((budget-html ()))
    (do ((i 0 (+ i 1)))
	((= i (vector-length budget)) budget-html)
      (let ((budget-line (vector-ref budget i))
	    (budget-report-line (vector-ref budget-report i)))
	(set! budget-html
	      (append
	       budget-html
	       (list
		(string-append
		 "<TR><TD>"
		 (gnc:budget-get-description budget-line)
		 "</TD><TD align=right>"
		 (sprintf 
		  #f "%.1f"
		  (gnc:budget-report-get-periods budget-report-line))
		 "</TD><TD align=right>"
		 (sprintf 
		  #f "%.2f" (gnc:budget-report-get-desired 
			     budget-report-line))
		 "</TD><TD align=right>"
		 (sprintf 
		  #f "%.2f" (gnc:budget-report-get-actual 
			     budget-report-line))
		 "</TD><TD align=right>"
		 (sprintf 
		  #f "%.2f" (- (gnc:budget-report-get-desired
				budget-report-line)
			       (gnc:budget-report-get-actual
				budget-report-line)))
		 "</TD></TR>"))))))))

;; given an account name, return the budget line number
;; return #f if there is no budget line for that account
(define (gnc:budget-get-line-number account-name budget)
  (let loop ((i 0))
;    (gnc:debug i)
;    (gnc:debug (car (gnc:budget-get-accounts (vector-ref budget i))))
    (cond ((= i (vector-length budget)) #f)
	  ((let loop2
	       ((accounts (gnc:budget-get-accounts (vector-ref budget i)))) 
	    (cond ((null? accounts) #f)
		  (else (or (string=? account-name (car accounts))
			    (loop2 (cdr accounts)))))) i)
;	  ((string=? account-name (car (gnc:budget-get-accounts (vector-ref budget i)))) i)
	  (else (loop (+ i 1))))))
    

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
					       (gnc:date-get-month lt))))))

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

;; calculate the # of periods on a budget line.  return the budget report line
;; dates are in # seconds after 1970
(define (gnc:budget-calculate-periods budget-line budget-report-line 
				      begin-date end-date)
  (let* ((N-type (gnc:budget-get-period-type budget-line))
	 (begin-N (gnc:date-to-N-fraction begin-date N-type))
	 (end-N (gnc:date-to-N-fraction end-date N-type)))
    (vector-set! budget-report-line 2 
		 (/ (- end-N begin-N)
		    (gnc:budget-get-period budget-line)))
    budget-report-line))

;; return what you are passed
(define (null-filter) 
  (lambda(x) x))

;; calculate the expected budget value.  return the budget report line
(define (gnc:budget-calculate-expected budget-line budget-report-line
				       begin-date end-date)
  (begin
    (vector-set! budget-report-line 1
		 (* (gnc:budget-get-amount budget-line)
		    (gnc:budget-report-get-periods budget-report-line)))
    budget-report-line))
  

(gnc:define-report
 ;; version
 1
 ;; Name
 "Budget"
 ;; Options
 trep-options-generator
 ;; renderer
 (lambda (options)
   (let* ((begindate (gnc:lookup-option options "Report Options" "From"))
	  (enddate (gnc:lookup-option options "Report Options" "To"))
	  (tr-report-account-op (gnc:lookup-option options
						   "Report Options" "Account"))
	  (prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>" 
			 "<TABLE border=1>"
			 "<TH>Description</TH>"
			 "<TH>Number of Periods</TH>"
			 "<TH>Amount Budgeted</TH>"
			 "<TH>Amount Spent</TH>"
			 "<TH>Delta</TH>"))
	  (suffix  (list "</TABLE>" "</BODY>" "</HTML>"))
	  (input-transactions '())
	  (budget-report #())
	  (budget-html "")
	  (date-filter-pred (gnc:tr-report-make-filter-predicate
			     (gnc:option-value begindate) 
			     (gnc:option-value enddate)))
	  (begin-date-secs (car (gnc:timepair-canonical-day-time 
				  (gnc:option-value begindate))))
	  (end-date-secs (car (gnc:timepair-canonical-day-time
			       (gnc:option-value enddate))))
	  (accounts (gnc:option-value tr-report-account-op)))
     (set! budget-report (make-vector 
				(vector-length gnc:budget)))
     (do ((i 0 (+ i 1)))
	 ((= i (vector-length gnc:budget)))
       (vector-set! budget-report i (vector 0 0 0)))
     (let loop ((group (gnc:get-current-group)))
       (if (not (pointer-token-null? group))
	   (gnc:group-map-accounts
	    (lambda (account)
	      (let* ((name (gnc:account-get-full-name account))
		     (line (gnc:budget-get-line-number name gnc:budget))
		     (children (gnc:account-get-children account)))
		(if line 
		    (gnc:for-each-split-in-account 
		     account
		     (lambda (split)
		       (vector-set! 
			budget-report line
			(gnc:budget-accumulate-actual
			 (gnc:split-get-value split)
			 (vector-ref budget-report line))))))
		(if (not (pointer-token-null? children)) (loop children))))
	    group)))
     (gnc:debug budget-report)
     (gnc:debug begin-date-secs)
     (gnc:debug end-date-secs)
     (gnc:debug (gnc:timepair-canonical-day-time 
		 (gnc:option-value begindate)))
     (gnc:debug (gnc:timepair-canonical-day-time 
		 (gnc:option-value enddate)))

     (do ((i 0 (+ i 1)))
	 ((= i (vector-length gnc:budget)))
       (vector-set! budget-report i 
		    (gnc:budget-calculate-expected
		     (vector-ref gnc:budget i)
		     (gnc:budget-calculate-periods
		      (vector-ref gnc:budget i)
		      (vector-ref budget-report i)
		      begin-date-secs
		      end-date-secs)
		     begin-date-secs
		     end-date-secs)))
     
     (gnc:debug budget-report)
	   
     (set! budget-html (gnc:budget-report-to-html
			gnc:budget budget-report))
     
     (append prefix budget-html suffix))))
