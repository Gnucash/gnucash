;; -*-scheme-*-
;; budget-report.scm
;; Report on budget
;; Bryan Larsen (blarsen@ada-works.com)

;; situations I want to handle
;; lunch M-F -- funny period
;; xmas gifts & birthday gifts in same budget line
;; car repairs -- contingency
;; car fuel-ups -- known amount, variable period
;; paychecks & rent payments -- specific dates

(require 'sort)
(gnc:depend "report-utilities.scm")
(gnc:depend "html-generator.scm")
(gnc:depend "date-utilities.scm")

;; time values
(define gnc:budget-day 1)
(define gnc:budget-week 2)
(define gnc:budget-month 3)
(define gnc:budget-year 4)

;; budget types
(define gnc:budget-recurring 1) ; regular, recurring budget expenses
				; that happen once per period
(define gnc:budget-contingency 2) ; a budget item where you estimate a
				  ; value over a long period for
				  ; unexpected expenses.

;; define the budget itself.  For prototype, define inline.
;; the budget is a vector of vectors.  the vectors contain:
;; 0 - description: a string describing the budget line
;; 1 - amount:
;; 2 - accounts: the list of accounts that this line spans
;;            (in colon delimited format)
;; 3 - period: the time span of the budget line in #4
;; 4 - period-type:
;; 5 - triggers:  as yet undefined

(define (make-budget-entry desc amt acct per ptype trigger)
  (vector desc amt acct per ptype trigger))

(define gnc:budget
  (vector 
   (make-budget-entry "lunch" 8 '("Food:Lunch") 1 
		      'gnc:budget-day gnc:budget-recurring)
   (make-budget-entry "junk food" 0.50 '("Food:Junk") 1 
		      'gnc:budget-day gnc:budget-recurring)
   (make-budget-entry "car repairs" 2500 '("Car:Repairs") 5
		      'gnc:budget-year gnc:budget-contingency)))

;;; For future: make-budget-entry should create a structure.
;;; And gnc:budget should be a list, not a vector.

(define gnc:budget-headers
  #(("" "Description")
    ("Amount" "per Period")
    ("" "Accounts")
    ("Period" "Size")
    ("Period" "Size Units")
    ("Budget" "Type")))

(define (gnc:budget-html-cell-pred)
  (vector
   (lambda (item) 
     (html-generic-cell #f #f #f item))
   (lambda (item)
     (html-currency-cell #f #f item))
   (lambda (item)
     '())  ; todo: accounts
   (lambda (item)
     (html-number-cell #f #f "%i" item))
   (lambda (item)
     (html-generic-cell #f #f #f (gnc:date-describe-type item)))
   (lambda (item)
     '())))  ; todo: budget-type

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
;; 1 - budgeted: the budgeted amount.  Simply the periods * amount
;; 2 - periods: the number of periods for the line in the report
;; 3 - mimimum-expected: minimum you expected to spend during the
;;          report period
;; 4 - maximum-expected: the maximum you can spend in the report period
;; 5 - time remaining: how much of a period is remaining until the end
;;          of the budget period

(define gnc:budget-report-headers
  #(("Amount" "Spent")
    ("Amount" "Budgeted")
    ("Number of" "Periods")
    ("Lower" "Limit")
    ("Upper" "Limit")
    ("Time" "Remaining")))

(define (gnc:budget-report-html-cell-pred)
  (vector
   (lambda (item)
     (html-currency-cell #f #f item))
   (lambda (item)
     (html-currency-cell #f #f item))
   (lambda (item)
     (html-number-cell #f #f "%.1f" item))
   (lambda (item)
     (html-currency-cell #f #f item))
   (lambda (item)
     (html-currency-cell #f #f item))
   (lambda (item)
     (html-number-cell #f #f "%.1f" item))))

(define (gnc:budget-report-get-actual brep-line)
  (vector-ref brep-line 0))

(define (gnc:budget-report-get-budgeted brep-line)
  (vector-ref brep-line 1))

(define (gnc:budget-report-get-periods brep-line)
  (vector-ref brep-line 2))

(define (gnc:budget-report-get-minimum-expected brep-line)
  (vector-ref brep-line 3))

(define (gnc:budget-report-get-maximum-expected brep-line)
  (vector-ref brep-line 4))

(define (gnc:budget-report-get-time-remaining brep-line)
  (vector-ref brep-line 5))

;; add a value to the budget accumulator
(define (gnc:budget-accumulate-actual! value budget-report-line)
  (vector-set! budget-report-line 0 
	       (+ (gnc:budget-report-get-actual budget-report-line)
		  value)))

;; calculate the # of periods on a budget line.
;; dates are in # seconds after 1970
(define (gnc:budget-calculate-periods! budget-line budget-report-line 
				      begin-date end-date)
  (let* ((N-type (gnc:budget-get-period-type budget-line))
	 (begin-N (gnc:date-to-N-fraction begin-date N-type))
	 (end-N (gnc:date-to-N-fraction end-date N-type)))
    (vector-set! budget-report-line 2 
		 (/ (- end-N begin-N)
		    (gnc:budget-get-period budget-line)))))

;; calculate the budgeted value.
;; dependency: budget-calculate-periods!
(define (gnc:budget-calculate-budgeted! budget-line budget-report-line)
  (vector-set! budget-report-line 1
	       (* (gnc:budget-get-amount budget-line)
		  (gnc:budget-report-get-periods budget-report-line))))

;; calculate the values for minimum-expected and maxmimum-expected
;; dependency: budget-calculate-periods!
(define (gnc:budget-calculate-expected! budget-line budget-report-line)
  (begin
    (vector-set! 
     budget-report-line 3
     (* (gnc:budget-get-amount budget-line)		    
	(floor (gnc:budget-report-get-periods budget-report-line))))
    (vector-set! 
     budget-report-line 4
     (* (gnc:budget-get-amount budget-line)
	(ceiling (gnc:budget-report-get-periods budget-report-line))))))

;; calculate the amount of time remaining in the budget period
;; dependency: budget-calculate-periods!
(define (gnc:budget-calculate-time-remaining! budget-line budget-report-line)
  (vector-set! 
   budget-report-line 5
   (* (- (ceiling (gnc:budget-report-get-periods budget-report-line))
	 (gnc:budget-report-get-periods budget-report-line))
      (gnc:budget-get-period budget-line))))

;; given an account name, return the budget line number
;; return #f if there is no budget line for that account
(define (gnc:budget-get-line-number account-name budget)
  (let loop ((i 0))
    (cond ((= i (vector-length budget)) #f)
	  ((let loop2
	       ((accounts (gnc:budget-get-accounts (vector-ref budget i)))) 
	    (cond ((null? accounts) #f)
		  (else (or (string=? account-name (car accounts))
			    (loop2 (cdr accounts)))))) i)
	  (else (loop (+ i 1))))))
    

;; register a configuration option for the budget report
(define (budget-report-options-generator)

  (define gnc:*budget-report-options* (gnc:new-options))

  (define (gnc:register-budget-report-option new-option)
    (gnc:register-option gnc:*budget-report-options* new-option))

  ;; from date
  ;; hack alert - could somebody set this to an appropriate date?
  (gnc:register-budget-report-option
   (gnc:make-date-option
    "Report Options" "From"
    "a" "Report start date" 
    (lambda ()
      (let ((bdtime (localtime (current-time))))
        (set-tm:sec bdtime 0)
        (set-tm:min bdtime 0)
        (set-tm:hour bdtime 0)
        (set-tm:mday bdtime 1)
        (set-tm:mon bdtime 0)
        (let ((time (car (mktime bdtime))))
          (cons time 0))))
    #f))

  ;; to-date
  (gnc:register-budget-report-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report end date"
    (lambda () (cons (current-time) 0))
    #f))

  ;; view
  (gnc:register-budget-report-option
   (gnc:make-multichoice-option
    "Report Options" "View"
    "c" "Type of budget report"
    'status
    (list #(full
	    "Full"
	    "Show all columns")
	  #(balancing
	    "Balancing"
	    "A report useful for balancing the budget")
	  #(status
	    "Status"
	    "How are you doing on your budget?"))))
  gnc:*budget-report-options*)

(gnc:define-report
 ;; version
 1
 ;; Name
 "Budget"
 ;; Options
 budget-report-options-generator
 ;; renderer
 (lambda (options)
   (let* ((maxrow (vector-length gnc:budget))
	  ;;; Note that by using maxrow, *all* references to
	  ;;; (vector-length gnc:budget) disappear, and this notably
	  ;;; takes some code out of at least 3 loops...
	  (begindate (gnc:lookup-option options "Report Options" "From"))
	  (enddate (gnc:lookup-option options "Report Options" "To"))
	  (date-filter-pred (gnc:tr-report-make-filter-predicate
			     (gnc:option-value begindate) 
			     (gnc:option-value enddate)))
	  (begin-date-secs (car (gnc:timepair-canonical-day-time 
				  (gnc:option-value begindate))))
	  (end-date-secs (car (gnc:timepair-canonical-day-time
			       (gnc:option-value enddate))))
	  (budget-report (make-vector maxrow))
	  (budget-order #())
	  (budget-report-order #()))
     (gnc:debug gnc:budget)
     
     (do ((i 0 (+ i 1)))
	 ((= i maxrow))
       (vector-set! budget-report i (vector 0 0 0 0 0 0)))

     (let loop ((group (gnc:get-current-group)))
       (if (not (pointer-token-null? group))
	   (gnc:group-map-accounts
	    (lambda (account)
	      (let ((line 
		     (gnc:budget-get-line-number 
		      (gnc:account-get-full-name account)
		      gnc:budget))
		    (children (gnc:account-get-children account)))
		(if line 
		    (gnc:for-each-split-in-account 
		     account
		     (lambda (split)
			(gnc:budget-accumulate-actual!
			 (gnc:split-get-value split)
			 (vector-ref budget-report line)))))
		(loop children)))
	    group)))

     ;;; Note: This shouldn't need to use a set of vectors...
     
     (do ((i 0 (+ i 1)))
	 ((= i maxrow))
       (let ((budget-line (vector-ref gnc:budget i))
	     (budget-report-line (vector-ref budget-report i)))
	 (gnc:budget-calculate-periods!
	  budget-line budget-report-line begin-date-secs end-date-secs)
	 (gnc:budget-calculate-budgeted! budget-line budget-report-line)
	 (gnc:budget-calculate-expected! budget-line budget-report-line)
	 (gnc:budget-calculate-time-remaining! budget-line budget-report-line)))
     
     (gnc:debug budget-report)

     (case (gnc:option-value 
	    (gnc:lookup-option options "Report Options" "View"))
       ((full)
	(set! budget-order (vector 1 2 #f 3 4 #f))
	(set! budget-report-order (vector 5 6 7 8 9 10)))
       ((balancing)
	(set! budget-order #(1 2 #f 3 4 #f))
	(set! budget-report-order #(#f 6 5 #f #f #f)))
       ((status)
	(set! budget-order #(1 #f #f #f 3 #f))
	(set! budget-report-order #(10 #f #f 4 5 2)))
       (else
	(gnc:debug "Invalid view option")))
     (let ((order (find-vector-mappings 
		   (vector budget-order budget-report-order))))
       (list
	(html-start-document)
	"<p>This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.</p>"
	(html-start-table)
	(html-table-row-manual
	 (vector-map-in-specified-order
	  (lambda (item) (html-cell-header (car item)))
	  (vector gnc:budget-headers gnc:budget-report-headers)
	  order))
	(html-table-row-manual
	 (vector-map-in-specified-order
	  (lambda (item) (html-cell-header (cadr item)))
	  (vector gnc:budget-headers gnc:budget-report-headers)
	  order))
	;;; This loop ought not to need to use a vector
	(let loop ((row 0))
	  (if (= row maxrow) 
	      '()		
	      (cons
	       (html-table-row-manual
		(vector-map-in-specified-order-uniquely
		 (vector (gnc:budget-html-cell-pred) 
			 (gnc:budget-report-html-cell-pred))
		 (vector (vector-ref gnc:budget row) 
			 (vector-ref budget-report row))
		 order))
	       (loop (+ row 1)))))
	(html-end-table)
	(html-end-document))))))
