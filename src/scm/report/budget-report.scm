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
(require 'record)
(gnc:depend "report-utilities.scm")
(gnc:depend "html-generator.scm")
(gnc:depend "date-utilities.scm")

;; budget types
;(define gnc:budget-recurring 1) ; regular, recurring budget expenses
; that happen once per period
;(define gnc:budget-contingency 2) ; a budget item where you estimate a
; value over a long period for
; unexpected expenses.

;; convert a date to a defined fraction
(define (gnc:date-to-N-fraction caltime type)
  (case type
    ((gnc:budget-day) (gnc:date-to-day-fraction caltime))
    ((gnc:budget-week) (gnc:date-to-week-fraction caltime))
    ((gnc:budget-month) (gnc:date-to-month-fraction caltime))
    ((gnc:budget-year) (gnc:date-to-year-fraction caltime))
    (else (gnc:debug "undefined period type in budget!") #f)))

(define (gnc:date-N-delta caltime1 caltime2 type)
  (case type
    ((gnc:budget-day) 
     (- (gnc:date-to-day-fraction caltime2) 
	(gnc:date-to-day-fraction caltime1)))
    ((gnc:budget-week) 
     (- (gnc:date-to-week-fraction caltime2)
	(gnc:date-to-week-fraction caltime1)))
    ((gnc:budget-month) 
     (- (gnc:date-to-month-fraction caltime2)
	(gnc:date-to-month-fraction caltime1)))
    ((gnc:budget-year) (gnc:date-year-delta caltime1 caltime2))
    (else (gnc:debug "undefined period type in budget!") #f)))

;; returns the "day number" of the specified period.
(define (gnc:date-to-N-remainder caltime type)
  (case type
    ((gnc:budget-day) 0)
    ((gnc:budget-week) (gnc:date-get-week-day (localtime caltime)))
    ((gnc:budget-month) (gnc:date-get-month-day (localtime caltime)))
    ((gnc:budget-year) (gnc:date-get-year-day (localtime caltime)))
    (else (gnc:debug "undefined period type in budget!") #f)))

;; describe a time type
(define (gnc:date-describe-type type)
  (case type
    ((gnc:budget-day) "days")
    ((gnc:budget-week) "weeks")
    ((gnc:budget-month) "months")
    ((gnc:budget-year) "years")))

;; define the budget itself.  For prototype, define inline.
;; the budget is a vector of vectors.  the vectors contain:
;; 0 - description: a string describing the budget line
;; 1 - amount:
;; 2 - accounts: the list of accounts that this line spans
;;            (in colon delimited format)
;; 3 - period: the time span of the budget line in #4
;; 4 - period-type:
;; 5 - budget type
;; 6 - triggers???

(define budget-entry-structure
  (make-record-type 
   "budget-entry-structure" 
   '(description amount accounts period period-type budget-type
		 trigger-day)))

(define (make-budget-entry desc amt acct per ptype budget-type trig-day)
  ((record-constructor budget-entry-structure) 
   desc amt acct per ptype budget-type trig-day))

(define gnc:budget-entries
  (list
   (make-budget-entry "lunch" 8 '("Food:Lunch") 1 
		      'gnc:budget-day 'gnc:budget-recurring 0)
   (make-budget-entry "junk food" 0.50 '("Food:Junk") 1 
		      'gnc:budget-day 'gnc:budget-recurring 0)
   (make-budget-entry "car repairs" 2500 '("Car:Repairs") 5
		      'gnc:budget-year 'gnc:budget-contingency 0)
   (make-budget-entry "rent" 312.50 '("Household:Rent") 1
		      'gnc:budget-month 'gnc:budget-trigger 15)))

(define (budget-entry-get-description budget-entry)
  ((record-accessor budget-entry-structure 'description) budget-entry))

(define (budget-entry-get-amount budget-entry)
  ((record-accessor budget-entry-structure 'amount) budget-entry))

(define (budget-entry-get-accounts budget-entry)
  ((record-accessor budget-entry-structure 'accounts) budget-entry))

(define (budget-entry-get-period budget-entry)
  ((record-accessor budget-entry-structure 'period) budget-entry))

(define (budget-entry-get-period-type budget-entry)
  ((record-accessor budget-entry-structure 'period-type) budget-entry))

(define (budget-entry-get-trigger-day budget-entry)
  ((record-accessor budget-entry-structure 'trigger-day) budget-entry))

(define (budget-description-html-proc)
  (lambda (budget-line)
    (html-generic-cell #f #f #f 
		       (budget-entry-get-description 
			(budget-line-get-entry budget-line)))))

(define (budget-amount-html-proc)
  (lambda (budget-line)
    (html-currency-cell #f #f (budget-entry-get-amount 
			       (budget-line-get-entry budget-line)))))

;; fixme -- only returns the first account in the list
(define (budget-accounts-html-proc)
  (lambda (budget-line)
    (html-generic-cell 
     #f #f #f 
     (car (budget-entry-get-accounts (budget-line-get-entry budget-line))))))

(define (budget-period-html-proc)
  (lambda (budget-line)
    (html-number-cell
     #f #f "%i" (budget-entry-get-period (budget-line-get-entry budget-line)))))

(define (budget-period-type-html-proc)
  (lambda (budget-line)
    (html-generic-cell
     #f #f #f
     (gnc:date-describe-type 
      (budget-entry-get-period-type (budget-line-get-entry budget-line))))))

(define (budget-trigger-day-html-proc)
  (lambda (budget-line)
    (html-number-cell
     #f #f "%i" (budget-entry-get-trigger-day (budget-line-get-entry budget-line)))))

;; budget report:  a vector with indexes corresponding to the budget
;; 0 - actual:  the amount spend / recieved
;; 1 - budgeted: the budgeted amount.  Simply the periods * amount
;; 2 - num-periods: the number of periods for the line in the report
;; 3 - mimimum-expected: minimum you expected to spend during the
;;          report period
;; 4 - maximum-expected: the maximum you can spend in the report period
;; 5 - time remaining: how much of a period is remaining until the end
;;          of the budget period

(define budget-report-structure
  (make-record-type 
   "budget-report-structure" 
   '(actual budgeted num-periods minimum-expected maximum-expected 
	    time-remaining num-triggers)))

(define (make-empty-budget-report)
  ((record-constructor budget-report-structure) 
   0 0 0 0 0 0 0))

(define (budget-report-get-actual brep)
  ((record-accessor budget-report-structure 'actual) brep))

(define (budget-report-get-budgeted brep)
  ((record-accessor budget-report-structure 'budgeted) brep))

(define (budget-report-get-num-periods brep)
  ((record-accessor budget-report-structure 'num-periods) brep))

(define (budget-report-get-minimum-expected brep)
  ((record-accessor budget-report-structure 'minimum-expected) brep))

(define (budget-report-get-maximum-expected brep)
  ((record-accessor budget-report-structure 'maximum-expected) brep))

(define (budget-report-get-time-remaining brep)
  ((record-accessor budget-report-structure 'time-remaining) brep))

(define (budget-report-get-num-triggers brep)
  ((record-accessor budget-report-structure 'num-triggers) brep))

(define (budget-actual-html-proc)
  (lambda (budget-line)
    (html-currency-cell #f #f (budget-report-get-actual 
			       (budget-line-get-report budget-line)))))

(define (budget-budgeted-html-proc)
  (lambda (budget-line)
    (html-currency-cell #f #f (budget-report-get-budgeted 
			       (budget-line-get-report budget-line)))))

(define (budget-num-periods-html-proc)
  (lambda (budget-line)
    (html-number-cell #f #f "%.6f" (budget-report-get-num-periods
				    (budget-line-get-report budget-line)))))

(define (budget-minimum-expected-html-proc)
  (lambda (budget-line)
    (html-currency-cell #f #f (budget-report-get-minimum-expected
			       (budget-line-get-report budget-line)))))

(define (budget-maximum-expected-html-proc)
  (lambda (budget-line)
    (html-currency-cell #f #f (budget-report-get-maximum-expected
			       (budget-line-get-report budget-line)))))

(define (budget-time-remaining-html-proc)
  (lambda (budget-line)
    (html-number-cell #f #f "%.1f" (budget-report-get-time-remaining
				    (budget-line-get-report budget-line)))))

(define (budget-num-triggers-html-proc)
  (lambda (budget-line)
    (html-number-cell #f #f "%.0f" (budget-report-get-num-triggers
				  (budget-line-get-report budget-line)))))

(define budget-line-structure
  (make-record-type "budget-line-structure"
		    '(entry report)))

(define (make-budget-line entry report)
  ((record-constructor budget-line-structure)  entry report))

(define (budget-line-get-entry line)
  ((record-accessor budget-line-structure 'entry) line))

(define (budget-line-get-report line)
  ((record-accessor budget-line-structure 'report) line))


;; add a value to the budget accumulator
(define (budget-report-accumulate-actual! value budget-line)
  ((record-modifier budget-report-structure 'actual)
   (budget-line-get-report budget-line)
   (+ value (budget-report-get-actual (budget-line-get-report budget-line)))))

;; calculate the # of periods on a budget line.
;; dates are in # seconds after 1970
(define (budget-calculate-periods! budget-line begin-date end-date)
  (let ((entry (budget-line-get-entry budget-line)))
    ((record-modifier budget-report-structure 'num-periods)
     (budget-line-get-report budget-line)
     (/ (gnc:date-N-delta begin-date end-date 
			  (budget-entry-get-period-type entry))
	(budget-entry-get-period entry)))))

;; calculate the budgeted value.
;; dependency: budget-calculate-periods!
(define (budget-calculate-budgeted! budget-line)
  ((record-modifier budget-report-structure 'budgeted)
   (budget-line-get-report budget-line)
   (* (budget-entry-get-amount (budget-line-get-entry budget-line))
      (budget-report-get-num-periods (budget-line-get-report budget-line)))))

;; calculate the values for minimum-expected and maxmimum-expected
;; dependency: budget-calculate-periods!
(define (budget-calculate-expected! budget-line)
  (let ((brep (budget-line-get-report budget-line))
	(entry (budget-line-get-entry budget-line)))
    ; fixme: contingency type budget entries may have a lower minimum
    ((record-modifier budget-report-structure 'minimum-expected) brep
     (* (budget-entry-get-amount entry)		    
	(floor (budget-report-get-num-periods brep))))
    ((record-modifier budget-report-structure 'maximum-expected) brep
     (* (budget-entry-get-amount entry)
	(ceiling (budget-report-get-num-periods brep))))))

;; calculate the amount of time remaining in the budget period
;; dependency: budget-calculate-periods!
(define (budget-calculate-time-remaining! budget-line)
  (let* ((entry (budget-line-get-entry budget-line))
	 (brep (budget-line-get-report budget-line))
	 (periods (budget-report-get-num-periods brep)))
    ((record-modifier budget-report-structure 'time-remaining) brep
     (* (- (ceiling periods) periods) 
	(budget-entry-get-period entry)))))

;; calculate the number of times the trigger day occurs in the budget
;; period
(define (budget-calculate-num-triggers! budget-line begin-date end-date)
  (let* ((entry (budget-line-get-entry budget-line))
	 (brep (budget-line-get-report budget-line))
	 (N-type (budget-entry-get-period-type entry))
	 (trigger-day (budget-entry-get-trigger-day entry)))
    ((record-modifier budget-report-structure 'num-triggers) brep
     (+	-1
	(if (<= (gnc:date-to-N-remainder begin-date N-type) trigger-day) 1 0)
	(if (>= (gnc:date-to-N-remainder end-date N-type) trigger-day) 1 0)
	(- (floor (gnc:date-to-N-fraction end-date N-type))
	   (floor (gnc:date-to-N-fraction begin-date N-type)))))))

;; given an account name, return the budget line
;; return #f if there is no budget line for that account
(define (budget-get-line account-name budget)
  (cond ((null? budget) #f)
	(else
	 (let loop2
	     ((accounts (budget-entry-get-accounts 
			 (budget-line-get-entry (car budget)))))
	   (cond ((null? accounts) #f)
		 (else 
		  (cond ((or (string=? account-name (car accounts))
			     (loop2 (cdr accounts)))
			 (car budget))
			(else
			 (budget-get-line account-name (cdr budget))))))))))


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
   (let* ((begindate (gnc:lookup-option options "Report Options" "From"))
	  (enddate (gnc:lookup-option options "Report Options" "To"))
	  (begin-date-secs (car (gnc:timepair-canonical-day-time 
                                 (gnc:option-value begindate))))
	  (end-date-secs (car (gnc:timepair-canonical-day-time
                               (gnc:option-value enddate))))
	  (budget-list (map 
			(lambda (entry)
			  (make-budget-line entry (make-empty-budget-report)))
			gnc:budget-entries)))

     (let loop ((group (gnc:get-current-group)))
       (if (not (pointer-token-null? group))
	   (gnc:group-map-accounts
	    (lambda (account)
	      (let ((line 
		     (budget-get-line
		      (gnc:account-get-full-name account)
		      budget-list)))
		(if line 
		    (gnc:for-each-split-in-account 
		     account
		     (lambda (split)
		       (budget-report-accumulate-actual! 
			(gnc:split-get-value split) line))))
		(loop (gnc:account-get-children account))))
	    group)))

     (for-each 
      (lambda (line)
	(begin
	  (budget-calculate-periods! line begin-date-secs end-date-secs)
	  (budget-calculate-budgeted! line)
	  (budget-calculate-expected! line)
	  (budget-calculate-time-remaining! line)
	  (budget-calculate-num-triggers! line begin-date-secs end-date-secs)))
      budget-list)
     
     (let ((report-headers '())
	   (report-procs '()))
       (case (gnc:option-value 
	      (gnc:lookup-option options "Report Options" "View"))
	 ((full)
	  (set! report-headers (list
				"Description"
				"Amount"
				"Accounts"
				"Period"
				""
				"Actual"
				"Trigger Day"
				"Budgeted"
				"Number of Periods"
				"Lower Limit"
				"Upper Limit"
				"Time Remaining"
				""
				"Num Triggers"))
	  (set! report-procs (list
			      budget-description-html-proc
			      budget-amount-html-proc
			      budget-accounts-html-proc
			      budget-period-html-proc	
			      budget-period-type-html-proc
			      budget-actual-html-proc
			      budget-trigger-day-html-proc
			      budget-budgeted-html-proc
			      budget-num-periods-html-proc
			      budget-minimum-expected-html-proc
			      budget-maximum-expected-html-proc
			      budget-time-remaining-html-proc
			      budget-period-type-html-proc
			      budget-num-triggers-html-proc)))
	 ((balancing)
	  (set! report-headers (list
				"Description"
				"Accounts"
				"Period"
				""
				"Amount"
				"Number of Periods"
				"Budgeted"))
	  (set! report-procs (list
			      budget-description-html-proc
			      budget-accounts-html-proc
			      budget-period-html-proc
			      budget-period-type-html-proc
			      budget-amount-html-proc
			      budget-num-periods-html-proc
			      budget-budgeted-html-proc)))
	 ((status)
	  (set! report-headers (list
				"Description"
				"Time Remaining"
				""
				"Lower Limit"
				"Upper Limit"
				"Actual"))
	  (set! report-procs (list
			      budget-description-html-proc
			      budget-time-remaining-html-proc
			      budget-period-type-html-proc
			      budget-minimum-expected-html-proc
			      budget-maximum-expected-html-proc
			      budget-actual-html-proc)))
	 (else
	  (gnc:debug "Invalid view option")))
       (list
	(html-start-document)
	"<p>This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.</p>"
	(html-start-table)
	(html-table-row-manual
	 (map-in-order
	  (lambda (item) (html-cell-header item))
	  report-headers))
	(map-in-order
	 (lambda (line)	 
	   (html-table-row-manual
	    (map-in-order
	     (lambda (proc)
	       ((proc) line))
	     report-procs)))
	 budget-list)
	(html-end-table)
	(html-end-document))))))
