;; -*-scheme-*-
;; budget-report.scm
;; Report on budget
;; Bryan Larsen (blarsen@ada-works.com)

;; TODO
;; properly handle income as well
;; proper totals
;; "upcoming/overdue bills" report
;; druids to enter budget
;; save/load budget
;; internationalization
;; speedup:  replace linear search with hash, 
;;           create structure functions on load, 
;;           move subexpressions outside loops
;; clean up report
;; graph budget progress

(require 'sort)
(require 'record)
(gnc:depend "report-utilities.scm")
(gnc:depend "html-generator.scm")
(gnc:depend "date-utilities.scm")
(gnc:depend "acc-create.scm")

;; budget types
;;(define gnc:budget-recurring 1) ; regular, recurring budget expenses
;; that happen once per period
;;(define gnc:budget-contingency 2) ; a budget item where you estimate a
;; value over a long period for
;; unexpected expenses.
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

;; returns the "day number" of the specified period.  For example,
;; December 31 is day #92 in a 3 month period.
;; This is one based arithmetic, so the name "remainder" may be slightly
;; confusing.
(define (gnc:date-to-N-remainder caltime type num-periods)
  (let ((lt (localtime caltime)))
    (case type
      ((gnc:budget-day) (+ 1 
			   (remainder 
			    (inexact->exact (floor
					     (gnc:date-to-day-fraction caltime)))
			    num-periods)))
      ((gnc:budget-week) (+ (gnc:date-get-week-day lt)
			    (* 7 (remainder 
				  (inexact->exact 
				   (floor (gnc:date-to-week-fraction caltime)))
				  num-periods))))
      ((gnc:budget-month) (+ (gnc:date-get-month-day lt)
			     (let loop ((month 
					 (inexact->exact 
					  (floor
					   (gnc:date-to-month-fraction caltime)))))
			       (if (= 0 (remainder month num-periods))
				   0
				   (+ (loop (- month 1))
				      (gnc:days-in-month
				       (+ 1 (remainder month 12))
				       (+ 1970 (quotient month 12))))))))
      ((gnc:budget-year) (+ (gnc:date-get-year-day lt)
			    (let loop ((year (gnc:date-get-year lt)))
			      (if (= 0 (remainder year num-periods))
				  0
				  (+ (loop (- year 1))
				     (gnc:days-in-year year))))))
      (else (gnc:debug "undefined period type in budget!") #f))))

;; describe a time type
(define (gnc:date-describe-type type)
  (case type
    ((gnc:budget-day) "days")
    ((gnc:budget-week) "weeks")
    ((gnc:budget-month) "months")
    ((gnc:budget-year) "years")))

;; returns the number of days in an n periods of type.
(define (gnc:days-in-period date type n)
  (let ((lt (localtime date)))
    (case type
      ((gnc:budget-day) n)
      ((gnc:budget-week) (* 7 n))
      ((gnc:budget-month) 
       (let loop
	   ((month (* (quotient (inexact->exact (floor (gnc:date-to-month-fraction date))) n) n)))
	 (+ (gnc:days-in-month (+ 1 (remainder month 12))
			       (+ 1970 (quotient month 12)))
	    (if (= (remainder month n) (- n 1))
		0
		(loop (+ 1 month))))))
      ((gnc:budget-year)
       (let loop
	   ((year (* (quotient (inexact->exact (floor (gnc:date-to-year-fraction date))) n) n)))
	 (+ (gnc:days-in-year (+ 1970 year))
	    (if (= (remainder year n) (- n 1))
		0
		(loop (+ 1 year)))))))))

;; define the budget itself.  For prototype, define inline.
;; the budget is a vector of vectors.  the vectors contain:
;; 0 - description: a string describing the budget line
;; 1 - amount:
;; 2 - accounts: the list of accounts that this line spans
;; 3 - period: the time span of the budget line
;; 4 - period-type: day, month, year, etc.
;; 5 - budget type: recurring or contingency
;; 6 - window-start: the first possible day in the period that the expenditure can occur on.  negative numbers count from the end of the period, zero indicates the last day of the previous period
;; 7 - window-end: the last possible day.

(define budget-entry-structure
  (make-record-type 
   "budget-entry-structure" 
   '(description accounts subentries)))

(define budget-subentry-structure 
  (make-record-type
   "budget-subentry-structure"
   '(description amount period period-type mechanism)))

(define budget-recurring-mechanism-structure
  (make-record-type 
   "budget-recurring-mechanism-structure"
   '()))

(define budget-bill-mechanism-structure
  (make-record-type 
   "budget-bill-mechanism-structure"
   '(window-start-day window-end-day)))

(define budget-contingency-mechanism-structure
  (make-record-type 
   "budget-contingency-mechanism-structure"
   '()))

(define (make-budget-entry desc acct subentries)
  ((record-constructor budget-entry-structure) 
   desc acct subentries))

(define (make-budget-subentry desc amt per ptype mech)
  ((record-constructor budget-subentry-structure)
   desc amt per ptype mech))

(define (make-recurring-mechanism)
  ((record-constructor budget-recurring-mechanism-structure)))

(define (make-bill-mechanism window-start-day window-end-day)
  ((record-constructor budget-bill-mechanism-structure)
   window-start-day window-end-day))

(define (make-contingency-mechanism)
  ((record-constructor budget-contingency-mechanism-structure)))

(define gnc:budget-entries
  (list
   ;; first line is always the "other" collector.
   (make-budget-entry "other" '()
    (list
     (make-budget-subentry "" 3 1 'gnc:budget-day
			   (make-recurring-mechanism))))
   (make-budget-entry  "lunch" '("Expense:Food:Lunch" "Expense:Food:Junk") 
    (list 
     (make-budget-subentry "" 8 1 'gnc:budget-day 
			   (make-recurring-mechanism))))
   (make-budget-entry "car repairs" '("Expense:Car:Repairs") 
    (list
     (make-budget-subentry "contingency" 2500 5 'gnc:budget-year
			   (make-contingency-mechanism))
     (make-budget-subentry "maintenance" 50 6 'gnc:budget-month 
			   (make-recurring-mechanism))))
   (make-budget-entry "rent" '("Expense:Household:Rent") 
    (list
     (make-budget-subentry "" 312.50 1 'gnc:budget-month 
			   (make-bill-mechanism 0 2))))
   (make-budget-entry "car payments" '("Expense:Car:Loan Payments") 
    (list
     (make-budget-subentry "" 374.80 1 'gnc:budget-month 
			   (make-bill-mechanism 13 17))))))

(define (budget-entry-get-description budget-entry)
  ((record-accessor budget-entry-structure 'description) budget-entry))

(define (budget-subentry-get-description subentry)
  ((record-accessor budget-subentry-structure 'description) subentry))

(define (budget-entry-get-accounts budget-entry)
  ((record-accessor budget-entry-structure 'accounts) budget-entry))

(define (budget-entry-get-subentries budget-entry)
  ((record-accessor budget-entry-structure 'subentries) budget-entry))

(define (budget-subentry-get-amount subentry)
  ((record-accessor budget-subentry-structure 'amount) subentry))

(define (budget-subentry-get-period subentry)
  ((record-accessor budget-subentry-structure 'period) subentry))

(define (budget-subentry-get-period-type subentry)
  ((record-accessor budget-subentry-structure 'period-type) subentry))

(define (budget-bill-get-window-start-day bill)
  ((record-accessor budget-bill-mechanism-structure 'window-start-day) bill))

(define (budget-bill-get-window-end-day bill)
  ((record-accessor budget-bill-mechanism-structure 'window-end-day) bill))

(define (budget-subentry-get-mechanism subentry)
  ((record-accessor budget-subentry-structure 'mechanism) subentry))

(define (budget-description-html-proc)
  (lambda (entry subentry report subreport)
    (html-generic-cell #f #f #f (budget-entry-get-description entry))))

(define (budget-sub-description-html-proc)
  (lambda (entry subentry report subreport)
    (html-generic-cell #f #f #f (budget-subentry-get-description subentry))))

(define (budget-accounts-html-proc)
  (lambda (entry subentry report subreport)
    (html-generic-cell 
     #f #f #f 
     (list->string (budget-entry-get-accounts entry)))))

(define (budget-amount-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-subentry-get-amount subentry))))

(define (budget-period-html-proc)
  (lambda (entry subentry report subreport)
    (html-number-cell
     #f #f "%i" (budget-subentry-get-period subentry))))

(define (budget-period-type-html-proc)
  (lambda (entry subentry report subreport)
    (html-generic-cell
     #f #f #f
     (gnc:date-describe-type 
      (budget-subentry-get-period-type subentry)))))

(define (budget-window-start-day-html-proc)
  (lambda (entry subentry report subreport)
    (let ((mechanism (budget-subentry-get-mechanism subentry)))
      (if ((record-predicate budget-bill-mechanism-structure) mechanism)
	  (html-number-cell
	   #f #f "%i" (budget-bill-get-window-start-day mechanism))
	  (html-generic-cell #f #f #f "")))))

(define (budget-window-end-day-html-proc)
  (lambda (entry subentry report subreport)
    (let ((mechanism (budget-subentry-get-mechanism subentry)))
      (if ((record-predicate budget-bill-mechanism-structure) mechanism)
	  (html-number-cell
	   #f #f "%i" (budget-bill-get-window-end-day mechanism))
	  (html-generic-cell #f #f #f "")))))

;; budget report:  a vector with indexes corresponding to the budget
;; 0 - actual:  the amount spend / recieved
;; 1 - nominal: the nominal budgeted amount.  Simply the periods * amount
;; 2 - num-periods: the number of periods for the line in the report
;; 3 - mimimum-expected: minimum you expected to spend during the
;;          report period
;; 4 - maximum-expected: the maximum you can spend in the report period
;; 5 - time remaining: how much of a period is remaining until the end
;;          of the budget period

(define budget-report-structure
  (make-record-type 
   "budget-report-structure" 
   '(actual nominal minimum-expected maximum-expected subreports)))

(define budget-subreport-structure
  (make-record-type
   "budget-subreport-structure"
   '(nominal minimum-expected maximum-expected)))

(define (make-empty-budget-report entry)
  ((record-constructor budget-report-structure) 
   0 0 0 0 
   (map
    (lambda (subentry)
      (make-empty-subreport))
    (budget-entry-get-subentries entry))))

(define (make-empty-subreport)
  ((record-constructor budget-subreport-structure)
   0 0 0))

(define (budget-report-get-subreports brep)
  ((record-accessor budget-report-structure 'subreports) brep))

(define (budget-report-get-actual brep)
  ((record-accessor budget-report-structure 'actual) brep))

(define (budget-report-get-nominal brep)
  ((record-accessor budget-report-structure 'nominal) brep))

(define (budget-subreport-get-nominal brep)
  ((record-accessor budget-subreport-structure 'nominal) brep))

(define (budget-report-get-minimum-expected brep)
  ((record-accessor budget-report-structure 'minimum-expected) brep))

(define (budget-subreport-get-minimum-expected brep)
  ((record-accessor budget-subreport-structure 'minimum-expected) brep))

(define (budget-report-get-maximum-expected brep)
  ((record-accessor budget-report-structure 'maximum-expected) brep))

(define (budget-subreport-get-maximum-expected brep)
  ((record-accessor budget-subreport-structure 'maximum-expected) brep))

(define (budget-actual-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-report-get-actual report))))

(define (budget-nominal-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-report-get-nominal report))))

(define (budget-minimum-expected-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-report-get-minimum-expected report))))

(define (budget-maximum-expected-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-report-get-maximum-expected report))))

(define (budget-sub-nominal-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-subreport-get-nominal subreport))))

(define (budget-sub-minimum-expected-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-subreport-get-minimum-expected subreport))))

(define (budget-sub-maximum-expected-html-proc)
  (lambda (entry subentry report subreport)
    (html-currency-cell #f #f (budget-subreport-get-maximum-expected subreport))))

(define (budget-null-html-proc)
  (lambda (entry subentry report subreport)
    (html-generic-cell 
     #f #f #f "")))

(define budget-line-structure
  (make-record-type "budget-line-structure"
		    '(entry report)))

(define (make-budget-line entry report)
  ((record-constructor budget-line-structure) entry report))

(define (budget-line-get-entry line)
  ((record-accessor budget-line-structure 'entry) line))

(define (budget-line-get-report line)
  ((record-accessor budget-line-structure 'report) line))

(define report-spec-structure
  (make-record-type
   "report-spec-structure"
   '(header format-proc type)))

(define (make-report-spec header format-proc type)
  ((record-constructor report-spec-structure)
   header format-proc type))

(define (report-spec-get-header spec)
  ((record-accessor report-spec-structure 'header) spec))

(define (report-spec-get-format-proc spec)
  ((record-accessor report-spec-structure 'format-proc) spec))

(define (report-spec-get-type spec)
  ((record-accessor report-spec-structure 'type) spec))

(define (budget-line-html line report-specs)
  (let ((entry (budget-line-get-entry line))
	(report (budget-line-get-report line)))
    ;;(map-in-order
    (map
     (lambda (subentry subreport)
       (html-table-row-manual
	(map
	 (lambda (specs)
	   (case (report-spec-get-type specs)
	     ((gnc:report-all)
	      ((report-spec-get-format-proc specs) 
	       entry subentry report subreport))
	     ((gnc:report-first)
	      (if (eqv? subreport (car (budget-report-get-subreports report)))
		  ((report-spec-get-format-proc specs)
		   entry subentry report subreport)
		  ((budget-null-html-proc) 
		   entry subentry report subreport)))
	     ((gnc:report-last)
	      (if (= (cdr subentry) '())
		  ((report-spec-get-format-proc specs)
		   entry subentry report subreport)
		  ((budget-null-html-proc) 
		   entry subentry report subreport)))
	     (else (gnc:debug "budget-line-html: invalid type"))))
	 report-specs)))
     (budget-entry-get-subentries entry)
     (budget-report-get-subreports report))))

;; add a value to the budget accumulator
(define (budget-report-accumulate-actual! value budget-line)
  ((record-modifier budget-report-structure 'actual)
   (budget-line-get-report budget-line)
   (+ value (budget-report-get-actual (budget-line-get-report budget-line)))))

(define (budget-subreport-set-min-expected! subreport min-expected)
  ((record-modifier budget-subreport-structure 'minimum-expected)
   subreport min-expected))

(define (budget-subreport-set-max-expected! subreport max-expected)
  ((record-modifier budget-subreport-structure 'maximum-expected)
   subreport max-expected))

(define (budget-report-accumulate-min-expected! report min-expected)
  ((record-modifier budget-report-structure 'minimum-expected) report
   (+ min-expected (budget-report-get-minimum-expected report))))

(define (budget-report-accumulate-max-expected! report max-expected)
  ((record-modifier budget-report-structure 'maximum-expected) report
   (+ max-expected (budget-report-get-maximum-expected report))))

;; return the # of budget periods over the report period
(define (budget-num-periods subentry begin-date end-date)
  (/ (gnc:date-N-delta begin-date end-date
		       (budget-subentry-get-period-type subentry))
     (budget-subentry-get-period subentry)))


(define (budget-calculate-expected! budget-line begin-date end-date)
  (let ((entry (budget-line-get-entry budget-line))
	(report (budget-line-get-report budget-line)))
    (for-each 
     (lambda (subentry subreport)
       (let ((mechanism (budget-subentry-get-mechanism subentry)))
	 (cond (((record-predicate 
		  budget-bill-mechanism-structure) mechanism)
		(budget-calculate-bill! 
		 subentry subreport mechanism begin-date end-date))
	       (((record-predicate
		  budget-recurring-mechanism-structure) mechanism)
		(budget-calculate-recurring! 
		 subentry subreport mechanism begin-date end-date))
	       (((record-predicate
		  budget-contingency-mechanism-structure) mechanism)
		(budget-calculate-contingency! 
		 subentry subreport mechanism begin-date end-date))
	       (else (gnc:debug "invalid mechanism!")))
	 (budget-report-accumulate-min-expected! 
	  report (budget-subreport-get-minimum-expected subreport))
	 (budget-report-accumulate-max-expected! 
	  report (budget-subreport-get-maximum-expected subreport))))
     (budget-entry-get-subentries entry)
     (budget-report-get-subreports report))))
	       
;; calculate the nominal value.
(define (budget-calculate-nominal! budget-line begin-date end-date)
  ((record-modifier budget-report-structure 'nominal)
   (budget-line-get-report budget-line)
   (apply +
	  (map
	   (lambda (subentry subreport)
	     (let ((t (* (budget-subentry-get-amount subentry)
			 (budget-num-periods subentry begin-date end-date))))
	       ((record-modifier budget-subreport-structure 'nominal)
		subreport t)
	       t))
	   (budget-entry-get-subentries (budget-line-get-entry budget-line))
	   (budget-report-get-subreports (budget-line-get-report budget-line))))))

(define (budget-calculate-recurring! subentry subreport mechanism begin end)
  (let ((np (budget-num-periods subentry begin end))
	(amount (budget-subentry-get-amount subentry)))
    (budget-subreport-set-min-expected! subreport (* amount (floor np)))
    (budget-subreport-set-max-expected! subreport (* amount (ceiling np)))))

(define (budget-calculate-contingency! subentry subreport mechanism begin end)
  (let ((np (budget-num-periods subentry begin end))
	(amount (budget-subentry-get-amount subentry)))
    (let ((min
	   (max 0 (* (- np 1.0) amount))))
      (budget-subreport-set-min-expected! subreport min)
      (budget-subreport-set-max-expected! subreport (+ min amount)))))

(define (budget-calculate-bill! subentry subreport mechanism begin-date end-date)
  (let ((N-type (budget-subentry-get-period-type subentry))
	(window-start (budget-bill-get-window-start-day mechanism))
	(window-end (budget-bill-get-window-end-day mechanism))
	(psize (budget-subentry-get-period subentry))
	(amount (budget-subentry-get-amount subentry)))
      ; convert negative numbers to positive numbers
      (let ((trig-start-A (if (> window-start 0)
			      window-start
			      (+ window-start 
				 (gnc:days-in-period
				  begin-date N-type psize))))
	    (trig-start-B (if (> window-start 0)
			      window-start
			      (+ window-start 
				 (gnc:days-in-period
				  end-date N-type psize))))
	    (trig-end-A (if (> window-end 0)
			    window-end
			    (+ window-end
			       (gnc:days-in-period
				begin-date N-type psize))))
	    (trig-end-B (if (> window-end 0)
			    window-end
			    (+ window-end
			       (gnc:days-in-period
				end-date N-type psize))))
	    (possible 0)
	    (sure 0)
	    (report-start (gnc:date-to-N-remainder begin-date N-type psize))
	    (report-end (gnc:date-to-N-remainder end-date N-type psize)))
	; special case if report start and end are in same period
	(cond ((= (floor (/ (gnc:date-to-N-fraction end-date N-type) psize))
		  (floor (/ (gnc:date-to-N-fraction begin-date N-type) psize)))
	       (cond ((<= trig-start-A trig-end-A)
		      (cond ((> report-start trig-end-A) #f)
			    ((< report-end trig-start-A) #f)
			    ((or (> report-start trig-start-A) 
				 (< report-end trig-end-A))
			     (set! possible 1))
			    (else (set! sure 1))))
		     (else
		      (if (<= report-start trig-end-A) 
			  (set! possible 1))
		      (if (>= report-end trig-start-A) 
			  (set! possible (+ possible 1))))))
	      ; not in same period.
	      (else
	       ; first calculate terminal periods
	       (cond ((<= trig-start-A trig-end-A)
		      (cond ((> report-start trig-end-A) #f)
			    ((<= report-start trig-start-A) 
			     (set! sure (+ sure 1)))
			    (else (set! possible (+ possible 1))))
		      (cond ((< report-end trig-start-B) #f)
			    ((>= report-end trig-end-B) (set! sure (+ sure 1)))
			    (else (set! possible (+ possible 1)))))
		     (else
		      (if (<= report-start trig-end-A) 
			  (set! possible (+ possible 1)))
		      (if (>= report-end trig-start-B) 
			  (set! possible (+ possible 1)))
		      (if (or (> report-start trig-start-A)
			      (< report-end trig-end-B)) 
			  (set! possible (+ possible 1))
			  (set! sure (+ sure 1)))))
	       ; then add intermediate periods
	       (set! sure (+ -1 sure 
			     (inexact->exact
			      (- (floor (/ (gnc:date-to-N-fraction 
					    end-date N-type) psize))
				 (floor (/ (gnc:date-to-N-fraction 
					    begin-date N-type) psize))))))))
	; now save 'em into the record
	(budget-subreport-set-min-expected! subreport 
					    (* amount sure))
	(budget-subreport-set-max-expected! subreport 
					    (* amount (+ sure possible))))))

;; given an account name, return the budget line
;; return #f if there is no budget line for that account
(define (budget-get-line account-name budg)
  (let loop1 ((budget budg))
    (cond ((null? budget) #f)
	  (else
	   (cond ((budget-get-line-2 account-name (car budget))
		  (car budget))
		 (else (loop1 (cdr budget))))))))

;; I should be able to put this inside budget-get-line, but for some
;; reason, it screws up.
(define (budget-get-line-2 account-name budget-line)
  (let loop2
      ((accounts (budget-entry-get-accounts 
		  (budget-line-get-entry budget-line))))
    (cond ((null? accounts) #f)
	  (else 
	   (cond ((or (string=? account-name (car accounts))
		      (loop2 (cdr accounts)))
		  budget-line)
		 (else #f))))))


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
    'full
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

(define gnc:budget-full-report-specs
  (list
   (make-report-spec 
    "Description" (budget-description-html-proc) 'gnc:report-first)
   (make-report-spec
    "Accounts" (budget-accounts-html-proc) 'gnc:report-first)
   (make-report-spec 
    "Description (subs)" (budget-sub-description-html-proc) 'gnc:report-all)
   (make-report-spec
    "Amount" (budget-amount-html-proc) 'gnc:report-all)
   (make-report-spec
    "Period" (budget-period-html-proc) 'gnc:report-all)
   (make-report-spec
    "" (budget-period-type-html-proc) 'gnc:report-all)
   (make-report-spec
    "Window Start Day" (budget-window-start-day-html-proc) 'gnc:report-all)
   (make-report-spec
    "Window End Day" (budget-window-end-day-html-proc) 'gnc:report-all)
   (make-report-spec
    "Actual" (budget-actual-html-proc) 'gnc:report-first)
   (make-report-spec
    "Nominal (total)" (budget-nominal-html-proc) 'gnc:report-first)
   (make-report-spec
    "Nominal" (budget-sub-nominal-html-proc) 'gnc:report-all)
   (make-report-spec
    "Upper Limit (total)" (budget-maximum-expected-html-proc) 'gnc:report-first)
   (make-report-spec
    "Upper Limit" (budget-sub-maximum-expected-html-proc) 'gnc:report-all)
   (make-report-spec
    "Lower Limit (total)" (budget-minimum-expected-html-proc) 'gnc:report-first)
   (make-report-spec
    "Lower Limit" (budget-sub-minimum-expected-html-proc) 'gnc:report-all)))

(define gnc:budget-balance-report-specs
  (list
   (make-report-spec 
    "Description" (budget-description-html-proc) 'gnc:report-first)
   (make-report-spec
    "Accounts" (budget-accounts-html-proc) 'gnc:report-first)
   (make-report-spec 
    "Description (subs)" (budget-sub-description-html-proc) 'gnc:report-all)
   (make-report-spec
    "Amount" (budget-amount-html-proc) 'gnc:report-all)
   (make-report-spec
    "Period" (budget-period-html-proc) 'gnc:report-all)
   (make-report-spec
    "" (budget-period-type-html-proc) 'gnc:report-all)
   (make-report-spec
    "Window Start Day" (budget-window-start-day-html-proc) 'gnc:report-all)
   (make-report-spec
    "Window End Day" (budget-window-end-day-html-proc) 'gnc:report-all)
   (make-report-spec
    "Nominal (total)" (budget-nominal-html-proc) 'gnc:report-first)
   (make-report-spec
    "Nominal" (budget-sub-nominal-html-proc) 'gnc:report-all)))

(define gnc:budget-status-report-specs
  (list
   (make-report-spec 
    "Description" (budget-description-html-proc) 'gnc:report-first)
   (make-report-spec
    "Upper Limit" (budget-maximum-expected-html-proc) 'gnc:report-first)
   (make-report-spec
    "Lower Limit" (budget-minimum-expected-html-proc) 'gnc:report-first)
   (make-report-spec
    "Actual" (budget-actual-html-proc) 'gnc:report-first)))

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
			  (make-budget-line entry (make-empty-budget-report entry)))
			gnc:budget-entries)))

     (let loop ((group (gnc:get-current-group)))
       (if (not (pointer-token-null? group))
	   (gnc:group-map-accounts
	    (lambda (account)
	      (if (eqv? (gnc:account-type->symbol (gnc:account-get-type account))
			'EXPENSE)
		  (let* ((line
			  (budget-get-line
			   (gnc:account-get-full-name account)
			   budget-list))
			 (line2 (if line line (car budget-list))))
		    (gnc:for-each-split-in-account 
		     account
		     (lambda (split)
		       (budget-report-accumulate-actual!
			(gnc:split-get-value split) line2)))))
	      (loop (gnc:account-get-children account)))
	    group)))

     (for-each 
      (lambda (line)
	(begin
	  (budget-calculate-nominal! line begin-date-secs end-date-secs)
	  (budget-calculate-expected! line begin-date-secs end-date-secs)))
      budget-list)
     
     (let ((report-specs 
	    (case (gnc:option-value
		   (gnc:lookup-option options "Report Options" "View"))
	      ((full) gnc:budget-full-report-specs)
	      ((balancing) gnc:budget-balance-report-specs)
	      ((status) gnc:budget-status-report-specs)
	      (else (gnc:debug (list "Invalid view option" 
				     (gnc:option-value
				      (gnc:lookup-option options "Report Options" "View"))))))))
       (list
	(html-start-document)
	"<p>This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.</p>"
	(html-start-table)
	(html-table-row-manual
	 ;;(map-in-order
	 (map
	  (lambda (spec) 
	    (html-cell-header 
	     (report-spec-get-header spec)))
	  report-specs))
	;;(map-in-order
	(map
	 (lambda (line)	 
	   (budget-line-html line report-specs))
	 budget-list)
	(html-end-table)
	(html-end-document))))))