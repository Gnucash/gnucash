;; -*-scheme-*-
;;
;; budget-report.scm
;; Report on budget
;; Bryan Larsen (blarsen@ada-works.com)
;; with contributions from Christopher Browne (cbbrowne@hex.net)

;; TODO
;; "upcoming/overdue bills" report
;; druids to enter budget
;; save/load budget
;; internationalization
;; speedup:  move subexpressions outside loops
;;           don't calculate values that aren't needed
;; graph budget progress
;; save report parameters - "favorite" reports
;; "unbudgeted" report
;; deal with non-integer periods (only if people ask for it.  I don't
;; think it's necessary and don't want to do it)

(require 'sort)
(require 'record)
(gnc:depend "report-utilities.scm")
(gnc:depend "html-generator.scm")
(gnc:depend "date-utilities.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Budget Entry
(define budget-entry-structure
  (make-record-type 
   "budget-entry-structure" 
   '(description accounts filter-pred subentries action)))

(define budget-subentry-structure 
  (make-record-type
   "budget-subentry-structure"
   '(description amount period period-type mechanism)))

(define budget-recurring-mechanism-structure
  (make-record-type 
   "budget-recurring-mechanism-structure"
   '()))

(define budget-nominal-mechanism-structure
  (make-record-type
   "budget-nominal-mechanism-structure"
   '()))

(define budget-bill-mechanism-structure
  (make-record-type 
   "budget-bill-mechanism-structure"
   '(window-start-day window-end-day)))

(define budget-contingency-mechanism-structure
  (make-record-type 
   "budget-contingency-mechanism-structure"
   '()))

(define make-budget-entry
  (record-constructor budget-entry-structure))

(define make-budget-subentry
  (record-constructor budget-subentry-structure))

(define make-recurring-mechanism
  (record-constructor budget-recurring-mechanism-structure))

(define make-bill-mechanism
  (record-constructor budget-bill-mechanism-structure))

(define make-contingency-mechanism
  (record-constructor budget-contingency-mechanism-structure))

(define make-nominal-mechanism
  (record-constructor budget-nominal-mechanism-structure))


(define budget-entry-get-description
  (record-accessor budget-entry-structure 'description))

(define budget-subentry-get-description
  (record-accessor budget-subentry-structure 'description))

(define budget-entry-get-accounts
  (record-accessor budget-entry-structure 'accounts))

(define budget-entry-get-subentries
  (record-accessor budget-entry-structure 'subentries))

(define budget-entry-get-action
  (record-accessor budget-entry-structure 'action))

(define budget-entry-get-filter-pred
  (record-accessor budget-entry-structure 'filter-pred))

(define budget-subentry-get-amount
  (record-accessor budget-subentry-structure 'amount))

(define budget-subentry-get-period
  (record-accessor budget-subentry-structure 'period))

(define budget-subentry-get-period-type
  (record-accessor budget-subentry-structure 'period-type))

(define budget-bill-get-window-start-day
  (record-accessor budget-bill-mechanism-structure 'window-start-day))

(define budget-bill-get-window-end-day
  (record-accessor budget-bill-mechanism-structure 'window-end-day))

(define budget-subentry-get-mechanism
  (record-accessor budget-subentry-structure 'mechanism))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; useful filter-pred's for a budget entry

(define (budget-filter-pred-debit split budget-line)
  (> (d-gnc:split-get-value split) 0))

;; make-budget-entry:  
;;  1: description, 
;;  2: list of accounts
;;  3: filter pred: given the split and the budget line, return #t if
;;                  the split should be added to the budget.  (before
;;                  calling, the transaction is already filtered on
;;                  date & accounts).
;;  4: list of subentries.
;;  5: action:  #t: normal budget line
;;              'gnc:budget-comment:  display, but do not total
;;              #f: ignore

;; make-budget-subentry:
;;  1: description
;;  2: amount
;;  3: period size (number of days, weeks, months or years)
;;  4: whether #3 is in days, months, weeks or years
;;  5: mechanism (nominal, bill, recurring or contingency)

;; make-nominal-mechanism.
;;  (no parameters).  A nominal budget line is probably the budget
;;  type you are most used to.  The expected value for half a year of
;;  a one year budget line would be half the amount.

;; make-recurring-mechanism.
;;  (no parameters).  This type is designed for budget items that
;;  happen regularly, but on no fixed date.  For example, if you
;;  service your car 3 times a year at $40 a pop, enter an amount of
;;  $40 and a period of 4 months rather than $120 a year.

;; make-bill-mechanism.
;;  This type is designed for budget items that happen on a fixed
;;  date.  You can specify a window around the date in case you pay a
;;  little early or late.
;;  The two parameters specify the start or stop day.  This is the day
;;  number in the period.  For example, 15 would specify the 15th of
;;  the month, 1 specifies sunday, et cetera.  The first day in the
;;  period is "1".  The last day in the period is "0", and negative
;;  numbers are used to count backwards from the last day.  For
;;  example, Christmas Day is -6 for a year.

;; make-contingency-mechansim.  
;;  Use this for unexpected expenses.  The budget saves up money for
;;  the unexpected expense, and then always keeps the full amount on
;;  hand.

(define gnc:budget-entries
  (list
   (make-budget-entry 
    "gross" '("Income:Gross Employment Income") #f 
    (list
     (make-budget-subentry #f -99999.99 1 'gnc:budget-month 
			   (make-bill-mechanism -1 2)))
    #t)
   (make-budget-entry 
    "bank interest" '("Expense:Bank Charges:Interest") #f 
    (list
     (make-budget-subentry #f 40 1 'gnc:budget-month 
			   (make-bill-mechanism -4 0)))
    #t)
   (make-budget-entry 
    "bank feed" '("Expense:Bank Charges:Fees") #f 
    (list
     (make-budget-subentry #f 50 1 'gnc:budget-year 
                           (make-bill-mechanism 27 27)))
    #t)
   (make-budget-entry 
    "cell phone" '("Expense:Bills:Cell phone") #f 
    (list
     (make-budget-subentry #f 60 1 'gnc:budget-month 
			   (make-bill-mechanism -4 -1)))
    #t)
   (make-budget-entry 
    "hydro" '("Expense:Bills:Hydro") #f 
    (list
     (make-budget-subentry #f 20 1 'gnc:budget-month
			   (make-bill-mechanism 15 19)))
    #t)
   (make-budget-entry 
    "life insurance" '("Expense:Bills:Life Insurance") #f 
    (list
     (make-budget-subentry #f 15 1 'gnc:budget-month 
			   (make-bill-mechanism 1 3)))
    #t)
   (make-budget-entry 
    "diesel" '("Expense:Car:Diesel") #f 
    (list
     (make-budget-subentry #f 30 4 'gnc:budget-week
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry 
    "licenses" '("Expense:Car:Licenses") #f 
    (list
     (make-budget-subentry #f 1000 1 'gnc:budget-year
			   (make-bill-mechanism -122 -108)))
    #t)
   (make-budget-entry
    "car maintenance" '("Expense:Car:Maintenance") #f 
    (list
     (make-budget-subentry #f 100 6 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "car misc" '("Expense:Car:Miscellaneous") #f 
    (list
     (make-budget-subentry #f 5 1 'gnc:budget-week
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "charitable" '("Expense:Charitable:Non-taxable"
		   "Expense:Charitable:Taxable") #f 
    (list
     (make-budget-subentry #f 200 1 'gnc:budget-year
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "entertainment" '("Expense:Entertainment:Beer (out)" 
		      "Expense:Entertainment:Cover"
		      "Expense:Entertainment:Date"
		      "Expense:Entertainment:Dining"
		      "Expense:Entertainment:Dues"
		      "Expense:Entertainment:Goodwill"
		      "Expense:Entertainment:Liquor (home)") #f 
    (list
     (make-budget-subentry #f 50 1 'gnc:budget-week
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "groceries" '("Expense:Food:Groceries") #f 
    (list
     (make-budget-subentry #f 125 1 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "junk food" '("Expense:Food:Junk") #f 
    (list
     (make-budget-subentry #f 0.5 1 'gnc:budget-day
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "lunch" '("Expense:Food:Lunch") #f
    (list
     (make-budget-subentry #f 8 1 'gnc:budget-day
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "gifts" '("Expense:Gifts") #f 
    (list
     (make-budget-subentry #f 400 1 'gnc:budget-year
			   (make-recurring-mechanism))
     (make-budget-subentry "xmas" 400 1 'gnc:budget-year
			   (make-bill-mechanism -30 -5)))
    #t)
   (make-budget-entry 
    "rent" '("Expense:Household:Rent") #f 
    (list
     (make-budget-subentry #f 312.50 1 'gnc:budget-month
			   (make-bill-mechanism 1 2)))
    #t)
   (make-budget-entry
    "house junk" '("Expense:Household:Stuff") #f 
    (list
     (make-budget-subentry #f 25 1 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "medical" '("Expense:Medical:Dental" 
		"Expense:Medical:Optical"
		"Expense:Medical:Other") #f 
    (list
     (make-budget-subentry #f 1000 1 'gnc:budget-year
			   (make-contingency-mechanism)))
    #t)
   (make-budget-entry
    "clothes" '("Expense:Personal:Clothes") #f 
    (list
     (make-budget-subentry #f 150 3 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "hygeine" '("Expense:Personal:Personal maintenance") #f 
    (list
     (make-budget-subentry #f 30 1 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "newspapers" '("Expense:Stuff:Newspapers") #f 
    (list
     (make-budget-subentry #f 20.52 1 'gnc:budget-month
			   (make-bill-mechanism 14 14)))
    #t)
   (make-budget-entry "stuff" '("Expense:Stuff:CD's"
				"Expense:Stuff:Electronic entertainment"
				"Expense:Stuff:Fiction" 
				"Expense:Stuff:Games"
				"Expense:Stuff:Magazines" 
				"Expense:Stuff:Musical Equipment"
				"Expense:Stuff:Software"
				"Expense:Stuff:Sports equipment"
				"Expense:Stuff:Videos") #f 
    (list
     (make-budget-subentry #f 250 1 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "student loan" '("Expense:Bills:Student Loan Interest"
		     "Loans:student loan") #f 
    (list
     (make-budget-subentry #f 94.54 1 'gnc:budget-month
			   (make-bill-mechanism -1 -1)))
    #t)
   (make-budget-entry
    "car loan" '("Expense:Car:Loan Interest"
				   "Loans:Car Loan") #f 
    (list
     (make-budget-subentry #f 374.18 1 'gnc:budget-month
			   (make-bill-mechanism 15 17)))
    #t)
   (make-budget-entry
    "RRSP loan" '("Expense:Investment Expenses:RRSP LOC Interest"
		  "Loans:Scotia Bank RRSP Line of Credit") 
    budget-filter-pred-debit
    (list
     (make-budget-subentry #f 100 1 'gnc:budget-month
			   (make-bill-mechanism 5 8)))
    #t)
   (make-budget-entry 
    "cash write off" '("Expense:Cash write-off") #f 
    (list
     (make-budget-subentry #f 60 1 'gnc:budget-month
			   (make-recurring-mechanism)))
    #t)
   (make-budget-entry
    "taxes" '("Expense:Taxes:CPP" "Expense:Taxes:EI" 
	      "Expense:Taxes:Federal Income Tax") #f 
    (list
     (make-budget-subentry #f 1034.38 1 'gnc:budget-month
			   (make-bill-mechanism -1 -2)))
    #t)))


;; these are the "other collectors".  This is where all transactions
;; that are not accounted for in the main budget go.  These are sorted
;; by account-type, which is an integer.
(define gnc:budget-others
  (list
   (make-budget-entry "other bank" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other cash" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other asset" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other credit" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other liability" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other stock" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other mutual" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other currency" '() #f
                      (list
                       (make-budget-subentry #f 0 1 'gnc:budget-month
                                             (make-nominal-mechanism)))
                      #f)
   (make-budget-entry "other income" '() #f
                      (list
                       (make-budget-subentry #f -10000 5 'gnc:budget-year
                                             (make-contingency-mechanism)))
                      'gnc:budget-comment)
   (make-budget-entry "other expense" '() #f
                      (list
                       (make-budget-subentry #f 10000 5 'gnc:budget-year
                                             (make-contingency-mechanism)))
                      'gnc:budget-comment)
   (make-budget-entry "other equity" '() #f
                      (list
                       (make-budget-subentry #f 10000 5 'gnc:budget-year
                                             (make-contingency-mechanism)))
                      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Budget Report
(define budget-report-structure
  (make-record-type 
   "budget-report-structure" 
   '(actual nominal minimum-expected maximum-expected delta account-type subreports)))

(define budget-subreport-structure
  (make-record-type
   "budget-subreport-structure"
   '(nominal minimum-expected maximum-expected)))

(define (make-empty-budget-report entry)
  ((record-constructor budget-report-structure) 
   0 0 0 0 0 0
   (map
    (lambda (subentry)
      (make-empty-subreport))
    (budget-entry-get-subentries entry))))

(define (make-empty-subreport)
  ((record-constructor budget-subreport-structure)
   0 0 0))

(define budget-report-get-subreports
  (record-accessor budget-report-structure 'subreports))

(define budget-report-get-account-type
  (record-accessor budget-report-structure 'account-type))

(define budget-report-set-account-type!
  (record-modifier budget-report-structure 'account-type))

(define budget-report-get-delta
  (record-accessor budget-report-structure 'delta))

(define budget-report-set-delta!
  (record-modifier budget-report-structure 'delta))

(define budget-report-get-actual
  (record-accessor budget-report-structure 'actual))

(define budget-report-get-nominal
  (record-accessor budget-report-structure 'nominal))

(define budget-subreport-get-nominal
  (record-accessor budget-subreport-structure 'nominal))

(define budget-report-get-minimum-expected
  (record-accessor budget-report-structure 'minimum-expected))

(define budget-subreport-get-minimum-expected
  (record-accessor budget-subreport-structure 'minimum-expected))

(define budget-report-get-maximum-expected
  (record-accessor budget-report-structure 'maximum-expected))

(define budget-subreport-get-maximum-expected
  (record-accessor budget-subreport-structure 'maximum-expected))

(define budget-report-set-actual!
  (record-modifier budget-report-structure 'actual))

(define (budget-report-accumulate-actual! value budget-line)
  (budget-report-set-actual!
   (budget-line-get-report budget-line)
   (+ value (budget-report-get-actual
	     (budget-line-get-report budget-line)))))

(define budget-subreport-set-min-expected!
  (record-modifier budget-subreport-structure 'minimum-expected))

(define budget-subreport-set-max-expected!
  (record-modifier budget-subreport-structure 'maximum-expected))

(define budget-report-set-min-expected!
  (record-modifier budget-report-structure 'minimum-expected))

(define budget-report-set-max-expected!
  (record-modifier budget-report-structure 'maximum-expected))

(define (budget-report-accumulate-min-expected! report min-expected)
  (budget-report-set-min-expected! report
   (+ min-expected (budget-report-get-minimum-expected report))))

(define (budget-report-accumulate-max-expected! report max-expected)
  (budget-report-set-max-expected! report
   (+ max-expected (budget-report-get-maximum-expected report))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Budget Line
(define budget-line-structure
  (make-record-type "budget-line-structure"
		    '(entry report)))

(define make-budget-line
  (record-constructor budget-line-structure))

(define budget-line-get-entry
  (record-accessor budget-line-structure 'entry))

(define budget-line-get-report
  (record-accessor budget-line-structure 'report))

(define (budget-line-make-entry-proc entry-proc)
  (lambda (line)
    (entry-proc (budget-line-get-entry line))))

(define (budget-line-make-subentry-list-proc subentry-proc)
  (lambda (line)
    (map
     (lambda (subentry)
       (subentry-proc subentry))
     (budget-entry-get-subentries (budget-line-get-entry line)))))

(define (budget-line-make-report-proc report-proc)
  (lambda (line)
    (report-proc (budget-line-get-report line))))

(define (budget-line-make-subreport-list-proc subreport-proc)
  (lambda (line)
    (map
     (lambda (subreport)
       (subreport-proc subreport))
     (budget-report-get-subreports (budget-line-get-report line)))))

(define (budget-line-count-subentries line)
  (length
   (budget-entry-get-subentries (budget-line-get-entry line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return the # of budget periods over the report period
(define (budget-num-periods subentry begin-date end-date)
  (/ (gnc:date-N-delta begin-date end-date
		       (budget-subentry-get-period-type subentry))
     (budget-subentry-get-period subentry)))

(define budget-bill-pred
  (record-predicate budget-bill-mechanism-structure))

(define budget-recurring-pred
  (record-predicate budget-recurring-mechanism-structure))

(define budget-contingency-pred
  (record-predicate budget-contingency-mechanism-structure))

(define budget-nominal-pred
  (record-predicate budget-nominal-mechanism-structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-calculate-expected! budget-line begin-date end-date)
  (let ((report (budget-line-get-report budget-line)))
    (for-each 
     (lambda (subentry subreport)
       (let ((mechanism (budget-subentry-get-mechanism subentry)))
	 (cond ((budget-bill-pred mechanism)
		(budget-calculate-bill! 
		 subentry subreport mechanism begin-date end-date))
	       ((budget-recurring-pred mechanism)
		(budget-calculate-recurring! 
		 subentry subreport mechanism begin-date end-date))
	       ((budget-contingency-pred mechanism)
		(budget-calculate-contingency! 
		 subentry subreport mechanism begin-date end-date))
	       ((budget-nominal-pred mechanism)
		(budget-calculate-nominal-subreport!
		 subentry subreport mechanism begin-date end-date))
	       (else (gnc:error "invalid mechanism!")))
	 (budget-report-accumulate-min-expected! 
	  report (budget-subreport-get-minimum-expected subreport))
	 (budget-report-accumulate-max-expected! 
	  report (budget-subreport-get-maximum-expected subreport))))
     
     (budget-entry-get-subentries (budget-line-get-entry budget-line))
     (budget-report-get-subreports report))))

;; calculate the nominal value.
(define budget-report-set-nominal!
  (record-modifier budget-report-structure 'nominal))
(define budget-subreport-set-nominal!
  (record-modifier budget-subreport-structure 'nominal))
(define (budget-calculate-nominal! budget-line begin-date end-date)
  (budget-report-set-nominal!
   (budget-line-get-report budget-line)
   (apply +
	  (map
	   (lambda (subentry subreport)
	     (let ((t (* (budget-subentry-get-amount subentry)
			 (budget-num-periods subentry begin-date end-date))))
	       (budget-subreport-set-nominal! subreport t)
	       t))
	   (budget-entry-get-subentries (budget-line-get-entry budget-line))
	   (budget-report-get-subreports (budget-line-get-report budget-line))))))

(define (budget-calculate-nominal-subreport! subentry subreport mechanism begin end)
  (let ((n (* (budget-subentry-get-amount subentry)
	      (budget-num-periods subentry begin end))))
    (budget-subreport-set-min-expected! subreport n)
    (budget-subreport-set-max-expected! subreport n)))

(define (budget-calculate-recurring! subentry subreport mechanism begin end)
  (let ((np (budget-num-periods subentry begin end))
	(amount (budget-subentry-get-amount subentry)))
    (budget-subreport-set-min-expected! subreport (* amount (floor np)))
    (budget-subreport-set-max-expected! subreport (* amount (ceiling np)))))

(define (budget-calculate-contingency! subentry subreport mechanism begin end)
  (let ((np (budget-num-periods subentry begin end))
	(amount (budget-subentry-get-amount subentry)))
    (let ((min
	   (if (>= amount 0)
	       (max 0 (* (- np 1.0) amount))
	       (min 0 (* (- np 1.0) amount)))))
      (budget-subreport-set-min-expected! subreport min)
      (budget-subreport-set-max-expected! subreport (+ min amount)))))

(define (budget-calculate-bill! subentry subreport mechanism begin-date end-date)
  (letrec ((N-type (budget-subentry-get-period-type subentry))
           (window-start (budget-bill-get-window-start-day mechanism))
           (window-end (budget-bill-get-window-end-day mechanism))
           (psize (budget-subentry-get-period subentry))
           (amount (budget-subentry-get-amount subentry))
           (date-positiverter
            (lambda (num date)
              (if (> num 0)
                  num
                  (+ num (gnc:days-in-period num date N-type psize))))))
    ;; convert negative numbers to positive numbers
    (let ((trig-start-A (date-positiverter window-start begin-date))
          (trig-start-B (date-positiverter window-start end-date))
          (trig-end-A (date-positiverter window-end begin-date))
          (trig-end-B (date-positiverter window-end end-date))
          (possible 0)
          (sure 0)
          (report-start (gnc:date-to-N-remainder begin-date N-type psize))
          (report-end (gnc:date-to-N-remainder end-date N-type psize))
          (date-diff (inexact->exact
                      (- (floor (/ (gnc:date-to-N-fraction 
                                    end-date N-type) psize))
                         (floor (/ (gnc:date-to-N-fraction 
                                    begin-date N-type) psize))))))
      ;; special case if report start and end are in same period
      (if (= 0 date-diff)
          (cond ((<= trig-start-A trig-end-A)
                 (cond ((or (> report-start trig-end-A)
                            (< report-end trig-start-A))
                        #f)
                       ((or (> report-start trig-start-A) 
                            (< report-end trig-end-A))
                        (set! possible 1))
                       (else
                        (set! sure 1))))
                (else
                 (if (<= report-start trig-end-A)
                     (set! possible 1))
                 (if (>= report-end trig-start-A)
                     (set! possible (+ possible 1)))))
          ;; not in same period.
          ;; first calculate terminal periods
          (begin
            (cond ((<= trig-start-A trig-end-A)
                   (cond ((> report-start trig-end-A)
                          #f)
                         ((<= report-start trig-start-A) 
                          (set! sure 1))
                         (else
                          (set! possible 1)))
                   (cond ((< report-end trig-start-B)
                          #f)
                         ((>= report-end trig-end-B)
                          (set! sure 1))
                         (else
                          (set! possible 1))))
                  (else
                   (if (<= report-start trig-end-A) 
                       (set! possible (+ possible 1)))
                   (if (>= report-end trig-start-B) 
                       (set! possible (+ possible 1)))
                   (if (or (> report-start trig-start-A)
                           (< report-end trig-end-B)) 
                       (set! possible (+ possible 1))
                       (set! sure (+ sure 1)))))
            ;; then add intermediate periods
            (set! sure (+ -1 sure date-diff))))
      ;; now save 'em into the record
      (budget-subreport-set-min-expected! subreport 
                                          (* amount sure))
      (budget-subreport-set-max-expected! subreport 
                                          (* amount (+ sure possible))))))

(define (budget-calculate-actual! budget-hash others begin-date-secs end-date-secs)
  (let loop ((group (gnc:get-current-group)))
    (cond 
     (group
      (gnc:group-map-accounts
       (lambda (account)
	 (let* ((line
		 (budget-get-line-hash budget-hash
                                       (gnc:account-get-full-name account)))
		(line2 (cond (line line) 
			     (else 
			      (vector-ref others
                                          (gnc:account-get-type account)))))
		(acc 0)
		(filter-pred (budget-entry-get-filter-pred 
			      (budget-line-get-entry line2))))
	   (budget-report-set-account-type! (budget-line-get-report line2)
					    (gnc:account-get-type account))
	   (cond
	    ((budget-entry-get-action (budget-line-get-entry line2))
	     (set! acc 0)
	     (gnc:for-each-split-in-account 
	      account
	      (lambda (split)
		(let ((date 
		       (car (gnc:timepair-canonical-day-time 
			     (gnc:transaction-get-date-posted
			      (gnc:split-get-parent split))))))
		  (cond 
		   ((and (>= date begin-date-secs)
			 (<= date end-date-secs))
		    (cond
		     ((not line)
		      (gnc:debug (list 
				  (gnc:account-get-full-name account)
				  (d-gnc:split-get-value split)))))
		    (cond 
		     (filter-pred 
		      (cond
		       ((filter-pred split line2)
			(set! acc (+ acc (d-gnc:split-get-value split))))))
		     (else
		      (set! acc (+ acc (d-gnc:split-get-value split))))))))))
	     (budget-report-accumulate-actual! acc line2)))
	   (loop (gnc:account-get-children account))))
       group)))))

(define (budget-calculate-delta! line)
  (let ((entry (budget-line-get-entry line))
	(report (budget-line-get-report line)))
    (let ((minimum (budget-report-get-minimum-expected report))
	  (maximum (budget-report-get-maximum-expected report))
	  (actual (budget-report-get-actual report)))
       ;; note: for income, min > max, so swap if necessary
      (let ((mn (min minimum maximum))
	    (mx (max minimum maximum)))
	(budget-report-set-delta! 
	 report 
	 (cond ((<= actual mn) (- mn actual))
	       ((>= actual mx) (- mx actual))
	       (else 0.0)))))))

;;; Hash search
(define budget-get-line-hash
  (hash-inquirer string=?))

(define make-budget-hash-entry
  (hash-associator string=?))

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
	  (cons 'absolute (cons time 0)))))
    #f 'absolute #f))

  ;; to-date
  (gnc:register-budget-report-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report end date"
    (lambda () (cons 'absolute (cons (current-time) 0)))
    #f 'absolute #f))

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

(define (gnc:date-to-N-fraction caltime type)
  (case type
    ((gnc:budget-day) (gnc:date-to-day-fraction caltime))
    ((gnc:budget-week) (gnc:date-to-week-fraction caltime))
    ((gnc:budget-month) (gnc:date-to-month-fraction caltime))
    ((gnc:budget-year) (gnc:date-to-year-fraction caltime))
    (else (gnc:error "undefined period type in budget!") #f)))

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
    (else (gnc:error "undefined period type in budget!") #f)))

;; returns the "day number" of the specified period.  For example,
;; December 31 is day #92 in a 3 month period.
;; This is one based arithmetic, so the name "remainder" may be slightly
;; confusing.
(define (gnc:date-to-N-remainder caltime type num-periods)
  (let ((lt (localtime caltime)))
    (case type
      ((gnc:budget-day) (+ 1 
			   (remainder 
			    (inexact->exact
                             (floor (gnc:date-to-day-fraction caltime)))
			    num-periods)))
      ((gnc:budget-week) (+ (gnc:date-get-week-day lt)
			    (* 7 (remainder 
				  (inexact->exact 
				   (floor (gnc:date-to-week-fraction caltime)))
				  num-periods))))
      ((gnc:budget-month) (+ (gnc:date-get-month-day lt)
			     (let loop
                                 ((month 
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
      (else (gnc:error "undefined period type in budget!") #f))))


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
           ((month (* (quotient (inexact->exact 
				 (floor (gnc:date-to-month-fraction date)))
				n) n)))
	 (+ (gnc:days-in-month (+ 1 (remainder month 12))
			       (+ 1970 (quotient month 12)))
	    (if (= (remainder month n) (- n 1))
		0
		(loop (+ 1 month))))))
      ((gnc:budget-year)
       (let loop
           ((year (* (quotient (inexact->exact 
				(floor (gnc:date-to-year-fraction date))) n)
		     n)))
	 (+ (gnc:days-in-year (+ 1970 year))
	    (if (= (remainder year n) (- n 1))
		0
		(loop (+ 1 year)))))))))


(define gnc:budget-full-report-specs
  (list
   (make-report-spec 
    "Description"
    (budget-line-make-entry-proc budget-entry-get-description)
    (html-make-left-cell html-string)
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-description)
    (html-make-left-cell (html-make-ital html-string)))
   ;; fixme:  accounts
;   (make-report-spec 
;    "Account Type"
;    (budget-line-make-report-proc budget-report-get-account-type)
;    (html-make-left-cell 
;     (lambda (acc) (symbol->string (gnc:account-type->symbol acc))))
;    #f ; total-proc
;    #f ; subtotal-html-proc
;    #f ; total-html-proc
;    #t ; first-last-preference
;    #f ; subentry-list-proc
;    #f) ; subentry-html-proc
   (make-report-spec 
    "Amount"
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-amount)
    (html-make-right-cell html-currency))
   (make-report-spec
    "Period"
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-period)
    (html-make-left-cell (lambda (n) (html-number "%i" n))))
   (make-report-spec
    ""
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-period-type)
    (html-make-left-cell 
     (lambda (type) (html-string (gnc:date-describe-type type)))))
   ;; todo: window start/end
   (make-report-spec
    "Actual"
    (budget-line-make-report-proc budget-report-get-actual)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    #f ; subentry-list-proc
    #f) ; subentry-html-proc
   (make-report-spec
    "Nominal"
    (budget-line-make-report-proc budget-report-get-nominal)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-nominal)
    (html-make-right-cell (html-make-ital html-currency)))
   (make-report-spec
    "Upper Limit"
    (budget-line-make-report-proc budget-report-get-maximum-expected)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-maximum-expected)
    (html-make-right-cell (html-make-ital html-currency)))
   (make-report-spec
    "Lower Limit"
    (budget-line-make-report-proc budget-report-get-minimum-expected)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-minimum-expected)
    (html-make-right-cell (html-make-ital html-currency)))
   (make-report-spec
    "Status"
    (budget-line-make-report-proc budget-report-get-delta)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    #f ; subentry-list-proc
    #f) ; subentry-html-proc
   ))

(define gnc:budget-full-report-sort-specs
  (list
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-account-type)
    <
    =
    =
    (lambda (acc) (symbol->string (gnc:account-type->symbol acc))))
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-actual)
    <
    =
    #f
    #f)))

(define gnc:budget-balance-report-specs
  (list
   (make-report-spec 
    "Description"
    (budget-line-make-entry-proc budget-entry-get-description)
    (html-make-left-cell html-string)
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-description)
    (html-make-left-cell (html-make-ital html-string)))
   (make-report-spec 
    "Amount"
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-amount)
    (html-make-right-cell html-currency))
   (make-report-spec
    "Period"
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-period)
    (html-make-left-cell (lambda (n) (html-number "%i" n))))
   (make-report-spec
    ""
    #f ; get-value-proc
    #f ; html-proc
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-period-type)
    (html-make-left-cell 
     (lambda (type) (html-string (gnc:date-describe-type type)))))
   ;; todo: window start/end
   (make-report-spec
    "Nominal"
    (budget-line-make-report-proc budget-report-get-nominal)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-nominal)
    (html-make-right-cell (html-make-ital html-currency)))
   ))

(define gnc:budget-balance-report-sort-specs
  (list
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-account-type)
    <
    =
    =
    (lambda (acc) (symbol->string (gnc:account-type->symbol acc))))
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-nominal)
    <
    =
    #f
    #f)))

(define gnc:budget-status-report-specs
  (list
   (make-report-spec 
    "Description"
    (budget-line-make-entry-proc budget-entry-get-description)
    (html-make-left-cell html-string)
    #f ; total-proc
    #f ; subtotal-html-proc
    #f ; total-html-proc
    #t ; first-last-preference
    (budget-line-make-subentry-list-proc budget-subentry-get-description)
    (html-make-left-cell (html-make-ital html-string)))
   (make-report-spec
    "Actual"
    (budget-line-make-report-proc budget-report-get-actual)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    #f ; subentry-list-proc
    #f) ; subentry-html-proc
   (make-report-spec
    "Upper Limit"
    (budget-line-make-report-proc budget-report-get-maximum-expected)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-maximum-expected)
    (html-make-right-cell (html-make-ital html-currency)))
   (make-report-spec
    "Lower Limit"
    (budget-line-make-report-proc budget-report-get-minimum-expected)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    (budget-line-make-subreport-list-proc budget-subreport-get-minimum-expected)
    (html-make-right-cell (html-make-ital html-currency)))
   (make-report-spec
    "Status"
    (budget-line-make-report-proc budget-report-get-delta)
    (html-make-right-cell html-currency)
    + ; total-proc
    (html-make-right-cell (html-make-strong html-currency))
    (html-make-right-cell (html-make-strong html-currency))
    #t ; first-last-preference
    #f ; subentry-list-proc
    #f) ; subentry-html-proc
   ))

(define gnc:budget-status-report-sort-specs
  (list
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-account-type)
    <
    =
    =
    (lambda (acc) (symbol->string (gnc:account-type->symbol acc))))
   (make-report-sort-spec
    (budget-line-make-report-proc budget-report-get-delta)
    <
    =
    #f
    #f)))

(define (gnc:budget-renderer options)
  (let* ((begindate (gnc:lookup-option options "Report Options" "From"))
         (enddate (gnc:lookup-option options "Report Options" "To"))
         (begin-date-secs (car (gnc:timepair-canonical-day-time 
                                (gnc:date-option-absolute-time (gnc:option-value begindate)))))
         (end-date-secs (car (gnc:timepair-canonical-day-time
                              (gnc:date-option-absolute-time (gnc:option-value enddate)))))
         (budget-hash (make-hash-table 313))
         (budget-list
	   (map
	    (lambda (entry)
	      (let ((line #f))
		(set! line (make-budget-line entry (make-empty-budget-report entry)))
		(for-each
		 (lambda (account)		   
		   (make-budget-hash-entry budget-hash account line))
		 (budget-entry-get-accounts entry))
		line))
	    gnc:budget-entries))
	 (others-vec
	  (list->vector
	   (map  ;; map-in-order
	    (lambda (entry)
	      (let ((line #f))
		(set! line (make-budget-line entry (make-empty-budget-report entry)))
		(cond ((budget-entry-get-action entry) 
		       (set! budget-list 
			     (cons line budget-list))))
		line))
	    gnc:budget-others))))
    
    ;;(for-each gnc:debug budget-list)
    ;;(for-each gnc:debug (vector->list others-vec))

    (budget-calculate-actual! 
     budget-hash others-vec begin-date-secs end-date-secs)
       
    (for-each 
     (lambda (line)
       (begin
         (budget-calculate-nominal! line begin-date-secs end-date-secs)
         (budget-calculate-expected! line begin-date-secs end-date-secs)
         (budget-calculate-delta! line)))
     budget-list)
     
    (case (gnc:option-value
	   (gnc:lookup-option options "Report Options" "View"))
      
      ((full)        
       (list
	(html-start-document-title "Budget Report -- Full Debug" #f)
	(html-para "This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.")
	(html-para "This is the full debug report.  It is mainly useful for debugging the budget report.")
	(html-start-table)
	(html-table-headers gnc:budget-full-report-specs)
	(html-table-render-entries budget-list
				   gnc:budget-full-report-specs
				   gnc:budget-full-report-sort-specs
				   html-table-entry-render-entries-first
				   budget-line-count-subentries)
	(html-table-totals budget-list gnc:budget-full-report-specs)
	(html-end-table)
	(html-end-document)))
      ((balancing)
       (list
	(html-start-document-title "Budget Report -- Balancing View" #f)
	(html-para "This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.")
	(html-para "This is the balancing view.  It is supposed to be useful when you are balancing your budget.")
	(html-start-table)
	(html-table-headers gnc:budget-balance-report-specs)
	(html-table-render-entries budget-list 
				   gnc:budget-balance-report-specs
				   gnc:budget-balance-report-sort-specs
				   html-table-entry-render-subentries-merged
				   budget-line-count-subentries)
	(html-table-totals budget-list gnc:budget-balance-report-specs)
	(html-end-table)
	(html-end-document)))
      ((status)
       (list
	(html-start-document-title "Budget Report -- Balancing View" #f)
	(html-para "This is a budget report.  It is very preliminary, but you may find it useful.  To actually change the budget, currently you have to edit budget-report.scm.")
	(html-para "This is the status view.  It is supposed to tell you the current status of your budget.")
	(html-start-table)
	(html-table-headers gnc:budget-status-report-specs)
	(html-table-render-entries budget-list 
				   gnc:budget-status-report-specs
				   gnc:budget-status-report-sort-specs
				   html-table-entry-render-entries-only
				   budget-line-count-subentries)
	(html-table-totals budget-list gnc:budget-status-report-specs)
	(html-end-table)
	(html-end-document))))))


(gnc:define-report
 'version 1
 'name "Budget"
 'options-generator budget-report-options-generator
 'renderer gnc:budget-renderer)

(let ()
  (define budget-item
    (gnc:make-menu-item
     "Budget (Testing, Unfinished)"
     "Test the budget dialog"
     (list "_Tools" "")
     ;; FIXME: need update.
     (lambda ()
       (display
        (string-append
         "FIXME: Please update calls to gnc:account-type->symbol.\n"
         "FIXME: If you need a string, use gnc:account-type-string,\n"
         "FIXME: otherwise use (gw:enum-GNCAccountType-val->sym\n"
         "FIXME:                (gnc:account-get-type acct)\n"
         "FIXME:                #f)\n")))))

;     (lambda ()
;       (gnc:budget-dialog-create
;        gnc:budget-entries
;        (lambda () (display "Applied the budget.\n"))))));
  
  (gnc:hook-add-dangler gnc:*main-window-opened-hook*
                        (lambda (win) (gnc:add-extension budget-item))))
