;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in an account
;; Robert Merkel (rgmerk@mira.net)

(use-modules (ice-9 slib))
(require 'printf)
(require 'sort)

;hack alert - is this line necessary?
(gnc:depend "text-export.scm")

;; hack alert - possibly unecessary globals


;; functions for manipulating total inflow and outflow counts.

(define gnc:total-inflow 0)
(define gnc:total-outflow 0)


(define (gnc:set-total-inflow! x)
    (set! gnc:total-inflow x))

(define (gnc:set-total-outflow! x)
    (set! gnc:total-outflow x))

(define gnc:tr-report-initialize-inflow-and-outflow!
  (begin
    (set! gnc:total-inflow 0)
    (set! gnc:total-outflow 0)
    #f))

;;returns a list contains elements of the-list for which predictate is
;; true
(define (gnc:filter-list the-list predicate)
  (cond ((not (list? the-list))
         (gnc:error("Attempted to filter a non-list object")))
        ((null? the-list) '())
        ((predicate (car the-list))
         (cons (car the-list)
               (gnc:filter-list (cdr the-list) predicate)))
        (else (gnc:filter-list (cdr the-list) predicate))))

;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.

(define (gnc:inorder-map the-list fn)
  (cond ((not (list? the-list))
	 (gnc:error("Attempted to map a non-list object")))
	((not (procedure? fn))
	 (gnc:error("Attempted to map a non-function object to a list")))
	((eq? the-list '()) '())
	(else (cons (fn (car the-list))
		    (gnc:inorder-map (cdr the-list) fn)))))

;; register a configuration option for the transaction report
(define (trep-options-generator)

  (define gnc:*transaction-report-options* (gnc:new-options))

  (define (gnc:register-trep-option new-option)
    (gnc:register-option gnc:*transaction-report-options* new-option))

  ;; from date
  ;; hack alert - could somebody set this to an appropriate date?
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "From"
    "a" "Report Items from this date" 
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
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report items up to and including this date"
    (lambda () (cons (current-time) 0))
    #f))

  ;; account to do report on
  (gnc:register-trep-option
   (gnc:make-account-list-option
    "Report Options" "Account"
    "c" "Do transaction report on this account"
    (lambda ()
      (let ((current-accounts (gnc:get-current-accounts))
            (num-accounts (gnc:group-get-num-accounts (gnc:get-current-group)))
            (first-account (gnc:group-get-account (gnc:get-current-group) 0)))
        (cond ((not (null? current-accounts)) (list (car current-accounts)))
              ((> num-accounts 0) (list first-account))
              (else ()))))
    #f #f))

  ;; primary sorting criterion
  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Primary Key"
     "a" "Sort by this criterion first"
     'date
     (list #(date
	     "Date"
	     "Sort by date")
	   #(time
	     "Time"
	     "Sort by EXACT entry time")
	   #(corresponding-acc
	     "Transfer from/to"
	     "Sort by account transferred from/to's name")
	   #(amount
	     "Amount"
	     "Sort by amount")
	   #(description
	     "Description"
	     "Sort by description")
	   #(number
	     "Number"
	     "Sort by check/transaction number")
	   #(memo
	     "Memo"
	     "Sort by memo"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Primary Sort Order"
    "b" "Order of primary sorting"
    'ascend
    (list #(ascend "Ascending" "smallest to largest, earliest to latest")
	  #(descend "Descending" "largest to smallest, latest to earliest"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Secondary Key"
     "c"
     "Sort by this criterion second"
     'corresponding-acc
     (list #(date
	     "Date"
	     "Sort by date")
	   #(time
	     "Time"
	     "Sort by EXACT entry time")
	   #(corresponding-acc
	     "Transfer from/to"
	     "Sort by account transferred from/to's name")
	   #(amount
	     "Amount"
	     "Sort by amount")
	   #(description
	     "Description"
	     "Sort by description")
	   #(number
	     "Number"
	     "Sort by check/transaction number")
	   #(memo
	     "Memo"
	     "Sort by memo"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Secondary Sort Order"
    "d" "Order of Secondary sorting"
    'ascend
    (list #(ascend "Ascending" "smallest to largest, earliest to latest")
	  #(descend "Descending" "largest to smallest, latest to earliest"))))

  gnc:*transaction-report-options*)


;; extract fields out of the scheme split representation

(define (gnc:tr-report-get-memo split-scm)
  (vector-ref split-scm 0))

(define (gnc:tr-report-get-action split-scm)
  (vector-ref split-scm 1))

(define (gnc:tr-report-get-description split-scm)
  (vector-ref split-scm 2))

(define (gnc:tr-report-get-date split-scm)
  (vector-ref split-scm 3))

(define (gnc:tr-report-get-reconcile-state split-scm)
  (vector-ref split-scm 4))

(define (gnc:tr-report-get-reconcile-date split-scm)
  (vector-ref split-scm 5))

(define (gnc:tr-report-get-share-amount split-scm)
  (vector-ref split-scm 6))

(define (gnc:tr-report-get-share-price split-scm)
  (vector-ref split-scm 7))

(define (gnc:tr-report-get-value split-scm)
  (vector-ref split-scm 8))

(define (gnc:tr-report-get-num split-scm)
  (vector-ref split-scm 9))

(define (gnc:tr-report-get-other-splits split-scm)
  (vector-ref split-scm 10))



(define (gnc:tr-report-get-first-acc-name split-scm)
  (let ((other-splits (gnc:tr-report-get-other-splits split-scm)))
    (cond ((= (length other-splits) 0) "")
	  (else  (caar other-splits)))))

;;; something like 
;;; for(i = first; i < last; i+= step) { thunk(i);}

(define (gnc:for-loop thunk first last step)
  (cond ((< first last) (thunk first) 
	 (gnc:for-loop thunk (+ first step) last step))
	(else #f)))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

;; get transactions date from split - needs to be done indirectly
;; as it's stored in the parent transaction

(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

;; ditto descriptions
(define (gnc:split-get-description-from-parent split)
  (gnc:transaction-get-description (gnc:split-get-parent split)))

;; get the account name of a split
(define (gnc:split-get-account-name split)  
  (gnc:account-get-name (gnc:split-get-account split)))

;; builds a list of the account name and values for the other
;; splits in a transaction

(define (gnc:split-get-corresponding-account-name-and-values split) 
  (let* ((my-sign (positive? (gnc:split-get-value split)))
         (diff-list '())
         (parent-transaction (gnc:split-get-parent split))
         (num-splits (gnc:transaction-get-split-count parent-transaction)))
    (cond 
     ((= num-splits 1) '())
     (else
	  
	  
      (gnc:for-loop 
       (lambda (n) 
	 (let ((split-in-trans 
		(gnc:transaction-get-split parent-transaction n)))
	   (if (not (eq? my-sign 
			 (positive? (gnc:split-get-value split-in-trans))))
	       (set! diff-list
		     (cons
		      (list
		       (gnc:split-get-account-name split-in-trans)
		       (gnc:split-get-value split-in-trans))
		      diff-list)))))
       0 num-splits 1)
      (reverse diff-list)))))


;; takes a C split, extracts relevant data and converts to a scheme 
;; representation

(define (gnc:make-split-scheme-data split)
  (vector (gnc:split-get-memo split) 
	  (gnc:split-get-action split)
	  (gnc:split-get-description-from-parent split)
	  (gnc:split-get-transaction-date split)
	  (gnc:split-get-reconcile-state split)
	  (gnc:split-get-reconciled-date split)
	  (gnc:split-get-share-amount split)
	  (gnc:split-get-share-price split)
	  (gnc:split-get-value split)
	  (gnc:transaction-get-num (gnc:split-get-parent split))
	  (gnc:split-get-corresponding-account-name-and-values split)))

;; timepair manipulation functions
;; hack alert  - these should probably be put somewhere else
;; and be implemented PROPERLY rather than hackily

(define (gnc:timepair-to-datestring tp)
  (let ((bdtime (localtime (car tp))))
    (strftime "%x" bdtime)))

;; given a timepair contains any time on a certain day (local time)
;; converts it to be midday that day.

(define (gnc:timepair-canonical-day-time tp)
  (let ((bdt (localtime (car tp))))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 12)
    (let ((newtime (car (mktime bdt))))
      (cons newtime (* 1000 newtime)))))

(define (gnc:timepair-earlier-or-eq-date t1 t2)
  (let ((time1 (car (gnc:timepair-canonical-day-time t1)))
        (time2 (car (gnc:timepair-canonical-day-time t2))))
    (<= time1 time2)))

(define (gnc:timepair-later-date t1 t2)
  (let ((time1 (car (gnc:timepair-canonical-day-time t1))) 
        (time2 (car (gnc:timepair-canonical-day-time t2))))
    (< time1 time2)))

(define (gnc:timepair-later-or-eq-date t1 t2)
  (gnc:timepair-earlier-or-eq-date t2 t1))

(define (gnc:sort-predicate-component component order)
  (let ((ascending-order-comparator
	(begin 
;	  (display (symbol->string component))
	(cond
	 ((eq? component 'date) 
	  (lambda (split-scm-a split-scm-b)
	    (- 
	     (car 
	      (gnc:timepair-canonical-day-time 
	       (gnc:tr-report-get-date split-scm-a)))
	     (car
	      (gnc:timepair-canonical-day-time
	       (gnc:tr-report-get-date split-scm-b))))))

	 ((eq? component 'time) 
	  (lambda (split-scm-a split-scm-b)
	    (-
	     (car (gnc:tr-report-get-date split-scm-a))
	     (car (gnc:tr-report-get-date split-scm-b)))))

	 ((eq? component 'amount) 
	  (lambda (split-scm-a split-scm-b)
	    (-
	     (gnc:tr-report-get-value split-scm-a)
	     (gnc:tr-report-get-value split-scm-b))))

	 ((eq? component 'description)
	  (lambda (split-scm-a split-scm-b)
	    (let ((description-a (gnc:tr-report-get-description split-scm-a))
		  (description-b (gnc:tr-report-get-description split-scm-b)))
	      (cond ((string<? description-a description-b) -1)
		    ((string=? description-a description-b) 0)
		    (else 1)))))
	 
	 ;; hack alert - should probably use something more sophisticated
	 ;; here - perhaps even making it user-definable
	 ((eq? component 'number)
	  (lambda (split-scm-a split-scm-b)
            (let ((num-a (gnc:tr-report-get-num split-scm-a))
                  (num-b (gnc:tr-report-get-num split-scm-b)))
              (cond ((string<? num-a num-b) -1)
                    ((string=? num-a num-b) 0)
                    (else 1)))))

	 ((eq? component 'corresponding-acc)
	  (lambda (split-scm-a split-scm-b)
	   (let ((corr-acc-a (gnc:tr-report-get-first-acc-name split-scm-a))
		 (corr-acc-b (gnc:tr-report-get-first-acc-name split-scm-b)))
	     (cond ((string<? corr-acc-a corr-acc-b) -1)
		   ((string=? corr-acc-a corr-acc-b) 0)
		   (else 1)))))

	 ((eq? component 'memo)
	   (lambda (split-scm-a split-scm-b)
	   (let ((memo-a (gnc:tr-report-get-memo split-scm-a))
		 (memo-b (gnc:tr-report-get-memo split-scm-b)))
	     (cond ((string<? memo-a memo-b) -1)
		   ((string=? memo-a memo-b) 0)
		   (else 1)))))
	 (else (gnc:error (sprintf "illegal sorting option %s- bug in transaction-report.scm" (symbol->string (component)) )))))))
	 (cond ((eq? order 'descend) 
		 (lambda (my-split-a my-split-b)
		   (- (ascending-order-comparator my-split-a my-split-b))))
		(else ascending-order-comparator))))


;; returns a predicate
(define (gnc:tr-report-make-sort-predicate primary-key-op primary-order-op
                                           secondary-key-op secondary-order-op)
  (let ((primary-comp (gnc:sort-predicate-component
		       (gnc:option-value primary-key-op)
		       (gnc:option-value primary-order-op)))
	  (secondary-comp (gnc:sort-predicate-component
			   (gnc:option-value secondary-key-op)
			   (gnc:option-value secondary-order-op))))
    (lambda (split-a split-b)  
      (let ((primary-comp-value (primary-comp split-a split-b)))
	(cond ((< primary-comp-value 0) #t)
	      ((> primary-comp-value 0) #f)
	      (else 
	       (let ((secondary-comp-value (secondary-comp split-a split-b)))
		 (cond ((< secondary-comp-value 0) #t)
		       (else #f)))))))))

;; returns a predicate that returns true only if a split-scm is
;; between early-date and late-date

(define (gnc:tr-report-make-filter-predicate early-date late-date)
  (lambda (split-scm)
    (let ((split-date (gnc:tr-report-get-date split-scm)))
      (and (gnc:timepair-later-or-eq-date split-date early-date)
           (gnc:timepair-earlier-or-eq-date split-date late-date)))))

;; converts a scheme split representation to a line of HTML,
;; updates the values of total-inflow and total-outflow based
;; on the split value
;; hack alert - no i8n on amount printing yet - must fix!

(define (gnc:tr-report-split-to-html split-scm 
                                     starting-balance)
  (let ((other-splits (gnc:tr-report-get-other-splits split-scm)))
    (string-append 
     "<TR><TD>" 
     (gnc:timepair-to-datestring
      (gnc:tr-report-get-date split-scm))
     "</TD><TD>"
     (gnc:tr-report-get-num split-scm)
     "</TD><TD>"
     (gnc:tr-report-get-description split-scm)
     "</TD><TD>"
     (gnc:tr-report-get-memo split-scm)
     "</TD><TD>"
   (cond ((null? other-splits) "")
	 ((= (length other-splits) 1) 
	  (cond ((eqv? (caar other-splits) #f)
		 "-")
		(else (caar other-splits))))
         (else "Multi-split (not implemented yet)"))
     "</TD><TD>"
    (cond ((> (gnc:tr-report-get-value split-scm) 0)
           (begin
	   (gnc:set-total-inflow! (+ gnc:total-inflow
                                  (gnc:tr-report-get-value split-scm)))
	    (string-append 
	     (sprintf #f "%.2f" (gnc:tr-report-get-value split-scm))
	     "</TD><TD>")))
    
           (else 
	    (begin
            (gnc:set-total-outflow! (+ gnc:total-outflow
                                   (- (gnc:tr-report-get-value split-scm))))
            (string-append 
	     "</TD><TD>"
             (sprintf #f "%.2f" 
                      (- (gnc:tr-report-get-value split-scm)))))))
	   "</TD><TD>"

    (sprintf #f "%.2f" (- (+ starting-balance gnc:total-inflow) 
			  gnc:total-outflow))

     "</TD></TR>")))

;; gets the balance for a list of splits before beginning-date
;; hack alert -
;; we are doing multiple passes over the list - if it becomes a performance
;; problem some code optimisation will become necessary

(define (gnc:tr-report-get-starting-balance scm-split-list beginning-date)
  (cond ((or 
	  (eq? scm-split-list '())
	  (gnc:timepair-later-date
	   (gnc:tr-report-get-date (car scm-split-list))
	   beginning-date))
	 0)
	(+ 
	 (gnc:tr-report-get-value 
	  (car scm-split-list))
	 (gnc:tr-report-get-starting-balance
	  (cdr scm-split-list) beginning-date))))


(gnc:define-report
 ;; version
 1
 ;; Name
 "Account Transactions"
 ;; Options
 trep-options-generator
 ;; renderer
 (lambda (options)
   (let* ((begindate (gnc:lookup-option options "Report Options" "From"))
          (enddate (gnc:lookup-option options "Report Options" "To"))
          (tr-report-account-op (gnc:lookup-option options
                                                   "Report Options" "Account"))
          (tr-report-primary-key-op (gnc:lookup-option options
                                                       "Sorting"
                                                       "Primary Key"))
          (tr-report-primary-order-op (gnc:lookup-option options
                                                         "Sorting"
                                                         "Primary Sort Order"))
          (tr-report-secondary-key-op (gnc:lookup-option options
                                                         "Sorting"
                                                         "Secondary Key"))
          (tr-report-secondary-order-op
           (gnc:lookup-option options "Sorting" "Secondary Sort Order"))
          (prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>" "<TABLE>"
	                 "<TH>Date</TH>"
                         "<TH>Num</TH>"
                         "<TH>Description</TH>"
                         "<TH>Memo</TH>"
                         "<TH>Category</TH>"
                         "<TH>Credit</TH>"
                         "<TH>Debit</TH>"
			 "<TH>Balance<TH>"))
	  (suffix  (list "</TABLE>" "</BODY>" "</HTML>"))
	  (balance-line '())
	  (inflow-outflow-line '())
	  (net-inflow-line '())
	  (report-lines '())
	  (date-filter-pred (gnc:tr-report-make-filter-predicate
			     (gnc:option-value begindate) 
			     (gnc:option-value enddate)))
	  (starting-balance 0)
          (accounts (gnc:option-value tr-report-account-op)))
     gnc:tr-report-initialize-inflow-and-outflow!
     (if (null? accounts)
         (set! report-lines
               (list "<TR><TD>There are no accounts to report on.</TD></TR>"))
	 (begin
	   
           (gnc:for-each-split-in-account
            (car accounts)
            (lambda (split)		
              (set! report-lines 
                    (append! report-lines 
                             (list (gnc:make-split-scheme-data split))))))
           (set! starting-balance
                 (gnc:tr-report-get-starting-balance
                  report-lines (gnc:option-value begindate)))
	   
           (set! report-lines (gnc:filter-list report-lines date-filter-pred))
	   (set! report-lines
                 (sort!
                  report-lines 
                  (gnc:tr-report-make-sort-predicate
                   tr-report-primary-key-op tr-report-primary-order-op
                   tr-report-secondary-key-op tr-report-secondary-order-op)))
	   (let ((html-mapper (lambda (split-scm) (gnc:tr-report-split-to-html
						  split-scm
						  starting-balance))))
	     (set! report-lines (gnc:inorder-map report-lines html-mapper)))
	   (set!
	    balance-line 
	    (list "<TR><TD><STRONG>Balance at: "
		  (gnc:timepair-to-datestring (gnc:option-value begindate))
		  "</STRONG></TD>"
		  "<TD></TD>" 
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD><STRONG>"
		  (sprintf #f "%.2f" starting-balance)
		  "</STRONG></TD></TR>"))
	   (set!
	    inflow-outflow-line
	    (list "<TR><TD><STRONG>Totals:</STRONG></TD>"
		  "<TD></TD>" 
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD></TD>"
		  "<TD><STRONG>"
		  (sprintf #f "%.2f" gnc:total-inflow)
		  "</TD></STRONG>"
		  "<TD><STRONG>"
		  (sprintf #f "%.2f" gnc:total-outflow)
		  "</TD></STRONG>"
		  "<TD></TD></TR>"))
	   (set!
	    net-inflow-line
	    (list "<TR><TD><STRONG>Net Inflow</STRONG></TD>"
		  "<TD></TD>"
		  "<TD></TD>" 
		  "<TD></TD>" 
		  "<TD></TD>"
		  "<TD></TD>" 
		  "<TD></TD>"
		  "<TD><STRONG>"
		  (sprintf #f "%.2f" (- gnc:total-inflow gnc:total-outflow))
		  "</TD></STRONG></TR>"))))
     (append prefix balance-line report-lines
             inflow-outflow-line net-inflow-line suffix))))
