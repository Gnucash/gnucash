;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in an account
;; Robert Merkel (rgmerk@mira.net)

(use-modules (ice-9 slib))
(require 'printf)

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

(define gnc:*transaction-report-options* '())

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

(define (gnc:register-trep-option new-option)
  (set! gnc:*transaction-report-options*
	(gnc:register-option gnc:*transaction-report-options* new-option))
  new-option)

;; from date
;; hack alert - could somebody set this to an appropriate date?
(define begindate
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
    #f)))

;; to-date
(define enddate
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report items up to and including this date"
    (lambda () (cons (current-time) 0))
    #f)))

;; account to do report on
;; hack alert - default setting doesn't work!

(define tr-report-account-op
  (gnc:register-trep-option
   (gnc:make-account-list-option
    "Report Options" "Account"
    "c" "Do transaction report on this account"
    (lambda () (list (gnc:group-get-account (gnc:get-current-group) 0)))
    #f #f)))

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

(define (gnc:timepair-earlier-or-eq t1 t2)
  (let ((time1 (car t1)) 
        (time2 (car t2)))
    (<= time1 time2)))

(define (gnc:timepair-later t1 t2)
  (let ((time1 (car t1)) 
        (time2 (car t2)))
    (< time1 time2)))

(define (gnc:timepair-later-or-eq t1 t2)
  (gnc:timepair-earlier-or-eq t2 t1))

;; returns a predicate that returns true only if a split-scm is
;; between early-date and late-date

(define (gnc:tr-report-make-filter-predicate early-date late-date)
  (lambda (split-scm)
    (let ((split-date (gnc:tr-report-get-date split-scm)))
      (and (gnc:timepair-later-or-eq split-date early-date)
           (gnc:timepair-earlier-or-eq split-date late-date)))))

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
	  (gnc:timepair-later
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
 gnc:*transaction-report-options*
 ;; renderer
 (lambda (options)
   (let* ((prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>" "<TABLE>"
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
			     (op-value begindate) 
			     (op-value enddate)))
	  (starting-balance 0)
          (accounts (op-value tr-report-account-op)))
     gnc:tr-report-initialize-inflow-and-outflow!
     (if (null? accounts)
         (set! report-lines
               (list "<TR><TD>You have not selected an account.</TD></TR>"))
	 (begin
	   
           (gnc:for-each-split-in-account
            (car accounts)
            (lambda (split)		
              (set! report-lines 
                    (append! report-lines 
                             (list (gnc:make-split-scheme-data split))))))
           (set! starting-balance
                 (gnc:tr-report-get-starting-balance
                  report-lines (op-value begindate)))
	   
           (set! report-lines (gnc:filter-list report-lines date-filter-pred))
	   (let ((html-mapper (lambda (split-scm) (gnc:tr-report-split-to-html
						  split-scm
						  starting-balance))))
	     (set! report-lines (gnc:inorder-map report-lines html-mapper)))
	   (set! 
	    balance-line 
	    (list "<TR><TD><STRONG>Balance at: "
		  (gnc:timepair-to-datestring (op-value begindate))
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
		  "</TD></STRONG></TR>"))
	   (append prefix balance-line report-lines inflow-outflow-line net-inflow-line suffix))))))
