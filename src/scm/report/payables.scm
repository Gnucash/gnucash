;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; payables.scm : accounts payable report
;;  
;; By Robert Merkel (rgmerk@mira.net) 
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; depends must be outside module scope
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")


(define-module (gnucash report payables))

(use-modules (ice-9 slib))

(require 'hash-table)
(require 'record)

(define opt-pay-acc (N_ "Payables Account"))
(define sect-acc (N_ "Accounts"))
(define optname-to-date (N_ "To"))
(define optname-use-description (N_ "Use Description?"))
(define optname-sort-by (N_ "Sort By"))
(define optname-sort-order (N_ "Sort Order"))

;; The idea is:  have a hash with the key being the creditor name
;; (In future this might be GUID'ed, but for now it's a string
;; from the description or the split memo.
;; The value is a record which contains the currency that creditor
;; is stored in (you can only owe a particular creditor one
;; currency, it just gets far too difficult otherwise), and a list
;; of buckets containing the money owed for each interval,
;; oldest first.
;; overpayment is just that - it stores the current overpayment, 
;; if any.  Any bills get taken out of the overpayment before
;; incurring debt.

(define creditor-info (make-record-type "CreditorInfo" 
				       '(currency
					 bucket-vector
					 overpayment)))

(define num-buckets 3)
(define (new-bucket-vector)
  (make-vector num-buckets (gnc:numeric-zero)))

(define make-creditor-private
  (record-constructor creditor-info '(currency bucket-vector 
					       overpayment)))

(define (make-creditor currency)
  (make-creditor-private currency (new-bucket-vector) 
			 (gnc:numeric-zero)))

(define creditor-get-currency
  (record-accessor creditor-info 'currency))

(define creditor-get-buckets
  (record-accessor creditor-info 'bucket-vector))

(define creditor-set-buckets
  (record-modifier creditor-info 'bucket-vector))

(define creditor-get-overpayment
  (record-accessor creditor-info 'overpayment))

(define creditor-set-overpayment
  (record-modifier creditor-info 'overpayment))

;; Put a bill in the appropriate bucket

(define (process-bill creditor amount bucket-intervals date)
  (define (in-interval this-date current-bucket)
    (gnc:timepair-lt this-date current-bucket))

  (define (find-bucket current-bucket bucket-intervals date)  
    (begin
      (gnc:debug "current bucket" current-bucket)
      (gnc:debug "bucket-intervals" bucket-intervals)
      (gnc:debug "date" date)
      (if (> current-bucket (vector-length bucket-intervals))
	  (gnc:error "sanity check failed in find-bucket")
	  (if (in-interval date (vector-ref bucket-intervals current-bucket))
	      current-bucket
	      (find-bucket (+ current-bucket 1) bucket-intervals date)))))

  (define (calculate-adjusted-values amount overpayment)
    (if (>= (gnc:numeric-compare amount overpayment) 0)
	(cons (gnc:numeric-sub-fixed amount overpayment)
	      (gnc:numeric-zero))
	(cons (gnc:numeric-zero)
	      (gnc:numeric-sub-fixed overpayment amount))))

  (let* ((current-overpayment (creditor-get-overpayment creditor))
	 (adjusted-values (calculate-adjusted-values amount current-overpayment))
	 (adjusted-amount (car adjusted-values))
	 (adjusted-overpayment (cdr adjusted-values))
	 (bucket-index (find-bucket 0 bucket-intervals date))
	 (buckets (creditor-get-buckets creditor))
	 (new-bucket-value 
	  (gnc:numeric-add-fixed adjusted-amount (vector-ref buckets bucket-index))))
    (vector-set! buckets bucket-index new-bucket-value)
    (creditor-set-buckets creditor buckets)
    (creditor-set-overpayment creditor adjusted-overpayment)))


;; NOTE: We assume that bill payments occur in a FIFO manner - ie
;; any payment to a creditor goes towards the *oldest* bill first


(define (process-payment creditor amount)
  (define (process-payment-driver amount buckets current-bucket-index)
    (if (>= current-bucket-index (vector-length buckets))
	amount
	(let ((current-bucket-amt (vector-ref buckets current-bucket-index)))
	  (if (>= (gnc:numeric-compare current-bucket-amt amount) 0)
	      (begin
		(vector-set! buckets current-bucket-index (gnc:numeric-sub-fixed 
							   current-bucket-amt amount))
		(gnc:numeric-zero))
	      (begin
		(vector-set! buckets current-bucket-index (gnc:numeric-zero))
		(process-payment-driver 
		 (gnc:numeric-sub-fixed amount current-bucket-amt)
		 buckets
		 (+ current-bucket-index 1)))))))
  
  (let ((overpayment (creditor-get-overpayment creditor)))
	;; if there's already an overpayment, make it bigger
	(if (gnc:numeric-positive-p overpayment)
	    (creditor-set-overpayment creditor (gnc:numeric-add-fixed overpayment amount))
	    
	    (let ((result (process-payment-driver amount (creditor-get-buckets creditor) 0)))
	      (creditor-set-overpayment creditor result)))))
		  
		    
    
  
;; deal with a transaction - figure out if we've seen the creditor before
;; if so, either process it as a bill or a payment, if not, create
;; a new creditor record in the hash

(define (update-creditor-hash hash split bucket-intervals use-description?)
  (let* ((transaction (gnc:split-get-parent split))
	 (creditor-name (if use-description?
			  (gnc:transaction-get-description transaction) 
			  (gnc:split-get-memo split)))
	 (this-currency (gnc:transaction-get-currency transaction))
	 (value (gnc:split-get-value split))
	 (this-date (gnc:transaction-get-date-posted transaction))
	 (creditor-info (hash-ref hash creditor-name)))

    (gnc:debug "update-creditor-hash called")
    (gnc:debug "creditor-name" creditor-name)
    (gnc:debug "split-value" value)
    (if creditor-info
	;; if it's an existing creditor, first check currencies match
	(if (not (gnc:commodity-equiv? this-currency
				       (creditor-get-currency creditor-info)))
	    (cons #f (sprintf (_ "Transactions relating to creditor  %d contain \
more than one currency.  This report is not designed to cope with this possibility.")))
	    (begin
	      (gnc:debug "it's an old creditor")
	      (if (gnc:numeric-negative-p value)
		  (process-bill creditor-info (gnc:numeric-neg value) bucket-intervals this-date)
		  (process-payment creditor-info value))
	      (hash-set! hash creditor-name creditor-info)
	      (cons #t creditor-name)))
	
	;; if it's a new creditor
	(begin
	  (gnc:debug "value" value)
	  (if (gnc:numeric-negative-p value) ;; if it's a new debt
	      ;; if not ignore it
	                                     ;;; XXX: is this right ?
	      (let ((new-creditor (make-creditor this-currency)))
		(process-bill new-creditor (gnc:numeric-neg value) bucket-intervals this-date)
		(hash-set! hash creditor-name new-creditor)))
	  (cons #t creditor-name)))))

(define (buckets-get-total buckets)
  (let ((running-total (gnc:numeric-zero))
	(buckets-list (vector->list buckets)))
    (for-each (lambda (bucket)
		(set! running-total
		      (gnc:numeric-add-fixed bucket running-total)))
	      buckets-list)
    running-total))

(define (compare-total litem-a litem-b)    
  (let*  ((creditor-a (cdr litem-a))
	 (bucket-a (creditor-get-buckets creditor-a))
	 (creditor-b (cdr litem-b))
	 (bucket-b (creditor-get-buckets creditor-b))
	 (total-a (buckets-get-total bucket-a))
	 (total-b (buckets-get-total bucket-b))
	 (difference-sign (gnc:numeric-compare (gnc:numeric-sub-fixed total-a total-b) (gnc:numeric-zero))))
	 ;; if same totals, compare by name
	 (if (= difference-sign 0)
	     (gnc:safe-strcmp (car litem-a) (car litem-b))
	     difference-sign)))

(define (compare-buckets litem-a litem-b)
  (define (driver buckets-a buckets-b)
    (if (null? buckets-a)
	0
	(let ((diff (gnc:numeric-compare 
		     (gnc:numeric-sub-fixed 
		      (car buckets-a) 
		      (car buckets-b)) 
		     (gnc:numeric-zero))))
	  (if (= diff 0)
	      (driver (cdr buckets-a) (cdr buckets-b))
	      diff))))
 
  (let*  ((creditor-a (cdr litem-a))
	 (bucket-a (vector->list (creditor-get-buckets creditor-a)))
	 (creditor-b (cdr litem-b))
	 (bucket-b (vector->list (creditor-get-buckets creditor-b)))
	 
	 (difference (driver bucket-a bucket-b)))
	 ;; if same totals, compare by name
	 (if (= difference 0)
	     (gnc:safe-strcmp (car litem-a) (car litem-b))
	     difference)))
  
    
;; set up the query to get the splits in the payables account

(define (setup-query query account date)
  (define (date-copy date)
    (cons (car date) (cdr date)))
  (let ((begindate (date-copy date)))
;    (gnc:debug "Account: " account)
    (set! begindate (decdate begindate NinetyDayDelta))
    (gnc:debug "begindate" begindate)
    (gnc:debug "date" date)
    (gnc:query-set-group query (gnc:get-current-group))
    (gnc:query-add-single-account-match query account 'query-and)
    (gnc:query-add-date-match-timepair query #t begindate #t date 'query-and)
    (gnc:query-set-sort-order query 'by-date 'by-none 'by-none)
    (gnc:query-set-sort-increasing query #t #t #t)))
     

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (gnc:options-add-report-date!
     options gnc:pagename-general
     optname-to-date "a")

    (add-option
     (gnc:make-account-list-option
      sect-acc opt-pay-acc
      "a" (N_ "Account where payables are stored.")
      ;; FIXME: Have a global preference for the payables account??
      ;; default-getter
      (lambda ()
	(define (find-first-liability current-group num-accounts this-account-ind)
	  (if
	   (>= this-account-ind num-accounts) 
	   #f
	   (let* 
	       ((this-account 
		 (gnc:group-get-account current-group this-account-ind))
		(account-type (gw:enum-<gnc:AccountType>-val->sym
			       (gnc:account-get-type this-account) #f)))
	     (begin 
	       (gnc:debug "this-account" this-account)
	       (gnc:debug "account-type" account-type)
	       (if (eq? account-type 'liability)
		   (begin 
		     (gnc:debug "this-account selected" this-account)
		      this-account)
		   (find-first-liability 
		    current-group num-accounts (+ this-account-ind 1)))))))

	(let* ((current-group (gnc:get-current-group))
	      (num-accounts (gnc:group-get-num-accounts
			     current-group)))
	  (if (> num-accounts 0)
	      (let ((first-liability (find-first-liability
				      current-group
				      num-accounts
				      0)))
		(gnc:debug "first-liability" first-liability)
		(if first-liability
		    (list first-liability)
		    (list (gnc:group-get-account current-group 0))))
	      '())))
     ;; value-validator
     (lambda (account-list)
	(let ((first-account) (car account-list))
	  (gnc:debug "account-list" account-list)
	  (if first-account
	    (let ((account-type (gw:enum-<gnc:AccountType>-val->sym 
				 (gnc:account-get-type first-account))))
	      (if (eq? 'liability account-type) 
	  
		  (cons #t  (list first-account))
		  (cons #f  (_ "The payables account must be a liability account"))))
	    ;; FIXME: until we can select a default account I need 
	    ;; to catch this at the report-writing stage
	    (#t '()))))
      #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general
      optname-sort-by
      "i"
      (N_ "Sort creditors by")
      'name
      (list 
       (vector 'name (N_ "Name") (N_ "Name of the creditor"))
       (vector 'total (N_ "Total Owed") (N_ "Total amount owed to Creditor"))
       (vector 'oldest-bracket (N_ "Bracket Total Owed") (N_ "Amount owed in oldest bracket - if same go to next oldest")))))

    (add-option 
     (gnc:make-multichoice-option
      gnc:pagename-general
       optname-sort-order
       "ia"
       (N_ "Sort order")
       'increasing
       (list
	(vector 'increasing (N_ "Increasing") (N_ "0 -> $999,999.99, A->Z"))
	(vector 'decreasing (N_ "Decreasing") (N_ "$999,999.99 -> $0, Z->A")))))

       (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general
      optname-use-description
      "h"
      (N_ "Use the description to identify individual creditors.\
  If false, use split memo")
      #t))

     
    (gnc:options-set-default-section options "General")      
    options))

(define (make-interval-list to-date)
  (let ((begindate to-date))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))

(define (payables-renderer report-obj)

  ;; Format: (cons 'sort-key (cons 'increasing-pred 'decreasing-pred))
  (define sort-preds
    (list 
     (cons 'name (cons (lambda (a b)
			 (string<? (car a) (car b)))
		       (lambda (a b)
			 (string>? (car a) (car b)))))
     (cons 'total (cons (lambda (a b)
			  (< (compare-total a b) 0))
			(lambda (a b)
			  (> (compare-total a b) 0))))
     (cons 'oldest-bracket (cons 
			    (lambda (a b) 
			     (< (compare-buckets a b) 0))
			    (lambda (a b)
			      (> (compare-buckets a b) 0))))))
				 
				 

  (define (get-sort-pred sort-criterion sort-order)
    (let ((choice (assq-ref sort-preds sort-criterion)))
      (gnc:debug "sort-criterion" sort-criterion)
      (gnc:debug "sort-order" sort-order)
      (gnc:debug "choice: " choice)
      (if choice
	  (if (eq? sort-order 'increasing)
	      (car choice)
	      (cdr choice))
	  (begin
	    (gnc:warn "internal sorting option errorin payables.scm")
	    (lambda (a b)
	      (string<? (car a) (car b)))))))
		       
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))
  
  (define (print-interval date-int)
    (gnc:make-html-text
     "("
     (gnc:timepair-to-datestring date-int)
     ")"))
				 
  (define (make-heading-list)
    (list 
     (N_ "Creditor Name")
     (N_ "0-30 days")
     (N_ "31-60 days")
     (N_ "61-90 days")
     (N_ "Total")))


  (define (convert-to-monetary-list bucket-list currency)
    (let* ((running-total (gnc:numeric-zero))
	  (monetised-buckets
	   (map (lambda (bucket-list-entry)
		  (begin
		    (set! running-total 
			  (gnc:numeric-add-fixed running-total bucket-list-entry))
		  (gnc:make-gnc-monetary currency bucket-list-entry)))
		(vector->list bucket-list))))
      (append (reverse monetised-buckets) (list (gnc:make-gnc-monetary currency running-total)))))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let* ((payables-account (car (op-value sect-acc opt-pay-acc)))
	 (creditors (make-hash-table 23))

	(report-title (op-value  gnc:pagename-general 
				 gnc:optname-reportname))
        ;; document will be the HTML document that we return.
	(report-date (gnc:timepair-end-day-time 
		      (gnc:date-option-absolute-time
		       (op-value gnc:pagename-general (N_ "To")))))
	(use-description? (op-value gnc:pagename-general optname-use-description))
	(interval-vec (list->vector (make-interval-list report-date)))
	(sort-pred (get-sort-pred (op-value gnc:pagename-general optname-sort-by)
				   (op-value gnc:pagename-general optname-sort-order)))
	(heading-list (make-heading-list))
	(table (gnc:make-html-table))
	(query (gnc:malloc-query))
	(creditor-list '())
        (document (gnc:make-html-document)))
    (gnc:debug "Payables-account: " payables-account)
    (gnc:html-document-set-title! document report-title)
    (gnc:html-table-set-col-headers! table heading-list)
				     
    (if payables-account
	(begin
	  (setup-query query payables-account report-date)
	  (let ((splits (gnc:glist->list (gnc:query-get-splits query) <gnc:Split*>)))
	    (gnc:debug "splits" splits)
	    (for-each (lambda (split)
			(update-creditor-hash creditors 
					      split 
					      interval-vec 
					      use-description?))
			splits)
	    (gnc:debug "creditors" creditors)
	    (hash-for-each (lambda (key value)
			     (set! creditor-list
				   (cons (cons key value) creditor-list)))
			   creditors)
	    (gnc:debug "creditor list" creditor-list)
	   
	    (set! creditor-list (sort-list! creditor-list
					    sort-pred))

	    (for-each (lambda (creditor-list-entry)
			(let ((monetary-list (convert-to-monetary-list
					      (creditor-get-buckets
					       (cdr creditor-list-entry))
					      (creditor-get-currency
					       (cdr creditor-list-entry)))))
			  (gnc:html-table-append-row!
			   table (cons (car creditor-list-entry) monetary-list))))
		      creditor-list)
	    (gnc:html-document-add-object!
	     document table)))
	(gnc:html-document-add-object!
	 document
	 (gnc:make-html-text
	  "No Valid Account Selected")))
    (gnc:free-query query)
    document))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Accounts Payable")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "Amount owing, grouped by debtors and age.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-asset-liability)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer payables-renderer)
