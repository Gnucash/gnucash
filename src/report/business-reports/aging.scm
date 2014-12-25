;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aging.scm : accounts payable/receivable aging report utilities
;;  
;; By Derek Atkins <warlord@MIT.EDU> taken from the original...
;; By Robert Merkel (rgmerk@mira.net) 
;; Copyright (c) 2002, 2003 Derek Atkins <warlord@MIT.EDU>
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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report aging))

(use-modules (gnucash main))
(use-modules (gnucash printf))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(define optname-to-date (N_ "To"))
(define optname-sort-by (N_ "Sort By"))
(define optname-sort-order (N_ "Sort Order"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-multicurrency-totals (N_ "Show Multi-currency Totals"))
(define optname-show-zeros (N_ "Show zero balance items"))
(define optname-date-driver (N_ "Due or Post Date"))

(export optname-show-zeros)

;; The idea is:  have a hash with the key being the contact name
;; (In future this might be GUID'ed, but for now it's a string
;; from the description or the split memo.
;; The value is a record which contains the currency that contact
;; is stored in (you can only owe a particular contact one
;; currency, it just gets far too difficult otherwise), and a list
;; of buckets containing the money owed for each interval,
;; oldest first.
;; overpayment is just that - it stores the current overpayment, 
;; if any.  Any bills get taken out of the overpayment before
;; incurring debt.

(define company-info (make-record-type "ComanyInfo" 
				       '(currency
					 bucket-vector
					 overpayment
					 owner-obj)))

(define num-buckets 5)
(define (new-bucket-vector)
  (make-vector num-buckets (gnc-numeric-zero)))

(define make-company-private
  (record-constructor company-info '(currency bucket-vector overpayment owner-obj)))

(define (make-company currency owner-obj)
  (make-company-private currency (new-bucket-vector) (gnc-numeric-zero) owner-obj))

(define company-get-currency
  (record-accessor company-info 'currency))

(define company-get-owner-obj
  (record-accessor company-info 'owner-obj))

(define company-set-owner-obj!
  (record-modifier company-info 'owner-obj))

(define company-get-buckets
  (record-accessor company-info 'bucket-vector))

(define company-set-buckets
  (record-modifier company-info 'bucket-vector))

(define company-get-overpayment
  (record-accessor company-info 'overpayment))

(define company-set-overpayment
  (record-modifier company-info 'overpayment))

;; Put an invoice in the appropriate bucket

(define (process-invoice company amount bucket-intervals date)
  (define (in-interval this-date current-bucket)
    (gnc:timepair-lt this-date current-bucket))

  (define (find-bucket current-bucket bucket-intervals date)  
    (gnc:debug "looking for bucket for date: " date)
    (begin
      (gnc:debug "current bucket: " current-bucket)
      (gnc:debug "bucket-intervals: " bucket-intervals)
      (if (> current-bucket (vector-length bucket-intervals))
	  (gnc:error "sanity check failed in find-bucket")
	  (if (in-interval date (vector-ref bucket-intervals current-bucket))
	      (begin
		(gnc:debug "found bucket")
		current-bucket)
	      (find-bucket (+ current-bucket 1) bucket-intervals date)))))

  (define (calculate-adjusted-values amount overpayment)
    (if (>= (gnc-numeric-compare amount overpayment) 0)
	(cons (gnc-numeric-sub-fixed amount overpayment)
	      (gnc-numeric-zero))
	(cons (gnc-numeric-zero)
	      (gnc-numeric-sub-fixed overpayment amount))))

  (let* ((current-overpayment (company-get-overpayment company))
	 (adjusted-values (calculate-adjusted-values amount current-overpayment))
	 (adjusted-amount (car adjusted-values))
	 (adjusted-overpayment (cdr adjusted-values))
	 (bucket-index (find-bucket 0 bucket-intervals date))
	 (buckets (company-get-buckets company))
	 (new-bucket-value 
	  (gnc-numeric-add-fixed adjusted-amount (vector-ref buckets bucket-index))))
    (vector-set! buckets bucket-index new-bucket-value)
    (company-set-buckets company buckets)
    (company-set-overpayment company adjusted-overpayment)))


;; NOTE: We assume that bill payments occur in a FIFO manner - ie
;; any payment to a company goes towards the *oldest* bill first


(define (process-payment company amount)
  (define (process-payment-driver amount buckets current-bucket-index)
    (if (>= current-bucket-index (vector-length buckets))
	amount
	(let ((current-bucket-amt (vector-ref buckets current-bucket-index)))
	  (if (>= (gnc-numeric-compare current-bucket-amt amount) 0)
	      (begin
		(vector-set! buckets current-bucket-index (gnc-numeric-sub-fixed
							   current-bucket-amt amount))
		(gnc-numeric-zero))
	      (begin
		(vector-set! buckets current-bucket-index (gnc-numeric-zero))
		(process-payment-driver 
		 (gnc-numeric-sub-fixed amount current-bucket-amt)
		 buckets
		 (+ current-bucket-index 1)))))))
  
  (let ((overpayment (company-get-overpayment company)))
	;; if there's already an overpayment, make it bigger
    (gnc:debug "processing payment of " amount)
    (gnc:debug "overpayment was " overpayment)

	(if (gnc-numeric-positive-p overpayment)
	    (company-set-overpayment company (gnc-numeric-add-fixed overpayment amount))
	    
	    (let ((result (process-payment-driver amount (company-get-buckets company) 0)))
	      (gnc:debug "payment-driver processed.  new overpayment: " result)
	      (company-set-overpayment company result)))))
		  
;; determine date function to use 
(define (get-selected-date-from-txn transaction date-type)
  (if (eq? date-type 'postdate)
      (gnc-transaction-get-date-posted transaction)
      (xaccTransRetDateDueTS transaction)))		    
  
;; deal with a transaction - figure out if we've seen the company before
;; if so, either process it as a bill or a payment, if not, create
;; a new company record in the hash

(define (update-company-hash hash split bucket-intervals
			     reverse? show-zeros date-type)

  (define (do-update value)
    (let* ((transaction (xaccSplitGetParent split))
	   (temp-owner (gncOwnerNew))
	   (owner (gnc:owner-from-split split temp-owner)))

      (if (not (null? owner))
       (let* ((guid (gncOwnerReturnGUID owner))
	      (this-currency (xaccTransGetCurrency transaction))
	      (this-date (get-selected-date-from-txn transaction date-type))
	      (company-info (hash-ref hash guid)))

	 (gnc:debug "update-company-hash called")
	 (gnc:debug "owner: " owner ", guid: " guid)
	 (gnc:debug "split-value: " value)
	 (if reverse? (set! value (gnc-numeric-neg value)))
	 (if company-info
	     ;; if it's an existing company, destroy the temp owner and
	     ;; then make sure the currencies match
	     (begin
	       (if (not (gnc-commodity-equiv
			 this-currency
			 (company-get-currency company-info)))
		   (let ((error-str
			  (string-append "IGNORING TRANSACTION!\n" "Invoice Owner: " (gncOwnerGetName owner)
					 "\nTransaction GUID:" (gncTransGetGuid transaction)
					 "\nTransaction Currency" (gnc-commodity-get-mnemonic this-currency)
					 "\nClient Currency" (gnc-ommodity-get-mnemonic(company-get-currency company-info)))))
		     (gnc-error-dialog '() error-str)
		     (gnc:error error-str)
		     (cons #f (sprintf
			       (_ "Transactions relating to '%s' contain \
more than one currency. This report is not designed to cope with this possibility.")  (gncOwnerGetName owner))))
		   (begin
		     (gnc:debug "it's an old company")
		     (if (gnc-numeric-negative-p value)
			 (process-invoice company-info (gnc-numeric-neg value) bucket-intervals this-date)
			 (process-payment company-info value))
		     (hash-set! hash guid company-info)
		     (cons #t guid)))
	       (gncOwnerFree temp-owner))
		 
	     ;; if it's a new company
	     (begin
	       (gnc:debug "value" value)
	       (let ((new-company (make-company this-currency owner)))
		 (if (gnc-numeric-negative-p value)
		     (process-invoice new-company (gnc-numeric-neg value) bucket-intervals this-date)
		     (process-payment new-company value))
		 (hash-set! hash guid new-company))
	       (cons #t guid))))
       ; else (no owner)
       (gncOwnerFree temp-owner))))
  
  ;; figure out if this split is part of a closed lot
  ;; also save the split value...
  (let* ((lot (xaccSplitGetLot split))
	 (value (xaccSplitGetValue split))
	 (is-paid? (if (null? lot) #f (gnc-lot-is-closed lot))))

    ;; if it's closed, then ignore it because it doesn't matter.
    ;; XXX: we _could_ just set the value to 0 in order to list
    ;;      the company.  I'm not sure what to do.  Perhaps add an
    ;;      option?
    (if (or (not is-paid?) show-zeros)
	(do-update value))))

;; get the total debt from the buckets
(define (buckets-get-total buckets)
  (let ((running-total (gnc-numeric-zero))
	(buckets-list (vector->list buckets)))
    (for-each (lambda (bucket)
		(set! running-total
		      (gnc-numeric-add-fixed bucket running-total)))
	      buckets-list)
    running-total))


;; compare by the total in the buckets

(define (compare-total litem-a litem-b)    
  (let*  ((company-a (cdr litem-a))
	 (bucket-a (company-get-buckets company-a))
	 (company-b (cdr litem-b))
	 (bucket-b (company-get-buckets company-b))
	 (total-a (buckets-get-total bucket-a))
	 (total-b (buckets-get-total bucket-b))
	 (difference-sign (gnc-numeric-compare (gnc-numeric-sub-fixed total-a total-b) (gnc-numeric-zero))))
	 ;; if same totals, compare by name
	 (if (= difference-sign 0)
	     (gnc:safe-strcmp (car litem-a) (car litem-b))
	     difference-sign)))

;; compare by buckets, oldest first.

(define (compare-buckets litem-a litem-b)
  (define (driver buckets-a buckets-b)
    (if (null? buckets-a)
	0
	(let ((diff (gnc-numeric-compare
		     (gnc-numeric-sub-fixed
		      (car buckets-a) 
		      (car buckets-b)) 
		     (gnc-numeric-zero))))
	  (if (= diff 0)
	      (driver (cdr buckets-a) (cdr buckets-b))
	      diff))))
 
  (let*  ((company-a (cdr litem-a))
	 (bucket-a (vector->list (company-get-buckets company-a)))
	 (company-b (cdr litem-b))
	 (bucket-b (vector->list (company-get-buckets company-b)))
	 
	 (difference (driver bucket-a bucket-b)))
	 ;; if same totals, compare by name
	 (if (= difference 0)
	     (gnc:safe-strcmp (car litem-a) (car litem-b))
	     difference)))
  
    
;; set up the query to get the splits in the chosen account
;; XXX: FIXME: begindate is a hack -- we currently only go back a year
(define (setup-query query account date)
  (define (date-copy date)
    (cons (car date) (cdr date)))
  (let ((begindate (make-zdate))) ;Set begindate to the start of the Epoch
;    (gnc:debug "Account: " account)
    (gnc:debug "begindate" begindate)
    (gnc:debug "date" date)
    (qof-query-set-book query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
    (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS query #t begindate #t date QOF-QUERY-AND)
    (qof-query-set-sort-order query
			      (list SPLIT-TRANS TRANS-DATE-POSTED)
			      '() '())
    (qof-query-set-sort-increasing query #t #t #t)))
     

(define (aging-options-generator options)
  (let* ((add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (gnc:options-add-report-date!
     options gnc:pagename-general
     optname-to-date "a")
    ;; Use a default report date of 'today'
    (gnc:option-set-value (gnc:lookup-option options
                                             gnc:pagename-general
                                             optname-to-date)
                          (cons 'relative 'today))
   
 ;; all about currencies
    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "b")

    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "c" 'weighted-average)

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general
      optname-sort-by
      "i"
      (N_ "Sort companies by.")
      'name
      (list 
       (vector 'name (N_ "Name") (N_ "Name of the company."))
       (vector 'total (N_ "Total Owed") (N_ "Total amount owed to/from Company."))
       (vector 'oldest-bracket (N_ "Bracket Total Owed") (N_ "Amount owed in oldest bracket - if same go to next oldest.")))))

    (add-option 
     (gnc:make-multichoice-option
      gnc:pagename-general
       optname-sort-order
       "ia"
       (N_ "Sort order.")
       'increasing
       (list
	(vector 'increasing (N_ "Increasing") (N_ "0 -> $999,999.99, A->Z."))
	(vector 'decreasing (N_ "Decreasing") (N_ "$999,999.99 -> $0, Z->A.")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general
      optname-multicurrency-totals
      "i"
      (N_ "Show multi-currency totals. If not selected, convert all \
totals to report currency.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general
      optname-show-zeros
      "j"
      (N_ "Show all vendors/customers even if they have a zero balance.")
      #f))

    (add-option
      (gnc:make-multichoice-option
       gnc:pagename-general
       optname-date-driver
       "k"
       (N_ "Leading date.")
       'duedate
       (list
         (vector 'duedate (N_ "Due Date") (N_ "Due date is leading.")) ;; Should be using standard label for due date?
	 (vector 'postdate (N_ "Post Date") (N_ "Post date is leading."))))) ;; Should be using standard label for post date?
    
    (gnc:options-set-default-section options "General")      
    options))

(define (make-interval-list to-date)
  (let ((begindate to-date))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))

;; Have make-list create a stepped list, then add a date in the future for the "current" bucket
(define (make-extended-interval-list to-date)
    (define dayforcurrent (incdate to-date YearDelta)) ;; MAGIC CONSTANT
    (define oldintervalreversed (reverse (make-interval-list to-date)))		
  (reverse (cons dayforcurrent oldintervalreversed)))

(define (aging-renderer report-obj reportname account reverse?)

  (define (get-name a)
    (let* ((owner (company-get-owner-obj (cdr a))))
      (gncOwnerGetName owner)))

  ;; Predicates for sorting the companys once the data has been collected

  ;; Format: (cons 'sort-key (cons 'increasing-pred 'decreasing-pred))
  (define sort-preds
    (list 
     (cons 'name (cons (lambda (a b)
			 (string<? (get-name a) (get-name b)))
		       (lambda (a b)
			 (string>? (get-name a) (get-name b)))))
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
	    (gnc:warn "internal sorting option errorin aging.scm")
	    (lambda (a b)
	      (string<? (car a) (car b)))))))
		       
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))


  ;; XXX: This is a hack - will be fixed when we move to a
  ;; more general interval scheme in this report
  (define (make-heading-list)
    (list 
     (_ "Company")
     (_ "Current")
     (_ "0-30 days")
     (_ "31-60 days")
     (_ "61-90 days")
     (_ "91+ days")
     (_ "Total")))


  ;;  Make a list of commodity collectors for column totals

  (define (make-collector-list)
    (define (make-collector-driver done total)
      (if (< done total) 
	  (cons 
	   (gnc:make-commodity-collector)
	   (make-collector-driver (+ done 1) total))
	  '()))
    (make-collector-driver 0 (+ num-buckets 1)))
	  

  ;; update the column totals 

  (define (add-to-column-totals column-totals monetary-list)
    (begin
      (gnc:debug "column-totals" column-totals)
      (gnc:debug "monetary-list" monetary-list)
      (map (lambda (amount collector)
	   (begin
	     (gnc:debug "amount" amount)
	     (gnc:debug "collector" collector)
	     (collector 'add 
			(gnc:gnc-monetary-commodity amount)
			(gnc:gnc-monetary-amount amount))))
	 monetary-list
	 column-totals)))

  ;; convert the buckets in the header data structure 
  (define (convert-to-monetary-list bucket-list currency overpayment)
    (let* ((running-total (gnc-numeric-neg overpayment))
	   (monetised-buckets
	   (map (lambda (bucket-list-entry)
		  (begin
		    (set! running-total 
			  (gnc-numeric-add-fixed running-total bucket-list-entry))
		  (gnc:make-gnc-monetary currency bucket-list-entry)))
		(vector->list bucket-list))))
      (append (reverse monetised-buckets) 
	      (list (gnc:make-gnc-monetary currency running-total)))))

  ;; convert the collectors to the right output format 

  (define (convert-collectors collector-list report-currency 
			      exchange-fn
			      multi-currencies-p)
    (define (fmt-one-currency collector)
      (let ((monetary (gnc:sum-collector-commodity collector report-currency exchange-fn)))
	(if monetary
	    monetary
	    (begin
	      (gnc:warn "Exchange-lookup failed in fmt-one-currency")
	      #f))))

    (define (fmt-multiple-currencies collector)
      (let ((mini-table (gnc:make-html-table)))
	(collector 'format 
		   (lambda (commodity amount)
		   (gnc:html-table-append-row!
		    mini-table
		    (list (gnc:make-gnc-monetary 
			   commodity amount))))
		   #f)
	mini-table))
			
    (let ((fmt-function 
	   (if multi-currencies-p
	       fmt-multiple-currencies
	       fmt-one-currency)))
      (map fmt-function collector-list)))


  (gnc:report-starting reportname)
  (let* ((companys (make-hash-table 23))
	 (report-title (op-value gnc:pagename-general gnc:optname-reportname))
        ;; document will be the HTML document that we return.
	(report-date (gnc:timepair-end-day-time 
		      (gnc:date-option-absolute-time
		       (op-value gnc:pagename-general optname-to-date))))
	(interval-vec (list->vector (make-extended-interval-list report-date)))
	(sort-pred (get-sort-pred 
		    (op-value gnc:pagename-general optname-sort-by)
		    (op-value gnc:pagename-general optname-sort-order)))
	(report-currency (op-value gnc:pagename-general optname-report-currency))
	(price-source (op-value gnc:pagename-general optname-price-source))
	(multi-totals-p (op-value gnc:pagename-general optname-multicurrency-totals))
	(show-zeros (op-value gnc:pagename-general optname-show-zeros))
        (date-type (op-value gnc:pagename-general optname-date-driver))        
	(heading-list (make-heading-list))
	(exchange-fn (gnc:case-exchange-fn price-source report-currency report-date))
	(total-collector-list (make-collector-list))
	(table (gnc:make-html-table))
	(query (qof-query-create-for-splits))
	(company-list '())
	(work-done 0)
	(work-to-do 0)
        (document (gnc:make-html-document)))
;    (gnc:debug "Account: " account)

    ;; set default title
    (gnc:html-document-set-title! document report-title)
    ;; maybe redefine better...
    (if (not (null? account))
        (begin
          (gnc:html-document-set-title!
           document (string-append report-title ": " (xaccAccountGetName account)))
          (gnc:html-document-set-headline! document
                                           (gnc:html-markup
                                            "!" 
                                            report-title
                                            ": "
                                            (gnc:html-markup-anchor
                                             (gnc:account-anchor-text account)
                                             (xaccAccountGetName account))))))

    (gnc:html-table-set-col-headers! table heading-list)
				     
    (if (not (null? account))
	(begin
	  (setup-query query account report-date)
	  ;; get the appropriate splits
	  (let ((splits (qof-query-run query)))
;	    (gnc:debug "splits" splits)

	    ;; build the table
	    (set! work-to-do (length splits))
	    ;; work-done is already zero
	    (for-each (lambda (split)
			(gnc:report-percent-done (* 50 (/ work-done work-to-do)))
			(set! work-done (+ 1 work-done))
			(update-company-hash companys 
					      split 
					      interval-vec 
					      reverse? show-zeros
                                              date-type))
			splits)
;	    (gnc:debug "companys" companys)
	    ;; turn the hash into a list
	    (hash-for-each (lambda (key value)
			     (set! company-list
				   (cons (cons key value) company-list)))
			   companys)
;	    (gnc:debug "company list" company-list)
	   
	    (set! company-list (sort-list! company-list
					    sort-pred))

	    ;; build the table
	    (set! work-to-do (length company-list))
	    (set! work-done 0)
	    (for-each (lambda (company-list-entry)
			(gnc:report-percent-done (+ 50 (* 50 (/ work-done work-to-do))))
			(set! work-done (+ 1 work-done))
			(let* ((monetary-list (convert-to-monetary-list
					       (company-get-buckets
						(cdr company-list-entry))
					       (company-get-currency
						(cdr company-list-entry))
					       (company-get-overpayment
						(cdr company-list-entry))))
			       (owner (company-get-owner-obj
				       (cdr company-list-entry)))
			       (company-name (gncOwnerGetName owner)))

			  (add-to-column-totals total-collector-list
						monetary-list)

			  (let* ((ml (reverse monetary-list))
				 (total (car ml))
				 (rest (cdr ml)))

			    (set! monetary-list
				  (reverse
				   (cons
				    (gnc:make-html-text
				     (gnc:html-markup-anchor
				      (gnc:owner-report-text owner account)
				      total))
				    rest))))

			  (gnc:html-table-append-row!
			   table (cons
				  (gnc:make-html-text
				   (gnc:html-markup-anchor
				    (gnc:owner-anchor-text owner)
				    company-name))
				  monetary-list))
			  (gncOwnerFree owner)))
		      company-list)

	    ;; add the totals
	    (gnc:html-table-append-row!
	     table 
	     (cons (_ "Total") (convert-collectors total-collector-list 
						   report-currency
						   exchange-fn
						   multi-totals-p)))
	     
	    (gnc:html-document-add-object!
	     document table)))
	(gnc:html-document-add-object!
	 document
	 (gnc:make-html-text
	  (_ "No valid account selected. Click on the Options button and select the account to use."))))
    (qof-query-destroy query)
    (gnc:report-finished)
    document))

(export aging-options-generator)
(export aging-renderer)
