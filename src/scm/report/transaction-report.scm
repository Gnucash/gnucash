(require 'record)
(gnc:support "report/transaction-report.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in account(s)
;; original report by Robert Merkel (rgmerk@mira.net)
;; redone from scratch by Bryan Larsen (blarsen@ada-works.com)
;; totally rewritten for new report generation code by Robert Merkel

(let ()
  
  (define-syntax addto!
    (syntax-rules ()
		  ((_ alist element) (set! alist (cons element alist)))))
		     
  (define (split-account-name-same-p a b)
    (= (gnc:split-compare-account-names a b) 0))

  (define (split-account-code-same-p a b)
    (= (gnc:split-compare-account-codes a b) 0))

  (define (split-same-corr-account-p a b)
    (= (gnc:split-compare-other-account-names a b) 0))

  (define (split-same-corr-account-code-p a b)
    (= (gnc:split-compare-other-account-codes a b) 0))

  (define (timepair-same-year tp-a tp-b)
    (= (tm:year (gnc:timepair->date tp-a))
       (tm:year (gnc:timepair->date tp-b))))
  
  (define (timepair-same-month tp-a tp-b)
    (and (timepair-same-year tp-a tp-b) 
	 (= (tm:mon (gnc:timepair->date tp-a))
            (tm:mon (gnc:timepair->date tp-b)))))
  
  (define (split-same-month-p a b)
    (let ((tp-a (gnc:transaction-get-date-entered (gnc:split-get-parent a)))
	  (tp-b (gnc:transaction-get-date-entered (gnc:split-get-parent b))))
	  (timepair-same-month tp-a tp-b)))
    
    (define (split-same-year-p a b)
      (let ((tp-a (gnc:transaction-get-date-entered (gnc:split-get-parent a)))
	    (tp-b (gnc:transaction-get-date-entered (gnc:split-get-parent b))))
	(timepair-same-year tp-a tp-b)))

    (define (render-account-name-subheading split table)
      (gnc:html-table-append-row! 
       table 
       (list (gnc:account-get-name (gnc:split-get-account split)))))

    (define (render-account-code-subheading split table)
      (gnc:html-table-append-row! 
       table 
       (list (gnc:account-get-code (gnc:split-get-account split)))))

    (define (render-corresponding-account-name-subheading split table)
      (gnc:html-table-append-row!
       table (list (gnc:split-get-corr-account-name split))))
    
    (define (render-corresponding-account-code-subheading split table)
      (gnc:html-table-append-row!
       table (list (gnc:split-get-corr-account-code split))))
    
    (define (render-month-subheading split table)
      (gnc:html-table-append-row!
       table (list (strftime "%B %Y" (gnc:timepair->date 
                                      (gnc:transaction-get-date-entered 
                                       (gnc:split-get-parent split)))))))

    (define (render-year-subheading split table)
      (gnc:html-table-append-row!
       table (list (strftime "%Y" (gnc:timepair->date
                                   (gnc:transaction-get-date-entered
                                    (gnc:split-get-parent split)))))))
    (let ()

      (define comp-funcs-assoc-list
	(list (cons 'account-name  (vector 
				    'by-account-name 
				    split-account-name-same-p 
				    render-account-name-subheading))
	      (cons 'account-code  (vector 
				    'by-account-code 
				    split-account-code-same-p
				    render-account-code-subheading))
	      (cons 'date          (vector 'by-date #f #f))
	      (cons 'date-monthly
                    (vector 'by-date
                            split-same-month-p render-month-subheading))
	      (cons 'date-yearly
                    (vector 'by-date 
			    split-same-year-p 
			    render-year-subheading))
	      (cons 'corresponding-acc-name
                    (vector 'by-corr-account-name #f #f))
	      (cons 'corresponding-acc-code
                    (vector 'by-corr-account-code #f #f))
	      (cons 'corresponding-acc-name-subtotal 
		    (vector 'by-corr-account-name 
			    split-same-corr-account-p 
			    render-corresponding-account-name-subheading))
	      (cons 'correspoinding-acc-code-subtotal 
		    (vector 
		     'by-corr-account-code 
		     split-same-corr-account-code-p 
		     render-corresponding-account-code-subheading))
	      (cons 'amount (vector 'by-amount #f #f))
	      (cons 'description (vector 'by-desc #f #f))
	      (cons 'number (vector 'by-num #f #f))
	      (cons 'memo   (vector 'by-memo #f #f))
	      (cons 'none    (vector 'by-none #f #f))))
      
;      (define <columns-used>
;	(make-record-type "<columns-used>"
;			  (list 'date 
;				'num 
;				'description 
;				'account 
;				'other-account 
;				'shares 
;				'price
;				'amount-single
;				'amount-double-positive
;				'amount-double-negative
;				'running-balance)))
      
      (define (used-date columns-used)
	(vector-ref columns-used 0))
      (define (used-num columns-used)
	(vector-ref columns-used 1))
      (define (used-description columns-used)
	(vector-ref columns-used 2))
      (define (used-account columns-used)
	(vector-ref columns-used 3))
      (define (used-other-account columns-used)
	(vector-ref columns-used 4))	
      (define (used-shares columns-used)
	(vector-ref columns-used 5))	
      (define (used-price columns-used)
	(vector-ref columns-used 6))	
      (define (used-amount-single columns-used)
	(vector-ref columns-used 7))	
      (define (used-amount-double-positive columns-used)
	(vector-ref columns-used 8))	
      (define (used-amount-double-negative columns-used)
	(vector-ref columns-used 9))	
      (define (used-running-balance columns-used)
	(vector-ref columns-used 10))	

      (define (build-column-used options)   
	(define (opt-val section name)
	  (gnc:option-value 
	   (gnc:lookup-option options section name)))
	(let ((column-list (make-vector 11 #f)))
	  
	  (define (opt-val section name)
	    (gnc:option-value 
	     (gnc:lookup-option options section name)))
	  (if (opt-val (N_ "Display") (N_ "Date"))
	      (vector-set! column-list 0 #t))
	  (if (opt-val (N_ "Display") (N_ "Num"))
	      (vector-set! column-list 1 #t))
	  (if (opt-val (N_ "Display") (N_ "Description"))
	      (vector-set! column-list 2 #t))
	  (if (opt-val (N_ "Display") (N_ "Account"))
	      (vector-set! column-list 3 #t))
	  (if (opt-val (N_ "Display") (N_ "Other Account"))
	      (vector-set! column-list 4 #t))
	  (if (opt-val (N_ "Display") (N_ "Shares"))
	      (vector-set! column-list 5 #t))

	  (if (opt-val (N_ "Display") (N_ "Price"))
	      (vector-set! column-list 6 #t))
;   (gnc:warn "Amount Display" (opt-val (N_ "Display") (N_ "Amount")))

	  (let ((amount-setting (opt-val (N_ "Display") (N_ "Amount"))))
	    (if (eq? amount-setting 'single)
	      (vector-set! column-list 7 #t))
	    (if (eq? amount-setting 'double)
		(begin 
	      (vector-set! column-list 8 #t)
	      (vector-set! column-list 9 #t))))
	  (if (opt-val (N_ "Display") (N_ "Running Balance"))
	      (vector-set! column-list 10 #t))
	;  (gnc:debug "Column list:" column-list)
      column-list))


  (define (make-heading-list column-vector)
    (let ((heading-list '()))
      (gnc:debug "Column-vector" column-vector)
      (if (used-date column-vector)
	  (addto! heading-list (N_ "Date")))
      (if (used-num column-vector)
	  (addto! heading-list (N_ "Num")))
      (if (used-description column-vector)
	  (addto! heading-list (N_ "Description")))
      (if (used-account column-vector)
	  (addto! heading-list (N_ "Account")))
      (if (used-other-account column-vector)
	  (addto! heading-list (N_ "Transfer from/to")))
      (if (used-shares column-vector)
	  (addto! heading-list (N_ "Shares")))
      (if (used-price column-vector)
	  (addto! heading-list (N_ "Price")))
      (if (used-amount-single column-vector)
	  (addto! heading-list (N_ "Amount")))
      ;; FIXME: Proper labels: what?
      (if (used-amount-double-positive column-vector)
	  (addto! heading-list (N_ "Debit")))
      (if (used-amount-double-negative column-vector)
	  (addto! heading-list (N_ "Credit")))
      (if (used-running-balance column-vector)
	  (addto! heading-list (N_ "Balance")))
      (reverse heading-list)))
  
  (define (add-split-row table split column-vector)
    (let* ((row-contents '())
	   (parent (gnc:split-get-parent split))
	   (account (gnc:split-get-account split))
	   (currency (gnc:account-get-commodity account))
	   (damount (gnc:split-get-share-amount split))
	   (split-value (gnc:make-gnc-monetary currency damount)))
      
      
      (if (used-date column-vector)
	  (addto! row-contents (gnc:timepair-to-datestring 
				(gnc:transaction-get-date-posted parent))))
      
      
      (if (used-num column-vector)
	  (addto! row-contents (gnc:transaction-get-num parent)))
      
      (if (used-description column-vector)
	  (addto! row-contents (gnc:transaction-get-description parent)))
      (if (used-account column-vector)
	  (addto! row-contents (gnc:account-get-name account)))
      (if (used-other-account column-vector)
	  (addto! row-contents (gnc:split-get-corr-account-name split)))
      (if (used-shares column-vector)
	  (addto! row-contents (gnc:split-get-share-amount split)))
      (if (used-price column-vector)
	  (addto! 
	   row-contents 
	   (gnc:make-gnc-monetary currency (gnc:split-get-share-price split))))
      (if (used-amount-single column-vector)
	  (addto! row-contents split-value))
      (if (used-amount-double-positive column-vector)
	  (if (gnc:numeric-positive-p split-amount)
	      (addto! row-contents split-amount)
	      (addto! row-contents " ")))
      (if (used-amount-double-negative column-vector)
	  (if (gnc:numeric-negative-p split-amount)
	      (addto! row-contents (gnc:monetary-neg split-amount))
	      (addto! row-contents " ")))
      (if (used-running-balance column-vector)
	  (addto! row-contents
                  (gnc:make-gnc-monetary currency
                                         (gnc:split-get-balance split))))
      (gnc:html-table-append-row! table (reverse row-contents))
      split-value))

  (define (lookup-sort-key sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 0))
  (define (lookup-subtotal-pred sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 1))
  (define (trep-options-generator)
    (define gnc:*transaction-report-options* (gnc:new-options))
    (define (gnc:register-trep-option new-option)
      (gnc:register-option gnc:*transaction-report-options* new-option))

    ;; from date
    ;; hack alert - could somebody set this to an appropriate date?
    (gnc:register-trep-option
     (gnc:make-date-option
      (N_ "Report Options") (N_ "From")
      "a" (N_ "Report Items from this date") 
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

    (gnc:register-trep-option
     (gnc:make-date-option
      (N_ "Report Options") (N_ "To")
      "b" (N_ "Report items up to and including this date")
      (lambda () (cons 'absolute (cons (current-time) 0)))
      #f 'absolute #f))

    ;; account to do report on
    (gnc:register-trep-option
     (gnc:make-account-list-option
      (N_ "Report Options") (N_ "Account")
      "c" (N_ "Do transaction report on these accounts")
      (lambda ()
        (let ((current-accounts (gnc:get-current-accounts))
              (num-accounts (gnc:group-get-num-accounts
                             (gnc:get-current-group)))
              (first-account (gnc:group-get-account
                              (gnc:get-current-group) 0)))
          (cond ((not (null? current-accounts)) (list (car current-accounts)))
                ((> num-accounts 0) (list first-account))
                (else ()))))
      #f #t))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      (N_ "Report Options") (N_ "Style")
      "d" (N_ "Report style")
;; XXX: merged style currently disabled because it breaks double-column
;; amounts.  If somebody wants it back just uncomment the commented code
;; below
      ; 'merged
      'single
      (list ;#(merged
	    ;  "Merged"
	    ;  "Display N-1 lines")
            (vector 'multi-line
                   (N_ "Multi-Line")
                   (N_ "Display N lines"))
            (vector 'single
                   (N_ "Single")
                   (N_ "Display 1 line")))))
    (let ((key-choice-list 
	   (list (vector 'account-name
                        (N_ "Account Name(w/subtotal)")
                        (N_ "Sort & subtotal by account name"))
		 (vector 'account-code
			(N_ "Account Code (w/subtotal)")
			(N_ "Sort & subtotal by account code"))
		 (vector 'date
                        (N_ "Date")
                        (N_ "Sort by date"))
		 (vector 'date-monthly
                        (N_ "Date (subtotal monthly)")
                        (N_ "Sort by date & subtotal each month"))
		 
                  (vector 'date-yearly
                        (N_ "Date (subtotal yearly)")
                        (N_ "Sort by date & subtotal each year"))
		 
		 
                  (vector 'corresponding-acc-name
                        (N_ "Transfer from/to")
                        (N_ "Sort by account transferred from/to's name"))
		 
                  (vector 'corresponding-acc-name-subtotal
                        (N_ "Transfer from/to (w/subtotal) by code ")
                        (N_ "Sort and subtotal by account transferred \
from/to's code"))
				
		  (vector 'corresponding-acc-code
                        (N_ "Transfer from/to code")
                        (N_ "Sort by account transferred from/to's code"))
		
		  (vector 'corresponding-acc-code-subtotal
                        (N_ "Transfer from/to (w/subtotal)")
                        (N_ "Sort and subtotal by account \
transferred from/to's code"))
		
                  (vector 'amount
                        (N_ "Amount")
                        (N_ "Sort by amount"))
		 
                  (vector 'description
                        (N_ "Description")
                        (N_ "Sort by description"))
		 
                  (vector 'number
                        (N_ "Number")
                        (N_ "Sort by check/transaction number"))
		 
                  (vector 'memo
                        (N_ "Memo")
                        (N_ "Sort by memo"))
		 
                  (vector 'none
                        (N_ "None")
                        (N_ "Do not sort")))))

       ;; primary sorting criterion
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	(N_ "Sorting") (N_ "Primary Key")
	"a" (N_ "Sort by this criterion first")
	'account-name
	key-choice-list))
     
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	(N_ "Sorting") (N_ "Primary Sort Order")
	"b" (N_ "Order of primary sorting")
	'ascend
	(list
        (vector 'ascend
		 (N_ "Ascending")
		 (N_ "smallest to largest, earliest to latest"))
	 (vector 'descend
		 (N_ "Descending")
		 (N_ "largest to smallest, latest to earliest")))))
   
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	(N_ "Sorting") (N_ "Secondary Key")
	"c"
	(N_ "Sort by this criterion second")
	'date
	key-choice-list)))
     
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	(N_ "Sorting") (N_ "Secondary Sort Order")
	"d" (N_ "Order of Secondary sorting")
	'ascend
	(list
         (vector 'ascend
		 (N_ "Ascending")
                (N_ "smallest to largest, earliest to latest"))
	 (vector 'descend
		(N_ "Descending")
		(N_ "largest to smallest, latest to earliest")))))
     
     (gnc:register-trep-option
       (gnc:make-simple-boolean-option
	(N_ "Display") (N_ "Date")
	"b" (N_ "Display the date?") #t))
    
     (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Num")
      "c" (N_ "Display the cheque number?") #t))
    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Description")
      "d" (N_ "Display the description?") #t))
    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Memo")
      "f" (N_ "Display the memo?") #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Account")
      "g" (N_ "Display the account?") #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Other Account")
      "h" (N_ "Display the other account?\
(if this is a split transaction, this parameter is guessed).") #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Shares")
      "ha" (N_ "Display the number of shares?") #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Price")
      "hb" "Display the shares price?" #f))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      (N_ "Display") (N_ "Amount")
      "i" (N_ "Display the amount?")  
      'single
      (list
       (vector 'none (N_ "None") (N_ "No amount display"))
      (vector 'single (N_ "Single") (N_ "Single Column Display"))
       (vector 'double (N_ "Double") (N_ "Two Column Display")))))
    
    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Headers")
      "j" (N_ "Display the headers?") #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Running Balance")
      "k" (N_ "Display a running balance") #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Totals")
      "l" (N_ "Display the totals?") #t))

    (gnc:options-set-default-section gnc:*transaction-report-options*
                                     "Report Options")

   gnc:*transaction-report-options*)

  (define (display-date-interval begin end)
    (let ((begin-string (strftime "%x" (localtime (car begin))))
	  (end-string (strftime "%x" (localtime (car end)))))
      (string-append (_ "From") " " begin-string (_"To") " " end-string)))

 (define (make-account-subheading acc-name from-date)
   (let* ((separator (string-ref (gnc:account-separator-char) 0))
	  (acc (gnc:get-account-from-full-name
		(gnc:get-current-group)
		acc-name
		separator))
          (unsigned-balance (d-gnc:account-get-balance-at-date
                            acc
			     from-date
			     #f))
	  (signed-balance (if (gnc:account-reverse-balance? acc)
			      (- unsigned-balance)
			      unsigned-balance)))
     (string-append acc-name
    " ("
                    (_ "Opening Balance")
		    " "
		    (gnc:amount->string signed-balance
                                        (gnc:account-value-print-info acc #f))
		    ")"
		    )))

  (define (make-split-table splits options)
    (define (add-subtotal-row table split used-columns subtotal-collector)
      (let ((currency-totals (subtotal-collector
                              'format gnc:make-gnc-monetary #f)))
;	(gnc:warn "Subtotal-collector" subtotal-collector)
;	(gnc:warn "Currency-totals:" currency-totals)
	(for-each (lambda (currency)
                    (gnc:html-table-append-row! table (list currency)))
                  currency-totals)))

    (define (get-primary-subtotal-pred options)
      (vector-ref (cdr 
		     (assq (gnc:option-value
                            (gnc:lookup-option options
                                               (N_ "Sorting")
                                               (N_ "Primary Key")))
			   comp-funcs-assoc-list)) 
		  1))
    (define (get-secondary-subtotal-pred options)
      (vector-ref (cdr 
                   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             (N_ "Sorting")
                                             (N_ "Secondary Key")))
			   comp-funcs-assoc-list))
		  1))

    (define (get-primary-subheading-renderer option)
      (vector-ref (cdr 
		   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             (N_ "Sorting")
                                             (N_ "Primary Key")))
			   comp-funcs-assoc-list))
		  2))

    (define (get-secondary-subheading-renderer option)
      (vector-ref (cdr 
		   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             (N_ "Sorting")
                                             (N_ "Secondary Key")))
			   comp-funcs-assoc-list))
		  2))

    (define (transaction-report-multi-rows-p options)
      (eq? (gnc:option-value
            (gnc:lookup-option options (N_ "Report Options") (N_ "Style")))
	   'multi-line))

    (define (add-other-split-rows split table used-columns)
      (define (other-rows-driver split parent table used-columns i)
	(let ((current (gnc:transaction-get-split parent i)))
	  (gnc:debug "i" i)
	  (gnc:debug "current" current)
	  (cond ((not current) #f)
		((equal? current split)
		 (other-rows-driver split parent table used-columns (+ i 1)))
		(else (begin
			(add-split-row table current used-columns)
			(other-rows-driver split parent table used-columns
                                           (+ i 1)))))))

	     (other-rows-driver split (gnc:split-get-parent split)
                                table used-columns 0))

    (define (do-rows-with-subtotals splits 
                                    table 
                                    used-columns
                                    multi-rows?
                                    primary-subtotal-pred 
                                    secondary-subtotal-pred 
                                    primary-subheading-renderer
                                    secondary-subheading-renderer
                                    primary-subtotal-collector 
                                    secondary-subtotal-collector 
                                    total-collector)
      (if (null? splits) #f
	  (let* ((current (car splits))

		 (rest (cdr splits))
		 (next (if (null? rest) #f
			   (car rest)))
	         (split-value (add-split-row table current used-columns)))
	    (if multi-rows?
		(add-other-split-rows current table used-columns))
	    (primary-subtotal-collector 'add 
					(gnc:gnc-monetary-commodity
                                         split-value) 
					(gnc:gnc-monetary-amount split-value))
	    (secondary-subtotal-collector 'add
					  (gnc:gnc-monetary-commodity
                                           split-value)
					  (gnc:gnc-monetary-amount
                                           split-value))
	    (total-collector 'add
			     (gnc:gnc-monetary-commodity split-value)
			     (gnc:gnc-monetary-amount split-value))
	    (if (and secondary-subtotal-pred
                     (or (not next)
                         (and next
                              (not (secondary-subtotal-pred current next)))))
		(begin (add-subtotal-row table current used-columns
                                         secondary-subtotal-collector)
		       (secondary-subtotal-collector 'reset #f #f)
		       (if next
                           (secondary-subheading-renderer current table))))
	    (if (and primary-subtotal-pred
                     (or (not next)
                         (and next
                              (not (primary-subtotal-pred current next)))))
		(begin (add-subtotal-row table current used-columns
                                         primary-subtotal-collector)
		       (primary-subtotal-collector 'reset #f #f)
		       (if next
		       (primary-subheading-renderer next table))))
	    (do-rows-with-subtotals rest 
				    table 
				    used-columns 
				    multi-rows?
				    primary-subtotal-pred 
				    secondary-subtotal-pred
				    primary-subheading-renderer 
				    secondary-subheading-renderer
				    primary-subtotal-collector 
				    secondary-subtotal-collector 
				    total-collector))))

    (let ((table (gnc:make-html-table))
	  (used-columns (build-column-used options))
	  (multi-rows? (transaction-report-multi-rows-p options))
	  (primary-subtotal-pred (get-primary-subtotal-pred options))
	  (secondary-subtotal-pred (get-secondary-subtotal-pred options))
	  (primary-subheading-renderer
           (get-primary-subheading-renderer options))
	  (secondary-subheading-renderer
           (get-secondary-subheading-renderer options)))
      (gnc:html-table-set-col-headers!
      table
       (make-heading-list used-columns))
;     (gnc:warn "Splits:" splits)
     (do-rows-with-subtotals splits table used-columns
                             multi-rows? primary-subtotal-pred
                             secondary-subtotal-pred
			     primary-subheading-renderer
			     secondary-subheading-renderer
 			     (gnc:make-commodity-collector)
                              (gnc:make-commodity-collector)
                              (gnc:make-commodity-collector))
      table))

  (define (trep-renderer report-obj)
    (define (opt-val section name)
      (gnc:option-value
       (gnc:lookup-option (gnc:report-options report-obj) section name)))

    (let ((document (gnc:make-html-document))
	  (c_accounts (opt-val "Report Options" "Account"))
	  (begindate (gnc:date-option-absolute-time
                      (opt-val "Report Options" "From")))
	  (enddate (gnc:date-option-absolute-time
                    (opt-val "Report Options" "To")))
	  (primary-key (opt-val "Sorting" "Primary Key"))
	  (primary-order (opt-val "Sorting" "Primary Sort Order"))
	  (secondary-key (opt-val "Sorting" "Secondary Key"))
	  (secondary-order (opt-val "Sorting" "Secondary Sort Order"))
	  (splits '())
          (table '())
	  (query (gnc:malloc-query)))

      (gnc:query-set-group query (gnc:get-current-group))
      (gnc:query-add-account-match query
                                   (gnc:list->glist c_accounts)
                                   'acct-match-any 'query-and)
      (gnc:query-add-date-match-timepair
       query #t begindate #t enddate 'query-and)
      (gnc:query-set-sort-order query
				(lookup-sort-key primary-key)
                                (lookup-sort-key secondary-key)
				'by-none)
      (gnc:query-set-sort-increasing query
				     (eq? primary-order 'ascend)
				     (eq? secondary-order 'ascend)
				     #t)

      (set! splits (gnc:glist->list (gnc:query-get-splits query)
                                    <gnc:Split*>))
;      (gnc:warn "Splits in trep-renderer:" splits)
      (set! table (make-split-table splits (gnc:report-options report-obj)))
							 
      (gnc:html-document-set-title! document (_ "Transaction Report"))
      (gnc:html-document-add-object! 
       document
       (gnc:make-html-text
	(gnc:html-markup-h3 (display-date-interval begindate enddate))))
      (gnc:html-document-add-object!
       document 
       table)
      (gnc:free-query query)

      document))

  (gnc:define-report

   ;; The version of this report.
   'version 2

   ;; The name of this report. This will be used, among other things,
   ;; for making its menu item in the main menu. You need to use the
   ;; untranslated value here!
   'name (N_ "Transaction Report")

   ;; The options generator function defined above.
   'options-generator trep-options-generator

   ;; The rendering function defined above.
   'renderer trep-renderer)))
