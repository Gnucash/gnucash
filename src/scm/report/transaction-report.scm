;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in account(s)
;; original report by Robert Merkel (rgmerk@mira.net)
;; redone from scratch by Bryan Larsen (blarsen@ada-works.com)

(gnc:support "report/transaction-report.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "date-utilities.scm")
(gnc:depend "html-generator.scm")

(let ()
 (define string-db (gnc:make-string-database))

 (define (gnc:split-get-sign-adjusted-value split)
   (let ((acc (gnc:split-get-account split))
	 (unsigned-value (d-gnc:split-get-value split)))
     (gnc:debug "Adjusting value" unsigned-value (gnc:account-reverse-balance? acc))     
     (if (gnc:account-reverse-balance? acc)
	 (- unsigned-value)
	 unsigned-value)))

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
		    (string-db 'lookup 'open-bal-string) 
		    " "
		    (gnc:amount->string signed-balance
                                        (gnc:account-value-print-info acc #f))
		    ")"
		    )))

 (define (make-split-report-spec options)
    (remove-if-not
     (lambda (x) x)
     (list
      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Date"))
       (make-report-spec 
	(string-db 'lookup 'date-string)
	(lambda (split) 
	  (gnc:transaction-get-date-posted 
	   (gnc:split-get-parent split)))
	(lambda (date) 
	  (html-left-cell (html-string (gnc:print-date date))))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	#f ; subs-list-proc
	#f)
       #f)
 
      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Num"))
       (make-report-spec 
	(string-db 'lookup 'num-string)
	(lambda (split)
	  (gnc:transaction-get-num
	   (gnc:split-get-parent split)))
	(lambda (num) (html-left-cell (html-string num)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	#f ; subs-list-proc
	#f) ; subentry-html-proc
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Description"))
       (make-report-spec 
	(string-db 'lookup 'desc-string)
	(lambda (split)
	  (gnc:transaction-get-description
	   (gnc:split-get-parent split)))
	(lambda (desc) (html-left-cell (html-string desc)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	#f ; subs-list-proc
	#f) ; subentry-html-proc
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Memo"))
       (make-report-spec 
	(string-db 'lookup 'memo-string)
	gnc:split-get-memo
	(lambda (memo) (html-left-cell (html-string memo)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	(lambda (split)
	  (map gnc:split-get-memo (gnc:split-get-other-splits split)))
	(lambda (memo) (html-left-cell (html-string memo))))
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Account"))
       (make-report-spec 
	(string-db 'lookup 'acc-string)
	(lambda (split) 
	  (gnc:account-get-full-name 
	   (gnc:split-get-account split)))
	(lambda (account-name) (html-left-cell (html-string account-name)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	(lambda (split)
	  (map 
	   (lambda (other)
	     (gnc:account-get-full-name (gnc:split-get-account other)))
	   (gnc:split-get-other-splits split)))
	(lambda (account-name) (html-left-cell (html-string account-name))))
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Other Account"))
       (make-report-spec 
	(string-db 'lookup 'other-acc-string)
	(lambda (split)
	  (let ((others (gnc:split-get-other-splits split)))
	    (if (null? others)
		""
		(gnc:account-get-full-name 
		 (gnc:split-get-account (car others))))))
	(lambda (account-name) (html-left-cell (html-string account-name)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	#f
	#f)
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Shares"))
       (make-report-spec 
	(string-db 'lookup 'shares-string)
	(lambda (split)
	  (d-gnc:split-get-share-amount split))
	(lambda (num) (html-right-cell (html-string num)))
	+ ; total-proc
	#f ; subtotal-html-proc
	(lambda (num) (html-right-cell (html-string num))) ; total-html-proc
	#t ; first-last-preference
	#f ; subs-list-proc
	#f) ; subentry-html-proc
       #f)

      (if 
       (gnc:option-value
	(gnc:lookup-option options "Display" "Price"))
       (make-report-spec 
	(string-db 'lookup 'price-string)
	(lambda (split)
	  (d-gnc:split-get-share-price split))
	(lambda (num) (html-right-cell (html-string num)))
	#f ; total-proc
	#f ; subtotal-html-proc
	#f ; total-html-proc
	#t ; first-last-preference
	#f ; subs-list-proc
	#f) ; subentry-html-proc
       #f)

      (if 
       (eq? (gnc:option-value
	(gnc:lookup-option options "Display" "Amount")) 'single)
       (make-report-spec
	(string-db 'lookup 'amount-string)
	gnc:split-get-sign-adjusted-value
	(lambda (value) (html-right-cell (html-currency value)))
	+ ; total-proc
	(lambda (value) 
	  (html-right-cell (html-strong (html-currency value))))
	(lambda (value) 
	  (html-right-cell (html-strong (html-currency value))))
	#t ; first-last-preference
	(lambda (split)
	  (map gnc:split-get-sign-adjusted-value (gnc:split-get-other-splits split)))
	(lambda (value) 
	  (html-right-cell (html-ital (html-currency value)))))
       #f)
      
      (if
        (eq? (gnc:option-value
 	(gnc:lookup-option options "Display" "Amount")) 'double)
        (make-report-spec
 	(string-db 'lookup 'debit-string)
 	(lambda (split)
	  (max 0 (gnc:split-get-sign-adjusted-value split)))
 	(lambda (value)
 	  (cond ((> value 0.0) (html-right-cell (html-currency value)))
	  (else (html-right-cell (html-ital (html-string " "))))))
;	(lambda (value)
; 	  (if (> value 0) (html-right-cell (html-currency value)))
;	  (html-right-cell (html-ital (html-string " "))))  
 	+ ; total-proc
 	(lambda (value)
 	  (html-right-cell (html-strong (html-currency value))))
 	(lambda (value)
 	  (html-right-cell (html-strong (html-currency value))))
 	#t ; first-last-preference
 	(lambda (split)
 	  (map gnc:split-get-sign-adjusted-value (gnc:split-get-other-splits split)))
 ;	(lambda (value)
; 	  (if (> value 0) (html-right-cell (html-ital (html-currency value)))
; 	      (html-right-cell (html-ital (html-string " ")))))
	(lambda (value)
 	  (cond ((> value 0.0) (html-right-cell (html-ital(html-currency value))))
	  (else (html-right-cell (html-ital (html-string " ")))))))
        #f)

      (if
        (eq? (gnc:option-value
  	(gnc:lookup-option options "Display" "Amount")) 'double)
        (make-report-spec
	 (string-db 'lookup 'credit-string)
	 (lambda (split)
	   (max  0 (- (gnc:split-get-sign-adjusted-value split))))
;	 (lambda (value) (html-right-cell (html-currency value)))
 	(lambda (value)
;	  (display value)
;	  (display (> value 0.0))
;	  (display "\n")
 	  (cond ((> value 0.0) (html-right-cell (html-currency value)))
	  (else (html-right-cell (html-ital (html-string " "))))))
 	+ ; total-proc
 	(lambda (value)
 	  (html-right-cell (html-strong (html-currency value))))	
 	(lambda (value)
 	  (html-right-cell (html-strong (html-currency value))))
 	#t ; first-last-preference
 	(lambda (split)
 	  (map gnc:split-get-sign-adjusted-value (gnc:split-get-other-splits split)))
 	(lambda (value)
 	  (cond  ((< value 0) (html-right-cell (html-ital (html-currency (- value)))))
 	      (else (html-right-cell (html-ital (html-string " ")))))))
        #f)

       
       (if 
       (eq? (gnc:option-value
	(gnc:lookup-option options "Display" "Amount")) 'double)
       (make-report-spec
	(string-db 'lookup 'total-string)
	gnc:split-get-sign-adjusted-value
	;(lambda (value) (html-right-cell (html-currency value)))
	;(lambda (value) (html-right-cell (html-string "hello")))
	#f
	+ ; total-proc
	(lambda (value) 
	  (html-right-cell (html-strong (html-currency value))))
	(lambda (value) 
	  (html-right-cell (html-strong (html-currency value))))
	#t ; first-last-preference
	#f ;
	#f)
       #f))))

  (define (split-report-get-sort-spec-entry key ascending? begindate)
    (case key
      ((account)
       (make-report-sort-spec
	(lambda (split) (gnc:account-get-full-name
                         (gnc:split-get-account split)))
	(if ascending? string-ci<? string-ci>?)
	string-ci=?
	string-ci=?
	(lambda (x) (make-account-subheading x begindate))))

      ((date)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))
	(if ascending? 
	    gnc:timepair-later-date
	    gnc:timepair-earlier-date)
	gnc:timepair-eq-date
	#f
	#f))

      ((date-monthly)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))
	(if ascending? 
	    gnc:timepair-later-date
	    gnc:timepair-earlier-date)
	gnc:timepair-eq-date
	(lambda (a b)
	  (= (gnc:timepair-get-month a)
	     (gnc:timepair-get-month b)))
	(lambda (date) 
	  (gnc:date-get-month-string (localtime (gnc:timepair->secs date))))))

      ((date-yearly)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))
	(if ascending? 
	    gnc:timepair-later-date
	    gnc:timepair-earlier-date)
	gnc:timepair-eq-date
	(lambda (a b)
	  (= (gnc:timepair-get-year a)
	     (gnc:timepair-get-year b)))
	(lambda (date) 
	  (number->string (gnc:timepair-get-year date)))))

      ((time)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-date-entered (gnc:split-get-parent split)))
	(if ascending?
	    gnc:timepair-later
	    gnc:timepair-earlier)
	gnc:timepair-eq
	#f
	#f))

      ((description)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-description (gnc:split-get-parent split)))
	(if ascending? string-ci<? string-ci>?)
	string-ci=?
	#f
	#f))

      ((number)
       (make-report-sort-spec
	(lambda (split) 
	  (gnc:transaction-get-num (gnc:split-get-parent split)))
	(if ascending? string-ci<? string-ci>?)
	string-ci=?
	#f
	#f))

      ((memo)
       (make-report-sort-spec
	gnc:split-get-memo
	(if ascending? string-ci<? string-ci>?)
	stri1ng-ci=?
	#f
	#f))

      ((corresponding-acc)
       (make-report-sort-spec
	(lambda (split)
	  (gnc:account-get-full-name
	   (gnc:split-get-account
	    (car (append
		  (gnc:split-get-other-splits split) ;;may return null
		  (list split))))))
	(if ascending? string-ci<? string-ci>?)
	string-ci=?
	#f
	#f))

      ((corresponding-acc-subtotal)
       (make-report-sort-spec
	(lambda (split)
	  (gnc:account-get-full-name
	   (gnc:split-get-account
	    (car (append
		  (gnc:split-get-other-splits split)
		  (list split))))))
	(if ascending? string-ci<? string-ci>?)
	string-ci=?
	string-ci=?
	(lambda (x) x)))

      ((amount)
       (make-report-sort-spec
	gnc:split-get-sign-adjusted-value
	(if ascending? < >)
	=
	#f
	#f))

      ((none) #f)
      (else (gnc:error "invalid sort argument"))))
    

  (define (make-split-list account split-filter-pred)
    (let ((num-splits (gnc:account-get-split-count account)))
      (let loop ((index 0)
		 (split (gnc:account-get-split account 0))
		 (slist '()))
	(if (= index num-splits)
	    (reverse slist)
	    (loop (+ index 1)
		  (gnc:account-get-split account (+ index 1))
		  (if (split-filter-pred split)
		      (cons split slist)
		      slist))))))

  ;; returns a predicate that returns true only if a split is
  ;; between early-date and late-date
  (define (split-report-make-date-filter-predicate begin-date-tp
                                                   end-date-tp)
    (lambda (split) 
      (let ((tp
	     (gnc:transaction-get-date-posted
	      (gnc:split-get-parent split))))
	(and (gnc:timepair-ge-date tp begin-date-tp)
	     (gnc:timepair-le-date tp end-date-tp)))))

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
            (cons 'absolute (cons time 0)))))
      #f 'absolute #f))   

    ;; to-date

    (gnc:register-trep-option
     (gnc:make-date-option
      "Report Options" "To"
      "b" "Report items up to and including this date"
      (lambda () (cons 'absolute (cons (current-time) 0)))
      #f 'absolute #f))

    ;; account to do report on
    (gnc:register-trep-option
     (gnc:make-account-list-option
      "Report Options" "Account"
      "c" "Do transaction report on these accounts"
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
      "Report Options" "Style"
      "d" "Report style"
;; XXX: merged style currently disabled because it breaks double-column
;; amounts.  If somebody wants it back just uncomment the commented code
;; below
      ; 'merged
      'single
      (list ;#(merged
	    ;  "Merged"
	    ;  "Display N-1 lines")
	    #(multi-line
	      "Multi-Line"
	      "Display N lines")
	    #(single
	      "Single"
	      "Display 1 line"))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Report Options" "Only positive Entries"
      "da" "Display only positive Entries?" #f))
    
    
    (let ((key-choice-list 
	   (list #(account
		   "Account (w/subtotal)"
		   "Sort & subtotal by account")
		 #(date
		   "Date"
		   "Sort by date")
		 #(date-monthly
		   "Date (subtotal monthly)"
		   "Sort by date & subtotal each month")
		 #(date-yearly
		   "Date (subtotal yearly)"
		   "Sort by date & subtotal each year")
		 #(time
		   "Time"
		   "Sort by exact entry time")
		 #(corresponding-acc
		   "Transfer from/to"
		   "Sort by account transferred from/to's name")
		 #(corresponding-acc-subtotal
		   "Transfer from/to (w/subtotal)"
		   "Sort and subtotal by account transferred from/to's name")
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
		   "Sort by memo")
		 #(none
		   "None"
		   "Do not sort"))))

      ;; primary sorting criterion
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	"Sorting" "Primary Key"
	"a" "Sort by this criterion first"
	'account
	key-choice-list))

      (gnc:register-trep-option
       (gnc:make-multichoice-option
	"Sorting" "Primary Sort Order"
	"b" "Order of primary sorting"
	'ascend
	(list
	 #(ascend "Ascending" "smallest to largest, earliest to latest")
	 #(descend "Descending" "largest to smallest, latest to earliest"))))

      (gnc:register-trep-option
       (gnc:make-multichoice-option
	"Sorting" "Secondary Key"
	"c"
	"Sort by this criterion second"
	'date
	key-choice-list))
      
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	"Sorting" "Secondary Sort Order"
	"d" "Order of Secondary sorting"
	'ascend
	(list
	 #(ascend "Ascending" "smallest to largest, earliest to latest")
	 #(descend "Descending" "largest to smallest, latest to earliest")))))
    
  
    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Date"
      "b" "Display the date?" #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Num"
      "c" "Display the cheque number?" #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Description"
      "d" "Display the description?" #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Memo"
      "f" "Display the memo?" #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Account"
      "g" "Display the account?" #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Other Account"
      "h" "Display the other account?  (if this is a split transaction, this parameter is guessed)." #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Shares"
      "ha" "Display the number of shares?" #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Price"
      "hb" "Display the shares price?" #f))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      "Display" "Amount"
      "i" "Display the amount?"  

      'single
      (list #(none "None" "No amount display")
	    #(single "Single" "Single Column Display")
	    #(double "Double" "Two Column Display"))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Headers" "j" "Display the headers?" #t))
 

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      "Display" "Totals"
      "k" "Display the totals?" #t))

    (gnc:options-set-default-section gnc:*transaction-report-options*
                                     "Report Options")

    gnc:*transaction-report-options*)


  (define (gnc:trep-renderer options)
    
     (let* ((begindate
             (gnc:date-option-absolute-time
              (gnc:option-value
               (gnc:lookup-option options "Report Options" "From"))))
            (enddate
             (gnc:date-option-absolute-time
              (gnc:option-value
               (gnc:lookup-option options "Report Options" "To"))))
            (tr-report-account-op
             (gnc:lookup-option options "Report Options" "Account"))
            (tr-report-primary-key-op
             (gnc:lookup-option options "Sorting" "Primary Key"))
            (tr-report-primary-order-op
             (gnc:lookup-option options "Sorting" "Primary Sort Order"))
            (tr-report-secondary-key-op
             (gnc:lookup-option options "Sorting" "Secondary Key"))
            (tr-report-secondary-order-op
             (gnc:lookup-option options "Sorting" "Secondary Sort Order"))
            (tr-report-style-op
             (gnc:lookup-option options "Report Options" "Style"))
            (accounts (gnc:option-value tr-report-account-op))
           (date-filter-pred (split-report-make-date-filter-predicate 
			      begindate 
			      (gnc:timepair-end-day-time
			       enddate)))
	   (filter-pred 
	    (if (gnc:option-value 
		 (gnc:lookup-option options "Report Options" 
				    "Only positive Entries")) 
		(lambda (split) 
		  (and (date-filter-pred split)
		       (> (gnc:split-get-sign-adjusted-value split) 0)))
		date-filter-pred))
	   (s1 (split-report-get-sort-spec-entry
		(gnc:option-value tr-report-primary-key-op)
		(eq? (gnc:option-value tr-report-primary-order-op) 'ascend)
		begindate))
	   (s2 (split-report-get-sort-spec-entry
		(gnc:option-value tr-report-secondary-key-op)
		(eq? (gnc:option-value tr-report-secondary-order-op) 'ascend)
		begindate))
	   (s2b (if s2 (list s2) '()))
	   (sort-specs (if s1 (cons s1 s2b) s2b))
 	   (split-list
 	    (apply
 	     append
 	     (map
 	      (lambda (account)
 		(make-split-list account filter-pred))
 	      accounts)))
 	   (split-report-specs (make-split-report-spec options)))
       
       (list
        (html-start-document-title (string-db 'lookup 'title) #f)
        (html-start-table)
        (if (gnc:option-value (gnc:lookup-option options "Display" "Headers"))
            (html-table-headers split-report-specs)
            '())

        (html-table-render-entries
         split-list
         split-report-specs
         sort-specs
         (case (gnc:option-value tr-report-style-op)
           ((multi-line)
            html-table-entry-render-entries-first)
           ((merged)
            html-table-entry-render-subentries-merged)
           ((single)
            html-table-entry-render-entries-only))
         (lambda (split)
           (length
            (gnc:split-get-other-splits split))))
        (if (gnc:option-value (gnc:lookup-option options "Display" "Totals"))
            (html-table-totals split-list split-report-specs)
            '())
        (html-end-table)
        (html-end-document))))


  (string-db 'store 'title "Transaction Report")
  (string-db 'store 'date-string "Date")
  (string-db 'store 'num-string "Num")
  (string-db 'store 'desc-string "Description")
  (string-db 'store 'memo-string "Memo")
  (string-db 'store 'acc-string "Account")
  (string-db 'store 'other-acc-string "Other Account")
  (string-db 'store 'shares-string "Shares")
  (string-db 'store 'price-string "Price")
  (string-db 'store 'amount-string "Amount")
  (string-db 'store 'debit-string "Debit")
  (string-db 'store 'credit-string "Credit")
  (string-db 'store 'total-string "Total")
  (string-db 'store 'open-bal-string "Opening Balance")

  (gnc:define-report
   'version 1
   'name (string-db 'lookup 'title)
   'options-generator trep-options-generator
   'renderer gnc:trep-renderer))
