;; -*-scheme-*-
;; $Id$
;; copied and modified from balance-and-pnl.scm
;; Tax Reports

;; This prints Tax related accounts.  For this to work, the user has to
;; segregate taxable and not taxable income to different accounts, as well
;; as deductible and non deductible expenses.
;; Tax related accounts have "{tax}" in the notes field.  This can be set/reset
;;   from the parameters dialog.
;; The user selects the accounts(s) to be printed, if none, all are checked.
;; Automatically prints up to 15 sub-account levels below selected account.
;;   Accounts below that are not printed. If you really need more levels,
;;   change the MAX_LEVELS constant
;; Optionally, does NOT print accounts with $0.00 values.
;; Prints data between the From and To dates.  Optional alternate periods:
;;   "Last Year", "1st Est Tax Quarter", ... "4th Est Tax Quarter"
;;   "Last Yr Est Tax Qtr", ... "Last Yr Est Tax Qtr"
;;   Estimated Tax Quarters: Dec 31, Mar 31, Jun 30, Aug 31)
;; NOTE: setting of specific dates is squirly! and seems to be current-date
;; dependabnt!  Actually, time of day dependant!
;; Just after midnight gives diffenent dates than just before!
;; Referencing all times to noon seems to fix this.
;; Subtracting 1 year sometimes subtracts 2!  see "(to-value" 
;; Optionally prints brief or full account names

;; made some changes to date options, as these changed since 1.4.4

(gnc:support "report/tax.scm")
(gnc:depend "text-export.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "options.scm")
(gnc:depend "date-utilities.scm")

;; make a list of accounts from a group pointer
(define (gnc:group-ptr->list group-prt)
  (if (pointer-token-null? group-prt)
      '()
      (gnc:group-map-accounts (lambda (x) x) group-prt)))

;; do loop string-search
(define (string-search string sub-str start)
  (do ((sub-len (string-length sub-str))
       ;; must recompute sub-len because order is unknown
       (limit (- (string-length string) (string-length sub-str)))
       (char0 (string-ref sub-str 0))
       ;; find first char of sub-str ; must recompute char0
       (match0 (string-index string (string-ref sub-str 0) start) ; init
	       (string-index string char0 (+ 1 match0))) ; step
       (match #f #f))
      ((or (eqv? #f match0) (> match0 limit)
	   ;; dows entire sub-str match?
	   (let ()
	     (set! match (string=? sub-str (substring string match0 
						      (+ match0 sub-len))))
	     (if match (set! match match0))
	     match))
       match)))

(define (string-search? string sub-str start)
  (number? (string-search string sub-str start)))

(define (make-level-collector num-levels)
  (let ((level-collector (make-vector num-levels)))
    (do ((i 0 (+ i 1)))
        ((= i num-levels) i)
      (vector-set! level-collector i (make-stats-collector)))
    level-collector))

;; Just a private scope.
(let* ((MAX-LEVELS 16)			; Maximum Account Levels
       (levelx-collector (make-level-collector MAX-LEVELS)))

  (define (lx-collector level action value)
    ((vector-ref levelx-collector (- level 1)) action value))

  (define string-db (gnc:make-string-database))

  ;; IRS asked congress to make the tax quarters sthe same as real quarters
  ;;   This is the year it is effective.  THIS IS A Y10K BUG!
  (define tax-qtr-real-qtr-year 10000)

  (define  (tax-options-generator)
    (define gnc:*tax-report-options* (gnc:new-options))
    (define (gnc:register-tax-option new-option)
      (gnc:register-option gnc:*tax-report-options* new-option))

    (gnc:register-tax-option
     (gnc:make-date-option
      "Tax Report Options" "From"
      "a" "Start of reporting period"
      (lambda ()
        (let ((bdtm (gnc:timepair->date (gnc:timepair-canonical-day-time
					 (cons (current-time) 0)))))
	  (set-tm:mday bdtm 1)  ; 01
          (set-tm:mon bdtm 0)   ; Jan
	  (cons 'absolute (cons (car (mktime bdtm)) 0))))
      #f 'absolute #f))
    
    (gnc:register-tax-option
     (gnc:make-date-option
      "Tax Report Options" "To"
      "b" "End of reporting period"
      (lambda ()
        (cons 'absolute (gnc:timepair-canonical-day-time
			 (cons (current-time) 0))))
      #f 'absolute #f))
    
    (gnc:register-tax-option
     (gnc:make-multichoice-option
      "Tax Report Options" "Alternate Period"
      "c" "Overide or modify From: & To:" 'from-to
      (list #(from-to "Use From - To" "Use From - To period")
            #(1st-est "1st Est Tax Quarter" "Jan 1 - Mar 31")
            #(2nd-est "2nd Est Tax Quarter" "Apr 1 - May 31")
            #(3rd-est "3rd Est Tax Quarter" "Jun 1 - Aug 31")
            #(4th-est "4th Est Tax Quarter" "Sep 1 - Dec 31")
            #(last-year "Last Year" "Last Year")
            #(1st-last "Last Yr 1st Est Tax Qtr" "Jan 1 - Mar 31, Last year")
            #(2nd-last "Last Yr 2nd Est Tax Qtr" "Apr 1 - May 31, Last year")
            #(3rd-last "Last Yr 3rd Est Tax Qtr" "Jun 1 - Aug 31, Last year")
            #(4th-last "Last Yr 4th Est Tax Qtr" "Sep 1 - Dec 31, Last year")
	    )))

    (gnc:register-tax-option
     (gnc:make-account-list-option
      "Tax Report Options" "Select Accounts (none = all)"
      "d" "Select accounts"
      (lambda () (gnc:get-current-accounts))
      #f
      #t))

    (gnc:register-tax-option
     (gnc:make-simple-boolean-option
      "Tax Report Options" "Suppress $0.00 values"
      "f" "$0.00 valued Accounts won't be printed." #t))

    (gnc:register-tax-option
     (gnc:make-simple-boolean-option
      "Tax Report Options" "Print Full account names"
      "g" "Print all Parent account names" #f))

    (gnc:register-tax-option
     (gnc:make-multichoice-option
      "Tax Report Options" "Set/Reset Tax Status"
      "h" "Set/Reset Selected Account Tax Status" 'tax-no-change
      (list #(tax-no-change "No Change" "No Change")
            #(tax-set "Set Tax Related" "Set Selected accounts as Tax Related")
            #(tax-reset "Reset Tax Related"
			"Reset Selected accounts as not Tax Related")
            #(tax-set-kids "Set Tax Related & sub-accounts" 
			   "Set Selected & sub-accounts as Tax Related")
            #(tax-reset-kids
	      "Reset Tax Related & sub-accounts"
	      "Reset Selected & sub-accounts as not Tax Related")
	    )))
    
    gnc:*tax-report-options*)

  ;; Render any level
  (define (render-level-x-account level max-level account lx-value
				  suppress-0 full-names)
    (let* ((indent-1 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
	   (indent-2 (string-append indent-1 indent-1))
	   (account-name ;(string-append
	    (if (or full-names (equal? level 1))
		(gnc:account-get-full-name account)
		(gnc:account-get-name account)))
	   (value (gnc:amount->formatted-string lx-value #f))
	   (account-name (do ((i 1 (+ i 1))
			      (accum account-name
				     (string-append indent-1 accum)))
			     ((>= i level) accum)))
           (nbsp-x-value (if (= max-level level)
			     (list value)
			     (append (vector->list (make-vector
						    (- max-level level)
						    "&nbsp;"))
				     (list value))))
	   (align-x (append (list "left")
	        	    (vector->list
			     (make-vector (- (+ max-level 1) level)
					  "right")))))
      ;;(if (not (equal? lx-value 0.0)) ; this fails, round off, I guess
      (if (or (not suppress-0) (= level 1)
              (not (equal? value (gnc:amount->formatted-string 0.0 #f))))
	  (html-table-row-align
	   (append (list account-name) nbsp-x-value)
	   align-x)
	  '())))

  (define blank-line
    (html-table-row (list "&nbsp;")))

  (define (is-type-income-or-expense? type)
    (member type '(INCOME EXPENSE)))

  (define (is-type-income? type)
    (member type '(INCOME)))

  (define tax-key "{tax}")

  (define (is-key-in-account-notes? key account)
    (string-search? (gnc:account-get-notes account) key 0))

  ;; This is a bit of a fudge, matching against strings in account notes.
  ;; It'd be better if this was a unique account field.
  ;; Recursivly validate children if parent is not a tax account.
  ;; Don't check children if parent is vaild, i.e., we assume all
  ;;   children are valid.
  ;; Returns the Parent if a child or grandchild is valid.
  (define (validate accounts key)
    (apply append (map (lambda (a)
			 (if (is-key-in-account-notes? key a)
			     (list a)
			     ;; check children
			     (if (null? (validate
					 (gnc:group-ptr->list
					  (gnc:account-get-children a)) key))
				 '()
				 (list a))))
		       accounts)))

  ;; Set or Reset key in account notes
  (define (key-status accounts set key kids)
    (let ((key-len (string-length key)))
      (map (lambda (a)
	     (let* ((notes (gnc:account-get-notes a))
		    (key-start (string-search notes key 0))
		    (notes-len (string-length notes)))
	       (if (eqv? #f key-start)
		   (if set		; set tax status
		       (gnc:account-set-notes a (string-append notes key)))
		   (if (not set)	; reset tax status
		       (gnc:account-set-notes a (string-append
						 (substring notes 0 key-start)
						 (substring notes
							    (+ key-start
							       key-len)
							    notes-len)))))
	       (if kids			; recurse to all sub accounta
		   (key-status 
		    (gnc:group-ptr->list (gnc:account-get-children a))
		    set key #t))))
	   accounts)))
  
  (define (generate-tax report-name
			report-description
			options)

    ;; These are some helper functions for looking up option values.
    (define (get-op section name)
      (gnc:lookup-option options section name))

    (define (op-value section name)
      (gnc:option-value (get-op section name)))

    ;; the number of account generations: children, grandchildren etc.
    (define (num-generations account gen)
      (let ((children (gnc:account-get-children account)))
	(if (pointer-token-null? children)
	    gen				; no kids, return input
	    (apply max (gnc:group-map-accounts
			(lambda (x) (num-generations x (+ 1 gen)))
			children)))))

    (let* ((from-value (gnc:date-option-absolute-time 
			(op-value "Tax Report Options" "From")))
           (to-value (gnc:timepair-end-day-time
		      (gnc:date-option-absolute-time 		       
		       (op-value "Tax Report Options" "To"))))
	   (alt-period (op-value "Tax Report Options" "Alternate Period"))
	   (suppress-0 (op-value "Tax Report Options" "Suppress $0.00 values"))
	   (full-names (op-value "Tax Report Options"
				 "Print Full account names"))
	   (tax-stat (op-value "Tax Report Options" "Set/Reset Tax Status"))
	   (user-sel-accnts (op-value "Tax Report Options"
				      "Select Accounts (none = all)"))
	   (not-used (case tax-stat
		       ((tax-set)
			(key-status user-sel-accnts #t tax-key #f)
			(gnc:refresh-main-window))
		       ((tax-reset)
			(key-status user-sel-accnts #f tax-key #f)
			(gnc:refresh-main-window))
		       ((tax-set-kids)
			(key-status user-sel-accnts #t tax-key #t)
			(gnc:refresh-main-window))
		       ((tax-reset-kids)
			(key-status user-sel-accnts #f tax-key #t)
			(gnc:refresh-main-window))))
	   (valid-user-sel-accnts (validate user-sel-accnts tax-key))
	   ;; If no selected accounts, check all.
	   (selected-accounts (if (not (null? user-sel-accnts))
				  valid-user-sel-accnts
				  (validate (gnc:group-ptr->list
					     (gnc:get-current-group))
					    tax-key)))
	   (generations (if (pair? selected-accounts)
			    (apply max (map (lambda (x) (num-generations x 1))
					    selected-accounts))
			    0))
	   (max-level (min MAX-LEVELS (max 1 generations)))
	   ;; Alternate dates are relative to from-date
	   (from-date (gnc:timepair->date from-value))
	   (from-value (gnc:timepair-start-day-time
			(let ((bdtm from-date))
			  (if (member alt-period 
				      '(last-year 1st-last 2nd-last
						  3rd-last 4th-last))
			      (set-tm:year bdtm (- (tm:year bdtm) 1)))
			  (set-tm:mday bdtm 1)
			  (if (< (gnc:date-get-year bdtm) 
				 tax-qtr-real-qtr-year)
			      (case alt-period
				((1st-est 1st-last last-year) ; Jan 1
				 (set-tm:mon bdtm 0))
				((2nd-est 2nd-last) ; Apr 1
				 (set-tm:mon bdtm 3))
				((3rd-est 3rd-last) ; Jun 1
				 (set-tm:mon bdtm 5))
				((4th-est 4th-last) ; Sep 1
				 (set-tm:mon bdtm 8)))
			      ;; Tax quaters equal Real quarters
			      (case alt-period
				((1st-est 1st-last last-year) ; Jan 1
				 (set-tm:mon bdtm 0))
				((2nd-est 2nd-last) ; Apr 1
				 (set-tm:mon bdtm 3))
				((3rd-est 3rd-last) ; Jul 1
				 (set-tm:mon bdtm 6))
				((4th-est 4th-last) ; Oct 1
				 (set-tm:mon bdtm 9))))
			  (cons (car (mktime bdtm)) 0))))
	   
	   (to-value (gnc:timepair-end-day-time
		      (let ((bdtm from-date))
			(if (member alt-period 
				    '(last-year 1st-last 2nd-last
						3rd-last 4th-last))
			    (set-tm:year bdtm (- (tm:year bdtm) 1)))
			;; Bug! Above subtracts two years, should only be one!
			;; The exact same code, in from-value, further above,
			;;   only subtraces one!  Go figure!
			;; So, we add one back below!
			(if (member alt-period 
				    '(last-year 1st-last 2nd-last
						3rd-last 4th-last))
			    (set-tm:year bdtm (+ (tm:year bdtm) 1)))
			(set-tm:mday bdtm 31)
			(if (< (gnc:date-get-year bdtm) tax-qtr-real-qtr-year)
			    (case alt-period
			      ((1st-est 1st-last) ; Mar 31
			       (set-tm:mon bdtm 2))
			      ((2nd-est 2nd-last) ; May 31
			       (set-tm:mon bdtm 4))
			      ((3rd-est 3rd-last) ; Aug 31
			       (set-tm:mon bdtm 7))
			      ((4th-est 4th-last last-year) ; Dec 31
			       (set-tm:mon bdtm 11))
			      (else (set! bdtm (gnc:timepair->date to-value))))
			    ;; Tax quaters equal Real quarters
			    (case alt-period
			      ((1st-est 1st-last) ; Mar 31
			       (set-tm:mon bdtm 2))
			      ((2nd-est 2nd-last) ; Jun 30
			       (set-tm:mday bdtm 30)
			       (set-tm:mon bdtm 5))
			      ((3rd-est 3rd-last) ; Sep 30
			       (set-tm:mday bdtm 30)
			       (set-tm:mon bdtm 8))
			      ((4th-est 4th-last last-year) ; Dec 31
			       (set-tm:mon bdtm 11))
			      (else 
			       (set! bdtm (gnc:timepair->date to-value)))))
			(cons (car (mktime bdtm)) 0))))
	   )
      
      (define (handle-level-x-account level account)
        (let ((type (gnc:account-type->symbol (gnc:account-get-type account)))
	      (name (gnc:account-get-name account)))
          (if (is-type-income-or-expense? type)
              (let* ((children (gnc:account-get-children account))
                     (childrens-output (gnc:group-map-accounts
                                        (lambda (x)
					  (if (>= max-level (+ 1 level))
					      (handle-level-x-account
					       (+ 1 level) x)))
					children))

                     (account-balance (if (is-key-in-account-notes? tax-key
								    account)
					  (gnc:account-get-balance-interval
					   account
					   from-value
					   to-value #f)
					  0))) ; don't add non tax related
		
                (set! account-balance (+ (if (> max-level level)
					     (lx-collector (+ 1 level)
							   'total #f)
					     0)
					 ;; make positive
					 (if (is-type-income? type)
					     (- account-balance )
					     account-balance)))
                (lx-collector level 'add account-balance)
                (let ((level-x-output
		       (render-level-x-account level max-level account
					       account-balance
					       suppress-0 full-names)))
                  (if (equal? 1 level)
		      (lx-collector 1 'reset #f))
                  (if (> max-level level)
		      (lx-collector (+ 1 level) 'reset #f))
                  (if (null? level-x-output)
		      '()
		      (if (null? childrens-output)
			  level-x-output
			  (list level-x-output
				childrens-output
				blank-line)))))
              ;; Ignore
              '())))

      (let
	  ((output '())
	   (from-date (strftime "%Y-%b-%d" (localtime (car from-value))))
	   (to-date (strftime "%Y-%b-%d" (localtime (car to-value)))))

	;; Now, the main body
	;; Reset all the balance collectors
	(do ((i 1 (+ i 1)))
	    ((> i MAX-LEVELS) i)
	  (lx-collector i 'reset #f))

	(set! output (list
		      (map (lambda (x) (handle-level-x-account 1 x))
			   selected-accounts)))
	
	(list
	 "<html>"
	 "<head>"
	 "<title>" report-name "</title>"
	 "</head>"
	 "<body bgcolor=#f6ffdb>"

	 "<center>"
	 "<p>" (string-db 'lookup 'tax-from) from-date
	 (string-db 'lookup 'tax-to) to-date "<br>"
	 "</center>"
	 "<table cellpadding=1>"
	 "<caption><b>" report-name "</b></caption>"
	 "<tr>"
	 "<th>" (string-db 'lookup 'account-name) "</th>"

	 (do ((i (- max-level 1) (- i 1))
	      (head "" (string-append head 
				      "<th align=right>"
				      (string-db 'lookup 'sub)
				      (number->string i)
				      ")</th>")))
	     ((< i 1) head))
	 "<th align=right>" (string-db 'lookup 'balance) "</th>"
	 "</tr>"
	 output
	 "</table>"
	 (if (null? (car output))
	     (string-append "<p><b>" (string-db 'lookup 'no-tax) "</b></p>")
	     " ")
	 "</body>"
	 "</html>"))))
  
  (string-db 'store 'net "Net")
  (string-db 'store 'account-name "Account Name")
  (string-db 'store 'no-tax "No Tax Related accounts were found. Click \
\"Parameters\" to set some  with the \"Set/Reset Tax Status:\" parameter.") 
  (string-db 'store 'sub "(Sub ")
  (string-db 'store 'balance "Total")
  (string-db 'store 'tax-title "Taxable / Deductable")
  (string-db 'store 'tax-from "Period From: ")
  (string-db 'store 'tax-to "  To: ")
  (string-db 'store 'tax-desc "This page shows your Taxable Income and Deductable Expenses.")

  (gnc:define-report
   'version 1
   'name "Tax"
   'options-generator tax-options-generator
   'renderer (lambda (options)
               (generate-tax 
                (string-db 'lookup 'tax-title)
                (string-db 'lookup 'tax-desc)
                options))))
