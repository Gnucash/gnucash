;; -*-scheme-*-

;; income-or-expense-pie.scm
;; Display expenses/incomes from various accounts as a pie chart
;; by Robert Merkel (rgmerk@mira.net)

(gnc:support "report/income-or-expense-pie.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ((pagename-general (N_ "General"))
      (optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-accounts (N_ "Accounts"))
      (optname-levels (N_ "Show Accounts until level"))
      (optname-report-currency (N_ "Report's currency"))

      (pagename-display (N_ "Display Format"))
      (optname-fullname (N_ "Show long account names"))
      (optname-show-total (N_ "Show Totals"))
      (optname-slices (N_ "Maximum Slices"))
      (optname-plot-width (N_ "Plot Width"))
      (optname-plot-height (N_ "Plot Height")))

  ;; Note the options-generator has a boolean argument, which
  ;; is true for income piecharts.  We use a lambda to wrap
  ;; up this function in the define-reports.

  (define (options-generator is-income?)    
    (let* ((options (gnc:new-options)) 
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      (gnc:options-add-date-interval!
       options pagename-general
       optname-from-date optname-to-date "a")

      (add-option
       (gnc:make-account-list-option
	pagename-general optname-accounts
	"b"
	(_ "Select accounts to calculate income on")
	(lambda ()
	  (gnc:filter-accountlist-type 
	   (if is-income? '(income) '(expense))
	   (gnc:group-get-subaccounts (gnc:get-current-group))))
	(lambda (accounts)
	  (list #t
		(gnc:filter-accountlist-type
		 (if is-income? '(income) '(expense))
		 accounts)))
	#t))

      (gnc:options-add-account-levels! 
       options pagename-general optname-levels "c" 
       (_ "Show accounts to this depth and not further") 
       2)

      (add-option
       (gnc:make-currency-option
	pagename-general optname-report-currency
	"d"
	(_ "Select the display value for the currency")
	(gnc:option-value
	 (gnc:lookup-global-option "International"
				   "Default Currency"))))

      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-fullname
        "a" (_ "Show the full account name in legend?") #f))

      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-show-total
        "b" (_ "Show the total balance in legend?") #t))

      (add-option
       (gnc:make-number-range-option
        pagename-display optname-slices
        "c" (N_ "Maximum number of slices in pie") 7
        2 24 0 1))

      (add-option
       (gnc:make-number-range-option
        pagename-display optname-plot-width 
        "d" (N_ "Width of plot in pixels.") 500
        100 1000 0 1))

      (add-option
       (gnc:make-number-range-option
        pagename-display optname-plot-height
        "e" (N_ "Height of plot in pixels.") 250
        100 1000 0 1))

      (gnc:options-set-default-section options pagename-general)      

      options))

  ;; Similar arrangement to the options-generator.
  (define (income-or-expense-pie-renderer report-obj is-income?)

    ;; These are some helper functions for looking up option values.
    (define (get-op section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))

    (define (op-value section name)
      (gnc:option-value (get-op section name)))

    ;; Get all options
    (let ((to-date-tp (gnc:timepair-end-day-time 
		       (vector-ref (op-value pagename-general
					     optname-to-date) 1)))
	  (from-date-tp (gnc:timepair-start-day-time 
			 (vector-ref (op-value pagename-general
					       optname-from-date) 1)))
	  (accounts (op-value pagename-general optname-accounts))
	  (account-levels (op-value pagename-general optname-levels))
	  (report-currency (op-value pagename-general
				     optname-report-currency))

	  (show-fullname? (op-value pagename-display optname-fullname))
	  (show-total? (op-value pagename-display optname-show-total))
	  (max-slices (op-value pagename-display optname-slices))
	  (height (op-value pagename-display optname-plot-height))
	  (width (op-value pagename-display optname-plot-width))

	  (document (gnc:make-html-document))
	  (chart (gnc:make-html-piechart))
	  (topl-accounts (gnc:filter-accountlist-type 
			  (if is-income? '(income) '(expense))
			  (gnc:group-get-account-list 
			   (gnc:get-current-group)))))

      ;; Returns true if the account a was selected in the account
      ;; selection option.
      (define (show-acct? a)
	(member a accounts))
      
      ;; Calculates the net balance (profit or loss) of an account
      ;; over the selected reporting period. If subaccts? == #t, all
      ;; subaccount's balances are included as well. Returns a
      ;; commodity-collector.
      (define (profit-fn account subaccts?)
	(gnc:account-get-comm-balance-interval
	 account from-date-tp to-date-tp subaccts?))
      
      ;; Define more helper variables.
      (let* ((exchange-alist (gnc:make-exchange-alist
			      report-currency to-date-tp))
	     (exchange-fn-internal 
	      (gnc:make-exchange-function exchange-alist))
	     (tree-depth (if (equal? account-levels 'all)
			     (gnc:get-current-group-depth)
			     account-levels))
	     (combined '())
	     (other-anchor "")
	     (print-info (gnc:commodity-print-info report-currency #t)))

	;; Converts a commodity-collector into one single double
	;; number, depending on the report currency and the
	;; exchange-alist calculated above. Returns the absolute value
	;; as double.
	(define (collector->double c)
	  ;; Future improvement: Let the user choose which kind of
	  ;; currency combining she want to be done. Right now
	  ;; everything foreign gets converted
	  ;; (gnc:sum-collector-commodity) based on the weighted
	  ;; average of all past transactions.
	  (abs (gnc:numeric-to-double 
		(gnc:gnc-monetary-amount
		 (gnc:sum-collector-commodity 
		  c report-currency 
		  exchange-fn-internal)))))

	;; Calculates all account's balances. Returns a list of
	;; balance <=> account pairs, like '((10.0 Earnings) (142.5
	;; Gifts)). If current-depth >= tree-depth, then the balances
	;; are calculated *with* subaccount's balances. Else only the
	;; current account is regarded. Note: All accounts in accts
	;; and all their subaccounts are processed, but a balances is
	;; calculated and returned *only* for those accounts where
	;; show-acct? is true. This is necessary because otherwise we
	;; would forget an account that is selected but not its
	;; parent.
	(define (traverse-accounts current-depth accts)
	  (if (< current-depth tree-depth)
	      (let ((res '()))
		(for-each
		 (lambda (a)
		   (begin
		     (if (show-acct? a)
			 (set! res (cons (list (collector->double 
						(profit-fn a #f)) a)
					 res)))
		     (set! res (append
				(traverse-accounts
				 (+ 1 current-depth)
				 (gnc:account-get-immediate-subaccounts a))
				res))))
		 accts)
		res)
	      (map
	       (lambda (a)
		 (list (collector->double (profit-fn a #t)) a))
	       (filter show-acct? accts))))
	
	(set! combined
	      (sort (filter (lambda (pair) (not (= 0.0 (car pair))))
			    (traverse-accounts 
			     1 topl-accounts))
		    (lambda (a b) (> (car a) (car b)))))

	;; if too many slices, condense them to an 'other' slice
	;; and add a link to a new pie report with just those
	;; accounts
	(if (> (length combined) max-slices)
	    (let* ((start (take combined (- max-slices 1)))
		   (finish (drop combined (- max-slices 1)))
		   (sum (apply + (unzip1 finish))))
	      (set! combined
		    (append start
			    (list (list sum (_ "Other")))))
	      (let* ((name (if is-income?
                               (N_ "Income Piechart") 
			       (N_ "Expense Piechart")))
		     (options (gnc:make-report-options name))
		     (account-op (gnc:lookup-option options
						    pagename-general
						    optname-accounts))
		     (level-op (gnc:lookup-option options
						  pagename-general
						  optname-levels)))
		(call-with-values (lambda () (unzip2 finish))
				  (lambda (ds as)
				    (gnc:option-set-value account-op as)))
		(gnc:option-set-value level-op account-levels)
		(set! other-anchor
		      (gnc:report-anchor-text
		       (gnc:make-report name options))))))

	(gnc:html-piechart-set-title!
	 chart (if is-income? 
		   (_ "Income by Account")
		   (_ "Expenses by Account")))

	(gnc:html-piechart-set-subtitle!
	 chart (string-append
                (sprintf #f
                         (_ "%s to %s")
                         (gnc:timepair-to-datestring from-date-tp) 
                         (gnc:timepair-to-datestring to-date-tp))
                (if show-total?
                    (let ((total (apply + (unzip1 combined))))
                      (sprintf #f ": %s"
                               (gnc:amount->string total print-info)))
                          
                    "")))

	(gnc:html-piechart-set-width! chart width)
	(gnc:html-piechart-set-height! chart height)
	(gnc:html-piechart-set-data! chart (unzip1 combined))
	(gnc:html-piechart-set-labels!
	 chart
	 (map (lambda (pair)
		(string-append
		 (if (string? (cadr pair))
		     (cadr pair)
		     ((if show-fullname?
			  gnc:account-get-full-name
			  gnc:account-get-name) (cadr pair)))
		 (if show-total?
		     (string-append 
		      " - "
		      (gnc:amount->string (car pair) print-info))
		     "")))
	      combined))
	(gnc:html-piechart-set-colors! chart
				       (gnc:assign-colors (length combined)))
	(let ((urls (map (lambda (pair)
			   (if (string? (cadr pair))
			       other-anchor
			       (gnc:account-anchor-text (cadr pair))))
			 combined)))
	  (gnc:html-piechart-set-button-1-slice-urls! chart urls)
	  (gnc:html-piechart-set-button-1-legend-urls! chart urls))

	(gnc:html-document-add-object! document chart) 

	document)))

  (gnc:define-report
   'version 1
   'name (N_ "Income Piechart")
   'options-generator (lambda () (options-generator #t))
   'renderer (lambda (report-obj)
               (income-or-expense-pie-renderer report-obj #t)))

  (gnc:define-report
   'version 1
   'name (N_ "Expense Piechart")
   'options-generator (lambda () (options-generator #f))
   'renderer (lambda (report-obj)
               (income-or-expense-pie-renderer report-obj #f))))
