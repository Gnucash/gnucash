;; -*-scheme-*-

;; income-or-expense-pie.scm
;; Display expenses/incomes from various accounts as a pie chart
;; by Robert Merkel (rgmerk@mira.net)



(gnc:support "report/income-or-expense-pie.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")


(let ()
  
  ;; Note the options-generator has a boolean argument, which
  ;; is true for income piecharts.  We use a lambda to wrap
  ;; up this function in the define-reports.

  (define (options-generator is-income?)    
    (let* ((options (gnc:new-options)) 
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      (gnc:options-add-date-interval!
       options "Report Options" 
       (N_ "From") (N_ "To")
       "d")
 
      (add-option
       (gnc:make-account-list-option
	(N_ "Report Options") (N_ "Accounts")
	"b"
	"Select accounts to calculate income on"
	(lambda ()
	  (gnc:filter-accountlist-type 
	   (if is-income? '(income) '(expense))
	   (gnc:group-get-account-list (gnc:get-current-group))))
	(lambda (account)
	  (let ((type (gw:enum-<gnc:AccountType>-val->sym
		       (gnc:account-type account)
		       #f)))
	    (member type (if is-income? '(income) '(expense)))))
	#t))

      (add-option
       (gnc:make-currency-option
	"Report Options"
	"Report Currency"
	"c"
	"Select the display value for the currency"
	(gnc:locale-default-currency)))

      (add-option
       (gnc:make-number-range-option
        (N_ "Display Format") (N_ "Plot Width")
        "a" (N_ "Width of plot in pixels.") 400
        100 1000 0 1))

      (add-option
       (gnc:make-number-range-option
        (N_ "Display Format") (N_ "Plot Height")
        "b" (N_ "Height of plot in pixels.") 400
        100 1000 0 1))
      (gnc:options-set-default-section options "Report Options")      
      options))
  
  ;; Similar arrangement to the options-generator.
  (define (income-or-expense-pie-renderer report-obj is-income?)
    
    
    ;; These are some helper functions for looking up option values.
    (define (get-op section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))
    
    (define (op-value section name)
      (gnc:option-value (get-op section name)))
    
    (let* ( 
	   (report-currency (op-value "Report Options" "Report Currency"))
	   (height (op-value "Display Format" "Plot Height"))
	   (width (op-value "Display Format" "Plot Width"))
	   (accounts (op-value "Report Options" "Accounts"))
	   (to-date-tp (gnc:timepair-end-day-time 
			(vector-ref (op-value "Report Options"
					      "To") 1)))
	   (from-date-tp (gnc:timepair-start-day-time 
			  (vector-ref (op-value "Report Options"
						"From") 1)))
	   (document (gnc:make-html-document))
	   (chart (gnc:make-html-piechart))
	   (exchange-alist (gnc:make-exchange-alist
			    report-currency to-date-tp))
	   (exchange-fn-internal (gnc:make-exchange-function exchange-alist))
	   (profit-collector-fn
	    (lambda (account)
	      (gnc:account-get-comm-balance-interval
	       account
	       from-date-tp
	       to-date-tp
	       #t)))
	   (profit-collector-list
	    (map profit-collector-fn accounts))
           

            ;;; FIXME: better currency handling here

	   (double-list
	    (map (lambda (commodity-collector)
		   (abs (gnc:numeric-to-double 
		    (cadr (commodity-collector 'getpair report-currency #t)))))
		 profit-collector-list))
	   (account-name-list (map gnc:account-get-name accounts)))
      (gnc:warn "account-name-list" account-name-list)
      

      (gnc:html-piechart-set-title! chart (if is-income? 
					      (N_ "Income by Account")
					      (N_ "Expenses by Account")))
      (gnc:html-piechart-set-subtitle! chart (string-append 
					      (gnc:timepair-to-datestring from-date-tp) 
					      " " (N_ "to") " " 
					      (gnc:timepair-to-datestring to-date-tp)))
      (gnc:html-piechart-set-width! chart width)
      (gnc:html-piechart-set-height! chart height)
      (gnc:html-piechart-set-data! chart double-list)
      (gnc:html-piechart-set-labels! chart account-name-list)

      (gnc:html-document-add-object! document chart) 


      document))
     
  
  (gnc:define-report
   
   'version 1

   'name (N_ "Income Breakdown Piechart")

   'options-generator (lambda () (options-generator #t))
   'renderer (lambda (report-obj) (income-or-expense-pie-renderer report-obj #t)))

  (gnc:define-report
   'version 1
   'name (N_ "Expense Breakdown Piechart")
   'options-generator (lambda () (options-generator #f))
   'renderer (lambda (report-obj) (income-or-expense-pie-renderer report-obj #f))))
