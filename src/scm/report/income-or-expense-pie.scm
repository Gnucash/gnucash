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

      (add-option
       (gnc:make-number-range-option
        (N_ "Report Options") (N_ "Maximum Slices")
        "a" (N_ "Maximum number of slices in pie") 7
        2 20 0 1))

      (add-option
       (gnc:make-account-list-option
	(N_ "Report Options") (N_ "Accounts")
	"b"
	(N_ "Select accounts to calculate income on")
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
      
      (add-option
       (gnc:make-currency-option
	(N_ "Report Options") (N_ "Report Currency")
	"c"
	(N_ "Select the display value for the currency")
	(gnc:locale-default-currency)))

      (gnc:options-add-date-interval!
       options "Report Options" 
       (N_ "From") (N_ "To")
       "d")

      (add-option
       (gnc:make-number-range-option
        (N_ "Display Format") (N_ "Plot Width")
        "a" (N_ "Width of plot in pixels.") 500
        100 1000 0 1))

      (add-option
       (gnc:make-number-range-option
        (N_ "Display Format") (N_ "Plot Height")
        "b" (N_ "Height of plot in pixels.") 250
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

    (let* ((max-slices (op-value "Report Options" "Maximum Slices"))
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
	       #f)))
	   (profit-collector-list
	    (map profit-collector-fn accounts))

            ;;; FIXME: better currency handling here

	   (double-list
	    (map (lambda (commodity-collector)
		   (abs (gnc:numeric-to-double 
                         (cadr (commodity-collector 'getpair
                                                    report-currency #t)))))
		 profit-collector-list))
           (combined (zip double-list accounts))
           (accounts-or-names '()))

      (set! combined
            (filter (lambda (pair) (not (= 0.0 (car pair))))
                    combined))

      (set! combined
            (sort combined
                  (lambda (a b) (> (car a) (car b)))))

      (if (> (length combined) max-slices)
          (let* ((start (take combined (- max-slices 1)))
                 (finish (drop combined (- max-slices 1)))
                 (sum (apply + (unzip1 finish))))
            (set! combined
                  (append start
                          (list (list sum (_ "Other")))))))

      (call-with-values (lambda () (unzip2 combined))
                        (lambda (ds as)
                          (set! double-list ds)
                          (set! accounts-or-names as)))

      (gnc:html-piechart-set-title!
       chart (if is-income? 
                 (N_ "Income by Account")
                 (N_ "Expenses by Account")))

      (gnc:html-piechart-set-subtitle!
       chart (sprintf #f
                      (_ "%s to %s")
                      (gnc:timepair-to-datestring from-date-tp) 
                      (gnc:timepair-to-datestring to-date-tp)))

      (gnc:html-piechart-set-width! chart width)
      (gnc:html-piechart-set-height! chart height)
      (gnc:html-piechart-set-data! chart double-list)
      (gnc:html-piechart-set-labels!
       chart
       (map (lambda (a) (if (string? a) a (gnc:account-get-full-name a)))
            accounts-or-names))
      (gnc:html-piechart-set-colors! chart
                                     (gnc:assign-colors (length combined)))
      (let ((urls (map (lambda (a)
                         (if (string? a) "" (gnc:account-anchor-text a)))
                       accounts-or-names)))
        (gnc:html-piechart-set-button-1-slice-urls! chart urls)
        (gnc:html-piechart-set-button-1-legend-urls! chart urls))

      (gnc:html-document-add-object! document chart) 

      document))

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
