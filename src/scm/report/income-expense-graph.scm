;; -*-scheme-*-

;; income-expense-graph.scm
;; Display a simple time series for graphs
;; by Robert Merkel (rgmerk@mira.net)

(gnc:support "report/income-expense-graph.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ()

  (define (options-generator)    
    (let* ((options (gnc:new-options)) 
           ;; This is just a helper function for making options.
           ;; See gnucash/src/scm/options.scm for details.
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
	  (filter
	   gnc:account-is-inc-exp?
	   (gnc:group-get-subaccounts (gnc:get-current-group))))
	gnc:account-is-inc-exp?
	#t))

      (add-option
       (gnc:make-currency-option
	"Report Options"
	"Report Currency"
	"c"
	"Select the display value for the currency"
	(gnc:locale-default-currency)))

     (add-option
       (gnc:make-multichoice-option
        (N_ "Report Options") (N_ "Step Size")
        "e" (N_ "The amount of time between data points") 'MonthDelta
        (list #(WeekDelta  "Week" "Week")
	      #(TwoWeekDelta "Two Week" "Two Weeks")
	      #(MonthDelta "Month" "Month")
	      #(QuarterDelta "Quarter" "Quarter")
              #(YearDelta "Year" "Year")
              )))      

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
  
  ;; This is the rendering function. It accepts a database of options
  ;; and generates an object of type <html-document>.  See the file
  ;; report-html.txt for documentation; the file report-html.scm
  ;; includes all the relevant Scheme code. The option database passed
  ;; to the function is one created by the options-generator function
  ;; defined above.
  (define (inc-exp-graph-renderer report-obj)

    ;; These are some helper functions for looking up option values.
    (define (get-op section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))
    
    (define (op-value section name)
      (gnc:option-value (get-op section name)))
    
    (let* ((report-currency (op-value "Report Options" "Report Currency"))
	   (height (op-value "Display Format" "Plot Height"))
	   (width (op-value "Display Format" "Plot Width"))
	   (accounts (op-value "Report Options" "Accounts"))
	   (to-date-tp (gnc:timepair-end-day-time 
			(vector-ref (op-value "Report Options"
					      "To") 1)))
	   (from-date-tp (gnc:timepair-start-day-time 
			  (vector-ref (op-value "Report Options"
						"From") 1)))
	   (interval (op-value "Report Options" "Step Size"))
	   (document (gnc:make-html-document))
	   (chart (gnc:make-html-barchart))
	   (exchange-alist (gnc:make-exchange-alist
			    report-currency to-date-tp))
	   (exchange-fn-internal (gnc:make-exchange-function exchange-alist))
	   (exchange-fn (lambda (foriegn)
                          (exchange-fn-internal foriegn report-currency)))
	   (dates-list (gnc:dateloop
                        (gnc:timepair-start-day-time from-date-tp) 
                        (gnc:timepair-end-day-time 
                         (decdate to-date-tp DayDelta))
                        (eval interval)))
	   (profit-collector-fn
	    (lambda (date-list-entry)
	      (let ((start-date (car date-list-entry))
		    (end-date (cadr date-list-entry)))
		(gnc:accounts-get-comm-total-profit
                 accounts 
                 (lambda (account)
                   (gnc:account-get-comm-balance-interval
                    account
                    start-date
                    end-date
                    #f))))))
	   (profit-collector-list
	    (map profit-collector-fn dates-list))
	   (double-list
	    (map (lambda (commodity-collector)
		   (- (gnc:numeric-to-double 
                       (cadr (commodity-collector 'getpair
                                                  report-currency #t)))))
		 profit-collector-list))
	   (date-string-list
	    (map (lambda (date-list-item)
		   (gnc:timepair-to-datestring
		    (car date-list-item)))
		 dates-list)))

      (gnc:html-barchart-set-title! chart (N_ "Income/Expense Chart"))
      (gnc:html-barchart-set-subtitle!
       chart (sprintf #f
                      (_ "%s to %s")
                      (gnc:timepair-to-datestring from-date-tp) 
                      (gnc:timepair-to-datestring to-date-tp)))
      (gnc:html-barchart-set-width! chart width)
      (gnc:html-barchart-set-height! chart height)
      (gnc:html-barchart-append-column! chart double-list)
      (gnc:html-barchart-set-row-labels! chart date-string-list)
      (gnc:html-barchart-set-row-labels-rotated?! chart #t)
      (gnc:html-barchart-set-col-labels! chart (list (_ "Net Profit")))
      (gnc:html-barchart-set-col-colors! chart (list "red"))
      (gnc:html-barchart-set-y-axis-label!
       chart (gnc:commodity-get-mnemonic report-currency))
      (gnc:html-document-add-object! document chart) 

;      (gnc:html-document-add-object! 
;       document ;;(gnc:html-markup-p
;       (gnc:html-make-exchangerates 
;	report-currency exchange-alist accounts #f))

      document))

  ;; Here we define the actual report with gnc:define-report
  (gnc:define-report

   ;; The version of this report.
   'version 1

   ;; The name of this report. This will be used, among other things,
   ;; for making its menu item in the main menu. You need to use the
   ;; untranslated value here!
   'name (N_ "Income/Expense Graph")

   ;; The options generator function defined above.
   'options-generator options-generator

   ;; The rendering function defined above.
   'renderer inc-exp-graph-renderer))
