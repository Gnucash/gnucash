;; -*-scheme-*-

;; income-expense-graph.scm
;; Display a simple time series for graphs
;; by Robert Merkel (rgmerk@mira.net)

(gnc:support "report/income-expense-graph.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ((pagename-general (N_ "General"))
      (optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-accounts (N_ "Accounts"))
      (optname-stepsize (N_ "Step Size"))
      (optname-report-currency (N_ "Report's currency"))

      (pagename-display (N_ "Display"))
      (optname-inc-exp (N_ "Show Income/Expense"))
      (optname-show-profit (N_ "Show Net Profit"))
      (optname-stacked (N_ "Use Stacked Bars"))
      (optname-plot-width (N_ "Plot Width"))
      (optname-plot-height (N_ "Plot Height")))

  (define (options-generator)    
    (let* ((options (gnc:new-options)) 
           ;; This is just a helper function for making options.
           ;; See gnucash/src/scm/options.scm for details.
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      (gnc:options-add-date-interval!
       options pagename-general
       optname-from-date optname-to-date "a")

      (gnc:options-add-interval-choice! 
       options pagename-general optname-stepsize "b" 'MonthDelta)

      (add-option
       (gnc:make-account-list-option
	pagename-general optname-accounts
	"c"
	(N_ "Report on these accounts, if chosen account level allows.")
	(lambda ()
	  (filter
	   gnc:account-is-inc-exp?
	   (gnc:group-get-subaccounts (gnc:get-current-group))))
	(lambda (accounts)
	  (list #t
		(filter gnc:account-is-inc-exp? accounts)))
	#t))

      (gnc:options-add-currency! 
       options pagename-general optname-report-currency "d")
      
      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-inc-exp
        "a" (N_ "Show Income and Expenses?") #t))

      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-show-profit
        "b" (N_ "Show the net profit?") #f))

      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-stacked
        "ba" (N_ "Show barchart as stacked barchart?") #f))

      (gnc:options-add-plot-size! 
       options pagename-display 
       optname-plot-width optname-plot-height "c" 400 400)

      (gnc:options-set-default-section options pagename-general)

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

    (define (collector-fn accounts income?)
      (lambda (date-list-entry)
        (let ((start-date (car date-list-entry))
              (end-date (cadr date-list-entry)))
          ((if income?
               gnc:accounts-get-comm-total-income
               gnc:accounts-get-comm-total-expense)
           accounts 
           (lambda (account)
             (gnc:account-get-comm-balance-interval account
                                                    start-date
                                                    end-date
                                                    #f))))))

    (define (collector-to-double-fn report-currency exchange-fn)
      (lambda (commodity-collector)
        (gnc:numeric-to-double
         (gnc:gnc-monetary-amount
          (gnc:sum-collector-commodity commodity-collector
                                       report-currency 
                                       exchange-fn)))))

    (let* ((to-date-tp (gnc:timepair-end-day-time 
			(vector-ref (op-value pagename-general
					      optname-to-date) 1)))
	   (from-date-tp (gnc:timepair-start-day-time 
			  (vector-ref (op-value pagename-general
						optname-from-date) 1)))
	   (interval (op-value pagename-general optname-stepsize))
	   (accounts (op-value pagename-general optname-accounts))
           (report-currency (op-value pagename-general
                                      optname-report-currency))

	   (show-net? (op-value pagename-display optname-show-profit))
	   (show-incexp? (op-value pagename-display optname-inc-exp))
	   (stacked? (op-value pagename-display optname-stacked))
	   (height (op-value pagename-display optname-plot-height))
	   (width (op-value pagename-display optname-plot-width))

	   (document (gnc:make-html-document))
	   (chart (gnc:make-html-barchart))
	   (exchange-alist (gnc:make-exchange-alist
			    report-currency to-date-tp))
	   (exchange-fn-internal (gnc:make-exchange-function exchange-alist))
	   (exchange-fn (lambda (foreign)
                          (exchange-fn-internal foreign report-currency)))
	   (dates-list (gnc:dateloop 
                        (gnc:timepair-start-day-time from-date-tp) 
                        (gnc:timepair-end-day-time to-date-tp)
                        (eval interval)))
           (income-collector-fn (collector-fn accounts #t))
           (expense-collector-fn  (collector-fn accounts #f))
	   (income-collector-list (map income-collector-fn dates-list))
	   (expense-collector-list (map expense-collector-fn dates-list))
	   (income-list
            (map (collector-to-double-fn report-currency exchange-fn-internal)
                 income-collector-list))
           (expense-list
            (map (collector-to-double-fn report-currency exchange-fn-internal)
                 expense-collector-list))
	   (date-string-list
	    (map (lambda (date-list-item)
		   (gnc:timepair-to-datestring
		    (car date-list-item)))
		 dates-list)))

      (gnc:html-barchart-set-title! chart (_ "Income/Expense Chart"))
      (gnc:html-barchart-set-subtitle!
       chart (sprintf #f
                      (_ "%s to %s")
                      (gnc:timepair-to-datestring from-date-tp) 
                      (gnc:timepair-to-datestring to-date-tp)))
      (gnc:html-barchart-set-width! chart width)
      (gnc:html-barchart-set-height! chart height)
      (gnc:html-barchart-set-row-labels! chart date-string-list)
      (gnc:html-barchart-set-y-axis-label!
       chart (gnc:commodity-get-mnemonic report-currency))
      (gnc:html-barchart-set-row-labels-rotated?! chart #t)
      (gnc:html-barchart-set-stacked?! chart stacked?)

      (if show-incexp?
          (begin
            (gnc:html-barchart-append-column! chart income-list)
            (gnc:html-barchart-append-column! chart (map - expense-list))))
      (if show-net?
	  (gnc:html-barchart-append-column! 
	   chart (map + income-list expense-list)))
      (gnc:html-barchart-set-col-labels! 
       chart (append
	      (if show-incexp?
		  (list (_ "Income") (_ "Expense")) '())
	      (if show-net?
		  (list (_ "Net Profit")) '())))
      (gnc:html-barchart-set-col-colors! 
       chart (append
	      (if show-incexp?
		  '("blue" "red") '())
	      (if show-net?
		  '("green") '())))
      
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
   'name (N_ "Income/Expense Chart")

   ;; The options generator function defined above.
   'options-generator options-generator

   ;; The rendering function defined above.
   'renderer inc-exp-graph-renderer))
