;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average-balance.scm
;; Report history of account balance and other info
;;
;; Author makes no implicit or explicit guarantee of accuracy of 
;;  these calculations and accepts no responsibility for direct
;;  or indirect losses incurred as a result of using this software.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/average-balance.scm")

(gnc:depend "report-html.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "date-utilities.scm")

(let ((optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-stepsize (N_ "Step Size"))
      (optname-report-currency (N_ "Report's currency"))
      (optname-price-source (N_ "Price Source"))
      (optname-subacct (N_ "Include Sub-Accounts")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Options
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (options-generator)
    (let* ((options (gnc:new-options))
           ;; register a configuration option for the report
           (register-option
            (lambda (new-option)
              (gnc:register-option options new-option))))      

      ;; General tab
      (gnc:options-add-date-interval!
       options gnc:pagename-general optname-from-date optname-to-date "a")

      (gnc:options-add-interval-choice! 
       options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

      ;; Report currency
      (gnc:options-add-currency! 
       options gnc:pagename-general optname-report-currency "c")
      
      (gnc:options-add-price-source! 
       options gnc:pagename-general
       optname-price-source "d" 'pricedb-latest)

      ;; Account tab
      (register-option
       (gnc:make-simple-boolean-option
        gnc:pagename-accounts optname-subacct
        "a" (N_ "Include sub-accounts of all selected accounts") #t))

      ;; account(s) to do report on
      (register-option
       (gnc:make-account-list-option
        gnc:pagename-accounts (N_ "Accounts")
        "b" (N_ "Do transaction report on this account")
        (lambda ()
          ;; FIXME : gnc:get-current-accounts disappeared
          (let ((current-accounts '()))
	    ;; If some accounts were selected, use those
            (cond ((not (null? current-accounts)) 
		   current-accounts)
                  (else
		   ;; otherwise get some accounts -- here as an
		   ;; example we get the asset and liability stuff
                   (gnc:filter-accountlist-type
		    '(bank cash credit asset liability) 
		    ;; or: '(bank cash checking savings stock
		    ;; mutual-fund money-market)
		    (gnc:group-get-account-list (gnc:get-current-group)))))))
        #f #t))

      ;; Display tab
      (register-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Show table")
        "a" (N_ "Display a table of the selected data.") #f))

      (register-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Show plot")
        "b" (N_ "Display a graph of the selected data.") #t))

      (register-option
       (gnc:make-list-option
        gnc:pagename-display (N_ "Plot Type")
        "c" (N_ "The type of graph to generate") (list 'AvgBalPlot)
        (list 
	 (vector 'AvgBalPlot (N_ "Average") (N_ "Average Balance"))
	 (vector 'GainPlot (N_ "Profit") (N_ "Profit (Gain minus Loss)"))
	 (vector 'GLPlot (N_ "Gain/Loss") (N_ "Gain And Loss")))))

      (gnc:options-add-plot-size! 
       options gnc:pagename-display (N_ "Plot Width") (N_ "Plot Height")
       "d" 400 400)

      ;; Set the general page as default option tab
      (gnc:options-set-default-section options gnc:pagename-general)      
      
      options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some utilities for generating the data 
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define columns
    ;; Watch out -- these names should be consistent with the display
    ;; option where you choose them, otherwise users are confused.
    (list (_ "Period start") (_ "Period end") (_ "Average") 
          (_ "Maximum") (_ "Minimum") (_ "Gain") 
          (_ "Loss") (_ "Profit") ))
  
  ;; analyze-splits crunches a split list into a set of period
  ;; summaries.  Each summary is a list of (start-date end-date
  ;; avg-bal max-bal min-bal total-in total-out net) if multiple
  ;; accounts are selected the balance is the sum for all.  Each
  ;; balance in a foreign currency will be converted to a double in
  ;; the report-currency by means of the monetary->double
  ;; function. 
  (define (analyze-splits splits start-bal-double 
			  start-date end-date interval monetary->double)
    (let ((interval-list 
	   (gnc:make-date-interval-list start-date end-date interval))
	  (data-rows '()))
      
      (define (output-row interval-start 
			  interval-end 
			  stats-accum 
			  minmax-accum
			  gain-loss-accum)
	 (set! data-rows
              (cons 
               (list (gnc:timepair-to-datestring interval-start)
                     (gnc:timepair-to-datestring interval-end)
                     (/ (stats-accum 'total #f)
                        (gnc:timepair-delta interval-start 
                                            interval-end))
                     (minmax-accum 'getmax #f)
                     (minmax-accum 'getmin #f)
                     (gain-loss-accum 'debits #f) 
                     (gain-loss-accum 'credits #f)
                     (- (gain-loss-accum 'debits #f)
                        (gain-loss-accum 'credits #f)))
               data-rows)))
      
      ;; Returns a double which is the split value, correctly
      ;; exchanged to the current report-currency. We use the exchange
      ;; rate at the 'date'.
      (define (get-split-value split date)
	(monetary->double
	 (gnc:make-gnc-monetary
	  (gnc:account-get-commodity (gnc:split-get-account split))
	  (gnc:split-get-amount split))
	 date))
      
      ;; calculate the statistics for one interval - returns a list 
      ;;  containing the following: 
      ;; min-max acculumator
      ;; average-accumulator
      ;; gain-loss accumulator
      ;; final balance for this interval
      ;; splits remaining to be processed.
      
      ;; note that it is assumed that every split in in the list
      ;; has a date >= from 

      (define (process-interval splits from to start-balance)

	(let ((minmax-accum (gnc:make-stats-collector))
	      (stats-accum (gnc:make-stats-collector))
	      (gain-loss-accum (gnc:make-drcr-collector))
	      (last-balance start-balance)
	      (last-balance-time from))
	 
	  
	  (define (update-stats  split-amt split-time)
	    (let ((time-difference (gnc:timepair-delta 
				    last-balance-time
				    split-time)))
	      (stats-accum 'add (* last-balance time-difference))
	      (set! last-balance (+ last-balance split-amt))
	      (set! last-balance-time split-time)
	      (minmax-accum 'add last-balance)
	      (gain-loss-accum 'add split-amt)))

	  (define (split-recurse)
	    (if (or (null? splits) (gnc:timepair-gt 
				    (gnc:transaction-get-date-posted 
				     (gnc:split-get-parent
				      (car splits))) to)) 
		#f
		(let* 
		    ((split (car splits))
		     (split-time (gnc:transaction-get-date-posted 
				  (gnc:split-get-parent split)))
		     ;; FIXME: Which date should we use here? The 'to'
		     ;; date? the 'split-time'?
		     (split-amt (get-split-value split split-time)))
		  
		  
		  (gnc:debug "split " split)
		  (gnc:debug "split-time " split-time)
		  (gnc:debug "split-amt " split-amt)
		  (gnc:debug "splits " splits)
		  (update-stats split-amt split-time)
		  (set! splits (cdr splits))
		(split-recurse))))

	  ;  the minmax accumulator

	  (minmax-accum 'add start-balance)

	  (if (not (null? splits))
	      (split-recurse))

	  ;; insert a null transaction at the end of the interval
	  (update-stats 0.0 to)
	  (list minmax-accum stats-accum gain-loss-accum last-balance splits)))
		
	      
      (for-each
       (lambda (interval)
	 (let* 
	     
	     ((interval-results 
	       (process-interval 
		splits 
		(car interval) 
		(cadr interval)
		start-bal-double))
	      (min-max-accum (car interval-results))
	      (stats-accum (cadr interval-results))
	      (gain-loss-accum (caddr interval-results))
	      (last-bal (cadddr interval-results))
	      (rest-splits (list-ref interval-results 4)))

	   (set! start-bal-double last-bal)
	   (set! splits rest-splits)
	   (output-row (car interval) 
		       (cadr interval) 
		       stats-accum 
		       min-max-accum gain-loss-accum)))
       interval-list)
	     
      
      (reverse data-rows)))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Renderer
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (renderer report-obj)

    (define (get-option section name)
      (gnc:option-value 
       (gnc:lookup-option (gnc:report-options report-obj) section name)))

    (let* ((report-title (get-option gnc:pagename-general 
				     gnc:optname-reportname))
           (begindate (gnc:timepair-start-day-time
		       (gnc:date-option-absolute-time 
			(get-option gnc:pagename-general optname-from-date))))
           (enddate (gnc:timepair-end-day-time 
		     (gnc:date-option-absolute-time 
		      (get-option gnc:pagename-general optname-to-date))))
           (stepsize (eval (get-option gnc:pagename-general optname-stepsize)))
	   (report-currency (get-option gnc:pagename-general 
				     optname-report-currency))
	   (price-source (get-option gnc:pagename-general
				     optname-price-source))

           (accounts   (get-option gnc:pagename-accounts (N_ "Accounts")))
           (dosubs?    (get-option gnc:pagename-accounts optname-subacct))

           (plot-type  (get-option gnc:pagename-display (N_ "Plot Type")))
           (show-plot? (get-option gnc:pagename-display (N_ "Show plot")))
           (show-table? (get-option gnc:pagename-display (N_ "Show table")))

           (document   (gnc:make-html-document))

	   (commodity-list (gnc:accounts-get-commodities 
			    (append 
			     (gnc:acccounts-get-all-subaccounts accounts)
			     accounts)
			    report-currency))
	   (exchange-fn (gnc:case-exchange-time-fn 
			 price-source report-currency 
			 commodity-list enddate))

	   (beforebegindate (gnc:timepair-end-day-time 
			     (gnc:timepair-previous-day begindate)))
	   ;; startbal will be a commodity-collector
           (startbal  '()))

      (define (monetary->double foreign-monetary date)
	(gnc:numeric-to-double
	 (gnc:gnc-monetary-amount
	  (exchange-fn foreign-monetary report-currency date))))

      (gnc:html-document-set-title! document report-title)
      ;;(warn commodity-list)

      (if (not (null? accounts))
          (let ((query (gnc:malloc-query))
                (splits '())
                (data '()))

            ;; initialize the query to find splits in the right 
            ;; date range and accounts
            (gnc:query-set-group query (gnc:get-current-group))
            
            ;; add accounts to the query (include subaccounts 
            ;; if requested)
            (if dosubs? 
                (let ((subaccts '()))
                  (for-each 
                   (lambda (acct)
                     (let ((this-acct-subs 
                            (gnc:account-get-all-subaccounts acct)))
                       (if (list? this-acct-subs)
                           (set! subaccts 
                                 (append subaccts this-acct-subs)))))
                   accounts)
		  ;; Beware: delete-duplicates is an O(n^2)
		  ;; algorithm. More efficient method: sort the list,
		  ;; then use a linear algorithm.
                  (set! accounts
                        (delete-duplicates (append accounts subaccts)))))

            (gnc:query-add-account-match 
             query (gnc:list->glist accounts) 
             'acct-match-any 'query-and)
            
            ;; match splits between start and end dates 
            (gnc:query-add-date-match-timepair
             query #t begindate #t enddate 'query-and)
            (gnc:query-set-sort-order 
             query 'by-date 'by-standard 'by-none)
            
            ;; get the query results 
            (set! splits (gnc:glist->list (gnc:query-get-splits query)
                                          <gnc:Split*>))
            
            ;; find the net starting balance for the set of accounts 
	    (set! startbal 
		  (gnc:accounts-get-balance-helper 
		   accounts 
		   (lambda (acct) (gnc:account-get-comm-balance-at-date 
				   acct beforebegindate #f))
		   gnc:account-reverse-balance?))

	    (set! startbal 
		  (gnc:numeric-to-double
		   (gnc:gnc-monetary-amount
		    (gnc:sum-collector-commodity 
		     startbal
		     report-currency 
		     (lambda (a b) 
		       (exchange-fn a b beforebegindate))))))
            
            ;; and analyze the data 
            (set! data (analyze-splits splits startbal
				       begindate enddate 
                                       stepsize monetary->double))
            
            ;; make a plot (optionally)... if both plot and table, 
            ;; plot comes first. 
            (if show-plot?
                (let ((barchart (gnc:make-html-barchart))
                      (width (get-option gnc:pagename-display 
				      (N_ "Plot Width")))
                      (height (get-option gnc:pagename-display 
				       (N_ "Plot Height")))
                      (col-labels '())
                      (col-colors '()))
                  (if (memq 'AvgBalPlot plot-type)
                      (begin 
                        (gnc:html-barchart-append-column! 
                         barchart
                         (map (lambda (row) (list-ref row 2)) data))
                        (set! col-labels 
                              (append col-labels 
                                      (list (list-ref columns 2))))
                        (set! col-colors
                              (append col-colors (list "blue")))))
                      
                  (if (memq 'GainPlot plot-type)
                      (begin 
                        (gnc:html-barchart-append-column! 
                         barchart
                         (map (lambda (row) (list-ref row 7)) data))
                        (set! col-labels 
                              (append col-labels 
                                      (list (list-ref columns 7))))
                        (set! col-colors
                              (append col-colors (list "green")))))
                  (if (memq 'GLPlot plot-type)
                      (begin 
                        ;; debit column 
                        (gnc:html-barchart-append-column! 
                         barchart
                         (map (lambda (row) (list-ref row 5)) data))
                        (set! col-labels 
                              (append col-labels 
                                      (list (list-ref columns 5))))
                        (set! col-colors
                              (append col-colors (list "black")))

                        ;; credit
                        (gnc:html-barchart-append-column! 
                         barchart
                         (map (lambda (row) (list-ref row 6)) data))
                        (set! col-labels 
                              (append col-labels 
                                      (list (list-ref columns 6))))
                        (set! col-colors
                              (append col-colors (list "red")))))
                  
                  (gnc:html-barchart-set-col-labels! 
                   barchart col-labels)
                  (gnc:html-barchart-set-col-colors!
                   barchart col-colors)
                  (gnc:html-barchart-set-row-labels! 
                   barchart (map car data))
                  (gnc:html-barchart-set-row-labels-rotated?! barchart #t)
                  
                  (gnc:html-barchart-set-width! barchart width)
                  (gnc:html-barchart-set-height! barchart height)
                  (gnc:html-barchart-set-height! barchart height)
                  (gnc:html-document-add-object! document barchart)))
            
            ;; make a table (optionally)
            (if show-table? 
                (let ((table (gnc:make-html-table)))
                  (gnc:html-table-set-col-headers!
                   table columns)
                  (for-each-in-order 
                   (lambda (row)
                     (gnc:html-table-append-row! table row))
                   data)
                  
                  ;; set numeric columns to align right 
                  (for-each 
                   (lambda (col)
                     (gnc:html-table-set-col-style! 
                      table col "td" 
                      'attribute (list "align" "right")))
                   '(2 3 4 5 6 7))
                  
                  (gnc:html-document-add-object! document table))))

          ;; if there are no accounts selected...
            (gnc:html-document-add-object! 
	     document
	     (gnc:html-make-no-account-warning)))
      document))
  
  (gnc:define-report
   'version 1
   'name (N_ "Average Balance")
   'menu-path (list gnc:menuname-asset-liability)
   'options-generator options-generator
   'renderer renderer))
