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

(let ((pagename-general (N_ "General"))
      (pagename-accounts (N_ "Accounts"))
      (pagename-display (N_ "Display"))
      (optname-subacct (N_ "Include Sub-Accounts"))
      (optname-report-currency (N_ "Report Currency")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Options
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (options-generator)
    (let* ((options (gnc:new-options))
           ;; register a configuration option for the report
           (register-option
            (lambda (new-option)
              (gnc:register-option options new-option))))      

      (gnc:options-add-date-interval!
       options pagename-general (N_ "From") (N_ "To") "a")

      ;; account(s) to do report on
      (register-option
       (gnc:make-account-list-option
        pagename-accounts (N_ "Accounts")
        "d" (N_ "Do transaction report on this account")
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
		    '(bank cash credit asset liability equity) 
		    ;; or: '(bank cash checking savings stock
		    ;; mutual-fund money-market)
		    (gnc:group-get-subaccounts (gnc:get-current-group)))))))
        #f #t))

      (gnc:options-add-interval-choice! 
       options pagename-general (N_ "Step Size") "b" 'TwoWeekDelta)

      (register-option
       (gnc:make-simple-boolean-option
        pagename-accounts optname-subacct
        "e" (N_ "Include sub-accounts of all selected accounts") #t))

      ;; Report currency
      (gnc:options-add-currency! 
       options pagename-general optname-report-currency "f")
      
      (register-option
       (gnc:make-simple-boolean-option
        pagename-display (N_ "Show table")
        "a" (N_ "Display a table of the selected data.") #f))

      (register-option
       (gnc:make-simple-boolean-option
        pagename-display (N_ "Show plot")
        "b" (N_ "Display a graph of the selected data.") #t))

      (register-option
       (gnc:make-list-option
        pagename-display (N_ "Plot Type")
        "c" (N_ "The type of graph to generate") (list 'AvgBalPlot)
        (list (list->vector
               (list 'AvgBalPlot (N_ "Average") (N_ "Average Balance")))
              (list->vector
               (list 'GainPlot (N_ "Net Gain") (N_ "Net Gain")))
              (list->vector
               (list 'GLPlot (N_ "Gain/Loss") (N_ "Gain And Loss"))))))

      (gnc:options-add-plot-size! 
       options pagename-display (N_ "Plot Width") (N_ "Plot Height") "d" 400 400)

      ;; Set the general page as default option tab
      (gnc:options-set-default-section options pagename-general)      
      
      options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some utilities for generating the data 
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define columns
    (list (_ "Period start") (_ "Period end") (_ "Avg Bal") 
          (_ "Max Bal") (_ "Min Bal") (_ "Total In") 
          (_ "Total Out") (_ "Net Change") ))
  
  ;; analyze-splits crunches a split list into a set of period
  ;; summaries.  Each summary is a list of (start-date end-date
  ;; avg-bal max-bal min-bal total-in total-out net) if multiple
  ;; accounts are selected the balance is the sum for all.  Each
  ;; balance in a foreign currency will be converted to a double in
  ;; the report-currency by means of the collector->double
  ;; function. 

  ;; FIXME: the exchange rate should change every time interval, of
  ;; course, but right now we assume the very last exchange rate to be
  ;; constant over the whole report period. Note that this might get
  ;; *really* complicated.

  (define (analyze-splits splits start-bal 
			  start-date end-date interval collector->double)
    (let* ((minmax-accum (gnc:make-stats-collector))
           (stats-accum (gnc:make-stats-collector))
           (gain-loss-accum (gnc:make-drcr-collector))
           (interval-start start-date)
	   ;; Note that this (decdate ... SecDelta) stuff is *vital*
	   ;; to make sure that our intervals are *not overlapping*.
           (interval-end (decdate (incdate start-date interval) SecDelta))
           (last-balance (collector->double start-bal))
           (last-balance-time interval-start)
           (data-rows '()))
      
      (define (output-row)
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
      ;; exchanged to the current report-currency.
      (define (get-split-value split)
	(let ((coll (gnc:make-commodity-collector)))
	  (coll 'add (gnc:account-get-commodity (gnc:split-get-account split))
		(gnc:split-get-amount split))
	  ;; FIXME: not as efficient as it would be possible because I
	  ;; only have the collector->double conversion at hand.
	  (collector->double coll)))
      
      ;; initialize the accumulators 
      (minmax-accum 'reset #f)
      (stats-accum 'reset #f)
      (gain-loss-accum 'reset #f)

      (minmax-accum 'add start-bal)

      ;; Now go through all splits. FIXME: This assumes that there is
      ;; at least one split in each time interval, especially in the
      ;; first and the last one. I haven't yet thoroughly thought
      ;; about what happens if that's not the case -- somebody should
      ;; think this through.
      (for-each 
       (lambda (split)
	 ;; xtn-date: The date of the current split.
         (let* ((xtn-date 
                 (gnc:transaction-get-date-posted 
                  (gnc:split-get-parent split)))
		;; split-amt: The value of this split. Is a double.
                (split-amt (get-split-value split)))

	   ;; procedure to be executed if the current split is in the
	   ;; interval
           (define (split-in-interval)
             (stats-accum 
              'add (* last-balance
                      (gnc:timepair-delta last-balance-time
                                          xtn-date)))
             ;; update other stats 
             (set! last-balance (+ last-balance split-amt))
             (set! last-balance-time xtn-date)
             (minmax-accum 'add last-balance)
             (gain-loss-accum 'add split-amt))
           
	   ;; procedure to be executed if the current split is not
	   ;; (yet) in the interval
           (define (split-outside-interval)
             (stats-accum 
              'add (* last-balance
                      (gnc:timepair-delta last-balance-time 
                                          interval-end)))
             (set! last-balance-time interval-end)
             (minmax-accum 'add last-balance)
             
             ;; output a row of info 
             (output-row)
             (set! interval-start (incdate interval-start interval))
             (set! interval-end (decdate (incdate interval-start interval) SecDelta))
             
             ;; reset collectors 
             (minmax-accum 'reset #f)
             (gain-loss-accum 'reset #f)
             (stats-accum 'reset #f))
           
           ;; is this split in the interval? 
           (let loop ()
             (if (gnc:timepair-le xtn-date interval-end)
                 ;; yes, it is inside interval 
                 (split-in-interval)
                 ;; otherwise, loop until it is
                 ;; in the interval.
                 (begin 
                   (split-outside-interval)
                   (loop))))))
       splits)
      
      ;; now spit out the last chunk of data (between the beginning
      ;; of the last interval and the last split)
      (if (not (gnc:timepair-eq last-balance-time interval-start))
          (begin 
            (set! interval-end last-balance-time)
            (output-row)))
      (reverse data-rows)))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Renderer
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (renderer report-obj)
    (let* ((opt-val 
            (lambda (sec value)
              (gnc:option-value 
               (gnc:lookup-option (gnc:report-options report-obj) sec value))))
           (begindate  (gnc:timepair-start-day-time
			(gnc:date-option-absolute-time 
			 (opt-val pagename-general (N_ "From")))))
           (enddate    (gnc:timepair-end-day-time 
			(gnc:date-option-absolute-time 
			 (opt-val pagename-general (N_ "To")))))
           (stepsize   (eval (opt-val pagename-general (N_ "Step Size"))))
           (accounts   (opt-val pagename-accounts (N_ "Accounts")))
           (dosubs?    (opt-val pagename-accounts optname-subacct))
	   (report-currency (opt-val pagename-general optname-report-currency))
           (plot-type  (opt-val pagename-display (N_ "Plot Type")))
           (show-plot? (opt-val pagename-display (N_ "Show plot")))
           (show-table? (opt-val pagename-display (N_ "Show table")))
           (document   (gnc:make-html-document))
	   (exchange-alist (gnc:make-exchange-alist
			    report-currency enddate))
	   (exchange-fn (gnc:make-exchange-function exchange-alist))
	   (beforebegindate (gnc:timepair-end-day-time 
			     (gnc:timepair-previous-day begindate)))
	   ;; startbal will be a commodity-collector
           (startbal  '()))
      
      (define (collector->double commodity-collector )
	(gnc:numeric-to-double
	 (gnc:gnc-monetary-amount
	  (gnc:sum-collector-commodity commodity-collector
				       report-currency 
				       exchange-fn))))

      (gnc:html-document-set-title! document (_ "Average Balance"))

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
                  (set! accounts (delete-duplicates (append accounts subaccts)))))
            
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
            
            ;; and analyze the data 
            (set! data (analyze-splits splits startbal begindate enddate 
                                       stepsize collector->double))
            
            ;; make a plot (optionally)... if both plot and table, 
            ;; plot comes first. 
            (if show-plot?
                (let ((barchart (gnc:make-html-barchart))
                      (width (opt-val pagename-display (N_ "Plot Width")))
                      (height (opt-val pagename-display (N_ "Plot Height")))
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
          (let ((p (gnc:make-html-text)))
            (gnc:html-text-append! 
             p 
             (gnc:html-markup-h2 (_ "No accounts selected"))
             (gnc:html-markup-p
              (_ "This report requires accounts to be selected.")))
            (gnc:html-document-add-object! document p)))
      document))
  
  (gnc:define-report
   'version 1
   'name (N_ "Average Balance")
   'menu-path (list "_Assets & Liabilities")
   'options-generator options-generator
   'renderer renderer))
