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

(let ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Options
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (options-generator)
    (let* ((options (gnc:new-options))
           ;; register a configuration option for the report
           (register-option
            (lambda (new-option)
              (gnc:register-option options new-option))))      
      ;; From date  
      (register-option
       (gnc:make-date-option
        (N_ "General") (N_ "From")
        "a" (N_ "Report Items from this date") 
        (lambda ()
          (let ((bdtime (localtime (current-time))))
            (set-tm:sec bdtime 0)
            (set-tm:min bdtime 0)
            (set-tm:hour bdtime 0)
            (set-tm:mday bdtime 1)
            (set-tm:mon bdtime 0)
            (cons 'absolute (cons (car (mktime bdtime)) 0))))
        #f 
	'absolute #f))
      
      ;; to-date
      (register-option
       (gnc:make-date-option
        (N_ "General") (N_ "To")
        "c" (N_ "Report items up to and including this date")
        (lambda ()
          (let ((bdtime (localtime (current-time))))
            (set-tm:sec bdtime 59)
            (set-tm:min bdtime 59)
            (set-tm:hour bdtime 23)
            (cons 'absolute (cons (car (mktime bdtime)) 0))))
        #f 'absolute #f))
      
      ;; account(s) to do report on
      (register-option
       (gnc:make-account-list-option
        (N_ "General") (N_ "Accounts")
        "d" (N_ "Do transaction report on this account")
        (lambda ()
          (let ((current-accounts (gnc:get-current-accounts)))
            (cond ((not (null? current-accounts)) current-accounts)
                  (else
                   (gnc:group-get-account-list (gnc:get-current-group))))))
        #f #t))

      (register-option
       (gnc:make-multichoice-option
        (N_ "General") (N_ "Step Size")
        "b" (N_ "The amount of time between data points") 'WeekDelta
        (list #(DayDelta "Day" "Day")
              #(WeekDelta "Week" "Week")
              #(TwoWeekDelta "2Week" "Two Week")
              #(MonthDelta "Month" "Month")
              #(YearDelta "Year" "Year")
              )))

      (register-option
       (gnc:make-simple-boolean-option
        (N_ "General") (N_ "Sub-Accounts")
        "e" (N_ "Include sub-accounts of all selected accounts") #f))

      (register-option
       (gnc:make-list-option
        (N_ "Output") (N_ "Plot Type")
        "a" (N_ "The type of graph to generate") (list 'AvgBalPlot)
        (list (list->vector
               (list 'AvgBalPlot (N_ "Average") (N_ "Average Balance")))
              (list->vector
               (list 'GainPlot (N_ "Net Gain") (N_ "Net Gain")))
              (list->vector
               (list 'GLPlot (N_ "Gain/Loss") (N_ "Gain And Loss"))))))

      (register-option
       (gnc:make-number-range-option
        (N_ "Output") (N_ "Plot Width")
        "b" (N_ "Width of plot in pixels.") 400
        100 1000 0 1))

      (register-option
       (gnc:make-number-range-option
        (N_ "Output") (N_ "Plot Height")
        "b" (N_ "Height of plot in pixels.") 400
        100 1000 0 1))

      (register-option
       (gnc:make-simple-boolean-option
        (N_ "Output") (N_ "Show plot")
        "b" (N_ "Display a graph of the selected data.") #t))

      (register-option
       (gnc:make-simple-boolean-option
        (N_ "Output") (N_ "Show table")
        "b" (N_ "Display a table of the selected data.") #f))

      
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
  ;; accounts are selected the balance is the sum for all.  we aren't
  ;; worrying about currency ATM :(

  (define (analyze-splits splits start-bal start-date end-date interval)
    (let* ((minmax-accum (gnc:make-stats-collector))
           (stats-accum (gnc:make-stats-collector))
           (gain-loss-accum (gnc:make-drcr-collector))
           (interval-start start-date)
           (interval-end (incdate start-date interval))
           (last-balance start-bal)
           (last-balance-time interval-start)
           (data-rows '()))
      
      (define (output-row)
        (set! data-rows 
              (cons 
               (list (gnc:timepair-to-datestring interval-start)
                     (gnc:timepair-to-datestring 
                      (decdate interval-end SecDelta))
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
      
      ;; initialize the accumulators 
      (minmax-accum 'reset #f)
      (stats-accum 'reset #f)
      (gain-loss-accum 'reset #f)

      (minmax-accum 'add start-bal)

      (for-each 
       (lambda (split)
         (let* ((xtn-date 
                 (gnc:transaction-get-date-posted 
                  (gnc:split-get-parent split)))
                (split-amt (gnc:numeric-to-double 
                            (gnc:split-get-value split))))

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
           
           (define (split-outside-interval)
             (stats-accum 
              'add (* last-balance
                      (gnc:timepair-delta last-balance-time 
                                          interval-end)))
             (set! last-balance-time interval-end)
             (minmax-accum 'add last-balance)
             
             ;; output a row of info 
             (output-row)
             (set! interval-start interval-end)
             (set! interval-end (incdate interval-start interval))
             
             ;; reset collectors 
             (minmax-accum 'reset #f)
             (gain-loss-accum 'reset #f)
             (stats-accum 'reset #f))
           
           ;; is this split in the interval? 
           (let loop ()
             (if (gnc:timepair-le xtn-date interval-end)
                 ;; transaction is inside interval 
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
           (begindate (gnc:date-option-absolute-time 
                       (opt-val  (N_ "General") (N_ "From"))))
           (enddate   (gnc:timepair-end-day-time 
                       (gnc:date-option-absolute-time 
                        (opt-val  (N_ "General") (N_ "To")))))
           (stepsize  (eval (opt-val  (N_ "General") (N_ "Step Size"))))
           (accounts  (opt-val  (N_ "General") (N_ "Accounts")))
           (dosubs?   (opt-val  (N_ "General") (N_ "Sub-Accounts")))
           (plot-type (opt-val  (N_ "Output") (N_ "Plot Type")))
           (show-plot?  (opt-val (N_ "Output") (N_ "Show plot")))
           (show-table? (opt-val (N_ "Output") (N_ "Show table")))
           (document  (gnc:make-html-document))
           (startbal  0.0))
      
      (gnc:html-document-set-title! document (N_ "Average Balance"))

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
                  (set! accounts (append accounts subaccts))))
            
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
            (for-each
             (lambda (acct)
               (set! startbal 
                     (+ startbal 
                        (gnc:account-get-balance-at-date acct begindate 
                                                         #f))))
             accounts)
            
            ;; and analyze the data 
            (set! data (analyze-splits splits startbal begindate enddate 
                                       stepsize))
            
            ;; make a plot (optionally)... if both plot and table, 
            ;; plot comes first. 
            (if show-plot?
                (let ((barchart (gnc:make-html-barchart))
                      (width (opt-val (N_ "Output") (N_ "Plot Width")))
                      (height (opt-val (N_ "Output") (N_ "Plot Height")))
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
   'options-generator options-generator
   'renderer renderer))
  
            