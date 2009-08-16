;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average-balance.scm
;; Report history of account balance and other info
;;
;; Author makes no implicit or explicit guarantee of accuracy of 
;;  these calculations and accepts no responsibility for direct
;;  or indirect losses incurred as a result of using this software.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports average-balance))
(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))

(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Average Balance"))

(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-subacct (N_ "Include Sub-Accounts"))
(define optname-internal (N_ "Exclude transactions between selected accounts?"))

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
     optname-price-source "d" 'weighted-average)

    ;; Account tab
    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-subacct
      "a" (N_ "Include sub-accounts of all selected accounts") #t))

    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-internal
      "b"
      (N_ "Exclude transactions that only involve two accounts, both of which are selected below.  This only affects the profit and loss columns of the table.")
      #f))

    ;; account(s) to do report on
    (register-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "c" (N_ "Do transaction report on this account")
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
                  (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CREDIT
                        ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY
                        ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE)
                  ;; or: (list ACCT-TYPE-BANK ACCT-TYPE-CASH
                  ;; ACCT-TYPE-CHECKING ACCT-TYPE-SAVINGS ACCT-TYPE-STOCK
                  ;; ACCT-TYPE-MUTUAL ACCT-TYPE-MONEYMRKT)
                  (gnc-account-get-children-sorted (gnc-get-current-root-account)))))))
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
                        start-date end-date interval monetary->double
			internal)
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
             (list (gnc-print-date interval-start)
                   (gnc-print-date interval-end)
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
        (xaccAccountGetCommodity (xaccSplitGetAccount split))
        (xaccSplitGetAmount split))
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
                                  (gnc-transaction-get-date-posted
                                   (xaccSplitGetParent
                                    (car splits))) to)) 
              #f
              (let* 
                  ((split (car splits))
                   (split-time (gnc-transaction-get-date-posted
                                (xaccSplitGetParent split)))
                   ;; FIXME: Which date should we use here? The 'to'
                   ;; date? the 'split-time'?
		   (split-amt (get-split-value split split-time))
		   (next (cdr splits)))
		
		(if 
		 ;; Check whether this split and next one are a pair
		 ;; from the same transaction, and the only ones in
		 ;; this transaction.
		 ;; If they are and the flag is set appropriately,
		 ;; then skip both.
		 (or internal
		     (null? next)
		     (let* ((next-split (car next))
			    (trans (xaccSplitGetParent split))
			    (next-trans (xaccSplitGetParent next-split))
			    (count (xaccTransCountSplits trans)))
		       (not (and (eqv? count 2)
				 (equal? trans next-trans)))))                
		 (begin
		  (gnc:debug "split " split)
		  (gnc:debug "split-time " split-time)
		  (gnc:debug "split-amt " split-amt)
		  ;; gnc:debug converts its input to a string before
		  ;; deciding whether to print it, and converting
		  ;; |splits| to a string is O(N) in its length.  Since
		  ;; this code runs for every split, leaving that
		  ;; gnc:debug in makes the whole thing O(N^2) in number
		  ;; of splits.  If someone really needs this output,
		  ;; they should uncomment the gnc:debug call.
					; (gnc:debug "splits " splits)
		  (update-stats split-amt split-time)
		  (set! splits next)
		  (split-recurse))
		 (begin
		  (set! splits (cdr next))
		  (split-recurse))))))

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

  (gnc:report-starting reportname)
  (let* ((report-title (get-option gnc:pagename-general 
                                   gnc:optname-reportname))
         (begindate (gnc:timepair-start-day-time
                     (gnc:date-option-absolute-time 
                      (get-option gnc:pagename-general optname-from-date))))
         (enddate (gnc:timepair-end-day-time 
                   (gnc:date-option-absolute-time 
                    (get-option gnc:pagename-general optname-to-date))))
         (stepsize (gnc:deltasym-to-delta (get-option gnc:pagename-general optname-stepsize)))
         (report-currency (get-option gnc:pagename-general 
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))

         (internal-included (not (get-option gnc:pagename-accounts optname-internal)))
         (accounts   (get-option gnc:pagename-accounts (N_ "Accounts")))
         (dosubs?    (get-option gnc:pagename-accounts optname-subacct))

         (plot-type  (get-option gnc:pagename-display (N_ "Plot Type")))
         (show-plot? (get-option gnc:pagename-display (N_ "Show plot")))
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))

         (document   (gnc:make-html-document))

	 (commodity-list #f)
	 (exchange-fn #f)

         (beforebegindate (gnc:timepair-end-day-time 
                           (gnc:timepair-previous-day begindate)))
         (all-zeros? #t)
         ;; startbal will be a commodity-collector
         (startbal  '()))

    (define (list-all-zeros? alist)
      (if (null? alist) #t
          (if (not (= 0.0 (car alist)))
              #f
              (list-all-zeros? (cdr alist)))))
    
    (define (monetary->double foreign-monetary date)
      (gnc-numeric-to-double
       (gnc:gnc-monetary-amount
        (exchange-fn foreign-monetary report-currency date))))

    (gnc:html-document-set-title! document report-title)
    ;;(warn commodity-list)

    (if (not (null? accounts))
        (let ((query (qof-query-create-for-splits))
              (splits '())
              (data '()))

          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
	  (gnc:report-percent-done 1)
	  (set! commodity-list (gnc:accounts-get-commodities 
                                (append 
                                 (gnc:acccounts-get-all-subaccounts accounts)
                                 accounts)
                                report-currency))
	  (gnc:report-percent-done 5)
	  (set! exchange-fn (gnc:case-exchange-time-fn 
                             price-source report-currency 
                             commodity-list enddate
			     5 20))
	  (gnc:report-percent-done 20)

          ;; initialize the query to find splits in the right 
          ;; date range and accounts
          (qof-query-set-book query (gnc-get-current-book))

	  ;; for balance purposes, we don't need to do this, but it cleans up
	  ;; the table display.
          (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
          ;; add accounts to the query (include subaccounts 
          ;; if requested)
	  (gnc:report-percent-done 25)
          (if dosubs? 
              (let ((subaccts '()))
                (for-each 
                 (lambda (acct)
                   (let ((this-acct-subs 
                          (gnc-account-get-descendants-sorted acct)))
                     (if (list? this-acct-subs)
                         (set! subaccts 
                               (append subaccts this-acct-subs)))))
                 accounts)
                ;; Beware: delete-duplicates is an O(n^2)
                ;; algorithm. More efficient method: sort the list,
                ;; then use a linear algorithm.
                (set! accounts
                      (delete-duplicates (append accounts subaccts)))))
	  (gnc:report-percent-done 30)

          (xaccQueryAddAccountMatch query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          
          ;; match splits between start and end dates 
          (xaccQueryAddDateMatchTS
           query #t begindate #t enddate QOF-QUERY-AND)
          (qof-query-set-sort-order query
				    (list SPLIT-TRANS TRANS-DATE-POSTED)
				    (list QUERY-DEFAULT-SORT)
				    '())
          
          ;; get the query results 
          (set! splits (qof-query-run query))
	  (gnc:report-percent-done 40)
          
          ;; find the net starting balance for the set of accounts 
          (set! startbal 
                (gnc:accounts-get-balance-helper 
                 accounts 
                 (lambda (acct) (gnc:account-get-comm-balance-at-date 
                                 acct beforebegindate #f))
                 (lambda (x) #f)))
	  (gnc:report-percent-done 50)

          (set! startbal 
                (gnc-numeric-to-double
                 (gnc:gnc-monetary-amount
                  (gnc:sum-collector-commodity 
                   startbal
                   report-currency 
                   (lambda (a b) 
                     (exchange-fn a b beforebegindate))))))
	  (gnc:report-percent-done 60)
	  
          ;; and analyze the data 
          (set! data (analyze-splits splits startbal
                                     begindate enddate 
                                     stepsize monetary->double
				     internal-included))
	  (gnc:report-percent-done 70)
          
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
                    (let
                        ((number-data
                          (map 
                           (lambda (row) (list-ref row 2)) data)))
                      (if (not (list-all-zeros? number-data))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
			     number-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 2))))
                            (set! col-colors
                                  (append col-colors (list "blue")))
                            (set! all-zeros? #f)))))
                
                
                (if (memq 'GainPlot plot-type)
                    (let ((number-data 
                           (map (lambda (row) (list-ref row 7)) data)))
                      (if (not (list-all-zeros? number-data))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
			     number-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 7))))
                            (set! col-colors
                                  (append col-colors (list "green")))
                            (set! all-zeros? #f)))))

                (if (memq 'GLPlot plot-type)
                    (let ((debit-data 
                           (map (lambda (row) (list-ref row 5)) data))
                          (credit-data
                           (map (lambda (row) (list-ref row 6)) data)))
                      ;; debit column 
                      (if (not (and
                                (list-all-zeros? debit-data)
                                (list-all-zeros? credit-data)))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
                             debit-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 5))))
                            (set! col-colors
                                  (append col-colors (list "black")))
                            
                            ;; credit
                            (gnc:html-barchart-append-column! 
                             barchart
                             credit-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 6))))
                            (set! col-colors
                                  (append col-colors (list "red")))
                            (set! all-zeros? #f)))))
                
                (if (not all-zeros?)
                    (begin
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
                      (gnc:html-document-add-object! document barchart))
                    (gnc:html-document-add-object!
                     document
                     (gnc:html-make-empty-data-warning 
                      report-title (gnc:report-id report-obj))))))
          
          ;; make a table (optionally)
	  (gnc:report-percent-done 80)
          (if show-table? 
              (let ((table (gnc:make-html-table)))
                (gnc:html-table-set-col-headers!
                 table columns)
                (for-each
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
         (gnc:html-make-no-account-warning 
	  report-title (gnc:report-id report-obj))))
    (gnc:report-finished)
    document))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "d5adcc61c62e4b8684dd8907448d7900"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer renderer)
