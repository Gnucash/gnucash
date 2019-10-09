;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average-balance.scm
;; Report history of account balance and other info
;;
;; Author makes no implicit or explicit guarantee of accuracy of 
;;  these calculations and accepts no responsibility for direct
;;  or indirect losses incurred as a result of using this software.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash report standard-reports average-balance))
(use-modules (srfi srfi-1))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Average Balance"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-subacct (N_ "Include Sub-Accounts"))
(define optname-internal (N_ "Exclude transactions between selected accounts"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

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

    ;; Report's currency
    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "c")
    
    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "d" 'weighted-average)

    ;; Account tab
    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-subacct
      "a" (N_ "Include sub-accounts of all selected accounts.") #t))

    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-internal
      "b"
      (N_ "Exclude transactions that only involve two accounts, both of which are selected below. This only affects the profit and loss columns of the table.")
      #f))

    ;; account(s) to do report on
    (register-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "c" (N_ "Do transaction report on this account.")
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
      "c" (N_ "The type of graph to generate.") (list 'AvgBalPlot)
      (list 
       (vector 'AvgBalPlot (N_ "Average") (N_ "Average Balance."))
       (vector 'GainPlot (N_ "Profit") (N_ "Profit (Gain minus Loss)."))
       (vector 'GLPlot (N_ "Gain/Loss") (N_ "Gain And Loss.")))))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

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


(define (analyze-splits splits balances daily-dates interval-dates
                        internal-included exchange-fn report-currency)
  ;; this is a tight loop. start with: daily-balances & daily-dates,
  ;; interval-dates, and the splitlist. traverse the daily balances
  ;; and splitlist until we cross an interval date boundary, then
  ;; summarize the interval-balances and interval-amounts
  (define work-to-do (length splits))
  (let loop ((results '())
             (interval-bals '())
             (interval-amts '())
             (splits splits)
             (work-done 0)
             (daily-balances (cdr balances))
             (daily-dates (cdr daily-dates))
             (interval-start (car interval-dates))
             (interval-dates (cdr interval-dates)))

    (cond
     ;; daily-dates finished. job done. add details for last-interval
     ;; which must be handled separately, and return to caller
     ((null? daily-dates)
      (reverse
       (cons (list
              (qof-print-date interval-start)
              (qof-print-date (car interval-dates))
              (/ (apply + interval-bals)
                 (length interval-bals))
              (apply max interval-bals)
              (apply min interval-bals)
              (apply + (filter positive? interval-amts))
              (- (apply + (filter negative? interval-amts)))
              (apply + interval-amts))
             results)))

     ;; first daily-date > first interval-date -- crossed interval
     ;; boundary -- add interval details to results
     ((> (car daily-dates) (car interval-dates))
      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
      (loop (cons (list
                   (qof-print-date interval-start)
                   (qof-print-date (decdate (car interval-dates)
                                            DayDelta))
                   (/ (apply + interval-bals)
                      (length interval-bals))
                   (apply max interval-bals)
                   (apply min interval-bals)
                   (apply + (filter positive? interval-amts))
                   (- (apply + (filter negative? interval-amts)))
                   (apply + interval-amts))
                  results)    ;process interval amts&bals
            '()               ;reset interval-bals
            '()               ;and interval-amts
            splits
            work-done
            daily-balances
            daily-dates
            (car interval-dates)
            (cdr interval-dates)))

     ;; we're still within interval, no more splits left within
     ;; current interval. add daily balance to interval.
     ((or (null? splits)
          (> (xaccTransGetDate (xaccSplitGetParent (car splits)))
             (car interval-dates)))
      (loop results
            (cons (car daily-balances) interval-bals)
            interval-amts
            splits
            work-done
            (cdr daily-balances)
            (cdr daily-dates)
            interval-start
            interval-dates))

     ;; we're still within interval. 'internal' is disallowed; there
     ;; are at least 2 splits remaining, both from the same
     ;; transaction. skip them. NOTE we should really expand this
     ;; conditional whereby all splits are internal, however the
     ;; option is labelled as 2-splits only. best maintain behaviour.
     ((and (not internal-included)
           (pair? (cdr splits))
           (= 2 (xaccTransCountSplits (xaccSplitGetParent (car splits))))
           (equal? (xaccSplitGetParent (car splits))
                   (xaccSplitGetParent (cadr splits))))
      (loop results
            interval-bals
            interval-amts ;interval-amts unchanged
            (cddr splits) ;skip two splits.
            (+ 2 work-done)
            daily-balances
            daily-dates
            interval-start
            interval-dates))

     ;; we're still within interval. there are splits remaining. add
     ;; split details to interval-amts
     (else
      (loop results
            interval-bals
            (cons (gnc:gnc-monetary-amount
                   (exchange-fn
                    (gnc:make-gnc-monetary
                     (xaccAccountGetCommodity
                      (xaccSplitGetAccount (car splits)))
                     (xaccSplitGetAmount (car splits)))
                    report-currency
                    (car interval-dates)))
                  interval-amts) ;add split amt to list
            (cdr splits)         ;and loop to next split
            (1+ work-done)
            daily-balances
            daily-dates
            interval-start
            interval-dates)))))

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
         (begindate (gnc:time64-start-day-time
                     (gnc:date-option-absolute-time 
                      (get-option gnc:pagename-general optname-from-date))))
         (enddate (gnc:time64-end-day-time 
                   (gnc:date-option-absolute-time 
                    (get-option gnc:pagename-general optname-to-date))))
         (stepsize (gnc:deltasym-to-delta
                    (get-option gnc:pagename-general optname-stepsize)))
         (report-currency (get-option gnc:pagename-general 
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))

         (internal-included (not (get-option gnc:pagename-accounts optname-internal)))
         (accounts   (get-option gnc:pagename-accounts (N_ "Accounts")))
         (dosubs?    (get-option gnc:pagename-accounts optname-subacct))
         (accounts (if dosubs?
                       (gnc:accounts-and-all-descendants accounts)
                       accounts))
         (plot-type  (get-option gnc:pagename-display (N_ "Plot Type")))
         (show-plot? (get-option gnc:pagename-display (N_ "Show plot")))
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))

         (document   (gnc:make-html-document))

	 (commodity-list #f)
	 (exchange-fn #f)
         (all-zeros? #t))

    ;;(warn commodity-list)

    (if (not (null? accounts))
        (let ((query (qof-query-create-for-splits))
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
                                accounts report-currency))

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

          (xaccQueryAddAccountMatch query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          
          ;; match splits between start and end dates 
          (xaccQueryAddDateMatchTT
           query #t begindate #t enddate QOF-QUERY-AND)
          (qof-query-set-sort-order query
				    (list SPLIT-TRANS TRANS-DATE-POSTED)
				    (list QUERY-DEFAULT-SORT)
				    '())
          
	  (gnc:report-percent-done 40)

          (let* ((splits (qof-query-run query))
                 (daily-dates (gnc:make-date-list begindate enddate DayDelta))
                 (interval-dates (gnc:make-date-list begindate enddate stepsize))

                 ;; for accounts-balances generation
                 (work-to-do (length accounts))
                 (accounts-balances (map
                                     (lambda (work-done acc)
                                       (gnc:report-percent-done
                                        (* 100 (/ work-done work-to-do)))
                                       (gnc:account-get-balances-at-dates
                                        acc daily-dates))
                                     (iota work-to-do)
                                     accounts))

                 ;; for daily-balances generation
                 (work-to-do (length daily-dates))
                 (balances (map
                            (lambda (work-done date accounts-balance)
                              (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
                              (gnc:gnc-monetary-amount
                               (gnc:sum-collector-commodity
                                (apply gnc:monetaries-add accounts-balance)
                                report-currency
                                (lambda (monetary target-curr)
                                  (exchange-fn monetary target-curr date)))))
                            (iota work-to-do)
                            daily-dates
                            (apply zip accounts-balances))))

            (qof-query-destroy query)

            (unless (null? splits)
              (set! data
                (analyze-splits splits balances daily-dates interval-dates
                                internal-included exchange-fn report-currency))))

          (gnc:report-percent-done 70)
          
          ;; make a plot (optionally)... if both plot and table, 
          ;; plot comes first. 
          (if show-plot?
              (let ((barchart (gnc:make-html-barchart))
                    (height (get-option gnc:pagename-display optname-plot-height))
                    (width (get-option gnc:pagename-display optname-plot-width))
                    (col-labels '())
                    (col-colors '()))
                (if (memq 'AvgBalPlot plot-type)
                    (let
                        ((number-data
                          (map 
                           (lambda (row) (list-ref row 2)) data)))
                      (if (not (every zero? number-data))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
			     number-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 2))))
                            (set! col-colors
                                  (append col-colors (list "#0074D9")))
                            (set! all-zeros? #f)))))
                
                
                (if (memq 'GainPlot plot-type)
                    (let ((number-data 
                           (map (lambda (row) (list-ref row 7)) data)))
                      (if (not (every zero? number-data))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
			     number-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 7))))
                            (set! col-colors
                                  (append col-colors (list "#2ECC40")))
                            (set! all-zeros? #f)))))

                (if (memq 'GLPlot plot-type)
                    (let ((debit-data 
                           (map (lambda (row) (list-ref row 5)) data))
                          (credit-data
                           (map (lambda (row) (list-ref row 6)) data)))
                      ;; debit column 
                      (if (not (and
                                (every zero? debit-data)
                                (every zero? credit-data)))
                          (begin
                            (gnc:html-barchart-append-column! 
                             barchart
                             debit-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 5))))
                            (set! col-colors
                                  (append col-colors (list "#111111")))
                            
                            ;; credit
                            (gnc:html-barchart-append-column! 
                             barchart
                             credit-data)
                            (set! col-labels 
                                  (append col-labels 
                                          (list (list-ref columns 6))))
                            (set! col-colors
                                  (append col-colors (list "#FF4136")))
                            (set! all-zeros? #f)))))
                
                (if (not all-zeros?)
                    (begin
                      (gnc:html-barchart-set-title! barchart report-title)
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
              (let ((table (gnc:make-html-table))
                    (scu (gnc-commodity-get-fraction report-currency)))
                (gnc:html-table-set-col-headers!
                 table columns)
                (for-each
                 (lambda (row)
                   (gnc:html-table-append-row!
                    table
                    (map
                     gnc:make-html-table-cell/markup
                     (list "date-cell" "date-cell"
                           "number-cell" "number-cell" "number-cell"
                           "number-cell" "number-cell" "number-cell")
                     (cons* (car row)
                            (cadr row)
                            (map
                             (lambda (amt)
                               (gnc:make-gnc-monetary
                                report-currency
                                (gnc-numeric-convert amt scu GNC-RND-ROUND)))
                             (cddr row))))))
                 data)
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
