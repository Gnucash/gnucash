;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; net-barchart.scm : Display a time series for either net worth or
;; net profit.
;;
;; By Robert Merkel <rgmerk@mira.net>
;; and Christian Stimming <stimming@tu-harburg.de>
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report net-barchart))

(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Income/Expense Chart"))

(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))

(define optname-inc-exp (N_ "Show Income/Expense"))
(define optname-show-profit (N_ "Show Net Profit"))

(define optname-sep-bars (N_ "Show Asset & Liability bars"))
(define optname-net-bars (N_ "Show Net Worth bars"))

(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define (options-generator inc-exp?)
  (let* ((options (gnc:new-options)) 
         ;; This is just a helper function for making options.
         ;; See gnucash/src/scm/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-interval-choice! 
     options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "c")
    
    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "d" 'weighted-average)

    ;; Account tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (filter
         (if inc-exp?
             gnc:account-is-inc-exp?
             (lambda (account) (not (gnc:account-is-inc-exp? account))))
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (filter 
               (if inc-exp?
                   gnc:account-is-inc-exp?
                   (lambda (account)
                     (not (gnc:account-is-inc-exp? account))))
               accounts)))
      #t))

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display 
      (if inc-exp? optname-inc-exp optname-sep-bars)
      "a" 
      (if inc-exp? 
          (N_ "Show Income and Expenses?")
          (N_ "Show the Asset and the Liability bars?"))
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display 
      (if inc-exp? optname-show-profit optname-net-bars)
      "b" 
      (if inc-exp?
          (N_ "Show the net profit?")
          (N_ "Show a Net Worth bar?")) 
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "c" (N_ "Display a table of the selected data.")
      #f))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "d" 500 400)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (net-renderer report-obj inc-exp?)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)
  (let* ((to-date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general 
				   optname-to-date))))
         (from-date-tp (gnc:timepair-start-day-time 
                        (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general 
				     optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))

         (accounts (get-option gnc:pagename-accounts optname-accounts))

         (show-sep? (get-option gnc:pagename-display 
				(if inc-exp? optname-inc-exp 
				    optname-sep-bars)))
         (show-net? (get-option gnc:pagename-display 
				(if inc-exp? optname-show-profit
				    optname-net-bars)))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))

         (commodity-list #f)
         (exchange-fn #f)

         (dates-list ((if inc-exp? gnc:make-date-interval-list
                          gnc:make-date-list)
                      ((if inc-exp? gnc:timepair-start-day-time 
                           gnc:timepair-end-day-time) from-date-tp) 
                      (gnc:timepair-end-day-time to-date-tp)
                      (gnc:deltasym-to-delta interval)))
	 (report-title (get-option gnc:pagename-general 
                                  gnc:optname-reportname))
         (classified-accounts (gnc:decompose-accountlist accounts))
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))
         (document (gnc:make-html-document))
         (chart (gnc:make-html-barchart))
         (non-zeros #f))

    (define (add-column! data-list)
      (begin
        (gnc:html-barchart-append-column! chart data-list)
        (if (gnc:not-all-zeros data-list) (set! non-zeros #t))
        #f))

    ;; This exchanges the commodity-collector 'c' to one single
    ;; 'report-currency' according to the exchange-fn. Returns a
    ;; double.
    (define (collector->double c date)
      (gnc-numeric-to-double
       (gnc:gnc-monetary-amount
        (gnc:sum-collector-commodity 
         c report-currency 
         (lambda (a b) (exchange-fn a b date))))))

    ;; This calculates the balances for all the 'accounts' for each
    ;; element of the list 'dates'. If income?==#t, the signs get
    ;; reversed according to income-sign-reverse general option
    ;; settings. Uses the collector->double conversion function
    ;; above. Returns a list of doubles.
    (define (process-datelist accounts dates income?)
      (map 
       (lambda (date)
         (collector->double
          ((if inc-exp?
               (if income?
                   gnc:accounts-get-comm-total-income
                   gnc:accounts-get-comm-total-expense)
               gnc:accounts-get-comm-total-assets)
           accounts 
           (lambda (account)
             (if inc-exp?
                 ;; for inc-exp, 'date' is a pair of time values, else
                 ;; it is a time value.
                 (gnc:account-get-comm-balance-interval 
                  account (first date) (second date) #f)
                 (gnc:account-get-comm-balance-at-date 
                  account date #f))))
          (if inc-exp? (second date) date)))
       dates))

    (gnc:report-percent-done 1)
    (set! commodity-list (gnc:accounts-get-commodities 
                          (append 
                           (gnc:acccounts-get-all-subaccounts accounts)
                           accounts)
                          report-currency))
    (gnc:report-percent-done 10)
    (set! exchange-fn (gnc:case-exchange-time-fn 
                       price-source report-currency 
                       commodity-list to-date-tp
		       10 40))
    (gnc:report-percent-done 50)

    (if 
     (not (null? accounts))
     (let* ((assets-list #f)
            (liability-list #f)
            (net-list #f)
            (date-string-list (map 
                               (if inc-exp?
                                   (lambda (date-list-item)
                                     (gnc-print-date
                                      (car date-list-item)))
                                   gnc-print-date)
                               dates-list)))

       (set! assets-list
             (process-datelist
              (if inc-exp? 
                  accounts
                  (assoc-ref classified-accounts ACCT-TYPE-ASSET))
              dates-list #t))
       (gnc:report-percent-done 70)
       (set! liability-list
             (process-datelist
              (if inc-exp?
                  accounts
                  (assoc-ref classified-accounts ACCT-TYPE-LIABILITY))
              dates-list #f))
       (gnc:report-percent-done 80)
       (set! net-list
             (map + assets-list liability-list))
       (gnc:report-percent-done 90)
          
       (gnc:html-barchart-set-title! 
        chart report-title)
       (gnc:html-barchart-set-subtitle!
        chart (sprintf #f
                       (_ "%s to %s")
                       (gnc-print-date from-date-tp)
                       (gnc-print-date to-date-tp)))
       (gnc:html-barchart-set-width! chart width)
       (gnc:html-barchart-set-height! chart height)
       (gnc:html-barchart-set-row-labels! chart date-string-list)
       (gnc:html-barchart-set-y-axis-label!
        chart (gnc-commodity-get-mnemonic report-currency))
       ;; Determine whether we have enough space for horizontal labels
       ;; -- kind of a hack. Assumptions: y-axis labels and legend
       ;; require 200 pixels, and each x-axes label needs 60 pixels.
       (gnc:html-barchart-set-row-labels-rotated?! 
        chart (< (/ (- width 200) 
                    (length date-string-list)) 60))
       
       ;; Add the data
       (if show-sep?
           (begin
             (add-column! assets-list)
             (add-column!		      ;;(if inc-exp?
              (map - liability-list)
              ;;liability-list)
              )))
       (if show-net?
           (add-column! net-list))

       ;; Legend labels, colors
       (gnc:html-barchart-set-col-labels! 
        chart (append
               (if show-sep?
                   (if inc-exp?
                       (list (_ "Income") (_ "Expense"))
                       (list (_ "Assets") (_ "Liabilities")))
                   '())
               (if show-net?
                   (if inc-exp?
                       (list (_ "Net Profit"))
                       (list (_ "Net Worth")))
                   '())))
       (gnc:html-barchart-set-col-colors! 
        chart (append
               (if show-sep?
                   '("blue" "red") '())
               (if show-net?
                   '("green") '())))
       
       ;; URLs for income/expense or asset/liabilities bars.
       (if show-sep?
           (let ((urls
                  (list
                   (gnc:make-report-anchor
                    (if inc-exp?
                        "Income Over Time"
                        "Assets Over Time")
                    report-obj
                    (list 
                     (list gnc:pagename-display
                           "Use Stacked Bars" #t)
                     (list gnc:pagename-general
                           gnc:optname-reportname
                           (if inc-exp?
                               (_ "Income Chart")
                               (_ "Asset Chart")))))
                   (gnc:make-report-anchor
                    (if inc-exp?
                        "Expense Over Time"
                        "Liabilities Over Time")
                    report-obj
                    (list 
                     (list gnc:pagename-display
                           "Use Stacked Bars" #t)
                     (list gnc:pagename-general
                           gnc:optname-reportname
                           (if inc-exp?
                               (_ "Expense Chart")
                               (_ "Liability Chart"))))))))
             (gnc:html-barchart-set-button-1-bar-urls! 
              chart urls)
             (gnc:html-barchart-set-button-1-legend-urls! 
              chart urls)))
       
       ;; Test for all-zero data here.
       (if non-zeros
           (begin
           (gnc:html-document-add-object! document chart) 
             (if show-table?
             (let ((table (gnc:make-html-table)))
                (gnc:html-table-set-col-headers!
                 table
                 (append
                  (list (_ "Date"))
                  (if show-sep?
                      (if inc-exp?
                          (list (_ "Income") (_ "Expense"))
                          (list (_ "Assets") (_ "Liabilities")))
                      '())
                  (if show-net?
                      (if inc-exp?
                          (list (_ "Net Profit"))
                          (list (_ "Net Worth")))
                      '()))
                 )
               (gnc:html-table-append-column! table date-string-list)
               (if show-sep?
                   (begin
                     (gnc:html-table-append-column! table assets-list)
                     (gnc:html-table-append-column! table liability-list)
                    )
                   )
               (if show-net?
                   (gnc:html-table-append-column! table net-list)
                   )
               ;; set numeric columns to align right
                (for-each
                 (lambda (col)
                   (gnc:html-table-set-col-style!
                    table col "td"
                    'attribute (list "align" "right")))
                 '(1 2 3))

              (gnc:html-document-add-object! document table))
             ))
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
	     report-title (gnc:report-id report-obj)))))
     
     ;; else no accounts selected
     (gnc:html-document-add-object!
      document
      (gnc:html-make-no-account-warning 
       report-title (gnc:report-id report-obj))))
    
    (gnc:report-finished)
    document))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name (N_ "Net Worth Barchart")
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator (lambda () (options-generator #f))
 'renderer (lambda (report-obj) (net-renderer report-obj #f)))

(gnc:define-report
 'version 1
 'name reportname
 'menu-name (N_ "Income & Expense Chart")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator (lambda () (options-generator #t))
 'renderer (lambda (report-obj) (net-renderer report-obj #t)))
