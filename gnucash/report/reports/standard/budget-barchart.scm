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

(define-module (gnucash reports standard budget-barchart))

(use-modules (srfi srfi-1))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define reportname
  (N_ "Budget Chart"))

(define optname-accounts (N_ "Accounts"))
(define optname-budget (N_ "Budget"))

(define optname-running-sum (N_ "Running Sum"))
(define optname-chart-type (N_ "Chart Type"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed."))

;(define (options-generator inc-exp?)
(define (options-generator)
  (let* (
      (options (gnc:new-options)) 
      ;; This is just a helper function for making options.
      ;; See libgnucash/scm/options.scm for details.
      (add-option 
        (lambda (new-option)
          (gnc:register-option options new-option)))
    )
    ;; Option to select Budget
    (add-option (gnc:make-budget-option
        gnc:pagename-general optname-budget
        "a" (N_ "Budget to use.")))

    ;; date interval
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "b")

    ;; Option to select the accounts to that will be displayed
    (add-option (gnc:make-account-list-option
        gnc:pagename-accounts optname-accounts
        "c" (N_ "Report on these accounts.")
        (lambda ()
	  (gnc:filter-accountlist-type
	    (list ACCT-TYPE-BANK ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY)
	    (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
        #f #t))

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-depth-limit
     "d" opthelp-depth-limit 6)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display 
      optname-running-sum
      "a"
      (N_ "Calculate as running sum?")
      #t))

    ;; Display tab
    (add-option
      (gnc:make-multichoice-option
        gnc:pagename-display                  ;; tab name
        optname-chart-type                    ;; displayed option name
        "b"                                   ;; localization in the tab
        (N_ "This is a multi choice option.") ;; option help text
        'bars                                 ;; default selectioin
        (list
          (vector 'bars
                  (N_ "Barchart")
                  (N_ "Show the report as a bar chart."))
          (vector 'lines
                  (N_ "Linechart")
                  (N_ "Show the report as a line chart.")))))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "c" (cons 'percent 100.0) (cons 'percent 100.0))

    ;; Set default page
    (gnc:options-set-default-section options gnc:pagename-general)

    ;; Return options
    options
))


;; For each period in the budget:
;; Retrieve the budgeted running sum and actual running sum
;; for bar chart.
;;
;; Create bar and values
;;
(define (gnc:chart-create-budget-actual budget acct running-sum chart-type width height report-start-time report-end-time)
  (let* (
          (chart #f)
        )

    (if (eqv? chart-type 'bars)
      (begin
        ;; Setup barchart
        (set! chart (gnc:make-html-barchart))
        (gnc:html-barchart-set-title! chart (xaccAccountGetName acct))
        (gnc:html-barchart-set-width! chart width)
        (gnc:html-barchart-set-height! chart height)
        (gnc:html-barchart-set-row-labels-rotated?! chart #t)
        (gnc:html-barchart-set-col-labels!
          chart (list (_ "Budget") (_ "Actual")))
        (gnc:html-barchart-set-col-colors!
          chart '("#0074D9" "#FF4136"))
      )
      ;; else
      (begin
        ;; Setup linechart
        (set! chart (gnc:make-html-linechart))
        (gnc:html-linechart-set-title! chart (xaccAccountGetName acct))
        (gnc:html-linechart-set-width! chart width)
        (gnc:html-linechart-set-height! chart height)
        (gnc:html-linechart-set-row-labels-rotated?! chart #t)
        (gnc:html-linechart-set-col-labels!
          chart (list (_ "Budget") (_ "Actual")))
        (gnc:html-linechart-set-col-colors!
          chart '("#0074D9" "#FF4136"))
      )
    )

    ;; Prepare vars for running sums, and to loop though periods
    (let* (
        (num-periods (gnc-budget-get-num-periods budget))
        (period 0)
        (bgt-sum 0)
        (act-sum 0)
        (date (gnc-budget-get-period-start-date budget period))
        (bgt-vals '())
        (act-vals '())
        (date-iso-string-list '())
        (save-fmt (qof-date-format-get))
      )

      ;; make sure jqplot receives the date strings in ISO format (Bug763257)
      (qof-date-format-set QOF-DATE-FORMAT-ISO)

      ;; Loop through periods
      (while (< period num-periods)
        ;;add calc new running sums
	(if running-sum
          (begin
            (set! bgt-sum (+ bgt-sum
              (gnc-numeric-to-double
                (gnc:get-account-period-rolledup-budget-value budget acct period))))
	    (set! act-sum (+ act-sum
              (gnc-numeric-to-double
                (gnc-budget-get-account-period-actual-value budget acct period))))
          )
        )
        (if (<= report-start-time date)
	  ;; within reporting period, update the display lists
          (begin
            (if (not running-sum)
              (begin
	        (set! bgt-sum
                  (gnc-numeric-to-double
                    (gnc:get-account-period-rolledup-budget-value budget acct period)))
	        (set! act-sum
                  (gnc-numeric-to-double
                    (gnc-budget-get-account-period-actual-value budget acct period)))
              )
            )
            (set! bgt-vals (append bgt-vals (list bgt-sum)))
            (set! act-vals (append act-vals (list act-sum)))
            (set! date-iso-string-list (append date-iso-string-list (list (qof-print-date date))))
          )
        )
        ;; prepare data for next loop repetition
        (set! period (+ period 1))
        (set! date (gnc-budget-get-period-start-date budget period))
        (if (< report-end-time date)
          (set! period num-periods) ;; reporting period has ended, break the loop
        )
      )

      ;; restore the date strings format
      (qof-date-format-set save-fmt)

      (if (eqv? chart-type 'bars)
        (begin
          ;; Add data to the bar chart
          (gnc:html-barchart-append-column! chart bgt-vals)
          (gnc:html-barchart-append-column! chart act-vals)
          (gnc:html-barchart-set-row-labels! chart date-iso-string-list)
          (if running-sum
              (gnc:html-barchart-set-subtitle!
               chart (format #f "Bgt: ~a Act: ~a" bgt-sum act-sum)))
        )
        ;; else
        (begin
          ;; Add data to the line chart
          (gnc:html-linechart-append-column! chart bgt-vals)
          (gnc:html-linechart-append-column! chart act-vals)
          (gnc:html-linechart-set-row-labels! chart date-iso-string-list)
          (if running-sum
              (gnc:html-linechart-set-subtitle!
               chart
               (format #f "Bgt: ~a Act: ~a" bgt-sum act-sum)))
        )
      )
    )

    ;; Return newly created chart
    chart
))


;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (net-renderer report-obj)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  ;; This is a helper function to find out the level of the account
  ;; with in the account tree
  (define (get-account-level account level)
    (let (
           (parent (gnc-account-get-parent account))
         )
      (cond
        (
          (null? parent) ;; exit
          level
        )
        (else
          (get-account-level parent (+ level 1))
        )
      )
    )
  )

  (let* (
      (budget (get-option gnc:pagename-general optname-budget))
      (budget-valid? (and budget (not (null? budget))))
      (running-sum (get-option gnc:pagename-display optname-running-sum))
      (chart-type (get-option gnc:pagename-display optname-chart-type))
      (height (get-option gnc:pagename-display optname-plot-height))
      (width (get-option gnc:pagename-display optname-plot-width))
      (accounts (get-option gnc:pagename-accounts optname-accounts))
      (depth-limit (get-option gnc:pagename-accounts optname-depth-limit))
      (report-title (get-option gnc:pagename-general
        gnc:optname-reportname))
      (document (gnc:make-html-document))
      (from-date-t64 (gnc:time64-start-day-time
                      (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general optname-from-date))))
      (to-date-t64 (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-to-date))))
    )
    (cond
      ((null? accounts)
        ;; No accounts selected
        (gnc:html-document-add-object!
          document
            (gnc:html-make-no-account-warning 
              report-title (gnc:report-id report-obj))))

      ((not budget-valid?)
        ;; No budget selected.
        (gnc:html-document-add-object!
          document (gnc:html-make-generic-budget-warning reportname)))

      ;; Else create chart for each account
      (else
        (for-each
          (lambda (acct)
            (if (or
                  (and (equal? depth-limit 'all)
                       (null? (gnc-account-get-descendants acct))
                  )
                  (and (not (equal? depth-limit 'all))
                       (<= (get-account-level acct 0) depth-limit)
                       (null? (gnc-account-get-descendants acct))
                  )
                  (and (not (equal? depth-limit 'all))
                       (= (get-account-level acct 0) depth-limit)
                  )
                )
              (gnc:html-document-add-object!
                document
                (gnc:chart-create-budget-actual budget acct running-sum chart-type width height from-date-t64 to-date-t64)
              )
            )
          )
          accounts
        )
      )
    ) ;; end cond
    
    document
))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "415cd38d39054d9e9c4040455290c2b1"
 'menu-path (list gnc:menuname-budget)
 'options-generator (lambda () (options-generator))
 'renderer (lambda (report-obj) (net-renderer report-obj)))
