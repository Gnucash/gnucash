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
(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report" 0)

(define reportname
  (N_ "Budget Chart"))

(define optname-accounts (N_ "Accounts"))
(define optname-budget (N_ "Budget"))

(define optname-running-sum (N_ "Running Sum"))
(define optname-chart-type (N_ "Chart Type"))
(define opthelp-chart-type (N_ "Select which chart type to use"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed."))

(define optname-budget-period-start (N_ "Range start"))
(define opthelp-budget-period-start
  (N_ "Select a budget period type that starts the reporting range."))
(define optname-budget-period-start-exact (N_ "Exact start period"))
(define opthelp-budget-period-start-exact
  (N_ "Select exact period that starts the reporting range."))

(define optname-budget-period-end (N_ "Range end"))
(define opthelp-budget-period-end
  (N_ "Select a budget period type that ends the reporting range."))
(define optname-budget-period-end-exact (N_ "Exact end period"))
(define opthelp-budget-period-end-exact
  (N_ "Select exact period that ends the reporting range."))

(define (options-generator)
  (let* ((options (gnc:new-options))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; Option to select Budget
    (add-option (gnc:make-budget-option
        gnc:pagename-general optname-budget
        "a" (N_ "Budget to use.")))

    ;; options to select budget period
    (let ((period-options
           (list (vector 'first
                         (N_ "First")
                         (N_ "The first period of the budget"))
                 (vector 'previous
                         (N_ "Previous")
                         (N_ "Budget period was before current period, according to report evaluation date"))
                 (vector 'current
                         (N_ "Current")
                         (N_ "Current period, according to report evaluation date"))
                 (vector 'next
                         (N_ "Next")
                         (N_ "Next period, according to report evaluation date"))
                 (vector 'last
                         (N_ "Last")
                         (N_ "Last budget period"))
                 (vector 'manual
                         (N_ "Manual period selection")
                         (N_ "Explicitly select period value with spinner below"))))
          (start-period 'first)
          (end-period 'last))

      (add-option
       (gnc:make-multichoice-callback-option
        gnc:pagename-general optname-budget-period-start
        "g1.1" opthelp-budget-period-start start-period
        period-options
        #f
        (lambda (new-val)
          (gnc-option-db-set-option-selectable-by-name
           options gnc:pagename-general optname-budget-period-start-exact
           (eq? new-val 'manual))
          (set! end-period new-val))))

      (add-option
       (gnc:make-number-range-option
        gnc:pagename-general optname-budget-period-start-exact
        "g1.2" opthelp-budget-period-start-exact
        1 1 60 0 1))

      (add-option
       (gnc:make-multichoice-callback-option
        gnc:pagename-general optname-budget-period-end
        "g2.1" opthelp-budget-period-end end-period
        period-options
        #f
        (lambda (new-val)
          (gnc-option-db-set-option-selectable-by-name
           options gnc:pagename-general optname-budget-period-end-exact
           (eq? new-val 'manual))
          (set! end-period new-val))))

      (add-option
       (gnc:make-number-range-option
        gnc:pagename-general optname-budget-period-end-exact
        "g2.2" opthelp-budget-period-end-exact
        1 1 60 0 1)))

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
      opthelp-chart-type                    ;; option help text
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
     optname-plot-width optname-plot-height
     "c" (cons 'percent 80) (cons 'percent 80))

    ;; Set default page
    (gnc:options-set-default-section options gnc:pagename-general)

    ;; Return options
    options))


;; For each period in the budget:
;; Retrieve the budgeted running sum and actual running sum
;; for bar chart.
;;
;; Create bar and values
;;
(define (gnc:chart-create-budget-actual budget acct running-sum chart-type width height
                                        startperiod endperiod)
  (define curr (xaccAccountGetCommodity acct))
  (define (amount->monetary amount)
    (gnc:monetary->string
     (gnc:make-gnc-monetary curr amount)))
  (let ((chart (gnc:make-html-chart)))
    (gnc:html-chart-set-type! chart (if (eq? chart-type 'bars) 'bar 'line))
    (gnc:html-chart-set-title! chart (xaccAccountGetName acct))
    (gnc:html-chart-set-width! chart width)
    (gnc:html-chart-set-height! chart height)
    (gnc:html-chart-set-currency-iso! chart (gnc-commodity-get-mnemonic curr))
    (gnc:html-chart-set-currency-symbol! chart (gnc-commodity-get-nice-symbol curr))
    (gnc:html-chart-set-y-axis-label! chart (gnc-commodity-get-mnemonic curr))

    ;; disable animation; with multiple accounts selected this report
    ;; will create several charts, all will want to animate
    (gnc:html-chart-set! chart '(options animation duration) 0)
    (gnc:html-chart-set! chart '(options hover animationDuration) 0)
    (gnc:html-chart-set! chart '(options responsiveAnimationDuration) 0)

    ;; loop though periods
    (let loop ((periods (iota (gnc-budget-get-num-periods budget)))
               (bgt-sum 0)
               (act-sum 0)
               (bgt-vals '())
               (act-vals '())
               (dates-list '()))

      (cond
       ((null? periods)
        (gnc:html-chart-add-data-series! chart
                                         (_ "Budget")
                                         (reverse bgt-vals)
                                         "#0074D9"
                                         'fill (eq? chart-type 'bars))
        (gnc:html-chart-add-data-series! chart
                                         (_ "Actual")
                                         (reverse act-vals)
                                         "#FF4136"
                                         'fill (eq? chart-type 'bars))
        (gnc:html-chart-set-data-labels! chart (reverse dates-list))
        (when running-sum
          (gnc:html-chart-set-title!
           chart
           (list (xaccAccountGetName acct)
                 ;; Translators: Bgt and Act refer to budgeted and
                 ;; actual total amounts.
                 (format #f (_ "Bgt: ~a Act: ~a")
                         (amount->monetary bgt-sum)
                         (amount->monetary act-sum))))))
       (else
        (let* ((period (car periods))
               (bgt-sum (+ (gnc:get-account-period-rolledup-budget-value
                            budget acct period)
                           (if running-sum bgt-sum 0)))
               (act-sum (+ (gnc-budget-get-account-period-actual-value
                            budget acct period)
                           (if running-sum act-sum 0))))
          (if (<= startperiod period endperiod)
              (loop (cdr periods)
                    bgt-sum
                    act-sum
                    (cons bgt-sum bgt-vals)
                    (cons act-sum act-vals)
                    (cons (qof-print-date
                           (gnc-budget-get-period-start-date budget period))
                          dates-list))
              (loop (cdr periods)
                    bgt-sum
                    act-sum
                    bgt-vals
                    act-vals
                    dates-list))))))

    ;; Return newly created chart
    chart))

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

  (define (curr-period budget)
    (let ((now (current-time))
          (max-period (1- (gnc-budget-get-num-periods budget))))
      (let loop ((period 0))
        (cond
         ((< now (gnc-budget-get-period-end-date budget period)) period)
         ((<= max-period period) period)
         (else (loop (1+ period)))))))

  (define (option->period period budget manual-period)
    (let ((max-period (1- (gnc-budget-get-num-periods budget))))
      (min max-period
           (max 0
                (case period
                  ((first) 0)
                  ((previous) (1- (curr-period budget)))
                  ((current) (curr-period budget))
                  ((next) (1+ (curr-period budget)))
                  ((last) max-period)
                  ((manual) (1- manual-period)))))))

  (let* ((budget (get-option gnc:pagename-general optname-budget))
         (budget-valid? (and budget (not (null? budget))))
         (running-sum (get-option gnc:pagename-display optname-running-sum))
         (chart-type (get-option gnc:pagename-display optname-chart-type))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (depth-limit (get-option gnc:pagename-accounts optname-depth-limit))
         (report-title (get-option gnc:pagename-general gnc:optname-reportname))
         (start-period (get-option gnc:pagename-general optname-budget-period-start))
         (start-period-exact (and budget-valid?
                                  (option->period
                                   start-period budget
                                   (get-option
                                    gnc:pagename-general
                                    optname-budget-period-start-exact))))
         (end-period (get-option gnc:pagename-general optname-budget-period-end))
         (end-period-exact (and budget-valid?
                                (option->period
                                 end-period budget
                                 (get-option
                                  gnc:pagename-general
                                  optname-budget-period-end-exact))))
         (document (gnc:make-html-document)))

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
          (if (or (and (eq? depth-limit 'all)
                       (null? (gnc-account-get-descendants acct)))
                  (and (not (eq? depth-limit 'all))
                       (<= (gnc-account-get-current-depth acct) depth-limit)
                       (null? (gnc-account-get-descendants acct)))
                  (and (not (eq? depth-limit 'all))
                       (= (gnc-account-get-current-depth acct) depth-limit)))
              (gnc:html-document-add-object!
               document
               (gnc:chart-create-budget-actual
                budget acct running-sum chart-type
                width height
                (min start-period-exact end-period-exact)
                (max start-period-exact end-period-exact)))))
        accounts)))
    
    document))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "415cd38d39054d9e9c4040455290c2b1"
 'menu-path (list gnc:menuname-budget)
 'options-generator (lambda () (options-generator))
 'renderer (lambda (report-obj) (net-renderer report-obj)))
