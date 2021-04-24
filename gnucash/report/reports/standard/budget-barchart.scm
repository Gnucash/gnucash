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

(define reportname (N_ "Budget Chart"))

(define optname-accounts (N_ "Accounts"))
(define optname-budget (N_ "Budget"))

(define optname-running-sum (N_ "Running Sum"))
(define optname-chart-type (N_ "Chart Type"))
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

(define period-options
  (list (vector 'first (N_ "First budget period"))
        (vector 'previous (N_ "Previous budget period"))
        (vector 'current (N_ "Current budget period"))
        (vector 'next (N_ "Next budget period"))
        (vector 'last (N_ "Last budget period"))
        (vector 'manual (N_ "Manual period selection"))))

(define (options-generator)
  (let ((options (gnc:new-options))
        (ui-start-period-type 'current)
        (ui-end-period-type 'next))

    (define (add-option new-option)
      (gnc:register-option options new-option))

    (define (set-option-enabled options page opt-name enabled)
      (gnc-option-db-set-option-selectable-by-name options page opt-name enabled))

    ;; Option to select Budget
    (add-option
     (gnc:make-budget-option
      gnc:pagename-general optname-budget "a" (N_ "Budget to use.")))

    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-start
      "g1.1" opthelp-budget-period-start 'current period-options #f
      (lambda (new-val)
        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-start-exact (eq? 'manual new-val))
        (set! ui-start-period-type new-val))))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-start-exact
      "g1.2" opthelp-budget-period-start-exact
      1 1 60 0 1))

    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-end
      "g2.1" opthelp-budget-period-end 'next period-options #f
      (lambda (new-val)
        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-end-exact (eq? 'manual new-val))
        (set! ui-end-period-type new-val))))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-end-exact
      "g2.2" opthelp-budget-period-end-exact
      1 1 60 0 1))

    ;; Option to select the accounts to that will be displayed
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "c" (N_ "Report on these accounts.")
      (lambda ()
        (gnc:filter-accountlist-type
         (list ACCT-TYPE-BANK ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-depth-limit "d" opthelp-depth-limit 6)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-running-sum "a"
      (N_ "Calculate as running sum?")  #t))

    ;; Display tab
    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-chart-type "b"
      (N_ "Select which chart type to use.") 'bars
      (list
       (vector 'bars (N_ "Bar Chart"))
       (vector 'lines (N_ "Line Chart")))))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "c"
     (cons 'percent 100) (cons 'percent 100))

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
(define (gnc:chart-create-budget-actual
         budget acct running-sum chart-type width height
         period-start period-end)

  (define chart (gnc:make-html-chart))
  (define num-periods (gnc-budget-get-num-periods budget))
  (define curr (xaccAccountGetCommodity acct))
  (define (amount->monetary amount)
    (gnc:monetary->string
     (gnc:make-gnc-monetary curr amount)))
  (let lp ((period 0)
           (bgt-sum 0)
           (act-sum 0)
           (bgt-vals '())
           (act-vals '())
           (dates-list '()))
    (cond
     ((>= period num-periods)
      (gnc:html-chart-set-width! chart width)
      (gnc:html-chart-set-height! chart height)
      (gnc:html-chart-set-type! chart (if (eq? chart-type 'bars) 'bar 'line))
      (gnc:html-chart-set-currency-iso! chart (gnc-commodity-get-mnemonic curr))
      (gnc:html-chart-set-currency-symbol! chart (gnc-commodity-get-nice-symbol curr))
      (gnc:html-chart-set-y-axis-label! chart (gnc-commodity-get-mnemonic curr))

      ;; disable animation; with multiple accounts selected this report
      ;; will create several charts, all will want to animate. Initial
      ;; animation is already disabled globally.
      (gnc:html-chart-set! chart '(options hover animationDuration) 0)
      (gnc:html-chart-set! chart '(options responsiveAnimationDuration) 0)
      (gnc:html-chart-set-title!
       chart (if running-sum
                 (list (xaccAccountGetName acct)
                       ;; Translators: Bgt and Act refer to budgeted and
                       ;; actual total amounts.
                       (format #f (G_ "Bgt: ~a Act: ~a")
                               (amount->monetary bgt-sum)
                               (amount->monetary act-sum)))
                 (list (xaccAccountGetName acct))))
      (gnc:html-chart-set-data-labels! chart (reverse dates-list))
      (gnc:html-chart-add-data-series! chart
                                       (G_ "Actual")
                                       (reverse act-vals)
                                       "#FF4136"
                                       'fill (eq? chart-type 'bars))
      (gnc:html-chart-add-data-series! chart
                                       (G_ "Budget")
                                       (reverse bgt-vals)
                                       "#0074D9"
                                       'fill (eq? chart-type 'bars))
      chart)
     (else
      (let ((date (gnc-budget-get-period-start-date budget period))
            (new-bgt-sum (+ (gnc:get-account-period-rolledup-budget-value
                             budget acct period)
                            (if running-sum bgt-sum 0)))
            (new-act-sum (+ (gnc-budget-get-account-period-actual-value
                             budget acct period)
                            (if running-sum act-sum 0))))
        (if (<= period-start period period-end)
            (lp (1+ period) new-bgt-sum new-act-sum
                (cons new-bgt-sum bgt-vals)
                (cons new-act-sum act-vals)
                (cons (qof-print-date date) dates-list))
            (lp (1+ period) new-bgt-sum new-act-sum
                bgt-vals
                act-vals
                dates-list)))))))


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
    (let ((parent (gnc-account-get-parent account)))
      (cond
       ((null? parent) level)
       (else (get-account-level parent (+ level 1))))))

  (define (find-period-relative-to-current budget adjuster)
    (define (period-start x) (gnc-budget-get-period-start-date budget x))
    (define (period-end x) (gnc-budget-get-period-end-date budget x))
    (let* ((now (current-time))
           (total-periods (gnc-budget-get-num-periods budget))
           (last-period (1- total-periods)))
      (cond ((< now (period-start 0)) 1)
            ((> now (period-end last-period)) total-periods)
            (else (let ((found-period
                         (find (lambda (period)
                                 (<= (period-start period)
                                     now
                                     (period-end period)))
                               (iota total-periods))))
                    (and found-period
                         (max 0 (min last-period (adjuster found-period)))))))))

  (define (calc-user-period budget period-type period-exact-val)
    (case period-type
      ((first)    0)
      ((last)     (1- (gnc-budget-get-num-periods budget)))
      ((manual)   (1- period-exact-val))
      ((previous) (find-period-relative-to-current budget 1-))
      ((current)  (find-period-relative-to-current budget identity))
      ((next)     (find-period-relative-to-current budget 1+))))

  (define (to-period-val v)
    (inexact->exact (round (get-option gnc:pagename-general v))))

  (let* ((budget (get-option gnc:pagename-general optname-budget))
         (running-sum (get-option gnc:pagename-display optname-running-sum))
         (chart-type (get-option gnc:pagename-display optname-chart-type))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (depth-limit (get-option gnc:pagename-accounts optname-depth-limit))
         (report-title (get-option gnc:pagename-general gnc:optname-reportname))
         (p-start (get-option gnc:pagename-general optname-budget-period-start))
         (p-start-exact (to-period-val optname-budget-period-start-exact))
         (p-end (get-option gnc:pagename-general optname-budget-period-end))
         (p-end-exact (to-period-val optname-budget-period-end-exact))
         (document (gnc:make-html-document)))

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((null? budget)
      (gnc:html-document-add-object!
       document (gnc:html-make-generic-budget-warning reportname)))

     ;; Else create chart for each account
     (else
      (let ((period-start (calc-user-period budget p-start p-start-exact))
            (period-end (calc-user-period budget p-end p-end-exact)))
        (for-each
         (lambda (acct)
           (when (or (and (eq? depth-limit 'all)
                          (null? (gnc-account-get-descendants acct)))
                     (and (not (eq? depth-limit 'all))
                          (<= (get-account-level acct 0) depth-limit)
                          (null? (gnc-account-get-descendants acct)))
                     (and (not (eq? depth-limit 'all))
                          (= (get-account-level acct 0) depth-limit)))
             (gnc:html-document-add-object!
              document (gnc:chart-create-budget-actual
                        budget acct running-sum chart-type width height
                        period-start period-end))))
         accounts))))

    document))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "415cd38d39054d9e9c4040455290c2b1"
 'menu-path (list gnc:menuname-budget)
 'options-generator (lambda () (options-generator))
 'renderer (lambda (report-obj) (net-renderer report-obj)))
