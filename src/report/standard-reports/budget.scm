;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget.scm: budget report
;;
;; (C) 2005 by Chris Shoemaker <c.shoemaker@cox.net>
;;
;; based on cash-flow.scm by:
;; Herbert Thoma <herbie@hthoma.de>
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

(define-module (gnucash report standard-reports budget))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))
(use-modules (gnucash engine))

(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define reportname (N_ "Budget Report"))

;; define all option's names so that they are properly defined
;; in *one* place.
;;(define optname-from-date (N_ "Start Date"))
;;(define optname-to-date (N_ "End Date"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-select-columns (N_ "Select Columns"))
(define optname-show-budget (N_ "Show Budget"))
(define opthelp-show-budget (N_ "Display a column for the budget values."))
(define optname-show-actual (N_ "Show Actual"))
(define opthelp-show-actual (N_ "Display a column for the actual values."))
(define optname-show-difference (N_ "Show Difference"))
(define opthelp-show-difference (N_ "Display the difference as budget - actual."))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals."))
(define optname-rollup-budget (N_ "Roll up budget amounts to parent"))
(define opthelp-rollup-budget (N_ "If parent account does not have its own budget value, use the sum of the child account budget values."))
(define optname-show-zb-accounts (N_ "Include accounts with zero total balances and budget values"))
(define opthelp-show-zb-accounts (N_ "Include accounts with zero total (recursive) balances and budget values in this report."))


(define optname-use-budget-period-range
  (N_ "Report for range of budget periods"))
(define opthelp-use-budget-period-range
  (N_ "Create report for a budget period range instead of the entire budget."))

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

(define optname-period-collapse-before (N_ "Include collapsed periods before selected."))
(define opthelp-period-collapse-before (N_ "Include in report previous periods as single collapsed column (one for all periods before starting)"))
(define optname-period-collapse-after (N_ "Include collapsed periods after selected."))
(define opthelp-period-collapse-after (N_ "Include in report further periods as single collapsed column (one for all periods after ending and to the end of budget range)"))

(define optname-bottom-behavior (N_ "Flatten list to depth limit"))
(define opthelp-bottom-behavior
  (N_ "Displays accounts which exceed the depth limit at the depth limit."))

(define optname-budget (N_ "Budget"))

;;List of common helper functions, that is not bound only to options generation or report evaluation
(define (get-option-val options pagename optname)
    (gnc:option-value
       (gnc:lookup-option options pagename optname)))

(define (set-option-enabled options page opt-name enabled) 
        (gnc-option-db-set-option-selectable-by-name
          options page opt-name enabled))

;; options generator
(define (budget-report-options-generator)
  (let* ( (options (gnc:new-options)) 
          (add-option 
           (lambda (new-option)
             (gnc:register-option options new-option)))
          (period-options (list (list->vector
                                 (list 'first
                                       (N_ "First")
                                       (N_ "The first period of the budget")))
                                (list->vector
                                 (list 'previous
                                       (N_ "Previous")
                                       (N_ "Budget period was before current period, according to report evaluation date")))
                                (list->vector
                                 (list 'current
                                       (N_ "Current")
                                       (N_ "Current period, according to report evaluation date")))
                                (list->vector
                                 (list 'next
                                       (N_ "Next")
                                       (N_ "Next period, according to report evaluation date")))
                                (list->vector
                                 (list 'last
                                       (N_ "Last")
                                       (N_ "Last budget period")))
                                (list->vector
                                 (list 'manual
                                       (N_ "Manual period selection")
                                       (N_ "Explicitly select period valud with spinner below")))))
          (ui-use-periods #f)
          (ui-start-period-type 'current)
          (ui-end-period-type 'next)
        )

    (gnc:register-option
     options
     (gnc:make-budget-option
      gnc:pagename-general optname-budget
      "a" (N_ "Budget to use.")))

    ;; date interval
    ;;(gnc:options-add-date-interval!
    ;; options gnc:pagename-general
    ;; optname-from-date optname-to-date "a")

    (gnc:options-add-price-source!
     options gnc:pagename-general optname-price-source "c" 'average-cost)

    ;;(gnc:register-option
    ;; options
    ;; (gnc:make-simple-boolean-option
    ;;  gnc:pagename-general optname-show-rates
    ;;  "d" (N_ "Show the exchange rates used") #f))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts).") #t))

    (add-option
     (gnc:make-complex-boolean-option
      gnc:pagename-general
      optname-use-budget-period-range
      "f"
      opthelp-use-budget-period-range
      #f
      #f
      ;; Make period only option widgets
      ;; selectable only when we are running the report for a budget period
      ;; range.
      (lambda (value)
          (let (
                (enabler (lambda (target-opt enabled) 
                      (set-option-enabled options gnc:pagename-general target-opt enabled)))
               )
            (for-each (lambda (target-opt) 
                      (enabler target-opt value))
                (list optname-budget-period-start optname-budget-period-end 
                        optname-period-collapse-before optname-period-collapse-after)
            )
            (enabler optname-budget-period-start-exact 
                     (and value 
                          (eq? 'manual ui-start-period-type)))
            (enabler optname-budget-period-end-exact 
                     (and value 
                          (eq? 'manual ui-end-period-type)))
            (set! ui-use-periods value)
        ))))

    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-start
      "g1.1" opthelp-budget-period-start 'current
      period-options
      #f
      (lambda (new-val)
              (set-option-enabled options gnc:pagename-general 
                                  optname-budget-period-start-exact 
                                  (and ui-use-periods (eq? 'manual new-val)))
              (set! ui-start-period-type new-val)
              )      
      ))
    
    (add-option
     (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-start-exact
      "g1.2" opthelp-budget-period-start-exact
      ;; FIXME: It would be nice if the max number of budget periods (60) was
      ;; defined globally somewhere so we could reference it here.  However, it
      ;; only appears to be defined currently in src/gnome/glade/budget.glade.
      1 1 60 0 1))
    
    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-end
      "g2.1" opthelp-budget-period-end 'next
      period-options
      #f
      (lambda (new-val)
              (set-option-enabled options gnc:pagename-general 
                                  optname-budget-period-end-exact 
                                  (and ui-use-periods (eq? 'manual new-val)))
              (set! ui-end-period-type new-val)
              )      
      ))
    
    (add-option
     (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-end-exact
      "g2.2" opthelp-budget-period-end-exact
      ;; FIXME: It would be nice if the max number of budget periods (60) was
      ;; defined globally somewhere so we could reference it here.  However, it
      ;; only appears to be defined currently in src/gnome/glade/budget.glade.
      1 1 60 0 1))
    ;; accounts to work on

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-period-collapse-before
      "g3" opthelp-period-collapse-before #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-period-collapse-after
      "g4" opthelp-period-collapse-after #t))

    (gnc:options-add-account-selection!
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type
        (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY ACCT-TYPE-INCOME
                          ACCT-TYPE-EXPENSE)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior #f))

    ;; columns to display
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-budget
      "s1" opthelp-show-budget #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-actual
      "s2" opthelp-show-actual #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-difference
      "s3" opthelp-show-difference #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-totalcol
      "s4" opthelp-show-totalcol #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-rollup-budget
      "s4" opthelp-rollup-budget #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-zb-accounts
      "s5" opthelp-show-zb-accounts #t))

      ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options)
  )

;; Create the html table for the budget report
;;
;; Parameters
;;   html-table - HTML table to fill in
;;   acct-table - Table of accounts to use
;;   budget - budget to use
;;   params - report parameters
(define (gnc:html-table-add-budget-values!
         html-table acct-table budget params)
  (let* ((get-val (lambda (alist key)
                    (let ((lst (assoc-ref alist key)))
                      (if lst (car lst) lst))))
         (show-actual? (get-val params 'show-actual))
         (show-budget? (get-val params 'show-budget))
         (show-diff? (get-val params 'show-difference))
         (show-totalcol? (get-val params 'show-totalcol))
         (rollup-budget? (get-val params 'rollup-budget))
         (num-rows (gnc:html-acct-table-num-rows acct-table))
         (rownum 0)
         (numcolumns (gnc:html-table-num-columns html-table))
     ;;(html-table (or html-table (gnc:make-html-table)))
         ;; WARNING: we implicitly depend here on the details of
         ;; gnc:html-table-add-account-balances.  Specifically, we
         ;; assume that it makes twice as many columns as it uses for
          ;; account labels.  For now, that seems to be a valid
         ;; assumption.
         (colnum (quotient numcolumns 2))

         )

        (define (negative-numeric-p x)
                (if (gnc-numeric-p x) (gnc-numeric-negative-p x) #f))
        (define (number-cell-tag x)
                (if (negative-numeric-p x) "number-cell-neg" "number-cell"))
        (define (total-number-cell-tag x)
                (if (negative-numeric-p x) "total-number-cell-neg" "total-number-cell"))

  ;; Calculate the value to use for the budget of an account for a specific set of periods.
  ;; If there is 1 period, use that period's budget value.  Otherwise, sum the budgets for
  ;; all of the periods.
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   acct - account
  ;;   periodlist - list of budget periods to use
  ;;
  ;; Return value:
  ;;   Budget sum
        (define (gnc:get-account-periodlist-budget-value budget acct periodlist)
          (cond
           ((= (length periodlist) 1) (gnc:get-account-period-rolledup-budget-value budget acct (car periodlist)))
           (else (gnc-numeric-add (gnc:get-account-period-rolledup-budget-value budget acct (car periodlist))
                                  (gnc:get-account-periodlist-budget-value budget acct (cdr periodlist))
                                  GNC-DENOM-AUTO GNC-RND-ROUND))
           )
          )

  ;; Calculate the value to use for the actual of an account for a specific set of periods.
  ;; This is the sum of the actuals for each of the periods.
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   acct - account
  ;;   periodlist - list of budget periods to use
  ;;
  ;; Return value:
  ;;   Budget sum
        (define (gnc:get-account-periodlist-actual-value budget acct periodlist)
          (cond
           ((= (length periodlist) 1)
            (gnc-budget-get-account-period-actual-value budget acct (car periodlist)))
           (else
            (gnc-numeric-add
             (gnc-budget-get-account-period-actual-value budget acct (car periodlist))
             (gnc:get-account-periodlist-actual-value budget acct (cdr periodlist))
             GNC-DENOM-AUTO GNC-RND-ROUND))
           )
          )

  ;; Adds a line to tbe budget report.
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   rownum - row number
  ;;   colnum - starting column number
  ;;   budget - budget to use
  ;;   acct - account being displayed
  ;;   rollup-budget? - rollup budget values for account children if account budget not set
  ;;   exchange-fn - exchange function (not used)
        (define (gnc:html-table-add-budget-line!
                 html-table rownum colnum budget acct rollup-budget? column-list exchange-fn)
          (let* (
                 (current-col (+ colnum 1))
                 (bgt-total (gnc-numeric-zero))
                 (bgt-total-unset? #t)
                 (act-total (gnc-numeric-zero))
                 (comm (xaccAccountGetCommodity acct))
                 (reverse-balance? (gnc-reverse-balance acct))
                 (income-acct? (eq? (xaccAccountGetType acct) ACCT-TYPE-INCOME))
                 )

      ;; Displays a set of budget column values
      ;;
      ;; Parameters
      ;;   html-table - html table being created
      ;;   rownum - row number
	  ;;   total? - is this a set of total columns
      ;;   bgt-numeric-val - budget value, or #f if column not to be shown
      ;;   act-numeric-val - actual value, or #f if column not to be shown
      ;;   dif-numeric val - difference value, or #f if column not to be shown
                (define (gnc:html-table-display-budget-columns!
                         html-table rownum total? bgt-numeric-val act-numeric-val dif-numeric-val)
                  (let* ((bgt-val #f)(act-val #f)(dif-val #f)
                         (style-tag (if total? "total-number-cell" "number-cell"))
                         (style-tag-neg (string-append style-tag "-neg"))
                         )
                     (if show-budget?
                       (begin
                         (set! bgt-val (if (gnc-numeric-zero-p bgt-numeric-val) "."
                                   (gnc:make-gnc-monetary comm bgt-numeric-val)))
                         (gnc:html-table-set-cell/tag!
                          html-table rownum current-col style-tag bgt-val)
                         (set! current-col (+ current-col 1))
                       )
                     )
                     (if show-actual?
                       (begin
                         (set! act-val (gnc:make-gnc-monetary comm act-numeric-val))
                         (gnc:html-table-set-cell/tag!
                          html-table rownum current-col
                          (if (gnc-numeric-negative-p act-numeric-val) style-tag-neg style-tag)
                          act-val)
                         (set! current-col (+ current-col 1))
                       )
                     )
                     (if show-diff?
                       (begin
                         (set! dif-val
                           (if (and (gnc-numeric-zero-p bgt-numeric-val) (gnc-numeric-zero-p act-numeric-val))
                             "."
                             (gnc:make-gnc-monetary comm dif-numeric-val)))
                         (gnc:html-table-set-cell/tag!
                          html-table rownum current-col
                          (if (gnc-numeric-negative-p dif-numeric-val) style-tag-neg style-tag)
                          dif-val)
                         (set! current-col (+ current-col 1))
                       )
                     )
                  )
                );;end of define gnc:html-table-display-budget-columns

      ;; Adds a set of column values to the budget report for a specific list
      ;; of periods.
      ;;
      ;; Parameters:
      ;;   html-table - html table being created
      ;;   rownum - row number
      ;;   budget - budget to use
      ;;   acct - account being displayed
      ;;   period-list - list of periods to use
                (define (gnc:html-table-add-budget-line-columns!
                         html-table rownum budget acct period-list)
                  (let* (
                      ;; budgeted amount
                      (bgt-numeric-val (gnc:get-account-periodlist-budget-value budget acct period-list))
            
                      ;; actual amount
                      (act-numeric-abs (gnc:get-account-periodlist-actual-value budget acct period-list))
                      (act-numeric-val
                        (if reverse-balance?
                          (gnc-numeric-neg act-numeric-abs)
                          act-numeric-abs))
            
                      ;; difference (budget to actual)
                      (dif-numeric-val
                        (gnc-numeric-sub
                          bgt-numeric-val act-numeric-val
                          GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER)))
                    )
          
                    (if (not (gnc-numeric-zero-p bgt-numeric-val))
                      (begin
                        (set! bgt-total (gnc-numeric-add bgt-total bgt-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
                        (set! bgt-total-unset? #f))
                    )
                    (set! act-total (gnc-numeric-add act-total act-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
                    (if income-acct?
                      (set! dif-numeric-val
                        (gnc-numeric-sub
                          act-numeric-val bgt-numeric-val
                          GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))))
                    (gnc:html-table-display-budget-columns!
                             html-table rownum #f
                             bgt-numeric-val act-numeric-val dif-numeric-val)
                  )
                );;end of define gnc:html-table-add-budget-line-columns

                (while (not (null? column-list))
                 (let* ((col-info (car column-list)))
                       (cond
                        ((equal? col-info 'total)
                         (gnc:html-table-display-budget-columns! 
                              html-table rownum #t bgt-total act-total
                              (if income-acct?
                                 (gnc-numeric-sub
                                    act-total bgt-total
                                    GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))
                                 (gnc-numeric-sub
                                    bgt-total act-total
                                    GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER)))
                          ))
                        ((list? col-info)
                         (gnc:html-table-add-budget-line-columns! html-table rownum budget acct col-info))
                        (else
                         (gnc:html-table-add-budget-line-columns! html-table rownum budget acct (list col-info)))
                        )
                       (set! column-list (cdr column-list))
                 )
                )
          )
        );; end of define gnc:html-table-add-budget-line

  ;; Adds header rows to the budget report.  The columns are specified by the
  ;; column-list parameter.
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   colnum - starting column number
  ;;   budget - budget to use
  ;;   column-list - column info list
        (define (gnc:html-table-add-budget-headers!
                 html-table colnum budget column-list)
                (let* (
                       (current-col (+ colnum 1))
                       (col-list column-list)
                       (col-span 0)
                      )

                      (if show-budget? (set! col-span (+ col-span 1)))
                      (if show-actual? (set! col-span (+ col-span 1)))
                      (if show-diff? (set! col-span (+ col-span 1)))
                      (if (eqv? col-span 0) (set! col-span 1))

                      ;; prepend 2 empty rows
                      (gnc:html-table-prepend-row! html-table '())
                      (gnc:html-table-prepend-row! html-table '())

                      (while (not (= (length col-list) 0))
                             (let* (
                                    (col-info (car col-list))
                                    (tc #f)
                                    (period-to-date-string (lambda (p) (gnc-print-date (gnc-budget-get-period-start-date budget p))))
                                   )
                                   (cond
                                    ((equal? col-info 'total)
                                     (gnc:html-table-set-cell! html-table 0 current-col (_ "Total"))
                                    )
                                    ((list? col-info)
                                     (gnc:html-table-set-cell!
                                      html-table 0 current-col (string-append 
                                                                    (period-to-date-string (car col-info)) 
                                                                    " &ndash; " 
                                                                    (period-to-date-string (car (reverse col-info)))
                                                               )
                                     )
                                    )
                                    (else
                                     (gnc:html-table-set-cell!
                                      html-table 0 current-col (period-to-date-string col-info)
                                     )
                                    )
                                   )
                                   (set! tc (gnc:html-table-get-cell html-table 0 current-col))
                                   (gnc:html-table-cell-set-colspan! tc col-span)
                                   (gnc:html-table-cell-set-tag! tc "centered-label-cell")
                                   (set! current-col (+ current-col 1))
                                   (set! col-list (cdr col-list))
                              )
                      )

                      ;; make the column headers
                      (set! col-list column-list)
                      (set! current-col (+ colnum 1))
                      (while (not (= (length column-list) 0))
                          (let* ((col-info (car column-list)))
                                (if show-budget?
                                  (begin
                                    (gnc:html-table-set-cell/tag!
                                     html-table 1 current-col "centered-label-cell"
                                     (_ "Bgt")) ;; Translators: Abbreviation for "Budget"
                                    (set! current-col (+ current-col 1))
                                  )
                                )
                                (if show-actual?
                                  (begin 
                                    (gnc:html-table-set-cell/tag!
                                     html-table 1 current-col "centered-label-cell"
                                     (_ "Act")) ;; Translators: Abbreviation for "Actual"
                                    (set! current-col (+ current-col 1))
                                  )
                                )
                                (if show-diff?
                                  (begin 
                                    (gnc:html-table-set-cell/tag!
                                     html-table 1 current-col "centered-label-cell"
                                     (_ "Diff")) ;; Translators: Abbrevation for "Difference"
                                    (set! current-col (+ current-col 1))
                                  )
                                )
                                (set! column-list (cdr column-list))
                        )
                      )
                )
        );;end of define gnc:html-table-add-budget-headers
           
        ;; Determines the budget period relative to current period. Budget period is current if
        ;; it start time <= current time and end time >= current time
        ;; When period is found it's passed to adjuster that is responsible for final calculation of period.
        ;; 
        ;; If budget in future then first period of bundget is returned, if it in past, then the last period is returned
        ;; if adjuster produced period number that is less then first period or greater then last period, the same rules apply.
        ;;
        ;; Parameters:
        ;;   budget - budget to use
        ;;   adjuster - function that is used for calculation of period relative to current
        (define (find-period-relative-to-current budget adjuster)
            (let* ((now (current-time))
                  (total-periods (gnc-budget-get-num-periods budget) )
                  (last-period (- total-periods 1))
                  (period-start (lambda (x) (car (gnc-budget-get-period-start-date budget x))))
                  (period-end (lambda (x) (car (gnc-budget-get-period-end-date budget x))))
                 )
                (cond ((< now (period-start 0)) 1)
                      ((> now (period-end last-period)) total-periods)
                      ( else (let ((found-period 
                                        (find (lambda (period)
                                                      (and (>= now (period-start period)) 
                                                           (<= now (period-end period))))
                                              (iota total-periods))
                                  ))
                                  (gnc:debug "current period =" found-period)
                                  (if found-period 
                                      (let ((adjusted (adjuster found-period)))
                                            (cond ((< adjusted 0) 0) ((> adjusted last-period) last-period) (else adjusted))
                                      )
                                      #f)
                     ))
                )
            )
        );;end of find-period-relative-to-current
        ;; Maps type of user selected period to concrete period number, if user not selected to use range false is returned
        (define (calc-user-period budget
                  use-ranges? period-type period-exact-val )
                (if (not use-ranges?)
                    #f
                    (cond
                        ((eq? 'first period-type) 0)
                        ((eq? 'last period-type) (- (gnc-budget-get-num-periods budget) 1))
                        ((eq? 'manual period-type) (- period-exact-val 1))
                        ((eq? 'previous period-type) 
                            (find-period-relative-to-current budget (lambda (period) (- period 1))))
                        ((eq? 'current period-type) 
                            (find-period-relative-to-current budget (lambda (period) period )))
                        ((eq? 'next period-type)
                            (find-period-relative-to-current budget (lambda (period) ( + period 1))))
                    )
                )
        );;end of calc-user-period budget
        ;; Performs calculation of periods list. If list element is a list itself, it means that 
        ;; elements of this sublist should be presented as summed value.
        ;; If user required a total column calculation a quoted total val appended to the end
        ;; For example if function produced list ( (0 1 2 3 4) 5 6 7 (8 9) 'total) then budget report will 
        ;; have 6 columns:
        ;; -- first column is a sum of values for periods 0..4
        ;; -- second .. forth columns is a values for periods 5,6,7
        ;; -- fifth is a sum of value for periods 8, 9
        ;; -- sixth a column with total of all columns
        ;;
        ;; Total is calculated only for selected periods. So if the list resulted in (3 4 'total), total column
        ;; will contain the sum of values for periods 3,4
        (define (calc-periods 
                 budget user-start user-end collapse-before? collapse-after? show-total?)
                 

                (define (range start end)
                  (define (int-range current end step lst)
                        (if (>=  current end)
                            lst
                            (int-range (+ current step) end step (cons current lst))))
                  (reverse (int-range (if (number? start) start 0) end 1 '()))
                )
  
                (let* ((num-periods (gnc-budget-get-num-periods budget))
                       (range-start (if user-start user-start 0))
                       (range-end (if user-end (+ 1 user-end) num-periods))
                       (fold-before-start 0)
                       (fold-before-end (if collapse-before? range-start 0))
                       (fold-after-start (if collapse-after? range-end num-periods))
                       (fold-after-end num-periods)
                       )
                      (map (lambda (x) (if (and (list? x) (= 1 (length x))) (car x) x))
                           (filter (lambda (x) (not (null? x)))
                                    (append (list (range fold-before-start fold-before-end))
                                            (range range-start range-end)
                                            (list (range fold-after-start fold-after-end))
                                            (if show-total? (list 'total) '()) 
                        )))
                )
        );;end of define calc-periods

       ;; end of defines
                         
         
        (let* ((rownum 0)
               (use-ranges? (get-val params 'use-ranges))
               (column-info-list (calc-periods budget 
                                               (calc-user-period budget
                                                                  use-ranges?
                                                                  (get-val params 'user-start-period)
                                                                  (get-val params 'user-start-period-exact)
                                               )
                                               (calc-user-period budget
                                                                  use-ranges?
                                                                  (get-val params 'user-end-period)
                                                                  (get-val params 'user-end-period-exact)
                                               )
                                               (get-val params 'collapse-before)
                                               (get-val params 'collapse-after)
                                               show-totalcol?
                                 ))
               ;;(html-table (or html-table (gnc:make-html-table)))
               ;; WARNING: we implicitly depend here on the details of
               ;; gnc:html-table-add-account-balances.  Specifically, we
               ;; assume that it makes twice as many columns as it uses for
               ;; account labels.  For now, that seems to be a valid
               ;; assumption.
              )
              ;;debug output for control of period list calculation
              (gnc:debug "use-ranges? =" use-ranges?)
              (gnc:debug "user-start-period =" (get-val params 'user-start-period))
              (gnc:debug "user-start-period-exact =" (get-val params 'user-start-period-exact))
              (gnc:debug "user-end-period =" (get-val params 'user-end-period))
              (gnc:debug "user-end-period-exact =" (get-val params 'user-end-period-exact))
              (gnc:debug "column-info-list=" column-info-list)

              ;; call gnc:html-table-add-budget-line! for each account
              (while (< rownum num-rows)
                     (let* (
                            (env (append (gnc:html-acct-table-get-row-env acct-table rownum) params))
                            (acct (get-val env 'account))
                            (exchange-fn (get-val env 'exchange-fn))
                           )
                           (gnc:html-table-add-budget-line!
                            html-table rownum colnum budget acct rollup-budget? column-info-list exchange-fn)
                           (set! rownum (+ rownum 1)) ;; increment rownum
                     )
              ) ;; end of while

              ;; column headers
              (gnc:html-table-add-budget-headers! html-table colnum budget column-info-list)
        )
  )
) ;; end of define gnc:html-table-add-budget-values

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-renderer
;; set up the document and add the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-renderer report-obj)
  (define (get-option pagename optname)
    (get-option-val (gnc:report-options report-obj) pagename optname))
;;     (gnc:lookup-option
;;      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((budget (get-option gnc:pagename-general optname-budget))
         (budget-valid? (and budget (not (null? budget))))
         (display-depth (get-option gnc:pagename-accounts
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (bottom-behavior (get-option gnc:pagename-accounts optname-bottom-behavior))
         (rollup-budget? (get-option gnc:pagename-display
                                     optname-rollup-budget))
         (show-zb-accts? (get-option gnc:pagename-display
                                     optname-show-zb-accounts))
         (use-ranges? (get-option gnc:pagename-general optname-use-budget-period-range))
         (include-collapse-before? (if use-ranges?
                                       (get-option gnc:pagename-general optname-period-collapse-before) #f))
         (include-collapse-after? (if use-ranges?
                                       (get-option gnc:pagename-general optname-period-collapse-after) #f))
         (row-num 0) ;; ???
         (work-done 0)
         (work-to-do 0)
         ;;(report-currency (get-option gnc:pagename-general
         ;;                             optname-report-currency))
         (show-full-names? (get-option gnc:pagename-general
                                       optname-show-full-names))
         (doc (gnc:make-html-document))
         ;;(table (gnc:make-html-table))
         ;;(txt (gnc:make-html-text))
         )

    ;; end of defines

    ;; add subaccounts if requested
    (if show-subaccts?
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account accounts))
                  (set! accounts (append accounts sub-accounts))))
            sub-accounts)))

    (cond
      ((null? accounts)
        ;; No accounts selected.
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning reportname (gnc:report-id report-obj))))
      ((not budget-valid?)
        ;; No budget selected.
        (gnc:html-document-add-object!
          doc (gnc:html-make-generic-budget-warning reportname)))
      (else (begin
        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))
               (to-period-val (lambda (v) (inexact->exact (truncate (get-option gnc:pagename-general v)))))
               ;;(account-disp-list '())

               (env (list 
                     (list 'start-date (gnc:budget-get-start-date budget))
                     (list 'end-date (gnc:budget-get-end-date budget))
                     (list 'display-tree-depth tree-depth)
                     (list 'depth-limit-behavior 
                           (if bottom-behavior 'flatten 'summarize))
                     (list 'zero-balance-mode 
                           (if show-zb-accts? 'show-leaf-acct 'omit-leaf-acct))
                     (list 'report-budget budget)
                          ))
               (acct-table #f)
               (html-table (gnc:make-html-table))
               (params '())
               (paramsBudget (list
                              (list 'show-actual
                                    (get-option gnc:pagename-display optname-show-actual))
                              (list 'show-budget
                                    (get-option gnc:pagename-display optname-show-budget))
                              (list 'show-difference
                                    (get-option gnc:pagename-display optname-show-difference))
                              (list 'show-totalcol
                                    (get-option gnc:pagename-display optname-show-totalcol))
                              (list 'rollup-budget
                                    (get-option gnc:pagename-display optname-rollup-budget))
                              (list 'use-ranges use-ranges?)
                              (list 'collapse-before include-collapse-before?)
                              (list 'collapse-after include-collapse-after?)
                              (list 'user-start-period (get-option gnc:pagename-general optname-budget-period-start))
                              (list 'user-end-period (get-option gnc:pagename-general optname-budget-period-end))
                              (list 'user-start-period-exact (to-period-val optname-budget-period-start-exact))
                              (list 'user-end-period-exact (to-period-val optname-budget-period-end-exact))
                              ))
               (report-name (get-option gnc:pagename-general
                                        gnc:optname-reportname))
               )

          (gnc:html-document-set-title!
           doc (sprintf #f (_ "%s: %s")
                        report-name (gnc-budget-get-name budget)))

          (set! accounts (sort accounts account-full-name<?))

          (set! acct-table
                (gnc:make-html-acct-table/env/accts env accounts))

          ;; We do this in two steps: First the account names...  the
          ;; add-account-balances will actually compute and add a
          ;; bunch of current account balances, too, but we'll
          ;; overwrite them.
          (set! html-table (gnc:html-table-add-account-balances
                            #f acct-table params))

          ;; ... then the budget values
          (gnc:html-table-add-budget-values!
           html-table acct-table budget paramsBudget)

          ;; hmmm... I expected that add-budget-values would have to
          ;; clear out any unused columns to the right, out to the
          ;; table width, since the add-account-balance had put stuff
          ;; there, but it doesn't seem to matter.

          (gnc:html-document-add-object! doc html-table))))
      ) ;; end cond

    (gnc:report-finished)
    doc))

(gnc:define-report
 'version 1.1
 'name reportname
 'report-guid "810ed4b25ef0486ea43bbd3dddb32b11"
 'menu-path (list gnc:menuname-budget)
 'options-generator budget-report-options-generator
 'renderer budget-renderer)
