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

(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash engine))

(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define reportname (N_ "Budget Report"))

;; define all option's names so that they are properly defined
;; in *one* place.

(define optname-display-depth
  (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-select-columns (N_ "Select Columns"))
(define optname-show-budget (N_ "Show Budget"))
(define opthelp-show-budget (N_ "Display a column for the budget values."))
(define optname-show-actual (N_ "Show Actual"))
(define opthelp-show-actual (N_ "Display a column for the actual values."))
(define optname-show-difference (N_ "Show Difference"))
(define opthelp-show-difference (N_ "Display the difference as budget - actual."))
(define optname-accumulate (N_ "Use accumulated amounts"))
(define opthelp-accumulate (N_ "Values are accumulated across periods."))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals."))
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
  (let* ((options (gnc:new-options))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option)))
         (period-options
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
         (ui-use-periods #f)
         (ui-start-period-type 'current)
         (ui-end-period-type 'next))

    (gnc:register-option
     options
     (gnc:make-budget-option
      gnc:pagename-general optname-budget
      "a" (N_ "Budget to use.")))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-accumulate
      "b" opthelp-accumulate #f))

    (add-option
     (gnc:make-complex-boolean-option
      gnc:pagename-general optname-use-budget-period-range
      "f" opthelp-use-budget-period-range #f #f
      (lambda (value)
        (for-each
         (lambda (opt)
           (set-option-enabled options gnc:pagename-general opt value))
         (list optname-budget-period-start optname-budget-period-end
               optname-period-collapse-before optname-period-collapse-after))

        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-start-exact
                            (and value (eq? 'manual ui-start-period-type)))

        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-end-exact
                            (and value (eq? 'manual ui-end-period-type)))

        (set! ui-use-periods value))))

    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-start
      "g1.1" opthelp-budget-period-start 'current period-options #f
      (lambda (new-val)
        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-start-exact
                            (and ui-use-periods (eq? 'manual new-val)))
        (set! ui-start-period-type new-val))))

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
      "g2.1" opthelp-budget-period-end 'next period-options #f
      (lambda (new-val)
        (set-option-enabled options gnc:pagename-general
                            optname-budget-period-end-exact
                            (and ui-use-periods (eq? 'manual new-val)))
        (set! ui-end-period-type new-val))))

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
     options gnc:pagename-accounts optname-display-depth
     optname-show-subaccounts optname-accounts "a" 2
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
      gnc:pagename-display optname-show-zb-accounts
      "s5" opthelp-show-zb-accounts #t))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options))

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
                      (and lst (car lst)))))
         (show-actual? (get-val params 'show-actual))
         (show-budget? (get-val params 'show-budget))
         (show-diff? (get-val params 'show-difference))
         (accumulate? (get-val params 'use-envelope))
         (show-totalcol? (get-val params 'show-totalcol))
         (use-ranges? (get-val params 'use-ranges))
         (num-rows (gnc:html-acct-table-num-rows acct-table))
         (numcolumns (gnc:html-table-num-columns html-table))
         ;; WARNING: we implicitly depend here on the details of
         ;; gnc:html-table-add-account-balances.  Specifically, we
         ;; assume that it makes twice as many columns as it uses for
         ;; account labels.  For now, that seems to be a valid
         ;; assumption.
         (colnum (quotient numcolumns 2)))

    ;; Calculate the value to use for the budget of an account for a
    ;; specific set of periods.  If there is 1 period, use that
    ;; period's budget value.  Otherwise, sum the budgets for all of
    ;; the periods.
    ;;
    ;; Parameters:
    ;;   budget - budget to use
    ;;   acct - account
    ;;   periodlist - list of budget periods to use
    ;;
    ;; Return value:
    ;;   Budget sum
    (define (gnc:get-account-periodlist-budget-value budget acct periodlist)
      (apply +
             (map
              (lambda (period)
                (gnc:get-account-period-rolledup-budget-value budget acct period))
              periodlist)))
    
    ;; Calculate the value to use for the actual of an account for a
    ;; specific set of periods.  This is the sum of the actuals for
    ;; each of the periods.
    ;;
    ;; Parameters:
    ;;   budget - budget to use
    ;;   acct - account
    ;;   periodlist - list of budget periods to use
    ;;
    ;; Return value:
    ;;   Budget sum
    (define (gnc:get-account-periodlist-actual-value budget acct periodlist)
      (apply + (map
                (lambda (period)
                  (gnc-budget-get-account-period-actual-value budget acct period))
                periodlist)))

    ;; Adds a line to the budget report.
    ;;
    ;; Parameters:
    ;;   html-table - html table being created
    ;;   rownum - row number
    ;;   colnum - starting column number
    ;;   budget - budget to use
    ;;   acct - account being displayed
    ;;   exchange-fn - exchange function (not used)
    (define (gnc:html-table-add-budget-line!
             html-table rownum colnum budget acct
             column-list exchange-fn)
      (let* ((comm (xaccAccountGetCommodity acct))
             (reverse-balance? (gnc-reverse-balance acct))
             (maybe-negate (lambda (amt) (if reverse-balance? (- amt) amt)))
             (unreversed? (gnc-using-unreversed-budgets
                           (gnc-get-current-book))) ;fwd-compatibility
             (allperiods (filter number? (gnc:list-flatten column-list)))
             (total-periods (if (and accumulate? (not (null? allperiods)))
                                (iota (1+ (apply max allperiods)))
                                allperiods))
             (income-acct? (eqv? (xaccAccountGetType acct) ACCT-TYPE-INCOME)))

        ;; Displays a set of budget column values
        ;;
        ;; Parameters
        ;;   style-tag - cell style
        ;;   col - starting column to modify in html-table
        ;;   bgt-val - budget value
        ;;   act-val - actual value
        ;;   dif-val - difference value
        ;;
        ;; Returns
        ;;   col - next column
        (define (disp-cols style-tag col0
                           bgt-val act-val dif-val)
          (let* ((col1 (+ col0 (if show-budget? 1 0)))
                 (col2 (+ col1 (if show-actual? 1 0)))
                 (col3 (+ col2 (if show-diff? 1 0))))
            (if show-budget?
                (gnc:html-table-set-cell/tag!
                 html-table rownum col0
                 style-tag
                 (if (zero? bgt-val) "."
                     (gnc:make-gnc-monetary comm bgt-val))))
            (if show-actual?
                (gnc:html-table-set-cell/tag!
                 html-table rownum col1
                 style-tag
                 (gnc:make-gnc-monetary comm act-val)))
            (if show-diff?
                (gnc:html-table-set-cell/tag!
                 html-table rownum col2
                 style-tag
                 (if (and (zero? bgt-val) (zero? act-val)) "."
                     (gnc:make-gnc-monetary comm dif-val))))
            col3))

        (let loop ((column-list column-list)
                   (current-col (1+ colnum)))
          (cond

           ((null? column-list)
            #f)

           ;; fwd-compatibility: the next cond may be removed in master
           ((and (eq? (car column-list) 'total) unreversed?)
            (let* ((bgt-total (maybe-negate
                               (gnc:get-account-periodlist-budget-value
                                budget acct total-periods)))
                   (act-total (maybe-negate
                               (gnc:get-account-periodlist-actual-value
                                budget acct total-periods)))
                   (dif-total (- bgt-total act-total)))
              (loop (cdr column-list)
                    (disp-cols "total-number-cell" current-col
                               bgt-total act-total dif-total))))

           ((eq? (car column-list) 'total)
            (let* ((bgt-total (gnc:get-account-periodlist-budget-value
                               budget acct total-periods))
                   (act-total (gnc:get-account-periodlist-actual-value
                               budget acct total-periods))
                   (act-total (if reverse-balance? (- act-total) act-total))
                   (dif-total (if income-acct?
                                  (- act-total bgt-total)
                                  (- bgt-total act-total))))
              (loop (cdr column-list)
                    (disp-cols "total-number-cell" current-col
                               bgt-total act-total dif-total))))

           ;; fwd-compatibility: the next cond may be removed in master
           (unreversed?
            (let* ((period-list (cond
                                 ((list? (car column-list)) (car column-list))
                                 (accumulate? (iota (1+ (car column-list))))
                                 (else (list (car column-list)))))
                   (bgt-val (maybe-negate
                             (gnc:get-account-periodlist-budget-value
                              budget acct period-list)))
                   (act-val (maybe-negate
                             (gnc:get-account-periodlist-actual-value
                              budget acct period-list)))
                   (dif-val (- bgt-val act-val)))
              (loop (cdr column-list)
                    (disp-cols "number-cell" current-col
                               bgt-val act-val dif-val))))

           (else
            (let* ((period-list (cond
                                 ((list? (car column-list)) (car column-list))
                                 (accumulate? (iota (1+ (car column-list))))
                                 (else (list (car column-list)))))
                   (bgt-val (gnc:get-account-periodlist-budget-value
                             budget acct period-list))
                   (act-abs (gnc:get-account-periodlist-actual-value
                             budget acct period-list))
                   (act-val (if reverse-balance?
                                (- act-abs)
                                act-abs))
                   (dif-val (if income-acct?
                                (- act-val bgt-val)
                                (- bgt-val act-val))))
              (loop (cdr column-list)
                    (disp-cols "number-cell" current-col
                               bgt-val act-val dif-val))))))))

    ;; Adds header rows to the budget report.  The columns are
    ;; specified by the column-list parameter.
    ;;
    ;; Parameters:
    ;;   html-table - html table being created
    ;;   colnum - starting column number
    ;;   budget - budget to use
    ;;   column-list - column info list
    (define (gnc:html-table-add-budget-headers!
             html-table colnum budget column-list)
      (let* ((current-col (1+ colnum))
             (col-span (max 1 (count identity
                                     (list show-budget? show-actual? show-diff?))))
             (period-to-date-string (lambda (p)
                                      (qof-print-date
                                       (gnc-budget-get-period-start-date budget p)))))

        ;; prepend 2 empty rows
        (gnc:html-table-prepend-row! html-table '())
        (gnc:html-table-prepend-row! html-table '())

        (let loop ((column-list column-list)
                   (current-col current-col))
          (unless (null? column-list)
            (gnc:html-table-set-cell!
             html-table 0 current-col
             (cond
              ((eq? (car column-list) 'total)
               (_ "Total"))
              ((list? (car column-list))
               (format #f (_ "~a to ~a")
                       (period-to-date-string (car (car column-list)))
                       (period-to-date-string (last (car column-list)))))
              (else
               (period-to-date-string (car column-list)))))

            (let ((tc (gnc:html-table-get-cell html-table 0 current-col)))
              (gnc:html-table-cell-set-colspan! tc col-span)
              (gnc:html-table-cell-set-tag! tc "centered-label-cell"))

            (loop (cdr column-list)
                  (1+ current-col))))

        ;; make the column headers
        (let loop ((column-list column-list)
                   (col0 current-col))
          (unless (null? column-list)
            (let* ((col1 (+ col0 (if show-budget? 1 0)))
                   (col2 (+ col1 (if show-actual? 1 0)))
                   (col3 (+ col2 (if show-diff? 1 0))))
              (when show-budget?
                (gnc:html-table-set-cell/tag!
                 html-table 1 col0 "centered-label-cell"
                 ;; Translators: Abbreviation for "Budget" amount
                 (_ "Bgt")))
              (when show-actual?
                (gnc:html-table-set-cell/tag!
                 html-table 1 col1 "centered-label-cell"
                 ;; Translators: Abbreviation for "Actual" amount
                 (_ "Act")))
              (when show-diff?
                (gnc:html-table-set-cell/tag!
                 html-table 1 col2 "centered-label-cell"
                 ;; Translators: Abbreviation for "Difference" amount
                 (_ "Diff")))
              (loop (cdr column-list)
                    col3))))))

    ;; Determines the budget period relative to current period. Budget
    ;; period is current if it start time <= current time and end time
    ;; >= current time When period is found it's passed to adjuster
    ;; that is responsible for final calculation of period.
    ;;
    ;; If budget in future then first period of budget is returned,
    ;; if it in past, then the last period is returned if adjuster
    ;; produced period number that is less then first period or
    ;; greater than last period, the same rules apply.
    ;;
    ;; Parameters:
    ;;   budget - budget to use
    ;;   adjuster - function that is used for calculation of period relative to current
    (define (find-period-relative-to-current budget adjuster)
      (let* ((now (current-time))
             (total-periods (gnc-budget-get-num-periods budget))
             (last-period (1- total-periods))
             (period-start (lambda (x) (gnc-budget-get-period-start-date budget x)))
             (period-end (lambda (x) (gnc-budget-get-period-end-date budget x))))
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
    ;; Maps type of user selected period to concrete period number, if
    ;; user not selected to use range false is returned
    (define (calc-user-period budget use-ranges? period-type period-exact-val)
      (and use-ranges?
           (case period-type
            ((first)    0)
            ((last)     (1- (gnc-budget-get-num-periods budget)))
            ((manual)   (1- period-exact-val))
            ((previous) (find-period-relative-to-current budget 1-))
            ((current)  (find-period-relative-to-current budget identity))
            ((next)     (find-period-relative-to-current budget 1+)))))
    ;; Performs calculation of periods list. If list element is a list
    ;; itself, it means that elements of this sublist should be
    ;; presented as summed value.  If user required a total column
    ;; calculation a quoted total val appended to the end For example
    ;; if function produced list ( (0 1 2 3 4) 5 6 7 (8 9) 'total)
    ;; then budget report will have 6 columns:
    ;; -- first column is a sum of values for periods 0..4
    ;; -- second .. forth columns is a values for periods 5,6,7
    ;; -- fifth is a sum of value for periods 8, 9
    ;; -- sixth a column with total of all columns
    ;;
    ;; Total is calculated only for selected periods. So if the list
    ;; resulted in (3 4 'total), total column will contain the sum of
    ;; values for periods 3,4
    (define (calc-periods
             budget user-start user-end collapse-before? collapse-after? show-total?)
      (define (range start end)
        (if (< start end)
            (iota (- end start) start)
            (iota (- start end) end)))
      (let* ((num-periods (gnc-budget-get-num-periods budget))
             (range-start (or user-start 0))
             (range-end (if user-end (1+ user-end) num-periods))
             (fold-before-start 0)
             (fold-before-end (if collapse-before? range-start 0))
             (fold-after-start (if collapse-after? range-end num-periods))
             (fold-after-end num-periods))
        (map (lambda (x) (if (and (list? x) (null? (cdr x))) (car x) x))
             (filter (lambda (x) (not (null? x)))
                     (append (list (range fold-before-start fold-before-end))
                             (range range-start range-end)
                             (list (range fold-after-start fold-after-end))
                             (if show-total? '(total) '()))))))
    ;; end of defines

    (let ((column-info-list (calc-periods
                             budget
                             (calc-user-period
                              budget use-ranges?
                              (get-val params 'user-start-period)
                              (get-val params 'user-start-period-exact))
                             (calc-user-period
                              budget use-ranges?
                              (get-val params 'user-end-period)
                              (get-val params 'user-end-period-exact))
                             (get-val params 'collapse-before)
                             (get-val params 'collapse-after)
                             show-totalcol?))
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
      (let loop ((rownum 0))
        (when (< rownum num-rows)
          (let* ((env (append (gnc:html-acct-table-get-row-env acct-table rownum)
                              params))
                 (acct (get-val env 'account))
                 (exchange-fn (get-val env 'exchange-fn)))
            (gnc:html-table-add-budget-line!
             html-table rownum colnum budget acct
             column-info-list exchange-fn)
            (loop (1+ rownum)))))

      ;; column headers
      (gnc:html-table-add-budget-headers!
       html-table colnum budget column-info-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-renderer
;; set up the document and add the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-renderer report-obj)
  (define (get-option pagename optname)
    (get-option-val (gnc:report-options report-obj) pagename optname))

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
         (show-zb-accts? (get-option gnc:pagename-display
                                     optname-show-zb-accounts))
         (use-ranges? (get-option gnc:pagename-general optname-use-budget-period-range))
         (include-collapse-before? (and use-ranges?
                                        (get-option gnc:pagename-general
                                                    optname-period-collapse-before)))
         (include-collapse-after? (and use-ranges?
                                       (get-option gnc:pagename-general
                                                   optname-period-collapse-after)))
         (doc (gnc:make-html-document))
         (accounts (if show-subaccts?
                       (gnc:accounts-and-all-descendants accounts)
                       accounts)))
    ;; end of defines

    (cond

     ((null? accounts)
      ;; No accounts selected.
      (gnc:html-document-add-object!
       doc (gnc:html-make-no-account-warning reportname (gnc:report-id report-obj))))

     ((not budget-valid?)
      ;; No budget selected.
      (gnc:html-document-add-object!
       doc (gnc:html-make-generic-budget-warning reportname)))

     (else
      (let* ((tree-depth (if (eq? display-depth 'all)
                             (accounts-get-children-depth accounts)
                             display-depth))
             (to-period-val (lambda (v)
                              (inexact->exact
                               (truncate
                                (get-option gnc:pagename-general v)))))
             (env (list
                   (list 'start-date (gnc:budget-get-start-date budget))
                   (list 'end-date (gnc:budget-get-end-date budget))
                   (list 'display-tree-depth tree-depth)
                   (list 'depth-limit-behavior
                         (if bottom-behavior 'flatten 'summarize))
                   (list 'zero-balance-mode
                         (if show-zb-accts? 'show-leaf-acct 'omit-leaf-acct))
                   (list 'report-budget budget)))
             (accounts (sort accounts account-full-name<?))
             (accumulate? (get-option gnc:pagename-general optname-accumulate))
             (acct-table (gnc:make-html-acct-table/env/accts env accounts))
             (paramsBudget
              (list
               (list 'show-actual
                     (get-option gnc:pagename-display optname-show-actual))
               (list 'show-budget
                     (get-option gnc:pagename-display optname-show-budget))
               (list 'show-difference
                     (get-option gnc:pagename-display optname-show-difference))
               (list 'use-envelope accumulate?)
               (list 'show-totalcol
                     (get-option gnc:pagename-display optname-show-totalcol))
               (list 'use-ranges use-ranges?)
               (list 'collapse-before include-collapse-before?)
               (list 'collapse-after include-collapse-after?)
               (list 'user-start-period
                     (get-option gnc:pagename-general
                                 optname-budget-period-start))
               (list 'user-end-period
                     (get-option gnc:pagename-general
                                 optname-budget-period-end))
               (list 'user-start-period-exact
                     (to-period-val optname-budget-period-start-exact))
               (list 'user-end-period-exact
                     (to-period-val optname-budget-period-end-exact))))
             (report-name (get-option gnc:pagename-general
                                      gnc:optname-reportname)))

        (gnc:html-document-set-title!
         doc (format #f "~a: ~a ~a"
                     report-name (gnc-budget-get-name budget)
                     ;; Translators: using accumulated amounts mean
                     ;; budget will report on budgeted and actual
                     ;; amounts from the beginning of budget, instead
                     ;; of only using the budget-period amounts.
                     (if accumulate? (_ "using accumulated amounts")
                         "")))

        ;; We do this in two steps: First the account names...  the
        ;; add-account-balances will actually compute and add a
        ;; bunch of current account balances, too, but we'll
        ;; overwrite them.
        (let ((html-table (gnc:html-table-add-account-balances #f acct-table '())))

          ;; ... then the budget values
          (gnc:html-table-add-budget-values! html-table acct-table budget paramsBudget)

          ;; hmmm... I expected that add-budget-values would have to
          ;; clear out any unused columns to the right, out to the
          ;; table width, since the add-account-balance had put stuff
          ;; there, but it doesn't seem to matter.

          (gnc:html-document-add-object! doc html-table)))))

    (gnc:report-finished)
    doc))

(gnc:define-report
 'version 1.1
 'name reportname
 'report-guid "810ed4b25ef0486ea43bbd3dddb32b11"
 'menu-path (list gnc:menuname-budget)
 'options-generator budget-report-options-generator
 'renderer budget-renderer)
