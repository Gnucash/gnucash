;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category-barchart.scm: shows barchart of income/expense categories
;;  
;; By Christian Stimming <stimming@tu-harburg.de>
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

;; depends must be outside module scope -- and should eventually go away.
(define-module (gnucash reports standard category-barchart))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

;; useful functions
(define (safe-/ x y)
  (if (zero? y) 0 (/ x y)))

;; The option names are defined here to 1. save typing and 2. avoid
;; spelling errors. The *reportnames* are defined here (and not only
;; once at the very end) because I need them to define the "other"
;; report, thus needing them twice.
(define menuname-income (N_ "Income Chart"))
(define menuname-expense (N_ "Expense Chart"))
(define menuname-assets (N_ "Asset Chart"))
(define menuname-liabilities (N_ "Liability Chart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income
  (N_ "Shows a chart with the Income per interval \
developing over time"))
(define menutip-expense
  (N_ "Shows a chart with the Expenses per interval \
developing over time"))
(define menutip-assets
  (N_ "Shows a chart with the Assets developing over time"))
(define menutip-liabilities
  (N_ "Shows a chart with the Liabilities \
developing over time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income Over Time"))
(define reportname-expense (N_ "Expense Over Time"))
(define reportname-assets (N_ "Assets Over Time"))
(define reportname-liabilities (N_ "Liabilities Over Time"))

;; Option names
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Levels of Subaccounts"))

(define optname-fullname (N_ "Show long account names"))

(define optname-chart-type (N_ "Chart Type"))

(define optname-stacked (N_ "Use Stacked Charts"))
(define optname-slices (N_ "Maximum Bars"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

(define (options-generator account-types do-intervals?)
  (let* ((options (gnc:new-options))
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

    (if do-intervals?
        (add-option
         (gnc:make-multichoice-option
          gnc:pagename-general optname-averaging
          "e" opthelp-averaging
          'None
          (list (vector 'None (N_ "No Averaging"))
                (vector 'MonthDelta (N_ "Monthly"))
                (vector 'WeekDelta (N_ "Weekly"))
                (vector 'DayDelta (N_ "Daily"))))))


    ;; Accounts tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (gnc:filter-accountlist-type
         account-types
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (gnc:filter-accountlist-type account-types accounts)))
      #t))

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-levels "c"
     (N_ "Maximum number of levels in the account tree displayed.")
     2)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-chart-type
      "b" "Select which chart type to use."
      'barchart
      (list (vector 'barchart (N_ "Bar Chart"))
            (vector 'linechart (N_ "Line Chart")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-stacked
      "c"
      (N_ "Show charts as stacked charts?")
      #t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "d" (N_ "Maximum number of stacks in the chart.") 8
      2 24 0 1))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "e" (N_ "Display a table of the selected data.")
      #f))

    ;; contributed by https://github.com/exxus
    ;; https://github.com/Gnucash/gnucash/pull/1272
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Percentage chart")
      "e1" (N_ "Display account contributions as a percentage of the total value for the period.")
      #f))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "f" (cons 'percent 100.0) (cons 'percent 100.0))

    (gnc:options-add-sort-method!
     options gnc:pagename-display
     optname-sort-method "g" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.

;; FIXME: the exchange rate should change every time interval, of
;; course, but right now we assume the very last exchange rate to be
;; constant over the whole report period. Note that this might get
;; *really* complicated.

(define (category-barchart-renderer report-obj reportname reportguid
                                    account-types do-intervals? reverse-bal?
                                    export-type)
  ;; A helper functions for looking up option values.
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)
  (let* ((to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (report-title (get-option gnc:pagename-general
                                   gnc:optname-reportname))
         (averaging-selection (if do-intervals?
                                  (get-option gnc:pagename-general
                                              optname-averaging)
                                  'None))

         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (account-levels (get-option gnc:pagename-accounts optname-levels))

         (chart-type (get-option gnc:pagename-display optname-chart-type))
         (stacked? (get-option gnc:pagename-display optname-stacked))
         (show-fullname? (get-option gnc:pagename-display optname-fullname))
         (max-slices (inexact->exact
                      (get-option gnc:pagename-display optname-slices)))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (sort-method (get-option gnc:pagename-display optname-sort-method))

         (work-done 0)
         (work-to-do 0)
         (all-data #f)
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))
         (ratio-chart? (get-option gnc:pagename-display (N_ "Percentage chart")))
         (document (gnc:make-html-document))
         (chart (gnc:make-html-chart))
         (topl-accounts (gnc:filter-accountlist-type
                         account-types
                         (gnc-account-get-children-sorted
                          (gnc-get-current-root-account)))))

    ;; Returns true if the account a was selected in the account
    ;; selection option.
    (define (show-acct? a)
      (member a accounts))

    (define tree-depth (if (eq? account-levels 'all)
                           (gnc:get-current-account-tree-depth)
                           account-levels))

    ;;(gnc:debug accounts)
    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((<= to-date-t64 from-date-t64)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-generic-warning
        report-title (gnc:report-id report-obj)
        (G_ "Invalid dates")
        (G_ "Start date must be earlier than End date"))))

     (else
      (let* ((commodity-list (gnc:accounts-get-commodities
                              (gnc-accounts-and-all-descendants accounts)
                              report-currency))
             (exchange-fn (gnc:case-exchange-time-fn
                           price-source report-currency
                           commodity-list to-date-t64
                           5 15))
             (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
             (interval-fraction-func (gnc:date-get-fraction-func interval))
             (averaging-multiplier
              (if averaging-fraction-func
                  ;; Calculate the divisor of the amounts so that an
                  ;; average is shown. Multiplier factor is a gnc-numeric
                  (let* ((start-frac-avg (averaging-fraction-func from-date-t64))
                         (end-frac-avg (averaging-fraction-func (1+ to-date-t64)))
                         (diff-avg (- end-frac-avg start-frac-avg))
                         (diff-avg-numeric
                          (/ (inexact->exact (round (* diff-avg 1000000)))
                             ;; 6 decimals precision
                             1000000))
                         (start-frac-int (interval-fraction-func from-date-t64))
                         (end-frac-int (interval-fraction-func (1+ to-date-t64)))
                         (diff-int (- end-frac-int start-frac-int))
                         (diff-int-numeric (inexact->exact diff-int)))
                    ;; Extra sanity check to ensure a number smaller than 1
                    (if (> diff-avg diff-int)
                        (/ diff-int-numeric diff-avg-numeric)
                        1))
                  1))
             ;; If there is averaging, the report-title is extended
             ;; accordingly.
             (report-title
              (case averaging-selection
                ((MonthDelta) (string-append report-title " " (G_ "Monthly Average")))
                ((WeekDelta) (string-append report-title " " (G_ "Weekly Average")))
                ((DayDelta) (string-append report-title " " (G_ "Daily Average")))
                (else report-title)))
             (currency-frac (gnc-commodity-get-fraction report-currency))
             ;; This is the list of date intervals to calculate.
             (dates-list (gnc:make-date-list
                          ((if do-intervals?
                               gnc:time64-start-day-time
                               gnc:time64-end-day-time) from-date-t64)
                          (gnc:time64-end-day-time to-date-t64)
                          (gnc:deltasym-to-delta interval)))
             ;; Here the date strings for the x-axis labels are
             ;; created.
             (other-anchor ""))

        ;; Converts a commodity-collector into amount in the report's
        ;; currency using the exchange-fn calculated above. Returns an amount
        ;; multiplied by the averaging-multiplier (smaller than one; multiplication
        ;; instead of division to avoid division-by-zero issues) in case
        ;; the user wants to see the amounts averaged over some value.
        (define (collector->report-currency-amount c date)
          (* averaging-multiplier
             (gnc:gnc-monetary-amount
              (gnc:sum-collector-commodity
               c report-currency
               (lambda (a b) (exchange-fn a b date))))))

        (define acct->name
          (if show-fullname? gnc-account-get-full-name xaccAccountGetName))

        (define (all-zeros data)
          (cond
           ((number? data) (zero? data))
           ((pair? data) (every all-zeros data))
           (else (error 'huh))))

        (define (get-negative-accounts data)
          (let lp ((data data) (retval #f))
            (match data
              (() retval)
              (((acc (? (cut any negative? <>))) . rest)
               (lp rest (cons (acct->name acc) (or retval '()))))
              ((_ . rest) (lp rest retval)))))

        ;; this is an alist of account-balances
        ;; (list (list acc0 bal0 bal1 bal2 ...)
        ;;       (list acc1 bal0 bal1 bal2 ...)
        ;;       ...)
        ;; whereby each balance is a gnc-monetary
        (define account-balances-alist
          (map
           (lambda (acc)
             (let* ((comm (xaccAccountGetCommodity acc))
                    (split->elt (if reverse-bal?
                                    (lambda (s)
                                      (gnc:make-gnc-monetary
                                       comm (- (xaccSplitGetNoclosingBalance s))))
                                    (lambda (s)
                                      (gnc:make-gnc-monetary
                                       comm (xaccSplitGetNoclosingBalance s))))))
               (cons acc
                     (gnc:account-accumulate-at-dates
                      acc dates-list
                      #:split->elt split->elt
                      #:nosplit->elt (gnc:make-gnc-monetary comm 0)))))
           ;; all selected accounts (of report-specific type), *and*
           ;; their descendants (of any type) need to be scanned.
           (gnc-accounts-and-all-descendants accounts)))

        ;; Creates the <balance-list> to be used in the function
        ;; below.
        (define (account->balance-list account subacct?)
          (let* ((accountslist (cons account
                                     (if subacct?
                                         (gnc-account-get-descendants account)
                                         '())))
                 (selected-balances (filter
                                     (lambda (entry)
                                       (member (car entry) accountslist))
                                     account-balances-alist))
                 (selected-monetaries (map cdr selected-balances))
                 (list-of-mon-collectors (apply map gnc:monetaries-add selected-monetaries)))
            (let loop ((list-of-mon-collectors list-of-mon-collectors)
                       (dates-list dates-list)
                       (result '()))
              (if (null? (if do-intervals?
                             (cdr list-of-mon-collectors)
                             list-of-mon-collectors))
                  (reverse result)
                  (loop (cdr list-of-mon-collectors)
                        (cdr dates-list)
                        (cons (if do-intervals?
                                  (collector->report-currency-amount
                                   (gnc:collector- (cadr list-of-mon-collectors)
                                                   (car list-of-mon-collectors))
                                   (cadr dates-list))
                                  (collector->report-currency-amount
                                   (car list-of-mon-collectors)
                                   (car dates-list)))
                              result))))))

        (define (count-accounts current-depth accts)
          (if (< current-depth tree-depth)
              (let ((sum 0))
                (for-each
                 (lambda (a)
                   (set! sum
                     (+ sum (1+ (count-accounts (1+ current-depth)
                                                (gnc-account-get-children a))))))
                 accts)
                sum)
              (length (filter show-acct? accts))))

        ;; Calculates all account's balances. Returns a list of pairs:
        ;; (<account> <balance-list>), like '((Earnings (10.0 11.2))
        ;; (Gifts (12.3 14.5))), where each element of <balance-list>
        ;; is the balance corresponding to one element in
        ;; <dates-list>.
        ;;
        ;; If current-depth >= tree-depth, then the balances are
        ;; calculated *with* subaccount's balances. Else only the
        ;; current account is regarded. Note: All accounts in accts
        ;; and all their subaccounts are processed, but a balances is
        ;; calculated and returned *only* for those accounts where
        ;; show-acct? is true. This is necessary because otherwise we
        ;; would forget an account that is selected but not its
        ;; parent.
        (define (traverse-accounts current-depth accts)
          (if (< current-depth tree-depth)
              (let ((res '()))
                (for-each
                 (lambda (a)
                   (set! work-done (1+ work-done))
                   (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                   (if (show-acct? a)
                       (set! res
                         (cons (list a (account->balance-list a #f))
                               res)))
                   (set! res
                     (append (traverse-accounts
                              (1+ current-depth)
                              (gnc-account-get-children a))
                             res)))
                 accts)
                res)
              ;; else (i.e. current-depth == tree-depth)
              (map
               (lambda (a)
                 (set! work-done (1+ work-done))
                 (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                 (list a (account->balance-list a #t)))
               (filter show-acct? accts))))

        (set! work-to-do (count-accounts 1 topl-accounts))

        ;; Sort the account list according to the account code field.
        (set! all-data
          (sort
           (filter (lambda (l) (not (zero? (apply + (cadr l)))))
                   (traverse-accounts 1 topl-accounts))
           (case sort-method
             ((alphabetical)
              (lambda (a b)
                (gnc:string-locale<? (acct->name (car a))
                                     (acct->name (car b)))))
             ((acct-code)
              (lambda (a b)
                (gnc:string-locale<? (xaccAccountGetCode (car a))
                                     (xaccAccountGetCode (car b)))))
             ((amount)
              (lambda (a b)
                (> (apply + (cadr a))
                   (apply + (cadr b))))))))

        (cond
         ((or (null? all-data) (all-zeros (map cadr all-data)))
          (gnc:html-document-add-object!
           document
           (gnc:html-make-empty-data-warning
            report-title (gnc:report-id report-obj))))

         ((and ratio-chart? (get-negative-accounts all-data)) =>
          (lambda (neg-accounts)
            (gnc:html-document-add-object!
             document
             (gnc:html-make-generic-warning
              report-title (gnc:report-id report-obj)
              "Negative amounts detected"
              "Charting ratios cannot occur on accounts with negative balances. \
Please deselect the accounts with negative balances."))

            (gnc:html-document-add-object!
             document (gnc:make-html-text (gnc:html-markup-ul neg-accounts)))))

         (else
          (let* ((dates-list (if do-intervals?
                                 (list-head dates-list (1- (length dates-list)))
                                 dates-list))
                 (date-string-list (map qof-print-date dates-list))
                 (list-of-rows #f)
                 (row-totals #f))

            ;; Set chart title, subtitle etc.
            (gnc:html-chart-set-type!
             chart (if (eq? chart-type 'barchart) 'bar 'line))

            (gnc:html-chart-set-title!
             chart (list report-title
                         (format #f
                                 (if do-intervals?
                                     (G_ "~a to ~a")
                                     (G_ "Balances ~a to ~a"))
                                 (qof-print-date from-date-t64)
                                 (qof-print-date to-date-t64))))

            (gnc:html-chart-set-width! chart width)
            (gnc:html-chart-set-height! chart height)

            (gnc:html-chart-set-data-labels! chart date-string-list)
            (gnc:html-chart-set-y-axis-label!
             chart (if ratio-chart? "Ratio"
                       (gnc-commodity-get-mnemonic report-currency)))

            ;; If we have too many categories, we sum them into a new
            ;; 'other' category and add a link to a new report with just
            ;; those accounts.
            (if (> (length all-data) max-slices)
                (let* ((start (take all-data (1- max-slices)))
                       (finish (drop all-data (1- max-slices)))
                       (other-sum (map
                                   (lambda (l) (apply + l))
                                   (apply zip (map cadr finish)))))
                  (set! all-data
                    (append start
                            (list (list (G_ "Other") other-sum))))
                  (let* ((options (gnc:make-report-options reportguid)))
                    ;; now copy all the options
                    (gnc:options-copy-values
                     (gnc:report-options report-obj) options)
                    ;; and set the destination accounts
                    (gnc:option-set-value
                     (gnc:lookup-option options gnc:pagename-accounts
                                        optname-accounts)
                     (map car finish))
                    ;; Set the URL to point to this report.
                    (set! other-anchor
                      (gnc:report-anchor-text
                       (gnc:make-report reportguid options))))))

            (set! list-of-rows (apply zip (map cadr all-data)))
            (set! row-totals (map (cut fold + 0 <>) list-of-rows))
            (gnc:report-percent-done 92)

            (for-each
             (lambda (series color stack)
               (let* ((acct (car series))
                      (label (cond
                              ((string? acct) (car series))
                              (else (acct->name acct))))
                      (amounts (cadr series))
                      (stack (if stacked? "default" (number->string stack)))
                      (fill (eq? chart-type 'barchart))
                      (urls (cond
                             ((string? acct) other-anchor)
                             ((null? (gnc-account-get-children acct))
                              (gnc:account-anchor-text acct))

                             ;; because the tree-depth option for
                             ;; accounts/levels goes up to 6. FIXME:
                             ;; magic number.
                             ((>= tree-depth 6) (gnc:account-anchor-text acct))

                             (else
                              (gnc:make-report-anchor
                               reportguid report-obj
                               (list
                                (list gnc:pagename-accounts optname-accounts
                                      (cons acct (gnc-account-get-children acct)))
                                (list gnc:pagename-accounts optname-levels
                                      (1+ tree-depth))
                                (list gnc:pagename-general
                                      gnc:optname-reportname
                                      (acct->name acct))))))))
                 (gnc:html-chart-add-data-series!
                  chart label (if ratio-chart? (map safe-/ amounts row-totals) amounts)
                  color 'stack stack 'fill fill 'urls urls)))
             all-data
             (gnc:assign-colors (length all-data))
             (iota (length all-data)))

            (gnc:html-chart-set-stacking?! chart stacked?)
            (gnc:html-chart-set-currency-iso!
             chart (gnc-commodity-get-mnemonic report-currency))
            (gnc:html-chart-set-currency-symbol!
             chart (gnc-commodity-get-nice-symbol report-currency))
            (gnc:html-chart-set-format-style!
             chart (if ratio-chart? "percent" "currency"))

            (gnc:report-percent-done 98)
            (gnc:html-document-add-object! document chart)

            (when show-table?
              (let ((table (gnc:make-html-table))
                    (scu (gnc-commodity-get-fraction report-currency))
                    (cols>1? (pair? (cdr all-data))))

                (define (make-cell contents)
                  (gnc:make-html-table-cell/markup "number-cell" contents))

                (define (make-cell-percent amt grandt)
                  (gnc:make-html-table-cell/markup "number-cell" (* (safe-/ amt grandt) 100) " %"))

                (define (make-monetary-cell amount)
                  (make-cell (gnc:make-gnc-monetary report-currency amount)))

                (for-each
                 (lambda (date row row-total)
                   (gnc:html-table-append-row!
                    table
                    (append (list (make-cell date))
                            (map (if ratio-chart?
                                     (cut make-cell-percent <> row-total)
                                     make-monetary-cell)
                                 row)
                            (if cols>1?
                                (list (make-monetary-cell (apply + row)))
                                '()))))
                 date-string-list
                 list-of-rows
                 row-totals)

                (gnc:html-table-set-col-headers!
                 table
                 (append
                  (list (G_ "Date"))
                  (map
                   (lambda (col)
                     (cond
                      ((string? col) col)
                      (else (acct->name col))))
                   (map car all-data))
                  (if cols>1?
                      (list (G_ "Grand Total"))
                      '())))

                (gnc:html-document-add-object! document table)))

            (cond
             ((eq? export-type 'csv)
              (let ((iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO)))
                (gnc:html-document-set-export-string
                 document
                 (gnc:lists->csv
                  (cons (append
                         (list (G_ "Date"))
                         (map
                          (lambda (col)
                            (cond
                             ((string? col) col)
                             (else (acct->name col))))
                          (map car all-data))
                         (if (pair? (cdr all-data))
                             (list (G_ "Grand Total"))
                             '()))
                        (map
                         (lambda (date row)
                           (append
                            (list date)
                            row
                            (if (pair? (cdr all-data))
                                (list (apply + row))
                                '())))
                         (map (cut gnc-print-time64 <> iso-date) dates-list)
                         list-of-rows)))))))))))))

    (unless (gnc:html-document-export-string document)
      (gnc:html-document-set-export-error document (G_ "No exportable data")))

    (gnc:report-finished)
    document))

(for-each
 (match-lambda
   ((reportname account-types inc-exp? menuname menutip reverse-bal? uuid)
    (gnc:define-report
     'version 1
     'name reportname
     'report-guid uuid
     'menu-path (if inc-exp?
                    (list gnc:menuname-income-expense)
                    (list gnc:menuname-asset-liability))
     'menu-name menuname
     'menu-tip menutip
     'options-generator (lambda () (options-generator account-types inc-exp?))
     'export-types '(("CSV" . csv))
     'export-thunk (lambda (report-obj export-type)
                     (category-barchart-renderer
                      report-obj reportname uuid account-types inc-exp? reverse-bal?
                      export-type))
     'renderer (lambda (report-obj)
                 (category-barchart-renderer
                  report-obj reportname uuid account-types inc-exp? reverse-bal?
                  #f)))))
 (list
  ;; reportname, account-types, inc-exp?,
  ;; menu-reportname, menu-tip, reverse-bal?, uuid
  (list reportname-income (list ACCT-TYPE-INCOME) #t menuname-income menutip-income #t category-barchart-income-uuid)
  (list reportname-expense (list ACCT-TYPE-EXPENSE) #t menuname-expense menutip-expense #f category-barchart-expense-uuid)
  (list reportname-assets
        (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
              ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
              ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
              ACCT-TYPE-CURRENCY)
        #f menuname-assets menutip-assets #f category-barchart-asset-uuid)
  (list reportname-liabilities
        (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
              ACCT-TYPE-CREDITLINE)
        #f menuname-liabilities menutip-liabilities #t category-barchart-liability-uuid)))

