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
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

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
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long account names"))

(define optname-chart-type (N_ "Chart Type"))

(define optname-stacked (N_ "Use Stacked Charts"))
(define optname-slices (N_ "Maximum Bars"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

(define (options-generator account-types reverse-balance? do-intervals?)
  (let* ((options (gnc:new-options))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; save off the reverse-balance option
    (add-option
     (gnc:make-internal-option "__report" "reverse-balance?" reverse-balance?))

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
          (list (vector 'None
                        (N_ "No Averaging")
                        (N_ "Just show the amounts, without any averaging."))
                (vector 'MonthDelta
                        (N_ "Monthly")
                        (N_ "Show the average monthly amount during the reporting period."))
                (vector 'WeekDelta
                        (N_ "Weekly")
                        (N_ "Show the average weekly amount during the reporting period."))
                (vector 'DayDelta
                        (N_ "Daily")
                        (N_ "Show the average daily amount during the reporting period."))))))


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
     (N_ "Show accounts to this depth and not further.")
     2)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-chart-type
      "b" "Select which chart type to use"
      'barchart
      (list (vector 'barchart
                    (N_ "Bar Chart")
                    (N_ "Use bar charts."))
            (vector 'linechart
                    (N_ "Line Chart")
                    (N_ "Use line charts.")))))

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
                                    account-types do-intervals?)
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
         (reverse-balance? (get-option "__report" "reverse-balance?"))

         (work-done 0)
         (work-to-do 0)
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))
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
    (if (not (null? accounts))

        ;; Define more helper variables.
        (let* ((commodity-list #f)
               (exchange-fn #f)
               (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
               (interval-fraction-func (gnc:date-get-fraction-func interval))
               (averaging-multiplier
                (if averaging-fraction-func
                    ;; Calculate the divisor of the amounts so that an
                    ;; average is shown. Multiplier factor is a gnc-numeric
                    (let* ((start-frac-avg (averaging-fraction-func from-date-t64))
                           (end-frac-avg (averaging-fraction-func (1+ to-date-t64)))
                           (diff-avg (- end-frac-avg start-frac-avg))
                           (diff-avg-numeric (/ (inexact->exact (round (* diff-avg 1000000))) ; 6 decimals precision
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
                  ((MonthDelta) (string-append report-title " " (_ "Monthly Average")))
                  ((WeekDelta) (string-append report-title " " (_ "Weekly Average")))
                  ((DayDelta) (string-append report-title " " (_ "Daily Average")))
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
               (other-anchor "")
               (all-data '()))

          ;; Converts a commodity-collector into gnc-monetary in the report's
          ;; currency using the exchange-fn calculated above. Returns a gnc-monetary
          ;; multiplied by the averaging-multiplier (smaller than one; multiplication
          ;; instead of division to avoid division-by-zero issues) in case
          ;; the user wants to see the amounts averaged over some value.
          (define (collector->monetary c date)
            (gnc:make-gnc-monetary
             report-currency
             (* averaging-multiplier
                (gnc:gnc-monetary-amount
                 (gnc:sum-collector-commodity
                  c report-currency
                  (lambda (a b) (exchange-fn a b date)))))))

          ;; copy of gnc:not-all-zeros using gnc-monetary
          (define (not-all-zeros data)
            (cond ((gnc:gnc-monetary? data) (not (zero? (gnc:gnc-monetary-amount data))))
                  ((list? data) (or-map not-all-zeros data))
                  (else #f)))

          ;; this is an alist of account-balances
          ;; (list (list acc0 bal0 bal1 bal2 ...)
          ;;       (list acc1 bal0 bal1 bal2 ...)
          ;;       ...)
          ;; whereby each balance is a gnc-monetary
          (define account-balances-alist
            (map
             (lambda (acc)
               (let* ((comm (xaccAccountGetCommodity acc))
                      (split->elt (if reverse-balance?
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
             (gnc:accounts-and-all-descendants accounts)))

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
                                    (collector->monetary
                                     (gnc:collector- (cadr list-of-mon-collectors)
                                                     (car list-of-mon-collectors))
                                     (cadr dates-list))
                                    (collector->monetary
                                     (car list-of-mon-collectors)
                                     (car dates-list)))
                                result))))))

          (define (count-accounts current-depth accts)
            (if (< current-depth tree-depth)
                (let ((sum 0))
                  (for-each
                   (lambda (a)
                     (set! sum (+ sum (1+ (count-accounts (1+ current-depth)
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
                     (begin
                       (set! work-done (1+ work-done))
                       (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                       (if (show-acct? a)
                           (set! res
                             (cons (list a (account->balance-list a #f))
                                   res)))
                       (set! res (append
                                  (traverse-accounts
                                   (1+ current-depth)
                                   (gnc-account-get-children a))
                                  res))))
                   accts)
                  res)
                ;; else (i.e. current-depth == tree-depth)
                (map
                 (lambda (a)
                   (set! work-done (1+ work-done))
                   (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                   (list a (account->balance-list a #t)))
                 (filter show-acct? accts))))


          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
          (gnc:report-percent-done 1)
          (set! commodity-list (gnc:accounts-get-commodities
                                (gnc:accounts-and-all-descendants accounts)
                                report-currency))
          (set! exchange-fn (gnc:case-exchange-time-fn
                             price-source report-currency
                             commodity-list to-date-t64
                             5 15))

          (set! work-to-do (count-accounts 1 topl-accounts))

          ;; Sort the account list according to the account code field.
          (set! all-data
            (sort
             (filter (lambda (l)
                       (not (zero? (gnc:gnc-monetary-amount
                                    (apply gnc:monetary+ (cadr l))))))
                     (traverse-accounts 1 topl-accounts))
             (case sort-method
               ((alphabetical)
                (lambda (a b)
                  (if show-fullname?
                      (string<? (gnc-account-get-full-name (car a))
                                (gnc-account-get-full-name (car b)))
                      (string<? (xaccAccountGetName (car a))
                                (xaccAccountGetName (car b))))))
               ((acct-code)
                (lambda (a b)
                  (string<? (xaccAccountGetCode (car a))
                            (xaccAccountGetCode (car b)))))
               ((amount)
                (lambda (a b)
                  (> (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr a)))
                     (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr b)))))))))

          ;; Proceed if the data is non-zeros
          (if
           (and (not (null? all-data))
                (not-all-zeros  (map cadr all-data)))

           (let ((dates-list (if do-intervals?
                                 (list-head dates-list (1- (length dates-list)))
                                 dates-list))
                 (date-string-list (map qof-print-date dates-list)))
             ;; Set chart title, subtitle etc.

             (gnc:html-chart-set-type!
              chart (if (eq? chart-type 'barchart) 'bar 'line))

             (gnc:html-chart-set-title!
              chart (list report-title
                          (format #f
                                  (if do-intervals?
                                      (_ "~a to ~a")
                                      (_ "Balances ~a to ~a"))
                                  (qof-print-date from-date-t64)
                                  (qof-print-date to-date-t64))))

             (gnc:html-chart-set-width! chart width)
             (gnc:html-chart-set-height! chart height)

             (gnc:html-chart-set-data-labels! chart date-string-list)
             (gnc:html-chart-set-y-axis-label!
              chart (gnc-commodity-get-mnemonic report-currency))

             ;; If we have too many categories, we sum them into a new
             ;; 'other' category and add a link to a new report with just
             ;; those accounts.
             (if (> (length all-data) max-slices)
                 (let* ((start (take all-data (1- max-slices)))
                        (finish (drop all-data (1- max-slices)))
                        (other-sum (map
                                    (lambda (l) (apply gnc:monetary+ l))
                                    (apply zip (map cadr finish)))))
                   (set! all-data
                     (append start
                             (list (list (_ "Other") other-sum))))
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

             (gnc:report-percent-done 92)

             (for-each
              (lambda (series color stack)
                (let* ((acct (car series))
                       (label (cond
                               ((string? acct)
                                (car series))
                               (show-fullname?
                                (gnc-account-get-full-name acct))
                               (else (xaccAccountGetName acct))))
                       (amounts (map gnc:gnc-monetary-amount (cadr series)))
                       (stack (if stacked?
                                  "default"
                                  (number->string stack)))
                       (fill (eq? chart-type 'barchart))
                       (urls (cond
                              ((string? acct)
                               other-anchor)

                              ((null? (gnc-account-get-children acct))
                               (gnc:account-anchor-text acct))

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
                                       (if show-fullname?
                                           (gnc-account-get-full-name acct)
                                           (xaccAccountGetName acct)))))))))
                  (gnc:html-chart-add-data-series!
                   chart label amounts color
                   'stack stack 'fill fill 'urls urls)))
              all-data
              (gnc:assign-colors (length all-data))
              (iota (length all-data)))

             (gnc:html-chart-set-stacking?! chart stacked?)
             (gnc:html-chart-set-currency-iso!
              chart (gnc-commodity-get-mnemonic report-currency))
             (gnc:html-chart-set-currency-symbol!
              chart (gnc-commodity-get-nice-symbol report-currency))

             (gnc:report-percent-done 98)
             (gnc:html-document-add-object! document chart)

             (when show-table?
               (let ((table (gnc:make-html-table))
                     (scu (gnc-commodity-get-fraction report-currency))
                     (cols>1? (> (length all-data) 1)))

                 (define (make-cell contents)
                   (gnc:make-html-table-cell/markup "number-cell" contents))

                 (define (monetary-round mon)
                   (gnc:make-gnc-monetary
                    report-currency
                    (gnc-numeric-convert
                     (gnc:gnc-monetary-amount mon)
                     scu GNC-HOW-RND-ROUND)))

                 (for-each
                  (lambda (date row)
                    (gnc:html-table-append-row!
                     table (map make-cell (append (list date)
                                                  (map monetary-round row)
                                                  (if cols>1?
                                                      (list
                                                       (monetary-round
                                                        (apply gnc:monetary+ row)))
                                                      '())))))
                  date-string-list
                  (apply zip (map cadr all-data)))

                 (gnc:html-table-set-col-headers!
                  table
                  (append
                   (list (_ "Date"))
                   (map
                    (lambda (col)
                      (cond
                       ((string? col) col)
                       (show-fullname? (gnc-account-get-full-name col))
                       (else (xaccAccountGetName col))))
                    (map car all-data))
                   (if cols>1?
                       (list (_ "Grand Total"))
                       '())))

                 (gnc:html-document-add-object! document table))))

           ;; else if empty data
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
             report-title (gnc:report-id report-obj)))))

        ;; else if no accounts selected
        (gnc:html-document-add-object!
         document
         (gnc:html-make-no-account-warning
          report-title (gnc:report-id report-obj))))

    (gnc:report-finished)
    document))

(define-record-type :variant
  (make-variant reportname acct-types intervals? menuname menutip reverse? uuid)
  variant?
  (reportname get-reportname)
  (acct-types get-acct-types)
  (intervals? get-intervals?)
  (menuname   get-menuname)
  (menutip    get-menutip)
  (reverse?   get-reverse?)
  (uuid       get-uuid))

(define variants
  (list
   (make-variant reportname-income
                 (list ACCT-TYPE-INCOME)
                 #t menuname-income menutip-income
                 #t category-barchart-income-uuid)

   (make-variant reportname-expense
                 (list ACCT-TYPE-EXPENSE)
                 #t menuname-expense menutip-expense
                 #f category-barchart-expense-uuid)

   (make-variant reportname-assets
                 (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
                       ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
                       ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
                       ACCT-TYPE-CURRENCY)
                 #f menuname-assets menutip-assets
                 #f category-barchart-asset-uuid)

   (make-variant reportname-liabilities
                 (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
                       ACCT-TYPE-CREDITLINE)
                 #f menuname-liabilities menutip-liabilities
                 #t category-barchart-liability-uuid)))

(for-each
 (lambda (variant)
   (gnc:define-report
    'version 1
    'name (get-reportname variant)
    'report-guid (get-uuid variant)
    'menu-path (if (get-intervals? variant)
                   (list gnc:menuname-income-expense)
                   (list gnc:menuname-asset-liability))
    'menu-name (get-menuname variant)
    'menu-tip (get-menutip variant)
    'options-generator (lambda ()
                         (options-generator (get-acct-types variant)
                                            (get-reverse? variant)
                                            (get-intervals? variant)))
    'renderer (lambda (report-obj)
                (category-barchart-renderer report-obj
                                            (get-reportname variant)
                                            (get-uuid variant)
                                            (get-acct-types variant)
                                            (get-intervals? variant)))))
 variants)
