;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balsheet-pnl.scm: multi-column report. includes
;; balance-sheet and p&l reports.
;; 
;; By Christopher Lam, 2018
;;
;; Improved from balance-sheet.scm
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

(define-module (gnucash report standard-reports balsheet-pnl))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/report/report-system" 0)

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-company-name (N_ "Company name"))
(define opthelp-company-name (N_ "Name of company/individual."))

(define optname-startdate (N_ "Start Date"))
(define optname-enddate (N_ "End Date"))

(define optname-period (N_ "Period duration"))
(define opthelp-period (N_ "Duration between time periods"))

(define optname-export (_ "Disable indenting for export?"))
(define opthelp-export (_ "Selecting this option disables indenting for export"))

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts (N_ "Report on these accounts, if display depth allows."))

(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit (N_ "Maximum number of levels in the account tree displayed."))

(define optname-subtotal-mode (N_ "Hierarchical subtotals"))
(define opthelp-subtotal-mode (N_ "This option enables hierarchical subtotals, otherwise parent accounts receive children account balances"))

(define optname-show-zb-accts (N_ "Include accounts with zero total balances"))
(define opthelp-show-zb-accts (N_ "Include accounts with zero total (recursive) balances in this report."))

(define optname-omit-zb-bals (N_ "Omit zero balance figures"))
(define opthelp-omit-zb-bals (N_ "Show blank space in place of any zero balances which would be shown."))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links (N_ "Shows each account in the table as a hyperlink to its register window."))

;; closing entries filter - for P&L report
(define pagename-entries "Entries")
(define optname-closing-pattern (N_ "Closing Entries pattern"))
(define opthelp-closing-pattern (N_ "Any text in the Description column which identifies closing entries."))
(define optname-closing-casing (N_ "Closing Entries pattern is case-sensitive"))
(define opthelp-closing-casing (N_ "Causes the Closing Entries Pattern match to be case-sensitive."))
(define optname-closing-regexp (N_ "Closing Entries Pattern is regular expression"))
(define opthelp-closing-regexp (N_ "Causes the Closing Entries Pattern to be treated as a regular expression."))

;; (define pagename-commodities (N_ "Commodities"))
;; (define optname-report-commodity (N_ "Report's currency"))
;; (define optname-price-source (N_ "Price Source"))
;; (define optname-show-foreign (N_ "Show Foreign Currencies"))
;; (define opthelp-show-foreign (N_ "Display any foreign currency amount in an account."))
;; (define optname-show-rates (N_ "Show Exchange Rates"))
;; (define opthelp-show-rates (N_ "Show the exchange rates used."))

(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")

(define periodlist
  (list
   (cons 'year (list
                (cons 'delta YearDelta)
                (cons 'text (_ "year"))
                (cons 'tip (_ "every year"))))

   (cons 'halfyear (list
                    (cons 'delta HalfYearDelta)
                    (cons 'text (_ "half-year"))
                    (cons 'tip (_ "every half year"))))

   (cons 'quarter (list
                   (cons 'delta QuarterDelta)
                   (cons 'text (_ "quarter"))
                   (cons 'tip (_ "every three months"))))

   (cons 'month (list
                 (cons 'delta MonthDelta)
                 (cons 'text (_ "month"))
                 (cons 'tip (_ "every month"))))

   (cons 'twoweek (list
                   (cons 'delta TwoWeekDelta)
                   (cons 'text (_ "two weeks"))
                   (cons 'tip (_ "every fortnight"))))

   (cons 'week (list
                (cons 'delta WeekDelta)
                (cons 'text (_ "week"))
                (cons 'tip (_ "every 7 days"))))))

(define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))

(define (keylist-get-info keylist key info)
  (cdr (assq info (cdr (assq key keylist)))))

;; options generator
(define (multicol-report-options-generator report-type reportname)
  (let* ((options (gnc:new-options))
         (book (gnc-get-current-book))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-report-title
      "a" opthelp-report-title (_ reportname)))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-company-name
      "b" opthelp-company-name (or (gnc:company-info book gnc:*company-name*) "")))

    ;; date at which to report balance
    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-startdate optname-enddate "c")

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-period
      "c2" opthelp-period
      'halfyear
      (keylist->vectorlist periodlist)))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-export
      "c3" opthelp-export #f))

    #;
    (add-option
    (gnc:make-simple-boolean-option
    gnc:pagename-general optname-single-column
    "d" opthelp-single-column #t))

    ;; accounts to work on
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      opthelp-accounts
      (lambda ()
        (gnc:filter-accountlist-type
         (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CREDIT
               ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY
               ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL ACCT-TYPE-CURRENCY
               ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE
               ACCT-TYPE-EQUITY ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE
               ACCT-TYPE-TRADING)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-depth-limit
     "b" opthelp-depth-limit 'all)

    ;; what to show for zero-balance accounts
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-zb-accts
      "a" opthelp-show-zb-accts #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-omit-zb-bals
      "b" opthelp-omit-zb-bals #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-subtotal-mode 
      "c" opthelp-subtotal-mode #t))

    ;; some detailed formatting options
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-links
      "e" opthelp-account-links #t))
    
    (when (eq? report-type 'pnl)
      ;; closing entry match criteria
      (add-option
       (gnc:make-string-option
        pagename-entries optname-closing-pattern
        "a" opthelp-closing-pattern (_ "Closing Entries")))
      (add-option
       (gnc:make-simple-boolean-option
        pagename-entries optname-closing-casing
        "b" opthelp-closing-casing #f))
      (add-option
       (gnc:make-simple-boolean-option
        pagename-entries optname-closing-regexp
        "c" opthelp-closing-regexp #f)))

    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)

    options))

(define* (add-multicolumn-acct-table
          table title accountlist maxindent get-cell-amount-fn list-of-headers #:key
          (omit-zb-bals? #f)
          (show-zb-accts? #t)
          (disable-indenting? #f)
          (hierarchical-subtotals? #t)
          (depth-limit #f)
          (get-cell-anchor-fn #f))
  ;; table - an existing html-table object
  ;; title - string as the first row
  ;; accountlist - list of accounts
  ;; maxindent - maximum account depth
  ;; list-of-headers - list of string
  ;; get-cell-amount-fn - a lambda (account col-idx) which produces a gnc-monetary

  ;; this function will add a 2D grid into the html-table
  ;; the data cells are generated from (get-cell-amount account col-idx)
  ;; horizontal labels are from list-of-headers
  ;; vertical labels are the account list
  ;; ^ the accountlist will have hierarchical multilevel subtotals displayed

  (define num-columns (length list-of-headers))

  (define (make-list-thunk n thunk)
    (let loop ((result '()) (n n))
      (if (zero? n) result
          (loop (cons (thunk) result) (1- n)))))

  (define (make-narrow-cell)
    (let ((narrow (gnc:make-html-table-cell/markup "text-cell" #f)))
      (gnc:html-table-cell-set-style! narrow "text-cell" 'attribute '("style" "width:1px"))
      narrow))

  (define (add-indented-row indent label label-markup rest)
    (gnc:html-table-append-row!
     table
     (append (if disable-indenting? '() (make-list-thunk indent make-narrow-cell))
             (list (if label-markup
                       (gnc:make-html-table-cell/size/markup 1 (if disable-indenting? 1 (- maxindent indent)) label-markup label)
                       (gnc:make-html-table-cell/size 1 (if disable-indenting? 1 (- maxindent indent)) label)))
             rest)))

  (define (monetary+ . monetaries)
    ;; usage: (monetary+ monetary...)
    ;; inputs: list of gnc-monetary (e.g. USD 10, USD 25, GBP 5, GBP 8)
    ;; outputs: list of gnc-monetary (e.g. USD 35, GBP 13)
    (let ((coll (gnc:make-commodity-collector)))
      (for-each
       (lambda (monetary)
         (coll 'add
               (gnc:gnc-monetary-commodity monetary)
               (gnc:gnc-monetary-amount monetary)))
       monetaries)
      (coll 'format gnc:make-gnc-monetary #f)))

  (define (list-of-monetary->html-text monetaries omit-zero? anchor)
    (let ((text (gnc:make-html-text)))
      (for-each
       (lambda (monetary)
         (if (not (and omit-zero? (zero? (gnc:gnc-monetary-amount monetary))))
             (gnc:html-text-append! text
                                    (if anchor
                                        (gnc:html-markup-anchor anchor monetary)
                                        monetary)
                                    (gnc:html-markup-br))))
       monetaries)
      text))

  (define (add-whole-line contents)
    (gnc:html-table-append-row!
     table (gnc:make-html-table-cell/size 1 (+ 1 (if disable-indenting? 0 maxindent) num-columns) contents)))

  (define labels
    (cons title (make-list (1- maxindent) #f)))

  (define collectors
    (let loop ((result '())
               (list-of-headers list-of-headers))
      (if (null? list-of-headers) result
          (loop (cons (make-list-thunk maxindent gnc:make-commodity-collector)
                      result)
                (cdr list-of-headers)))))

  ;; header ASSET/LIABILITY etc
  (add-indented-row 0
                    title
                    "total-label-cell"
                    (map
                     (lambda (header)
                       (gnc:make-html-table-cell/markup "total-number-cell" header))
                     list-of-headers))

  (let loop ((accountlist accountlist))
    (if (pair? accountlist)
        (let* ((curr (car accountlist))
               (rest (cdr accountlist))
               (next (and (pair? rest) (car rest)))
               (lvl-curr (gnc-account-get-current-depth curr))
               (lvl-next (if next (gnc-account-get-current-depth next) 0))
               (curr-label (xaccAccountGetName curr))
               (curr-commodity (xaccAccountGetCommodity curr))
               (curr-descendants-list (if (not hierarchical-subtotals?)
                                          (gnc-account-get-descendants curr)
                                          '()))
               (curr-balance-display (lambda (idx)
                                       (map (lambda (acc) (get-cell-amount-fn acc idx))
                                            (cons curr curr-descendants-list)))))

          (if (and (or show-zb-accts? (not (every zero? (map (lambda (col-idx)
                                                               (gnc:gnc-monetary-amount
                                                                (get-cell-amount-fn curr col-idx)))
                                                             (iota num-columns)))))
                   (or (not depth-limit) (<= lvl-curr depth-limit)))
              (add-indented-row lvl-curr
                                (string-append curr-label (if (null? curr-descendants-list) "" "+"))
                                "text-cell"
                                (map
                                 (lambda (col-idx)
                                   (gnc:make-html-table-cell/markup
                                    "number-cell" (list-of-monetary->html-text
                                                   (apply monetary+ (curr-balance-display col-idx))
                                                   omit-zb-bals?
                                                   (get-cell-anchor-fn curr col-idx))))
                                 (iota num-columns))))

          (list-set! labels lvl-curr (gnc-account-get-full-name curr))
          ;; where the magic happens. this section will cycle
          ;; through ALL columns. each column will cycle through
          ;; ALL account-depth levels from root to & including
          ;; current account-depth. each column/level collector
          ;; will accumulate account amount.
          (let columns-loop ((col-idx 0))
            (when (< col-idx num-columns)
              (let level-loop ((level 0))
                (when (<= level lvl-curr)
                  (let ((mon (get-cell-amount-fn curr col-idx)))
                    ((list-ref (list-ref collectors col-idx) level) 'add
                     (gnc:gnc-monetary-commodity mon)
                     (gnc:gnc-monetary-amount mon)))
                  (level-loop (1+ level))))
              (columns-loop (1+ col-idx))))

          (cond
           ;; no change in level. reset the current level accumulator.
           ((= lvl-curr lvl-next)
            (let columns-loop ((col-idx 0))
              (when (< col-idx num-columns)
                ((list-ref (list-ref collectors col-idx) lvl-curr) 'reset #f #f)
                (columns-loop (1+ col-idx)))))

           ;; hierarchical subtotals. we're going UP hierarchy
           ;; towards the root. start from the current level (minus
           ;; 1), and cycle until the next account level. add level
           ;; subtotals if conditions are met below. when all
           ;; subtotals complete, add a single empty row before
           ;; moving on to the next (higher level) account.
           ((> lvl-curr lvl-next)
            (let add-subtotal-row ((lvl (1- lvl-curr)))
              (if (< lvl lvl-next)
                  (if hierarchical-subtotals?
                      (add-whole-line #f))
                  (let* ((level-subtotals (map (lambda (col-idx) ((list-ref (list-ref collectors col-idx) lvl)
                                                                  'format gnc:make-gnc-monetary #f))
                                               (iota num-columns))))

                    ;; the following conditions tests whether we should display the subtotal
                    (if (or (zero? lvl)
                            (and hierarchical-subtotals?
                                 (or show-zb-accts? (not (every zero? (map gnc:gnc-monetary-amount (concatenate level-subtotals)))))
                                 (or (not depth-limit) (<= lvl depth-limit))
                                 (list-ref labels lvl)))
                        (add-indented-row lvl
                                          (string-append
                                           (_ "Total for ")
                                           (list-ref labels lvl))
                                          "total-label-cell"
                                          (map
                                           (lambda (level-subtotal)
                                             (gnc:make-html-table-cell/markup
                                              "total-number-cell"
                                              (list-of-monetary->html-text level-subtotal #f #f)))
                                           level-subtotals)))
                    (list-set! labels lvl #f)
                    (for-each
                     (lambda (col-idx)
                       ((list-ref (list-ref collectors col-idx) lvl) 'reset #f #f)
                       ((list-ref (list-ref collectors col-idx) (1+ lvl)) 'reset #f #f))
                     (iota num-columns))
                    (add-subtotal-row (1- lvl)))))))

          (loop rest))))
  (add-whole-line #f)
  ;; return collector level 0 total
  (map (lambda (col-idx) (car (list-ref collectors col-idx))
               'format gnc:make-gnc-monetary #f)
       (iota num-columns)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multicol-report-renderer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multicol-report-renderer report-obj report-type reportname)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all options values
  (let* ((report-title (get-option gnc:pagename-general optname-report-title))
         (company-name (get-option gnc:pagename-general optname-company-name))
         (startdate (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general
                                 optname-startdate)))
         (enddate (gnc:date-option-absolute-time
                   (get-option gnc:pagename-general
                               optname-enddate)))
         (export? (get-option gnc:pagename-general
                              optname-export))
         (incr (keylist-get-info periodlist
                                 (get-option gnc:pagename-general optname-period)
                                 'delta))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (depth-limit (let ((limit (get-option gnc:pagename-accounts
                                               optname-depth-limit)))
                        (and (not (eq? limit 'all)) limit)))
         (subtotal-mode (get-option gnc:pagename-display
                                    optname-subtotal-mode))
         (show-zb-accts? (get-option gnc:pagename-display
                                     optname-show-zb-accts))
         (omit-zb-bals? (get-option gnc:pagename-display
                                    optname-omit-zb-bals))
         (use-links? (get-option gnc:pagename-display
                                 optname-account-links))

         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (asset-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-ASSET))
         (liability-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY))
         (income-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-INCOME))
         (expense-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE))
         (equity-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))
         (trading-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-TRADING))
         (doc (gnc:make-html-document)))

    (gnc:html-document-set-title!
     doc (string-append company-name " " report-title " "
                        (qof-print-date startdate) " - "
                        (qof-print-date enddate)))

    (if (null? accounts)

        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          reportname (gnc:report-id report-obj)))

        (case report-type
          ((balsheet)
           (let* ((multicol-table (gnc:make-html-table))
                  (maxindent (gnc-account-get-tree-depth (gnc-get-current-root-account)))
                  (reportdates (gnc:make-date-list startdate enddate incr))
                  (get-cell-amount-fn (lambda (account col-idx)
                                        (gnc:make-gnc-monetary
                                         (xaccAccountGetCommodity account)
                                         (xaccAccountGetBalanceAsOfDate account
                                                                        (gnc:time64-end-day-time
                                                                         (list-ref reportdates col-idx))))))
                  (get-cell-anchor-fn (lambda (account col-idx)
                                        (let* ((splits (xaccAccountGetSplitList account))
                                               (split-date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
                                               (date (gnc:time64-end-day-time (list-ref reportdates col-idx)))
                                               (valid-split? (lambda (s) (< (split-date s) date)))
                                               (valid-splits (filter valid-split? splits))
                                               (sorted-splits (stable-sort! valid-splits
                                                                            (lambda (a b)
                                                                              (< (split-date a) (split-date b)))))
                                               (split (and (pair? sorted-splits) (last sorted-splits))))
                                          (and split
                                               (gnc:split-anchor-text split)
                                               #;(gnc:account-anchor-text account)))))
                  (reportheaders (map qof-print-date reportdates))
                  (add-to-table (lambda (title accounts summary?)
                                  (add-multicolumn-acct-table
                                   multicol-table title accounts
                                   maxindent get-cell-amount-fn reportheaders
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:disable-indenting? export?
                                   #:hierarchical-subtotals? (and (not summary?) subtotal-mode)
                                   #:depth-limit (if summary? 0 depth-limit)
                                   #:get-cell-anchor-fn get-cell-anchor-fn
                                   ))))

             (add-to-table (_ "Asset") asset-accounts #f)
             (add-to-table (_ "Liability") liability-accounts #f)
             (add-to-table (_ "Equity") equity-accounts #f)
             (add-to-table (_ "Trading Accounts") trading-accounts #f)
             (add-to-table (_ "Net Worth") (append asset-accounts liability-accounts trading-accounts) #t)

             (gnc:html-document-add-object!
              doc (gnc:html-render-options-changed (gnc:report-options report-obj)))

             (gnc:html-document-add-object!
              doc multicol-table)))

          ((pnl)
           (let* ((multicol-table (gnc:make-html-table))
                  (maxindent (gnc-account-get-tree-depth (gnc-get-current-root-account)))
                  (closing-str (get-option pagename-entries optname-closing-pattern))
                  (closing-cased (get-option pagename-entries optname-closing-casing))
                  (closing-regexp (get-option pagename-entries optname-closing-regexp))
                  ;; datepairs - start from startdate to startdate + incr - 1day
                  ;; repeat until enddate is reached. e.g. 1/1/18 - 31/1/18, 1/2/18 - 28/2/18, etc
                  (report-datepairs (let loop ((result '())
                                               (date startdate))
                                      (if (> date enddate) (reverse result)
                                          (let ((nextdate (incdate date incr)))
                                            (loop (cons (cons date (min enddate (decdate nextdate DayDelta)))
                                                        result)
                                                  nextdate)))))
                  ;; this object will cache *all* closing entries from inc/exp accounts to equity.
                  ;; retrieve both KVP-based transaction flags, and the closing-entries string above.
                  (closing-entries (let ((query (qof-query-create-for-splits)))
                                     (qof-query-set-book query (gnc-get-current-book))
                                     (xaccQueryAddAccountMatch query (append income-accounts expense-accounts)
                                                               QOF-GUID-MATCH-ANY QOF-QUERY-AND)
                                     (if (and closing-str (not (string-null? closing-str)))
                                         (xaccQueryAddDescriptionMatch query closing-str closing-cased closing-regexp
                                                                       QOF-COMPARE-CONTAINS QOF-QUERY-AND))
                                     (xaccQueryAddClosingTransMatch query #t QOF-QUERY-OR)
                                     (let ((splits (qof-query-run query)))
                                       (qof-query-destroy query)
                                       splits)))
                  ;; this function will query the above closing-entries for splits within the date range,
                  ;; and produce the total amount for these closing entries
                  (closing-adjustment (lambda (account fromdate todate)
                                        (define (include-split? split)
                                          (and (equal? (xaccSplitGetAccount split) account)
                                               (<= fromdate
                                                   (xaccTransGetDate (xaccSplitGetParent split))
                                                   todate)))
                                        (let ((account-closing-splits (filter include-split? closing-entries)))
                                          (apply + (map xaccSplitGetAmount account-closing-splits)))))
                  (get-cell-amount-fn (lambda (account col-idx)
                                        (let* ((datepair (list-ref report-datepairs col-idx))
                                               (startdate (gnc:time64-start-day-time (car datepair)))
                                               (enddate (gnc:time64-end-day-time (cdr datepair))))
                                          (gnc:make-gnc-monetary
                                           (xaccAccountGetCommodity account)
                                           (- (xaccAccountGetBalanceAsOfDate account enddate)
                                              (xaccAccountGetBalanceAsOfDate account startdate)
                                              (closing-adjustment account startdate enddate))))))
                  (get-cell-anchor-fn (lambda (account col-idx)
                                        (let ((datepair (list-ref report-datepairs col-idx)))
                                          (gnc:make-report-anchor
                                           trep-uuid report-obj
                                           (list (list "General" "Start Date" (cons 'absolute (car datepair)))
                                                 (list "General" "End Date" (cons 'absolute (cdr datepair)))
                                                 (list "Accounts" "Accounts" (list account)))))))
                  (reportheaders (map (lambda (pair)
                                        (gnc:make-html-text
                                         (qof-print-date (car pair))
                                         (gnc:html-markup-br)
                                         (_ " to ")
                                         (qof-print-date (cdr pair))))
                                      report-datepairs))
                  (add-to-table (lambda (title accounts summary?)
                                  (add-multicolumn-acct-table
                                   multicol-table title accounts
                                   maxindent get-cell-amount-fn reportheaders
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:disable-indenting? export?
                                   #:hierarchical-subtotals? (and (not summary?) subtotal-mode)
                                   #:depth-limit (if summary? 0 depth-limit)
                                   #:get-cell-anchor-fn get-cell-anchor-fn))))

             (add-to-table (_ "Income") income-accounts #f)
             (add-to-table (_ "Expense") expense-accounts #f)
             (add-to-table (_ "Net Income") (append income-accounts expense-accounts) #t)

             (gnc:html-document-add-object!
              doc (gnc:html-render-options-changed (gnc:report-options report-obj)))

             (gnc:html-document-add-object!
              doc multicol-table)))))
    (gnc:report-finished)
    ;; (gnc:html-document-set-style-text!
    ;;  doc " table, td{ border-width: 1px; border-style:solid; border-color: lightgray; border-collapse: collapse}")
    doc))

(define balsheet-reportname (_ "Balance Sheet (Multicolumn)"))
(define pnl-reportname (_ "Income Statement (Multicolumn)"))

(gnc:define-report
 'version 1
 'name balsheet-reportname
 'report-guid "065d5d5a77ba11e8b31e83ada73c5eea"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator (lambda () (multicol-report-options-generator 'balsheet balsheet-reportname))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'balsheet balsheet-reportname)))

(gnc:define-report
 'version 1
 'name pnl-reportname
 'report-guid "0e94fd0277ba11e8825d43e27232c9d4"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator (lambda () (multicol-report-options-generator 'pnl pnl-reportname))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'pnl pnl-reportname)))

;; END
