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

(define optname-company-name (N_ "Company name"))
(define opthelp-company-name (N_ "Name of company/individual."))

(define optname-startdate (N_ "Start Date"))
(define optname-enddate (N_ "End Date"))

(define optname-period (N_ "Period duration"))
(define opthelp-period (N_ "Duration between time periods"))

(define optname-dual-columns (N_ "Enable dual columns"))
(define opthelp-dual-columns (N_ "Selecting this option will enable double-column \
reporting."))

(define optname-disable-amount-indent (N_ "Disable amount indenting"))
(define opthelp-disable-amount-indent (N_ "Selecting this option will disable amount indenting, and condense amounts into a single column."))

(define optname-account-full-name (N_ "Account full name instead of indenting"))
(define opthelp-account-full-name (N_ "Selecting this option enables full account name instead, and disables indenting account names."))

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts (N_ "Report on these accounts, if display depth allows."))

(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit (N_ "Maximum number of levels in the account tree displayed."))

(define optname-parent-balance-mode (N_ "Parent account amounts include children"))
(define opthelp-parent-balance-mode (N_ "If this option is enabled, subtotals are \
displayed within parent amounts, and if parent has own amount, it is displayed on \
the next row as a child account. If this option is disabled, subtotals are displayed \
below parent and children groups."))

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

(define pagename-commodities (N_ "Commodities"))
(define optname-include-chart (N_ "Enable chart"))
(define opthelp-include-chart (N_ "Enable link to barchart report"))

(define optname-common-currency (N_ "Convert to common currency"))
(define opthelp-common-currency (N_ "Convert all amounts to a single currency."))

(define optname-report-commodity (N_ "Report's currency"))

(define optname-price-source (N_ "Price Source"))
(define opthelp-price-source (N_ "How to determine exchange rates."))

(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign (N_ "Display any foreign currency amount in an account."))

(define optname-include-overall-period (N_ "If more than 1 period column, include overall period?"))
(define opthelp-include-overall-period (N_ "If several profit & loss period columns are shown, \
also show overall period profit & loss."))

(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))

(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")
(define networth-barchart-uuid "cbba1696c8c24744848062c7f1cf4a72")
(define pnl-barchart-uuid "80769921e87943adade887b9835a7685")

(define periodlist
  (list
   (cons #f (list
             (cons 'delta #f)
             (cons 'text (_ "disabled"))
             (cons 'tip (_ "disable multicolumn"))))

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

(define pricesource-list-balsheet
  (list
   (cons 'pricedb-nearest (list
                           (cons 'text (_ "nearest"))
                           (cons 'tip (_ "Nearest to date. Balance sheet prices \
are converted using the price on the balance sheet date."))))

   (cons 'pricedb-latest (list
                          (cons 'text (_ "latest"))
                          (cons 'tip (_ "Latest price. This uses the latest prices \
available, i.e. closest to today's prices."))))))

(define pricesource-list-pnl
  (list
   (cons 'startperiod (list
                       (cons 'text (_ "start-period"))
                       (cons 'tip (_ "Prices closest to the start of the reporting period \
are used."))))

   (cons 'midperiod (list
                     (cons 'text (_ "mid-period"))
                     (cons 'tip (_ "Prices in the middle of the reporting period \
are used."))))

   (cons 'endperiod (list
                     (cons 'text (_ "end-period"))
                     (cons 'tip (_ "Prices in the end of the reporting period \
are used."))))

   (cons 'pricedb-latest (list
                  (cons 'text (_ "latest"))
                  (cons 'tip (_ "Latest price. This uses the latest prices \
available, i.e. closest to today's prices."))))))

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
(define (multicol-report-options-generator report-type)
  (let* ((options (gnc:new-options))
         (book (gnc-get-current-book))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-company-name
      "b" opthelp-company-name (or (gnc:company-info book gnc:*company-name*) "")))

    ;; date at which to report balance
    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-startdate optname-enddate "c")

    (add-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-period
      "c2" opthelp-period
      #f
      (keylist->vectorlist periodlist)
      #f
      (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options
         gnc:pagename-general optname-disable-amount-indent
         (not x))
        (gnc-option-db-set-option-selectable-by-name
         options
         gnc:pagename-general optname-dual-columns
         (not x))
        (gnc-option-db-set-option-selectable-by-name
         options
         gnc:pagename-general
         (case report-type
           ((balsheet) optname-startdate)
           ((pnl) optname-include-overall-period))
         x))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-disable-amount-indent
      "c3" opthelp-disable-amount-indent #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-include-chart
      "d" opthelp-include-chart #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-dual-columns
      "c4" opthelp-dual-columns #t))

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

    ;; all about currencies
    (add-option
     (gnc:make-complex-boolean-option
      pagename-commodities optname-common-currency
      "b" opthelp-common-currency #f #f
      (lambda (x)
        (for-each
         (lambda (optname)
           (gnc-option-db-set-option-selectable-by-name
            options pagename-commodities optname x))
         (list optname-report-commodity
               optname-show-rates
               optname-show-foreign
               optname-price-source)))))

    (gnc:options-add-currency!
     options pagename-commodities
     optname-report-commodity "c")

    (add-option
     (gnc:make-multichoice-option
      pagename-commodities optname-price-source
      "d" opthelp-price-source
      (case report-type
        ((pnl) 'midperiod)
        ((balsheet) 'pricedb-nearest))
      (keylist->vectorlist
       (case report-type
         ((pnl) pricesource-list-pnl)
         ((balsheet) pricesource-list-balsheet)))))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-foreign
      "e" opthelp-show-foreign #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-rates
      "f" opthelp-show-rates #t))

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
      gnc:pagename-display optname-parent-balance-mode
      "c" opthelp-parent-balance-mode #t))

    ;; some detailed formatting options
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-links
      "e" opthelp-account-links #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-full-name
      "f" opthelp-account-full-name #f))

    (when (eq? report-type 'pnl)
      ;; include overall period column?
      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-general optname-include-overall-period
        "e" opthelp-include-overall-period #f))

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
          table title accountlist maxindent get-cell-monetary-fn cols-data #:key
          (omit-zb-bals? #f)
          (show-zb-accts? #t)
          (account-full-name? #f)
          (disable-amount-indent? #f)
          (hide-accounts? #f)
          (hide-grand-total? #f)
          (depth-limit #f)
          (negate-amounts? #f)
          (recursive-bals? #f)
          (account-anchor-fn #f)
          (get-col-header-fn #f)
          (get-cell-orig-monetary-fn #f)
          (get-cell-anchor-fn #f))

  ;; this function will add a 2D grid into the html-table
  ;; the data cells are generated from (get-cell-monetary-fn account col-datum)
  ;; the data cells may request an alternative (eg. original currency) monetary
  ;; horizontal labels are generated from calling (get-col-header-fn col-datum)
  ;; vertical labels are the account list. it can have multilevel subtotals.

  ;; the following are compulsory arguments:
  ;; table - an existing html-table object
  ;; title - string as the first row
  ;; accountlist - list of accounts
  ;; maxindent - maximum account depth
  ;; cols-data - list of data to be passed as parameter to the following helper functions
  ;; get-cell-monetary-fn - a lambda (account cols-data) which produces a gnc-monetary or #f (eg price conversion impossible)

  ;; the following are optional:
  ;; omit-zb-bals?      - a boolean to omit "$0.00" amounts
  ;; show-zb-accts?     - a boolean to omit whole account lines where all amounts are $0.00 (eg closed accts)
  ;; account-full-name? - a boolean to disable narrow-cell indenting, and render account full-name instead
  ;; disable-amount-indent? - a bool to disable amount indenting (only for single data column reports)
  ;; negate-amounts?    - a boolean to negate amounts. useful for e.g. income-type accounts.
  ;; depth-limit        - (untested) accounts whose levels exceed this depth limit are not shown
  ;; recursive-bals?    - a boolean to confirm recursive-balances enabled (parent-accounts show balances) or
  ;;                      disabled (multilevel subtotals after each parent+children)
  ;; get-col-header-fn  - a lambda (accounts cols-data) to produce html-object - this is optional
  ;; get-cell-orig-monetary-fn   - a lambda (account cols-data) which produces a gnc-monetary or #f - optional
  ;; get-cell-anchor-fn - a lambda (account cols-data) which produces a url string - optional

  (define num-columns (length cols-data))

  (define amount-indenting? (and (not disable-amount-indent?) (= num-columns 1)))

  (define (make-list-thunk n thunk)
    (let loop ((result '()) (n n))
      (if (zero? n) result
          (loop (cons (thunk) result) (1- n)))))

  (define (make-narrow-cell)
    (let ((narrow (gnc:make-html-table-cell/markup "text-cell" #f)))
      (gnc:html-table-cell-set-style! narrow "text-cell" 'attribute '("style" "width:1px"))
      narrow))

  (define (add-indented-row indent label label-markup amount-indent rest)
    (gnc:html-table-append-row!
     table
     (append (if account-full-name? '() (make-list-thunk indent make-narrow-cell))
             (list (if label-markup
                       (gnc:make-html-table-cell/size/markup 1 (if account-full-name? 1 (- maxindent indent)) label-markup label)
                       (gnc:make-html-table-cell/size 1 (if account-full-name? 1 (- maxindent indent)) label)))
             (gnc:html-make-empty-cells (if amount-indenting? amount-indent 0))
             rest
             (gnc:html-make-empty-cells (if amount-indenting? (- maxindent amount-indent) 0)))))

  (define (monetary+ . monetaries)
    ;; usage: (monetary+ monetary...)
    ;; inputs: list of gnc-monetary (e.g. USD 10, USD 25, GBP 5, GBP 8)
    ;; outputs: list of gnc-monetary (e.g. USD 35, GBP 13)
    (let ((coll (gnc:make-commodity-collector)))
      (for-each
       (lambda (monetary)
         (coll 'add
               (gnc:gnc-monetary-commodity monetary)
               (let ((amount (gnc:gnc-monetary-amount monetary)))
                 (if negate-amounts? (- amount) amount))))
       monetaries)
      (coll 'format gnc:make-gnc-monetary #f)))

  (define (list-of-monetary->html-text monetaries monetaries-orig anchor)
    ;; inputs:
    ;; monetaries: list of gnc-monetary (or #f, or html-text object)
    ;; monetaries-orig: list of gnc-monetary (or html-text object) (or #f)
    ;; anchor: url string for monetaries (or #f) (all have same anchor)
    ;;
    ;; outputs: html-text object
    (let ((text (gnc:make-html-text)))
      (for-each
       (lambda (monetary)
         (if (not (and omit-zb-bals? (gnc:gnc-monetary? monetary) (zero? (gnc:gnc-monetary-amount monetary))))
             (gnc:html-text-append! text monetary (gnc:html-markup-br))))
       (or monetaries-orig '()))
      (for-each
       (lambda (monetary)
         (if (not (and omit-zb-bals? (gnc:gnc-monetary? monetary) (zero? (gnc:gnc-monetary-amount monetary))))
             (gnc:html-text-append! text
                                    (if anchor
                                        (gnc:html-markup-anchor anchor monetary)
                                        monetary)
                                    (gnc:html-markup-br))))
       (or monetaries '()))
      text))

  (define (render-account account)
    ;; input: account-name
    ;; outputs: string or html-markup-anchor object
    (let ((acct-label (if account-full-name?
                          (gnc-account-get-full-name account)
                          (xaccAccountGetName account)))
          (acct-url (and account-anchor-fn (account-anchor-fn account))))
      (gnc:make-html-text
       (if acct-url
           (gnc:html-markup-anchor acct-url acct-label)
           acct-label))))

  (define (add-whole-line contents)
    (gnc:html-table-append-row!
     table (gnc:make-html-table-cell/size 1 (+ 1 (if account-full-name? 0 maxindent) num-columns) contents)))

  (define collectors
    (make-list-thunk num-columns gnc:make-commodity-collector))

  ;; header ASSET/LIABILITY etc
  (add-indented-row 0
                    title
                    "total-label-cell"
                    maxindent
                    (if get-col-header-fn
                        (map
                         (lambda (col-datum)
                           (get-col-header-fn accountlist col-datum))
                         cols-data)
                        (gnc:html-make-empty-cells maxindent)))

  (let loop ((accounts accountlist))
    (if (pair? accounts)
        (let* ((curr (car accounts))
               (rest (cdr accounts))
               (next (and (pair? rest) (car rest)))
               (lvl-curr (gnc-account-get-current-depth curr))
               (lvl-next (if next (gnc-account-get-current-depth next) 0))
               (curr-commodity (xaccAccountGetCommodity curr))
               (curr-descendants-list (filter (lambda (acc) (member acc accountlist))
                                              (gnc-account-get-descendants curr)))
               (recursive-parent-acct? (and recursive-bals? (pair? curr-descendants-list)))
               (multilevel-parent-acct? (and (not recursive-bals?) (pair? curr-descendants-list)))
               (curr-balance-display (lambda (get-cell-fn idx include-descendants?)
                                       (map (lambda (acc) (get-cell-fn acc idx))
                                            (cons curr (if include-descendants?
                                                           curr-descendants-list
                                                           '()))))))

          (let collector-loop ((col-idx 0)
                               (cols-data cols-data))
            (when (pair? cols-data)
              (let ((mon (get-cell-monetary-fn curr (car cols-data))))
                ((list-ref collectors col-idx)
                 'add
                 (gnc:gnc-monetary-commodity mon)
                 (let ((amount (gnc:gnc-monetary-amount mon)))
                   (if negate-amounts? (- amount) amount)))
                (collector-loop (1+ col-idx) (cdr cols-data)))))

          (if (and (or show-zb-accts?
                       ;; the following function tests whether accounts (with descendants) of
                       ;; all columns are zero
                       (not (every zero? (concatenate
                                          (map (lambda (acc)
                                                 (map (lambda (col-datum)
                                                        (gnc:gnc-monetary-amount
                                                         (get-cell-monetary-fn acc col-datum)))
                                                      cols-data))
                                               (cons curr curr-descendants-list))))))
                   (not hide-accounts?)
                   (or (not depth-limit) (<= lvl-curr depth-limit)))

              (begin

                (add-indented-row lvl-curr
                                  (render-account curr)
                                  "text-cell"
                                  (- maxindent
                                     lvl-curr
                                     (if multilevel-parent-acct? 1 0))
                                  (map
                                   (lambda (col-datum)
                                     (gnc:make-html-table-cell/markup
                                      (if (or (not recursive-bals?) (null? curr-descendants-list)) "number-cell" "total-number-cell")
                                      (list-of-monetary->html-text
                                       (apply monetary+ (curr-balance-display get-cell-monetary-fn col-datum recursive-bals?))
                                       (and get-cell-orig-monetary-fn
                                            (apply monetary+ (filter identity (curr-balance-display get-cell-orig-monetary-fn col-datum #f))))
                                       (and get-cell-anchor-fn
                                            (not recursive-parent-acct?)
                                            (get-cell-anchor-fn curr col-datum)))))
                                   cols-data))

                ;; the following handles 'special' case where placeholder has descendants. only for recursive-bals? = true
                (if (and recursive-parent-acct?
                         (not hide-accounts?)
                         (or (not depth-limit) (<= (1+ lvl-curr) depth-limit))
                         (not (every zero? (map (lambda (col-datum) (gnc:gnc-monetary-amount (get-cell-monetary-fn curr col-datum)))
                                                cols-data))))
                    (add-indented-row (1+ lvl-curr)
                                      (render-account curr)
                                      "text-cell"
                                      (- maxindent lvl-curr 1)
                                      (map
                                       (lambda (col-datum)
                                         (gnc:make-html-table-cell/markup
                                          "number-cell"
                                          (list-of-monetary->html-text
                                           (apply monetary+
                                                  (curr-balance-display get-cell-monetary-fn col-datum #f))
                                           (and get-cell-orig-monetary-fn
                                                (apply monetary+
                                                       (filter identity (curr-balance-display get-cell-orig-monetary-fn col-datum #f))))
                                           (and get-cell-anchor-fn
                                                (get-cell-anchor-fn curr col-datum)))))
                                       cols-data)))))

          (if (and (not recursive-bals?)
                   (not hide-accounts?)
                   (or (not depth-limit) (<= lvl-curr depth-limit))
                   (> lvl-curr lvl-next))
              (let multilevel-loop ((lvl (1- lvl-curr))
                                    (lvl-acct (gnc-account-get-parent curr)))
                (unless (or (zero? lvl)
                            (not (member lvl-acct accountlist))
                            (< lvl lvl-next))
                  (add-indented-row lvl
                                    (string-append (_ "Total For ")
                                                   ((if account-full-name?
                                                        gnc-account-get-full-name
                                                        xaccAccountGetName) lvl-acct))
                                    "total-label-cell"
                                    (- maxindent lvl)
                                    (map
                                     (lambda (col-datum)
                                       (gnc:make-html-table-cell/markup
                                        "total-number-cell"
                                        (list-of-monetary->html-text
                                         (apply monetary+
                                                (map (lambda (acc) (get-cell-monetary-fn acc col-datum))
                                                     (cons lvl-acct (filter (lambda (acc) (member acc accountlist))
                                                                            (gnc-account-get-descendants lvl-acct)))))
                                         #f
                                         #f)))
                                     cols-data))
                  (multilevel-loop (1- lvl)
                                   (gnc-account-get-parent lvl-acct)))))
          (loop rest))))

  (add-whole-line #f)
  (let ((grand-totals (map (lambda (col-idx) ((list-ref collectors col-idx)
                                              'format gnc:make-gnc-monetary #f))
                           (iota num-columns))))

    (if (not hide-grand-total?)
        (add-indented-row 0
                          (string-append (_"Total For ") title)
                          "total-label-cell"
                          maxindent
                          (map
                           (lambda (col-total)
                             (let ((total-cell (gnc:make-html-table-cell/markup
                                                "total-number-cell"
                                                (list-of-monetary->html-text col-total #f #f))))
                               (gnc:html-table-cell-set-style!
                                total-cell "total-number-cell"
                                'attribute '("style" "border-top-style:solid; border-top-width: 1px; border-bottom-style:double"))
                               total-cell))
                           grand-totals)))

    ;; return grandtotal
    grand-totals))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multicol-report-renderer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multicol-report-renderer report-obj report-type)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting (get-option gnc:pagename-general gnc:optname-reportname))

  ;; get all options values
  (let* ((report-title (get-option gnc:pagename-general gnc:optname-reportname))
         (company-name (get-option gnc:pagename-general optname-company-name))
         (startdate (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general
                                 optname-startdate)))
         (enddate (gnc:date-option-absolute-time
                   (get-option gnc:pagename-general
                               optname-enddate)))
         (account-full-name? (get-option gnc:pagename-display
                                         optname-account-full-name))
         (incr (let ((period (get-option gnc:pagename-general optname-period)))
                 (and period
                      (keylist-get-info periodlist period 'delta))))
         (disable-amount-indent? (and (not incr)
                                      (get-option gnc:pagename-general
                                                  optname-disable-amount-indent)))
         (enable-dual-columns? (and (not incr)
                                    (get-option gnc:pagename-general
                                                optname-dual-columns)))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (depth-limit (let ((limit (get-option gnc:pagename-accounts
                                               optname-depth-limit)))
                        (and (not (eq? limit 'all)) limit)))
         (show-zb-accts? (get-option gnc:pagename-display
                                     optname-show-zb-accts))
         (omit-zb-bals? (get-option gnc:pagename-display
                                    optname-omit-zb-bals))
         (recursive-bals? (get-option gnc:pagename-display
                                      optname-parent-balance-mode))
         (use-links? (get-option gnc:pagename-display
                                 optname-account-links))
         (include-chart? (get-option gnc:pagename-general optname-include-chart))
         (common-currency (and (get-option pagename-commodities optname-common-currency)
                               (get-option pagename-commodities optname-report-commodity)))
         (has-price? (lambda (commodity)
                       ;; the following tests whether an amount in
                       ;; commodity can be converted to
                       ;; common-currency. if conversion successful,
                       ;; it will be a non-zero value.  note if we use
                       ;; API gnc-pricedb-has-prices, we're only
                       ;; querying the pricedb.  if we use
                       ;; gnc-pricedb-convert-balance-latest-price, we
                       ;; can potentially use an intermediate
                       ;; currency.
                       (not (zero? (gnc-pricedb-convert-balance-latest-price
                                    (gnc-pricedb-get-db (gnc-get-current-book))
                                    (gnc-commodity-get-fraction commodity)
                                    commodity
                                    common-currency)))))
         (price-source (get-option pagename-commodities optname-price-source))
         (get-exchange-rates-fn (lambda (accounts date)
                                  (let ((commodities (delete common-currency
                                                             (delete-duplicates (map xaccAccountGetCommodity accounts)
                                                                                gnc-commodity-equal)
                                                             gnc-commodity-equal))
                                        (cell (gnc:make-html-text)))
                                    (for-each
                                     (lambda (commodity)
                                       (if (has-price? commodity)
                                           (let* ((domestic (gnc:make-gnc-monetary commodity 1))
                                                  (foreign (gnc:exchange-by-pricedb-nearest
                                                            domestic common-currency
                                                            (case price-source
                                                              ((startperiod) startdate)
                                                              ((midperiod) (floor (/ (+ startdate enddate) 2)))
                                                              ((endperiod) enddate)
                                                              ((pricedb-nearest) (gnc:time64-end-day-time date))
                                                              ((pricedb-latest) (current-time))))))
                                             (gnc:html-text-append!
                                              cell
                                              (format #f "~a ~a"
                                                      (gnc:monetary->string domestic)
                                                      (gnc:monetary->string foreign))))
                                           (gnc:html-text-append!
                                            cell
                                            (format #f (string-append "~a/~a " (_ "missing"))
                                                    (gnc-commodity-get-mnemonic common-currency)
                                                    (gnc-commodity-get-mnemonic commodity))))
                                       (gnc:html-text-append! cell (gnc:html-markup-br)))
                                     commodities)
                                    (gnc:make-html-table-cell/markup "number-cell" cell))))

         (account->url (lambda (acct) (and use-links?
                                           (not (xaccAccountGetPlaceholder acct))
                                           (gnc:account-anchor-text acct))))
         ;; decompose the account list
         (show-foreign? (get-option pagename-commodities optname-show-foreign))
         (show-rates? (get-option pagename-commodities optname-show-rates))
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
          report-title (gnc:report-id report-obj)))

        (case report-type
          ((balsheet)
           (let* ((multicol-table-left (gnc:make-html-table))
                  (multicol-table-right (if enable-dual-columns?
                                            (gnc:make-html-table)
                                            multicol-table-left))
                  (maxindent (gnc-account-get-tree-depth (gnc-get-current-root-account)))
                  (report-dates (if incr
                                    (gnc:make-date-list startdate enddate incr)
                                    (list enddate)))
                  (amount-col-amount (lambda (account col-datum)
                                       (gnc:make-gnc-monetary
                                        (xaccAccountGetCommodity account)
                                        (xaccAccountGetBalanceAsOfDate
                                         account
                                         (gnc:time64-end-day-time col-datum)))))
                  (get-cell-orig-monetary-fn (lambda (account col-datum)
                                                      (and common-currency
                                                           show-foreign?
                                                           (not (gnc-commodity-equal (xaccAccountGetCommodity account) common-currency))
                                                           (amount-col-amount account col-datum))))
                  (get-cell-monetary-fn (lambda (account col-datum)
                                          (let* ((col-date (gnc:time64-end-day-time col-datum))
                                                 (monetary (amount-col-amount account col-datum)))
                                            (if (and common-currency
                                                     (has-price? (xaccAccountGetCommodity account)))
                                                (gnc:exchange-by-pricedb-nearest
                                                 monetary common-currency
                                                 (case price-source
                                                   ((pricedb-nearest) col-date)
                                                   ((pricedb-latest) (current-time))))
                                                monetary))))
                  (get-cell-anchor-fn (lambda (account col-datum)
                                        (let* ((splits (xaccAccountGetSplitList account))
                                               (split-date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
                                               (date (gnc:time64-end-day-time col-datum))
                                               (valid-split? (lambda (s) (< (split-date s) date)))
                                               (valid-splits (filter valid-split? splits))
                                               (split (and (pair? valid-splits)
                                                           (last valid-splits))))
                                          (and split
                                               (gnc:split-anchor-text split)))))
                  (chart (and include-chart?
                              (gnc:make-report-anchor
                               networth-barchart-uuid report-obj
                               (list (list "General" "Start Date" (cons 'absolute startdate))
                                     (list "General" "End Date" (cons 'absolute enddate))
                                     (list "General" "Report's currency" (or common-currency (gnc-default-report-currency)))
                                     (list "General" "Price Source" price-source)
                                     (list "Accounts" "Accounts" (append asset-accounts liability-accounts))))))
                  (get-col-header-fn (lambda (accounts col-datum)                                      
                                      (let* ((header (qof-print-date col-datum))
                                             (cell (gnc:make-html-table-cell/markup "total-label-cell" header)))
                                        (gnc:html-table-cell-set-style! cell "total-label-cell" 'attribute '("style" "text-align:right"))
                                        cell)))
                  (add-to-table (lambda (table title accounts get-col-header-fn hide-accounts? hide-grand-total? negate-amounts?)
                                  (add-multicolumn-acct-table
                                   table title accounts
                                   maxindent get-cell-monetary-fn report-dates
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:account-full-name? account-full-name?
                                   #:negate-amounts? negate-amounts?
                                   #:disable-amount-indent? disable-amount-indent?
                                   #:hide-accounts? hide-accounts?
                                   #:hide-grand-total? hide-grand-total?
                                   #:depth-limit (if get-col-header-fn 0 depth-limit)
                                   #:recursive-bals? recursive-bals?
                                   #:account-anchor-fn account->url
                                   #:get-cell-orig-monetary-fn (and common-currency get-cell-orig-monetary-fn)
                                   #:get-col-header-fn get-col-header-fn
                                   #:get-cell-anchor-fn get-cell-anchor-fn
                                   ))))

             (add-to-table multicol-table-left (_ "Date") '() get-col-header-fn #t #t #f)
             (if enable-dual-columns?
                 (add-to-table multicol-table-right (_ "Date") '() get-col-header-fn #t #t #f))
             (add-to-table multicol-table-left (_ "Asset") asset-accounts #f #f #f #f)
             (add-to-table multicol-table-right (_ "Liability") liability-accounts #f #f #f #t)
             ;; (add-to-table (_ "Equity") equity-accounts #f #f #f #t)
             (unless (null? trading-accounts)
               (add-to-table multicol-table-right (_ "Trading Accounts") trading-accounts #f #f #f #f))
             (add-to-table multicol-table-right (_ "Net Worth") (append asset-accounts liability-accounts trading-accounts) #f #t #f #f)
             (if (and common-currency show-rates?)
                 (add-to-table multicol-table-right (_ "Exchange Rates") (append asset-accounts liability-accounts trading-accounts) get-exchange-rates-fn #t #t #f))

             (gnc:html-document-add-object!
              doc (gnc:html-render-options-changed (gnc:report-options report-obj)))

             (if include-chart?
                 (gnc:html-document-add-object!
                  doc
                  (gnc:make-html-text
                   (gnc:html-markup-anchor chart "Barchart"))))

             (let ((multicol-table (if enable-dual-columns?
                                       (gnc:make-html-table)
                                       multicol-table-left)))
               (when enable-dual-columns?
                 (gnc:html-table-append-row! multicol-table
                                             (list multicol-table-left multicol-table-right)))
               (gnc:html-document-add-object!
                doc multicol-table))))

          ((pnl)
           (let* ((multicol-table-left (gnc:make-html-table))
                  (multicol-table-right (if enable-dual-columns?
                                            (gnc:make-html-table)
                                            multicol-table-left))
                  (maxindent (gnc-account-get-tree-depth (gnc-get-current-root-account)))
                  (closing-str (get-option pagename-entries optname-closing-pattern))
                  (closing-cased (get-option pagename-entries optname-closing-casing))
                  (closing-regexp (get-option pagename-entries optname-closing-regexp))
                  (include-overall-period? (get-option gnc:pagename-general optname-include-overall-period))
                  ;; datepairs - start from startdate to startdate + incr - 1day
                  ;; repeat until enddate is reached. e.g. 1/1/18 - 31/1/18, 1/2/18 - 28/2/18, etc
                  ;; if incr is false, datepair will have 1 list element - (cons startdate enddate)
                  (report-datepairs (if incr
                                        (let loop ((result '())
                                                   (date startdate))
                                          (if (> date enddate)
                                              (if (and include-overall-period? (> (length result) 1))
                                                  (reverse (cons (cons startdate enddate)
                                                                 result))
                                                  (reverse result))
                                              (let ((nextdate (incdate date incr)))
                                                (loop (cons (cons date (min enddate (decdate nextdate DayDelta)))
                                                            result)
                                                      nextdate))))
                                        (list (cons startdate enddate))))
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
                  (account-col-amount (lambda (account col-datum)
                                        (let* ((startdate (gnc:time64-start-day-time (car col-datum)))
                                               (enddate (gnc:time64-end-day-time (cdr col-datum))))
                                          (gnc:make-gnc-monetary
                                           (xaccAccountGetCommodity account)
                                           (- (xaccAccountGetBalanceAsOfDate account enddate)
                                              (xaccAccountGetBalanceAsOfDate account startdate)
                                              (closing-adjustment account startdate enddate))))))
                  (get-cell-orig-monetary-fn (lambda (account col-datum)
                                                      (and common-currency
                                                           show-foreign?
                                                           (not (gnc-commodity-equal (xaccAccountGetCommodity account) common-currency))
                                                           (account-col-amount account col-datum))))
                  (get-cell-monetary-fn (lambda (account col-datum)
                                          (let* ((monetary (account-col-amount account col-datum))
                                                 (col-startdate (car col-datum))
                                                 (col-enddate (cdr col-datum)))
                                            (if (and common-currency
                                                     (has-price? (xaccAccountGetCommodity account)))
                                                (gnc:exchange-by-pricedb-nearest
                                                 monetary common-currency
                                                 (case price-source
                                                   ((startperiod) startdate)
                                                   ((midperiod) (floor (/ (+ startdate enddate) 2)))
                                                   ((endperiod) enddate)
                                                   ((pricedb-latest) (current-time))))
                                                monetary))))
                  (get-cell-anchor-fn (lambda (account datepair)
                                        (gnc:make-report-anchor
                                         trep-uuid report-obj
                                         (list (list "General" "Start Date" (cons 'absolute (car datepair)))
                                               (list "General" "End Date" (cons 'absolute (cdr datepair)))
                                               (list "Accounts" "Accounts" (list account))))))
                  (chart (and include-chart?
                              (gnc:make-report-anchor
                               pnl-barchart-uuid report-obj
                               (list (list "General" "Start Date" (cons 'absolute startdate))
                                     (list "General" "End Date" (cons 'absolute enddate))
                                     (list "General" "Report's currency" (or common-currency (gnc-default-report-currency)))
                                     (list "General" "Price Source" (case price-source
                                                                      ((pricedb-latest) 'pricedb-latest)
                                                                      (else 'pricedb-nearest)))
                                     (list "Accounts" "Accounts" (append income-accounts expense-accounts))))))
                  (get-col-header-fn (lambda (accounts col-datum)
                                       (let* ((header (gnc:make-html-text
                                                       (qof-print-date (car col-datum))
                                                       (gnc:html-markup-br)
                                                       (_ " to ")
                                                       (qof-print-date (cdr col-datum))))
                                              (cell (gnc:make-html-table-cell/markup "total-label-cell" header)))
                                         (gnc:html-table-cell-set-style! cell "total-label-cell" 'attribute '("style" "text-align:right"))
                                         cell)))
                  (add-to-table (lambda (table title accounts get-col-header-fn hide-accounts? hide-grand-total? negate-amounts?)
                                  (add-multicolumn-acct-table
                                   table title accounts
                                   maxindent get-cell-monetary-fn report-datepairs
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:account-full-name? account-full-name?
                                   #:negate-amounts? negate-amounts?
                                   #:disable-amount-indent? disable-amount-indent?
                                   #:depth-limit (if get-col-header-fn 0 depth-limit)
                                   #:hide-accounts? hide-accounts?
                                   #:hide-grand-total? hide-grand-total?
                                   #:account-anchor-fn account->url
                                   #:get-cell-orig-monetary-fn (and common-currency get-cell-orig-monetary-fn)
                                   #:get-col-header-fn get-col-header-fn
                                   #:recursive-bals? recursive-bals?
                                   #:get-cell-anchor-fn get-cell-anchor-fn
                                   ))))

             (add-to-table multicol-table-left (_ "Period") '() get-col-header-fn #t #t #f)
             (if enable-dual-columns?
                 (add-to-table multicol-table-right (_ "Period") '() get-col-header-fn #t #t #f))             
             (add-to-table multicol-table-left (_ "Income") income-accounts #f #f #f #t)
             (add-to-table multicol-table-right (_ "Expense") expense-accounts #f #f #f #f)
             (add-to-table multicol-table-left (_ "Net Income") (append income-accounts expense-accounts) #f #t #f #t)
             (if (and common-currency show-rates?)
                 (add-to-table multicol-table-left (_ "Exchange Rates") (append income-accounts expense-accounts) get-exchange-rates-fn #t #t #f))

             (gnc:html-document-add-object!
              doc (gnc:html-render-options-changed (gnc:report-options report-obj)))

             (if include-chart?
                 (gnc:html-document-add-object!
                  doc
                  (gnc:make-html-text
                   (gnc:html-markup-anchor chart "Barchart"))))

             (let ((multicol-table (if enable-dual-columns?
                                       (gnc:make-html-table)
                                       multicol-table-left)))
               (when enable-dual-columns?
                 (gnc:html-table-append-row! multicol-table
                                             (list multicol-table-left multicol-table-right)))
               (gnc:html-document-add-object!
                doc multicol-table))))))

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
 'options-generator (lambda () (multicol-report-options-generator 'balsheet))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'balsheet)))

(gnc:define-report
 'version 1
 'name pnl-reportname
 'report-guid "0e94fd0277ba11e8825d43e27232c9d4"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator (lambda () (multicol-report-options-generator 'pnl))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'pnl)))

;; END
