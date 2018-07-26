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

(define optname-options-summary (N_ "Add options summary"))
(define opthelp-options-summary (N_ "Add summary of options."))

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

;; section labels - for P&L report
(define optname-label-revenue (N_ "Label the revenue section"))
(define opthelp-label-revenue (N_ "Whether or not to include a label for the revenue section."))
(define optname-total-revenue (N_ "Include revenue total"))
(define opthelp-total-revenue (N_ "Whether or not to include a line indicating total revenue."))
(define optname-label-trading (N_ "Label the trading accounts section"))
(define opthelp-label-trading (N_ "Whether or not to include a label for the trading accounts section."))
(define optname-total-trading (N_ "Include trading accounts total"))
(define opthelp-total-trading (N_ "Whether or not to include a line indicating total trading accounts balance."))
(define optname-label-expense (N_ "Label the expense section"))
(define opthelp-label-expense (N_ "Whether or not to include a label for the expense section."))
(define optname-total-expense (N_ "Include expense total"))
(define opthelp-total-expense (N_ "Whether or not to include a line indicating total expense."))

;; section labels - balance-sheet
(define optname-label-assets (N_ "Label the assets section"))
(define opthelp-label-assets (N_ "Whether or not to include a label for the assets section."))
(define optname-total-assets (N_ "Include assets total"))
(define opthelp-total-assets (N_ "Whether or not to include a line indicating total assets."))
(define optname-label-liabilities (N_ "Label the liabilities section"))
(define opthelp-label-liabilities (N_ "Whether or not to include a label for the liabilities section."))
(define optname-total-liabilities (N_ "Include liabilities total"))
(define opthelp-total-liabilities (N_ "Whether or not to include a line indicating total liabilities."))
(define optname-label-equity (N_ "Label the equity section"))
(define opthelp-label-equity (N_ "Whether or not to include a label for the equity section."))
(define optname-total-equity (N_ "Include equity total"))
(define opthelp-total-equity (N_ "Whether or not to include a line indicating total equity."))

;; legacy account options
(define optname-bottom-behavior (N_ "Flatten list to depth limit"))

;; legacy display options
(define optname-use-rules (N_ "Show accounting-style rules"))

;; legacy general options
(define optname-standard-order-balsheet "Use standard US layout")
(define optname-standard-order-pnl "Display in standard, income first, order")
(define optname-report-form "Single column Balance Sheet")
(define optname-two-column "Display as a two column report")

;; commodities
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

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-options-summary
      "d" opthelp-options-summary #f))

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

    (for-each
     (lambda (opt)
       (add-option
        (gnc:make-simple-boolean-option
         gnc:pagename-display (vector-ref opt 0) (vector-ref opt 1) (vector-ref opt 2) #t)))
     (case report-type
       ((balsheet)
        (list (vector optname-label-assets "g1" opthelp-label-assets)
              (vector optname-total-assets "g2" opthelp-total-assets)
              (vector optname-label-liabilities "g3" opthelp-label-liabilities)
              (vector optname-total-liabilities "g4" opthelp-total-liabilities)
              (vector optname-label-equity "g5" opthelp-label-equity)
              (vector optname-total-equity "g6" opthelp-total-equity)))
       ((pnl)
        (list (vector optname-label-revenue "g1" opthelp-label-revenue)
              (vector optname-total-revenue "g2" opthelp-total-revenue)
              (vector optname-label-trading "g3" opthelp-label-trading)
              (vector optname-total-trading "g4" opthelp-total-trading)
              (vector optname-label-expense "g5" opthelp-label-expense)
              (vector optname-total-expense "g6" opthelp-total-expense)))))

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

    ;; set unused legacy options
    (for-each
     (lambda (optionset)
       (for-each
        (lambda (optname)
          (add-option
           (gnc:make-internal-option (car optionset) optname #f)))
        (cdr optionset)))
     (list
      (list gnc:pagename-general
            optname-standard-order-balsheet
            optname-standard-order-pnl
            optname-two-column
            optname-report-form)
      (list gnc:pagename-display optname-use-rules)
      (list gnc:pagename-accounts optname-bottom-behavior)))

    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)

    options))

(define* (add-multicolumn-acct-table
          table title accountlist maxindent get-cell-monetary-fn cols-data #:key
          (omit-zb-bals? #f)
          (show-zb-accts? #t)
          (account-full-name? #f)
          (disable-amount-indent? #f)
          (show-orig-cur? #t)
          (show-accounts? #t)
          (show-total? #t)
          (depth-limit #f)
          (negate-amounts? #f)
          (recursive-bals? #f)
          (account-anchor-fn #f)
          (get-col-header-fn #f)
          (convert-curr-fn #f)
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
  ;; show-title?        - a bool to show/hide individual sections
  ;; show-accounts?     - a bool to show/hide individual sections
  ;; show-total?        - a bool to show/hide individual sections
  ;; account-full-name? - a boolean to disable narrow-cell indenting, and render account full-name instead
  ;; disable-amount-indent? - a bool to disable amount indenting (only for single data column reports)
  ;; negate-amounts?    - a boolean to negate amounts. useful for e.g. income-type accounts.
  ;; depth-limit        - (untested) accounts whose levels exceed this depth limit are not shown
  ;; recursive-bals?    - a boolean to confirm recursive-balances enabled (parent-accounts show balances) or
  ;;                      disabled (multilevel subtotals after each parent+children)
  ;; get-col-header-fn  - a lambda (accounts cols-data) to produce html-object - this is optional
  ;; convert-curr-fn    - a lambda (monetary cols-data) which produces a gnc-monetary or #f - optional
  ;; show-orig-cur?     - a boolean to enable/disable original currency after convert-curr-fn
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
         (if monetary
             (coll 'add
                   (gnc:gnc-monetary-commodity monetary)
                   (let ((amount (gnc:gnc-monetary-amount monetary)))
                     (if negate-amounts? (- amount) amount)))))
       monetaries)
      (coll 'format gnc:make-gnc-monetary #f)))

  (define (list-of-monetary->html-text monetaries col-datum anchor)
    ;; inputs:
    ;; monetaries: list of gnc-monetary (or #f, or html-text object)
    ;; col-datum: col-datum to help convert monetary currency
    ;; anchor: url string for monetaries (or #f) (all have same anchor)
    ;;
    ;; outputs: html-text object
    (let ((text (gnc:make-html-text)))
      (for-each
       (lambda (monetary)
         (let ((converted (and show-orig-cur? convert-curr-fn (convert-curr-fn monetary col-datum))))
           (if (not (and omit-zb-bals? (gnc:gnc-monetary? monetary) (zero? (gnc:gnc-monetary-amount monetary))))
               (gnc:html-text-append! text
                                      (if converted
                                          (gnc:html-markup-i
                                           (gnc:html-markup "small" monetary))
                                          "")
                                      " "
                                      (if anchor
                                          (gnc:html-markup-anchor anchor (or converted monetary))
                                          (or converted monetary))
                                      (gnc:html-markup-br)))))
       (or monetaries '()))
      text))

  (define (render-account account total?)
    ;; input: account-name
    ;; outputs: string or html-markup-anchor object
    (let* ((acct-name ((if account-full-name?
                           gnc-account-get-full-name
                           xaccAccountGetName) account))
           (acct-label (if total?
                           (string-append (_ "Total For ") acct-name)
                           acct-name))
           (acct-url (and (not total?) account-anchor-fn (account-anchor-fn account))))
      (gnc:make-html-text
       (if acct-url
           (gnc:html-markup-anchor acct-url acct-label)
           acct-label))))

  (define (add-whole-line contents)
    (gnc:html-table-append-row!
     table (gnc:make-html-table-cell/size 1 (+ 1 (if account-full-name? 0 maxindent) num-columns) contents)))

  (define (account-and-descendants account)
    (cons account (filter (lambda (acc) (member acc accountlist))
                          (gnc-account-get-descendants account))))

  (define (sum-accounts-at-col accounts datum convert?)
    ;; outputs: list of gnc-monetary
    (apply monetary+
           (map (lambda (acc)
                  (let ((monetary (get-cell-monetary-fn acc datum)))
                    (or (and convert? convert-curr-fn (convert-curr-fn monetary datum))
                        monetary)))
                accounts)))

  ;; header ASSET/LIABILITY etc
  (if show-title?
      (add-indented-row 0
                        title
                        "total-label-cell"
                        maxindent
                        (if get-col-header-fn
                            (map
                             (lambda (col-datum)
                               (get-col-header-fn accountlist col-datum))
                             cols-data)
                            (gnc:html-make-empty-cells num-columns))))

  (let loop ((accounts (if show-accounts? accountlist '())))
    (if (pair? accounts)
        (let* ((curr (car accounts))
               (rest (cdr accounts))
               (next (and (pair? rest) (car rest)))
               (lvl-curr (gnc-account-get-current-depth curr))
               (lvl-next (if next (gnc-account-get-current-depth next) 0))
               (curr-descendants-list (filter (lambda (acc) (member acc accountlist))
                                              (gnc-account-get-descendants curr)))
               (recursive-parent-acct? (and recursive-bals? (pair? curr-descendants-list)))
               (multilevel-parent-acct? (and (not recursive-bals?) (pair? curr-descendants-list))))

          (if (and (or show-zb-accts?
                       ;; the following function tests whether accounts (with descendants) of
                       ;; all columns are zero
                       (not (every zero? (concatenate
                                          (map (lambda (col-datum)
                                                 (map gnc:gnc-monetary-amount
                                                      (sum-accounts-at-col (account-and-descendants curr)
                                                                           col-datum
                                                                           #f)))
                                               cols-data)))))
                   (or (not depth-limit) (<= lvl-curr depth-limit)))

              (begin

                (add-indented-row lvl-curr
                                  (render-account curr #f)
                                  "text-cell"
                                  (- maxindent
                                     lvl-curr
                                     (if multilevel-parent-acct? 1 0))
                                  (map
                                   (lambda (col-datum)
                                     (gnc:make-html-table-cell/markup
                                      (if (or (not recursive-bals?) (null? curr-descendants-list)) "number-cell" "total-number-cell")
                                      (list-of-monetary->html-text
                                       (sum-accounts-at-col (if recursive-bals? (account-and-descendants curr) (list curr))
                                                            col-datum
                                                            (not show-orig-cur?))
                                       col-datum
                                       (and get-cell-anchor-fn
                                            (not recursive-parent-acct?)
                                            (get-cell-anchor-fn curr col-datum)))))
                                   cols-data))

                ;; the following handles 'special' case where placeholder has descendants. only for recursive-bals? = true
                (if (and recursive-parent-acct?
                         (or (not depth-limit) (<= (1+ lvl-curr) depth-limit))
                         (not (every zero? (map (lambda (col-datum) (gnc:gnc-monetary-amount (get-cell-monetary-fn curr col-datum)))
                                                cols-data))))
                    (add-indented-row (1+ lvl-curr)
                                      (render-account curr #f)
                                      "text-cell"
                                      (- maxindent lvl-curr 1)
                                      (map
                                       (lambda (col-datum)
                                         (gnc:make-html-table-cell/markup
                                          "number-cell"
                                          (list-of-monetary->html-text
                                           (sum-accounts-at-col (list curr)
                                                                col-datum
                                                                (not show-orig-cur?))
                                           col-datum
                                           (and get-cell-anchor-fn
                                                (get-cell-anchor-fn curr col-datum)))))
                                       cols-data)))))

          (if (and (not recursive-bals?)
                   (or (not depth-limit) (<= lvl-curr depth-limit))
                   (> lvl-curr lvl-next))
              (let multilevel-loop ((lvl (1- lvl-curr))
                                    (lvl-acct (gnc-account-get-parent curr)))
                (unless (or (zero? lvl)
                            (not (member lvl-acct accountlist))
                            (< lvl lvl-next))
                  (add-indented-row lvl
                                    (render-account lvl-acct #t)
                                    "total-label-cell"
                                    (- maxindent lvl)
                                    (map
                                     (lambda (col-datum)
                                       (gnc:make-html-table-cell/markup
                                        "total-number-cell"
                                        (list-of-monetary->html-text
                                         (sum-accounts-at-col (account-and-descendants lvl-acct)
                                                              col-datum
                                                              (not show-orig-cur?))
                                         col-datum
                                         #f)))
                                     cols-data))
                  (multilevel-loop (1- lvl)
                                   (gnc-account-get-parent lvl-acct)))))
          (loop rest))))

  (add-whole-line #f)

  (if show-total?
      (add-indented-row 0
                        (string-append (_ "Total For ") title)
                        "total-label-cell"
                        maxindent
                        (map
                         (lambda (col-datum)
                           (let ((total-cell (gnc:make-html-table-cell/markup
                                              "total-number-cell"
                                              (list-of-monetary->html-text
                                               (sum-accounts-at-col accountlist
                                                                    col-datum
                                                                    #t)
                                               col-datum
                                               #f))))
                             (gnc:html-table-cell-set-style!
                              total-cell "total-number-cell"
                              'attribute '("style" "border-top-style:solid; border-top-width: 1px; border-bottom-style:double"))
                             total-cell))
                         cols-data))))

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
                       ;; the following tests whether an amount in commodity can be converted to
                       ;; common-currency. if conversion successful, it will be a non-zero value.
                       ;; note if we use API gnc-pricedb-has-prices, we're only querying the pricedb.
                       ;; if we use gnc-pricedb-convert-balance-latest-price, we can potentially
                       ;; use an intermediate currency.
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
         (doc (gnc:make-html-document))
         (multicol-table-left (gnc:make-html-table))
         (multicol-table-right (if enable-dual-columns?
                                   (gnc:make-html-table)
                                   multicol-table-left))
         (maxindent (gnc-account-get-tree-depth (gnc-get-current-root-account))))

    (gnc:html-document-set-title!
     doc (string-append company-name " " report-title " "
                        (if (and (eq? report-type 'balsheet) (not incr))
                            ""
                            (string-append (qof-print-date startdate) " - "))
                        (qof-print-date enddate)))

    (if (get-option gnc:pagename-general optname-options-summary)
        (gnc:html-document-add-object!
         doc (gnc:html-render-options-changed (gnc:report-options report-obj))))

    (if (null? accounts)

        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          report-title (gnc:report-id report-obj)))

        (case report-type
          ((balsheet)
           (let* ((report-dates (if incr
                                    (gnc:make-date-list startdate enddate incr)
                                    (list enddate)))
                  (convert-curr-fn (lambda (monetary col-datum)
                                     (and common-currency
                                          (not (gnc-commodity-equal (gnc:gnc-monetary-commodity monetary) common-currency))
                                          (has-price? (gnc:gnc-monetary-commodity monetary))
                                          (gnc:exchange-by-pricedb-nearest
                                           monetary common-currency
                                           (case price-source
                                             ((pricedb-nearest) col-datum)
                                             ((pricedb-latest) (current-time)))))))
                  (get-cell-monetary-fn (lambda (account col-datum)
                                          (let* ((col-date (gnc:time64-end-day-time col-datum)))
                                            (gnc:make-gnc-monetary
                                             (xaccAccountGetCommodity account)
                                             (xaccAccountGetBalanceAsOfDate
                                              account
                                              (gnc:time64-end-day-time col-date))))))
                  (get-cell-anchor-fn (lambda (account col-datum)
                                        (let* ((splits (xaccAccountGetSplitList account))
                                               (split-date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
                                               (date (gnc:time64-end-day-time col-datum))
                                               (valid-split? (lambda (s) (< (split-date s) date)))
                                               (valid-splits (filter valid-split? splits))
                                               (sorted-splits (stable-sort! valid-splits
                                                                            (lambda (a b)
                                                                              (< (split-date a) (split-date b)))))
                                               (split (and (pair? sorted-splits) (last sorted-splits))))
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
                  (add-to-table (lambda (table title accounts get-col-header-fn show-accounts? show-section-total? negate-amounts?)
                                  (add-multicolumn-acct-table
                                   table title accounts
                                   maxindent get-cell-monetary-fn report-dates
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:account-full-name? account-full-name?
                                   #:negate-amounts? negate-amounts?
                                   #:disable-amount-indent? disable-amount-indent?
                                   #:show-orig-cur? show-foreign?
                                   #:show-accounts? show-accounts?
                                   #:show-section-total? show-section-total?
                                   #:depth-limit (if get-col-header-fn 0 depth-limit)
                                   #:recursive-bals? recursive-bals?
                                   #:account-anchor-fn account->url
                                   #:convert-curr-fn (and common-currency convert-curr-fn)
                                   #:get-col-header-fn get-col-header-fn
                                   #:get-cell-anchor-fn get-cell-anchor-fn
                                   ))))

             (add-to-table multicol-table-left (_ "Date") '() get-col-header-fn #f #f #f)
             (if enable-dual-columns?
                 (add-to-table multicol-table-right (_ "Date") '() get-col-header-fn #f #f #f))
             (add-to-table multicol-table-left (_ "Asset") asset-accounts #f #t #t #f)
             (add-to-table multicol-table-right (_ "Liability") liability-accounts #f #t #t #t)
             ;; (add-to-table (_ "Equity") equity-accounts #f #f #f #t)
             ;; (unless (null? trading-accounts)
             ;;   (add-to-table multicol-table-right (_ "Trading Accounts") trading-accounts #f #t #t #f))
             (add-to-table multicol-table-right (_ "Net Worth") (append asset-accounts liability-accounts) #f #f #t #f)
             (if (and common-currency show-rates?)
                 (add-to-table multicol-table-right (_ "Exchange Rates") (append asset-accounts liability-accounts trading-accounts) get-exchange-rates-fn #f #f #f))

             (if include-chart?
                 (gnc:html-document-add-object!
                  doc
                  (gnc:make-html-text
                   (gnc:html-markup-anchor chart "Barchart"))))))

          ((pnl)
           (let* ((closing-str (get-option pagename-entries optname-closing-pattern))
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
                  (convert-curr-fn (lambda (monetary col-datum)
                                     (and common-currency
                                          (not (gnc-commodity-equal (gnc:gnc-monetary-commodity monetary) common-currency))
                                          (has-price? (gnc:gnc-monetary-commodity monetary))
                                          (gnc:exchange-by-pricedb-nearest
                                           monetary common-currency
                                           (case price-source
                                             ((startperiod) (car col-datum))
                                             ((midperiod) (floor (/ (+ (car col-datum) (cdr col-datum)) 2)))
                                             ((endperiod) (cdr col-datum))
                                             ((pricedb-latest) (current-time)))))))
                  (get-cell-monetary-fn (lambda (account col-datum)
                                          (let* ((startdate (car col-datum))
                                                 (enddate (cdr col-datum)))
                                            (gnc:make-gnc-monetary
                                             (xaccAccountGetCommodity account)
                                             (- (xaccAccountGetBalanceAsOfDate account enddate)
                                                (xaccAccountGetBalanceAsOfDate account startdate)
                                                (closing-adjustment account startdate enddate))))))
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
                  (add-to-table (lambda (table title accounts get-col-header-fn show-accounts? show-section-total? negate-amounts?)
                                  (add-multicolumn-acct-table
                                   table title accounts
                                   maxindent get-cell-monetary-fn report-datepairs
                                   #:omit-zb-bals? omit-zb-bals?
                                   #:show-zb-accts? show-zb-accts?
                                   #:account-full-name? account-full-name?
                                   #:negate-amounts? negate-amounts?
                                   #:disable-amount-indent? disable-amount-indent?
                                   #:depth-limit (if get-col-header-fn 0 depth-limit)
                                   #:show-orig-cur? show-foreign?
                                   #:show-accounts? show-accounts?
                                   #:show-section-total? show-section-total?
                                   #:account-anchor-fn account->url
                                   #:convert-curr-fn (and common-currency convert-curr-fn)
                                   #:get-col-header-fn get-col-header-fn
                                   #:recursive-bals? recursive-bals?
                                   #:get-cell-anchor-fn get-cell-anchor-fn
                                   ))))

             (add-to-table multicol-table-left (_ "Period") '() get-col-header-fn #f #f #f)
             (if enable-dual-columns?
                 (add-to-table multicol-table-right (_ "Period") '() get-col-header-fn #f #f #f))
             (add-to-table multicol-table-left (_ "Income") income-accounts #f #t #t #t)
             (add-to-table multicol-table-right (_ "Expense") expense-accounts #f #f #t #f)
             (add-to-table multicol-table-left (_ "Net Income") (append income-accounts expense-accounts) #f #f #t #t)
             (if (and common-currency show-rates?)
                 (add-to-table multicol-table-left (_ "Exchange Rates") (append income-accounts expense-accounts) get-exchange-rates-fn #f #f #f))

             (if include-chart?
                 (gnc:html-document-add-object!
                  doc
                  (gnc:make-html-text
                   (gnc:html-markup-anchor chart "Barchart"))))))))

    (let ((multicol-table (if enable-dual-columns?
                              (gnc:make-html-table)
                              multicol-table-left)))
      (when enable-dual-columns?
        (gnc:html-table-append-row! multicol-table
                                    (list multicol-table-left multicol-table-right)))
      (gnc:html-document-add-object!
       doc multicol-table))

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
