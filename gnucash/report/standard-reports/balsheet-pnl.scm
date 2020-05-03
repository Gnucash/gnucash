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
(use-modules (srfi srfi-2))
(use-modules (srfi srfi-9))

(gnc:module-load "gnucash/report/report-system" 0)

;; the column-data record. the gnc:account-accumulate-at-dates will
;; create a record for each report-date with split-data as follows:
(define-record-type :col-datum
  (make-datum last-split split-balance split-value-balance)
  col-datum?
  (last-split col-datum-get-last-split)
  (split-balance col-datum-get-split-balance)
  (split-value-balance col-datum-get-split-value-balance))

(define FOOTER-TEXT
  (gnc:make-html-text
   (_ "WARNING: Foreign currency conversions, and unrealized gains
calculations are not confirmed correct. This report may be modified
without notice. Bug reports are very welcome at
https://bugs.gnucash.org/")))

;; define all option's names and help text so that they are properly

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

(define optname-amount-links (N_ "Display amounts as hyperlinks"))
(define opthelp-amount-links (N_ "Shows each amounts in the table as a hyperlink to a register or report."))

;; section labels
(define optname-label-sections (N_ "Label sections"))
(define opthelp-label-sections (N_ "Whether or not to include a label for sections."))
(define optname-total-sections (N_ "Include totals"))
(define opthelp-total-sections (N_ "Whether or not to include a line indicating total amounts."))

;; commodities
(define pagename-commodities (N_ "Commodities"))
(define optname-include-chart (N_ "Enable chart"))
(define opthelp-include-chart (N_ "Enable link to chart"))

(define optname-common-currency (N_ "Common Currency"))
(define opthelp-common-currency (N_ "Convert all amounts to a single currency."))

(define optname-report-commodity (N_ "Report's currency"))

(define optname-price-source (N_ "Price Source"))

(define optname-show-foreign (N_ "Show original currency amount"))
(define opthelp-show-foreign (N_ "Also show original currency amounts"))

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
   (list #f
         (cons 'text (_ "Disabled"))
         (cons 'tip (_ "Disabled")))

   (list 'YearDelta
         (cons 'text (_ "Year"))
         (cons 'tip (_ "One year.")))

   (list 'HalfYearDelta
         (cons 'text (_ "Half Year"))
         (cons 'tip (_ "Half Year.")))

   (list 'QuarterDelta
         (cons 'text (_ "Quarter"))
         (cons 'tip (_ "One Quarter.")))

   (list 'MonthDelta
         (cons 'text (_ "Month"))
         (cons 'tip (_ "One Month.")))

   (list 'TwoWeekDelta
         (cons 'text (_ "2Week"))
         (cons 'tip (_ "Two Weeks.")))

   (list 'WeekDelta
         (cons 'text (_ "Week"))
         (cons 'tip (_ "One Week.")))))

(define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))

(define (keylist-get-info keylist key info)
  (assq-ref (assq-ref keylist key) info))

;; options generator
(define (multicol-report-options-generator report-type)
  (let* ((options (gnc:new-options))
         (book (gnc-get-current-book))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

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
        (case report-type
          ((balsheet)
           (gnc-option-db-set-option-selectable-by-name
            options gnc:pagename-general optname-include-chart x)

           (gnc-option-db-set-option-selectable-by-name
            options gnc:pagename-general optname-startdate x))

          ((pnl)
           (gnc-option-db-set-option-selectable-by-name
            options gnc:pagename-general optname-include-overall-period x))))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-disable-amount-indent
      "c3" opthelp-disable-amount-indent #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-include-chart
      "c5" opthelp-include-chart #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-dual-columns
      "c4" opthelp-dual-columns #t))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-options-summary
      "d" opthelp-options-summary
      'never
      (list (vector 'always
                    (_ "Always")
                    (_ "Always display summary."))
            (vector 'never
                    (_ "Never")
                    (_ "Disable report summary.")))))

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

    ;; the depth-limit option is not well debugged; it may be better
    ;; to disable it altogether
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

    (gnc:options-add-price-source!
     options pagename-commodities
     optname-price-source "d" 'pricedb-nearest)

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
      gnc:pagename-display optname-amount-links
      "e5" opthelp-amount-links #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-full-name
      "f" opthelp-account-full-name #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-sections "g" opthelp-label-sections #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-sections "h" opthelp-total-sections #t))

    (when (eq? report-type 'pnl)
      ;; include overall period column?
      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-general optname-include-overall-period
        "c6" opthelp-include-overall-period #f)))

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

(define* (add-multicolumn-acct-table
          table title accountlist maxindent get-cell-monetary-fn cols-data #:key
          (omit-zb-bals? #f)
          (show-zb-accts? #t)
          (disable-account-indent? #f)
          (disable-amount-indent? #f)
          (show-orig-cur? #t)
          (show-title? #t)
          (show-accounts? #t)
          (show-total? #t)
          (depth-limit #f)
          (negate-amounts? #f)
          (recursive-bals? #f)
          (account-anchor? #t)
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
  ;; show-title?        - a bool to show/hide individual sections: title row
  ;; show-accounts?     - a bool to show/hide individual sections: accounts list and data columns
  ;; show-total?        - a bool to show/hide individual sections: accounts total
  ;; disable-account-indent? - a boolean to disable narrow-cell indenting, and render account full-name instead
  ;; disable-amount-indent? - a bool to disable amount indenting (only for single data column reports)
  ;; negate-amounts?    - a boolean to negate amounts. useful for e.g. income-type accounts.
  ;; depth-limit        - (untested) accounts whose levels exceed this depth limit are not shown
  ;; recursive-bals?    - a boolean to confirm recursive-balances enabled (parent-accounts show balances) or
  ;;                      disabled (multilevel subtotals after each parent+children)
  ;; account-anchor?    - a boolean to enable/disable account link to account
  ;; amount-anchor?     - a boolean to enable/disable amount link to report/register
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
    (gnc:make-html-table-cell/min-width 1))

  (define (add-indented-row indent label label-markup row-markup amount-indent rest)
    (when (or (not depth-limit) (<= indent depth-limit))
      (let* ((account-cell (if label-markup
                               (gnc:make-html-table-cell/size/markup
                                1 (if disable-account-indent? 1 (- maxindent indent))
                                label-markup label)
                               (gnc:make-html-table-cell/size
                                1 (if disable-account-indent? 1 (- maxindent indent))
                                label)))
             (row (append
                   (if disable-account-indent?
                       '()
                       (make-list-thunk indent make-narrow-cell))
                   (list account-cell)
                   (gnc:html-make-empty-cells
                    (if amount-indenting? (1- amount-indent) 0))
                   rest
                   (gnc:html-make-empty-cells
                    (if amount-indenting? (- maxindent amount-indent) 0)))))
        (if row-markup
            (gnc:html-table-append-row/markup! table row-markup row)
            (gnc:html-table-append-row! table row)))))

  (define (monetary+ . monetaries)
    ;; usage: (monetary+ monetary...)
    ;; inputs: list of gnc-monetary (e.g. USD 10, USD 25, GBP 5, GBP 8)
    ;; outputs: list of gnc-monetary (e.g. USD 35, GBP 13), or '()
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
         (let ((converted (and show-orig-cur?
                               convert-curr-fn
                               (convert-curr-fn monetary col-datum))))
           (if (not (and omit-zb-bals?
                         (gnc:gnc-monetary? monetary)
                         (zero? (gnc:gnc-monetary-amount monetary))))
               (gnc:html-text-append! text
                                      (if converted
                                          (gnc:html-markup-i
                                           (gnc:html-markup "small" monetary " "))
                                          "")
                                      (if anchor
                                          (gnc:html-markup-anchor
                                           anchor (or converted monetary))
                                          (or converted monetary))
                                      (gnc:html-markup-br)))))
       monetaries)
      text))

  (define (account->depth acc)
    (cond ((vector? acc) 0)
          (else (gnc-account-get-current-depth acc))))

  (define (account->descendants acc)
    (cond ((vector? acc) '())
          (else (gnc-account-get-descendants acc))))

  (define (render-account account total?)
    ;; input: account-name
    ;; outputs: string or html-markup-anchor object
    (let* ((virtual? (vector? account))
           (acct-name (cond
                       (virtual? (vector-ref account 0))
                       (disable-account-indent? (gnc-account-get-full-name account))
                       (else (xaccAccountGetName account))))
           (acct-label (if (and (not virtual?) total?)
                           (string-append (_ "Total For ") acct-name)
                           acct-name))
           (acct-url (and account-anchor?
                          (not total?)
                          (not virtual?)
                          (not (xaccAccountGetPlaceholder account))
                          (gnc:account-anchor-text account))))
      (gnc:make-html-text
       (if acct-url
           (gnc:html-markup-anchor acct-url acct-label)
           acct-label))))

  (define (add-whole-line contents)
    (gnc:html-table-append-row!
     table (gnc:make-html-table-cell/size
            1 (+ 1 (if disable-account-indent? 0 maxindent) num-columns)
            contents)))

  (define (account-and-descendants account)
    (cons account (filter (lambda (acc) (member acc accountlist))
                          (account->descendants account))))

  (define (sum-accounts-at-col accounts datum convert?)
    ;; outputs: list of gnc-monetary

    (let loop ((accounts accounts)
               (result '()))
      (cond
       ((null? accounts)
        (apply monetary+ result))
       (else
        (let* ((acc (car accounts))
               (monetary (if (vector? acc)
                             ((vector-ref acc 1) datum)
                             (get-cell-monetary-fn acc datum)))
               (amt (or (and convert? convert-curr-fn
                             (not (list? monetary))
                             (convert-curr-fn monetary datum))
                        monetary)))
          (loop (cdr accounts)
                (if (list? amt)
                    (append-reverse amt result)
                    (cons amt result))))))))

  (define (is-not-zero? accts)
    ;; this function tests whether accounts (with descendants) of all
    ;; columns are zero.
    (not (every zero? (concatenate
                       (map
                        (lambda (col-datum)
                          (map gnc:gnc-monetary-amount
                               (sum-accounts-at-col accts col-datum #f)))
                        cols-data)))))

  (define* (add-recursive-subtotal lvl lvl-acct #:key account-style-normal?)
    (if (or show-zb-accts?
            (is-not-zero? (account-and-descendants lvl-acct)))
        (add-indented-row lvl
                          (render-account lvl-acct (not account-style-normal?))
                          (if account-style-normal?
                              "text-cell"
                              "total-label-cell")
                          #f
                          (- maxindent lvl)
                          (map
                           (lambda (col-datum)
                             (gnc:make-html-table-cell/markup
                              "total-number-cell"
                              (list-of-monetary->html-text
                               (sum-accounts-at-col (account-and-descendants lvl-acct)
                                                    col-datum
                                                    #t)
                               col-datum
                               (and get-cell-anchor-fn
                                    (get-cell-anchor-fn
                                     (account-and-descendants lvl-acct)
                                     col-datum)))))
                           cols-data))))

  (define* (add-account-row lvl-curr curr #:key
                            (override-show-zb-accts? #f)
                            (account-indent 0))
    (if (or show-zb-accts?
            override-show-zb-accts?
            (is-not-zero? (list curr)))
        (add-indented-row lvl-curr
                          (render-account curr #f)
                          "text-cell"
                          #f
                          (- maxindent lvl-curr account-indent)
                          (map
                           (lambda (col-datum)
                             (gnc:make-html-table-cell/markup
                              "number-cell"
                              (list-of-monetary->html-text
                               (sum-accounts-at-col
                                (list curr)
                                col-datum
                                (not show-orig-cur?))
                               col-datum
                               (and get-cell-anchor-fn
                                    (not (vector? curr))
                                    (get-cell-anchor-fn curr col-datum)))))
                           cols-data))))

  ;; header ASSET/LIABILITY etc
  (if show-title?
      (add-indented-row 0
                        title
                        "total-label-cell"
                        "primary-subheading"
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
               (lvl-curr (account->depth curr))
               (lvl-next (if next (account->depth next) 0))
               (curr-descendants-list (filter
                                       (lambda (acc) (member acc accountlist))
                                       (account->descendants curr)))
               (recursive-parent-acct? (and recursive-bals?
                                            (pair? curr-descendants-list)))
               (multilevel-parent-acct? (and (not recursive-bals?)
                                             (pair? curr-descendants-list))))

          (if recursive-parent-acct?
              (begin
                (add-recursive-subtotal lvl-curr curr #:account-style-normal? #t)
                (if (is-not-zero? (list curr))
                    (add-account-row (1+ lvl-curr) curr #:override-show-zb-accts? #t)))
              (add-account-row lvl-curr curr
                               #:account-indent (if multilevel-parent-acct? 1 0)
                               #:override-show-zb-accts? multilevel-parent-acct?))

          (if (and (not recursive-bals?)
                   (> lvl-curr lvl-next))
              (let multilevel-loop ((lvl (1- lvl-curr))
                                    (lvl-acct (gnc-account-get-parent curr)))
                (unless (or (zero? lvl)
                            (not (member lvl-acct accountlist))
                            (< lvl lvl-next))
                  (add-recursive-subtotal lvl lvl-acct)
                  (multilevel-loop (1- lvl)
                                   (gnc-account-get-parent lvl-acct)))))
          (loop rest))))

  (if show-total?
      (add-indented-row 0
                        (string-append (_ "Total For ") title)
                        "total-label-cell"
                        "primary-subheading"
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
                         cols-data)))
  (add-whole-line #f))

(define (monetary-less . monetaries)
  ;; syntax: (monetary-less mon0 mon1 mon2 ...)
  ;; equiv:  (- mon0 mon1 mon2 ...)
  ;; this works only if all monetaries have the same commodity
  (let ((res (gnc:make-commodity-collector)))
    (res 'add (gnc:gnc-monetary-commodity (car monetaries))
         (gnc:gnc-monetary-amount (car monetaries)))
    (for-each
     (lambda (mon)
       (res 'add (gnc:gnc-monetary-commodity mon) (- (gnc:gnc-monetary-amount mon))))
     (cdr monetaries))
    (let ((reslist (res 'format gnc:make-gnc-monetary #f)))
      (if (null? (cdr reslist))
          (car reslist)
          (gnc:error "monetary-less: 1 commodity only" monetaries)))))

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
         (startdate ((if (eq? report-type 'pnl)
                         gnc:time64-start-day-time
                         gnc:time64-end-day-time)
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-startdate))))
         (enddate (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general optname-enddate))))
         (disable-account-indent? (get-option gnc:pagename-display
                                              optname-account-full-name))
         (incr (get-option gnc:pagename-general optname-period))
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
         (label-sections? (get-option gnc:pagename-display
                                      optname-label-sections))
         (total-sections? (get-option gnc:pagename-display
                                      optname-total-sections))
         (use-links? (get-option gnc:pagename-display
                                 optname-account-links))
         (use-amount-links? (get-option gnc:pagename-display
                                        optname-amount-links))
         (include-chart? (get-option gnc:pagename-general optname-include-chart))
         (common-currency (and
                           (get-option pagename-commodities optname-common-currency)
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
         (price-source (and common-currency
                            (get-option pagename-commodities optname-price-source)))

         (report-dates
          (cond
           (incr (gnc:make-date-list startdate enddate (gnc:deltasym-to-delta incr)))
           ((eq? report-type 'pnl) (list startdate enddate))
           (else (list enddate))))

         ;; an alist of (cons account account-cols-data) whereby
         ;; account-cols-data is a list of col-datum records
         (accounts-cols-data
          (map
           (lambda (acc)
             (let* ((comm (xaccAccountGetCommodity acc))
                    (val-coll (gnc:make-commodity-collector))
                    (amt->monetary (lambda (amt) (gnc:make-gnc-monetary comm amt))))
               (cons acc
                     (gnc:account-accumulate-at-dates
                      acc report-dates
                      #:nosplit->elt (make-datum #f (amt->monetary 0)
                                                 (gnc:make-commodity-collector))
                      #:split->elt
                      (lambda (s)
                        (unless (xaccTransGetIsClosingTxn (xaccSplitGetParent s))
                          (val-coll 'add
                                    (xaccTransGetCurrency (xaccSplitGetParent s))
                                    (xaccSplitGetValue s)))
                        (make-datum s (amt->monetary (xaccSplitGetNoclosingBalance s))
                                    (gnc:collector+ val-coll)))))))
           accounts))

         ;; an alist of (cons account account-balances) whereby
         ;; account-balances is a list of monetary amounts
         (accounts-balances
          (map
           (lambda (acc)
             (cons acc (let ((cols-data (assoc-ref accounts-cols-data acc)))
                         (map col-datum-get-split-balance cols-data))))
           accounts))

         (exchange-fn (and common-currency
                           (gnc:case-exchange-time-fn
                            price-source common-currency
                            (map xaccAccountGetCommodity accounts) enddate
                            #f #f)))

         ;; this function will convert the monetary found at col-idx
         ;; into report-currency if the latter exists. The price
         ;; applicable to the col-idx column is used. If the monetary
         ;; cannot be converted (eg. missing price) then it is not converted.
         (convert-curr-fn
          (lambda (monetary col-idx)
            (and common-currency
                 (not (gnc-commodity-equal
                       (gnc:gnc-monetary-commodity monetary)
                       common-currency))
                 (has-price? (gnc:gnc-monetary-commodity monetary))
                 (exchange-fn
                  monetary common-currency
                  (cond
                   ((eq? price-source 'pricedb-latest) (current-time))
                   ((eq? col-idx 'overall-period) (last report-dates))
                   ((eq? report-type 'balsheet) (list-ref report-dates col-idx))
                   ((eq? report-type 'pnl) (list-ref report-dates (1+ col-idx))))))))

         ;; the following function generates an gnc:html-text object
         ;; to dump exchange rate for a particular column. From the
         ;; accountlist given, obtain commodities, and convert 1 unit
         ;; currency into report-currency. If cannot convert due to
         ;; missing price, say so.
         (get-exchange-rates-fn
          (lambda (accounts col-idx)
            (let ((commodities (gnc:accounts-get-commodities accounts common-currency))
                  (cell (gnc:make-html-text)))
              (for-each
               (lambda (commodity)
                 (let ((orig-monetary (gnc:make-gnc-monetary commodity 1)))
                   (if (has-price? commodity)
                       (let ((conv-monetary (convert-curr-fn orig-monetary col-idx)))
                         (gnc:html-text-append!
                          cell
                          (format #f "~a ~a"
                                  (gnc:monetary->string orig-monetary)
                                  (gnc:monetary->string conv-monetary))))
                       (gnc:html-text-append!
                        cell
                        (string-append
                         (format #f "~a ~a "
                                 (gnc:monetary->string orig-monetary)
                                 (gnc-commodity-get-nice-symbol common-currency))
                         (_ "missing")))))
                 (gnc:html-text-append! cell (gnc:html-markup-br)))
               commodities)
              (gnc:make-html-table-cell/markup "number-cell" cell))))

         ;; scan accounts' commodities, filter currencies only, create
         ;; hash-map counter, convert to alist, sort descending tally,
         ;; return first pair's car. result=most used currency. the
         ;; (cons default-currency 0) avoids crash in an empty-book by
         ;; ensuring there is at least 1 currency.
         (book-main-currency
          (let ((h (make-hash-table)))
            (for-each
             (lambda (curr)
               (hash-set! h curr (1+ (hash-ref h curr 0))))
             (filter gnc-commodity-is-currency (map xaccAccountGetCommodity accounts)))
            (caar (sort! (cons (cons (gnc-default-report-currency) 0)
                               (hash-map->list cons h))
                         (lambda (a b) (> (cdr a) (cdr b)))))))

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

         (asset-liability (append-reverse asset-accounts liability-accounts))
         (income-expense (append-reverse income-accounts expense-accounts))

         (doc (gnc:make-html-document))
         (multicol-table-left (gnc:make-html-table))
         (multicol-table-right (if enable-dual-columns?
                                   (gnc:make-html-table)
                                   multicol-table-left))
         (maxindent (1+ (apply max (cons 0 (map gnc-account-get-current-depth
                                                accounts))))))

    (define (sum-balances-of-accounts alist accts adder)
      (let ((balances
             (fold (lambda (a b) (if (member (car a) accts) (cons (cdr a) b) b))
                   '() alist)))
        (list->vector
         (if (null? balances)
             (map (const (adder)) report-dates)
             (apply map adder balances)))))

    (gnc:html-document-set-title!
     doc (with-output-to-string
           (lambda ()
             (display report-title)
             (display " ")
             (if (or incr (eq? report-type 'pnl))
                 (format #t (_ "~a to ~a")
                         (qof-print-date startdate) (qof-print-date enddate))
                 (display (qof-print-date enddate))))))

    (if (eq? (get-option gnc:pagename-general optname-options-summary) 'always)
        (gnc:html-document-add-object!
         doc (gnc:html-render-options-changed (gnc:report-options report-obj))))

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       doc
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((eq? report-type 'balsheet)
      (let* ((get-cell-monetary-fn
              (lambda (account col-idx)
                (list-ref (assoc-ref accounts-balances account) col-idx)))

             ;; an alist of (cons account vector-of-splits) where each
             ;; split is the last one at date boundary
             (accounts-splits-dates
              (map
               (lambda (acc)
                 (cons acc (let ((cols-data (assoc-ref accounts-cols-data acc)))
                             (list->vector
                              (map col-datum-get-last-split cols-data)))))
               accounts))

             (get-cell-anchor-fn
              (lambda (account col-idx)
                (and-let* (((not (pair? account)))
                           (date-splits (assoc-ref accounts-splits-dates account))
                           (split (vector-ref date-splits col-idx)))
                  (gnc:split-anchor-text split))))

             ;; a vector of collectors whereby collector is the sum of
             ;; asset and liabilities at report dates
             (asset-liability-balances
              (sum-balances-of-accounts
               accounts-balances asset-liability gnc:monetaries-add))

             ;; a vector of collectors whereby collector is the sum of
             ;; incomes and expenses at report dates
             (income-expense-balances
              (sum-balances-of-accounts
               accounts-balances income-expense gnc:monetaries-add))

             ;; an alist of (cons account list-of-collectors) whereby each
             ;; collector is the split-value-balances at report
             ;; dates. split-value-balance determined by transaction currency.
             (accounts-value-balances
              (map
               (lambda (acc)
                 (cons acc (let ((cols-data (assoc-ref accounts-cols-data acc)))
                             (map col-datum-get-split-value-balance cols-data))))
               accounts))

             ;; a vector of collectors whereby each collector is the sum
             ;; of asset and liability split-value-balances at report
             ;; dates
             (asset-liability-value-balances
              (sum-balances-of-accounts
               accounts-value-balances asset-liability gnc:collector+))

             ;; converts monetaries to common currency
             (monetaries->exchanged
              (lambda (monetaries target-currency price-source date)
                (let ((exchange-fn (gnc:case-exchange-fn
                                    price-source target-currency date)))
                  (apply gnc:monetary+
                         (cons (gnc:make-gnc-monetary target-currency 0)
                               (map
                                (lambda (mon)
                                  (exchange-fn mon target-currency))
                                (monetaries 'format gnc:make-gnc-monetary #f)))))))

             ;; the unrealized gain calculator retrieves the
             ;; asset-and-liability report-date balance and
             ;; value-balance, and calculates the difference,
             ;; converted to report currency.
             (unrealized-gain-fn
              (lambda (col-idx)
                (and-let* (common-currency
                           (date (case price-source
                                   ((pricedb-latest) (current-time))
                                   (else (list-ref report-dates col-idx))))
                           (asset-liability-balance
                            (vector-ref asset-liability-balances col-idx))
                           (asset-liability-basis
                            (vector-ref asset-liability-value-balances col-idx))
                           (unrealized (gnc:collector- asset-liability-basis
                                                       asset-liability-balance)))
                  (monetaries->exchanged
                   unrealized common-currency price-source date))))

             ;; the retained earnings calculator retrieves the
             ;; income-and-expense report-date balance, and converts
             ;; to report currency.
             (retained-earnings-fn
              (lambda (col-idx)
                (let* ((date (case price-source
                               ((pricedb-latest) (current-time))
                               (else (list-ref report-dates col-idx))))
                       (income-expense-balance
                        (vector-ref income-expense-balances col-idx)))
                  (if (and common-currency
                           (every has-price?
                                  (gnc:accounts-get-commodities income-expense #f)))
                      (monetaries->exchanged income-expense-balance
                                             common-currency price-source date)
                      (income-expense-balance 'format gnc:make-gnc-monetary #f)))))

             (chart (and-let* (include-chart?
                               incr
                               (curr (or common-currency book-main-currency))
                               (price (or price-source 'pricedb-nearest)))
                      (gnc:make-report-anchor
                       networth-barchart-uuid report-obj
                       (list (list "General" "Start Date" (cons 'absolute startdate))
                             (list "General" "End Date" (cons 'absolute enddate))
                             (list "General" "Report's currency" curr)
                             (list "General" "Step Size" incr)
                             (list "General" "Price Source" price)
                             (list "Accounts" "Accounts" asset-liability)))))

             (get-col-header-fn
              (lambda (accounts col-idx)
                (let* ((date (list-ref report-dates col-idx))
                       (header (qof-print-date date))
                       (cell (gnc:make-html-table-cell/markup
                              "total-label-cell" header)))
                  (gnc:html-table-cell-set-style!
                   cell "total-label-cell"
                   'attribute '("style" "text-align:right"))
                  cell)))

             (add-to-table (lambda* (table title accounts #:key
                                           (get-col-header-fn #f)
                                           (show-accounts? #t)
                                           (show-total? #t)
                                           (show-title? #t)
                                           (force-total? #f)
                                           (convert-fn #f)
                                           (negate-amounts? #f))
                             (add-multicolumn-acct-table
                              table title accounts
                              maxindent get-cell-monetary-fn
                              (iota (length report-dates))
                              #:omit-zb-bals? omit-zb-bals?
                              #:show-zb-accts? show-zb-accts?
                              #:disable-account-indent? disable-account-indent?
                              #:negate-amounts? negate-amounts?
                              #:disable-amount-indent? disable-amount-indent?
                              #:depth-limit (if get-col-header-fn 0 depth-limit)
                              #:show-orig-cur? (and (not convert-fn) show-foreign?)
                              #:show-title? (and show-title? label-sections?)
                              #:show-accounts? show-accounts?
                              #:show-total? (or (and total-sections? show-total?)
                                                force-total?)
                              #:recursive-bals? recursive-bals?
                              #:account-anchor? use-links?
                              #:convert-curr-fn (and common-currency
                                                     (or convert-fn convert-curr-fn))
                              #:get-col-header-fn get-col-header-fn
                              #:get-cell-anchor-fn (and use-amount-links?
                                                        get-cell-anchor-fn)
                              ))))

        (when incr
          (add-to-table multicol-table-left (_ "Date") '()
                        #:get-col-header-fn get-col-header-fn
                        #:show-accounts? #f
                        #:show-total? #f)
          (if enable-dual-columns?
              (add-to-table multicol-table-right (_ "Date") '()
                            #:get-col-header-fn get-col-header-fn
                            #:show-accounts? #f
                            #:show-total? #f)))

        (unless (null? asset-accounts)
          (add-to-table multicol-table-left (_ "Asset") asset-accounts))

        (unless (null? liability-accounts)
          (add-to-table multicol-table-right (_ "Liability") liability-accounts
                        #:negate-amounts? #t))

        (add-to-table
         multicol-table-right (_ "Equity")
         (append equity-accounts
                 (if common-currency
                     (list (vector (_ "Unrealized Gains")
                                   unrealized-gain-fn))
                     '())
                 (if (null? income-expense)
                     '()
                     (list (vector (_ "Retained Earnings")
                                   retained-earnings-fn))))
         #:negate-amounts? #t)

        (if (and common-currency show-rates?)
            (add-to-table multicol-table-right (_ "Exchange Rates")
                          asset-liability
                          #:get-col-header-fn get-exchange-rates-fn
                          #:show-accounts? #f
                          #:show-total? #f))

        (if (and include-chart? incr)
            (gnc:html-document-add-object!
             doc
             (gnc:make-html-text
              (gnc:html-markup-anchor chart (_ "Barchart")))))))

     ((eq? report-type 'pnl)
      (let* ((include-overall-period? (get-option gnc:pagename-general
                                                  optname-include-overall-period))
             (col-idx->datepair
              (lambda (idx)
                (cond
                 ((eq? idx 'overall-period)
                  (cons (car report-dates) (last report-dates)))
                 ((= idx (- (length report-dates) 2))
                  (cons (list-ref report-dates idx) (last report-dates)))
                 (else
                  (cons (list-ref report-dates idx)
                        (decdate (list-ref report-dates (1+ idx)) DayDelta))))))

             (col-idx->monetarypair (lambda (balancelist idx)
                                      (if (eq? idx 'overall-period)
                                          (cons (car balancelist) (last balancelist))
                                          (cons (list-ref balancelist idx)
                                                (list-ref balancelist (1+ idx))))))

             (get-cell-monetary-fn
              (lambda (account col-idx)
                (let* ((balances (assoc-ref accounts-balances account))
                       (monetarypair (col-idx->monetarypair balances col-idx)))
                  (monetary-less
                   (cdr monetarypair)
                   (car monetarypair)))))

             (get-cell-anchor-fn
              (lambda (account col-idx)
                (let ((datepair (col-idx->datepair col-idx))
                      (show-orig? (and common-currency #t))
                      (curr (or common-currency book-main-currency))
                      (delta (or incr 'MonthDelta))
                      (price (or price-source 'pricedb-nearest))
                      (accts (if (pair? account) account (list account))))
                  (gnc:make-report-anchor
                   trep-uuid report-obj
                   (list
                    (list "General" "Start Date" (cons 'absolute (car datepair)))
                    (list "General" "End Date" (cons 'absolute (cdr datepair)))
                    (list "General" "Show original currency amount" show-orig?)
                    (list "General" "Common Currency" common-currency)
                    (list "General" "Report's currency" curr)
                    (list "Display" "Amount" 'double)
                    (list "Accounts" "Accounts" accts))))))

             (chart
              (and-let* (include-chart?
                         (curr (or common-currency book-main-currency))
                         (delta (or incr 'MonthDelta))
                         (price (or price-source 'pricedb-nearest)))
                (gnc:make-report-anchor
                 pnl-barchart-uuid report-obj
                 (list (list "General" "Start Date" (cons 'absolute startdate))
                       (list "General" "End Date" (cons 'absolute enddate))
                       (list "General" "Report's currency" curr)
                       (list "General" "Step Size" delta)
                       (list "General" "Price Source" price)
                       (list "Accounts" "Accounts" income-expense)))))

             (get-col-header-fn
              (lambda (accounts col-idx)
                (let* ((datepair (col-idx->datepair col-idx))
                       (header (gnc:make-html-text
                                (qof-print-date (car datepair))
                                (gnc:html-markup-br)
                                (_ " to ")
                                (qof-print-date (cdr datepair))))
                       (cell (gnc:make-html-table-cell/markup
                              "total-label-cell" header)))
                  (gnc:html-table-cell-set-style!
                   cell "total-label-cell"
                   'attribute '("style" "text-align:right"))
                  cell)))

             (add-to-table (lambda* (table title accounts #:key
                                           (get-col-header-fn #f)
                                           (show-accounts? #t)
                                           (show-total? #t)
                                           (force-total? #f)
                                           (negate-amounts? #f))
                             (add-multicolumn-acct-table
                              table title accounts
                              maxindent get-cell-monetary-fn
                              (append
                               (iota (1- (length report-dates)))
                               (if (and include-overall-period?
                                        (> (length report-dates) 2))
                                   '(overall-period)
                                   '()))
                              #:omit-zb-bals? omit-zb-bals?
                              #:show-zb-accts? show-zb-accts?
                              #:disable-account-indent? disable-account-indent?
                              #:negate-amounts? negate-amounts?
                              #:disable-amount-indent? disable-amount-indent?
                              #:depth-limit (if get-col-header-fn 0 depth-limit)
                              #:show-orig-cur? show-foreign?
                              #:show-title? label-sections?
                              #:show-accounts? show-accounts?
                              #:show-total? (or (and total-sections? show-total?)
                                                force-total?)
                              #:recursive-bals? recursive-bals?
                              #:account-anchor? use-links?
                              #:convert-curr-fn (and common-currency convert-curr-fn)
                              #:get-col-header-fn get-col-header-fn
                              #:get-cell-anchor-fn (and use-amount-links?
                                                        get-cell-anchor-fn)))))

        (when incr
          (add-to-table multicol-table-left (_ "Period") '()
                        #:get-col-header-fn get-col-header-fn
                        #:show-accounts? #f
                        #:show-total? #f)
          (if enable-dual-columns?
              (add-to-table multicol-table-right (_ "Period") '()
                            #:get-col-header-fn get-col-header-fn
                            #:show-accounts? #f
                            #:show-total? #f)))

        (unless (null? income-accounts)
          (add-to-table multicol-table-left (_ "Income") income-accounts
                        #:negate-amounts? #t))

        (unless (null? expense-accounts)
          (add-to-table multicol-table-right (_ "Expense") expense-accounts))

        (unless (or (null? income-accounts)
                    (null? expense-accounts))
          (add-to-table multicol-table-left (_ "Net Income")
                        income-expense
                        #:show-accounts? #f
                        #:negate-amounts? #t
                        #:force-total? #t))

        (if (and common-currency show-rates?)
            (add-to-table multicol-table-left (_ "Exchange Rates")
                          income-expense
                          #:get-col-header-fn get-exchange-rates-fn
                          #:show-accounts? #f
                          #:show-total? #f))

        (if include-chart?
            (gnc:html-document-add-object!
             doc
             (gnc:make-html-text
              (gnc:html-markup-anchor chart "Barchart")))))))

    (let ((multicol-table (if enable-dual-columns?
                              (gnc:make-html-table)
                              multicol-table-left)))
      (when enable-dual-columns?
        (gnc:html-table-append-row! multicol-table
                                    (list multicol-table-left multicol-table-right)))
      (gnc:html-document-add-object!
       doc multicol-table))

    (gnc:html-document-add-object!
     doc FOOTER-TEXT)

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
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (multicol-report-options-generator 'balsheet))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'balsheet)))

(gnc:define-report
 'version 1
 'name pnl-reportname
 'report-guid "0e94fd0277ba11e8825d43e27232c9d4"
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (multicol-report-options-generator 'pnl))
 'renderer (lambda (rpt) (multicol-report-renderer rpt 'pnl)))

;; END
