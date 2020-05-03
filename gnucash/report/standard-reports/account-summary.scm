;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-summary.scm : account listing/chart of accounts
;; 
;; Rewritten 2004.07.27 by David Montenegro <sunrise2000@comcast.net>
;;   same license & restrictions apply
;; 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
;; Copyright 2000-2001 Bill Gribble <grib@gnumatic.com>
;;
;; Even older original version by  Terry D. Boldt (tboldt@attglobal.net>
;;   Author makes no implicit or explicit guarantee of accuracy of
;;   these calculations and accepts no responsibility for direct
;;   or indirect losses incurred as a result of using this software.
;; 
;;  * BUGS:
;;    
;;    Does not currently provide all possible account attributes.
;;    
;;    Table does not currently use row style attributes.
;;    
;;    Progress bar functionality is currently mostly broken.
;;    
;;    This code makes the assumption that you want your account
;;    summary to no more than daily resolution.
;;    
;;    The Company Name field does not currently default to the name
;;    in (gnc-get-current-book).
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    See also all the "FIXME"s in the code.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2019: This report has merged in sx-summary.scm originally copied
;; from account-summary.scm. The amounts for the accounts are drawn
;; from the future Scheduled Transactions which will get realized in
;; the respective time periods.

(define-module (gnucash report standard-reports account-summary))

(use-modules (srfi srfi-1))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

;; account summary report prints a table of account information,
;; optionally with clickable links to open the corresponding register
;; window.

(define accsum-reportname (N_ "Account Summary"))
(define fsts-reportname (N_ "Future Scheduled Transactions Summary"))

(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual."))

;; fsts:
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

;; account-summary:
(define optname-date (N_ "Date"))

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts
  (N_ "Report on these accounts, if display depth allows."))
(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed."))
(define optname-bottom-behavior (N_ "Depth limit behavior"))
(define opthelp-bottom-behavior
  (N_ "How to treat accounts which exceed the specified depth limit (if any)."))

(define optname-parent-balance-mode (N_ "Parent account balances"))
(define optname-parent-total-mode (N_ "Parent account subtotals"))

(define optname-show-zb-accts (N_ "Include accounts with zero total balances"))
(define opthelp-show-zb-accts
  (N_ "Include accounts with zero total (recursive) balances in this report."))
(define optname-omit-zb-bals (N_ "Omit zero balance figures"))
(define opthelp-omit-zb-bals
  (N_ "Show blank space in place of any zero balances which would be shown."))

(define optname-use-rules (N_ "Show accounting-style rules"))
(define opthelp-use-rules
  (N_ "Use rules beneath columns of added numbers like accountants do."))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links
  (N_ "Shows each account in the table as a hyperlink to its register window."))

(define optname-show-bals (N_ "Account Balance"))
(define opthelp-show-bals (N_ "Show an account's balance."))
(define optname-show-code (N_ "Account Code"))
(define opthelp-show-code (N_ "Show an account's account code."))
(define optname-show-type (N_ "Account Type"))
(define opthelp-show-type (N_ "Show an account's account type."))
(define optname-show-desc (N_ "Account Description"))
(define opthelp-show-desc (N_ "Show an account's description."))
(define optname-show-notes (N_ "Account Notes"))
(define opthelp-show-notes (N_ "Show an account's notes."))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account."))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))

;; FIXME: add more account metadata options!

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; options generator
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accsum-options-generator sx? reportname)
  (let* ((options (gnc:new-options))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-report-title
      "a" opthelp-report-title (_ reportname)))
    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-party-name
      "b" opthelp-party-name ""))
    ;; this should default to company name in (gnc-get-current-book)
    ;; does anyone know the function to get the company name??

    ;; date at which to report balance
    (if sx?
        (gnc:options-add-date-interval!
         options gnc:pagename-general
         optname-from-date optname-to-date "c")
        (gnc:options-add-report-date!
         options gnc:pagename-general optname-date "c"))

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
               ACCT-TYPE-EQUITY ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-depth-limit
     "b" opthelp-depth-limit 3)

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior 'summarize
      (list
       (vector 'summarize
               (N_ "Recursive Balance")
               (N_ "Show the total balance, including balances in subaccounts, of any account at the depth limit."))
       (vector 'flatten
               (N_ "Raise Accounts")
               (N_ "Shows accounts deeper than the depth limit at the depth limit."))
       (vector 'truncate
               (N_ "Omit Accounts")
               (N_ "Disregard completely any accounts deeper than the depth limit.")))))

    ;; all about currencies
    (gnc:options-add-currency!
     options pagename-commodities
     optname-report-commodity "a")

    (gnc:options-add-price-source!
     options pagename-commodities
     optname-price-source "b" 'pricedb-nearest)

    (add-option
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-foreign
      "c" opthelp-show-foreign #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-rates
      "d" opthelp-show-rates #f))

    ;; what to show for zero-balance accounts
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-zb-accts
      "a" opthelp-show-zb-accts #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-omit-zb-bals
      "b" opthelp-omit-zb-bals #f))
    ;; what to show for non-leaf accounts
    (gnc:options-add-subtotal-view!
     options gnc:pagename-display
     optname-parent-balance-mode optname-parent-total-mode
     "c")

    ;; some detailed formatting options
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-links
      "e" opthelp-account-links #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-use-rules
      "f" opthelp-use-rules #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-bals
      "g" opthelp-show-bals #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-code
      "h" opthelp-show-code #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-desc
      "i" opthelp-show-desc #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-type
      "j" opthelp-show-type #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-notes
      "k" opthelp-show-notes #f))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-display)
    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accsum-renderer
;; set up the table and put it in an html document
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accsum-renderer report-obj sx? reportname)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  (let* ((report-title (get-option gnc:pagename-general optname-report-title))
         (company-name (get-option gnc:pagename-general optname-party-name))
         (from-date (and sx?
                         (gnc:time64-start-day-time
                          (gnc:date-option-absolute-time
                           (get-option gnc:pagename-general optname-from-date)))))
         (to-date (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general
                                (if sx? optname-to-date optname-date)))))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (depth-limit (get-option gnc:pagename-accounts optname-depth-limit))
         (bottom-behavior (get-option gnc:pagename-accounts optname-bottom-behavior))
         (report-commodity (get-option pagename-commodities optname-report-commodity))
         (price-source (get-option pagename-commodities optname-price-source))
         (show-fcur? (get-option pagename-commodities optname-show-foreign))
         (show-rates? (get-option pagename-commodities optname-show-rates))
         (parent-mode (get-option gnc:pagename-display optname-parent-balance-mode))
         (parent-total-mode
          (assq-ref '((t . #t) (f . #f) (canonically-tabbed . canonically-tabbed))
                    (get-option gnc:pagename-display optname-parent-total-mode)))
         (show-zb-accts? (get-option gnc:pagename-display optname-show-zb-accts))
         (omit-zb-bals? (get-option gnc:pagename-display optname-omit-zb-bals))
         (use-links? (get-option gnc:pagename-display optname-account-links))
         (use-rules? (get-option gnc:pagename-display optname-use-rules))
         (show-code? (get-option gnc:pagename-display optname-show-code))
         (show-type? (get-option gnc:pagename-display optname-show-type))
         (show-desc? (get-option gnc:pagename-display optname-show-desc))
         (show-notes? (get-option gnc:pagename-display optname-show-notes))
         (show-bals? (get-option gnc:pagename-display optname-show-bals))

         (doc (gnc:make-html-document))
         ;; just in case we need this information...
         (tree-depth (if (eq? depth-limit 'all)
                         (gnc:get-current-account-tree-depth)
                         depth-limit))
         ;; exchange rates calculation parameters
         (exchange-fn (gnc:case-exchange-fn price-source report-commodity to-date)))

    (gnc:html-document-set-title!
     doc (string-append
          company-name " " report-title " "
          (if sx?
              (format #f (_ "For Period Covering ~a to ~a")
                      (qof-print-date from-date)
                      (qof-print-date to-date))
              (qof-print-date to-date))))

    (if (null? accounts)

        ;; error condition: no accounts specified
        ;; is this *really* necessary??  i'd be fine with an all-zero
        ;; account summary that would, technically, be correct....
        (gnc:html-document-add-object!
         doc (gnc:html-make-no-account-warning reportname (gnc:report-id report-obj)))

        ;; otherwise, generate the report...
        (let* ((sx-value-hash
                (and sx? (gnc-sx-all-instantiate-cashflow-all from-date to-date)))
               (hold-table (gnc:make-html-table))  ;; temporary gnc:html-table
               (build-table (gnc:make-html-table)) ;; gnc:html-table reported
               (table-env
                (list
                 (list 'start-date from-date)
                 (list 'end-date to-date)
                 (list 'display-tree-depth tree-depth)
                 (list 'depth-limit-behavior bottom-behavior)
                 (list 'report-commodity report-commodity)
                 (list 'exchange-fn exchange-fn)
                 (list 'parent-account-subtotal-mode parent-total-mode)
                 (list 'zero-balance-mode (if show-zb-accts?
                                              'show-leaf-acct
                                              'omit-leaf-acct))
                 (list 'account-label-mode (if use-links? 'anchor 'name))
                 (list 'get-balance-fn
                       (and sx?
                            (lambda (account start-date end-date)
                              (let* ((guid (gncAccountGetGUID account))
                                     (num (hash-ref sx-value-hash guid)))
                                (if num
                                    (gnc:monetaries-add
                                     (gnc:make-gnc-monetary
                                      (xaccAccountGetCommodity account) num))
                                    (gnc:make-commodity-collector))))))))
               (params
                (list
                 (list 'parent-account-balance-mode parent-mode)
                 (list 'zero-balance-display-mode (if omit-zb-bals?
                                                      'omit-balance
                                                      'show-balance))
                 (list 'multicommodity-mode (and show-fcur? 'table))
                 (list 'rule-mode use-rules?)))

               (cur-col 0)
               (chart-table (gnc:make-html-acct-table/env/accts table-env accounts))
               (table-rows (or (gnc:html-acct-table-num-rows chart-table) 0))
               (account-cols
                (cond
                 ((zero? table-rows) 0)
                 ((assq-ref (gnc:html-acct-table-get-row-env chart-table 0)
                            'account-cols) => car)
                 (else 0)))
               (hold-table-width 0))

          (define (add-col key)
            (let rowloop ((row 0))
              (when (< row table-rows)
                (gnc:html-table-set-cell!
                 build-table (1+ row) cur-col
                 (car
                  (assq-ref (gnc:html-acct-table-get-row-env chart-table row) key)))
                (rowloop (1+ row))))
            (set! cur-col (1+ cur-col)))

          (define (make-header str)
            (gnc:make-html-table-cell/markup "number-header" str))

          (gnc:html-table-add-account-balances hold-table chart-table params)

          ;; place the column headers
          (gnc:html-table-append-row!
           build-table
           (map make-header
                (append
                 (if show-code? (list (_ "Code")) '())
                 (if show-type? (list (_ "Type")) '())
                 (if show-desc? (list (_ "Description")) '())
                 (list (_ "Account title")))))
          ;; add any fields to be displayed before the account name
          (if show-code? (add-col 'account-code))
          (if show-type? (add-col 'account-type-string))
          (if show-desc? (add-col 'account-description))

          (set! hold-table-width
            (if show-bals?
                (gnc:html-table-num-columns hold-table)
                account-cols))
          (when show-bals?
            (gnc:html-table-set-cell/tag!
             build-table 0 (+ cur-col account-cols) "number-header" (_ "Balance")))
          (let rowloop ((row 0))
            (when (< row table-rows)
              (gnc:html-table-set-row-markup!
               build-table (1+ row) (gnc:html-table-row-markup hold-table row))
              (let colloop ((col 0))
                (when (< col hold-table-width)
                  (gnc:html-table-set-cell!
                   build-table (1+ row) (+ cur-col col)
                   (gnc:html-table-get-cell hold-table row col))
                  (colloop (1+ col))))
              (rowloop (1+ row))))
          (set! cur-col (+ cur-col hold-table-width))
          (when show-notes?
            (gnc:html-table-set-cell/tag!
             build-table 0 cur-col "number-header" (_ "Notes"))
            (add-col 'account-notes))

          (gnc:html-document-add-object! doc build-table)

          ;; add currency information
          (when show-rates?
            (gnc:html-document-add-object!
             doc (gnc:html-make-exchangerates
                  report-commodity exchange-fn
                  (gnc:accounts-and-all-descendants accounts))))))

    (gnc:report-finished)
    doc))

(gnc:define-report
 'version 1
 'name accsum-reportname
 'report-guid "3298541c236b494998b236dfad6ad752"
 'options-generator (lambda () (accsum-options-generator #f accsum-reportname))
 'renderer (lambda (obj) (accsum-renderer obj #f accsum-reportname)))

(gnc:define-report
 'version 1
 'name fsts-reportname
 'report-guid "47f45d7d6d57b68518481c1fc8d4e4ba"
 'options-generator (lambda () (accsum-options-generator #t fsts-reportname))
 'renderer (lambda (obj) (accsum-renderer obj #t fsts-reportname)))

;; END
