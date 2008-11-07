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

(define-module (gnucash report budget-flow))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define reportname (N_ "Budget Flow"))

;; define all option's names so that they are properly defined
;; in *one* place.

(define optname-accounts (N_ "Account"))
(define optname-price-source (N_ "Price Source"))
(define optname-budget (N_ "Budget"))
(define optname-periods (N_ "Period"))
(define optname-report-currency (N_ "Report's currency"))

;; options generator
(define (budget-report-options-generator)
  (let ((options (gnc:new-options)))

    ;; Option to select Budget
    (gnc:register-option
      options
      (gnc:make-budget-option
        gnc:pagename-general optname-budget
        "a" (N_ "Budget")))

    ;; Option to select Period of selected Budget
    (gnc:register-option
      options
      (gnc:make-number-range-option
        gnc:pagename-general optname-periods
        "b" (N_ "Period") 1 1 12 0 1))

    ;; Option to select the currency the report will be shown in
    (gnc:options-add-currency!
      options gnc:pagename-general
      optname-report-currency "d")

    ;; Option to select the price source used in currency conversion
    (gnc:options-add-price-source!
      options gnc:pagename-general optname-price-source "c" 'pricedb-latest)

    ;;Option to select the accounts to that will be displayed
    (gnc:register-option 
      options  
      (gnc:make-account-list-option
        gnc:pagename-accounts optname-accounts
        (string-append "a" "c")
        (N_ "Report on these accounts")
        (lambda ()
          (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
        #f #t))
    
    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options
))


;; Append a row to html-table with markup and values
(define (gnc:html-table-add-budget-row! 
  html-table markup text total1 total2)

  ;; Cell order is text, budgeted, actual
  (gnc:html-table-append-row/markup! html-table "normal-row"
    (list
      (gnc:make-html-table-cell/markup "text-cell" text)
      (gnc:make-html-table-cell/markup markup total1)
      (gnc:make-html-table-cell/markup markup total2)

)))

;; For each account in acct-table:
;; Retrive the budgeted and actual amount
;; Display the row
;; 
;; Display the grand total for acct-table
;;
;; Return: (list budgeted-grand-total actual-grand-total)
;;
(define (gnc:html-table-add-budget-accounts!
  html-table acct-table budget period exchange-fn report-currency)

  (let* (
      ;; Used to sum up the budgeted and actual totals
      (bgt-total (gnc:make-commodity-collector))
      (act-total (gnc:make-commodity-collector))
    )

    ;; Loop though each account
    ;;
    ;; FIXME: because gnc:budget-get-account-period-actual-value
    ;; sums the total for a parent and all child accounts displaying 
    ;; and summing a parent account cause the totals to be off.
    ;; so we do not display parent accounts
    ;;
    (for-each (lambda (acct)

        ;; If acct has children do nto display (see above)
        (if (null? (gnc-account-get-children acct))
          (let* (
              ;; Retrive the budgeted and actual amount and convert to <gnc:monetary>
              (comm (xaccAccountGetCommodity acct))
              (bgt-numeric (gnc-budget-get-account-period-value budget acct period))
              (bgt-monetary (gnc:make-gnc-monetary comm bgt-numeric))
              (act-numeric (gnc-budget-get-account-period-actual-value budget acct period))
              (act-monetary (gnc:make-gnc-monetary comm act-numeric))
            )
            
            ;; Add amounts to collectors
            (bgt-total 'add comm bgt-numeric)
            (act-total 'add comm act-numeric)

            ;; Display row
            (gnc:html-table-add-budget-row! html-table "number-cell"
              (gnc:make-html-text (gnc:html-markup-anchor (gnc:account-anchor-text acct) (gnc-account-get-full-name acct)))
              bgt-monetary
              act-monetary
      ))))

      acct-table
    )

    ;; Total collectors and display
    (let* (
        (bgt-total-numeric (gnc:sum-collector-commodity bgt-total report-currency exchange-fn))
        (act-total-numeric (gnc:sum-collector-commodity act-total report-currency exchange-fn))
      )
      (gnc:html-table-add-budget-row! html-table "total-number-cell" "Total:" bgt-total-numeric act-total-numeric)
      
      ;; Display hr FIXME: kind of a hack
      (gnc:html-table-append-row! html-table "<tr><td colspan='3'><hr></td></tr>")
    
      ;; Return (list budgeted-total actual-total)
      (list bgt-total-numeric act-total-numeric)

))) ;; end of define

;; Displays account types
;;
;; acct-table: a list from gnc:decompose-accountlist
;;
;; Return: a assoc list of (type (budgeted-grand-total actual-grand-total))
;;
(define (gnc:html-table-add-budget-types!
  html-table acct-table budget period exchange-fn report-currency)

  ;;Account totals is the assoc list that is returned  
  (let* ((accounts-totals '()))

    ;;Display each account type
    (for-each (lambda (pair)

      ;; key - type
      ;; value - list of accounts
      (let* ((key (car pair)) (value (cdr pair)))

        ;; Display and add totals
        (set! accounts-totals (assoc-set! accounts-totals key 
          (gnc:html-table-add-budget-accounts! html-table value budget period exchange-fn report-currency)
        ))
      ))

      acct-table
    )

    ;; Reutrn assoc list
    accounts-totals
))

;; Displays type-totals
;;
;; type-totals: a list of (type (budget-total actual-total))
;;
(define (gnc:html-table-add-budget-totals!
  html-table type-totals exchange-fn report-currency)

  (let* (
      ;; Collector of grand totals
      (bgt-total-collector (gnc:make-commodity-collector))
      (act-total-collector (gnc:make-commodity-collector))
    )
    
    ;; Loop though each pair
    (for-each (lambda (pair)
        (let* (
            ;; tuple is (type (budgeted actual))
            (key (car pair))
            (value (cdr pair))
            (bgt-total (car value))
            (act-total (cadr value))
          )

          ;; Add to collectors 
          (bgt-total-collector 'add (gnc:gnc-monetary-commodity bgt-total) (gnc:gnc-monetary-amount bgt-total))
          (act-total-collector 'add (gnc:gnc-monetary-commodity act-total) (gnc:gnc-monetary-amount act-total))

          ;; Display row
          (gnc:html-table-add-budget-row! html-table "number-cell" (gnc:account-get-type-string-plural key) bgt-total act-total)
      ))

      type-totals
    )
    (let* (
        ;; Sum collectors    
        (bgt-total-numeric (gnc:sum-collector-commodity bgt-total-collector report-currency exchange-fn))
        (act-total-numeric (gnc:sum-collector-commodity act-total-collector report-currency exchange-fn))
      )

      ;; Display Grand Total
      (gnc:html-table-add-budget-row! html-table "total-number-cell" "Total:" bgt-total-numeric act-total-numeric)

)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-renderer
;; set up the document and add the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-renderer report-obj)

  ;; Helper function retrives options
  (define (get-option pagename optname)
    (gnc:option-value
      (gnc:lookup-option
        (gnc:report-options report-obj) pagename optname)))

  ;; Update progress bar
  (gnc:report-starting reportname)

  ;; get all option's values
  (let* (
      (budget (get-option gnc:pagename-general optname-budget))
      (accounts (get-option gnc:pagename-accounts optname-accounts))
      (period (inexact->exact (get-option gnc:pagename-general
        optname-periods)))
      (report-currency (get-option gnc:pagename-general
        optname-report-currency))
      (price-source (get-option gnc:pagename-general
        optname-price-source))

      ;; calculate the exchange rates
      (exchange-fn (gnc:case-exchange-fn
        price-source report-currency #f))

      ;; The HTML document
      (doc (gnc:make-html-document))
    )

    ;; If no account are select show a warring page    
    (if (not (or (null? accounts) (null? budget) (not budget)))
      (let* (
          (html-table (gnc:make-html-table))
          (report-name (get-option gnc:pagename-general
            gnc:optname-reportname))

          ;; decompose the account list
          (split-up-accounts (gnc:decompose-accountlist accounts))
          (accounts-totals '())
     
        )

        ;; Display Title Name - Budget - Period
        (gnc:html-document-set-title!
          doc (sprintf #f (_ "%s: %s - %s")
            report-name (gnc-budget-get-name budget)
            (gnc-print-date (gnc-budget-get-period-start-date budget period))))

        ;; Display accounts and totals
        (set! accounts-totals (gnc:html-table-add-budget-types! html-table split-up-accounts budget period exchange-fn report-currency))
        (gnc:html-table-add-budget-totals! html-table accounts-totals exchange-fn report-currency)

        ;; Display table
        (gnc:html-document-add-object! doc html-table)
      )

      ;; error condition: either no accounts or no budgets specified
      (gnc:html-document-add-object!
        doc (gnc:html-make-generic-options-warning
	  reportname (gnc:report-id report-obj)))
    )

    ;; Update progress bar
    (gnc:report-finished)
    doc))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "e6e34fa3b6e748debde3cb3bc76d3e53"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator budget-report-options-generator
 'renderer budget-renderer)

