;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-simple.scm: simplified budget report
;;
;; Kiernan Roche <self@kiernanro.ch> 8/14/2019
;;
;; based on budget.scm and budget-flow.scm by:
;; Chris Shoemaker <c.shoemaker@cox.net>
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

(define-module (gnucash report standard-reports budget-simple))

(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash engine))

(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define reportname (N_ "Budget Simple"))

;; Options definitions
(define optname-accounts (N_ "Account"))
(define optname-budget (N_ "Budget"))
(define optname-report-currency (N_ "Report currency"))
(define optname-price-source (N_ "Price source"))
(define optname-use-current-period (N_ "Use current period"))
(define opthelp-use-current-period (N_ "Whether the report should use the current period. Defaults to true."))
(define optname-specific-period (N_ "Period"))
(define opthelp-specific-period (N_ "The budget period to report on."))
(define optname-show-nb (N_ "Show non-budgeted accounts"))
(define opthelp-show-nb (N_ "Show non-budgeted accounts in the table."))
(define optname-order-by (N_ "Order By"))

;; Helper functions

(define (get-option-val options pagename optname)
    (gnc:option-value
        (gnc:lookup-option options pagename optname)))

(define (set-option-enabled options page opt-name enabled)
    (gnc-option-db-set-option-selectable-by-name
        options page opt-name enabled))

;; Determines the current budget period. Budget period is current if
;; it start time <= current time and end time >= current time
;; 
;; This function is based on a function from Chris Shoemaker's budget.scm. Thanks Chris!
;;
;; Parameters:
;;   budget - budget to use
(define (find-current-period budget)
    (let* ((now (current-time))
          (total-periods (gnc-budget-get-num-periods budget) )
          (last-period (- total-periods 1))
          (period-start (lambda (x) (gnc-budget-get-period-start-date budget x)))
          (period-end (lambda (x) (gnc-budget-get-period-end-date budget x)))
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
                          (if found-period (inexact->exact found-period) #f)
             ))
        )
    )
)

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
;; Retrieve the budgeted and actual amount
;; Display the row
;; 
;; Display the grand total for acct-table
;;
;; Return: (list budgeted-grand-total actual-grand-total)
;;
(define (gnc:html-table-add-budget-accounts!
  html-table acct-table budget period exchange-fn report-currency show-nb)
  
    (let* (
      ;; Used to sum up the budgeted and actual totals
      (bgt-total (gnc:make-commodity-collector))
      (act-total (gnc:make-commodity-collector))
      (over-bgt-total (gnc:make-commodity-collector))
      (non-bgt-total (gnc:make-commodity-collector))
    )

    ;; defining our recursive function
    (define (process-account acct)
            ;; init variables 
            (let* (
                (comm (xaccAccountGetCommodity acct))
                (bgt-numeric (gnc-budget-get-account-period-value budget acct period))
                (bgt-monetary (gnc:make-gnc-monetary comm bgt-numeric))
                (act-numeric (gnc-budget-get-account-period-actual-value budget acct period))
                (act-monetary (gnc:make-gnc-monetary comm act-numeric))
                (children (gnc-account-get-children acct))
            )
             ;; if the account has a budget
             (if (not (equal? bgt-numeric 0))(begin
                    ;; add values to accumulators
                    ;; TODO: add accumulator for overbudget amount and add the overbudget value to it, if it is above zero
                    ;; TODO: negating actual values for liability and income accounts should happen here somewhere
                    ;; TODO: show diff, and show it in red if it's overbudget (or underbudget for asset/income/liability accounts)
                    (bgt-total 'add comm bgt-numeric)
                    (act-total 'add comm act-numeric)
                    ;; display row
                    (gnc:html-table-add-budget-row! html-table "number-cell"
                      (gnc:make-html-text (gnc:html-markup-anchor (gnc:account-anchor-text acct) (gnc-account-get-full-name acct)))
                      bgt-monetary
                      act-monetary
                    ))
                    ;; if the account doesn't have a budget
                    (begin
                        ;; if actual spending is nonzero
                        (if (not (equal? act-numeric 0))
                            ;; if the account doesn't have children
                            (if (null? children)(begin
                                ;; add amount to non-budgeted total
                                (non-bgt-total 'add comm (gnc-budget-get-account-period-actual-value budget acct period))
                                ;; if we are displaying non-budgeted accounts in the table
                                (if show-nb
                                    ;; add an entry in the table for the account
                                    (gnc:html-table-add-budget-row! html-table "number-cell"
                                      (gnc:make-html-text (gnc:html-markup-anchor (gnc:account-anchor-text acct) (gnc-account-get-full-name acct)))
                                      bgt-monetary
                                      act-monetary
                                    ) #f)) #f) 
                        #f)
                        ;; if the account has children, recurse
                        (for-each (lambda (child)
                            (process-account child)
                        ) children)
                    )
            )
        )
    )
    ;; Loop though each account
    (for-each (lambda (acct)
        ;; Only process direct descendants of the root. We don't want to double-count
        (if (equal? (gnc-account-get-current-depth acct) 2)
            (process-account acct) #f)
        ) acct-table
    )

    ;; Total collectors and display
    (let* (
        (bgt-total-monetary (gnc:sum-collector-commodity bgt-total report-currency exchange-fn))
        (act-total-monetary (gnc:sum-collector-commodity act-total report-currency exchange-fn))
        (non-bgt-total-monetary (gnc:sum-collector-commodity non-bgt-total report-currency exchange-fn))
        (gnd-total-monetary (gnc:make-gnc-monetary report-currency (gnc-numeric-add (gnc:gnc-monetary-amount act-total-monetary) (gnc:gnc-monetary-amount non-bgt-total-monetary) 0 GNC-DENOM-LCD)))
      )
      (gnc:html-table-add-budget-row! html-table "total-number-cell" (string-append (_ "Budgeted subtotal") ":") bgt-total-monetary act-total-monetary)
      (if show-nb (begin
          (gnc:html-table-add-budget-row! html-table "total-number-cell" (string-append (_ "Non-budgeted subtotal") ":") (gnc:make-gnc-monetary report-currency 0.00) non-bgt-total-monetary)
          (gnc:html-table-add-budget-row! html-table "total-number-cell" (string-append (_ "Total") ":") bgt-total-monetary gnd-total-monetary))
          ;; else
          (gnc:html-table-add-budget-row! html-table "total-number-cell" (string-append (_ "Total") ":") bgt-total-monetary act-total-monetary)
      )
      
      ;; Display hr FIXME: kind of a hack
      (gnc:html-table-append-row! html-table "<tr><td colspan='3'><hr></td></tr>")
    
      ;; Return (list budgeted-total actual-total)
      (if show-nb
        (list bgt-total-monetary gnd-total-monetary)
        (list bgt-total-monetary act-total-monetary))

))) ;; end of define

;; Displays account types
;;
;; acct-table: a list from gnc:decompose-accountlist
;;
;; Return: a assoc list of (type (budgeted-grand-total actual-grand-total))
;;
(define (gnc:html-table-add-budget-types!
  html-table acct-table budget period exchange-fn report-currency show-nb)

  ;;Account totals is the assoc list that is returned  
  (let* ((accounts-totals '()))

    ;;Display each account type
    (for-each (lambda (pair)

      ;; key - type
      ;; value - list of accounts
      (let* ((key (car pair)) (value (cdr pair)))

        ;; Display and add totals
        (set! accounts-totals (assoc-set! accounts-totals key 
          (gnc:html-table-add-budget-accounts! html-table value budget period exchange-fn report-currency show-nb)
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
  html-table type-totals exchange-fn report-currency show-nb)

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
      (gnc:html-table-add-budget-row! html-table "total-number-cell" (string-append (_ "Total") ":") bgt-total-numeric act-total-numeric)

)))

;; Options generator
(define (options-generator)
    (let* ((options (gnc:new-options))
        (add-option
            (lambda (new-option) (gnc:register-option options new-option))))
        ;; budget selector
        (add-option
            (gnc:make-budget-option
                gnc:pagename-general optname-budget
                "a" (N_ "Budget to use")))

        ;; account selector
        (add-option
            (gnc:make-account-list-option
                gnc:pagename-accounts optname-accounts (string-append "a" "c")
                (N_ "Consider these accounts in the report.")
                (lambda ()
                    (gnc:filter-accountlist-type
                        (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-LIABILITY ACCT-TYPE-CREDIT ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
                        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
            #f #t)
        )

        ;; Option to select the currency the report will be shown in
        (gnc:options-add-currency!
            options gnc:pagename-general
            optname-report-currency "d")

        ;; Option to select the price source used in currency conversion
        (gnc:options-add-price-source!
            options gnc:pagename-general optname-price-source "c" 'pricedb-latest)

        
        ;; use current period checkbox
        (add-option
            (gnc:make-complex-boolean-option
            gnc:pagename-general
            optname-use-current-period "f" opthelp-use-current-period #t #f
            (lambda (value)
                (let (
                (enabler (lambda (target-opt enabled) 
                      (set-option-enabled options gnc:pagename-general target-opt enabled)))
                )
                (enabler optname-specific-period (not value))
        ))))
        
        ;; period selector
        (add-option
            (gnc:make-number-range-option
             gnc:pagename-general optname-specific-period "ee" opthelp-specific-period 1 1 60 0 1))

        ;; show non-budgeted report
        (add-option
            (gnc:make-simple-boolean-option
                gnc:pagename-general optname-show-nb "g"
                opthelp-show-nb #t))

    options
    )
)

;; Document renderer
(define (budget-renderer report-obj)
    (define (get-option pagename optname)
        (get-option-val (gnc:report-options report-obj) pagename optname))


    ;; Update progress bar
    (gnc:report-starting reportname)

    ;; Get option values, initialize variables, and set up document object
    (let* (
            ;; document
            (document (gnc:make-html-document))
            ;; options
            (budget (get-option gnc:pagename-general optname-budget))
            (budget-valid? (and budget (not (null? budget))))
            ;;(use-current-p? (get-option gnc:pagename-general optname-use-current-period))
            (accounts (get-option gnc:pagename-accounts optname-accounts))
            ;; if "use current period" is selected, find the current period and use that
            ;; otherwise, use the one from the range option
            (period (if (get-option gnc:pagename-general optname-use-current-period)
                        (find-current-period budget)
                        (- (inexact->exact (get-option gnc:pagename-general optname-specific-period)) 1) ;; periods are zero-based
                        ))
            (display period)
            ;; variables
            )
        (cond
            ((null? accounts)
             ;; no accounts selected
                (gnc:html-document-add-object! doc
                    (gnc:html-make-no-account-warning
                        reportname (gnc:report-id report-obj))))
            ;; no budget selected
            ((not budget-valid?)
                (gnc:html-document-add-object! doc
                    (gnc:html-make-generic-budget-warning reportname)))
            ;; core logic
            (else (begin
                    ;; initialize variables
                    (let* (
                        (budgeted-html-table (gnc:make-html-table))
                        (total-spend-budget 0) ;; the total budgeted spend TODO: maybe remove these since they are calculated and shown in add-budget-accounts!?
                        (total-spend-budget-actual 0) ;; the total actual spend on budgeted account
                        (total-spend-overbudget 0) ;; the total amount spent over budget
                        (total-spend-budget-diff 0) ;; the difference between budget and budget actual. to be calculated later
                        (total-spend-non-budget 0) ;; the total spend on non-budgeted accounts
                        (decomposed-accounts (gnc:decompose-accountlist accounts))
                        (accounts-totals '())

                        (report-currency (get-option gnc:pagename-general
                            optname-report-currency))
                        (price-source (get-option gnc:pagename-general
                            optname-price-source))

                        (exchange-fn (gnc:case-exchange-fn
                            price-source report-currency #f))
                    )
                     ;; report title
                    (gnc:html-document-set-title!
                     document (format #f (_ "~a: ~a - period beginning ~a") 
                         (get-option gnc:pagename-general gnc:optname-reportname) 
                         (gnc-budget-get-name budget)
                         (qof-print-date (gnc-budget-get-period-start-date budget period))
                    ))

                    ;; calculating budgeted spend
                    (set! accounts-totals (gnc:html-table-add-budget-types! budgeted-html-table decomposed-accounts budget period exchange-fn report-currency (get-option gnc:pagename-general optname-show-nb)))
                    (gnc:html-table-add-budget-totals! budgeted-html-table accounts-totals exchange-fn report-currency (get-option gnc:pagename-general optname-show-nb))

                    ;; calculating non-budgeted spend
                    (gnc:html-document-add-object! document budgeted-html-table))
                )
            )
        )
    ;; Update progress bar
    (gnc:report-finished)
    document)
)

(gnc:define-report
 'version 1.0
 'name reportname
 'report-guid "ff6c9de4738846018bb5f01975e8d8df"
 'menu-path (list gnc:menuname-budget)
 'options-generator options-generator
 'renderer budget-renderer)
