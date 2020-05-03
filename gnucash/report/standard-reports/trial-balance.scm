;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trial-balance.scm: trial balance and work sheet
;; By David Montenegro <sunrise2000@comcast.net>
;; 
;; Prepares a trial balance of your books.
;; Optionally prepares a complete work sheet.
;; 
;; N.B.: Since GnuCash ensures that all your debits and credits
;; balance, preparing a Trial Balance isn't technically necessary for
;; GnuCash users.  This report is included primarily for pedagogical
;; and corroborative purposes.
;; 
;; BUGS:
;; 
;;    This code makes the assumption that you want your trial
;;    balance to no more than daily resolution.
;;    
;;    The Company Name field does not currently default to the name
;;    in (gnc-get-current-book).
;;    
;;    Progress bar functionality is currently mostly broken.
;;    
;;    Unsure if the multi-currency support is correct.
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    See also any "FIXME"s in the code.
;; 
;; Largely borrowed from balance-sheet.scm By Robert Merkel <rgmerk@mira.net>
;;
;; Largely borrowed from pnl.scm by:
;; Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report standard-reports trial-balance))
(use-modules (srfi srfi-1))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Trial Balance"))

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual."))

(define optname-start-date (N_ "Start of Adjusting/Closing"))
(define optname-end-date (N_ "Date of Report"))
(define optname-report-variant (N_ "Report variation"))
(define opthelp-report-variant (N_ "Kind of trial balance to generate."))

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts
  (N_ "Report on these accounts."))
(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed."))

(define pagename-merchandising (N_ "Merchandising"))
(define optname-gross-adjustment-accounts (N_ "Gross adjustment accounts."))
(define opthelp-gross-adjustment-accounts
  (N_ "Do not net, but show gross debit/credit adjustments to these accounts. Merchandising businesses will normally select their inventory accounts here."))
(define optname-income-summary-accounts (N_ "Income summary accounts"))
(define opthelp-income-summary-accounts
  (N_ "Adjustments made to these accounts are gross adjusted (see above) in the Adjustments, Adjusted Trial Balance, and Income Statement columns. Mostly useful for merchandising businesses."))

(define pagename-entries (N_ "Entries"))
(define optname-adjusting-pattern (N_ "Adjusting Entries pattern"))
(define opthelp-adjusting-pattern
  (N_ "Any text in the Description column which identifies adjusting entries."))
(define optname-adjusting-casing
  (N_ "Adjusting Entries pattern is case-sensitive"))
(define opthelp-adjusting-casing
  (N_ "Causes the Adjusting Entries Pattern match to be case-sensitive."))
(define optname-adjusting-regexp
  (N_ "Adjusting Entries Pattern is regular expression"))
(define opthelp-adjusting-regexp
  (N_ "Causes the Adjusting Entries Pattern to be treated as a regular expression."))

(define optname-closing-pattern (N_ "Closing Entries pattern"))
(define opthelp-closing-pattern
  (N_ "Any text in the Description column which identifies closing entries."))
(define optname-closing-casing
  (N_ "Closing Entries pattern is case-sensitive"))
(define opthelp-closing-casing
  (N_ "Causes the Closing Entries Pattern match to be case-sensitive."))
(define optname-closing-regexp
  (N_ "Closing Entries Pattern is regular expression"))
(define opthelp-closing-regexp
  (N_ "Causes the Closing Entries Pattern to be treated as a regular expression."))

;; FIXME: this option doesn't produce a correct work sheet when
;; selected after closing... it omits adjusted temporary accounts
;;
;; the fix for this really should involve passing thunks to
;; gnc:make-html-acct-table
(define optname-show-zb-accts (N_ "Include accounts with zero total balances"))
(define opthelp-show-zb-accts
  (N_ "Include accounts with zero total (recursive) balances in this report."))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links (N_ "Shows each account in the table as a hyperlink to its register window."))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account."))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))


;; utility to assist with double-column balance tables
;; a request is made with the <req> argument
;; <req> may currently be 'entry|'debit-q|'credit-q|'zero-q|'debit|'credit
;; 'debit-q|'credit-q|'zero-q tests the sign of the balance
;; 'side returns 'debit or 'credit, the column in which to display
;; 'debt|'credit return the entry, if appropriate, or #f
(define (double-col
         req signed-balance report-commodity exchange-fn show-comm?)
  (let* ((sum (and signed-balance
                   (gnc:sum-collector-commodity
                    signed-balance
                    report-commodity
                    exchange-fn)))
         (amt (and sum (gnc:gnc-monetary-amount sum)))
         (neg? (and amt (negative? amt)))
         (bal (if neg?
                  (gnc:commodity-collector-get-negated signed-balance)
                  signed-balance))
         (bal-sum (gnc:sum-collector-commodity
                   bal
                   report-commodity
                   exchange-fn))
         (balance
          (if (gnc:uniform-commodity? bal report-commodity)
              (if (zero? amt) #f bal-sum)
              (if show-comm?
                  (gnc-commodity-table bal report-commodity exchange-fn)
                  bal-sum))))
    (assq-ref (list
               (cons 'entry balance)
               (cons 'debit (if neg? #f balance))
               (cons 'credit (if neg? balance #f))
               (cons 'zero-q (if neg? #f (if balance #f #t)))
               (cons 'debit-q (if neg? #f (if balance #t #f)))
               (cons 'credit-q (if neg? #t #f)))
              req)))

;; options generator
(define (trial-balance-options-generator)
  (let* ((options (gnc:new-options))
         (book (gnc-get-current-book))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-string-option
      (N_ "General") optname-report-title
      "a" opthelp-report-title (_ reportname)))
    (add-option
     (gnc:make-string-option
      (N_ "General") optname-party-name
      "b" opthelp-party-name (or (gnc:company-info book gnc:*company-name*) "")))

    ;; the period over which to collect adjusting/closing entries and
    ;; date at which to report the balance
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-start-date optname-end-date "c")

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-report-variant
      "d" opthelp-report-variant
      'current
      (list (vector 'current
                    (N_ "Current Trial Balance")
                    (N_ "Uses the exact balances in the general journal"))
            (vector 'pre-adj
                    (N_ "Pre-adjustment Trial Balance")
                    (N_ "Ignores Adjusting/Closing entries"))
            (vector 'work-sheet
                    (N_ "Work Sheet")
                    (N_ "Creates a complete end-of-period work sheet")))))

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
     "b" opthelp-depth-limit 1)

    ;; options for merchandising business work sheets
    (add-option
     (gnc:make-account-list-option
      pagename-merchandising optname-gross-adjustment-accounts
      "c"
      opthelp-gross-adjustment-accounts
      (lambda ()
        ;; Here, it would be useful to have an inventory account type.
        ;; Lacking that, just select no accounts by default.
        '())
      #f #t))
    (add-option
     (gnc:make-account-list-option
      pagename-merchandising optname-income-summary-accounts
      "d"
      opthelp-income-summary-accounts
      (lambda ()
        '())
      #f #t))

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
      "c" opthelp-show-foreign #f))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-rates
      "d" opthelp-show-rates #f))

    ;; adjusting/closing entry match criteria
    ;;
    ;; N.B.: transactions really should have a field where we can put
    ;; transaction types like "Adjusting/Closing/Correcting Entries"
    (add-option
     (gnc:make-string-option
      pagename-entries optname-adjusting-pattern
      "a" opthelp-adjusting-pattern (_ "Adjusting Entries")))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-adjusting-casing
      "b" opthelp-adjusting-casing #f))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-adjusting-regexp
      "c" opthelp-adjusting-regexp #f))
    (add-option
     (gnc:make-string-option
      pagename-entries optname-closing-pattern
      "d" opthelp-closing-pattern (_ "Closing Entries")))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-closing-casing
      "e" opthelp-closing-casing #f))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-closing-regexp
      "f" opthelp-closing-regexp #f))

    ;; what to show for zero-balance accounts
    ;;(add-option
    ;; (gnc:make-simple-boolean-option
    ;;  gnc:pagename-display optname-show-zb-accts
    ;;  "a" opthelp-show-zb-accts #t))

    ;; some detailed formatting options
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-links
      "e" opthelp-account-links #t))

    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-display)

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trial-balance-renderer
;; set up the document and add the table
;; then then return the document or, if
;; requested, export it to a file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (trial-balance-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((report-title (get-option gnc:pagename-general optname-report-title))
         (company-name (get-option gnc:pagename-general optname-party-name))
         (start-date-printable
          (gnc:date-option-absolute-time
           (get-option gnc:pagename-general optname-start-date)))
         (start-date (gnc:time64-end-day-time
                      (gnc:time64-previous-day start-date-printable)))
         (end-date (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general optname-end-date))))
         (report-variant (get-option gnc:pagename-general optname-report-variant))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (ga-accounts (get-option pagename-merchandising
                                  optname-gross-adjustment-accounts))
         (is-accounts (get-option pagename-merchandising
                                  optname-income-summary-accounts))
         (depth-limit (get-option gnc:pagename-accounts optname-depth-limit))
         (adjusting-str (get-option pagename-entries optname-adjusting-pattern))
         (adjusting-cased (get-option pagename-entries optname-adjusting-casing))
         (adjusting-regexp (get-option pagename-entries optname-adjusting-regexp))
         (closing-str (get-option pagename-entries optname-closing-pattern))
         (closing-cased (get-option pagename-entries optname-closing-casing))
         (closing-regexp (get-option pagename-entries optname-closing-regexp))
         (report-commodity (get-option pagename-commodities optname-report-commodity))
         (price-source (get-option pagename-commodities optname-price-source))
         (show-fcur? (get-option pagename-commodities optname-show-foreign))
         (show-rates? (get-option pagename-commodities optname-show-rates))
         (show-zb-accts? #t)
         (use-links? (get-option gnc:pagename-display optname-account-links))

         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (asset-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-ASSET))
         (liability-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY))
         (income-expense-accounts
          (append-reverse (assoc-ref split-up-accounts ACCT-TYPE-INCOME)
                          (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)))
         (equity-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))

         ;; (all-accounts (map (lambda (X) (cadr X)) split-up-accounts))
         ;; ^ will not do what we want
         (all-accounts
          (append asset-accounts liability-accounts
                  equity-accounts income-expense-accounts))

         ;; same for gross adjustment accounts...
         (split-up-ga-accounts (gnc:decompose-accountlist ga-accounts))
         (all-ga-accounts
          (append (assoc-ref split-up-ga-accounts ACCT-TYPE-ASSET)
                  (assoc-ref split-up-ga-accounts ACCT-TYPE-LIABILITY)
                  (assoc-ref split-up-ga-accounts ACCT-TYPE-EQUITY)
                  (assoc-ref split-up-ga-accounts ACCT-TYPE-INCOME)
                  (assoc-ref split-up-ga-accounts ACCT-TYPE-EXPENSE)))
         (split-up-is-accounts (gnc:decompose-accountlist is-accounts))

         ;; same for income statement accounts...
         (all-is-accounts
          (append (assoc-ref split-up-is-accounts ACCT-TYPE-ASSET)
                  (assoc-ref split-up-is-accounts ACCT-TYPE-LIABILITY)
                  (assoc-ref split-up-is-accounts ACCT-TYPE-EQUITY)
                  (assoc-ref split-up-is-accounts ACCT-TYPE-INCOME)
                  (assoc-ref split-up-is-accounts ACCT-TYPE-EXPENSE)))

         (doc (gnc:make-html-document))
         ;; exchange rates calculation parameters
         (exchange-fn
          (gnc:case-exchange-fn price-source report-commodity end-date))
         (period-for (string-append " " (_ "for Period"))))

    (gnc:html-document-set-title!
     doc (if (eq? report-variant 'current)
             (format #f "~a ~a ~a"
                     company-name report-title
                     (qof-print-date end-date))
             (format #f (string-append "~a ~a "
                                       (_ "For Period Covering ~a to ~a"))
                     company-name report-title
                     (qof-print-date start-date-printable)
                     (qof-print-date end-date))))

    (if (null? accounts)

        ;; error condition: no accounts specified
        ;; is this *really* necessary??
        ;; i'd be fine with an all-zero trial balance
        ;; that would, technically, be correct....
        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          reportname (gnc:report-id report-obj)))

        ;; Get all the balances for each account group.
        (let* ((build-table (gnc:make-html-table))
               (acct-table #f)
               (debit-tot (gnc:make-commodity-collector))
               (credit-tot (gnc:make-commodity-collector))
               (neg-unrealized-gain-collector (gnc:make-commodity-collector))
               (table-env #f)    ;; parameters for :make-
               (account-cols #f)
               (indented-depth #f)
               (header-rows 0)
               (adj-debits (gnc:make-commodity-collector))
               (adj-credits (gnc:make-commodity-collector))
               (atb-debits (gnc:make-commodity-collector))
               (atb-credits (gnc:make-commodity-collector))
               (is-debits (gnc:make-commodity-collector))
               (is-credits (gnc:make-commodity-collector))
               (bs-debits (gnc:make-commodity-collector))
               (bs-credits (gnc:make-commodity-collector)))

          ;; Wrapper to call gnc:html-table-add-labeled-amount-line!
          ;; with the proper arguments.
          ;; (This is used to fill in the Trial Balance columns.)
          (define (add-line table label signed-balance)
            (let* ((entry (double-col
                           'entry signed-balance
                           report-commodity exchange-fn show-fcur?))
                   (credit? (double-col
                             'credit-q signed-balance
                             report-commodity exchange-fn show-fcur?)))
              (gnc:html-table-add-labeled-amount-line!
               table (+ account-cols 2) "primary-subheading"
               #f label indented-depth 1 "text-cell"
               entry (+ account-cols (if credit? 1 0)) 1 "number-cell")
              ;; update the running totals
              (if credit?
                  (credit-tot 'minusmerge signed-balance #f)
                  (debit-tot 'merge signed-balance #f))))

          (define (get-val alist key)
            (let ((lst (assq-ref alist key)))
              (and lst (car lst))))

          (define pa-col  0) ;; pre-adjustments column
          (define adj-col 1) ;; adjustments column
          (define atb-col 2) ;; adjusted trial balance column
          (define is-col  3) ;; income statement column
          (define bs-col  4) ;; balance sheet column
          (define bal-col 5) ;; for the current (general journal) balance

          (define (report-val amt)
            (gnc:sum-collector-commodity
             amt report-commodity exchange-fn))

          ;; Returns a gnc:html-table-cell containing the absolute value
          ;; of the given amount in the report commodity.
          (define (tot-abs-amt-cell amt)
            (let* ((neg-amt (gnc:make-commodity-collector))
                   (rv (report-val amt)))
              (neg-amt 'minusmerge amt #f)
              (let ((cell (gnc:make-html-table-cell/markup
                           "total-number-cell"
                           (if (negative? (gnc:gnc-monetary-amount rv))
                               (report-val neg-amt)
                               rv))))
                (gnc:html-table-cell-set-style!
                 cell "total-number-cell"
                 'attribute '("align" "right")
                 'attribute '("valign" "top"))
                cell)))

          ;; sum any unrealized gains
          ;;
          ;; Hm... unrealized gains....  This is when you purchase
          ;; something and its value increases/decreases (prior to
          ;; your selling it) and you have to reflect that on your
          ;; balance sheet.
          ;;
          ;; I *think* a decrease in the value of a liability or
          ;; equity constitutes an unrealized loss.  I'm unsure about
          ;; that though....
          ;;
          ;; This procedure returns a commodity collector.
          (define (collect-unrealized-gains)
            (define (acct->bal acct)
              (gnc:account-get-comm-balance-at-date acct end-date #f))
            (if (eq? price-source 'average-cost)
                ;; No need to calculate if doing valuation at cost.
                (gnc:make-commodity-collector)
                (let* ((cost-fn (gnc:case-exchange-fn
                                 'average-cost report-commodity end-date))
                       (acct-balances (map acct->bal all-accounts))
                       (book-balance (apply gnc:collector+ acct-balances))
                       (value (gnc:sum-collector-commodity
                               book-balance report-commodity exchange-fn))
                       (cost (gnc:sum-collector-commodity
                              book-balance report-commodity cost-fn)))
                  ;; Get the unrealized gain or loss (value minus cost).
                  (gnc:monetaries-add value (gnc:monetary-neg cost)))))

          ;; set default cell alignment
          (gnc:html-table-set-style!
           build-table "td"
           'attribute '("align" "right")
           'attribute '("valign" "top"))

          (gnc:report-percent-done 4)

          ;; Get any unrealized gains/losses.
          (neg-unrealized-gain-collector
           'minusmerge (collect-unrealized-gains) #f)

          (set! table-env
            (list
             (list 'start-date #f)
             (list 'end-date end-date)
             (list 'display-tree-depth
                   (and (integer? depth-limit) depth-limit))
             (list 'depth-limit-behavior 'flatten)
             (list 'report-commodity report-commodity)
             (list 'exchange-fn exchange-fn)
             (list 'parent-account-subtotal-mode #f)
             (list 'zero-balance-mode (if show-zb-accts? 'show-leaf-acct
                                          'omit-leaf-acct))
             (list 'account-label-mode (if use-links? 'anchor 'name))))

          (set! acct-table
            (gnc:make-html-acct-table/env/accts table-env all-accounts))

          (gnc:report-percent-done 80)
          (let* ((env (gnc:html-acct-table-get-row-env acct-table 0)))
            (set! account-cols (get-val env 'account-cols)))

          (let* ((ncols (+ account-cols (if (eq? report-variant 'work-sheet) 10 2)))
                 (space (make-list ncols (gnc:make-html-table-cell/min-width 60))))
            (gnc:html-table-append-row! build-table space)
            (set! header-rows (1+ header-rows)))

          ;; add the double-column headers if required
          (if (eq? report-variant 'work-sheet)
              (let* ((headings
                      (list
                       (_ "Trial Balance")
                       (_ "Adjustments")
                       (_ "Adjusted Trial Balance")
                       (_ "Income Statement")
                       (_ "Balance Sheet")))
                     (parent-headings #f))
                (set! parent-headings
                  (apply append
                         (map
                          (lambda (heading)
                            (list
                             (gnc:make-html-table-cell/size/markup 1 1 "th" heading)
                             (gnc:html-make-empty-cell)))
                          headings)))
                (gnc:html-table-append-row!
                 build-table
                 (append
                  (gnc:html-make-empty-cells account-cols)
                  parent-headings))
                (set! header-rows (+ header-rows 1))))
          ;; add the DEBIT/CREDIT headers
          (let* ((debit-cell
                  (gnc:make-html-table-cell/markup
                   "th" (_ "Debit")))
                 (credit-cell
                  (gnc:make-html-table-cell/markup
                   "th" (_ "Credit")))
                 (row (append
                       (list (gnc:make-html-table-cell/markup
                              "th" (_ "Account Name")))
                       (gnc:html-make-empty-cells (- account-cols 1))
                       (list debit-cell)
                       (list credit-cell)))
                 (ws-col 0))
            (if (eq? report-variant 'work-sheet)
                (let ((rownum 0)
                      (ws-cols 4))
                  (while (< rownum ws-cols)
                    (set! row (append row (list debit-cell credit-cell)))
                    (set! rownum (+ rownum 1)))))
            (gnc:html-table-append-row! build-table row)
            (set! header-rows (+ header-rows 1)))

          ;; now, for each account, calculate all the column values
          ;; and store them in the utility object...
          ;;
          ;; this handles merchandising (inventory and income summary)
          ;; accounts specially. instead of storing a commodity collector,
          ;; it stores a two-element list of commodity collectors:
          ;;  (list debit-collector credit-collector)
          (let* ((row 0)
                 (rows (gnc:html-acct-table-num-rows acct-table))
                 (closing-splits
                  (gnc:account-get-trans-type-splits-interval
                   all-accounts (list (list 'str closing-str)
                                      (list 'cased closing-cased)
                                      (list 'regexp closing-regexp)
                                      (list 'closing #t))
                   start-date end-date))
                 (adjusting-splits
                  (gnc:account-get-trans-type-splits-interval
                   all-accounts (list (list 'str adjusting-str)
                                      (list 'cased adjusting-cased)
                                      (list 'regexp adjusting-regexp))
                   start-date end-date)))

            (define (sum-account-splits account splits pos-filter?)
              (let ((total (gnc:make-commodity-collector))
                    (comm (xaccAccountGetCommodity account)))
                (for-each
                 (lambda (s)
                   (let ((amt (xaccSplitGetAmount s)))
                     (when (and (equal? (xaccSplitGetAccount s) account)
                                (or (not pos-filter?) (positive? amt)))
                       (total 'add comm amt))))
                 splits)
                total))

            (while (< row rows)
              (let* ((env (gnc:html-acct-table-get-row-env acct-table row))
                     (acct (get-val env 'account))
                     (curr-bal (get-val env 'account-bal))
                     (closing (sum-account-splits acct closing-splits #f))
                     (adjusting (sum-account-splits acct adjusting-splits #f))
                     (is? (member acct all-is-accounts))
                     (ga-or-is? (or (member acct all-ga-accounts) is?))
                     (pos-adjusting
                      (and ga-or-is? (sum-account-splits acct adjusting-splits #t)))
                     (neg-adjusting
                      (and ga-or-is? (gnc:collector- adjusting pos-adjusting)))
                     (pre-closing-bal (gnc:collector- curr-bal closing))
                     (pre-adjusting-bal (gnc:collector- pre-closing-bal
                                                        adjusting))
                     (atb (cond ((not is?) pre-closing-bal)
                                ((double-col 'credit-q pre-adjusting-bal
                                             report-commodity exchange-fn show-fcur?)
                                 (list (gnc:collector+ pos-adjusting)
                                       (gnc:collector+ neg-adjusting
                                                       pre-adjusting-bal)))
                                (else
                                 (list (gnc:collector+ pos-adjusting pre-adjusting-bal)
                                       (gnc:collector+ neg-adjusting))))))

                ;; curr-bal = account-bal with closing & adj entries
                ;; pre-closing-bal = account-bal with adj entries only
                ;; pre-adjusting-bal = account-bal without both

                ;; pos- and neg-adjusting are adjusting's +ve and -ve amounts

                ;; atb is account-bal with adjusting entries separated
                ;; into +ve and -ve columns, or pre-closing-bal. no
                ;; explanation was offered in bug #150008.

                (gnc:html-acct-table-set-cell!
                 acct-table row pa-col pre-adjusting-bal)
                (gnc:html-acct-table-set-cell!
                 acct-table row adj-col
                 (if ga-or-is?
                     (list pos-adjusting neg-adjusting)
                     adjusting))
                (gnc:html-acct-table-set-cell! acct-table row atb-col atb)
                (gnc:html-acct-table-set-cell!
                 acct-table row
                 (if (or (gnc:account-is-inc-exp? acct) is?)
                     is-col bs-col)
                 atb)
                (gnc:html-acct-table-set-cell! acct-table row bal-col curr-bal)

                (set! row (+ row 1)))))

          ;; next, set up the account tree and pre-adjustment balances
          ;; (This fills in the Account Title and Trial Balance columns.)
          (let ((row 0)
                (rows (gnc:html-acct-table-num-rows acct-table)))
            (while (< row rows)
              (let* ((env (gnc:html-acct-table-get-row-env acct-table row))
                     (account-bal
                      (gnc:html-acct-table-get-cell
                       acct-table row
                       (assq-ref (list (cons 'pre-adj pa-col)
                                       (cons 'work-sheet pa-col)
                                       (cons 'current bal-col))
                                report-variant)))
                     (label (get-val env 'account-label)))
                ;; yeah, i know, global vars are devil... so deal with it
                (set! indented-depth (get-val env 'indented-depth))
                (add-line build-table label account-bal))
              (set! row (+ row 1))))

          ;; handle any unrealized gains
          ;;
          ;; we omit unrealized gains from the balance report, if
          ;; zero, since they are not present on normal trial balances
          (and (not (gnc-commodity-collector-allzero?
                     neg-unrealized-gain-collector))
               (let* ((ug-row (+ header-rows
                                 (gnc:html-acct-table-num-rows acct-table)))
                      (credit? (double-col
                                'credit-q neg-unrealized-gain-collector
                                report-commodity exchange-fn show-fcur?))
                      (entry (double-col
                              'entry neg-unrealized-gain-collector
                              report-commodity exchange-fn show-fcur?)))
                 (add-line build-table
                           (if credit?
                               (_ "Unrealized Gains")
                               (_ "Unrealized Losses"))
                           neg-unrealized-gain-collector)
                 (when (eq? report-variant 'work-sheet)
                   ;; make table line wide enough
                   (gnc:html-table-set-cell!
                    build-table ug-row (+ account-cols (* 2 bs-col) 1) #f)
                   (gnc:html-table-set-cell!
                    build-table ug-row
                    (+ account-cols (* 2 atb-col) (if credit? 1 0)) entry)
                   (gnc:html-table-set-cell!
                    build-table ug-row
                    (+ account-cols (* 2 bs-col) (if credit? 1 0)) entry)
                   (if credit?
                       (and (atb-credits 'minusmerge neg-unrealized-gain-collector #f)
                            (bs-credits 'minusmerge neg-unrealized-gain-collector #f))
                       (and (atb-debits 'merge neg-unrealized-gain-collector #f)
                            (bs-debits 'merge neg-unrealized-gain-collector #f))))))

          ;;
          ;; now, if requested, complete the worksheet
          ;;
          ;; to complete the worksheet, we mostly just have to dink
          ;; around, reading acct-table, putting values in the right
          ;; build-table cells... which is comparatively easy.
          ;;
          (if (eq? report-variant 'work-sheet)
              (let ((row 0)
                    (rows (gnc:html-acct-table-num-rows acct-table))
                    (last-col #f)
                    (html-row #f))
                (while (< row rows)
                  (for-each
                   (lambda (colpair debit-coll credit-coll)
                     (set! html-row (+ row header-rows))
                     (let* ((bal (gnc:html-acct-table-get-cell acct-table row colpair))
                            (gross-bal? (list? bal))
                            (entry (and bal
                                        (not gross-bal?)
                                        (double-col
                                         'entry bal report-commodity
                                         exchange-fn show-fcur?)))
                            (credit? (and bal
                                          (or gross-bal?
                                              (double-col
                                               'credit-q bal report-commodity
                                               exchange-fn show-fcur?))))
                            (non-credit? (and bal (or gross-bal? (not credit?))))
                            (debit (or (and gross-bal? (car bal))
                                       (and non-credit? bal)))
                            (credit (or (and gross-bal? (cadr bal))
                                        (and credit? bal)))
                            (debit-entry
                             (and gross-bal?
                                  (double-col 'entry debit report-commodity
                                              exchange-fn show-fcur?)))
                            (credit-entry
                             (and gross-bal?
                                  (double-col 'entry credit report-commodity
                                              exchange-fn show-fcur?)))
                            (col (+ account-cols
                                    (* 2 colpair)
                                    (if non-credit? 0 1))))
                       (gnc:html-table-set-cell!
                        build-table html-row col (or entry debit-entry))
                       (if gross-bal?
                           (gnc:html-table-set-cell! build-table html-row
                            (+ col 1) credit-entry))
                       ;; update the corresponding running total
                       (when bal
                         (when credit?
                           (credit-coll 'minusmerge (if gross-bal? credit bal) #f))
                         (when non-credit?
                           (debit-coll 'merge (if gross-bal? debit bal) #f)))))
                   (list adj-col atb-col is-col bs-col)
                   (list adj-debits atb-debits is-debits bs-debits)
                   (list adj-credits atb-credits is-credits bs-credits))
                  ;; make sure the row extends to the final column
                  (set! last-col (+ account-cols (* 2 bs-col) 1))
                  (or (gnc:html-table-get-cell build-table html-row last-col)
                      (gnc:html-table-set-cell! build-table html-row last-col #f))
                  (set! row (+ row 1)))))

          ;; now do the column totals
          (gnc:html-table-append-row/markup!
           build-table "primary-subheading"
           (append
            (list (gnc:make-html-table-cell/markup "total-label-cell" #f))
            (gnc:html-make-empty-cells (- account-cols 1))
            (list (tot-abs-amt-cell debit-tot))
            (list (tot-abs-amt-cell credit-tot))
            (if (eq? report-variant 'work-sheet)
                (list
                 (tot-abs-amt-cell adj-debits)
                 (tot-abs-amt-cell adj-credits)
                 (tot-abs-amt-cell atb-debits)
                 (tot-abs-amt-cell atb-credits)
                 (tot-abs-amt-cell is-debits)
                 (tot-abs-amt-cell is-credits)
                 (tot-abs-amt-cell bs-debits)
                 (tot-abs-amt-cell bs-credits))
                '())))
          (if (eq? report-variant 'work-sheet)
              (let* ((net-is (gnc:collector- is-debits is-credits))
                     (net-bs (gnc:collector- bs-debits bs-credits))
                     (tot-is (gnc:make-commodity-collector))
                     (tot-bs (gnc:make-commodity-collector))
                     (is-entry #f)
                     (is-credit? #f)
                     (bs-entry #f)
                     (bs-credit? #f)
                     (tbl-width (+ account-cols (* 2 bs-col) 2))
                     (this-row (gnc:html-table-num-rows build-table)))
                (set! is-entry
                  (double-col 'entry net-is report-commodity exchange-fn show-fcur?))
                (set! is-credit?
                  (double-col 'credit-q net-is report-commodity
                              exchange-fn show-fcur?))
                (set! bs-entry
                  (double-col 'entry net-bs report-commodity
                              exchange-fn show-fcur?))
                (set! bs-credit?
                  (double-col 'credit-q net-bs report-commodity
                              exchange-fn show-fcur?))
                (gnc:html-table-add-labeled-amount-line!
                 build-table tbl-width "primary-subheading" #f
                 (if is-credit? (_ "Net Income") (_ "Net Loss"))
                 0 1 "total-label-cell" is-entry
                 (+ account-cols (* 2 is-col) (if is-credit? 0 1))
                 1 "total-number-cell")
                (gnc:html-table-set-cell!
                 build-table this-row
                 (+ account-cols (* 2 bs-col) (if bs-credit? 0 1))
                 (tot-abs-amt-cell net-bs))
                (set! this-row (+ this-row 1))

                ;; now slap on the grand totals
                (tot-is 'merge (if is-credit? is-debits is-credits) #f)
                (if is-credit?
                    (tot-is 'minusmerge net-is #f)
                    (tot-is 'merge net-is #f))
                (tot-bs 'merge (if bs-credit? bs-debits bs-credits) #f)
                (if bs-credit?
                    (tot-bs 'minusmerge net-bs #f)
                    (tot-bs 'merge net-bs #f))

                (gnc:html-table-append-row/markup!
                 build-table "primary-subheading"
                 (append
                  (gnc:html-make-empty-cells (+ account-cols (* 2 is-col)))
                  (list (tot-abs-amt-cell (if is-credit? tot-is is-debits))
                        (tot-abs-amt-cell (if is-credit? is-credits tot-is))
                        (tot-abs-amt-cell (if bs-credit? tot-bs bs-debits))
                        (tot-abs-amt-cell (if bs-credit? bs-credits tot-bs)))))))

          ;; ...and that's a complete trial balance/work sheet
          (gnc:html-document-add-object! doc build-table)

          ;; add currency information if requested
          (gnc:report-percent-done 90)
          (if show-rates?
              (gnc:html-document-add-object!
               doc
               (gnc:html-make-exchangerates
                report-commodity exchange-fn accounts)))
          (gnc:report-percent-done 100)))

    (gnc:report-finished)

    doc))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "216cd0cf6931453ebcce85415aba7082"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator trial-balance-options-generator
 'renderer trial-balance-renderer)

;; END
