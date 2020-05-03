;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Income-GST-Statement.scm : Produce report suitable for
;; annual income tax returns and periodic VAT/GST reporting.
;;
;; Original Income and GST Report designed by Christopher Lam
;; Will reuse the Transaction Report with customised options
;; and calculated cells.
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

(define-module (gnucash report standard-reports income-gst-statement))

(use-modules (gnucash utilities)) 
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report/report-system" 0)

;; Define the strings here to avoid typos and make changes easier.
(define reportname (N_ "Income and GST Statement"))
(define pagename-sorting (N_ "Sorting"))
(define pagename-filter (N_ "Filter"))
(define TAX-SETUP-DESC
  (gnc:make-html-text
   (_ "This report is useful to calculate periodic business tax payable/receivable from \
authorities. From 'Edit report options' above, choose your Business Income and Business Expense accounts. \
Each transaction may contain, in addition to the accounts payable/receivable or bank accounts, \
a split to a tax account, e.g. Income:Sales -$1000, Liability:GST on Sales -$100, Asset:Bank $1100.")
   (gnc:html-markup-br)
   (gnc:html-markup-br)
   (_ "These tax accounts can either be populated using the standard register, or from Business Invoices and Bills \
which will require Tax Tables to be set up correctly. Please see the documentation.")
   (gnc:html-markup-br)
   (gnc:html-markup-br)
   (_ "From the Report Options, you will need to select the accounts which will \
hold the GST/VAT taxes collected or paid. These accounts must contain splits which document the \
monies which are wholly sent or claimed from tax authorities during periodic GST/VAT returns. These \
accounts must be of type ASSET for taxes paid on expenses, and type LIABILITY for taxes collected on sales.")
   (gnc:html-markup-br)
   (gnc:html-markup-br)))

(define (gst-statement-renderer rpt)
  (gnc:trep-renderer
   rpt
   #:custom-calculated-cells gst-calculated-cells
   #:empty-report-message TAX-SETUP-DESC
   #:custom-split-filter gst-custom-split-filter))

(define (gst-custom-split-filter split)
  ;; split -> bool
  ;;
  ;; additional split filter - returns #t if split must be included
  ;; we need to exclude Link and Payment transactions
  (memv (xaccTransGetTxnType (xaccSplitGetParent split))
        (list TXN-TYPE-NONE TXN-TYPE-INVOICE)))

(define (gst-statement-options-generator)

  ;; Retrieve the list of options specified within the transaction report
  (define options (gnc:trep-options-generator))

  ;; Delete Accounts selector
  (gnc:unregister-option options gnc:pagename-accounts (N_ "Accounts"))

  ;;  and recreate with limited account types
  (gnc:register-option
   options
   (gnc:make-account-list-limited-option
    gnc:pagename-accounts (N_ "Accounts") "b1" (N_ "Report on these accounts.")
    (lambda () '()) #f #t
    (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE)))

  (gnc:register-option
   options
   (gnc:make-account-list-limited-option
    gnc:pagename-accounts (N_ "Tax Accounts")
    "b17" (N_ "Please find and select the accounts which will hold the tax collected or paid. \
These accounts must contain splits which document the monies which are wholly sent or claimed \
from tax authorities during periodic GST/VAT returns. These accounts must be of type ASSET \
for taxes paid on expenses, and type LIABILITY for taxes collected on sales.")
    (lambda () '()) #f #t
    (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY)))

  (for-each
   (lambda (l)
     (gnc:register-option
      options
      (gnc:make-simple-boolean-option
       gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
   (list
    (list (N_ "Individual sales columns")     "p" (N_ "Display individual sales columns rather than their sum") #f)
    (list (N_ "Individual purchases columns") "q" (N_ "Display individual purchases columns rather than their sum") #f)
    (list (N_ "Individual tax columns")       "r" (N_ "Display individual tax columns rather than their sum") #f)
    (list (N_ "Gross Balance")                "s" (N_ "Display the gross balance (gross sales - gross purchases)") #f)
    (list (N_ "Net Balance")                  "t" (N_ "Display the net balance (sales without tax - purchases without tax)") #f)
    (list (N_ "Tax payable")                  "u" (N_ "Display the tax payable (tax on sales - tax on purchases)") #f)))

  ;; Enable option to retrieve unique transactions only
  (gnc:option-set-value (gnc:lookup-option options "__trep" "unique-transactions") #t)
  ;; Disable account filtering
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter Type")
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter By...")
  (gnc:option-make-internal! options gnc:pagename-general "Show original currency amount")
  ;; Disallow closing transactions
  (gnc:option-set-value (gnc:lookup-option options pagename-filter "Closing transactions") 'exclude-closing)
  (gnc:option-make-internal! options pagename-filter "Closing transactions")
  ;; Disable display options not being used anymore
  (gnc:option-make-internal! options gnc:pagename-display "Shares")
  (gnc:option-make-internal! options gnc:pagename-display "Price")
  (gnc:option-make-internal! options gnc:pagename-display "Amount")
  (gnc:option-make-internal! options gnc:pagename-display "Sign Reverses")
  (gnc:option-make-internal! options gnc:pagename-display "Running Balance")
  ;; No multilines allowed
  (gnc:option-make-internal! options gnc:pagename-display "Detail Level")
  (gnc:option-make-internal! options pagename-sorting "Show Informal Debit/Credit Headers")
  options)

(define (gst-calculated-cells options)
  (define (opt-val section name)
    (gnc:option-value (gnc:lookup-option options section name)))
  (define (accfilter accounts type)
    (filter
     (lambda (acc)
       (eqv? (xaccAccountGetType acc) type))
     accounts))
  (letrec*
      ((myadd (lambda (X Y) (if X (if Y (gnc:monetary+ X Y) X) Y)))
       (myneg (lambda (X) (and X (gnc:monetary-neg X))))
       (accounts (opt-val gnc:pagename-accounts "Accounts"))
       (tax-accounts (opt-val gnc:pagename-accounts "Tax Accounts"))
       (accounts-tax-collected (accfilter tax-accounts ACCT-TYPE-LIABILITY))
       (accounts-tax-paid      (accfilter tax-accounts ACCT-TYPE-ASSET))
       (accounts-sales         (accfilter accounts ACCT-TYPE-INCOME))
       (accounts-purchases     (accfilter accounts ACCT-TYPE-EXPENSE))
       (common-currency (and (opt-val gnc:pagename-general "Common Currency")
                             (opt-val gnc:pagename-general "Report's currency")))
       (split->date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
       (split->currency (lambda (s) (xaccAccountGetCommodity (xaccSplitGetAccount s))))
       (split-adder (lambda (split accountlist)
                      (let* ((txn (xaccSplitGetParent split))
                             (filtered-splits (filter
                                               (lambda (s)
                                                 (member (xaccSplitGetAccount s)
                                                         accountlist))
                                               (xaccTransGetSplitList txn)))
                             (split->monetary (lambda (s)
                                                (gnc:make-gnc-monetary
                                                 (split->currency s)
                                                 (if (xaccTransGetVoidStatus txn)
                                                     (xaccSplitVoidFormerAmount s)
                                                     (xaccSplitGetAmount s)))))
                             (split->converted
                              (lambda (s)
                                (gnc:exchange-by-pricedb-nearest
                                 (split->monetary s)
                                 (or common-currency (split->currency split))
                                 (time64CanonicalDayTime (split->date s)))))
                             (list-of-values (map split->converted filtered-splits)))
                        (fold myadd #f list-of-values))))
       (account-adder (lambda (acc) (lambda (s) (split-adder s (list acc)))))
       (account-adder-neg (lambda (acc) (lambda (s) (myneg (split-adder s (list acc))))))

       ;; Calculate sales amounts
       (sales-without-tax (lambda (s) (myneg (split-adder s accounts-sales))))
       (tax-on-sales (lambda (s) (myneg (split-adder s accounts-tax-collected))))
       (gross-sales (lambda (s) (myadd (tax-on-sales s) (sales-without-tax s))))

       ;; Calculate purchase amounts
       (purchases-without-tax (lambda (s) (split-adder s accounts-purchases)))
       (tax-on-purchases (lambda (s) (split-adder s accounts-tax-paid)))
       (gross-purchases (lambda (s) (myadd (tax-on-purchases s) (purchases-without-tax s))))

       ;; Calculate derived amounts
       (gross-balance (lambda (s) (myadd (gross-sales s) (myneg (gross-purchases s)))))
       (net-balance (lambda (s) (myadd (sales-without-tax s) (myneg (purchases-without-tax s)))))
       (tax-payable (lambda (s) (myadd (tax-on-sales s) (myneg (tax-on-purchases s))))))

    ;; each column will be a vector
    ;; (vector heading              - string
    ;;         calculator-function  - (calculator-function split) to obtain amount
    ;;         reverse-column?      - #f - already handled via myneg in fns above
    ;;         subtotal?            - #t - all columns need subtotals
    ;;         start-dual-column?   - unused in GST report
    ;;         friendly-heading-fn  - unused in GST report
    (append
     ;; Translators: "Gross Sales" refer to Net Sales + GST/VAT on Sales
     (list (vector (_ "Gross Sales")
                   gross-sales
                   #f #t #f #f))
     (if (opt-val gnc:pagename-display (N_ "Individual sales columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder-neg acc)
                                    #f #t #f #f))
              accounts-sales)
         (list (vector (_ "Net Sales")
                       sales-without-tax
                       #f #t #f #f)))
     (if (opt-val gnc:pagename-display (N_ "Individual tax columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder-neg acc)
                                    #f #t #f #f))
              accounts-tax-collected)
         (list (vector (_ "Tax on Sales")
                       tax-on-sales
                       #f #t #f #f)))
     ;; Translators: "Gross Purchases" refer to Net Purchase + GST/VAT on Purchase
     (list (vector (_ "Gross Purchases")
                   gross-purchases
                   #f #t #f #f))
     (if (opt-val gnc:pagename-display (N_ "Individual purchases columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder acc)
                                    #f #t #f #f))
              accounts-purchases)
         (list (vector (_ "Net Purchases")
                       purchases-without-tax
                       #f #t #f #f)))
     (if (opt-val gnc:pagename-display (N_ "Individual tax columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder acc)
                                    #f #t #f #f))
              accounts-tax-paid)
         (list (vector (_ "Tax on Purchases")
                       tax-on-purchases
                       #f #t #f #f)))
     (if (opt-val gnc:pagename-display (N_ "Gross Balance"))
         ;; Translators: "Gross Balance" refer to "Gross Sales - Gross Purchases" in GST Report
         (list (vector (_ "Gross Balance")
                       gross-balance
                       #f #t #f #f))
         '())
         ;; Note: Net income = net balance - other costs
     (if (opt-val gnc:pagename-display (N_ "Net Balance"))
         ;; Translators: "Net Balance" refer to Net Sales - Net Purchases in GST Report
         (list (vector (_ "Net Balance")
                       net-balance
                       #f #t #f #f))
         '())
     (if (opt-val gnc:pagename-display (N_ "Tax payable"))
         ;; Translators: "Tax Payable" refer to the difference GST Sales - GST Purchases
         (list (vector (_ "Tax payable")
                       tax-payable
                       #f #t #f #f))
         '()))))

;; Define the report.
(gnc:define-report
 'version 1
 'menu-path (list gnc:menuname-income-expense)
 'name reportname
 'report-guid "5bf27f249a0d11e7abc4cec278b6b50a"
 'options-generator gst-statement-options-generator
 'renderer gst-statement-renderer
 'export-types (list (cons "CSV" 'csv))
 'export-thunk (lambda (report-obj export-type file-name)
                 (gnc:trep-renderer
                  report-obj
                  #:custom-calculated-cells gst-calculated-cells
                  #:empty-report-message TAX-SETUP-DESC
                  #:custom-split-filter gst-custom-split-filter
                  #:export-type export-type
                  #:filename file-name)))
