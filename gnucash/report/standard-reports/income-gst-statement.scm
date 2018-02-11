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
(use-modules (gnucash report standard-reports transaction))

;; Define the strings here to avoid typos and make changes easier.
(define reportname (N_ "Income & GST Statement"))
(define pagename-sorting (N_ "Sorting"))
(define TAX-SETUP-DESC
  (string-append
   (_ "This report is useful to calculate periodic business tax payable/receivable from
 authorities. From <i>Edit report options</i> above, choose your Business Income and Business Expense accounts.
 Each transaction may contain, in addition to the accounts payable/receivable or bank accounts,
 a split to a tax account, e.g. Income:Sales -$1000, Liability:GST on Sales -$100, Asset:Bank $1100.")
   "<br><br>"
   (_ "These tax accounts can either be populated using the standard register, or from Business Invoices and Bills
 which will require Business > Sales Tax Tables to be set up correctly. Please see the documentation.")
   "<br><br>"
   (_ "From the Report Options, you will need to select the accounts which will \
hold the GST/VAT taxes collected or paid. These accounts must contain splits which document the \
monies which are wholly sent or claimed from tax authorities during periodic GST/VAT returns. These \
accounts must be of type ASSET for taxes paid on expenses, and type LIABILITY for taxes collected on sales.")))

(define (income-gst-statement-renderer rpt)
  (trep-renderer rpt
                 #:custom-calculated-cells gst-calculated-cells
                 #:empty-report-message TAX-SETUP-DESC
                 #:custom-split-filter gst-custom-split-filter))

(define (gst-custom-split-filter split)
  ;; split -> bool
  ;;
  ;; additional split filter - returns #t if split must be included
  ;; we need to exclude Closing, Link and Payment transactions
  (let ((trans (xaccSplitGetParent split)))
    (and (member (xaccTransGetTxnType trans) (list TXN-TYPE-NONE TXN-TYPE-INVOICE))
         (not (xaccTransGetIsClosingTxn trans)))))

(define (gst-statement-options-generator)

  ;; Retrieve the list of options specified within the transaction report
  (define options (trep-options-generator))

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
    (list (N_ "Individual income columns")    "p" (N_ "Display individual income columns rather than their sum") #f)
    (list (N_ "Individual expense columns")   "q" (N_ "Display individual expense columns rather than their sum") #f)
    (list (N_ "Individual tax columns")       "r" (N_ "Display individual tax columns rather than their sum") #f)
    (list (N_ "Remittance amount")            "s" (N_ "Display the remittance amount (total sales - total purchases)") #f)
    (list (N_ "Net Income")                   "t" (N_ "Display the net income (sales without tax - purchases without tax)") #f)
    (list (N_ "Tax payable")                  "u" (N_ "Display the tax payable (tax on sales - tax on purchases)") #f)))

  ;; Enable secret option to delete transactions with >1 split
  (gnc:option-set-value (gnc:lookup-option options "__trep" "unique-transactions") #t)
  ;; Disable account filtering
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter Type")
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter By...")
  (gnc:option-make-internal! options gnc:pagename-general "Show original currency amount")
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
    (let ((option (gnc:lookup-option options section name)))
      (if option
          (gnc:option-value option)
          (gnc:error "gnc:lookup-option error: " section "/" name))))
  (letrec*
      ((monetary+ (lambda (a b)
                    (if (and (gnc:gnc-monetary? a) (gnc:gnc-monetary? b))
                        (let ((same-currency? (gnc-commodity-equal (gnc:gnc-monetary-commodity a) (gnc:gnc-monetary-commodity b)))
                              (amount (+ (gnc:gnc-monetary-amount a) (gnc:gnc-monetary-amount b))))
                          (if same-currency?
                              (gnc:make-gnc-monetary (gnc:gnc-monetary-commodity a) amount)
                              (gnc:error "incompatible currencies in monetary+: " a b)))
                        (gnc:error "wrong arguments for monetary+: " a b))))
       (myadd (lambda (X Y) (if X (if Y (monetary+ X Y) X) Y)))               ; custom adder which understands #f values
       (myneg (lambda (X) (and X (gnc:monetary-neg X))))                      ; custom monetary negator which understands #f
       (accounts (opt-val gnc:pagename-accounts "Accounts"))
       (tax-accounts (opt-val gnc:pagename-accounts "Tax Accounts"))
       (accounts-tax-collected (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-LIABILITY)) tax-accounts))
       (accounts-tax-paid      (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-ASSET))     tax-accounts))
       (accounts-sales         (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-INCOME))    accounts))
       (accounts-purchases     (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-EXPENSE))   accounts))
       (common-currency (and (opt-val gnc:pagename-general (_ "Common Currency"))             ; if a common currency was specified,
                             (opt-val gnc:pagename-general (_ "Report's currency"))))         ; use it
       (split-date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
       (split-currency (lambda (s) (xaccAccountGetCommodity (xaccSplitGetAccount s))))
       (split-adder (lambda (split accountlist)
                      (let* (;; 1. from split, get the trans
                             (transaction (xaccSplitGetParent split))
                             ;; 2. from trans, get all splits
                             (splits-in-transaction (xaccTransGetSplitList transaction))
                             ;; 3. but only from accounts specified
                             (include-split? (lambda (s) (member (xaccSplitGetAccount s) accountlist)))
                             (filtered-splits (filter include-split? splits-in-transaction))
                             ;; 4. get the filtered split amount
                             (split-get-monetary (lambda (s)
                                                   (gnc:make-gnc-monetary
                                                    (split-currency s)
                                                    (if (xaccTransGetVoidStatus transaction)
                                                        (xaccSplitVoidFormerAmount s)
                                                        (xaccSplitGetAmount s)))))
                             ;; 5. amount - always convert to
                             ;; either report currency or the original split currency
                             (split-monetary-converted (lambda (s)
                                                         (gnc:exchange-by-pricedb-nearest
                                                          (split-get-monetary s)
                                                          (or common-currency
                                                              (split-currency split))
                                                          (time64CanonicalDayTime
                                                           (split-date s)))))
                             (list-of-values (map split-monetary-converted filtered-splits)))
                        (fold myadd #f list-of-values))))
       (account-adder (lambda (acc) (lambda (s) (split-adder s (list acc)))))
       (account-adder-neg (lambda (acc) (lambda (s) (myneg (split-adder s (list acc))))))
       ;; Calculate sales amounts
       (sales-without-tax (lambda (s) (myneg (split-adder s accounts-sales))))
       (tax-on-sales (lambda (s) (myneg (split-adder s accounts-tax-collected))))
       (total-sales (lambda (s) (myadd (tax-on-sales s) (sales-without-tax s))))
       ;; Calculate purchase amounts
       (purchases-without-tax (lambda (s) (split-adder s accounts-purchases)))
       (tax-on-purchases (lambda (s) (split-adder s accounts-tax-paid)))
       (total-purchases (lambda (s) (myadd (tax-on-purchases s) (purchases-without-tax s))))
       ;; Calculate derived amounts
       (bank-remittance (lambda (s) (myadd (total-sales s) (myneg (total-purchases s)))))
       (net-income (lambda (s) (myadd (sales-without-tax s) (myneg (purchases-without-tax s)))))
       (tax-payable (lambda (s) (myadd (tax-on-sales s) (myneg (tax-on-purchases s))))))
    (append
     ;; each column will be a vector
     ;; (vector heading
     ;;         calculator-function                          ;; (calculator-function split) to obtain amount
     ;;         reverse-column?                              ;; unused in GST report
     ;;         subtotal?                                    ;; #t - all columns need subtotals
     ;;         start-dual-column?                           ;; unused in GST report
     ;;         friendly-heading-fn                          ;; unused in GST report
     (list (vector "TOTAL SALES"
                   total-sales
                   #t #t #f
                   (lambda (a) "")))
     (if (opt-val gnc:pagename-display (N_ "Individual income columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder-neg acc)
                                    #t #t #f
                                    (lambda (a) "")))
              accounts-sales)
         (list (vector "Net Sales"
                       sales-without-tax
                       #t #t #f
                       (lambda (a) ""))))
     (if (opt-val gnc:pagename-display (N_ "Individual tax columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder acc)
                                    #t #t #f
                                    (lambda (a) "")))
              accounts-tax-collected)
         (list (vector "Tax on Sales"
                       tax-on-sales
                       #t #t #f
                       (lambda (a) ""))))
     (list (vector "TOTAL PURCHASES"
                   total-purchases
                   #f #t #f
                   (lambda (a) "")))
     (if (opt-val gnc:pagename-display (N_ "Individual expense columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder acc)
                                    #f #t #f
                                    (lambda (a) "")))
              accounts-purchases)
         (list (vector "Net Purchases"
                       purchases-without-tax
                       #f #t #f
                       (lambda (a) ""))))
     (if (opt-val gnc:pagename-display (N_ "Individual tax columns"))
         (map (lambda (acc) (vector (xaccAccountGetName acc)
                                    (account-adder acc)
                                    #f #t #f
                                    (lambda (a) "")))
              accounts-tax-paid)
         (list (vector "Tax on Purchases"
                       tax-on-purchases
                       #f #t #f
                       (lambda (a) ""))))
     (if (opt-val gnc:pagename-display (N_ "Remittance amount"))
         (list (vector "Remittance"
                       bank-remittance
                       #f #t #f
                       (lambda (a) "")))
         '())
     (if (opt-val gnc:pagename-display (N_ "Net Income"))
         (list (vector "Net Income"
                       net-income
                       #f #t #f
                       (lambda (a) "")))
         '())
     (if (opt-val gnc:pagename-display (N_ "Tax payable"))
         (list (vector "Tax Payable"
                       tax-payable
                       #f #t #f
                       (lambda (a) "")))
         '()))))

;; Define the report.
(gnc:define-report
 'version 1
 'menu-path (list gnc:menuname-income-expense)
 'name reportname
 'report-guid "5bf27f249a0d11e7abc4cec278b6b50a"
 'options-generator gst-statement-options-generator
 'renderer income-gst-statement-renderer)
