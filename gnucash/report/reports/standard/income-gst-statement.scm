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

(define-module (gnucash reports standard income-gst-statement))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

;; Define the strings here to avoid typos and make changes easier.
(define reportname (N_ "Income and GST Statement"))
(define pagename-sorting (N_ "Sorting"))
(define pagename-filter (N_ "Filter"))
(define pagename-format (N_ "Format"))

(define TAX-SETUP-DESC
  (gnc:make-html-text

   (gnc:html-markup-p
    (G_ "This report is useful to calculate periodic business tax \
payable/receivable from authorities. From 'Edit report options', \
choose your business sales and purchase accounts. Each transaction \
may contain, in addition to the asset, liability, A/Payable or \
A/Receivable accounts, a split to a tax account, e.g. Income:Sales \
-$1000, A/Receivable $1100, Liability:GST on Sales -$100."))

   (gnc:html-markup-p
    (G_ "These tax accounts can either be populated using the standard register, or from Business Invoices and Bills \
which will require Tax Tables to be set up correctly. Please see the documentation."))

   (gnc:html-markup-p
    (G_ "From the Report Options, you will need to select the accounts which will \
hold the GST/VAT taxes collected or paid. These accounts must contain splits which document the \
monies which are wholly sent or claimed from tax authorities during periodic GST/VAT returns. These \
accounts must be of type ASSET for taxes paid on expenses, and type LIABILITY for taxes collected on sales."))

   (gnc:html-markup-p
    (G_ "Note the UK variant may specify EU VAT accounts may be tagged \
with *EUVAT* in the VAT account description. EU Goods sales and purchase \
accounts may be tagged with *EUGOODS* in the account description."))

   (gnc:html-markup-p
    (G_ "This message will be removed when tax accounts are specified."))))

(define* (gst-statement-renderer rpt #:optional export-type)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options rpt) section name)))
  (define sales-purch-accounts
    (append (opt-val "Accounts" "Sales") (opt-val "Accounts" "Purchases")))
  (define document
    (gnc:trep-renderer
     rpt
     #:custom-calculated-cells gst-calculated-cells
     #:custom-source-accounts sales-purch-accounts
     #:custom-split-filter gst-custom-split-filter
     #:export-type export-type))
  (when (null? (opt-val "Accounts" "Tax Accounts"))
    (gnc:html-document-add-object! document TAX-SETUP-DESC))
  document)

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
  (define all-accounts
    (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
  (define format-options
    (list
     (list (N_ "Individual sales columns") "p"
           (N_ "Display individual sales columns rather than their sum") #f)
     (list (N_ "Individual purchases columns") "q"
           (N_ "Display individual purchases columns rather than their sum") #f)
     (list (N_ "Individual tax columns") "r"
           (N_ "Display individual tax columns rather than their sum") #f)
     (list (N_ "Gross Balance") "s"
           (N_ "Display the gross balance (gross sales - gross purchases)") #f)
     (list (N_ "Net Balance") "t"
           (N_ "Display the net balance (sales without tax - purchases without tax)")
           #f)
     (list (N_ "Tax payable") "u"
           (N_ "Display the tax payable (tax on sales - tax on purchases)") #f)))

  ;; Delete Accounts selector
  (gnc:unregister-option options gnc:pagename-accounts (N_ "Accounts"))

  ;;  and recreate with limited account types
  (gnc:register-option
   options
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Sales") "a" (N_ "Report on these accounts.")
    (lambda ()
      (gnc:filter-accountlist-type
       (list ACCT-TYPE-INCOME)
       all-accounts))
    #f #t))

  (gnc:register-option
   options
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Purchases") "b" (N_ "Report on these accounts.")
    (lambda ()
      (gnc:filter-accountlist-type
       (list ACCT-TYPE-EXPENSE)
       all-accounts)) #f #t))

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

  (gnc:register-option
   options
   (gnc:make-multichoice-callback-option
    pagename-format (N_ "Report Format")
    "a"
    (string-join
     (list
      (G_ "Report Format")
      (G_ "Default Format")
      (G_ "Australia Business Activity Statement. Specify sales, \
purchase and tax accounts.")
      (G_ "UK VAT Return. Specify sales, purchase and tax \
accounts. EU rules may be used. Denote EU VAT accounts *EUVAT* in \
account description, and denote EU goods sales and purchases accounts \
with *EUGOODS* in the account description."))
     "\n* ")
    'default
    (list (vector 'default (G_ "Default Format"))
          (vector 'au-bas (G_ "Australia BAS"))
          (vector 'uk-vat (G_ "UK VAT Return")))
     #f
    (lambda (x)
      (for-each
       (match-lambda
         ((name . _)
          (gnc-option-db-set-option-selectable-by-name
           options pagename-format name (eq? x 'default))))
       format-options))))

  (for-each
   (match-lambda
     ((name sort help default)
      (gnc:register-option options
                           (gnc:make-simple-boolean-option
                            pagename-format name sort help default))))
   format-options)

  ;; Enable option to retrieve unique transactions only
  (gnc:option-set-default-value
   (gnc:lookup-option options "__trep" "unique-transactions") #t)
  ;; Disable account filtering
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter Type")
  (gnc:option-make-internal! options gnc:pagename-accounts "Filter By...")
  (gnc:option-make-internal! options "Currency" "Show original currency amount")

  ;; Enforce compulsory common-currency. It's senseless to allow
  ;; multiple currencies in a government report. Plus, single currency
  ;; means only 1 amount per heading for CSV output.
  (gnc:option-set-default-value
   (gnc:lookup-option options "Currency" "Common Currency") #t)
  (gnc:option-make-internal! options "Currency" "Common Currency")

  ;; Set default dates to report on last quarter.
  (gnc:option-set-default-value
   (gnc:lookup-option options gnc:pagename-general "Start Date")
   '(relative . start-prev-quarter))
  (gnc:option-set-default-value
   (gnc:lookup-option options gnc:pagename-general "End Date")
   '(relative . end-prev-quarter))

  ;; Disallow closing transactions
  (gnc:option-make-internal! options pagename-filter "Closing transactions")
  (gnc:option-set-default-value
   (gnc:lookup-option options pagename-filter "Closing transactions")
   'exclude-closing)

  ;; Set good sorting options
  (gnc:option-set-default-value
   (gnc:lookup-option options pagename-sorting "Primary Key")
   'date)
  (gnc:option-set-default-value
   (gnc:lookup-option options pagename-sorting "Primary Subtotal for Date Key")
   'none)
  (gnc:option-set-default-value
   (gnc:lookup-option options pagename-sorting "Secondary Key")
   'none)

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

(define (myadd a b)
  (if a (if b (gnc:monetary+ a b) a) b))

(define (myneg X)
  (and X (gnc:monetary-neg X)))

(define (accfilter accounts type)
  (filter (lambda (acc) (eqv? (xaccAccountGetType acc) type)) accounts))

(define split->date (compose xaccTransGetDate xaccSplitGetParent))
(define split->currency (compose xaccAccountGetCommodity xaccSplitGetAccount))

(define (gst-calculated-cells options)
  (define (opt-val section name)
    (gnc:option-value (gnc:lookup-option options section name)))
  (let* ((tax-accounts (opt-val gnc:pagename-accounts "Tax Accounts"))
         (accounts-tax-collected (accfilter tax-accounts ACCT-TYPE-LIABILITY))
         (accounts-tax-paid      (accfilter tax-accounts ACCT-TYPE-ASSET))
         (accounts-sales         (opt-val gnc:pagename-accounts "Sales"))
         (accounts-purchases     (opt-val gnc:pagename-accounts "Purchases"))
         (common-currency        (opt-val "Currency" "Report's currency")))

    (define (split-adder split accountlist)
      (define txn (xaccSplitGetParent split))
      (define (not-in-accountlist? s)
        (not (member (xaccSplitGetAccount s) accountlist)))
      (let lp ((splits (xaccTransGetSplitList txn)) (result #f))
        (match splits
          (() result)
          (((? not-in-accountlist?) . rest) (lp rest result))
          ((split . rest)
           (lp rest
               (myadd (gnc:exchange-by-pricedb-nearest
                       (gnc:make-gnc-monetary
                        (split->currency split)
                        (if (xaccTransGetVoidStatus txn)
                            (xaccSplitVoidFormerAmount split)
                            (xaccSplitGetAmount split)))
                       common-currency
                       (time64CanonicalDayTime (split->date split)))
                      result))))))

    (define (account-adder acc)
      (lambda (s) (split-adder s (list acc))))

    (define (account-adder-neg acc)
      (lambda (s) (myneg (split-adder s (list acc)))))

    ;; each column will be a vector
    ;; (vector heading              - string
    ;;         calculator-function  - (calculator-function split) to obtain amount
    ;;         reverse-column?      - #f - already handled via myneg in fns above
    ;;         subtotal?            - #t - all columns need subtotals
    ;;         start-dual-column?   - unused in GST report
    ;;         friendly-heading-fn  - unused in GST report

    (case (opt-val pagename-format "Report Format")
      ((default)
       (let* ((net-sales (lambda (s) (myneg (split-adder s accounts-sales))))
              (tax-sales (lambda (s) (myneg (split-adder s accounts-tax-collected))))
              (tot-sales (lambda (s) (myadd (tax-sales s) (net-sales s))))
              (net-purch (lambda (s) (split-adder s accounts-purchases)))
              (tax-purch (lambda (s) (split-adder s accounts-tax-paid)))
              (tot-purch (lambda (s) (myadd (tax-purch s) (net-purch s))))
              (tot-bal   (lambda (s) (myadd (tot-sales s) (myneg (tot-purch s)))))
              (net-bal   (lambda (s) (myadd (net-sales s) (myneg (net-purch s)))))
              (tax-diff  (lambda (s) (myadd (tax-sales s) (myneg (tax-purch s))))))
         (append

          ;; Translators: "Gross Sales" refer to Net Sales + GST/VAT on Sales
          (list (vector (G_ "Gross Sales") tot-sales #f #t #f #f))

          (if (opt-val pagename-format "Individual sales columns")
              (map
               (lambda (acc)
                 (vector (xaccAccountGetName acc) (account-adder-neg acc) #f #t #f #f))
               accounts-sales)
              (list (vector (G_ "Net Sales") net-sales #f #t #f #f)))

          (if (opt-val pagename-format "Individual tax columns")
              (map
               (lambda (acc)
                 (vector (xaccAccountGetName acc) (account-adder-neg acc) #f #t #f #f))
               accounts-tax-collected)
              (list (vector (G_ "Tax on Sales") tax-sales #f #t #f #f)))

          ;; Translators: "Gross Purchases" refer to Net Purchase +
          ;; GST/VAT on Purchase
          (list (vector (G_ "Gross Purchases") tot-purch #f #t #f #f))

          (if (opt-val pagename-format "Individual purchases columns")
              (map
               (lambda (acc)
                 (vector (xaccAccountGetName acc) (account-adder acc) #f #t #f #f))
               accounts-purchases)
              (list
               (vector (G_ "Net Purchases") net-purch #f #t #f #f)))

          (if (opt-val pagename-format "Individual tax columns")
              (map
               (lambda (acc)
                 (vector (xaccAccountGetName acc) (account-adder acc) #f #t #f #f))
               accounts-tax-paid)
              (list
               (vector (G_ "Tax on Purchases") tax-purch #f #t #f #f)))

          (if (opt-val pagename-format "Gross Balance")
              ;; Translators: "Gross Balance" refer to "Gross Sales
              ;; minus Gross Purchases" in GST Report
              (list
               (vector (G_ "Gross Balance") tot-bal #f #t #f #f))
              '())

          ;; Note: Net income = net balance - other costs
          (if (opt-val pagename-format "Net Balance")
              ;; Translators: "Net Balance" refer to Net Sales - Net
              ;; Purchases in GST Report
              (list
               (vector (G_ "Net Balance") net-bal #f #t #f #f))
              '())

          (if (opt-val pagename-format "Tax payable")
              ;; Translators: "Tax Payable" refer to the difference
              ;; GST Sales - GST Purchases
              (list
               (vector (G_ "Tax payable") tax-diff #f #t #f #f))
              '()))))

      ((au-bas)
       (let* ((gst-sales (lambda (s) (myneg (split-adder s accounts-tax-collected))))
              (gst-purch (lambda (s) (split-adder s accounts-tax-paid)))
              (sales-net (lambda (s) (myneg (split-adder s accounts-sales))))
              (sales-gross (lambda (s) (myadd (sales-net s) (gst-sales s)))))
         (list
          (vector "G1 Total Sales inc GST" sales-gross #f #t #f #f)
          (vector "1A GST on Sales"        gst-sales   #f #t #f #f)
          (vector "1B GST on Purchases"    gst-purch   #f #t #f #f))))

      ((uk-vat)
       (let* ((EUVAT?
               (lambda (acc)
                 (string-contains (xaccAccountGetDescription acc) "*EUVAT*")))
              (EUGOODS?
               (lambda (acc)
                 (string-contains (xaccAccountGetDescription acc) "*EUGOODS*")))
              (vat-non-ec (filter (negate EUVAT?) tax-accounts))
              (vat-on-sales (accfilter vat-non-ec ACCT-TYPE-LIABILITY))
              (vat-on-purchases (accfilter vat-non-ec ACCT-TYPE-ASSET))
              (eu-sales-accts (filter EUGOODS? accounts-sales))
              (eu-purch-accts (filter EUGOODS? accounts-purchases))
              (eu-vat-accounts (filter EUVAT? tax-accounts))
              (eu-vat-rev-purchases (accfilter eu-vat-accounts ACCT-TYPE-LIABILITY))
              (box-1 (lambda (s) (myneg (split-adder s vat-on-sales))))
              (box-2 (lambda (s) (myneg (split-adder s eu-vat-rev-purchases))))
              (box-3 (lambda (s) (myadd (box-1 s) (box-2 s))))
              (box-4 (lambda (s) (split-adder s vat-on-purchases)))
              (box-5 (lambda (s) (myadd (box-3 s) (myneg (box-4 s)))))
              (box-6 (lambda (s) (myneg (split-adder s accounts-sales))))
              (box-7 (lambda (s) (split-adder s accounts-purchases)))
              (box-8 (lambda (s) (myneg (split-adder s eu-sales-accts))))
              (box-9 (lambda (s) (split-adder s eu-purch-accts))))
         (list
          (vector "Box 1 VAT Sales"                         box-1 #f #t #f #f)
          (vector "Box 2 VAT Goods Purchases from EU to NI" box-2 #f #t #f #f)
          (vector "Box 3 VAT Output"                        box-3 #f #t #f #f)
          (vector "Box 4 VAT Purchases"                     box-4 #f #t #f #f)
          (vector "Box 5 VAT Difference"                    box-5 #f #t #f #f)
          (vector "Box 6 Tot Sales"                         box-6 #f #t #f #f)
          (vector "Box 7 Tot Purchases"                     box-7 #f #t #f #f)
          (vector "Box 8 Net Goods Sales from NI to EU"     box-8 #f #t #f #f)
          (vector "Box 9 Net Goods Purchases from EU to NI" box-9 #f #t #f #f)))))))

;; Define the report.
(gnc:define-report
 'version 1
 'menu-path (list gnc:menuname-income-expense)
 'name reportname
 'report-guid "5bf27f249a0d11e7abc4cec278b6b50a"
 'options-generator gst-statement-options-generator
 'renderer gst-statement-renderer
 'export-types '(("CSV" . csv))
 'export-thunk gst-statement-renderer)
