;; -*-scheme-*-
;; by  Richard -Gilligan- Uschold
;;
;; updated by  J. Alex Aycinena, July 2008, October 2009
;;
;; This report prints transaction details and account totals for accounts
;; relevant to United States taxes, sorted by form/schedule, copy, line
;; and tax code, and exports TXF files for import to TaxCut, TurboTax, etc.
;;
;; For this to work, the user has to segregate taxable and not taxable
;; income to different accounts, as well as deductible and non-
;; deductible expenses and the accounts need to be referenced to the tax codes.
;; However, there is no need to limit tax codes to just one account. For codes
;; like N286 (Dividend, Ordinary) that can have the "payer" printed on
;; Schedule B on separate lines, to have amounts from different accounts
;; summarized together for one "payer" line, the accounts referenced to the
;; same tax code for a given "payer" need to be adjacent to each other in the
;; account hierarchy.
;;
;; The user selects the accounts(s) to be printed; if none are specified, all
;; are selected. Includes all sub-account levels below selected account, that
;; are coded for taxes.
;;
;; Optionally, does NOT print tax codes and accounts with $0.00 values.
;; Prints data between the From and To dates, inclusive.
;; Optional alternate periods:
;; "Last Year", "1st Est Tax Quarter", ... "4th Est Tax Quarter"
;; "Last Yr Est Tax Qtr", ... "Last Yr Est Tax Qtr"
;; Estimated Tax Quarters: Dec 31, Mar 31, Jun 30, Aug 31
;; Optionally prints brief or full account names
;; Optionally prints multi-split details for transactions
;; Optionally prints TXF export parameters for codes and accounts
;; Optionally prints Action/Memo data for a transaction split
;; Optionally prints transaction detail
;; Optionally uses special date processing for selected accounts (see
;;   definition for 'txf-special-split?' in the code below)
;; Optionally shades alternate transactions for ease of reading
;; Converts non-USD transaction amounts based on transaction data or, if
;;   transaction data is not applicable, on pricedb and user specified date:
;;   nearest transaction date or nearest report end date. Converts to zero
;;   if there is no entry in pricedb and provides comment accordingly.
;;
;; November, 2009 Update:
;;
;; Add support for multiple copies of Forms/Schedules
;; Add support for Format 6
;; Use Form/Schedule line #'s to sort report.
;; Update from "V037" to "V041"
;; Add support for taxpayer types other than F1040
;;
;; September, 2010 Update:
;;
;; Add support for code N673, Format 4
;;
;; September, 2012 Update:
;;
;; Add support of book option for num-source; use function gnc-get-num-action in
;; place of xaccTransGetNum and function gnc-get-action-num in place of
;; xaccSplitGetAction and modify report headings accordingly
;;
;; February, 2013 Update:
;;
;; Fix beginning balance sign and signs for Transfer From/To amounts for
;; liability/equity accounts
;;
;; January, 2019 Update:
;;
;; Update from "V041" to "V042", although added codes are not implemented
;;   because cost/gain data not reliably available
;; The format for code 673 can be 4 or 5, per spec, so leave as 4
;; Fix beginning balance off-by-one-day error for B/S accounts
;;
;; From prior version:
;; NOTE: setting of specific dates is squirly! and seems
;; to be current-date dependent!  Actually, time of day dependent!  Just
;; after midnight gives diffenent dates than just before!  Referencing
;; all times to noon seems to fix this.  Subtracting 1 year sometimes
;; subtracts 2!  see "(to-value"
;;
;; Based on prior taxtxf.scm and with references to transaction.scm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash reports locale-specific us taxtxf))
(use-modules (gnucash utilities)) 
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(eval-when
      (compile load eval expand)
      (load-extension "libgncmod-gnome-utils" "scm_init_sw_gnome_utils_module"))
(use-modules (sw_gnome_utils)) ;; to get to gnc-error-dialog

(use-modules (gnucash core-utils)) ; for gnc:version

(gnc:module-load "gnucash/html" 0)
(gnc:module-load "gnucash/tax/us" 0)
(gnc:module-load "gnucash/report" 0)

(define reportname (N_ "Tax Schedule Report/TXF Export"))

;(define USD-currency (gnc-commodity-table-lookup
;                        (gnc-commodity-table-get-table (gnc-get-current-book))
;                        "CURRENCY"
;                        "USD"))
;; this comes back as PEN??? rather than USD; need to define further down to get
;; to work - weird

(define selected-accounts-sorted-by-form-line-acct (list))

(define today (time64CanonicalDayTime (current-time)))

(define bdtm
  (let ((result (gnc-localtime today)))
    (set-tm:mday result 16)             ; 16
    (set-tm:mon result 3)               ; Apr
    (set-tm:isdst result -1)
    result))

(define tax-day (gnc-mktime bdtm))

(define after-tax-day (< tax-day today))

(define (make-split-list account split-filter-pred)
  (filter split-filter-pred (xaccAccountGetSplitList account)))

;; returns a predicate that returns true only if a split is
;; between early-date and late-date
(define (split-report-make-date-filter-predicate begin-date-t64 end-date-t64)
  (lambda (split)
    (let ((t64 (xaccTransGetDate (xaccSplitGetParent split))))
      (and (>= t64 begin-date-t64)
           (<= t64 end-date-t64)))))

;; This is nearly identical to, and could be shared with
;; display-report-list-item in report-impl.scm. This adds warn-msg parameter
(define (gnc:display-report-list-item item port warn-msg)
  (cond
   ((string? item) (display item port))
   ((null? item) #t)
   ((list? item) (map (lambda (item)
                        (gnc:display-report-list-item item port warn-msg))
                      item))
   (else (gnc:warn warn-msg item " is the wrong type."))))

;; IRS asked congress to make the tax quarters the same as real quarters
;;   This is the year it is effective.  THIS IS A Y10K BUG!
(define tax-qtr-real-qtr-year 10000)

(define (tax-options-generator)
  (define options (gnc:new-options))
  (define (gnc:register-tax-option new-option)
    (gnc:register-option options new-option))

  ;; date at which to report
  (gnc:options-add-date-interval!
   options gnc:pagename-general
   (N_ "From") (N_ "To") "a")

  (gnc:register-tax-option
   (gnc:make-multichoice-option
    gnc:pagename-general (N_ "Alternate Period")
    "c" (N_ "Override or modify From: & To:.")
    (if after-tax-day 'from-to 'last-year)
    (list (list->vector
           (list 'from-to (N_ "Use From - To") (N_ "Use From - To period.")))
          (list->vector
           (list '1st-est (N_ "1st Est Tax Quarter") (N_ "Jan 1 - Mar 31.")))
          (list->vector
           (list '2nd-est (N_ "2nd Est Tax Quarter") (N_ "Apr 1 - May 31.")))
          (list->vector
	   ;; Translators: The US tax quarters are different from
	   ;; actual year's quarters! See the definition of
	   ;; tax-qtr-real-qtr-year variable above.
           (list '3rd-est (N_ "3rd Est Tax Quarter") (N_ "Jun 1 - Aug 31.")))
          (list->vector
           (list '4th-est (N_ "4th Est Tax Quarter") (N_ "Sep 1 - Dec 31.")))
          (list->vector
           (list 'last-year (N_ "Last Year") (N_ "Last Year.")))
          (list->vector
           (list '1st-last (N_ "Last Yr 1st Est Tax Qtr")
                 (N_ "Jan 1 - Mar 31, Last year.")))
          (list->vector
           (list '2nd-last (N_ "Last Yr 2nd Est Tax Qtr")
                 (N_ "Apr 1 - May 31, Last year.")))
          (list->vector
           (list '3rd-last (N_ "Last Yr 3rd Est Tax Qtr")
		 ;; Translators: The US tax quarters are different from
		 ;; actual year's quarters! See the definition of
		 ;; tax-qtr-real-qtr-year variable above.
                 (N_ "Jun 1 - Aug 31, Last year.")))
          (list->vector
           (list '4th-last (N_ "Last Yr 4th Est Tax Qtr")
                 (N_ "Sep 1 - Dec 31, Last year."))))))

  (gnc:register-tax-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Select Accounts (none = all)")
    "d" (N_ "Select accounts.")
    (lambda () '())
    #f #t))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Suppress $0.00 values")
    "f" (N_ "$0.00 valued Tax codes won't be printed.") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not print full account names")
    "g" (N_ "Do not print all Parent account names.") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Print all Transfer To/From Accounts")
    "h" (N_ "Print all split details for multi-split transactions.") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Print TXF export parameters")
    "i" (N_ "Show TXF export parameters for each TXF code/account on report.") #f))

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc:register-tax-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Do not print T-Num:Memo data")
        "j" (N_ "Do not print T-Num:Memo data for transactions.") #f))
      (gnc:register-tax-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Do not print Action:Memo data")
        "j" (N_ "Do not print Action:Memo data for transactions.") #f)))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not print transaction detail")
    "k" (N_ "Do not print transaction detail for accounts.") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not use special date processing")
    "l" (N_ "Do not print transactions out of specified dates.") #f))

  (gnc:register-tax-option
   (gnc:make-multichoice-option
    gnc:pagename-display (N_ "Currency conversion date")
    "m" (N_ "Select date to use for PriceDB lookups.")
    'conv-to-tran-date
    (list (list->vector
           (list 'conv-to-tran-date (N_ "Nearest transaction date") (N_ "Use nearest to transaction date.")))
          (list->vector
           (list 'conv-to-report-date (N_ "Nearest report date") (N_ "Use nearest to report date.")))
    )))

  #t

  (gnc:options-set-default-section options gnc:pagename-general)

  options)

;; Render txf information
(define crlf (string #\return #\newline)) ; TurboTax seems to want these

(define txf-last-payer "")		; if same as current, inc txf-l-count
					; this only works if different
					; accounts from the same payer are
					; grouped in the accounts list
(define txf-l-count 0)		; count repeated N codes

;; stores invalid txf codes so we can list
(define txf-invalid-alist '())

(define txf-account-name "")

(define (gnc:account-get-txf account)
  (and (xaccAccountGetTaxRelated account)
       (not (equal? (gnc:account-get-txf-code account) 'N000))))

(define (gnc:account-get-txf-code account)
  (let ((code (xaccAccountGetTaxUSCode account)))
       (string->symbol (if (string-null? code) "N000" code))))

(define (get-acct-txf-info info-type acct-type code)
  (let ((categories (assv-ref
                     (list (cons ACCT-TYPE-INCOME txf-income-categories)
                           (cons ACCT-TYPE-EXPENSE txf-expense-categories)
                           (cons ACCT-TYPE-BANK txf-asset-categories)
                           (cons ACCT-TYPE-CASH txf-asset-categories)
                           (cons ACCT-TYPE-ASSET txf-asset-categories)
                           (cons ACCT-TYPE-STOCK txf-asset-categories)
                           (cons ACCT-TYPE-MUTUAL txf-asset-categories)
                           (cons ACCT-TYPE-RECEIVABLE txf-asset-categories)
                           (cons ACCT-TYPE-CREDIT txf-liab-eq-categories)
                           (cons ACCT-TYPE-LIABILITY txf-liab-eq-categories)
                           (cons ACCT-TYPE-EQUITY txf-liab-eq-categories)
                           (cons ACCT-TYPE-PAYABLE txf-liab-eq-categories))
                     acct-type))
        (get-info-fn
         (case info-type
           ((form) gnc:txf-get-form)
           ((desc) gnc:txf-get-description)
           ((pns) gnc:txf-get-payer-name-source)
           ((format) gnc:txf-get-format)
           ((multiple) gnc:txf-get-multiple)
           ((cat-key) gnc:txf-get-category-key)
           ((line) gnc:txf-get-line-data)
           ((last-yr) gnc:txf-get-last-year)
           (else #f))))
    (and categories
         get-info-fn
         (get-info-fn categories code (gnc-get-current-book-tax-type)))))

(define (gnc:account-get-txf-payer-source account)
  (let ((pns (xaccAccountGetTaxUSPayerNameSource account)))
       (string->symbol (if (string-null? pns) "none" pns))))

;; some codes require split detail, only two for now, Federal estimated tax,
;; qrtrly and state estimated tax, qrtrly
(define (txf-special-split? code)
  (member code (list 'N521 'N522)))

;; some codes require special date handling, only one for now, Federal estimated
;; tax, qrtrly
(define (txf-special-date? code)
  (member code (list 'N521)))

(define (txf-beg-bal-only? code)
  (member code (list 'N440)))   ;only one so far: F8606, IRA basis at beg of year

(define (fill-clamp-sp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 (- len 1)) " "))

(define (fill-clamp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 len)))

(define (render-header-row table heading-line-text)
  (let ((heading (gnc:make-html-text)))
       (gnc:html-text-append! heading (gnc:html-markup-b heading-line-text))
       (let ((heading-cell (gnc:make-html-table-cell/markup
                                                   "header-just-top" heading)))
            (gnc:html-table-cell-set-colspan! heading-cell 6)
            (gnc:html-table-append-row!
                   table
                   (list heading-cell)
            )
       )
  )
)

(define (render-account-detail-header-row table suppress-action-memo? format4?)
  (gnc:html-table-append-row!
       table
       (append (list (gnc:make-html-table-header-cell/markup
                          "column-heading-center" "Date"))
               (list (gnc:make-html-table-header-cell/markup
                          "column-heading-center"
                         (if (qof-book-use-split-action-for-num-field
                                                         (gnc-get-current-book))
                             "Num/Action"
                             "Num")))
               (list (gnc:make-html-table-header-cell/markup
                          "column-heading-center" "Description"))
               (list (gnc:make-html-table-header-cell/markup
                          "column-heading-center"
                         (if suppress-action-memo?
                             "Notes"
                             (if (qof-book-use-split-action-for-num-field
                                                         (gnc-get-current-book))
                                 "Notes/T-Num:Memo"
                                 "Notes/Action:Memo"))))
               (list (gnc:make-html-table-header-cell/markup
                          "column-heading-center"
                         (if format4?
                             "Capital Gains Data"
                             "Transfer To/From Account(s)")))
               (list (gnc:make-html-table-header-cell/markup
                              "column-heading-center" "Amount"))
       )
  )
)

(define (render-total-row table total-amount total-line-text tax_code?
                          transaction-details? end-bal-text total-amount-neg?
                          format4? cap-gain-sales-total cap-gain-basis-total)
  (let ((description (gnc:make-html-text))
        (total (gnc:make-html-text)))
       (if (or tax_code? transaction-details?)
           (gnc:html-text-append! description (gnc:html-markup-b
              (string-append "&nbsp; &nbsp; &nbsp; &nbsp;"
                             (if end-bal-text end-bal-text "Total For "))))
           (if (not tax_code?)
               (gnc:html-text-append! description (gnc:html-markup-b
                  "&nbsp; &nbsp; &nbsp; &nbsp;"))
           )
       )
       (gnc:html-text-append! description (gnc:html-markup-b
              total-line-text))
       (gnc:html-text-append! description (gnc:html-markup-b
              " "))
       (gnc:html-text-append! total (gnc:html-markup-b
              total-amount))
       (let ((description-cell (if (or tax_code? transaction-details?)
                                       (gnc:make-html-table-cell/markup
                                            "column-heading-right" description)
                                   (gnc:make-html-table-cell description)))
             (amount-table (gnc:make-html-table)) ;; to line up totals to details
             (cap-gains-detail-table (gnc:make-html-table))
            )
            (gnc:html-table-set-style! amount-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
            (gnc:html-table-set-style! cap-gains-detail-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
            (let ((total-amnt (if total-amount-neg?
                                  (gnc:make-html-table-cell/markup
                                             "number-cell-bot-neg" total)
                                  (gnc:make-html-table-cell/markup
                                             "number-cell-bot" total)))
                 )
                 (gnc:html-table-append-row! amount-table total-amnt)
            )
            (if format4?
                (let* ((total-cell (gnc:make-html-table-cell/markup
                                    "column-heading-right"
                                    "Capital Gains Totals: Sales Amount: "))
                       (combined-cell (gnc:make-html-table-cell))
                      )
                      (gnc:html-table-cell-append-objects!
                         total-cell
                         cap-gain-sales-total
                         " Cost/Basis: Not Available"
                      )
                      (gnc:html-table-append-row! cap-gains-detail-table
                            (append (list total-cell)))
                      (gnc:html-table-append-row! cap-gains-detail-table
                            (append (list description-cell)))
                      (gnc:html-table-cell-append-objects!
                         combined-cell
                         cap-gains-detail-table
                      )
                      (gnc:html-table-cell-set-colspan! combined-cell 5)
                      (gnc:html-table-append-row! table
                            (append (list combined-cell)
                                    (list (gnc:make-html-table-cell/markup
                                              "number-cell-bot" amount-table))))
                )
                (begin
                   (gnc:html-table-cell-set-colspan! description-cell 5)
                   (gnc:html-table-append-row! table
                            (append (list description-cell)
                                    (list (gnc:make-html-table-cell/markup
                                              "number-cell-bot" amount-table))))
                )
            ) ;; end of if
       ) ;; end of let
  ) ;; end of let
)

(define (render-txf-account account account-value d? date x? x-date
                            type code copy tax-entity-type sold-desc)
  (let* ((print-info (gnc-account-print-info account #f))
         (txf? (gnc:account-get-txf account)))
    (if (and txf?
             (not (gnc-numeric-zero-p account-value)))
        (let* ((date-str (if date
                             (gnc-print-time64 date "%m/%d/%Y")
                             #f))
               (x-date-str (if x-date
                               (gnc-print-time64 x-date "%m/%d/%Y")
                               #f))
               ;; Only formats 1,3,4,6 implemented now! Others are treated as 1.
               (format_type (get-acct-txf-info 'format type code))
               (action (if (eq? type ACCT-TYPE-INCOME)
                           (case code
                             ((N286 N488) "ReinvD")
                             (else "Income"))
                           (if (eq? type ACCT-TYPE-EXPENSE)
                               "Expense"
                               (if (or (eq? type ACCT-TYPE-BANK)
                                       (eq? type ACCT-TYPE-CASH)
                                       (eq? type ACCT-TYPE-ASSET)
                                       (eq? type ACCT-TYPE-STOCK)
                                       (eq? type ACCT-TYPE-MUTUAL)
                                       (eq? type ACCT-TYPE-RECEIVABLE))
                                   "Asset"
                                   (if (or (eq? type ACCT-TYPE-CREDIT)
                                           (eq? type ACCT-TYPE-LIABILITY)
                                           (eq? type ACCT-TYPE-PAYABLE))
                                       "Liability"
                                       (if (eq? type ACCT-TYPE-EQUITY)
                                           "Equity"
                                           ""))))))
               (category-key (get-acct-txf-info 'cat-key type code))
               (value-name (cond
                             ((string=? tax-entity-type "F1040")
                               (if (equal? "ReinvD" action)
                                   (string-append
                                     (xaccPrintAmount
                                     (gnc-numeric-neg account-value) print-info)
                                     " " txf-account-name)
                                     txf-account-name))
                             ((or (string=? tax-entity-type "F1065")
                                  (string=? tax-entity-type "F1120S"))
                               (if (string=? (xaccAccountGetName account)
                                             txf-account-name)
                                   ""
                                   (xaccAccountGetName account)))
                             (else "")))

               (value (string-append "$"  ; in txf output, income is positive; expense negative
                                          ; liabilities are positive, assets are negative;
                                          ; essentially, just reverse signs on dr's & cr's
                                     (format #f "~0,2f" (gnc-numeric-to-double
                                                          (gnc-numeric-neg
                                                            account-value)))))
          )
          ;; Based on TXF Spec of 11/30/11, V 042, and Quicken 98 output, the
          ;; fields by format are as follows, for F1040:
          ;; Format Type Fields                       Comments/Status
          ;; 0      D    T, N, C, L, X                Spec unclear, unverified
          ;; 0      S    T, N, C, L                   Spec unclear, unverified
          ;; 1      D    T, N, C, L, $, X             Spec clear & verified Q98
          ;; 1      S    T, N, C, L, $                Spec clear & verified Q98
          ;; 2      D    T, N, C, L, P, X             Spec unclear, unverified
          ;; 2      S    T, N, C, L, P                Spec unclear, unverified
          ;; 3      D    T, N, C, L, $, X             Spec clear & verified Q98
          ;; 3      S    T, N, C, L, $, P             Spec clear & verified Q98
          ;; 4      D    T, N, C, L, P, D, D, $, $, X Spec clear, unverified
          ;; 4      S    T, N, C, L, $, $             Spec unclear, unverified
          ;; 5      D    T, N, C, L, P, D, D, $, $, $ Spec unclear, unverified
          ;; 5      S    T, N, C, L, $, $, $          Spec unclear, unverified
          ;; 6      D    T, N, C, L, D, $, P, X       Spec unclear, verified Q98
          ;; 6      S    T, N, C, L, $                Spec unclear, verified Q98
          ;;
          ;; For F1040, support only formats 1, 3 and 6 (based on Q98) and
          ;;    for 4 (guessed at) for code 673 only (no date acquired or basis)
          ;; For F1065, F1120 and F1120S, support only format 1
          ;;
          (list (if x? "TD" "TS") crlf
                (symbol->string code) crlf
                (string-append "C" copy) crlf
                ;; not to be confused with Form/Sched line number: so for
                ;; example, Schedule B line 5 for 2008 has separate lines for
                ;; individual payers of ordinary dividends so these are like
                ;; sub-lines of line 5 starting with 1 for first reported payer
                ;; these apply if pns is either 'current or 'parent', but not
                ;; otherwise
                "L" (number->string txf-l-count) crlf
                (if (= format_type 4)
                    (if x?
                        (list "P" sold-desc crlf "D" crlf "D" date-str crlf
                                                                       "$" crlf)
                        (list "$" crlf))
                    '())
                (if (and d? (= format_type 6) x?)
                    (list "D" date-str crlf)
                    '())
                (case format_type
                  ((1 3 4 6) (list value crlf))
                  ((0 2 5) '()))
                (case format_type
                  ((3) (if (not x?) (list "P" txf-account-name crlf) '()))
                  ((6) (if x?
                           (if (string=? "N521" (symbol->string code))
                               (list "P" crlf) ;; Federal, no state initials
                               (list "P" txf-account-name crlf));; state initials
                           '())) ;; not detail
                  (else '()))
                (if x?
                    (cond
                      ((string=? tax-entity-type "F1040")
                        (list "X" x-date-str " "
                            (fill-clamp-sp txf-account-name 31)
                            (fill-clamp-sp action 7)
                            (fill-clamp-sp value-name 82)
                            (fill-clamp category-key 15) crlf))
                      ((or (string=? tax-entity-type "F1065")
                           (string=? tax-entity-type "F1120")
                           (string=? tax-entity-type "F1120S"))
                        (list "X"
                            (fill-clamp "" 47)
                            (fill-clamp-sp txf-account-name 41)
                            (if (string=? value-name "")
                                (list crlf)
                                (list
                                    (fill-clamp "" 41)
                                    (fill-clamp value-name 15) crlf))))
                      (else '())))
                "^" crlf))
        "")))

(define (process-currency-conversion split
                                     USD-currency
                                     account-commodity
                                     lookup-date
                                     trans-currency
                                     splt-rpt-amount
                                     print-info
                                     neg?)
;; called if account-commodity does not equal USD-currency or if
;;    trans-currency not equal USD-currency
;; if trans-currency = USD-currency and account-commodity does not equal
;;    USD-currency, use split value & transaction rate
;; if account-commodity = USD-currency and trans-currency does not equal
;;    USD-currency, use split amount & inverse of transaction rate
;; if neither trans-currency nor account-commodity = USD-currency,
;;    use split amount & pricedb lookup using lookup date
;; returns the converted amount, the conversion text, and, if the conversion
;;   price was looked up, the pricedb-lookup-price and addtitional text in
;;   a list

  (let*
    ((splt-value (if (and split
                          (or (gnc-commodity-equiv trans-currency USD-currency)
                              (gnc-commodity-equiv account-commodity
                                                                 USD-currency)))
                     (xaccSplitGetValue split)
                     100/100))
     (missing-pricedb-entry? #f)
     (pricedb-lookup-price #f)
     (pricedb-lookup-price-value (gnc-numeric-zero))
     (pricedb-lookup-price-time 0)
     (amount (if (gnc-commodity-equiv trans-currency USD-currency)
                 splt-value
                 (if (gnc-commodity-equiv account-commodity USD-currency)
                     (if neg?
                         (gnc-numeric-neg splt-rpt-amount)
                         splt-rpt-amount)
                     ;; otherwise lookup from pricedb
                     (let ((pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
                          ) ;; if we can convert to USD
                          (if (gnc-pricedb-has-prices pricedb
                                                      account-commodity
                                                      USD-currency)
                              (begin ;; do so
                                (set! missing-pricedb-entry? #f)
                                (set! pricedb-lookup-price
                                        (let ((price (gnc-pricedb-lookup-nearest-in-time64
                                          pricedb
                                          account-commodity
                                          USD-currency
                                          (time64CanonicalDayTime
                                           lookup-date))))
                                          (if (gnc-commodity-equiv account-commodity (gnc-price-get-currency price))
                                              (set! price (gnc-price-invert price)))
                                          price))
                                (set! pricedb-lookup-price-value
                                        (gnc-price-get-value
                                                          pricedb-lookup-price))
                                (set! pricedb-lookup-price-time
                                        (gnc-price-get-time64 pricedb-lookup-price))
                                (gnc-pricedb-convert-balance-nearest-price-t64
                                        pricedb
                                        (if neg?
                                            (gnc-numeric-neg splt-rpt-amount)
                                            splt-rpt-amount)
                                        account-commodity
                                        USD-currency
                     ;; Use midday as the transaction time so it matches a price
                     ;; on the same day.  Otherwise it uses midnight which will
	                 ;; likely match a price on the previous day
                                        (time64CanonicalDayTime lookup-date))
                              )
                              (begin ;; otherwise set flag and set to zero
                                (set! missing-pricedb-entry? #t)
                                (gnc-numeric-zero)
                              )))
                 )
             )
     )
     (amount (if neg?
                 (gnc-numeric-neg amount)
                 amount))
     (converted-qty (xaccPrintAmount
                       (if (gnc-commodity-equiv account-commodity USD-currency)
                           (if neg?
                                (gnc-numeric-neg splt-value)
                                splt-value
                           )
                           splt-rpt-amount
                       )
                       print-info))
     (conversion-text (if missing-pricedb-entry?
                          (string-append
                            "(Missing priceDB entry to convert "
                            (gnc-commodity-get-mnemonic account-commodity)
                            " "
                            converted-qty
                            " to USD. Go to the Tools->Price Editor dialog to enter prices. Set to zero for this report.)"
                          )
                          (string-append
                            "(Converted "
                            (if (gnc-commodity-equiv account-commodity
                                                     USD-currency)
                                (gnc-commodity-get-mnemonic trans-currency)
                                (gnc-commodity-get-mnemonic account-commodity)
                            )
                            " "
                            converted-qty
                            (if
                                (and (not (gnc-commodity-equiv account-commodity
                                                               USD-currency))
                                     (not (gnc-commodity-equiv trans-currency
                                                               USD-currency))
                                )
                                (string-append " @ PriceDB lookup rate of ")
                                (string-append
                                   " @ transaction split rate of "
                                   (xaccPrintAmount
                                       (if (not (gnc-commodity-equiv
                                                           trans-currency
                                                           USD-currency))
                                           (gnc-numeric-div
                                               100/100
                                               (xaccSplitGetSharePrice split)
                                               GNC-DENOM-AUTO
                                               (logior (GNC-DENOM-SIGFIGS 6)
                                                       GNC-RND-ROUND))
                                           (xaccSplitGetSharePrice split)
                                       )
                                       print-info)
                                   ")"
                                )
                            )
                          )
                      )
     )
     (conversion-text2 (if missing-pricedb-entry?
                           ""
                           (if (and (not (gnc-commodity-equiv account-commodity
                                                              USD-currency))
                                    (not (gnc-commodity-equiv trans-currency
                                                              USD-currency))
                               )
                               (string-append
                                 " on "
                                 (gnc-print-time64 pricedb-lookup-price-time
                                                   "%Y-%b-%d")
                                 ")"
                               )
                               ""))
     )
    )
    (list amount conversion-text pricedb-lookup-price conversion-text2)
  )
)

(define (process-transaction-multi-transfer-detail split parent
            USD-currency full-names? trans-date trans-currency acct-type
            currency-conversion-date to-date transfer-table print-amnt format_type
            split-details? tax-mode? account account-type tax-code copy
            tax-entity-type)
  (let* ((all-tran-splits (xaccTransGetSplitList parent))
         (tran-splits-to-render (- (length all-tran-splits) 1))
         (acct-print-info (gnc-account-print-info account #f))
         (output '())
         (trans-rpt-currency-total (gnc-numeric-zero)) ;;for USD-currency
         (cap-gains-detail-table (gnc:make-html-table))
         (trans-sub-heading-table (gnc:make-html-table))
         (trans-sub-table (gnc:make-html-table))
         (trans-cap-gain-sales-USD-total (gnc-numeric-zero))
         (trans-cap-gain-basis-USD-total (gnc-numeric-zero))
        )
        (if (= 4 format_type)
            (begin
               (gnc:html-table-set-style! cap-gains-detail-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "3")
                                          'attribute (list "width" "100%"))
               (gnc:html-table-set-style! trans-sub-heading-table "table"
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
               (gnc:html-table-set-style! trans-sub-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
               (gnc:html-table-append-row! cap-gains-detail-table
                      (append (list (gnc:make-html-table-cell/markup
                                           "column-heading-center" "Item"))
                              (list (gnc:make-html-table-cell/markup
                                           "column-heading-center"
                                           "Date Acquired"))
                              (list (gnc:make-html-table-cell/markup
                                           "column-heading-center"
                                           "Date Sold"))
                              (list (gnc:make-html-table-cell/markup
                                           "column-heading-center"
                                           "Sales Amount"))
                              (list (gnc:make-html-table-cell/markup
                                           "column-heading-center"
                                           "Cost/Basis"))
                      )
               )
               (gnc:html-table-append-row! trans-sub-heading-table
                      (append (list (gnc:make-html-table-header-cell/markup
                                          "column-heading-center"
                                          "Transfer To/From Account(s)"))))
            )
        )
        (map (lambda (tran-split)
             (if (not (xaccSplitEqual split tran-split #t #f #f))
                 (let* ((split-acct (xaccSplitGetAccount tran-split))
                        (split-acct-commodity
                                           (xaccAccountGetCommodity split-acct))
                        (splt-amnt (xaccSplitGetAmount tran-split))
                        (splt-amnt (if (or (eq? acct-type ACCT-TYPE-INCOME)
                                           (eq? acct-type ACCT-TYPE-CREDIT)
                                           (eq? acct-type ACCT-TYPE-PAYABLE)
                                           (eq? acct-type ACCT-TYPE-LIABILITY)
                                           (eq? acct-type ACCT-TYPE-EQUITY))
                                            splt-amnt
                                            (gnc-numeric-neg splt-amnt)))
                        (print-info (gnc-account-print-info split-acct #f))
                        (splt-curr-conv-note "")
                        (splt-curr-conv-data (list splt-amnt
                                                   splt-curr-conv-note #f ""))
                        (splt-curr-conv-data (if (and (gnc-commodity-equiv
                                                           split-acct-commodity
                                                           USD-currency)
                                                      (gnc-commodity-equiv
                                                           trans-currency
                                                           USD-currency))
                                                 splt-curr-conv-data
                                                 (process-currency-conversion
                                                     tran-split
                                                     USD-currency
                                                     split-acct-commodity
                                                     (if (equal?
                                                        currency-conversion-date
                                                             'conv-to-tran-date)
                                                         trans-date
                                                         to-date)
                                                     trans-currency
                                                     splt-amnt
                                                     print-info
                                                     (if (eq? acct-type
                                                               ACCT-TYPE-INCOME)
                                                          #f
                                                          #t))
                                             ))
                        (splt-print-amnt (car splt-curr-conv-data))
                        (splt-account-name (if full-names?
                                               (gnc-account-get-full-name
                                                                     split-acct)
                                               (xaccAccountGetName split-acct)
                                           ))
                        (cell (gnc:make-html-table-cell
                                  (if (string=? (cadr splt-curr-conv-data) "")
                                      splt-account-name
                                      (string-append splt-account-name " "
                                                     (cadr splt-curr-conv-data))
                                  )
                              ))
                       )
                       (if (caddr splt-curr-conv-data)
                           (begin
                              (gnc:html-table-cell-append-objects!
                                   cell
                                   (gnc:html-price-anchor
                                               (caddr splt-curr-conv-data) #f))
                              (gnc:html-table-cell-append-objects!
                                   cell
                                   (car (cdddr splt-curr-conv-data)))
                           )
                           #f
                       )
                       (if (and (= 4 format_type) (gnc-numeric-negative-p
                                               (xaccSplitGetAmount tran-split)))
                           (begin
                              (if tax-mode?
                                (gnc:html-table-append-row!
                                     cap-gains-detail-table
                                     (append (list (gnc:make-html-table-cell
                                                    (string-append
                                                      (xaccPrintAmount
                                                       (gnc-numeric-neg
                                                        (xaccSplitGetAmount
                                                                 tran-split))
                                                                     print-info)
                                                      " "
                                                      (gnc-commodity-get-mnemonic
                                                        split-acct-commodity))))
                                             (list (gnc:make-html-table-cell/markup
                                                    "text-cell-center"
                                                    "Not Available"))
                                             (list (gnc:make-html-table-cell/markup
                                                    "text-cell-center"
                                                  (gnc-print-time64 trans-date "%Y-%b-%d")))
                                             (list (gnc:make-html-table-cell/markup
                                                    "number-cell-bot"
                                                    (xaccPrintAmount
                                                       (gnc-numeric-neg
                                                              splt-print-amnt)
                                                              acct-print-info)))
                                             (list (gnc:make-html-table-cell/markup
                                                    "number-cell-bot"
                                                    "Not Available"))
                                     )
                                )
                                (let ((cap-gain-output
                                       (list (render-txf-account
                                               account
                                               splt-print-amnt
                                               #f
                                               trans-date
                                               #t
                                               trans-date
                                               account-type
                                               tax-code
                                               copy
                                               tax-entity-type
                                               (string-append
                                                (xaccPrintAmount
                                                 (gnc-numeric-neg
                                                  (xaccSplitGetAmount
                                                        tran-split)) print-info)
                                               " "
                                               (gnc-commodity-get-mnemonic
                                                          split-acct-commodity))
                                             )))
                                     )
                                     (set! output
                                          (if (null? output)
                                              (if (null? cap-gain-output)
                                                  '()
                                                  (list cap-gain-output))
                                              (if (null? cap-gain-output)
                                                  (list output)
                                                  (list output
                                                        cap-gain-output)))
                                     )
                                )
                              )
                              (set! trans-cap-gain-sales-USD-total
                                          (gnc-numeric-add-fixed
                                              trans-cap-gain-sales-USD-total
                                              (gnc-numeric-neg splt-print-amnt))
                              )
;                              (set! trans-cap-gain-basis-USD-total #f)
                           )
                       )
                       (let* ((splt-amnt-anchor (gnc:html-split-anchor
                                                            tran-split
                                                            splt-print-amnt))
                              (splt-amnt-anchor
                                 (if (gnc-numeric-negative-p splt-print-amnt)
                                     (gnc:make-html-table-cell/markup
                                         "number-cell-bot-neg" splt-amnt-anchor)
                                     (gnc:make-html-table-cell/markup
                                         "number-cell-bot" splt-amnt-anchor)))
                             )
                             (if (= 4 format_type)
                                 (gnc:html-table-append-row!
                                      trans-sub-table
                                      (append (list cell)
                                              (list splt-amnt-anchor
                                              )))
                                 (gnc:html-table-append-row!
                                      transfer-table
                                      (append (list cell)
                                              (list splt-amnt-anchor
                                              )))
                             )
                       )
                       (set! trans-rpt-currency-total (gnc-numeric-add-fixed
                                trans-rpt-currency-total
                                splt-print-amnt))
                 ) ;; end of let*
             ) ;; end of if
             ) ;; end of lamda
        all-tran-splits)  ;; end of map
        ;; if several splits are converted from several currencies, it is
        ;; possible that they won't add - this is a 'plug' amount to make
        ;; the converted amounts for the transaction add to zero on the report.
        (if (not (gnc-numeric-equal print-amnt trans-rpt-currency-total))
            (let* ((conversion-text (gnc:make-html-text))
                   (conversion-text-content
                                 "Multiple currency conversion differences")
                   (conversion-cell (gnc:make-html-table-cell
                                        conversion-text-content)))
                  (let* ((plug-amnt (gnc-numeric-add-fixed print-amnt
                                                           (gnc-numeric-neg
                                                     trans-rpt-currency-total)))
                         (plug-amnt (if (gnc-numeric-negative-p plug-amnt)
                                        (gnc:make-html-table-cell/markup
                                            "number-cell-bot-neg" plug-amnt)
                                        (gnc:make-html-table-cell/markup
                                            "number-cell-bot" plug-amnt)))
                        )
                        (if (= 4 format_type)
                            (gnc:html-table-append-row! trans-sub-table
                                      (append (list conversion-cell)
                                              (list plug-amnt)))
                            (gnc:html-table-append-row! transfer-table
                                      (append (list conversion-cell)
                                              (list plug-amnt)))
                        )
                  )
            )
        ) ;; end of if
        (if (= 4 format_type)
            (let* ((total-cell (gnc:make-html-table-cell/markup
                                    "column-heading-right" "Totals: "))
                  )
                  (gnc:html-table-cell-set-colspan! total-cell 3)
                  (gnc:html-table-append-row! cap-gains-detail-table
                        (append (list total-cell)
                                (list (gnc:make-html-table-cell/markup
                                                  "number-cell-bot"
                                                  (xaccPrintAmount
                                                   trans-cap-gain-sales-USD-total
                                                          acct-print-info)))
                                (list (gnc:make-html-table-cell/markup
                                                  "number-cell-bot"
                                                  "Not Available"))
                        )
                  )
                  (gnc:html-table-append-row! transfer-table
                        (append (list (gnc:make-html-table-cell
                                           cap-gains-detail-table))))
                  (if split-details?
                      (begin
                      (gnc:html-table-append-row! transfer-table
                            (append (list (gnc:make-html-table-cell
                                               trans-sub-heading-table))))
                      (gnc:html-table-append-row! transfer-table
                            (append (list (gnc:make-html-table-cell
                                               trans-sub-table))))
                      )
                  )
            )
        )
        (list trans-cap-gain-sales-USD-total
              trans-cap-gain-basis-USD-total
              output)
  ) ;; end of let*
)

;; Process transaction detail; render, if appropriate; accum account totals
(define (process-account-transaction-detail table account split-list
                split-details? full-names? currency-conversion-date to-value
                transaction-details? suppress-action-memo?
				shade-alternate-transactions? splits-period full-year? from-value
                tax-mode? show-TXF-data? USD-currency account-type
                tax-code acct-full-name acct-beg-bal-collector
                acct-end-bal-collector copy tax-entity-type)

  (let*
    ((account-commodity (xaccAccountGetCommodity account))
     (format_type (get-acct-txf-info 'format account-type tax-code))
     (payer-src (gnc:account-get-txf-payer-source account))
     (code-pns (get-acct-txf-info 'pns account-type tax-code))
     (acct-collector (gnc:make-commodity-collector))
     (acct-collector-as-dr (gnc:make-commodity-collector))
     (account-commodity-total (gnc-numeric-zero))
     (account-commodity-total-as-dr (gnc-numeric-zero))
     (account-USD-total (gnc-numeric-zero))
     (account-cap-gain-sales-USD-total (gnc-numeric-zero))
     (account-cap-gain-basis-USD-total (gnc-numeric-zero))
     (account-desc (string-append
                      acct-full-name
                      (if (gnc-commodity-equiv account-commodity USD-currency)
                          ""
                          (string-append " (Account Commodity: "
                                  (gnc-commodity-get-mnemonic account-commodity)
                                  ")"))
                      (if show-TXF-data?
                          (let* ((pns (if (or (eq? 'parent code-pns)
                                              (eq? 'current code-pns))
                                          (if (eq? 'parent payer-src)
                                              "Name Source is Parent"
                                              "Name Source is Current")
                                          ""))
                                 (line (if (and (= format_type 3)
                                                (or (eq? code-pns 'parent)
                                                    (eq? code-pns 'current)))
                                           (string-append "Item "
                                                   (number->string txf-l-count))
                                           ""))
                                )
                                (if (eq? pns "")
                                    (if (eq? line "")
                                        ""
                                        (string-append
                                          " (TXF Parameter: " line ")"))
                                    (if (eq? line "")
                                        (string-append
                                          " (TXF Parameter: " pns ")")
                                        (string-append
                                          " (TXF Parameters: " pns ", "
                                          line ")")))
                          )
                          "")))
     (print-info (gnc-account-print-info account #f))
     (shade-this-line? #f)
     (output '())
     (account-name (if full-names? acct-full-name
                                   (xaccAccountGetName account)))
     (beg-bal-acct-curr (gnc-numeric-zero))
     (beg-bal-rpt-amount (gnc-numeric-zero))
    )
    (acct-collector 'reset #f #f)  ;initialize to zero for this account
    (acct-collector-as-dr 'reset #f #f)  ;initialize to zero for this account
    (if (and transaction-details? tax-mode?)
        (begin ;; print account header for all accts
          (render-header-row table (string-append
                                    "&nbsp; &nbsp; &nbsp; &nbsp;" account-desc))
          (render-account-detail-header-row table suppress-action-memo?
                                                        (if (= format_type 4) #t #f))
        ))
    (if (not (or (eq? account-type ACCT-TYPE-INCOME)
                 (eq? account-type ACCT-TYPE-EXPENSE)))
        (begin ;; set beginning amount for B/S accts
          (set! beg-bal-acct-curr (gnc-numeric-add-fixed beg-bal-acct-curr
                                       (cadr (acct-beg-bal-collector
                                               'getpair account-commodity #f))))
          (set! beg-bal-rpt-amount (if (or (eq? account-type ACCT-TYPE-CREDIT)
                                           (eq? account-type ACCT-TYPE-PAYABLE)
                                           (eq? account-type ACCT-TYPE-LIABILITY)
                                           (eq? account-type ACCT-TYPE-EQUITY))
                                   (gnc-numeric-neg beg-bal-acct-curr)
                                   beg-bal-acct-curr))
          (acct-collector 'add account-commodity beg-bal-rpt-amount) ;set beg bal
          (acct-collector-as-dr 'add account-commodity beg-bal-acct-curr)
          (if (or (not (gnc-numeric-zero-p beg-bal-acct-curr))
                  (> (length split-list) 0)
                  (not (gnc-numeric-zero-p (cadr (acct-end-bal-collector
                                               'getpair account-commodity #f))))
              );; B/S acct with either beg bal, splits or end bal
              ;; print beg bal line for B/S accts
              (let* ((curr-conv-note "")
                     (curr-conv-data (list beg-bal-rpt-amount
                                                          curr-conv-note #f ""))
                     (curr-conv-data (if (gnc-commodity-equiv
                                                 account-commodity USD-currency)
                                         curr-conv-data
                                         (process-currency-conversion
                                           #f
                                           USD-currency
                                           account-commodity
                                           (if (equal? currency-conversion-date
                                                             'conv-to-tran-date)
                                               (gnc:time64-previous-day
                                                                     from-value)
                                               to-value)
                                           account-commodity ;; force price lookup
                                           beg-bal-rpt-amount
                                           print-info
                                           (if (or
                                            (eq? account-type ACCT-TYPE-CREDIT)
                                            (eq? account-type ACCT-TYPE-PAYABLE)
                                            (eq? account-type ACCT-TYPE-LIABILITY)
                                            (eq? account-type ACCT-TYPE-EQUITY))
                                               #t
                                               #f))
                                     )
                     )
                     (print-amnt (car curr-conv-data))
                     (account-beg-amnt (xaccPrintAmount print-amnt print-info))
                     (curr-conv-note (cadr curr-conv-data))
                     (curr-conv-data (if (and (txf-beg-bal-only? tax-code)
                                              (not transaction-details?))
                                         (list print-amnt curr-conv-note #f "")
                                         curr-conv-data))
                     (amnt-acct-curr (xaccPrintAmount beg-bal-acct-curr
                                                                    print-info))
                     (account-beg-bal-line-text
                        (if (and (txf-beg-bal-only? tax-code)
                                 (not transaction-details?))
                            ""
                            (string-append "Balance on "
                                           (gnc-print-time64
                                            (gnc:time64-previous-day from-value)
                                            "%Y-%b-%d")
                                         (if (string=? curr-conv-note "")
                                             ":"
                                             (string-append  " " curr-conv-note)
                                         )
                            )
                        )
                     )
                    )
                    (if (and transaction-details? tax-mode?)
                        ;; print beg bal line line
                        (let ((beg-bal-cell (gnc:make-html-table-cell/markup
                                                     "number-cell-bot"
                                                     account-beg-bal-line-text))
                              (beg-bal-neg?
                                       (if (gnc-numeric-negative-p print-amnt)
                                           #t
                                           #f))
                              ;;to line up to details
                              (amount-table (gnc:make-html-table)))
                             (if (caddr curr-conv-data)
                                 (begin
                                   (gnc:html-table-cell-append-objects!
                                        beg-bal-cell
                                        (gnc:html-price-anchor
                                                     (caddr curr-conv-data) #f))
                                   (gnc:html-table-cell-append-objects!
                                        beg-bal-cell
                                        (string-append
                                              (car (cdddr curr-conv-data)) ":"))
                                 )
                                 #f)
                             (gnc:html-table-cell-set-colspan! beg-bal-cell 5)
                             (gnc:html-table-set-style! amount-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
                             (let ((beg-bal-amnt
                                       (if beg-bal-neg?
                                           (gnc:make-html-table-cell/markup
                                                     "number-cell-bot-neg"
                                                     account-beg-amnt)
                                           (gnc:make-html-table-cell/markup
                                                     "number-cell-bot"
                                                     account-beg-amnt)))
                                  )
                                  (gnc:html-table-append-row!
                                                      amount-table beg-bal-amnt)
                             )
                             (gnc:html-table-append-row/markup!
                                        table
                                        (if shade-alternate-transactions?
                                            (if shade-this-line?
                                                (begin
                                                  (set! shade-this-line? #f)
                                                  "tran-detail-shade"
                                                )
                                                (begin
                                                  (set! shade-this-line? #t)
                                                  "tran-detail"
                                                ))
                                            "tran-detail")
                                        (append (list beg-bal-cell)
                                                (list
                                                (gnc:make-html-table-cell/markup
                                                      "number-cell-bot"
                                                      amount-table))))
                        )
                    );; end of if
                    (set! account-USD-total (gnc-numeric-add-fixed
                                                  account-USD-total print-amnt))
              );; end of let*
          );; end of if
        );; end of begin
        #f
    ) ;; end of if
    (if (and (> (length split-list) 0)
             (not (txf-beg-bal-only? tax-code)))
      (set! output
        (map (lambda (split)
           (let* ((parent (xaccSplitGetParent split))
                  (trans-date (xaccTransGetDate parent))
                  ;; TurboTax 1999 and 2000 ignore dates after Dec 31
                  (fudge-date (if splits-period
                                  (if (and full-year?
                                           (< to-value trans-date))
                                      to-value
                                      trans-date)
                                  trans-date))
                  (notes (xaccTransGetNotes parent))
                  (action (if suppress-action-memo?
                              ""
                              (gnc-get-action-num  parent split)))
                  (memo  (if suppress-action-memo?
                             ""
                             (xaccSplitGetMemo split)))
                  (action-memo (if (and (string=? action "") (string=? memo ""))
                                   ""
                                   (begin
                                     (string-append "/" action
                                       (if (string=? memo "")
                                           ""
                                           (string-append ":" memo))))))
                  (notes-act-memo (string-append notes action-memo))
                  (trans-currency (xaccTransGetCurrency parent))
                  (splt-amount (xaccSplitGetAmount split))
                  (splt-amount-is-dr? (if (gnc-numeric-positive-p splt-amount)
                                          #t
                                          #f))
                  (splt-rpt-amount (if (or (eq? account-type ACCT-TYPE-INCOME)
                                           (eq? account-type ACCT-TYPE-CREDIT)
                                           (eq? account-type ACCT-TYPE-PAYABLE)
                                           (eq? account-type ACCT-TYPE-LIABILITY)
                                           (eq? account-type ACCT-TYPE-EQUITY))
                                       (gnc-numeric-neg splt-amount)
                                       splt-amount))
                  (curr-conv-note "")
                  (curr-conv-data (list splt-rpt-amount curr-conv-note #f ""))
                  (curr-conv-data (if (and (gnc-commodity-equiv
                                                 account-commodity USD-currency)
                                           (gnc-commodity-equiv trans-currency
                                                                USD-currency))
                                      curr-conv-data
                                      (process-currency-conversion
                                         split
                                         USD-currency
                                         account-commodity
                                         (if (equal? currency-conversion-date
                                                     'conv-to-tran-date)
                                             trans-date
                                             to-value)
                                         trans-currency
                                         splt-rpt-amount
                                         print-info
                                         (if (or
                                           (eq? account-type ACCT-TYPE-INCOME)
                                           (eq? account-type ACCT-TYPE-CREDIT)
                                           (eq? account-type ACCT-TYPE-PAYABLE)
                                           (eq? account-type ACCT-TYPE-LIABILITY)
                                           (eq? account-type ACCT-TYPE-EQUITY))
                                             #t
                                             #f))))
                  (print-amnt (car curr-conv-data))
                  (print-amnt-is-dr? (if (gnc-numeric-positive-p print-amnt)
                                         #t
                                         #f))
                  (curr-conv-note (cadr curr-conv-data))
                  (other-account (xaccSplitGetAccount
                                 (xaccSplitGetOtherSplit split)))
                  (other-account-name (if (null? other-account)
                                          "Split Transaction"
                                          (if full-names?
                                              (gnc-account-get-full-name
                                                                  other-account)
                                              (xaccAccountGetName other-account)
                                          )))
                  (cap-gain-txf-output '())
                  ;; use tables within cells for all items so that row lines up
                  ;; properly
                  (date-table (gnc:make-html-table))
                  (num-table (gnc:make-html-table))
                  (desc-table (gnc:make-html-table))
                  (notes-table (gnc:make-html-table))
                  (transfer-table (gnc:make-html-table))
                  (amount-table (gnc:make-html-table))
                 ) ;;end of let* variable definitions
                 (acct-collector 'add account-commodity
                     (if (or (eq? account-type ACCT-TYPE-INCOME)
                             (eq? account-type ACCT-TYPE-CREDIT)
                             (eq? account-type ACCT-TYPE-PAYABLE)
                             (eq? account-type ACCT-TYPE-LIABILITY)
                             (eq? account-type ACCT-TYPE-EQUITY))
                         (gnc-numeric-neg splt-amount)
                         splt-amount))
                 (acct-collector-as-dr 'add account-commodity splt-amount)
                 (set! account-USD-total (gnc-numeric-add-fixed
                                              account-USD-total print-amnt))
                 ;; for capital gains format 4, we need to go get data from the
                 ;; transaction-multi-transfer-detail routine for TXF output and
                 ;; to accumulate capital gains totals for account-, tax-code-,
                 ;; and form-level totals even when not printing transaction
                 ;; details and/or Transfer To/From Accounts
                 (if (or (and transaction-details? tax-mode?
                                        (null? other-account) split-details?)
                         (= 4 format_type)
                     )
                     (let ((cap-gain-data
                                      (process-transaction-multi-transfer-detail
                                             split
                                             parent
                                             USD-currency
                                             full-names?
                                             trans-date
                                             trans-currency
                                             account-type
                                             currency-conversion-date
                                             to-value
                                             transfer-table
                                             print-amnt
                                             format_type
                                             split-details?
                                             tax-mode?
                                             account
                                             account-type
                                             tax-code
                                             copy
                                             tax-entity-type))
                          )
                          (set! account-cap-gain-sales-USD-total
                                          (gnc-numeric-add-fixed
                                              account-cap-gain-sales-USD-total
                                              (car cap-gain-data))
                          )
                          (set! account-cap-gain-basis-USD-total
                                          (gnc-numeric-add-fixed
                                              account-cap-gain-basis-USD-total
                                              (cadr cap-gain-data))
                          )
                          (set! cap-gain-txf-output (caddr cap-gain-data))
                     ))
                 (if (and transaction-details? tax-mode?)
                     (begin
                       (gnc:html-table-set-style! date-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0"))
                       (gnc:html-table-append-row!
                            date-table
                            (gnc:make-html-table-cell/markup
                                         "date-cell"
                                         (gnc-print-time64 trans-date "%Y-%b-%d")))
                       (gnc:html-table-set-style! num-table "table" 
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0"))
                       (gnc:html-table-append-row!
                            num-table
                            (gnc:make-html-table-cell (gnc-get-num-action
                                                                parent split)))
                       (gnc:html-table-set-style! desc-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0"))
                       (gnc:html-table-append-row!
                            desc-table
                            (gnc:make-html-table-cell
                                            (xaccTransGetDescription parent)))
                       (gnc:html-table-set-style! notes-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0"))
                       (gnc:html-table-append-row!
                            notes-table
                            (gnc:make-html-table-cell notes-act-memo))
                       (gnc:html-table-set-style! transfer-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
                       (if (not (and (null? other-account) split-details?))
                           (let ((cell (gnc:make-html-table-cell
                                         (if (string=? (cadr curr-conv-data) "")
                                             other-account-name
                                             (string-append other-account-name
                                                       " "
                                                       (cadr curr-conv-data)))))
                                )
                                (if (caddr curr-conv-data)
                                    (begin
                                      (gnc:html-table-cell-append-objects!
                                           cell
                                           (gnc:html-price-anchor
                                                 (caddr curr-conv-data) #f))
                                      (gnc:html-table-cell-append-objects!
                                           cell
                                           (car (cdddr curr-conv-data)))
                                    )
                                    #f)
                                (gnc:html-table-append-row!
                                     transfer-table
                                     cell)
                           )
                           (if (not (string=? (cadr curr-conv-data) ""))
                               (let ((conversion-cell
                                          (gnc:make-html-table-cell/markup
                                              "number-cell-bot" curr-conv-note))
                                    )
                                    (if (caddr curr-conv-data)
                                        (begin
                                          (gnc:html-table-cell-append-objects!
                                               conversion-cell
                                               (gnc:html-price-anchor
                                                    (caddr curr-conv-data) #f))
                                          (gnc:html-table-cell-append-objects!
                                               conversion-cell
                                               (car (cdddr curr-conv-data)))
                                        )
                                        #f)
                                    (gnc:html-table-cell-set-colspan!
                                                              conversion-cell 2)
                                    (gnc:html-table-append-row!
                                         transfer-table
                                         conversion-cell)
                               )
                           )
                       )
                       (gnc:html-table-set-style! amount-table "table"
                                          'attribute (list "border" "0")
                                          'attribute (list "cellspacing" "0")
                                          'attribute (list "cellpadding" "0")
                                          'attribute (list "width" "100%"))
                       (let* ((splt-amnt-anchor (gnc:html-split-anchor
                                                              split print-amnt))
                              (splt-amnt-anchor
                                (if (gnc-numeric-negative-p print-amnt)
                                    (gnc:make-html-table-cell/markup
                                         "number-cell-bot-neg" splt-amnt-anchor)
                                    (gnc:make-html-table-cell/markup
                                         "number-cell-bot" splt-amnt-anchor)))
                             )
                             (gnc:html-table-append-row!
                                                  amount-table splt-amnt-anchor)
                       )
                       ;; print transaction line
                       (gnc:html-table-append-row/markup!
                            table
                            (if shade-alternate-transactions?
                                (if shade-this-line?
                                    (begin
                                      (set! shade-this-line? #f)
                                      "tran-detail-shade"
                                    )
                                    (begin
                                      (set! shade-this-line? #t)
                                      "tran-detail"
                                    ))
                                "tran-detail")
                            (append (list (gnc:make-html-table-cell/markup
                                               "date-cell" date-table))
                                    (list (gnc:make-html-table-cell
                                               num-table))
                                    (list (gnc:make-html-table-cell
                                               desc-table))
                                    (list (gnc:make-html-table-cell
                                               notes-table))
                                    (list (gnc:make-html-table-cell/markup
                                               "just-bot" transfer-table))
                                    (list (gnc:make-html-table-cell/markup
                                               "number-cell-bot" amount-table))
                            )
                       )
                     ) ;; end of begin
                 ) ;; end of if
                 ;; for quarterly estimated tax payments, we need to go
                 ;; get data from splits for TXF output
                 (if (and (txf-special-split? tax-code) (not tax-mode?))
                     (list cap-gain-txf-output
                           (render-txf-account
                                       account
                                       (if (or (and print-amnt-is-dr?
                                                    splt-amount-is-dr?)
                                               (and (not print-amnt-is-dr?)
                                                    (not splt-amount-is-dr?))
                                           )
                                           print-amnt
                                           (gnc-numeric-neg print-amnt))
                                       #t fudge-date  #t trans-date
                                       account-type tax-code copy
                                       tax-entity-type #f)
                     )
                     cap-gain-txf-output
                 )
           ) ;;end of let*
           ) ;;end of lambda
        split-list) ;;end of map
      ) ;; end of set!
    ) ;; end of if
    ;; print account totals
    (set! account-commodity-total (gnc-numeric-add-fixed account-commodity-total
                         (cadr (acct-collector 'getpair account-commodity #f))))
    (set! account-commodity-total-as-dr (gnc-numeric-add-fixed
                                                   account-commodity-total-as-dr
                   (cadr (acct-collector-as-dr 'getpair account-commodity #f))))
    (if tax-mode?
        (let* ((amnt-acct-curr (xaccPrintAmount account-commodity-total
                                                print-info))
               (account-total-amount (xaccPrintAmount account-USD-total
                                                print-info))
               (account-cap-gain-sales-total-amount
                                     (xaccPrintAmount
                                                account-cap-gain-sales-USD-total
                                                print-info))
               (account-cap-gain-basis-total-amount
                                     (xaccPrintAmount
                                                account-cap-gain-basis-USD-total
                                                print-info))
               (account-total-line-text
                  (string-append (if transaction-details?
                                     "Account: "
                                     "")
                                 account-name
                                 (if (not (gnc-commodity-equiv account-commodity
                                                               USD-currency))
                                     (string-append " ("
                                                    amnt-acct-curr
                                                    "  In "
                                                    (gnc-commodity-get-mnemonic
                                                              account-commodity)
                                                    ") ")
                                     "")))
              )
              (render-total-row table
                                account-total-amount
                                account-total-line-text
                                #f
                                transaction-details?
                                (if (or (eq? account-type ACCT-TYPE-INCOME)
                                        (eq? account-type ACCT-TYPE-EXPENSE))
                                    #f
                                    (if (txf-beg-bal-only? tax-code)
                                        (string-append "Balance on "
                                                       (gnc-print-time64
                                                        (gnc:time64-previous-day
                                                         from-value)
                                                        "%Y-%b-%d")
                                           " For "
                                        )
                                        (string-append "Balance on "
                                           (gnc-print-time64 to-value "%Y-%b-%d")
                                           " For "
                                        )
                                    )
                                )
                                (if (gnc-numeric-negative-p account-USD-total)
                                    #t
                                    #f)
                                (if (= format_type 4)
                                    #t
                                    #f)
                                account-cap-gain-sales-total-amount
                                account-cap-gain-basis-total-amount
              )
        ) ;; end of let*
    ) ;; end of if
    (list account-USD-total
          output
          (if (or (and (gnc-numeric-positive-p account-USD-total)
                       (gnc-numeric-positive-p account-commodity-total-as-dr))
                  (and (gnc-numeric-negative-p account-USD-total)
                       (gnc-numeric-negative-p account-commodity-total-as-dr)))
              account-USD-total
              (gnc-numeric-neg account-USD-total))
          account-cap-gain-sales-USD-total
          account-cap-gain-basis-USD-total
    )
  ) ;;end of let*
)

;; Recursivly validate children if parent is not a tax account.
;; Don't check children if parent is valid.
;; Returns the Parent if a child or grandchild is valid.
(define (validate accounts)
  (filter (lambda (a)
            (if (xaccAccountGetTaxRelated a)
                #t
                ;; check children
                (if (null? (validate (gnc-account-get-descendants a)))
                    #f
                    #t)))
          accounts))

(define (generate-tax-schedule report-name
                             report-description
                             report-obj
                             tax-mode?
                             file-name)

  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (define tax-entity-type (gnc-get-current-book-tax-type))

  ;; Returns the line number string for the first pair whose year is less than
  ;; year argument. Assumes pairs are in year descending order. If year of last
  ;; pair is greater than year argument, no line info is returned.
  (define get-line-info
    (lambda (year line-list)
      (cond
        ((not line-list) #f)
        ((null? line-list) #f)
        ((> (caar line-list) (string->number year))
            (get-line-info year (cdr line-list)))
        ((<= (caar line-list) (string->number year)) (cadar line-list)))))

  ;; List of entries, each containing a form, form copy number, form line
  ;; number, tax-code (as string), account name, account, and, to avoid having
  ;; to fetch again later, account type and tax-code as symbol. Only accounts
  ;; that are tax related, with a tax code that is valid for the tax-entity-type
  ;; and account type are put on list, along with those assigned code N000.
  ;; Accounts that are not tax-related and have a tax code or are tax-related
  ;; and have an invalid tax code are put on an error list. Codes N438 and N440
  ;; have special processing: if an asset account is assigned to either of these
  ;; two codes, an additional 'form-line-acct' entry is created for the other
  ;; code so that either both codes are represented or neither.
  (define (make-form-line-acct-list accounts tax-year)
     (map (lambda (account)
            (let* ((account-name (gnc-account-get-full-name account))
                   (children (gnc-account-get-children account))
                   (tax-related (xaccAccountGetTaxRelated account))
                   (tax-code (xaccAccountGetTaxUSCode account))
                   (tax-code-sym (string->symbol tax-code))
                   (type (xaccAccountGetType account))
                   (form (get-acct-txf-info 'form type tax-code-sym))
                   (last-year (get-acct-txf-info 'last-yr type tax-code-sym))
                  )
              (if (not (string-null? tax-code))
                (if (not (or (null? tax-entity-type)
                             (string=? tax-entity-type "")
                             (string=? tax-entity-type "Other")))
                  (if tax-related
                    (if (or (not (eqv? form #f))
                            (string=? tax-code "N000"))
                     (if (or (not last-year)
                             (if (and last-year
                                      (> (string->number tax-year) last-year))
                                 #f
                                 #t))
                      (let* ((form (if form form "")) ;; needed for "N000'
                             (copy (number->string
                                      (xaccAccountGetTaxUSCopyNumber account)))
                             (line (get-acct-txf-info 'line type tax-code-sym))
                             (line (if line
                                       (get-line-info tax-year line)
                                       ""))
                             (line (if line
                                       line ;; this might be a tax year before
                                       "")) ;; earliest available year line pair
                             (form-line-acct (list (list form)
                                                   (list copy)
                                                   (list line)
                                                   (list tax-code)
                                                   (list account-name)
                                                   (list account)
                                                   (list type)
                                                   (list tax-code-sym))))
                            (set! selected-accounts-sorted-by-form-line-acct
                                    (append (list form-line-acct)
                                    selected-accounts-sorted-by-form-line-acct))
                            (if (or (string=? tax-code "N438")
                                    (string=? tax-code "N440"))
                                (let* ((tax-code2 (if (string=? tax-code
                                                               "N438")
                                                      "N440" "N438"))
                                       (tax-code2-sym (string->symbol
                                                                     tax-code2))
                                       (line2 (get-acct-txf-info 'line
                                                            type tax-code2-sym))
                                       (line2 (if line2
                                                  (get-line-info tax-year line2)
                                                  ""))
                                       (line2 (if line2
                                                  line2
                                                  ""))
                                      )
                                      (set!
                                      selected-accounts-sorted-by-form-line-acct
                                         (append (list
                                                   (list (list form)
                                                         (list copy)
                                                         (list line2)
                                                         (list tax-code2)
                                                         (list account-name)
                                                         (list account)
                                                         (list type)
                                                         (list tax-code2-sym)))
                                    selected-accounts-sorted-by-form-line-acct))
                                )
                            );; end if
                      );; end let*
                      (begin
                        (set! txf-invalid-alist (assoc-set!
                                 txf-invalid-alist
                                 tax-code
                                 (list "Set as tax-related, but assigned tax code no longer valid for tax year"
                                       account-name form account)))
                        selected-accounts-sorted-by-form-line-acct)
                     );; end if
                     (begin
                         (set! txf-invalid-alist (assoc-set!
                                 txf-invalid-alist
                                 tax-code
                                 (list "Set as tax-related, tax code assigned for different tax entity type"
                                       account-name form account)))
                        selected-accounts-sorted-by-form-line-acct)
                    )
                    (begin ;; not tax related
                      (if (or (not (eqv? form #f))
                              (string=? tax-code "N000"))
                          (set! txf-invalid-alist (assoc-set!
                                   txf-invalid-alist
                                   tax-code
                                   (list "Set as not tax-related, but tax code assigned"
                                         account-name form account)))
                          (set! txf-invalid-alist (assoc-set!
                                   txf-invalid-alist
                                   tax-code
                                   (list "Set as not tax-related, tax code assigned for different tax entity type"
                                         account-name form account)))
                      )
                    selected-accounts-sorted-by-form-line-acct)
                  )
                  (begin;; 'Other' tax entity type selected - message on report
                    selected-accounts-sorted-by-form-line-acct)
                )
                (begin;; no tax code
                  (if (not (or (null? tax-entity-type)
                               (string=? tax-entity-type "")
                               (string=? tax-entity-type "Other")))
                    (if tax-related
                      (begin
                        (set! txf-invalid-alist (assoc-set!
                               txf-invalid-alist
                               "None"
                               (list "Set as tax-related, no tax code assigned"
                                     account-name form account)))
                         selected-accounts-sorted-by-form-line-acct)
                      (begin ;; not tax related - skip for report
                      selected-accounts-sorted-by-form-line-acct)
                    )
                    (begin ;; 'Other' tax entity type selected - message on report
                    selected-accounts-sorted-by-form-line-acct)
                  )
                  selected-accounts-sorted-by-form-line-acct)
               );; end of if
               (if (not (null? children))
                        (make-form-line-acct-list children tax-year)
                        selected-accounts-sorted-by-form-line-acct
               )
            );; end let*
          );; end lambda
      accounts)
  )

  ;; The first elements of the lists, form and copy, are compared as strings as
  ;; are the last parts, tax-code and account name. The line number is
  ;; decomposed into numeric and alpha parts and the sub-parts are compared.
  ;; This is so that, for example, line number '9a' sorts before '12b'.
  (define (form-line-acct-less a b)
     (let ((string-a-first (string-append (caar a) " " (caadr a)))
           (string-b-first (string-append (caar b) " " (caadr b))))
          (if (string<? string-a-first string-b-first) ;; consider form and copy
              #t
              (if (string>? string-a-first string-b-first)
                  #f
                  ;; consider line number looking at sub-parts, if necessary
                  (let* ((get-parts (lambda (str)
                          (let ((prior-char-num? #f)
                                (string-part "")
                                (lst '()))
                               (map (lambda (char)
                                 (if (char-numeric? char)
                                     (if prior-char-num?
                                        (begin
                                          (set! string-part (string-append
                                                     string-part (string char)))
                                          (append lst (list
                                                  (string->number string-part)))
                                        )
                                        (begin
                                          (if (string=? string-part "")
                                              #f
                                              (set! lst (append lst
                                                      (list string-part))))
                                          (set! string-part (string char))
                                          (set! prior-char-num? #t)
                                          (append lst (list (string->number
                                                              (string char))))
                                        ))
                                     (if prior-char-num?
                                        (begin
                                          (if (string=? string-part "")
                                              #f
                                              (set! lst (append lst (list
                                                (string->number string-part)))))
                                          (set! string-part (string char))
                                          (set! prior-char-num? #f)
                                          (append lst (list (string char)))
                                        )
                                        (begin
                                          (set! string-part (string-append
                                                     string-part (string char)))
                                          (append lst (list string-part))
                                        ))))
                               (string->list str))
                          )))
                         (a-line-list (get-parts (car (caddr a))))
                         (b-line-list (get-parts (car (caddr b))))
                         (a-line-list (if (null? a-line-list)
                                          a-line-list
                                          (list-ref a-line-list
                                                   (- (length a-line-list) 1))))
                         (b-line-list (if (null? b-line-list)
                                          b-line-list
                                          (list-ref b-line-list
                                                   (- (length b-line-list) 1))))
                        )
                        (letrec
                         ((line-list<? (lambda (ls1 ls2)
                          (if (null? ls2)
                              #f
                              (if (null? ls1)
                                  #t
                                  (if (integer? (car ls1))
                                      (if (integer? (car ls2))
                                          (if (< (car ls1) (car ls2))
                                              #t
                                              (if (> (car ls1) (car ls2))
                                                  #f
                                                  (line-list<? (cdr ls1)
                                                                    (cdr ls2))))
                                          (if (string<?
                                                    (number->string (car ls1))
                                                                      (car ls2))
                                              #t
                                              (if (string>?
                                                    (number->string (car ls1))
                                                                      (car ls2))
                                                  #f
                                                  (line-list<? (cdr ls1)
                                                                   (cdr ls2)))))
                                      (if (integer? (car ls2))
                                          (if (string<? (car ls1)
                                                     (number->string (car ls2)))
                                              #t
                                              (if (string>? (car ls1)
                                                     (number->string (car ls2)))
                                                  #f
                                                  (line-list<? (cdr ls1)
                                                                    (cdr ls2))))
                                          (if (string<? (car ls1) (car ls2))
                                              #t
                                              (if (string>? (car ls1) (car ls2))
                                                  #f
                                                  (line-list<? (cdr ls1)
                                                             (cdr ls2)))))))))))
                        (if (line-list<? a-line-list b-line-list)
                            #t
                            (if (line-list<? b-line-list a-line-list)
                                #f
                                ;; consider rest of line all together
                                (let ((string-a-rest (string-append
                                         (car (caddr a)) " " (caar (cdddr a))
                                                        " " (caadr (cdddr a))))
                                      (string-b-rest (string-append
                                         (car (caddr b)) " " (caar (cdddr b))
                                                        " " (caadr (cdddr b)))))
                                     (if (string<? string-a-rest string-b-rest)
                                         #t
                                         #f
                                     ))))
                        )
                  )
              )
          )
     )
  )

  (define USD-currency (gnc-commodity-table-lookup
                          (gnc-commodity-table-get-table (gnc-get-current-book))
                          "CURRENCY"
                          "USD"))

  (gnc:report-starting reportname)
  (let* ((from-value (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general "From")))
         (to-value (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general "To"))))
         (alt-period (get-option gnc:pagename-general "Alternate Period"))
         (selected-style-sheet (get-option gnc:pagename-general "Stylesheet"))
         (suppress-0? (get-option gnc:pagename-display
                                 "Suppress $0.00 values"))
         (full-names? (not (get-option gnc:pagename-display
                                 "Do not print full account names")))
         (split-details? (get-option gnc:pagename-display
                                 "Print all Transfer To/From Accounts"))
         (show-TXF-data? (get-option gnc:pagename-display
                                 "Print TXF export parameters"))
         (transaction-details? (not (get-option gnc:pagename-display
                                 "Do not print transaction detail")))
         (no-special-dates? (get-option gnc:pagename-display
                                 "Do not use special date processing"))
         (suppress-action-memo? (if (gnc:lookup-option
                                        (gnc:report-options report-obj)
                                            gnc:pagename-display
                                            "Do not print Action:Memo data")
                                    (get-option gnc:pagename-display
                                     "Do not print Action:Memo data")
                                    (get-option gnc:pagename-display
                                     "Do not print T-Num:Memo data")))
         (shade-alternate-transactions? #t)
         (currency-conversion-date (get-option gnc:pagename-display
                                 "Currency conversion date"))
         (user-sel-accnts (get-option gnc:pagename-accounts
                                 "Select Accounts (none = all)"))
         (valid-user-sel-accnts (validate user-sel-accnts))
         ;; If no selected accounts, check all.
         (selected-accounts (if (not (null? user-sel-accnts))
                                valid-user-sel-accnts
                                (validate (reverse
                                           (gnc-account-get-children-sorted
                                            (gnc-get-current-root-account))))))

         (work-to-do 0)
         (work-done 0)

         ;; Alternate dates are relative to from-date
         (from-date (gnc-localtime from-value))
         (from-value (gnc:time64-start-day-time
                      (let ((bdtm from-date))
                        (if (member alt-period
                                    '(last-year 1st-last 2nd-last
                                                3rd-last 4th-last))
                            (set-tm:year bdtm (- (tm:year bdtm) 1)))
                        (or (eq? alt-period 'from-to)
                            (set-tm:mday bdtm 1))
                        (if (< (gnc:date-get-year bdtm)
                               tax-qtr-real-qtr-year)
                            (case alt-period
                              ((1st-est 1st-last last-year) ; Jan 1
                               (set-tm:mon bdtm 0))
                              ((2nd-est 2nd-last) ; Apr 1
                               (set-tm:mon bdtm 3))
                              ((3rd-est 3rd-last) ; Jun 1
                               (set-tm:mon bdtm 5))
                              ((4th-est 4th-last) ; Sep 1
                               (set-tm:mon bdtm 8)))
                            ;; Tax quaters equal Real quarters
                            (case alt-period
                              ((1st-est 1st-last last-year) ; Jan 1
                               (set-tm:mon bdtm 0))
                              ((2nd-est 2nd-last) ; Apr 1
                               (set-tm:mon bdtm 3))
                              ((3rd-est 3rd-last) ; Jul 1
                               (set-tm:mon bdtm 6))
                              ((4th-est 4th-last) ; Oct 1
                               (set-tm:mon bdtm 9))))
                        (set-tm:isdst bdtm -1)
                        (gnc-mktime bdtm))))

         (to-value (gnc:time64-end-day-time
                    (let ((bdtm from-date))
                      (if (member alt-period
                                  '(last-year 1st-last 2nd-last
                                              3rd-last 4th-last))
                          (set-tm:year bdtm (- (tm:year bdtm) 1)))
                      ;; Bug! Above subtracts two years, should only be one!
                      ;; The exact same code, in from-value, further above,
                      ;;   only subtraces one!  Go figure!
                      ;; So, we add one back below!
;; comment - could it be because from-date coming in has already had 1
;; subtracted in setting from-value?
                      (if (member alt-period
                                  '(last-year 1st-last 2nd-last
                                              3rd-last 4th-last))
                          (set-tm:year bdtm (+ (tm:year bdtm) 1)))
                      (or (eq? alt-period 'from-to)
                          (set-tm:mday bdtm 31))
                      (if (< (gnc:date-get-year bdtm) tax-qtr-real-qtr-year)
                          (case alt-period
                            ((1st-est 1st-last) ; Mar 31
                             (set-tm:mon bdtm 2))
                            ((2nd-est 2nd-last) ; May 31
                             (set-tm:mon bdtm 4))
                            ((3rd-est 3rd-last) ; Aug 31
                             (set-tm:mon bdtm 7))
                            ((4th-est 4th-last last-year) ; Dec 31
                             (set-tm:mon bdtm 11))
                            (else (set! bdtm (gnc-localtime to-value))))
                          ;; Tax quaters equal Real quarters
                          (case alt-period
                            ((1st-est 1st-last) ; Mar 31
                             (set-tm:mon bdtm 2))
                            ((2nd-est 2nd-last) ; Jun 30
                             (set-tm:mday bdtm 30)
                             (set-tm:mon bdtm 5))
                            ((3rd-est 3rd-last) ; Sep 30
                             (set-tm:mday bdtm 30)
                             (set-tm:mon bdtm 8))
                            ((4th-est 4th-last last-year) ; Dec 31
                             (set-tm:mon bdtm 11))
                            (else
                             (set! bdtm (gnc-localtime to-value)))))
                      (set-tm:isdst bdtm -1)
                      (gnc-mktime bdtm))))

         (form-line-acct-header-printed? #f)
         (form-schedule-header-printed? #f)
         (tax-code-header-printed? #f)
         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
         (error-table (gnc:make-html-table))
        )

    ;; for quarterly estimated tax payments, we need a different period
    ;; return the sometimes changed (from-est to-est full-year?) dates
    (define (txf-special-splits-period account from-value to-value)
      (if (and (xaccAccountGetTaxRelated account)
               (txf-special-date? (gnc:account-get-txf-code account)))
          (let*
              ((full-year?
                (let ((bdto (gnc-localtime to-value))
                      (bdfrom (gnc-localtime from-value)))
                  (and (equal? (tm:year bdto) (tm:year bdfrom))
                       (equal? (tm:mon bdfrom) 0)
                       (equal? (tm:mday bdfrom) 1)
                       (equal? (tm:mon bdto) 11)
                       (equal? (tm:mday bdto) 31))))
              ;; Adjust dates so we get the final Estimated Tax
              ;; paymnent from the right year
               (from-est (if full-year?
                             (let ((bdtm (gnc-localtime
                                          (time64CanonicalDayTime
                                           from-value))))
                               (set-tm:mday bdtm 1) ; 01
                               (set-tm:mon bdtm 2) ; Mar
                               (set-tm:isdst bdtm -1)
                               (gnc-mktime bdtm))
                             from-value))
               (to-est (if full-year?
                           (let* ((bdtm (gnc-localtime
                                         (time64CanonicalDayTime
                                          from-value))))
                             (set-tm:mday bdtm 28) ; 28
                             (set-tm:mon bdtm 1) ; Feb
                             (set-tm:year bdtm (+ (tm:year bdtm) 1))
                             (set-tm:isdst bdtm -1)
                             (gnc-mktime bdtm))
                           to-value)))
            (list from-est to-est full-year?))
          #f))

    (define (handle-account account
                            table
                            need-form-line-acct-header?
                            need-form-schedule-header?
                            current-form-schedule
                            need-tax-code-header?
                            tax-code-heading-text
                            account-type
                            tax-code
                            acct-full-name
                            copy)
       (let* ((splits-period (txf-special-splits-period account
                                                        from-value
                                                        to-value))
              (full-year? (if splits-period
                              (caddr splits-period)))
              (from-special (if splits-period
                                (car splits-period)
                                #f))
              (to-special (if splits-period
                              (cadr splits-period)
                              #f))
              (split-filter-pred (split-report-make-date-filter-predicate
                                 (if (and (not no-special-dates?) splits-period)
                                     from-special
                                     from-value)
                                 (if (and (not no-special-dates?) splits-period)
                                     to-special
                                     to-value)))
              (split-list (make-split-list account split-filter-pred))
              (account-USD-total (gnc-numeric-zero))
              (account-cap-gain-sales-USD-total (gnc-numeric-zero))
              (account-cap-gain-basis-USD-total (gnc-numeric-zero))
              (form-line-acct-text (string-append
                                         "Form or Schedule / Line (TXF Code"
                                         (if show-TXF-data?
                                             ": Parameters"
                                             "")
                                         ") / Account Name"))
              (acct-beg-bal-collector (if (not
                                         (or (eq? account-type ACCT-TYPE-INCOME)
                                           (eq? account-type ACCT-TYPE-EXPENSE)))
                             (gnc:account-get-comm-balance-at-date account
                                      (gnc:time64-start-day-time from-value) #f)
                             #f))
              (acct-end-bal-collector (if (not
                                         (or (eq? account-type ACCT-TYPE-INCOME)
                                           (eq? account-type ACCT-TYPE-EXPENSE)))
                             (gnc:account-get-comm-balance-at-date account
                                                                    to-value #f)
                             #f))
              (account-commodity (xaccAccountGetCommodity account))
             )
             (if (or (and (or (eq? account-type ACCT-TYPE-INCOME)
                              (eq? account-type ACCT-TYPE-EXPENSE))
                          (> (length split-list) 0)) ;; P/L acct with splits
                     (and (not (or (eq? account-type ACCT-TYPE-INCOME)
                                   (eq? account-type ACCT-TYPE-EXPENSE)))
                          (or (not (gnc-numeric-zero-p
                                        (cadr (acct-beg-bal-collector
                                               'getpair account-commodity #f))))
                              (> (length split-list) 0)
                              (not (gnc-numeric-zero-p
                                        (cadr (acct-end-bal-collector
                                               'getpair account-commodity #f))))
                          )));; B/S acct with beg bal or splits or end bal
                 (begin
                    (if tax-mode?
                        ;; print header for new account, detail and sub-total
                        (begin
                           (if need-form-line-acct-header?
                               (begin
                                 (render-header-row table form-line-acct-text)
                                 (set! form-line-acct-header-printed? #t)
                               )
                           )
                           (if need-form-schedule-header?
                               (begin
                                 (if (not (string=? current-form-schedule ""))
                                     (render-header-row table
                                                          current-form-schedule)
                                 )
                                 (set! form-schedule-header-printed? #t)
                               )
                           )
                           (if need-tax-code-header?
                               (begin
                                 (render-header-row table
                                       (string-append "&nbsp; &nbsp;"
                                                         tax-code-heading-text))
                                 (set! tax-code-header-printed? #t)
                               )
                           )
                        )
                    )
                    (let* ((tran-output (process-account-transaction-detail
                                               table
                                               account
                                               split-list
                                               split-details?
                                               full-names?
                                               currency-conversion-date
                                               to-value
                                               transaction-details?
                                               suppress-action-memo?
                                               shade-alternate-transactions?
                                               splits-period
                                               full-year?
                                               from-value
                                               tax-mode?
                                               show-TXF-data?
                                               USD-currency
                                               account-type
                                               tax-code
                                               acct-full-name
                                               acct-beg-bal-collector
                                               acct-end-bal-collector
                                               copy
                                               tax-entity-type))
                           (tran-txf (cadr tran-output))
                           (account-USD-total-as-dr (caddr tran-output))
                          )
                          (set! account-USD-total (car tran-output))
                          (set! account-cap-gain-sales-USD-total
                                                    (cadddr tran-output))
                          (set! account-cap-gain-basis-USD-total
                                                    (car (cddddr tran-output)))
                          (list
                            account-USD-total
                            (if (or (txf-special-split? tax-code)
                                    (= 4 (get-acct-txf-info
                                                   'format
                                                   (xaccAccountGetType account)
                                                   tax-code)))
                                tran-txf
                                (if (not tax-mode?)
                                    (render-txf-account account
                                            account-USD-total-as-dr
                                                  #f #f #t from-value
                                                  account-type tax-code copy
                                                  tax-entity-type #f)
                                    '()))
                            account-USD-total-as-dr
                            account-cap-gain-sales-USD-total
                            account-cap-gain-basis-USD-total
                          )
                    )
                 )
                 (begin;;P/L with no splits or B/S with no beg/end bal or splits
                    (if suppress-0?
                        (list account-USD-total
                              '()
                              account-USD-total
                              account-cap-gain-sales-USD-total
                              account-cap-gain-basis-USD-total
                        )
                        (begin
                           (if need-form-line-acct-header?
                               (begin
                                  (render-header-row table form-line-acct-text)
                                  (set! form-line-acct-header-printed? #t)
                               )
                           )
                           (if need-form-schedule-header?
                               (begin
                                  (render-header-row table
                                                          current-form-schedule)
                                  (set! form-schedule-header-printed? #t)
                               )
                           )
                           (list account-USD-total
                                 '()
                                 account-USD-total
                                 account-cap-gain-sales-USD-total
                                 account-cap-gain-basis-USD-total
                           )
                        )
                    )
                 )
             )
          ) ;; end of let*
    )

    (let ((from-date  (gnc-print-time64 from-value "%Y-%b-%d"))
          (to-date    (gnc-print-time64 to-value "%Y-%b-%d"))
          (today-date (gnc-print-time64 (time64CanonicalDayTime (current-time))
                                        "D%m/%d/%Y"))
          (tax-year   (gnc-print-time64 from-value "%Y"))
          (tax-entity-type (gnc-get-current-book-tax-type))
          (tax-entity-type-valid? #f)
          (prior-form-schedule "")
          (prior-form-sched-line "")
          (prior-tax-code "")
          (prior-account #f)
          (prior-account-copy #f)
          (form-sched-line-USD-total (gnc-numeric-zero))
          (form-sched-line-cap-gain-sales-USD-total (gnc-numeric-zero))
          (form-sched-line-cap-gain-basis-USD-total (gnc-numeric-zero))
          (tax-code-sub-item-USD-total (gnc-numeric-zero))
          (tax-code-sub-item-USD-total-as-dr (gnc-numeric-zero))
          (tax-code-USD-total (gnc-numeric-zero))
          (tax-code-cap-gain-sales-USD-total (gnc-numeric-zero))
          (tax-code-cap-gain-basis-USD-total (gnc-numeric-zero))
          (tax-code-USD-total-as-dr (gnc-numeric-zero))
          (saved-tax-code-text "")
          (need-form-line-acct-header? #f)
          (need-form-schedule-header? #f)
          (need-tax-code-header? #f)
          (tax-code-heading-text "")
          (tax-code-text "")
         )

         (define (handle-tax-code form-line-acct)
            (let* ((current-form-schedule (caar form-line-acct))
                   (copy (caadr form-line-acct))
                   (current-form-schedule (if (> (string->number copy) 1)
                                              (string-append
                                                      current-form-schedule
                                                                   "(" copy ")")
                                              current-form-schedule))
                   (current-form-sched-line (car (caddr form-line-acct)))
                   (current-tax-code (caar (cdddr form-line-acct))) ;; string
                   (acct-full-name (caadr (cdddr form-line-acct)))
                   (account (caar (cddr (cdddr form-line-acct))))
                   (type (caar (cdddr (cdddr form-line-acct))))
                   (tax-code (caadr (cdddr (cdddr form-line-acct)))) ;;symbol
                   (output '())
                   (payer-src (gnc:account-get-txf-payer-source account))
                   (txf-pyr (xaccAccountGetName
                                            (if (eq? payer-src 'parent)
                                                (gnc-account-get-parent account)
                                                account)))
                   (format_type (get-acct-txf-info 'format type tax-code))
                   (code-pns (get-acct-txf-info 'pns type tax-code))
                   (txf-new-payer? (if (= 3 format_type)
                                       (if (string=? prior-tax-code
                                                               current-tax-code)
                                           (if (string=? txf-last-payer txf-pyr)
                                               #f
                                               #t)
                                           #t)
                                       #f))
                  )
                  ;; if not tax-code break, but if tax-code allows
                  ;; multiple lines and there is a new payer, process subline
                  (if (string=? prior-tax-code "")
                      #t ;; do nothing
                      (if (and (or (eqv? (get-acct-txf-info
                                                       'pns
                                                       (xaccAccountGetType
                                                                  prior-account)
                                                       (string->symbol
                                                                prior-tax-code))
                                                    'current)
                                   (eqv? (get-acct-txf-info
                                                       'pns
                                                       (xaccAccountGetType
                                                                  prior-account)
                                                       (string->symbol
                                                                prior-tax-code))
                                                    'parent))
                               (if (string=? prior-tax-code current-tax-code)
                                   (if (> txf-l-count 0)
                                       txf-new-payer?
                                       #f)
                                   (if (= 3 (get-acct-txf-info 'format
                                              (xaccAccountGetType prior-account)
                                               (string->symbol prior-tax-code)))
                                       #t
                                       #f))
                          )
                          (begin
                            (if tax-mode?
                                ;; printed report processing
                                ;; print a sub-line subtotal
                                (if (and suppress-0? (gnc-numeric-zero-p
                                                   tax-code-sub-item-USD-total))
                                    #t ;; do nothing
                                    (let* ((print-info (gnc-account-print-info
                                                              prior-account #f))
                                           (tax-code-sub-item-total-amount
                                               (xaccPrintAmount
                                                     tax-code-sub-item-USD-total
                                                     print-info))
                                          )
                                          ;; print prior tax-code-sub-item
                                          ;; total and reset accum
                                          (render-total-row
                                             table
                                             tax-code-sub-item-total-amount
                                             (string-append
                                                (if (string=? ""
                                                          prior-form-sched-line)
                                                    "Line (Code): "
                                                    "")
                                                saved-tax-code-text
                                                ", Item "
                                                (number->string txf-l-count)
                                                ": "
                                                txf-last-payer
                                                " "
                                             )
                                             #f
                                             transaction-details?
                                             #f
                                             (if (gnc-numeric-negative-p
                                                    tax-code-sub-item-USD-total)
                                                 #t
                                                 #f)
                                              #f ;; format = 3, not 4
                                              #f ;; not applicable
                                              #f ;; not applicable
                                          )
                                    )
                                )
                                ;; txf output processing
                                (if (gnc-numeric-zero-p
                                              tax-code-sub-item-USD-total-as-dr)
                                    #t ;; do nothing
                                    (begin
                                      (set! output
                                            (list (render-txf-account
                                              prior-account
                                              tax-code-sub-item-USD-total-as-dr
                                              #f #f #f #f
                                              (xaccAccountGetType prior-account)
                                              (string->symbol prior-tax-code)
                                              prior-account-copy
                                              tax-entity-type #f)))
                                      (set! tax-code-USD-total (gnc-numeric-zero))
                                      (set! tax-code-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                      (if (not (string=? prior-tax-code
                                                              current-tax-code))
                                          (begin
                                            (set! txf-new-payer? #t)
                                            (set! txf-l-count 0)
                                          ))
                                    )
                                )
                            )
                            (set! tax-code-sub-item-USD-total
                                                             (gnc-numeric-zero))
                            (set! tax-code-sub-item-USD-total-as-dr
                                                             (gnc-numeric-zero))
                          )
                          #f ;; else do nothing
                      )
                  )
                  ;; process prior tax code break, if appropriate, before
                  ;; processing current account
                  (if (string=? prior-tax-code "")
                      #t ;; do nothing
                      (if tax-mode?
                         ;; printed report processing
                         (if (and (string=? prior-tax-code current-tax-code)
                                  (string=? prior-form-sched-line
                                                       current-form-sched-line)
                                  (string=? prior-form-schedule
                                                         current-form-schedule))
                             #t ;; do nothing
                             (if (and suppress-0? (gnc-numeric-zero-p
                                                            tax-code-USD-total))
                                 #t ;; do nothing
                                 (let* ((print-info (gnc-account-print-info
                                                              prior-account #f))
                                        (tax-code-total-amount
                                               (xaccPrintAmount
                                                     tax-code-USD-total
                                                     print-info))
                                        (tax-code-cap-gain-sales-total-amount
                                               (xaccPrintAmount
                                                     tax-code-cap-gain-sales-USD-total
                                                     print-info))
                                        (tax-code-cap-gain-basis-total-amount
                                               (xaccPrintAmount
                                                     tax-code-cap-gain-basis-USD-total
                                                     print-info))
                                       )
                                       ;; print prior tax-code total and
                                       ;; reset accum
                                       (render-total-row
                                                 table
                                                 tax-code-total-amount
                                                 (string-append
                                                   (if (string=? ""
                                                          prior-form-sched-line)
                                                       "Line (Code): "
                                                       "")
                                                   saved-tax-code-text
                                                 )
                                                 #t
                                                 transaction-details?
                                                 #f
                                                 (if (gnc-numeric-negative-p
                                                             tax-code-USD-total)
                                                     #t
                                                     #f)
                                                 (if (= 4 (get-acct-txf-info
                                                             'format
                                                             (xaccAccountGetType
                                                                prior-account)
                                                             (string->symbol
                                                                prior-tax-code))
                                                     )
                                                     #t
                                                     #f)
                                                 tax-code-cap-gain-sales-total-amount
                                                 tax-code-cap-gain-basis-total-amount
                                       )
                                       (set! tax-code-USD-total
                                                             (gnc-numeric-zero))
                                       (set! tax-code-cap-gain-sales-USD-total
                                                             (gnc-numeric-zero))
                                       (set! tax-code-cap-gain-basis-USD-total
                                                             (gnc-numeric-zero))
                                       (set! tax-code-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                       (set! tax-code-sub-item-USD-total
                                                             (gnc-numeric-zero))
                                       (set! tax-code-sub-item-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                       (set! txf-l-count 0)
                                 )
                             )
                         )
                         ;; txf output processing
                         (if (gnc-numeric-zero-p tax-code-USD-total-as-dr)
                             #t ;; do nothing
                             (if (or ;; tax-code break
                                     (not (and (string=?
                                                prior-tax-code current-tax-code)
                                               (string=? prior-form-sched-line
                                                        current-form-sched-line)
                                               (string=? prior-form-schedule
                                                        current-form-schedule)))
                                     ;; not tax-code break, but tax-code allows
                                     ;; multiple lines and there is a new payer
                                     (and (or (eqv? (get-acct-txf-info
                                                       'pns
                                                       (xaccAccountGetType
                                                                  prior-account)
                                                       (string->symbol
                                                                prior-tax-code))
                                                       'current)
                                              (eqv? (get-acct-txf-info
                                                       'pns
                                                       (xaccAccountGetType
                                                                  prior-account)
                                                       (string->symbol
                                                                prior-tax-code))
                                                       'parent))
                                          (if (> txf-l-count 0)
                                              txf-new-payer?
                                              #f)
                                     )
                                 )
                                 (begin
                                    (set! output
                                          (list (render-txf-account
                                              prior-account
                                              (if (= 4 (get-acct-txf-info
                                                          'format
                                                          (xaccAccountGetType
                                                                  prior-account)
                                                          (string->symbol
                                                               prior-tax-code)))
                                                  (gnc-numeric-neg
                                                   tax-code-cap-gain-sales-USD-total)
                                                  tax-code-USD-total-as-dr)
                                              #f #f #f #f
                                              (xaccAccountGetType prior-account)
                                              (string->symbol prior-tax-code)
                                              prior-account-copy
                                              tax-entity-type #f)))
                                    (set! tax-code-USD-total (gnc-numeric-zero))
                                    (set! tax-code-cap-gain-sales-USD-total
                                                             (gnc-numeric-zero))
                                    (set! tax-code-cap-gain-basis-USD-total
                                                             (gnc-numeric-zero))
                                    (set! tax-code-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                    (set! tax-code-sub-item-USD-total
                                                             (gnc-numeric-zero))
                                    (set! tax-code-sub-item-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                    (if (not (string=? prior-tax-code
                                                              current-tax-code))
                                        (begin
                                          (set! txf-new-payer? #t)
                                          (set! txf-l-count 0)
                                        ))
                                 )
                                 #f ;; do nothing
                             )
                         )
                      )
                  )
                  ;; process prior form-schedule-line break, if appropriate,
                  ;; before processing current account
                  (if (string=? prior-form-sched-line "")
                      (set! form-sched-line-USD-total (gnc-numeric-zero))
                      (if tax-mode?
                         ;; printed report processing
                         (if (and (string=? prior-form-sched-line
                                                       current-form-sched-line)
                                  (string=? prior-form-schedule
                                                         current-form-schedule))
                             #t ;; do nothing
                             (if (and suppress-0? (gnc-numeric-zero-p
                                                     form-sched-line-USD-total))
                                 #t ;; do nothing
                                 (let* ((print-info (gnc-account-print-info
                                                              prior-account #f))
                                        (form-sched-line-total-amount
                                          (xaccPrintAmount
                                             form-sched-line-USD-total
                                             print-info))
                                        (form-sched-line-cap-gain-sales-total-amount
                                          (xaccPrintAmount
                                             form-sched-line-cap-gain-sales-USD-total
                                             print-info))
                                        (form-sched-line-cap-gain-basis-total-amount
                                          (xaccPrintAmount
                                             form-sched-line-cap-gain-sales-USD-total
                                             print-info))
                                       )
                                       ;; print prior form-schedule-line total
                                       ;; and reset accum
                                       (render-total-row
                                            table
                                            form-sched-line-total-amount
                                            (string-append
                                                prior-form-schedule
                                                " Line "
                                                prior-form-sched-line
                                            )
                                            #t
                                            transaction-details?
                                            #f
                                            (if (gnc-numeric-negative-p
                                                 form-sched-line-USD-total)
                                                #t
                                                #f)
                                            (if (= 4 (get-acct-txf-info
                                                             'format
                                                             (xaccAccountGetType
                                                                prior-account)
                                                             (string->symbol
                                                                prior-tax-code))
                                                )
                                                #t
                                                #f)
                                            form-sched-line-cap-gain-sales-total-amount
                                            form-sched-line-cap-gain-basis-total-amount
                                       )
                                       (set! form-sched-line-USD-total
                                                             (gnc-numeric-zero))
                                       (set! form-sched-line-cap-gain-sales-USD-total
                                                             (gnc-numeric-zero))
                                       (set! form-sched-line-cap-gain-basis-USD-total
                                                             (gnc-numeric-zero))
                                       (set! txf-l-count 0)
                                 )
                             )
                         )
                         #f
                      )
                  )
                  (if (string=? prior-form-schedule current-form-schedule)
                      (begin
                         (if form-line-acct-header-printed?
                             (set! need-form-line-acct-header? #f)
                             (set! need-form-line-acct-header? #t)
                         )
                         (if form-schedule-header-printed?
                             (set! need-form-schedule-header? #f)
                             (set! need-form-schedule-header? #t)
                         )
                      )
                      (begin ;; new form
                         (set! need-form-line-acct-header? #t)
                         (set! need-form-schedule-header? #t)
                         (set! need-tax-code-header? #t)
                         (set! form-line-acct-header-printed? #f)
                         (set! form-schedule-header-printed? #f)
                         (set! tax-code-header-printed? #f)
                         (set! prior-form-schedule current-form-schedule)
                      )
                  )
                  (if (and (string=? prior-tax-code current-tax-code)
                           (string=? prior-form-sched-line
                                                       current-form-sched-line)
                           (string=? prior-form-schedule current-form-schedule))
                      (if tax-code-header-printed?
                          (set! need-tax-code-header? #f)
                          (set! need-tax-code-header? #t)
                      )
                      (begin ;; if new tax-code
                         (let* ((description (get-acct-txf-info
                                                   'desc
                                                   type
                                                   tax-code))
                                (description (if description description ""))
                               )
                              (set! need-tax-code-header? #t)
                              (set! tax-code-header-printed? #f)
                              (set! tax-code-text
                                    (string-append
                                      (if (string=? current-form-sched-line "")
                                          ""
                                          (string-append "Line "
                                                  current-form-sched-line ": "))
                                      description " ("
                                      (substring current-tax-code 1
                                           (string-length current-tax-code))
                                      ")"))
                              (set! tax-code-heading-text
                                    (string-append
                                      (if (string=? current-form-sched-line "")
                                          ""
                                          (string-append "Line "
                                                  current-form-sched-line ": "))
                                      description " ("
                                      (substring current-tax-code 1
                                           (string-length current-tax-code))
                                      (if show-TXF-data?
                                          (string-append
                                            ": Payer Name Option "
                                            (if (or (eq? 'parent
                                                         (get-acct-txf-info
                                                              'pns
                                                              type
                                                              tax-code))
                                                    (eq? 'current
                                                         (get-acct-txf-info
                                                              'pns
                                                              type
                                                              tax-code)))
                                                "Y"
                                                "N")
                                            ", TXF Format "
                                            (number->string
                                                    (get-acct-txf-info
                                                         'format
                                                         type
                                                         tax-code))
                                            ", Multiple Copies "
                                            (if (get-acct-txf-info
                                                     'multiple
                                                     type
                                                     tax-code)
                                                "Y"
                                                "N")
                                            ", Special Dates "
                                            (if (txf-special-date? tax-code)
                                                "Y"
                                                "N")
                                            ", Special Splits "
                                            (if (txf-special-split? tax-code)
                                                "Y"
                                                "N")
                                          )
                                      "")
                                    ")"))
                         )
                         (set! saved-tax-code-text tax-code-text)
                      )
                  )
                  (set! txf-account-name txf-pyr)
                  (set! txf-l-count (if (and (= format_type 3)
                                             (or (eq? code-pns 'parent)
                                                 (eq? code-pns 'current)))
                                        (if (equal? txf-last-payer
                                                               txf-account-name)
                                            txf-l-count
                                            (if (equal? "" txf-last-payer)
                                                1
                                                (+ 1 txf-l-count)))
                                        1))
                  (set! txf-last-payer (if (and (= format_type 3)
                                                (or (eq? code-pns 'parent)
                                                    (eq? code-pns 'current)))
                                           txf-account-name
                                           ""))
                  (let* ((account-output (handle-account
                                                 account
                                                 table
                                                 need-form-line-acct-header?
                                                 need-form-schedule-header?
                                                 current-form-schedule
                                                 need-tax-code-header?
                                                 tax-code-heading-text
                                                 type
                                                 tax-code
                                                 acct-full-name
                                                 copy))
                         (account-USD-total-as-dr (caddr account-output))
                         (code-tfx-output (if (null? output)
                                              (if (null? (cadr account-output))
                                                  '()
                                                  (list (cadr account-output)))
                                              (if (null? (cadr account-output))
                                                  (list output)
                                                  (list output
                                                       (cadr account-output)))))
                        )
                        (set! tax-code-USD-total (gnc-numeric-add-fixed
                                                          tax-code-USD-total
                                                          (car account-output)))
                        (set! tax-code-cap-gain-sales-USD-total
                              (gnc-numeric-add-fixed
                                       tax-code-cap-gain-sales-USD-total
                                       (cadddr account-output)))
                        (set! tax-code-cap-gain-basis-USD-total
                              (gnc-numeric-add-fixed
                                       tax-code-cap-gain-basis-USD-total
                                       (car (cddddr account-output))))
                        (set! tax-code-USD-total-as-dr (gnc-numeric-add-fixed
                                                       tax-code-USD-total-as-dr
                                                       account-USD-total-as-dr))
                        (set! tax-code-sub-item-USD-total (gnc-numeric-add-fixed
                                                     tax-code-sub-item-USD-total
                                                          (car account-output)))
                        (set! tax-code-sub-item-USD-total-as-dr
                                         (gnc-numeric-add-fixed
                                             tax-code-sub-item-USD-total-as-dr
                                                       account-USD-total-as-dr))
                        (set! form-sched-line-USD-total (gnc-numeric-add-fixed
                                                       form-sched-line-USD-total
                                                          (car account-output)))
                        (set! form-sched-line-cap-gain-sales-USD-total
                              (gnc-numeric-add-fixed
                                       form-sched-line-cap-gain-sales-USD-total
                                       (cadddr account-output)))
                        (set! form-sched-line-cap-gain-basis-USD-total
                              (gnc-numeric-add-fixed
                                       form-sched-line-cap-gain-basis-USD-total
                                       (car (cddddr account-output))))
                        (set! need-form-line-acct-header? #f)
                        (set! need-form-schedule-header? #f)
                        (set! need-tax-code-header? #f)
                        (set! work-done (+ 1 work-done))
                            (gnc:report-percent-done
                                    (* 100 (if (> work-to-do 0)
                                               (/ work-done work-to-do)
                                               1)))
                        (set! prior-form-sched-line current-form-sched-line)
                        (set! prior-tax-code current-tax-code)
                        (set! prior-account account)
                        (set! prior-account-copy copy)
                        (if tax-mode?
                            '()
                            code-tfx-output)
                  ) ;; end of let
            ) ;; end of let*
         )

      ;; Now, the main body
      (set! selected-accounts-sorted-by-form-line-acct '())
      (set! txf-invalid-alist '())
      (if (gnc:txf-get-tax-entity-type (string->symbol tax-entity-type))
          (set! tax-entity-type-valid? #t)
          (set! tax-entity-type-valid? #f))
      (if tax-entity-type-valid?
          (begin
            (make-form-line-acct-list selected-accounts tax-year)
            (set! selected-accounts-sorted-by-form-line-acct
               (sort-list
                   selected-accounts-sorted-by-form-line-acct
                   form-line-acct-less
               ))
            (set! work-to-do (length selected-accounts-sorted-by-form-line-acct))
            (set! txf-l-count 0)
          ))

      (if (not tax-mode?) ; Do Txf mode
          (if tax-entity-type-valid?
              (if file-name		; cancel TXF if no file selected
                  (let ((port (catch #t ;;e.g., system-error
                                 (lambda () (open-output-file file-name))
                                 (lambda (key . args)
                                    (gnc-error-dialog
                                          '()
                                          (string-append
                                              "Could not open the file: "
                                              file-name
                                              ". The error is: "
                                              (symbol->string key)
                                              " - "
                                              (car (caddr args))
                                              "."
                                          ))
                                     #f)))
                       )
                       (if port ;; port opened successfully
                           (let* ((output (map (lambda (form-line-acct)
                                               (handle-tax-code form-line-acct))
                                    selected-accounts-sorted-by-form-line-acct))
                                  (output-txf
                                    (list
                                      "V042" crlf
                                      "AGnuCash " gnc:version crlf
                                      today-date crlf
                                      "^" crlf
                                      output
                                      (if (or
                                             (gnc-numeric-zero-p tax-code-USD-total)
                                             (not prior-account))
                                          '()
                                          (render-txf-account
                                              prior-account
                                              (if (= 4 (get-acct-txf-info
                                                          'format
                                                          (xaccAccountGetType
                                                                  prior-account)
                                                          (gnc:account-get-txf-code
                                                                prior-account)))
                                                  (gnc-numeric-neg
                                                   tax-code-cap-gain-sales-USD-total)
                                                  tax-code-USD-total-as-dr)
                                              #f #f #f #f
                                              (xaccAccountGetType prior-account)
                                              (gnc:account-get-txf-code
                                                                prior-account)
                                              prior-account-copy
                                              tax-entity-type #f))
                                    ))
                                 )
                                 ;; prior-account can be #f if selected accounts are
                                 ;; marked as 'tax-related' in the account edit
                                 ;; dialog but not actually assigned to a tax code
                                 ;; using the 'Tax Options' dialog (UI bug?).
                                 ;; An empty file is unfortunately put out with
                                 ;; no user warning other than message on report.
                                 (if prior-account
                                     (gnc:display-report-list-item output-txf port
                                                           "taxtxf.scm - ")
                                     #f)
                                 (close-output-port port)
                                 #t
                           ) ; end of let
                           ;; Could not open port successfully
                           #t ;; to prevent 2nd error dialog in
                              ;; gnc_plugin_page_report_export_cb
                       ) ;; end of if
                  ) ;; end of let*
              #f) ;;end of if
          #f) ;;end of if
          (begin  ; else do tax report
                  (gnc:html-document-set-style!
                   doc "header-just-top"
                   'tag "th"
                   'attribute (list "class" "column-heading-left")
                   'attribute (list "valign" "top"))

                  (gnc:html-document-set-style!
                   doc "header-just-bot"
                   'tag "th"
                   'attribute (list "class" "column-heading-left")
                   'attribute (list "valign" "bottom"))

                  (gnc:html-document-set-style!
                   doc "tran-detail"
                   'tag "tr"
                   'attribute (list "class" "normal-row")
                   'attribute (list "valign" "top"))

                  (gnc:html-document-set-style!
                   doc "tran-detail-shade"
                   'tag "tr"
                   'attribute (list "class" "alternate-row")
                   'attribute (list "valign" "top"))

                  (gnc:html-document-set-style!
                   doc "text-cell-center"
                   'tag "td"
                   'attribute (list "class" "text-cell")
                   'attribute (list "align" "center"))

                  (gnc:html-document-set-style!
                   doc "number-cell-bot"
                   'tag "td"
                   'attribute (list "class" "number-cell")
                   'attribute (list "valign" "bottom"))

                  (gnc:html-document-set-style!
                   doc "number-cell-bot-neg"
                   'tag "td"
                   'attribute (list "class" "number-cell neg")
                   'attribute (list "valign" "bottom"))

             (gnc:html-document-set-style!
              doc "just-bot"
              'tag "td"
              'attribute (list "valign" "bottom"))

             (gnc:html-document-set-title! doc report-name)

             (gnc:html-document-add-object!
              doc (gnc:make-html-text
                   (gnc:html-markup-p
                    (gnc:html-markup
                     "center"
                     (gnc:html-markup/format
                      (string-append (if (and (gnc-get-current-book-tax-name)
                                              (not (string-null? (gnc-get-current-book-tax-name))))
                                         "Tax Name: ~a<br/>"
                                         "~a")
                      "Period from ~a to ~a<br/>Tax Year ~a<br/>Tax Entity Type: ~a<br/>All amounts in USD unless otherwise noted")
                           (gnc-get-current-book-tax-name)
                           from-date
                           to-date
                           tax-year
                           (or (gnc:txf-get-tax-entity-type-description
                                (string->symbol tax-entity-type))
                               "None specified")
                     )))))

             (if (not (null? txf-invalid-alist))
                 (begin
                   (gnc:html-document-add-object!
                    doc (gnc:make-html-text
                          (gnc:html-markup-p
                           (gnc:html-markup/format
                      "<br/>The following Account(s) have errors with their Income Tax code assignments (use 'Edit->Tax Report Options' to correct):"))))
                   (gnc:html-document-add-object! doc error-table)
                    (gnc:html-table-append-row!
                      error-table
                      (append (list (gnc:make-html-table-header-cell/markup
                                     "header-just-bot" "Account"))
                              (list (gnc:make-html-table-header-cell/markup
                                     "header-just-bot" "Error Description"))
                              (list (gnc:make-html-table-header-cell/markup
                                     "header-just-bot" "Code"))
                              (list (gnc:make-html-table-header-cell/markup
                                     "header-just-bot" "Form"))
                              (list (gnc:make-html-table-header-cell/markup
                                     "header-just-bot" "Description"))
                      )
                    )
                    (map (lambda (error)
                          (let* ((form (car (cdddr error)))
                                 (acct (cadr (cdddr error)))
                                 (num-code (if (string=? (car error) "")
                                               ""
                                               (if (string=? (car error) "None")
                                                   "None"
                                                   (substring (car error) 1
                                                    (string-length (car error)))
                                               )
                                           )
                                 )
                                 (form-desc (if form
                                                (let* ((tax-code
                                                 (xaccAccountGetTaxUSCode acct))
                                                       (tax-code-sym
                                                      (string->symbol tax-code))
                                                       (type
                                                      (xaccAccountGetType acct))
                                                      )
                                                  (get-acct-txf-info 'desc type
                                                                   tax-code-sym)
                                                )
                                                ""))
                                 (form (if form form "")))
                            (gnc:html-table-append-row/markup!
                               error-table
                               "tran-detail"
                               (append (list (gnc:make-html-table-cell
                                              (caddr error)))
                                       (list (gnc:make-html-table-cell
                                              (cadr error)))
                                       (list (gnc:make-html-table-cell
                                              num-code))
                                       (list (gnc:make-html-table-cell
                                              form))
                                       (list (gnc:make-html-table-cell
                                              form-desc))
                               )
                            )
                          )
                         )
                     txf-invalid-alist)
                   (gnc:html-document-add-object!
                    doc (gnc:make-html-text
                          (gnc:html-markup-p
                           (gnc:html-markup/format
                      " <br/> "))))
                 )
             )

             (gnc:html-document-add-object! doc table)

             (if tax-entity-type-valid?
                 (map (lambda (form-line-acct) (handle-tax-code form-line-acct))
                      selected-accounts-sorted-by-form-line-acct))

             ;; if tax-code allows multiple lines, print subline
             (if (or (and suppress-0? (gnc-numeric-zero-p
                                                   tax-code-sub-item-USD-total))
                     (null? selected-accounts)
                     (not prior-account))
                 #t ;; do nothing
                 (if (and (or (eqv? (get-acct-txf-info 'pns
                                            (xaccAccountGetType prior-account)
                                              (string->symbol prior-tax-code))
                                                                      'current)
                              (eqv? (get-acct-txf-info 'pns
                                            (xaccAccountGetType prior-account)
                                              (string->symbol prior-tax-code))
                                                                       'parent))
                          (if (> txf-l-count 0)
                              (if (= 3 (get-acct-txf-info 'format
                                             (xaccAccountGetType prior-account)
                                              (string->symbol prior-tax-code)))
                                  #t
                                  #f)
                          #f)
                     )
                     ;; print a sub-line subtotal
                     (if (and suppress-0? (gnc-numeric-zero-p
                                                   tax-code-sub-item-USD-total))
                         #t ;; do nothing
                         (let* ((print-info (gnc-account-print-info
                                                              prior-account #f))
                                (tax-code-sub-item-total-amount
                                   (xaccPrintAmount tax-code-sub-item-USD-total
                                                                    print-info))
                               )
                               (render-total-row
                                           table
                                           tax-code-sub-item-total-amount
                                           (string-append
                                              (if (string=? ""
                                                          prior-form-sched-line)
                                                  "Line (Code): "
                                                  "")
                                              saved-tax-code-text
                                              ", Item "
                                              (number->string txf-l-count)
                                              ": "
                                              txf-last-payer
                                              " "
                                           )
                                           #f
                                           transaction-details?
                                           #f
                                           (if (gnc-numeric-negative-p
                                                    tax-code-sub-item-USD-total)
                                               #t
                                               #f)
                                           #f ;; format = 3, not 4
                                           #f ;; not applicable
                                           #f ;; not applicable
                               )
                               (set! tax-code-sub-item-USD-total
                                                             (gnc-numeric-zero))
                               (set! tax-code-sub-item-USD-total-as-dr
                                                             (gnc-numeric-zero))
                         )
                     )
                     #f ;; else do nothing
                 )
             )
             ;; print final tax-code totals
             (if (or (and suppress-0? (gnc-numeric-zero-p tax-code-USD-total))
                     (null? selected-accounts)
                     (not prior-account))
                 #t ;; do nothing
                 (let* ((print-info (gnc-account-print-info prior-account #f))
                        (tax-code-total-amount (xaccPrintAmount
                                                           tax-code-USD-total
                                                           print-info))
                        (tax-code-cap-gain-sales-total-amount
                                   (xaccPrintAmount
                                            tax-code-cap-gain-sales-USD-total
                                                           print-info))
                        (tax-code-cap-gain-basis-total-amount
                                   (xaccPrintAmount
                                            tax-code-cap-gain-basis-USD-total
                                                           print-info))
                       )
                       (render-total-row table tax-code-total-amount
                                              (string-append "Line (Code): "
                                                            saved-tax-code-text)
                                              #t
                                              transaction-details?
                                              #f
                                              (if (gnc-numeric-negative-p
                                                             tax-code-USD-total)
                                                  #t
                                                  #f)
                                              (if (= 4 (get-acct-txf-info
                                                          'format
                                                          (xaccAccountGetType
                                                                  prior-account)
                                                          (string->symbol
                                                               prior-tax-code)))
                                                  #t
                                                  #f)
                                              tax-code-cap-gain-sales-total-amount
                                              tax-code-cap-gain-basis-total-amount
                       )
                 )
             )
             ;; print final Form line number totals
             (if (not (string=? prior-form-sched-line ""))
                 (if (or (and suppress-0?
                                 (gnc-numeric-zero-p form-sched-line-USD-total))
                         (null? selected-accounts)
                         (not prior-account)
                     )
                     #t ;; do nothing
                     (let* ((print-info (gnc-account-print-info prior-account
                                                                            #f))
                            (form-sched-line-total-amount
                                      (xaccPrintAmount
                                          form-sched-line-USD-total print-info))
                            (form-sched-line-cap-gain-sales-total-amount
                                      (xaccPrintAmount
                                        form-sched-line-cap-gain-sales-USD-total
                                                                    print-info))
                            (form-sched-line-cap-gain-basis-total-amount
                                      (xaccPrintAmount
                                        form-sched-line-cap-gain-basis-USD-total
                                                                    print-info))
                           )
                           ;; print prior form-schedule-line total; reset accum
                           (render-total-row
                                table
                                form-sched-line-total-amount
                                (string-append prior-form-schedule " Line "
                                               prior-form-sched-line)
                                #t
                                transaction-details?
                                #f
                                (if (gnc-numeric-negative-p
                                                      form-sched-line-USD-total)
                                    #t
                                    #f)
                                (if (= 4 (get-acct-txf-info
                                                          'format
                                                          (xaccAccountGetType
                                                                  prior-account)
                                                          (string->symbol
                                                               prior-tax-code)))
                                    #t
                                    #f)
                                form-sched-line-cap-gain-sales-total-amount
                                form-sched-line-cap-gain-basis-total-amount
                           )
                           (set! form-sched-line-USD-total (gnc-numeric-zero))
                     )
                 )
             )

             (if (or (null? selected-accounts)
                     (null? selected-accounts-sorted-by-form-line-acct))
                 ;; print message for no accounts; note: it's possible to flag
                 ;; an account as 'tax-related' in the account edit dialog but
                 ;; not to actually assign it to a tax code using 'Tax Options'
                 ;; which allows 'selected-accounts' to be not null while
                 ;; 'selected-accounts-sorted-by-form-line-acct' may be null
                 (gnc:html-document-add-object!
                  doc
                  (gnc:make-html-text
                   (gnc:html-markup-p
                     (if (or (null? (gnc-get-current-book-tax-type))
                             (string=? (gnc-get-current-book-tax-type) "")
                             (string=? (gnc-get-current-book-tax-type) "Other"))
                       "The Income Tax Report is only available for valid Income Tax Entity Types. Go to the Edit->Tax Report Options dialog to change your Income Tax Entity Type selection and set up tax-related accounts."
                       "No Tax Related accounts were found with your account selection. Change your selection or go to the Edit->Tax Report Options dialog to set up tax-related accounts."))))
                 ;; or print selected report options
                 (gnc:html-document-add-object!
                  doc (gnc:make-html-text
                        (gnc:html-markup-p
                         (gnc:html-markup/format
                          (string-append
                             "Selected Report Options:<br/>"
                             ;; selected accounts
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; suppress 0.00 values
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; full acct names
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; transfer detail
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; TXF detail
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; action:memo detail
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; transaction detail
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; special dates
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; currency conversion date
                             "&nbsp; &nbsp; &nbsp; ~a <br/>"
                             ;; alternate transaction shading
                             "")
                             (if (not (null? user-sel-accnts))
                                 "Subset of accounts"
                                 "No accounts (none = all accounts)")
                             (if suppress-0?
                                 "Suppress $0.00 valued Tax Codes"
                                 "Do not suppress $0.00 valued Tax Codes")
                             (if full-names?
                                 "Display full account names"
                                 "Do not display full account names")
                             (if split-details?
                                 "Display all Transfer To/From Accounts"
                                 "Do not display all Transfer To/From Accounts")
                             (if show-TXF-data?
                                 "Print TXF export parameters"
                                 "Do not Print TXF export parameters")
                             (if suppress-action-memo?
                                 "Do not display Action:Memo data"
                                 "Display Action:Memo data")
                             (if transaction-details?
                                 "Display transactions for selected accounts"
                                 "Do not display transactions for selected accounts")
                             (if no-special-dates?
                                 "Do not include transactions outside of selected dates"
                                 "Include some transactions outside of selected dates")
                             (if (equal? currency-conversion-date
                                         'conv-to-tran-date)
                                 "PriceDB lookups nearest to transaction date"
                                 "PriceDB lookups nearest to report end date")
                          )
                        ))))

             (gnc:report-finished)
             doc
          ) ;end begin
      ) ;end if
    ) ;end let
  )  ;end let*
) ;end define

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "f8921f4e5c284d7caca81e239f468a68"
 'menu-name (N_ "Tax Schedule Report & TXF Export")
 ;;'menu-path (list gnc:menuname-taxes)
 'menu-tip (N_ "Taxable Income/Deductible Expenses with Transaction Detail/Export to .TXF file")
 'options-generator tax-options-generator
 'renderer (lambda (report-obj)
             (generate-tax-schedule
              (_ "Taxable Income/Deductible Expenses")
              (_ "This report shows transaction detail for your accounts \
related to Income Taxes.")
              report-obj
              #t
              #f))
 'export-types (list (cons "TXF" 'txf))
 'export-thunk (lambda (report-obj choice file-name)
                 (generate-tax-schedule
                  (_ "Taxable Income/Deductible Expenses")
                  (_ "This page shows transaction detail for relevant \
Income Tax accounts.")
                  report-obj
                  #f
                  file-name)))
