;; -*-scheme-*-
;; by  Richard -Gilligan- Uschold 
;;
;; updated by  J. Alex Aycinena, July 2008 
;;
;; This report prints transaction detail and account totals for Tax-related
;; accounts sorted by tax code and form/schedule, and exports TXF files for
;; import to TaxCut, TurboTax, etc.
;;
;; For this to work, the user has to segregate taxable and not taxable
;; income to different accounts, as well as deductible and non-
;; deductible expenses and the accounts need to be referenced to the tax codes.
;; However, there is no need to limit tax codes to just one account. For codes
;; like N286 (Dividend, Ordinary) that can have the "payer" printed on
;; Schedule B on seperate lines, to have amounts from different accounts
;; summarized together for one "payer" line, the accounts referenced to the
;; same tax code for a given "payer" need to be adjacent to each other in the
;; account hierarchy.
;;
;; The user selects the accounts(s) to be printed; if none, all are selected.
;; Includes all sub-account levels below selected account, that are coded for
;; taxes.
;;
;; Optionally, does NOT print tax codes and accounts with $0.00 values.
;; Prints data between the From and To dates, inclusive.
;; Optional alternate periods:
;; "Last Year", "1st Est Tax Quarter", ... "4th Est Tax Quarter"
;; "Last Yr Est Tax Qtr", ... "Last Yr Est Tax Qtr"
;; Estimated Tax Quarters: Dec 31, Mar 31, Jun 30, Aug 31)
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
;; From prior version:
;; NOTE: setting of specific dates is squirly! and seems
;; to be current-date dependant!  Actually, time of day dependant!  Just
;; after midnight gives diffenent dates than just before!  Referencing
;; all times to noon seems to fix this.  Subtracting 1 year sometimes
;; subtracts 2!  see "(to-value"
;;
;; Based on prior taxtxf.scm and with references to transaction.scm.

(define-module (gnucash report taxtxf))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))
(use-modules (sw_gnome_utils)) ;; to get to gnc-error-dialog

(require 'printf)

(gnc:module-load "gnucash/tax/us" 0)
(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Tax Schedule Report/TXF Export"))

;(define USD-currency (gnc-commodity-table-lookup
;                        (gnc-commodity-table-get-table (gnc-get-current-book))
;                        "CURRENCY"
;                        "USD"))
;; this comes back as PEN??? rather than USD; need to define further down to get
;; to work - weird

(define selected-accounts-sorted-by-form-line-acct (list))

(define today (timespecCanonicalDayTime (cons (current-time) 0)))

(define bdtm
  (let ((result (gnc:timepair->date today)))
    (set-tm:mday result 16)             ; 16
    (set-tm:mon result 3)               ; Apr
    (set-tm:isdst result -1)
    result))

(define tax-day (cons (car (mktime bdtm)) 0))

(define after-tax-day (gnc:timepair-later tax-day today))

(define (make-split-list account split-filter-pred)
  (filter split-filter-pred (xaccAccountGetSplitList account)))

;; returns a predicate that returns true only if a split is
;; between early-date and late-date
(define (split-report-make-date-filter-predicate begin-date-tp end-date-tp)
  (lambda (split) 
    (let ((tp
           (gnc-transaction-get-date-posted
            (xaccSplitGetParent split))))
      (and (gnc:timepair-ge-date tp begin-date-tp)
           (gnc:timepair-le-date tp end-date-tp)))))

;; This is nearly identical to, and could be shared with
;; display-report-list-item in report.scm. This adds warn-msg parameter
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
    "c" (N_ "Override or modify From: & To:")
    (if after-tax-day 'from-to 'last-year)
    (list (list->vector
           (list 'from-to (N_ "Use From - To") (N_ "Use From - To period")))
          (list->vector
           (list '1st-est (N_ "1st Est Tax Quarter") (N_ "Jan 1 - Mar 31")))
          (list->vector
           (list '2nd-est (N_ "2nd Est Tax Quarter") (N_ "Apr 1 - May 31")))
          (list->vector
	   ;; Translators: The US tax quarters are different from
	   ;; actual year's quarters! See the definition of
	   ;; tax-qtr-real-qtr-year variable above.
           (list '3rd-est (N_ "3rd Est Tax Quarter") (N_ "Jun 1 - Aug 31")))
          (list->vector
           (list '4th-est (N_ "4th Est Tax Quarter") (N_ "Sep 1 - Dec 31")))
          (list->vector
           (list 'last-year (N_ "Last Year") (N_ "Last Year")))
          (list->vector
           (list '1st-last (N_ "Last Yr 1st Est Tax Qtr")
                 (N_ "Jan 1 - Mar 31, Last year")))
          (list->vector
           (list '2nd-last (N_ "Last Yr 2nd Est Tax Qtr")
                 (N_ "Apr 1 - May 31, Last year")))
          (list->vector
           (list '3rd-last (N_ "Last Yr 3rd Est Tax Qtr")
		 ;; Translators: The US tax quarters are different from
		 ;; actual year's quarters! See the definition of
		 ;; tax-qtr-real-qtr-year variable above.
                 (N_ "Jun 1 - Aug 31, Last year")))
          (list->vector
           (list '4th-last (N_ "Last Yr 4th Est Tax Qtr")
                 (N_ "Sep 1 - Dec 31, Last year"))))))

  (gnc:register-tax-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Select Accounts (none = all)")
    "d" (N_ "Select accounts")
    (lambda () '())
    #f #t))
  
  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Suppress $0.00 values")
    "f" (N_ "$0.00 valued Tax codes won't be printed.") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not print full account names")
    "g" (N_ "Do not print all Parent account names") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Print all Transfer To/From Accounts")
    "h" (N_ "Print all split details for multi-split transactions") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Print TXF export parameters")
    "i" (N_ "Show TXF export parameters for each TXF code/account on report") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not print Action:Memo data")
    "j" (N_ "Do not print Action:Memo data for transactions") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not print transaction detail")
    "k" (N_ "Do not print transaction detail for accounts") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Do not use special date processing")
    "l" (N_ "Do not print transactions out of specified dates") #f))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Shade alternate transactions")
    "m" (N_ "Shade background of alternate transactions, if more than one displayed") #f))

  (gnc:register-tax-option
   (gnc:make-multichoice-option
    gnc:pagename-display (N_ "Currency conversion date")
    "n" (N_ "Select date to use for PriceDB lookups")
    'conv-to-tran-date
    (list (list->vector
           (list 'conv-to-tran-date (N_ "Nearest transaction date") (N_ "Use nearest to transaction date")))
          (list->vector
           (list 'conv-to-report-date (N_ "Nearest report date") (N_ "Use nearest to report date")))
    )))

  (gnc:options-set-default-section options gnc:pagename-general)

  options)

;; Render txf information
(define crlf (string #\return #\newline)) ; TurboTax seems to want these

(define txf-last-payer "")		; if same as current, inc txf-l-count
					; this only works if different
					; accounts from the same payer are
					; grouped in the accounts list
(define txf-l-count 0)		; count repeated N codes
(define txf-account-name "")

(define (gnc:account-get-txf account)
  (and (xaccAccountGetTaxRelated account)
       (not (equal? (gnc:account-get-txf-code account) 'N000))))

(define (gnc:account-get-txf-code account)
  (let ((code (xaccAccountGetTaxUSCode account)))
       (string->symbol (if (string-null? code) "N000" code))))

(define (gnc:get-txf-format code income?)
  (gnc:txf-get-format (if income?
                          txf-income-categories
                          txf-expense-categories)
                      code))

(define (gnc:get-txf-multiple code income?)
  (gnc:txf-get-multiple (if income?
                          txf-income-categories
                          txf-expense-categories)
                      code))

(define (gnc:get-txf-pns code income?)
  (gnc:txf-get-payer-name-source (if income?
                          txf-income-categories
                          txf-expense-categories)
                      code))

(define (gnc:account-get-txf-payer-source account)
  (let ((pns (xaccAccountGetTaxUSPayerNameSource account)))
       (string->symbol (if (string-null? pns) "none" pns))))

;; some codes require special handling
(define (txf-special-split? code)
  (member code (list 'N521)))   ;only one for now Federal estimated tax, qrtrly

(define (fill-clamp-sp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 (- len 1)) " "))

(define (fill-clamp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 len)))

(define (render-header-row table heading-line-text)
  (let ((heading (gnc:make-html-text)))
       (gnc:html-text-append! heading (gnc:html-markup-b heading-line-text)) 
       (let ((heading-cell (gnc:make-html-table-cell heading)))
            (gnc:html-table-cell-set-colspan! heading-cell 5)
            (gnc:html-table-append-row!
             table
             (append (list heading-cell)
                     (list (gnc:make-html-table-cell "&nbsp; &nbsp;")))
            )
       )
  )
)

(define (render-account-detail-header-row table suppress-action-memo?)
  (gnc:html-table-append-row!
       table
       (append (list (gnc:make-html-table-header-cell
                      (_ "Date")))
               (list (gnc:make-html-table-header-cell
                      (_ "Num")))
               (list (gnc:make-html-table-header-cell
                      (_ "Description")))
               (list (gnc:make-html-table-header-cell
                      (if suppress-action-memo?
                          (_ "Notes")
                          (_ "Notes/Action:Memo"))))
               (list (gnc:make-html-table-header-cell
                      (_ "Transfer To/From Account(s)")))
               (list (gnc:make-html-table-header-cell/markup
                      "number-header" (_ "Amount")))
       )
  )
)

(define (render-total-row table total-amount total-line-text
                          tax_code? transaction-details?)
  (let ((description (gnc:make-html-text))
        (total (gnc:make-html-text)))
       (if (or tax_code? transaction-details?)
           (gnc:html-text-append! description (gnc:html-markup-b 
              (string-append "&nbsp; &nbsp; &nbsp; &nbsp;" (_ "Total For "))))
           (if (not tax_code?)
               (gnc:html-text-append! description (gnc:html-markup-b 
                  "&nbsp; &nbsp; &nbsp; &nbsp;"))
           )
       )
       (gnc:html-text-append! description (gnc:html-markup-b 
              total-line-text))
       (gnc:html-text-append! description (gnc:html-markup-b 
              (_ " ")))
       (gnc:html-text-append! total (gnc:html-markup-b
              total-amount))
       (let ((description-cell (if (or tax_code? transaction-details?)
                                   (gnc:make-html-table-cell/markup
                                        "account-total" description)
                                   (gnc:make-html-table-cell description))))
            (gnc:html-table-cell-set-colspan! description-cell 5)
            (gnc:html-table-append-row!
             table
             (append (list description-cell)
                     (list (gnc:make-html-table-cell/markup
                            "number-cell" total))))
       ) ;; end of let
  ) ;; end of let
)

(define (render-txf-account account account-value d? date x? x-date
                            type code)
  (let* ((print-info (gnc-account-print-info account #f))
         (value (xaccPrintAmount account-value print-info))
         (txf? (gnc:account-get-txf account)))
    (if (and txf?
             (not (gnc-numeric-zero-p account-value)))
        (let* ((date-str (if date
                             (strftime "%m/%d/%Y" (localtime (car date)))
                             #f))
               (x-date-str (if x-date
                               (strftime "%m/%d/%Y" (localtime (car x-date)))
                               #f))
               ;; Only formats 1,3 implemented now! Others are treated as 1.
               (format (gnc:get-txf-format code (eq? type ACCT-TYPE-INCOME)))
               (action (if (eq? type ACCT-TYPE-INCOME)
                           (case code
                             ((N286 N488) "ReinvD")
                             (else "Income"))
                           "Expense"))
               (category-key (if (eq? type ACCT-TYPE-INCOME)
                                 (gnc:txf-get-category-key 
                                  txf-income-categories code)
                                 (gnc:txf-get-category-key
                                  txf-expense-categories code)))
               (value-name (if (equal? "ReinvD" action)
                               (string-append 
                                (xaccPrintAmount (gnc-numeric-neg account-value)
                                                 print-info)
                                " " txf-account-name)
                               txf-account-name))
               (value (string-append "$"  ; reverse signs on dr's & cr's
                                     (xaccPrintAmount
                                                 (gnc-numeric-neg account-value)
                                                 print-info)))
          )
          (list (if x? "TD" "TS") crlf
                (symbol->string code) crlf
                "C1" crlf
                "L" (number->string txf-l-count) crlf
                (if d?
                    (list "D" date-str crlf)
                    '())
                value crlf 
                (case format
                  ((3) (list "P" txf-account-name crlf))
                  (else (if (and x? (txf-special-split? code))
                            (list "P" crlf)
                            '())))
                (if x?
                    (list "X" x-date-str " " (fill-clamp-sp txf-account-name 31)
                          (fill-clamp-sp action 7) 
                          (fill-clamp-sp value-name 82)
                          (fill-clamp category-key 15) crlf)
                    '())
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
    ((splt-value (if (or (gnc-commodity-equiv trans-currency USD-currency)
                         (gnc-commodity-equiv account-commodity USD-currency))
                     (xaccSplitGetValue split)
                     (gnc:make-gnc-numeric 100 100)))
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
                                        (gnc-pricedb-lookup-nearest-in-time
                                          pricedb
                                          account-commodity
                                          USD-currency
                                          (timespecCanonicalDayTime
                                                                  lookup-date)))
                                (set! pricedb-lookup-price-value
                                        (gnc-price-get-value
                                                          pricedb-lookup-price))
                                (set! pricedb-lookup-price-time
                                        (gnc-price-get-time
                                                          pricedb-lookup-price))
                                (gnc-pricedb-convert-balance-nearest-price
                                        pricedb
                                        (if neg?
                                            (gnc-numeric-neg splt-rpt-amount)
                                            splt-rpt-amount)
                                        account-commodity
                                        USD-currency
                     ;; Use midday as the transaction time so it matches a price
                     ;; on the same day.  Otherwise it uses midnight which will
	                 ;; likely match a price on the previous day
                                        (timespecCanonicalDayTime lookup-date))
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
                            (_ "(Missing priceDB entry to convert ")
                            (gnc-commodity-get-mnemonic account-commodity)
                            " "
                            converted-qty
                            (_ " to USD. Go to the Tools->Price Editor dialog to enter prices. Set to zero for this report.)")
                          )
                          (string-append
                            (_ "(Converted ")
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
                                (string-append (_ " @ PriceDB lookup rate of "))
                                (string-append
                                   (_ " @ transaction split rate of ")
                                   (xaccPrintAmount
                                       (if (not (gnc-commodity-equiv
                                                           trans-currency
                                                           USD-currency))
                                           (gnc-numeric-div
                                               (gnc:make-gnc-numeric 100 100)
                                               (xaccSplitGetSharePrice split)
                                               GNC-DENOM-AUTO
                                               (logior (GNC-DENOM-SIGFIGS 6)
                                                       GNC-RND-ROUND))
                                           (xaccSplitGetSharePrice split)
                                       )
                                       print-info)
                                   (_ ")")
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
                                 (_ " on ")
                                 (strftime "%Y-%b-%d"
                                    (localtime (car pricedb-lookup-price-time)))
                                 (_ ")")
                               )
                               ""))
     )
    ) 
    (list amount conversion-text pricedb-lookup-price conversion-text2)
  )
)

(define (process-transaction-multi-transfer-detail split parent
            USD-currency full-names? trans-date trans-currency acct-type
            currency-conversion-date to-date transfer-table print-amnt)
  (let* ((all-tran-splits (xaccTransGetSplitList parent))
         (tran-splits-to-render (- (length all-tran-splits) 1))
         (trans-rpt-currency-total (gnc-numeric-zero)) ;;for USD-currency
        )
        (map (lambda (tran-split)
             (if (not (xaccSplitEqual split tran-split #t #f #f))
                 (let* ((split-acct (xaccSplitGetAccount tran-split))
                        (split-acct-commodity
                                           (xaccAccountGetCommodity split-acct))
                        (splt-amnt (xaccSplitGetAmount tran-split))
                        (splt-amnt (if (eq? acct-type ACCT-TYPE-INCOME)
                                            splt-amnt
                                            (gnc-numeric-neg splt-amnt)))
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
                                                     (gnc-account-print-info
                                                                 split-acct #f)
                                                     (if (eq? acct-type
                                                               ACCT-TYPE-INCOME)
                                                          #f
                                                          #t))
                                             ))
                        (splt-print-amnt (car splt-curr-conv-data))
                        (splt-account-name (if full-names?
                                               (xaccAccountGetFullName split-acct)
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
                       (gnc:html-table-append-row!
                         transfer-table
                         (append
                           (list cell)
                           (list (gnc:make-html-table-cell/markup
                                 "num-cell-align-bot" (gnc:html-split-anchor
                                                     tran-split
                                                     splt-print-amnt))
                           )
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
                                 (_ "Multiple currency conversion differences"))
                   (conversion-cell (gnc:make-html-table-cell
                                        conversion-text-content)))
                  (gnc:html-table-append-row!
                     transfer-table
                     (append
                       (list conversion-cell)
                       (list (gnc:make-html-table-cell/markup
                             "num-cell-align-bot" (gnc-numeric-add-fixed
                                                     print-amnt
                                                     (gnc-numeric-neg
                                                      trans-rpt-currency-total)))
                       )
                     )
                  )
            )
        ) ;; end of if
  ) ;; end of let*
)

;; Render transaction detail
(define (process-account-transaction-detail table account split-list
                split-details? full-names? currency-conversion-date to-date
                transaction-details? suppress-action-memo?
				shade-alternate-transactions? splits-period full-year? to-value
                tax-mode? show-TXF-data? USD-currency account-type
                tax-code acct-full-name)
                                                                                    
  (let*
    ((account-commodity (xaccAccountGetCommodity account))
     (format (gnc:get-txf-format tax-code (eq? account-type ACCT-TYPE-INCOME)))
     (payer-src (gnc:account-get-txf-payer-source account))
     (code-pns (gnc:get-txf-pns tax-code (eq? account-type ACCT-TYPE-INCOME)))
     (l-value (begin
                 (set! txf-account-name (xaccAccountGetName
                                            (if (eq? payer-src 'parent)
                                                (gnc-account-get-parent account)
                                                account)))
                 (set! txf-l-count (if (= format 3)
                                       (if (equal? txf-last-payer
                                                               txf-account-name)
                                           txf-l-count
                                           (if (equal? "" txf-last-payer)
                                               1
                                               (+ 1 txf-l-count)))
                                       1))
                 (set! txf-last-payer (if (= format 3)
                                          txf-account-name
                                          ""))
                 (number->string txf-l-count)))
     (acct-collector (gnc:make-commodity-collector))
     (acct-collector-as-dr (gnc:make-commodity-collector))
     (account-commodity-total (gnc-numeric-zero))
     (account-commodity-total-as-dr (gnc-numeric-zero))
     (account-USD-total (gnc-numeric-zero))
     (account-desc (string-append
                      acct-full-name
                      (if (gnc-commodity-equiv account-commodity USD-currency)
                          ""
                          (string-append (_ " (Account Commodity: ")
                                  (gnc-commodity-get-mnemonic account-commodity)
                                  (_ ")")))
                      (if show-TXF-data?
                          (let* ((pns (if (or (eq? 'parent code-pns)
                                              (eq? 'current code-pns))
                                          (if (eq? 'parent payer-src)
                                              (_ "Name Source is Parent")
                                              (_ "Name Source is Current"))
                                          ""))
                                 (line (if (= format 3)
                                           (string-append (_ "Line ") l-value)
                                           ""))
                                )
                                (if (eq? pns "")
                                    (if (eq? line "")
                                        ""
                                        (string-append
                                          (_ " (TXF Parameter: ") line (_ ")")))
                                    (if (eq? line "")
                                        (string-append
                                          (_ " (TXF Parameter: ") pns (_ ")"))
                                        (string-append
                                          (_ " (TXF Parameters: ") pns (_", ")
                                          line (_ ")"))))
                          )
                          "")))
     (print-info (gnc-account-print-info account #f))
     (shade-this-line? #f)
     (output '())
    )
    (acct-collector 'reset #f #f)  ;initialize to zero for this account
    (if (and transaction-details? tax-mode?)
        (begin
          (render-header-row table (string-append
                                   "&nbsp; &nbsp; &nbsp; &nbsp;" account-desc))
          (render-account-detail-header-row table suppress-action-memo?)))
    (set! output
      (map (lambda (split) 
           (let* ((parent (xaccSplitGetParent split))
                  (trans-date (gnc-transaction-get-date-posted parent))
                  ;; TurboTax 1999 and 2000 ignore dates after Dec 31
                  (fudge-date (if splits-period
                                  (if (and full-year? 
                                           (gnc:timepair-lt to-value trans-date))
                                      to-value
                                      trans-date)
                                  trans-date))
                  (notes (xaccTransGetNotes parent))
                  (action (if suppress-action-memo?
                              ""
                              (xaccSplitGetAction split)))
                  (memo  (if suppress-action-memo?
                             ""
                             (xaccSplitGetMemo split)))
                  (action-memo (if (and (string=? action "") (string=? memo ""))
                                   ""
                                   (begin
                                     (string-append (_ "/") action
                                       (if (string=? memo "")
                                           ""
                                           (string-append (_ ":") memo))))))
                  (notes-act-memo (string-append notes action-memo))
                  (trans-currency (xaccTransGetCurrency parent))
                  (splt-amount (xaccSplitGetAmount split))
                  (splt-amount-is-dr? (if (gnc-numeric-positive-p splt-amount)
                                          #t
                                          #f))
                  (splt-rpt-amount (if (eq? account-type ACCT-TYPE-INCOME)
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
                                             to-date)
                                         trans-currency
                                         splt-rpt-amount
                                         print-info
                                         (if (eq? account-type ACCT-TYPE-INCOME)
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
                                          (_ "Split")
                                          (if full-names?
                                              (xaccAccountGetFullName
                                                                  other-account)
                                              (xaccAccountGetName other-account)
                                          )))
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
                     (if (eq? account-type ACCT-TYPE-INCOME)
                         (gnc-numeric-neg splt-amount)
                         splt-amount))              
                 (acct-collector-as-dr 'add account-commodity splt-amount)
                 (set! account-USD-total (gnc-numeric-add-fixed
                                              account-USD-total print-amnt))
                 (if (and transaction-details? tax-mode?)
                     (begin
                       (if (and (null? other-account) split-details?)
                           (process-transaction-multi-transfer-detail
                                             split parent
                                             USD-currency
                                             full-names?
                                             trans-date
                                             trans-currency 
                                             account-type
                                             currency-conversion-date
                                             to-date
                                             transfer-table print-amnt))
                       (gnc:html-table-append-row!
                            date-table
                            (gnc:make-html-table-cell
                                             (strftime "%Y-%b-%d"
                                                 (localtime (car trans-date)))))
                       (gnc:html-table-append-row!
                            num-table
                            (gnc:make-html-table-cell (xaccTransGetNum parent)))
                       (gnc:html-table-append-row!
                            desc-table
                            (gnc:make-html-table-cell
                                            (xaccTransGetDescription parent)))
                       (gnc:html-table-append-row!
                            notes-table
                            (gnc:make-html-table-cell notes-act-memo))
                       (gnc:html-table-set-style! transfer-table "table" 
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
                                                "just-right" curr-conv-note))
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
                       (gnc:html-table-append-row!
                            amount-table
                            (gnc:make-html-table-cell/markup
                                 "num-cell-align-bot" (gnc:html-split-anchor
                                                             split print-amnt)))
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
                            (append (list (gnc:make-html-table-cell
                                               date-table))
                                    (list (gnc:make-html-table-cell
                                               num-table))
                                    (list (gnc:make-html-table-cell
                                               desc-table))
                                    (list (gnc:make-html-table-cell
                                               notes-table))
                                    (list (gnc:make-html-table-cell/markup
                                               "just-bot" transfer-table))
                                    (list (gnc:make-html-table-cell/markup
                                             "num-cell-align-bot" amount-table))
                            )
                       )
                     ) ;; end of begin
                 ) ;; end of if
                 ;; for quarterly estimated tax payments, we need to go
                 ;; get data from splits for TXF output
                 (if (and splits-period (not tax-mode?))
                     (render-txf-account account
                                       (if (or (and print-amnt-is-dr?
                                                    splt-amount-is-dr?)
                                               (and (not print-amnt-is-dr?)
                                                    (not splt-amount-is-dr?))
                                           )
                                           print-amnt
                                           (gnc-numeric-neg print-amnt))
                                       #t fudge-date  #t trans-date
                                       account-type tax-code)
                 )
           ) ;;end of let*
           ) ;;end of lambda
      split-list) ;;end of map
    ) ;; end of set!
    ;; print account totals
    (set! account-commodity-total (gnc-numeric-add-fixed account-commodity-total
                         (cadr (acct-collector 'getpair account-commodity #f))))
    (set! account-commodity-total-as-dr (gnc-numeric-add-fixed
                                                   account-commodity-total-as-dr
                   (cadr (acct-collector-as-dr 'getpair account-commodity #f))))
    (if tax-mode?
        (let* ((account-name (if full-names?
                                 acct-full-name
                                 (xaccAccountGetName account)))
               (amnt-acct-curr (xaccPrintAmount account-commodity-total
                                                print-info))
               (account-total-amount (xaccPrintAmount account-USD-total
                                                print-info))
               (account-total-line-text
                  (string-append (if transaction-details?
                                     (_ "Account: ")
                                     "")
                                 account-name
                                 (if (not (gnc-commodity-equiv account-commodity
                                                               USD-currency))
                                     (string-append (_ " (")
                                                    amnt-acct-curr
                                                    (_ "  In ")
                                                    (gnc-commodity-get-mnemonic
                                                              account-commodity)
                                                    (_ ") "))
                                     "")))
              )
              (render-total-row table
                                account-total-amount
                                account-total-line-text
                                #f
                                transaction-details?)
        ) ;; end of let*
    ) ;; end of if
    (list account-USD-total
          output
          (if (or (and (gnc-numeric-positive-p account-USD-total)
                       (gnc-numeric-positive-p account-commodity-total-as-dr))
                  (and (gnc-numeric-negative-p account-USD-total)
                       (gnc-numeric-negative-p account-commodity-total-as-dr)))
              account-USD-total
              (gnc-numeric-neg account-USD-total)))
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


  ;; List of entries, each containing a form, tax-code (as string), account name
  ;; account, and, to avoid having to fetch again later, type and code as 
  ;; symbol. Only accounts with a tax code are put on list.
  (define (make-form-line-acct-list accounts)
     (map (lambda (account)
                  (let* ((account-name (gnc-account-get-full-name account))
                         (children (gnc-account-get-children account)))
                    (if (string-null? (xaccAccountGetTaxUSCode account))
                        selected-accounts-sorted-by-form-line-acct
                        (let* ((type (xaccAccountGetType account))
                               (tax-code (gnc:account-get-txf-code account))
                               (form (gnc:txf-get-form (if (eq? type
                                                               ACCT-TYPE-INCOME)
                                            txf-income-categories
                                            txf-expense-categories) tax-code))
                               (form-code-acct (list (list form)
                                                     (list (symbol->string
                                                                      tax-code))
                                                     (list account-name)
                                                     (list account)
                                                     (list type)
                                                     (list tax-code))))
                              (set! selected-accounts-sorted-by-form-line-acct
                                      (append (list form-code-acct)
                                    selected-accounts-sorted-by-form-line-acct))
                        )
                    )
                    (if (not (null? children))
                        (make-form-line-acct-list children)
                        selected-accounts-sorted-by-form-line-acct
                    )
                  )
          )
      accounts)
  )

  (define (form-line-acct-less a b)
     (let ((string-a (string-append
                         (car (car a)) " " (car (cadr a)) " " (car (caddr a))))
           (string-b (string-append
                         (car (car b)) " " (car (cadr b)) " " (car (caddr b)))))
          (if (string<? string-a string-b)
              #t
              #f
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
         (to-value (gnc:timepair-end-day-time
                    (gnc:date-option-absolute-time 		       
                     (get-option gnc:pagename-general "To"))))
         (alt-period (get-option gnc:pagename-general "Alternate Period"))
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
         (suppress-action-memo? (get-option gnc:pagename-display 
                                 "Do not print Action:Memo data"))
         (shade-alternate-transactions? (get-option gnc:pagename-display 
                                 "Shade alternate transactions"))
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
         (from-date (gnc:timepair->date from-value))
         (from-value (gnc:timepair-start-day-time
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
                        (cons (car (mktime bdtm)) 0))))

         (to-value (gnc:timepair-end-day-time
                    (let ((bdtm from-date))
                      (if (member alt-period 
                                  '(last-year 1st-last 2nd-last
                                              3rd-last 4th-last))
                          (set-tm:year bdtm (- (tm:year bdtm) 1)))
                      ;; Bug! Above subtracts two years, should only be one!
                      ;; The exact same code, in from-value, further above,
                      ;;   only subtraces one!  Go figure!
                      ;; So, we add one back below!
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
                            (else (set! bdtm (gnc:timepair->date to-value))))
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
                             (set! bdtm (gnc:timepair->date to-value)))))
                      (set-tm:isdst bdtm -1)
                      (cons (car (mktime bdtm)) 0))))

         (form-line-acct-header-printed? #f)
         (form-schedule-header-printed? #f)
         (tax-code-header-printed? #f)
         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
        )

    ;; for quarterly estimated tax payments, we need a different period
    ;; return the sometimes changed (from-est to-est full-year?) dates
    (define (txf-special-splits-period account from-value to-value)
      (if (and (xaccAccountGetTaxRelated account)
               (txf-special-split? (gnc:account-get-txf-code account)))
          (let* 
              ((full-year?
                (let ((bdto (localtime (car to-value)))
                      (bdfrom (localtime (car from-value))))
                  (and (equal? (tm:year bdto) (tm:year bdfrom))
                       (equal? (tm:mon bdfrom) 0)
                       (equal? (tm:mday bdfrom) 1)
                       (equal? (tm:mon bdto) 11)
                       (equal? (tm:mday bdto) 31))))
              ;; Adjust dates so we get the final Estimated Tax
              ;; paymnent from the right year
               (from-est (if full-year?
                             (let ((bdtm (gnc:timepair->date
                                          (timespecCanonicalDayTime
                                           from-value))))
                               (set-tm:mday bdtm 1) ; 01
                               (set-tm:mon bdtm 2) ; Mar
                               (set-tm:isdst bdtm -1)
                               (cons (car (mktime bdtm)) 0))
                             from-value))
               (to-est (if full-year?
                           (let* ((bdtm (gnc:timepair->date
                                         (timespecCanonicalDayTime
                                          from-value))))
                             (set-tm:mday bdtm 28) ; 28
                             (set-tm:mon bdtm 1) ; Feb
                             (set-tm:year bdtm (+ (tm:year bdtm) 1))
                             (set-tm:isdst bdtm -1)
                             (cons (car (mktime bdtm)) 0))
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
                            acct-full-name)
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
              (form-line-acct-text (string-append
                                         (_ "Form or Schedule / Line (TXF Code")
                                         (if show-TXF-data?
                                             (_ ": Parameters")
                                             "")
                                         (_ ") / Account Name")))
             )
             (if (> (length split-list) 0)
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
                                 (render-header-row table current-form-schedule)
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
                                               to-value
                                               tax-mode?
                                               show-TXF-data?
                                               USD-currency
                                               account-type
                                               tax-code
                                               acct-full-name))
                           (tran-txf (cadr tran-output))
                           (account-USD-total-as-dr (caddr tran-output))
                          )
                          (set! account-USD-total (car tran-output))
                          (list
                            account-USD-total
                            (if (not to-special)
                                (if (not tax-mode?)
                                    (render-txf-account account
                                            account-USD-total-as-dr
                                                  #f #f #t from-value
                                                  account-type tax-code)
                                    '())
                                tran-txf)
                            account-USD-total-as-dr
                          )
                          
                    )
                 )
                 (begin ;; no split case
                    (if suppress-0?
                        (list account-USD-total
                              '()
                              account-USD-total
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
                                 account-USD-total)
                        )
                    )
                 )
             )
          ) ;; end of let*
    )

    (let ((from-date  (strftime "%Y-%b-%d" (localtime (car from-value))))
          (to-date    (strftime "%Y-%b-%d" (localtime (car to-value))))
          (today-date (strftime "D%m/%d/%Y" 
                                (localtime 
                                 (car (timespecCanonicalDayTime
                                       (cons (current-time) 0))))))
          (prior-form-schedule "")
          (prior-tax-code "")
          (prior-account #f)
          (tax-code-USD-total (gnc-numeric-zero))
          (tax-code-USD-total-as-dr (gnc-numeric-zero))
          (saved-tax-code-text "")
          (need-form-line-acct-header? #f)
          (need-form-schedule-header? #f)
          (need-tax-code-header? #f)
          (tax-code-heading-text "")
          (tax-code-text "")
         )

         (define (handle-tax-code form-line-acct)
            (let* ((current-form-schedule (car (car form-line-acct)))
                   (current-tax-code (car (cadr form-line-acct))) ;; string
                   (acct-full-name (car (caddr form-line-acct)))
                   (account (car (car (cdddr form-line-acct))))
                   (type (car (car (cdr (cdddr form-line-acct)))))
                   (tax-code (car (cadr (cdr (cdddr form-line-acct))))) ;;symbol
                   (output '())
                   (txf-pyr (if (eq? (gnc:account-get-txf-payer-source account)
                                     'parent)
					       (xaccAccountGetName (gnc-account-get-parent account))
					       (xaccAccountGetName account)))
                   (txf-new-payer? (if (string=? txf-last-payer txf-pyr)
                                       #f
                                       #t))
                  )
                  ;; process prior tax code break, if appropriate, before
                  ;; processing current account 
                  (if (string=? prior-tax-code "")
                      #t ;; do nothing
                      (if tax-mode?
                         ;; printed report processing
                         (if (string=? prior-tax-code current-tax-code)
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
                                       ) 
                                       ;; print prior tax-code total and
                                       ;; reset accum
                                       (render-total-row
                                                 table
                                                 tax-code-total-amount
                                                 (string-append
                                                     (_ "Line (Code): ")
                                                     saved-tax-code-text
                                                 )
                                                 #t
                                                 transaction-details?
                                       )
                                       (set! tax-code-USD-total
                                                             (gnc-numeric-zero))
                                       (set! tax-code-USD-total-as-dr
                                                             (gnc-numeric-zero))
                                       (set! txf-l-count 0)
                                 )
                             )
                         )
                         ;; txf output processing
                         (if (gnc-numeric-zero-p tax-code-USD-total-as-dr)
                             #t ;; do nothing
                             (if (or ;; tax-code break
                                     (not (string=?
                                                prior-tax-code current-tax-code)
                                     )
                                     ;; not tax-code break, but tax-code allows
                                     ;; multiple lines and there is a new payer
                                     (and
                                      (string=? prior-tax-code current-tax-code)
                                      (gnc:get-txf-multiple
                                                  (gnc:account-get-txf-code
                                                                  prior-account)
                                                  (eq? (xaccAccountGetType
                                                                  prior-account)
                                                       ACCT-TYPE-INCOME))
                                      txf-new-payer?
                                     )
                                 )
                                 (begin
                                    (set! output
                                          (list (render-txf-account
                                              prior-account
                                              tax-code-USD-total-as-dr
                                              #f #f #f #f
                                              (xaccAccountGetType prior-account)
                                              (gnc:account-get-txf-code
                                                               prior-account))))
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
                                 #f ;; do nothing
                             )
                         )
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
                        (set! form-line-acct-header-printed? #f)
                        (set! form-schedule-header-printed? #f)
                        (set! prior-form-schedule current-form-schedule)
                     )
                  )
                  (if (string=? prior-tax-code current-tax-code)
                      (if tax-code-header-printed?
                          (set! need-tax-code-header? #f)
                          (set! need-tax-code-header? #t)
                      )
                      (begin ;; if new tax-code
                         (let ((description (gnc:txf-get-description
                                                (if (eq? type ACCT-TYPE-INCOME)
                                                    txf-income-categories
                                                    txf-expense-categories)
                                                tax-code))
                              )
                              (set! need-tax-code-header? #t)
                              (set! tax-code-header-printed? #f)
                              (set! tax-code-text
                                    (string-append description (_ " (")
                                                   current-tax-code (_ ")")))
                              (set! tax-code-heading-text
                                    (string-append description (_ " (")
                                      current-tax-code
                                      (if show-TXF-data?
                                          (string-append
                                            (_ ": Payer Name Option ")
                                            (if (or (eq? 'parent
                                                     (gnc:get-txf-pns tax-code
                                                          (eq? ACCT-TYPE-INCOME
                                                               type)))
                                                    (eq? 'current
                                                     (gnc:get-txf-pns tax-code
                                                          (eq? ACCT-TYPE-INCOME
                                                               type))))
                                                (_ "Y")
                                                (_ "N"))
                                            (_ ", TXF Format ")
                                            (number->string 
                                                    (gnc:get-txf-format tax-code
                                                         (eq? ACCT-TYPE-INCOME
                                                              type)))
                                            (_ ", Multiple Copies ")
                                            (if (gnc:get-txf-multiple tax-code
                                                    (eq? ACCT-TYPE-INCOME type))
                                                (_ "Y")
                                                (_ "N"))
                                            (_ ", Special Dates ")
                                            (if (txf-special-split? tax-code)
                                                (_ "Y")
                                                (_ "N"))
                                          )
                                      "")
                                    (_ ")")))
                         )
                         (set! saved-tax-code-text tax-code-text)
                      )
                  )
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
                                                 acct-full-name))
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
                        (set! tax-code-USD-total-as-dr (gnc-numeric-add-fixed
                                                       tax-code-USD-total-as-dr
                                                       account-USD-total-as-dr))
                        (set! need-form-line-acct-header? #f)
                        (set! need-form-schedule-header? #f)
                        (set! need-tax-code-header? #f)
                        (set! work-done (+ 1 work-done))
                            (gnc:report-percent-done
                                    (* 100 (if (> work-to-do 0)
                                               (/ work-done work-to-do)
                                               1)))
                        (set! prior-tax-code current-tax-code)
                        (set! prior-account account)
                        (if tax-mode?
                            '()
                            code-tfx-output)
                  ) ;; end of let
            ) ;; end of let*
         )

      ;; Now, the main body
      (set! selected-accounts-sorted-by-form-line-acct '())
      (make-form-line-acct-list selected-accounts)
      (set! selected-accounts-sorted-by-form-line-acct
         (sort-list
             selected-accounts-sorted-by-form-line-acct
             form-line-acct-less
         ))
      (set! work-to-do (length selected-accounts-sorted-by-form-line-acct))

      (if (not tax-mode?) ; Do Txf mode
          (if file-name		; cancel TXF if no file selected
              (let ((port (catch #t ;;e.g., system-error
                                 (lambda () (open-output-file file-name))
                                 (lambda (key . args)
                                    (gnc-error-dialog
                                          '()
                                          (string-append
                                              (_ "Could not open the file: ")
                                              file-name
                                              (_ ". The error is: ")
                                              (symbol->string key)
                                              (_ " - ")
                                              (car (caddr args))
                                              (_ ".")
                                          ))
                                     #f)))
                   )
                   (if port ;; port opened successfully
                       (let* ((output (map (lambda (form-line-acct)
                                            (handle-tax-code form-line-acct))
                                    selected-accounts-sorted-by-form-line-acct))
                              (output-txf
                                (list
                                  "V037" crlf
                                  "AGnuCash " gnc:version crlf
                                  today-date crlf
                                  "^" crlf
                                  output
                                  (if (or 
                                         (gnc-numeric-zero-p tax-code-USD-total)
                                         (not prior-account))
                                      '()
                                      (render-txf-account prior-account
                                              tax-code-USD-total-as-dr
                                              #f #f #f #f
                                              (xaccAccountGetType prior-account)
                                              (gnc:account-get-txf-code
                                                                prior-account)))
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
                                                           "taxschedule.scm - ")
                                 #f) 
                             (close-output-port port)
                             #t
                       ) ; end of let
                       ;; Could not open port successfully
                       #t ;; to prevent 2nd error dialog in gnc_plugin_page_report_export_cb
                   ) ;; end of if
              ) ;; end of let*
          #f) ;;end of if
          (begin  ; else do tax report
             (gnc:html-document-set-style!
              doc "account-total"
              'tag "th"
              'attribute (list "align" "right"))

             (gnc:html-document-set-style!
              doc "num-cell-align-bot"
              'tag "td"
              'attribute (list "align" "right")
              'attribute (list "valign" "bottom"))

             (gnc:html-document-set-style!
              doc "just-right"
              'tag "td"
              'attribute (list "align" "right"))

             (gnc:html-document-set-style!
              doc "just-bot"
              'tag "td"
              'attribute (list "valign" "bottom"))

             (gnc:html-document-set-style!
              doc "tran-detail"
              'tag "tr"
              'attribute (list "valign" "top"))

             (gnc:html-document-set-style!
              doc "tran-detail-shade"
              'tag "tr"
              'attribute (list "valign" "top")
              'attribute (list "bgcolor" "grey"))

             (gnc:html-document-set-title! doc report-name)

             (gnc:html-document-add-object! 
              doc (gnc:make-html-text         
                   (gnc:html-markup 
                    "center"
                    (gnc:html-markup-p
                     (gnc:html-markup/format
                      (_ "Period from %s to %s<BR>All amounts in USD unless otherwise noted")
                           from-date
                           to-date
                     )))))

             (gnc:html-document-add-object! doc table)

             (map (lambda (form-line-acct) (handle-tax-code form-line-acct))
                  selected-accounts-sorted-by-form-line-acct)

             ;; print final tax-code totals
             (if (or (and suppress-0? (gnc-numeric-zero-p tax-code-USD-total))
                     (null? selected-accounts)
                     (not prior-account))
                 #t ;; do nothing
                 (let* ((print-info (gnc-account-print-info prior-account #f))
                        (tax-code-total-amount (xaccPrintAmount
                                                           tax-code-USD-total
                                                           print-info))
                       ) 
                       (render-total-row table tax-code-total-amount
                                              (string-append (_ "Line (Code): ")
                                                            saved-tax-code-text)
                                              #t
                                              transaction-details?
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
                     (_ "No Tax Related accounts were found with your account selection. Change your selection or go to the Edit->Tax Options dialog to set up tax-related accounts."))))
                 ;; or print selected report options
                 (gnc:html-document-add-object! 
                  doc (gnc:make-html-text         
                        (gnc:html-markup-p
                         (gnc:html-markup/format
                          (string-append
                             (_ "Selected Report Options:<BR>")
                             ;; selected accounts
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; suppress 0.00 values
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; full acct names
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; transfer detail
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; TXF detail
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; action:memo detail
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; transaction detail
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; special dates
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; alternate transaction shading
                             "&nbsp; &nbsp; &nbsp; %s <BR>"
                             ;; currency conversion date
                             "&nbsp; &nbsp; &nbsp; %s <BR>")
                             (if (not (null? user-sel-accnts))
                                 (_ "Subset of accounts")
                                 (_ "No accounts (none = all accounts)"))
                             (if suppress-0?
                                 (_ "Suppress $0.00 valued Tax Codes")
                                 (_ "Do not suppress $0.00 valued Tax Codes"))
                             (if full-names?
                                 (_ "Display full account names")
                                 (_ "Do not display full account names"))
                             (if split-details?
                                 (_ "Display all Transfer To/From Accounts")
                                 (_ "Do not display all Transfer To/From Accounts"))
                             (if show-TXF-data?
                                 (_ "Print TXF export parameters")
                                 (_ "Do not Print TXF export parameters"))
                             (if suppress-action-memo?
                                 (_ "Do not display Action:Memo data")
                                 (_ "Display Action:Memo data"))
                             (if transaction-details?
                                 (_ "Display transactions for selected accounts")
                                 (_ "Do not display transactions for selected accounts"))
                             (if no-special-dates?
                                 (_ "Do not include transactions outside of selected dates")
                                 (_ "Include some transactions outside of selected dates"))
                             (if shade-alternate-transactions?
                                 (_ "Shade alternate transactions")
                                 (_ "Do not shade alternate transactions"))
                             (if (equal? currency-conversion-date
                                         'conv-to-tran-date)
                                 (_ "PriceDB lookups nearest to transaction date")
                                 (_ "PriceDB lookups nearest to report end date"))
                          )
                        ))))

             (gnc:report-finished)
             doc
          ) ;end begin
      ) ;end if
	)))

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
              (_ "This report shows transaction detail for your Taxable Income \
and Deductible Expenses.")
              report-obj
              #t
              #f))
 'export-types (list (cons (_ "TXF") 'txf))
 'export-thunk (lambda (report-obj choice file-name)
                 (generate-tax-schedule
                  (_ "Taxable Income/Deductible Expenses")
                  (_ "This page shows your Taxable Income and \
Deductible Expenses.")
                  report-obj
                  #f
                  file-name)))
