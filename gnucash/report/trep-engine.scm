;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trep-engine.scm : Transaction Report engine
;;
;; Original report by Robert Merkel <rgmerk@mira.net>
;; Contributions by Bryan Larsen <blarsen@ada-works.com>
;; More contributions for new report generation code by Robert Merkel
;; More contributions by Christian Stimming <stimming@tuhh.de>
;; Modified to support the intersection of two account lists by
;; Michael T. Garrison Stuber
;; Modified account names display by Tomas Pospisek
;; <tpo_deb@sourcepole.ch> with a lot of help from "warlord"
;; Refactored by Christopher Lam (2017)
;; - introduced account/transaction substring/regex matcher
;; - add custom sorter in scheme
;; - common currency - optionally show original currency amount
;;   and enable multiple data columns
;; - add support for indenting for better grouping
;; - add subtotal summary grid
;; - by default, exclude closing transactions from the report
;; - converted to module in 2019
;; - CSV export, exports the report headers and totals
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

(define-module (gnucash report trep-engine))

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash utilities))
(use-modules (gnucash report report-core)
             (gnucash report report-utilities)
             (gnucash report options-utilities)
             (gnucash report commodity-utilities)
             (gnucash report html-document)
             (gnucash report html-style-info)
             (gnucash report html-utilities)
             (gnucash report html-table)
             (gnucash report html-text))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))

(export gnc:trep-options-generator)
(export gnc:trep-renderer)
(export gnc:lists->csv)

;; Define the strings here to avoid typos and make changes easier.

;;Accounts
(define optname-accounts (N_ "Accounts"))
(define optname-filterby (N_ "Filter By…"))
(define optname-filtertype (N_ "Filter Type"))

;;Display
(define optname-detail-level (N_ "Detail Level"))
(define optname-grid (N_ "Subtotal Table"))
(define optname-grand-total (N_ "Grand Total"))
;; Translators: a running total is a total that is continually adjusted on every line.
;; To be consistent, also consider how the term "Running Balance" is translated.
;; "Running Totals" is the plural form as it refers to the running total and running subtotals.
;; To be consistent, also consider how the singular form "Running Total" is translated.
(define optname-running-totals (N_ "Running Totals"))

;;Sorting
(define pagename-sorting (N_ "Sorting"))
(define optname-prime-sortkey (N_ "Primary Key"))
(define optname-prime-subtotal (N_ "Primary Subtotal"))
(define optname-prime-sortorder (N_ "Primary Sort Order"))
(define optname-prime-date-subtotal (N_ "Primary Subtotal for Date Key"))
(define optname-full-account-name (N_ "Show Full Account Name"))
(define optname-show-account-code (N_ "Show Account Code"))
(define optname-show-account-description (N_ "Show Account Description"))
(define optname-show-informal-headers (N_ "Show Informal Debit/Credit Headers"))
(define optname-show-subtotals-only
  (N_ "Show subtotals only (hide transactional data)"))
(define optname-indenting (N_ "Add indenting columns"))
(define optname-sec-sortkey (N_ "Secondary Key"))
(define optname-sec-subtotal (N_ "Secondary Subtotal"))
(define optname-sec-sortorder  (N_ "Secondary Sort Order"))
(define optname-sec-date-subtotal (N_ "Secondary Subtotal for Date Key"))

;;General
(define optname-startdate (N_ "Start Date"))
(define optname-enddate (N_ "End Date"))
(define optname-date-source (N_ "Date Filter"))
(define optname-table-export (N_ "Table for Exporting"))
(define optname-infobox-display (N_ "Add options summary"))

;; Currency
(define pagename-currency (N_ "Currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-common-currency (N_ "Common Currency"))
(define optname-orig-currency (N_ "Show original currency amount"))
(define optname-currency (N_ "Report's currency"))

;;Filtering
(define pagename-filter (N_ "Filter"))
(define optname-account-matcher (N_ "Account Name Filter"))
(define optname-account-matcher-regex
  (N_ "Use regular expressions for account name filter"))
(define optname-account-matcher-exclude
  (N_ "Account Name Filter excludes matched strings"))
(define optname-transaction-matcher (N_ "Transaction Filter"))
(define optname-transaction-matcher-regex
  (N_ "Use regular expressions for transaction filter"))
(define optname-transaction-matcher-exclude
  (N_ "Transaction Filter excludes matched strings"))
(define optname-transaction-matcher-caseinsensitive
  (N_ "Transaction Filter is case insensitive"))
(define optname-reconcile-status (N_ "Reconciled Status"))
(define optname-void-transactions (N_ "Void Transactions"))
(define optname-closing-transactions (N_ "Closing transactions"))

;;Styles
(define def:grand-total-style "grand-total")
(define def:normal-row-style "normal-row")
(define def:alternate-row-style "alternate-row")
(define def:primary-subtotal-style "primary-subheading")
(define def:secondary-subtotal-style "secondary-subheading")

(define NO-MATCHING-TRANS-HEADER (G_ "No matching transactions found"))
(define NO-MATCHING-TRANS-TEXT (G_ "No transactions were found that \
match the time interval and account selection specified \
in the Options panel."))

(define DATE-SORTING-TYPES
  (list 'date 'reconciled-date))

(define ACCOUNT-SORTING-TYPES
  (list 'account-name 'corresponding-acc-name
        'account-code 'corresponding-acc-code))

(define SORTKEY-INFORMAL-HEADERS
  (list 'account-name 'account-code))

(define reconcile-list
  (list (cons #\n (G_ "Unreconciled"))
        (cons #\c (G_ "Cleared"))
        (cons #\y (G_ "Reconciled"))
        (cons #\f (G_ "Frozen"))
        (cons #\v (G_ "Voided"))))

(define (sortkey-list parameters)
  ;; Defines the different sorting keys, as an association-list
  ;; together with the subtotal functions. Each entry:
  ;;  'sortkey             - sort parameter sent via qof-query
  ;;  'split-sortvalue     - function retrieves number/string for comparing splits
  ;;  'text                - text displayed in Display tab
  ;;  'renderer-fn         - helper function to select subtotal/subheading renderer
  ;;       behaviour varies according to sortkey.
  ;;       account-types converts split->account
  ;;       #f means the sortkey cannot be subtotalled
  ;;       otherwise it converts split->string
  ;;
  (list (list 'account-name
              (cons 'sortkey (list SPLIT-ACCT-FULLNAME))
              (cons 'split-sortvalue
                    (compose gnc-account-get-full-name xaccSplitGetAccount))
              (cons 'text (G_ "Account Name"))
              (cons 'renderer-fn xaccSplitGetAccount))

        (list 'account-code
              (cons 'sortkey (list SPLIT-ACCOUNT ACCOUNT-CODE-))
              (cons 'split-sortvalue (compose xaccAccountGetCode xaccSplitGetAccount))
              (cons 'text (G_ "Account Code"))
              (cons 'renderer-fn xaccSplitGetAccount))

        (list 'date
              (cons 'sortkey (list SPLIT-TRANS TRANS-DATE-POSTED))
              (cons 'split-sortvalue (compose xaccTransGetDate xaccSplitGetParent))
              (cons 'text (G_ "Date"))
              (cons 'renderer-fn #f))

        (list 'reconciled-date
              (cons 'sortkey (list SPLIT-DATE-RECONCILED))
              (cons 'split-sortvalue xaccSplitGetDateReconciled)
              (cons 'text (G_ "Reconciled Date"))
              (cons 'renderer-fn #f))

        (list 'reconciled-status
              (cons 'sortkey #f)
              (cons 'split-sortvalue (lambda (s)
                                       (length (memv (xaccSplitGetReconcile s)
                                                     (map car reconcile-list)))))
              (cons 'text (G_ "Reconciled Status"))
              (cons 'renderer-fn (lambda (s)
                                   (assv-ref reconcile-list
                                             (xaccSplitGetReconcile s)))))

        (list 'register-order
              (cons 'sortkey (list QUERY-DEFAULT-SORT))
              (cons 'split-sortvalue #f)
              (cons 'text (G_ "Register Order"))
              (cons 'renderer-fn #f))

        (list 'corresponding-acc-name
              (cons 'sortkey (list SPLIT-CORR-ACCT-NAME))
              (cons 'split-sortvalue xaccSplitGetCorrAccountFullName)
              (cons 'text (G_ "Other Account Name"))
              (cons 'renderer-fn (compose xaccSplitGetAccount xaccSplitGetOtherSplit)))

        (list 'corresponding-acc-code
              (cons 'sortkey (list SPLIT-CORR-ACCT-CODE))
              (cons 'split-sortvalue xaccSplitGetCorrAccountCode)
              (cons 'text (G_ "Other Account Code"))
              (cons 'renderer-fn (compose xaccSplitGetAccount xaccSplitGetOtherSplit)))

        (list 'amount
              (cons 'sortkey (list SPLIT-VALUE))
              (cons 'split-sortvalue xaccSplitGetValue)
              (cons 'text (G_ "Amount"))
              (cons 'renderer-fn #f))

        (list 'description
              (cons 'sortkey (list SPLIT-TRANS TRANS-DESCRIPTION))
              (cons 'split-sortvalue (compose xaccTransGetDescription
                                              xaccSplitGetParent))
              (cons 'text (G_ "Description"))
              (cons 'renderer-fn (compose xaccTransGetDescription xaccSplitGetParent)))

        (if (assq-ref parameters 'split-action)
            (list 'number
                  (cons 'sortkey (list SPLIT-ACTION))
                  (cons 'split-sortvalue xaccSplitGetAction)
                  (cons 'text (G_ "Number/Action"))
                  (cons 'renderer-fn #f))

            (list 'number
                  (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
                  (cons 'split-sortvalue (compose xaccTransGetNum xaccSplitGetParent))
                  (cons 'text (G_ "Number"))
                  (cons 'renderer-fn #f)))

        (list 't-number
              (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
              (cons 'split-sortvalue (compose xaccTransGetNum xaccSplitGetParent))
              (cons 'text (G_ "Transaction Number"))
              (cons 'renderer-fn #f))

        (list 'memo
              (cons 'sortkey (list SPLIT-MEMO))
              (cons 'split-sortvalue xaccSplitGetMemo)
              (cons 'text (G_ "Memo"))
              (cons 'renderer-fn xaccSplitGetMemo))

        (list 'notes
              (cons 'sortkey #f)
              (cons 'split-sortvalue (compose xaccTransGetNotes xaccSplitGetParent))
              (cons 'text (G_ "Notes"))
              (cons 'renderer-fn (compose xaccTransGetNotes xaccSplitGetParent)))

        (list 'none
              (cons 'sortkey '())
              (cons 'split-sortvalue #f)
              (cons 'text (G_ "None"))
              (cons 'renderer-fn #f))))

(define (time64-year t64)
  (gnc:date-get-year (gnc-localtime t64)))
(define (time64-quarter t64)
  (+ (* 10 (gnc:date-get-year (gnc-localtime t64)))
     (gnc:date-get-quarter (gnc-localtime t64))))
(define (time64-month t64)
  (+ (* 100 (gnc:date-get-year (gnc-localtime t64)))
     (gnc:date-get-month (gnc-localtime t64))))
(define (time64-week t64)
  (gnc:date-get-week (gnc-localtime t64)))
(define (time64-day t64)
  (+ (* 500 (gnc:date-get-year (gnc-localtime t64)))
     (gnc:date-get-year-day (gnc-localtime t64))))
(define (split->time64 s)
  (xaccTransGetDate (xaccSplitGetParent s)))

(define date-subtotal-list
  ;; List for date option.
  ;; Defines the different date sorting keys, as an association-list. Each entry:
  ;;  'split-sortvalue     - func retrieves number/string used for comparing splits
  ;;  'text                - text displayed in Display tab
  ;;  'renderer-fn         - func retrieves string for subtotal/subheading renderer
  ;;         #f means the date sortkey is not grouped
  ;;         otherwise it converts split->string
  (list
   (list 'none
         (cons 'split-sortvalue #f)
         (cons 'date-sortvalue #f)
         (cons 'text (G_ "None"))
         (cons 'renderer-fn #f))

   (list 'daily
         (cons 'split-sortvalue (lambda (s) (time64-day (split->time64 s))))
         (cons 'date-sortvalue time64-day)
         (cons 'text (G_ "Daily"))
         (cons 'renderer-fn (lambda (s) (qof-print-date (split->time64 s)))))

   (list 'weekly
         (cons 'split-sortvalue (lambda (s) (time64-week (split->time64 s))))
         (cons 'date-sortvalue time64-week)
         (cons 'text (G_ "Weekly"))
         (cons 'renderer-fn (compose gnc:date-get-week-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'monthly
         (cons 'split-sortvalue (lambda (s) (time64-month (split->time64 s))))
         (cons 'date-sortvalue time64-month)
         (cons 'text (G_ "Monthly"))
         (cons 'renderer-fn (compose gnc:date-get-month-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'quarterly
         (cons 'split-sortvalue (lambda (s) (time64-quarter (split->time64 s))))
         (cons 'date-sortvalue time64-quarter)
         (cons 'text (G_ "Quarterly"))
         (cons 'renderer-fn (compose gnc:date-get-quarter-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'yearly
         (cons 'split-sortvalue (lambda (s) (time64-year (split->time64 s))))
         (cons 'date-sortvalue time64-year)
         (cons 'text (G_ "Yearly"))
         (cons 'renderer-fn (compose gnc:date-get-year-string
                                     gnc-localtime
                                     split->time64)))))

(define filter-list
  (list
   (list 'none
         (cons 'text (G_ "Do not do any filtering")))

   (list 'include
         (cons 'text (G_ "Include Transactions to/from Filter Accounts")))

   (list 'exclude
         (cons 'text (G_ "Exclude Transactions to/from Filter Accounts")))))

(define show-void-list
  (list
   (list 'non-void-only
         (cons 'how (logand CLEARED-ALL (lognot CLEARED-VOIDED)))
         (cons 'text (G_ "Non-void only")))

   (list 'void-only
         (cons 'how CLEARED-VOIDED)
         (cons 'text (G_ "Void only")))

   (list 'both
         (cons 'how CLEARED-ALL)
         (cons 'text (G_ "Both (and include void transactions in totals)")))))

(define show-closing-list
  (list
   (list 'exclude-closing
         (cons 'text (G_ "Exclude closing transactions"))
         (cons 'closing-match #f))

   (list 'include-both
         (cons 'text (G_ "Show both closing and regular transactions"))
         (cons 'closing-match 'both))

   (list 'closing-only
         (cons 'text (G_ "Show closing transactions only"))
         (cons 'closing-match #t))))

(define reconcile-status-list
  ;; 'filter-types must be either #f (i.e. disable reconcile filter)
  ;; or a value defined as defined in Query.c
  ;; e.g. CLEARED-NO for unreconciled
  ;;      (logior CLEARED-NO CLEARED-CLEARED) for unreconciled & cleared
  (list
   (list 'all
         (cons 'text (G_ "Show All Transactions"))
         (cons 'filter-types CLEARED-ALL))

   (list 'unreconciled
         (cons 'text (G_ "Unreconciled only"))
         (cons 'filter-types CLEARED-NO))

   (list 'cleared
         (cons 'text (G_ "Cleared only"))
         (cons 'filter-types CLEARED-CLEARED))

   (list 'reconciled
         (cons 'text (G_ "Reconciled only"))
         (cons 'filter-types CLEARED-RECONCILED))))


(define ascending-list
  (list
   (list 'ascend
         (cons 'text (G_ "Ascending")))
   (list 'descend
         (cons 'text (G_ "Descending")))))

(define sign-reverse-list
  (list
   (list 'global
         (cons 'text (G_ "Use Global Preference"))
         (cons 'acct-types #f))
   (list 'none
         (cons 'text (G_ "Don't change any displayed amounts"))
         (cons 'acct-types '()))
   (list 'income-expense
         (cons 'text (G_ "Income and Expense"))
         (cons 'acct-types (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)))
   (list 'credit-accounts
         (cons 'text (G_ "Credit Accounts"))
         (cons 'acct-types (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE
                                 ACCT-TYPE-EQUITY ACCT-TYPE-CREDIT
                                 ACCT-TYPE-INCOME)))))

(define (keylist-get-info keylist key info)
  (assq-ref (assq-ref keylist key) info))

(define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)))
   keylist))

(define (SUBTOTAL-ENABLED? sortkey parameters)
  ;; this returns whether sortkey *can* be subtotalled/grouped.
  ;; it checks whether a renderer-fn is defined.
  (keylist-get-info (sortkey-list parameters) sortkey 'renderer-fn))

(define (CUSTOM-SORTING? sortkey parameters)
  ;; sortkey -> bool
  ;;
  ;; this returns which sortkeys which *must* use the custom sorter.
  ;; it filters whereby a split-sortvalue is defined (i.e. the splits
  ;; can be compared according to their 'sortvalue) but the QofQuery
  ;; sortkey is not defined (i.e. their 'sortkey is #f).
  (and (keylist-get-info (sortkey-list parameters) sortkey 'split-sortvalue)
       (not (keylist-get-info (sortkey-list parameters) sortkey 'sortkey))))

(define (lists->csv lst)
  ;; converts a list of lists into CSV
  ;; this function aims to follow RFC4180, and will pad lists to
  ;; ensure equal number of items per row.
  ;; e.g. '(("from" "01/01/2010")
  ;;        ("to" "31/12/2010")
  ;;        ("total" 23500 30000 25/7 'sym))
  ;; will output
  ;;  "from","01/01/2010",,,
  ;;  "to","31/12/2010",,,
  ;;  "total",23500.0,30000.0,3.5714285714285716,sym
  (define (string-sanitize-csv str)
    (call-with-output-string
      (lambda (port)
        (display #\" port)
        (string-for-each
         (lambda (c)
           (if (char=? c #\") (display #\" port))
           (display c port))
         str)
        (display #\" port))))

  (define max-items
    (let lp ((lst lst) (maximum 0))
      (cond
       ((null? lst) maximum)
       ((pair? lst) (lp (cdr lst) (max maximum (length (car lst)))))
       (else (error "strify " lst " must be a proper list")))))

  (define (strify obj)
    (cond
     ((or (null? obj) (not obj)) "")
     ((string? obj) (string-sanitize-csv obj))
     ((number? obj) (number->string (exact->inexact obj)))
     ((pair? obj) (let lp ((row obj) (acc '()) (pad max-items))
                    (cond
                     ((zero? pad) (string-concatenate-reverse acc))
                     ((null? row) (lp '() (cons "," acc) (1- pad)))
                     ((pair? row) (lp (cdr row)
                                      (cons* (if (pair? (cdr row)) "," "")
                                             (strify (car row))
                                             acc)
                                      (1- pad)))
                     (else (error "strify " obj " must be a proper list")))))
     ((gnc:gnc-monetary? obj) (strify (gnc:gnc-monetary-amount obj)))
     (else (object->string obj))))

  (string-join (map strify lst) "\n"))

(define gnc:lists->csv lists->csv)


;;
;; Default Transaction Report
;;
(define (gnc:trep-options-generator)
  (define parameters
    (list
      (cons 'split-action (qof-book-use-split-action-for-num-field (gnc-get-current-book)))))

  ;; (Feb 2018) Note to future hackers - this gnc:trep-options-generator
  ;; defines a long set of options to be assigned as an object in
  ;; the report. This long list (52 at Feb 2018 count) of options
  ;; may be modified in a derived report (see income-gst-statement.scm)
  ;; via gnc:make-internal! and gnc-unregister-option to hide
  ;; and remove options, respectively. If an option is unregistered,
  ;; don't forget to re-register them via gnc-register-option, unless
  ;; your derived report truly does not require them.

  (let ((options (gnc-new-optiondb)))

  ;; General options

  (gnc:options-add-date-interval!
   options gnc:pagename-general optname-startdate optname-enddate "a")

  (gnc-register-multichoice-option options
    gnc:pagename-general optname-date-source
    "a5" (G_ "Specify date to filter by…")
    "posted"
    (list (vector 'posted (G_ "Date Posted"))
          (vector 'reconciled (G_ "Reconciled Date"))
          (vector 'entered (G_ "Date Entered"))))

  (gnc-register-complex-boolean-option  options
    pagename-currency optname-common-currency
    "a" (G_ "Convert all transactions into a common currency.") #f
    (lambda (x)
      (gnc-optiondb-set-option-selectable-by-name
       options pagename-currency optname-currency x)
      (gnc-optiondb-set-option-selectable-by-name
       options pagename-currency optname-orig-currency x)
      (gnc-optiondb-set-option-selectable-by-name
       options pagename-currency optname-price-source x)))

  (gnc-register-simple-boolean-option options
    pagename-currency optname-orig-currency
    "b" (G_ "Also show original currency amounts") #f)

  (gnc:options-add-currency!
   options pagename-currency optname-currency "c")

  (gnc:options-add-price-source!
   options pagename-currency optname-price-source "d" 'pricedb-nearest)

  (gnc-register-simple-boolean-option options
    gnc:pagename-general optname-table-export
    "g" (G_ "Formats the table suitable for cut & paste exporting with extra cells.")
    #f)

  (gnc-register-multichoice-option options
    gnc:pagename-general optname-infobox-display
    "h" (G_ "Add summary of options.")
    "no-match"
    ;; This is an alist of conditions for displaying the infobox
    ;; 'no-match for empty-report
    ;; 'match for generated report
    (list (vector 'no-match (G_ "If no transactions matched"))
          (vector 'always (G_ "Always"))
          (vector 'never (G_ "Never"))))

  ;; Filtering Options

  (gnc-register-string-option options
    pagename-filter optname-account-matcher
    "a5" (G_ "Show only accounts whose full name matches this filter e.g. ':Travel' will match \
Expenses:Travel:Holiday and Expenses:Business:Travel. It can be left blank, which will \
disable the filter.")
    "")

  (gnc-register-simple-boolean-option options
    pagename-filter optname-account-matcher-regex
    "a6"
    (G_ "By default the account filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. 'Car|Flights' will match both \
Expenses:Car and Expenses:Flights. Use a period (.) to match a single character e.g. \
'20../.' will match 'Travel 2017/1 London'. ")
    #f)

  (gnc-register-simple-boolean-option options
    pagename-filter optname-account-matcher-exclude "a7"
    (G_ "If this option is selected, accounts matching filter are excluded.")
    #f)

  (gnc-register-string-option options
    pagename-filter optname-transaction-matcher
    "i1" (G_ "Show only transactions where description, notes, or memo matches this filter.
e.g. '#gift' will find all transactions with #gift in description, notes or memo. It can be left \
blank, which will disable the filter.")
    "")

  (gnc-register-simple-boolean-option options
    pagename-filter optname-transaction-matcher-regex
    "i2"
    (G_ "By default the transaction filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. '#work|#family' will match both \
tags within description, notes or memo.")
    #f)

  (gnc-register-simple-boolean-option options
    pagename-filter optname-transaction-matcher-exclude
    "i3"
    (G_ "If this option is selected, transactions matching filter are excluded.")
    #f)

  (gnc-register-simple-boolean-option options
    pagename-filter optname-transaction-matcher-caseinsensitive
    "i4"
    (G_ "If this option is selected, transactions matching filter is not case sensitive.")
    #f)

  (gnc-register-multichoice-option options
    pagename-filter optname-reconcile-status
    "j1" (G_ "Filter by reconcile status.")
    "all"
    (keylist->vectorlist reconcile-status-list))

  (gnc-register-multichoice-option options
    pagename-filter optname-void-transactions
    "k" (N_ "How to handle void transactions.")
    "non-void-only"
    (keylist->vectorlist show-void-list))

  (gnc-register-multichoice-option options
    pagename-filter optname-closing-transactions
    "l" (G_ "By default most users should not include closing \
transactions in a transaction report. Closing transactions are \
transfers from income and expense accounts to equity, and must usually \
be excluded from periodic reporting.")
    "exclude-closing"
    (keylist->vectorlist show-closing-list))

  ;; Accounts options

  ;; account to do report on
  (gnc-register-account-list-option options
    gnc:pagename-accounts optname-accounts
    "a" (G_ "Report on these accounts.")
    ;; select, by default, no accounts! Selecting all accounts will
    ;; always imply an insanely long waiting time upon opening, and it
    ;; is almost never useful. So we instead display the normal error
    ;; message saying "Click here", and the user knows how to
    ;; continue.
    '())

  (gnc-register-account-list-option options
    gnc:pagename-accounts optname-filterby
    "c1" (G_ "Filter on these accounts.")
    '())

  (gnc-register-multichoice-callback-option options
    gnc:pagename-accounts optname-filtertype
    "c" (G_ "Filter account.")
    "none"
    (keylist->vectorlist filter-list)
    (lambda (x)
      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-accounts optname-filterby
       (not (eq? x 'none)))))

  ;; Sorting options

  (let ((ascending-choice-list (keylist->vectorlist ascending-list))
        (key-choice-list (keylist->vectorlist (sortkey-list parameters)))
        (date-subtotal-choice-list (keylist->vectorlist date-subtotal-list))
        (prime-sortkey 'account-name)
        (prime-sortkey-subtotal-true #t)
        (prime-date-subtotal 'monthly)
        (sec-sortkey 'register-order)
        (sec-sortkey-subtotal-true #f)
        (sec-date-subtotal 'monthly))

    (define (apply-selectable-by-name-sorting-options)
      (let* ((prime-sortkey-enabled (not (eq? prime-sortkey 'none)))
             (prime-sortkey-subtotal-enabled
              (SUBTOTAL-ENABLED? prime-sortkey parameters))
             (prime-date-sortingtype-enabled (memq prime-sortkey DATE-SORTING-TYPES))
             (sec-sortkey-enabled (not (eq? sec-sortkey 'none)))
             (sec-sortkey-subtotal-enabled
              (SUBTOTAL-ENABLED? sec-sortkey parameters))
             (sec-date-sortingtype-enabled (memq sec-sortkey DATE-SORTING-TYPES)))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-prime-subtotal
         prime-sortkey-subtotal-enabled)

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-prime-sortorder
         prime-sortkey-enabled)

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-sec-subtotal
         sec-sortkey-subtotal-enabled)

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-sec-sortorder
         sec-sortkey-enabled)

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-full-account-name
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-show-account-code
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-show-account-description
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-indenting
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)
             (and prime-date-sortingtype-enabled (not (eq? 'none prime-date-subtotal)))
             (and sec-date-sortingtype-enabled (not (eq? 'none sec-date-subtotal)))))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-show-subtotals-only
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)
             (and prime-date-sortingtype-enabled (not (eq? 'none prime-date-subtotal)))
             (and sec-date-sortingtype-enabled (not (eq? 'none sec-date-subtotal)))))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-show-informal-headers
         (or (memq prime-sortkey (list 'account-name 'account-code))
             (memq sec-sortkey (list 'account-name 'account-code))))

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-prime-date-subtotal
         prime-date-sortingtype-enabled)

        (gnc-optiondb-set-option-selectable-by-name
         options pagename-sorting optname-sec-date-subtotal
         sec-date-sortingtype-enabled)))

    ;; primary sorting criterion
    (gnc-register-multichoice-callback-option options
      pagename-sorting optname-prime-sortkey
      "a" (G_ "Sort by this criterion first.")
      (symbol->string prime-sortkey)
      key-choice-list
      (lambda (x)
        (set! prime-sortkey x)
        (apply-selectable-by-name-sorting-options)))

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-full-account-name
      "j1"
      (G_ "Show the full account name for subtotals and subheadings?")
      #f)

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-show-account-code
      "j2"
      (G_ "Show the account code for subtotals and subheadings?")
      #f)

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-show-account-description
      "j3"
      (G_ "Show the account description for subheadings?")
      #f)

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-show-informal-headers
      "j4"
      (G_ "Show the informal headers for debit/credit accounts?")
      #f)

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-indenting
      "j5"
      (G_ "Add indenting columns with grouping and subtotals?")
      #t)

    (gnc-register-simple-boolean-option options
      pagename-sorting optname-show-subtotals-only
      "j6"
      (G_ "Show subtotals only, hiding transactional detail?")
      #f)

    (gnc-register-complex-boolean-option options
      pagename-sorting optname-prime-subtotal
      "e5"
      (G_ "Subtotal according to the primary key?") prime-sortkey-subtotal-true
      (lambda (x)
        (set! prime-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options)))

     (gnc-register-multichoice-callback-option options
      pagename-sorting optname-prime-date-subtotal
      "e2" (G_ "Do a date subtotal.")
      (symbol->string prime-date-subtotal)
      date-subtotal-choice-list
      (lambda (x)
        (set! prime-date-subtotal x)
        (apply-selectable-by-name-sorting-options)))

    (gnc-register-multichoice-option options
      pagename-sorting optname-prime-sortorder
      "e" (G_ "Order of primary sorting.")
      "ascend"
      ascending-choice-list)

    ;; Secondary sorting criterion
    (gnc:register-multichoice-callback-option options
      pagename-sorting optname-sec-sortkey
      "f"
      (G_ "Sort by this criterion second.")
      (symbol->string sec-sortkey)
      key-choice-list
      (lambda (x)
        (set! sec-sortkey x)
        (apply-selectable-by-name-sorting-options)))

    (gnc-register-complex-boolean-option options
      pagename-sorting optname-sec-subtotal
      "i5"
      (G_ "Subtotal according to the secondary key?") sec-sortkey-subtotal-true
      (lambda (x)
        (set! sec-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options)))

    (gnc-register-multichoice-callback-option options
      pagename-sorting optname-sec-date-subtotal
      "i2" (G_ "Do a date subtotal.")
      (symbol->string sec-date-subtotal)
      date-subtotal-choice-list
      (lambda (x)
        (set! sec-date-subtotal x)
        (apply-selectable-by-name-sorting-options)))

    (gnc-register-multichoice-option options
      pagename-sorting optname-sec-sortorder
      "i" (G_ "Order of Secondary sorting.")
      "ascend"
      ascending-choice-list))

  ;; Display options

  (let ((disp-memo? #t)
        (disp-accname? #f)
        (disp-other-accname? #t)
        (disp-detail-level? 'single)
        (amount-value 'double))

    (define (apply-selectable-by-name-display-options)
      (define detail-is-single? (eq? disp-detail-level? 'single))
      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Use Full Account Name")
       disp-accname?)

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Other Account Name")
       detail-is-single?)

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Sign Reverses")
       (eq? amount-value 'single))

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display optname-grid
       (not (eq? amount-value 'none)))

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display "Enable Links"
       (not (eq? amount-value 'none)))

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Use Full Other Account Name")
       (and disp-other-accname? detail-is-single?))

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Other Account Code")
       detail-is-single?)

      (gnc-optiondb-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Notes")
       disp-memo?))

    (for-each
     (lambda (l)
       (gnc-register-simple-boolean-option options
         gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l)))
     ;; One list per option here with: option-name, sort-tag,
     ;; help-string, default-value
     (list
      (list (N_ "Date")                         "a"  (G_ "Display the date?") #t)
      (list (N_ "Reconciled Date")              "a2" (G_ "Display the reconciled date?") #f)
      (list (N_ "Date Entered")                 "a3" (G_ "Display the entered date?") #f)
      (if (assq-ref parameters 'split-action)
          (list (N_ "Num/Action")               "b"  (G_ "Display the check number?") #t)
          (list (N_ "Num")                      "b"  (G_ "Display the check number?") #t))
      (list (N_ "Description")                  "c"  (G_ "Display the description?") #t)
      (list (N_ "Notes")                        "d2" (G_ "Display the notes if the memo is unavailable?") #t)
      ;; account name option appears here
      (list (N_ "Use Full Account Name")        "f"  (G_ "Display the full account name?") #t)
      (list (N_ "Account Code")                 "g"  (G_ "Display the account code?") #f)
      ;; other account name option appears here
      (list (N_ "Use Full Other Account Name")  "i"  (G_ "Display the full account name?") #f)
      (list (N_ "Other Account Code")           "j"  (G_ "Display the other account code?") #f)
      ;; Translators: The number of units of any kind of investment (shares, fonds, bonds, …)
      (list (N_ "Shares")                       "k"  (G_ "Display the number of shares?") #f)
      (list (N_ "Link")                         "l5" (G_ "Display the transaction linked document") #f)
      (list (N_ "Price")                        "l"  (G_ "Display the shares price?") #f)
      ;; note the "Amount" multichoice option in between here
      (list optname-grid                        "m5" (G_ "Display a subtotal summary table.") #f)
      (list (N_ "Account Balance")              "n"  (G_ "Display the balance of the underlying account on each line?") #f)
      (list optname-grand-total                 "o"  (G_ "Display a grand total section at the bottom?") #t)))

    (when (assq-ref parameters 'split-action)
      (gnc-register-simple-boolean-option options
        gnc:pagename-display (N_ "Trans Number")
        "b2" (G_ "Display the trans number?") #f))

    ;; Add an option to display the memo, and disable the notes option
    ;; when memos are not included.
    (gnc-register-complex-boolean-option options
      gnc:pagename-display (N_ "Memo")
      "d"  (G_ "Display the memo?") disp-memo?
      (lambda (x)
        (set! disp-memo? x)
        (apply-selectable-by-name-display-options)))

    ;; Ditto for Account Name #t -> Use Full Account Name is selectable
    (gnc-register-complex-boolean-option options
      gnc:pagename-display (N_ "Account Name")
      "e"  (G_ "Display the account name?") disp-accname?
      (lambda (x)
        (set! disp-accname? x)
        (apply-selectable-by-name-display-options)))

    ;; Ditto for Other Account Name #t -> Use Full Other Account Name is selectable
    (gnc-register-complex-boolean-option options
      gnc:pagename-display (N_ "Other Account Name")
      "h5"  (G_ "Display the other account name? If this is a split transaction, this parameter is guessed.") disp-other-accname?
      (lambda (x)
        (set! disp-other-accname? x)
        (apply-selectable-by-name-display-options)))

    (gnc-register-multichoice-callback-option options
      gnc:pagename-display optname-detail-level
      "h" (G_ "Amount of detail to display per transaction.")
      (symbol->string disp-detail-level?)
      (list (vector 'multi-line (G_ "One split per line"))
            (vector 'single (G_ "One transaction per line")))
      (lambda (x)
        (set! disp-detail-level? x)
        (apply-selectable-by-name-display-options)))

    (gnc-register-multichoice-callback-option options
      gnc:pagename-display (N_ "Amount")
      "m" (G_ "Display the amount?")
      (symbol->string amount-value)
      (list
       (vector 'none   (G_ "Hide"))
       (vector 'single (G_ "Single Column"))
       (vector 'double (G_ "Two Columns")))
      (lambda (x)
        (set! amount-value x)
        (apply-selectable-by-name-display-options)))

    (gnc-register-simple-boolean-option options
      gnc:pagename-display (N_ "Enable Links")
      "m2" (G_ "Enable hyperlinks in amounts.") #t)

    (gnc-register-multichoice-option options
      gnc:pagename-display (N_ "Sign Reverses")
      "m1" (G_ "Reverse amount display for certain account types.")
      "global"
      (keylist->vectorlist sign-reverse-list))

    (gnc-register-multichoice-option options
      gnc:pagename-display optname-running-totals
      "o2" (G_ "Display running totals as per report sort order?")
      "none"
      (list (vector 'none  (G_ "None"))
            (vector 'all   (G_ "Grand Total and Subtotals"))
            (vector 'grand (G_ "Grand Total Only"))
            (vector 'sub   (G_ "Subtotals Only")))))

  ;; this hidden option will toggle whether the default
  ;; qof-query is run, or a different query which ensures
  ;; no transaction is duplicated. It can be enabled in
  ;; a derived report (eg income-gst-statement.scm)
  (gnc-register-internal-option options "__trep" "unique-transactions" #f)

  (GncOptionDBPtr-set-default-section options gnc:pagename-general)
    options))

(define (upgrade-vector-to-assoclist list-of-columns)
  (map (lambda (col)
         (list (cons 'heading (vector-ref col 0))
               (cons 'calc-fn (lambda (s tr?) ((vector-ref col 1) s)))
               (cons 'reverse-column? (vector-ref col 2))
               (cons 'subtotal? (vector-ref col 3))
               (cons 'start-dual-column? (vector-ref col 4))
               (cons 'friendly-heading-fn (vector-ref col 5))
               ;; the following is a backward-compatibility hack
               ;; being used by income-gst-statement.scm
               (cons 'merge-dual-column? (and (<= 7 (vector-length col))
                                              (vector-ref col 6)))))
       list-of-columns))

(define (invalid-cell? cell)
  (let lp ((fields '(heading calc-fn reverse-column? subtotal? start-dual-column?
                             friendly-heading-fn merge-dual-column?)))
    (match fields
      (() #f)
      (((? (cut assq <> cell)) . rest) (lp rest))
      ((fld . _) (gnc:error "field " fld " missing in cell " cell) #t))))

;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the big function that builds the whole table.

(define (make-split-table splits options parameters custom-calculated-cells)

  (define (report-uses? param)
    (assq-ref parameters param))

  (let* ((work-to-do (length splits))
         (table (gnc:make-html-table))
         (account-types-to-reverse
          (keylist-get-info sign-reverse-list
                            (report-uses? 'reversed-signs)
                            'acct-types)))

    (define (acc-reverse? acc)
      (if account-types-to-reverse
          (memv (xaccAccountGetType acc) account-types-to-reverse)
          (gnc-reverse-balance acc)))

    (define exchange-fn
      (if (report-uses? 'common-currency)
          (gnc:case-exchange-time-fn
           (report-uses? 'common-currency/price-source)
           (report-uses? 'common-currency)
           (gnc:accounts-get-commodities (report-uses? 'accounts-matched) #f)
           (report-uses? 'enddate) #f #f)
          gnc:exchange-by-pricedb-nearest))

    ;; Returns #t if a calculated-cell definition has the subtotal flag
    (define (cell-with-subtotals? cell)
      (assq-ref cell 'subtotal?))

    ;; Collectors for running total and subtotals
    (define converted-running-total-collector (gnc:make-commodity-collector))
    (define original-running-total-collector (gnc:make-commodity-collector))
    (define converted-running-prime-collector (gnc:make-commodity-collector))
    (define original-running-prime-collector (gnc:make-commodity-collector))
    (define converted-running-sec-collector (gnc:make-commodity-collector))
    (define original-running-sec-collector (gnc:make-commodity-collector))

    (define left-columns
      (let* ((add-if (lambda (pred? . items) (if pred? items '())))
             (left-cols-list
              (append
               (add-if (report-uses? 'date)
                       (list (cons 'heading (G_ "Date"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (and transaction-row?
                                          (gnc:make-html-table-cell/markup
                                           "date-cell"
                                           (qof-print-date
                                            (xaccTransGetDate
                                             (xaccSplitGetParent split)))))))))

               (add-if (report-uses? 'entered)
                       (list (cons 'heading (G_ "Date Entered"))
                             (cons 'renderer-fn (lambda (split transaction-row?)
                                                  (and transaction-row?
                                                       (gnc:make-html-table-cell/markup
                                                        "date-cell" (qof-print-date
                                                                     (xaccTransRetDateEntered
                                                                      (xaccSplitGetParent split)))))))))

               (add-if (report-uses? 'reconciled-date)
                       (list (cons 'heading (G_ "Reconciled Date"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (let ((reconcile-date
                                            (and (char=? (xaccSplitGetReconcile split) #\y)
                                                 (xaccSplitGetDateReconciled split))))
                                       (and reconcile-date
                                            (gnc:make-html-table-cell/markup
                                             "date-cell"
                                             (qof-print-date reconcile-date))))))))

               (add-if (report-uses? 'num)
                       (list (cons 'heading (if (report-uses? 'trans-number)
                                                (G_ "Num/T-Num")
                                                (G_ "Num")))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (let* ((trans (xaccSplitGetParent split))
                                            (num (gnc-get-num-action trans split))
                                            (t-num (if (report-uses? 'trans-number)
                                                       (gnc-get-num-action trans #f)
                                                       ""))
                                            (num-string (if (string-null? t-num)
                                                            num
                                                            (string-append num "/" t-num))))
                                       (and transaction-row?
                                            (gnc:make-html-table-cell/markup
                                             "text-cell" num-string)))))))

               (add-if (report-uses? 'description)
                       (list (cons 'heading (G_ "Description"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (define trans (xaccSplitGetParent split))
                                     (and transaction-row?
                                          (gnc:make-html-table-cell/markup
                                           "text-cell"
                                           (xaccTransGetDescription trans)))))))

               (add-if (report-uses? 'memo)
                       (list (cons 'heading (if (report-uses? 'notes)
                                                (string-append (G_ "Memo") "/" (G_ "Notes"))
                                                (G_ "Memo")))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (define trans (xaccSplitGetParent split))
                                     (define memo (xaccSplitGetMemo split))
                                     (if (and (string-null? memo) (report-uses? 'notes))
                                         (xaccTransGetNotes trans)
                                         memo)))))

               (add-if (or (report-uses? 'account-name) (report-uses? 'account-code))
                       (list (cons 'heading (G_ "Account"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (account-namestring
                                      (xaccSplitGetAccount split)
                                      (report-uses? 'account-code)
                                      (report-uses? 'account-name)
                                      (report-uses? 'account-full-name))))))

               (add-if (or (report-uses? 'other-account-name)
                           (report-uses? 'other-account-code))
                       (list (cons 'heading (G_ "Transfer from/to"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (and (< 1 (xaccTransCountSplits
                                                (xaccSplitGetParent split)))
                                          (account-namestring
                                           (xaccSplitGetAccount
                                            (xaccSplitGetOtherSplit split))
                                           (report-uses? 'other-account-code)
                                           (report-uses? 'other-account-name)
                                           (report-uses? 'other-account-full-name)))))))

               (add-if (report-uses? 'shares)
                       (list (cons 'heading (G_ "Shares"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (gnc:make-html-table-cell/markup
                                      "number-cell"
                                      (xaccSplitGetAmount split))))))

               (add-if (report-uses? 'doclink)
                       (list (cons 'heading "")
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (let ((url (xaccTransGetDocLink
                                                 (xaccSplitGetParent split))))
                                       (and (not (string-null? url))
                                            (gnc:make-html-table-cell/markup
                                             "text-cell"
                                             (if (report-uses? 'links)
                                                 (gnc:html-transaction-doclink-anchor
                                                  (xaccSplitGetParent split)
                                                  ;; Translators: 'L' is short for Linked Document
                                                  (C_ "Column header for 'Document Link'" "L"))
                                                 (C_ "Column header for 'Document Link'" "L")))))))))

               (add-if (report-uses? 'price)
                       (list (cons 'heading (G_ "Price"))
                             (cons 'renderer-fn
                                   (lambda (split transaction-row?)
                                     (gnc:make-html-table-cell/markup
                                      "number-cell"
                                      (gnc:default-price-renderer
                                       (xaccTransGetCurrency (xaccSplitGetParent split))
                                       (xaccSplitGetSharePrice split))))))))))

        (if (or (report-uses? 'subtotals-only)
                (and (null? left-cols-list)
                     (or (report-uses? 'grand-total)
                         (report-uses? 'primary-key/renderer-fn)
                         (report-uses? 'secondary-key/renderer-fn))))
            `(((heading . "") (renderer-fn . ,(const #f))))
            left-cols-list)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; calculated-cells
    ;;
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define default-calculated-cells
      (letrec
          ((split-amount (lambda (s) (if (gnc:split-voided? s)
                                         (xaccSplitVoidFormerAmount s)
                                         (xaccSplitGetAmount s))))
           (split-currency (compose xaccAccountGetCommodity xaccSplitGetAccount))
           (row-currency (lambda (s) (or (report-uses? 'common-currency)
                                         (split-currency s))))
           (friendly-debit (lambda (a) (gnc-account-get-debit-string (xaccAccountGetType a))))
           (friendly-credit (lambda (a) (gnc-account-get-credit-string (xaccAccountGetType a))))
           (header-commodity (lambda (str)
                               (string-append
                                str
                                (if (report-uses? 'common-currency)
                                    (format #f " (~a)"
                                            (gnc-commodity-get-mnemonic
                                             (report-uses? 'common-currency)))
                                    ""))))
           ;; For conversion to row-currency.
           (converted-amount (lambda (s tr?)
                               (exchange-fn
                                (gnc:make-gnc-monetary (split-currency s)
                                                       (split-amount s))
                                (row-currency s)
                                (xaccTransGetDate (xaccSplitGetParent s)))))
           (converted-debit-amount (lambda (s tr?) (and (positive? (split-amount s))
                                                        (converted-amount s tr?))))
           (converted-credit-amount (lambda (s tr?)
                                      (and (not (positive? (split-amount s)))
                                           (gnc:monetary-neg (converted-amount s tr?)))))
           (converted-account-balance (lambda (s tr?)
                                        (exchange-fn
                                         (gnc:make-gnc-monetary
                                          (split-currency s)
                                          (xaccSplitGetBalance s))
                                         (row-currency s)
                                         (time64CanonicalDayTime
                                          (xaccTransGetDate (xaccSplitGetParent s))))))
           (original-amount (lambda (s tr?)
                              (gnc:make-gnc-monetary
                               (split-currency s) (split-amount s))))
           (original-debit-amount (lambda (s tr?)
                                    (and (positive? (split-amount s))
                                         (original-amount s tr?))))
           (original-credit-amount (lambda (s tr?)
                                     (and (not (positive? (split-amount s)))
                                          (gnc:monetary-neg (original-amount s tr?)))))
           (original-account-balance (lambda (s tr?)
                                       (gnc:make-gnc-monetary
                                        (split-currency s) (xaccSplitGetBalance s))))
           ;; Helper function to add the splits to the running total collectors.
           ;; Third argument is the function that returns converted or original amount.
           ;; Returns current monetary from specified running total collector.
           (add-split-to-collector (lambda (s tr? coll amt-fn)
                                        (coll 'add
                                          (gnc:gnc-monetary-commodity (amt-fn s tr?))
                                          (gnc:gnc-monetary-amount (amt-fn s tr?)))
                                        (coll 'getmonetary
                                          (gnc:gnc-monetary-commodity (amt-fn s tr?)) #f)))
           (converted-running-total (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          converted-running-total-collector
                                          converted-amount))))
           (converted-running-prime (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          converted-running-prime-collector
                                          converted-amount))))
           (converted-running-sec (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          converted-running-sec-collector
                                          converted-amount))))
           (original-running-total (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          original-running-total-collector
                                          original-amount))))
           (original-running-prime (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          original-running-prime-collector
                                          original-amount))))
           (original-running-sec (lambda (s tr?)
                                      (and tr?
                                        (add-split-to-collector s tr?
                                          original-running-sec-collector
                                          original-amount)))))
        (append
         ;; each column will be a list of pairs whose car is a metadata header,
         ;; and whose cdr is the procedure, string or bool to obtain the metadata
         ;;   'heading            the heading string
         ;;   'calc-fn            (calc-fn split transaction-row?) to obtain gnc:monetary
         ;;   'reverse-column?    #t to allow reverse signs
         ;;   'subtotal?          #t to allow subtotals (ie must be #f for
         ;;                       running balance)
         ;;   'start-dual-column? #t for the debit side of a dual column
         ;;                       (i.e. debit/credit) which means the next
         ;;                       column must be the credit side
         ;;   'friendly-heading-fn (friendly-heading-fn account) to retrieve
         ;;                       friendly name for account debit/credit
         ;;                       or 'bal-bf for balance-brought-forward
         ;;                       or 'original-bal-bf for bal-bf in original currency
         ;;                       when currency conversion is used
         ;;   'merge-dual-column?  #t: merge with next cell.

         (if (report-uses? 'amount-single)
             (list (list (cons 'heading (header-commodity (G_ "Amount")))
                         (cons 'calc-fn converted-amount)
                         (cons 'reverse-column? #t)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (report-uses? 'amount-double)
             (list (list (cons 'heading (header-commodity (G_ "Debit")))
                         (cons 'calc-fn converted-debit-amount)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #t)
                         (cons 'friendly-heading-fn friendly-debit)
                         (cons 'merge-dual-column? #t))
                   (list (cons 'heading (header-commodity (G_ "Credit")))
                         (cons 'calc-fn converted-credit-amount)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn friendly-credit)
                         (cons 'merge-dual-column? #f)))
             '())

         (if (report-uses? 'running-balance)
             (if (report-uses? 'bal-bf)
                 (list (list (cons 'heading (header-commodity (G_ "Running Balance")))
                             (cons 'calc-fn converted-account-balance)
                             (cons 'reverse-column? #t)
                             (cons 'subtotal? #f)
                             (cons 'start-dual-column? #f)
                             (cons 'friendly-heading-fn 'bal-bf)
                             (cons 'merge-dual-column? #f)))
                 (list (list (cons 'heading (header-commodity (G_ "Account Balance")))
                             (cons 'calc-fn converted-account-balance)
                             (cons 'reverse-column? #t)
                             (cons 'subtotal? #f)
                             (cons 'start-dual-column? #f)
                             (cons 'friendly-heading-fn #f)
                             (cons 'merge-dual-column? #f))))
             '())

         (if (report-uses? 'running-sec)
             (list (list (cons 'heading (header-commodity
                           ;; Translators: this is the running total for the secondary subtotal.
                           ;; For translation to be consistent, make sure it follows the same
                           ;; pattern as for these other strings: “Running Totals”,
                           ;; "Secondary Subtotal” and "Running Primary Subtotal"
                           (G_ "Running Secondary Subtotal")))
                         (cons 'calc-fn converted-running-sec)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (report-uses? 'running-prime)
             (list (list (cons 'heading (header-commodity
                           (if (report-uses? 'secondary-key/renderer-fn)
                               ;; Translators: this is the running total for the primary subtotal.
                               ;; For translation to be consistent, make sure it follows the same
                               ;; pattern as for these other strings: “Running Totals” and
                               ;; “Primary Subtotal”
                               (G_ "Running Primary Subtotal")
                               ;; Translators: "Running Subtotal" is a shorter version of
                               ;; "Running Primary Subtotal" used when a running primary subtotal
                               ;; is displayed without a secondary subtotal.
                               (G_ "Running Subtotal"))))
                         (cons 'calc-fn converted-running-prime)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (report-uses? 'running-grand-total)
             (list (list (cons 'heading (header-commodity
                           (if (or (report-uses? 'primary-key/renderer-fn)
                                   (report-uses? 'secondary-key/renderer-fn))
                               ;; Translators: this is the running total for the grand total.
                               ;; For translation to be consistent, make sure it follows the same
                               ;; pattern as for these other strings: “Running Totals” and
                               ;; "Grand Total”
                               (G_ "Running Grand Total")
                               ;; Translators: "Running Total" is a shorter version of
                               ;; "Running Grand Total" used when the running grand total is
                               ;; displayed without subtotals. To be consistent, also consider
                               ;; how the plural form "Running Totals" is translated.
                               (G_ "Running Total"))))
                         (cons 'calc-fn converted-running-total)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'amount-single))
             (list (list (cons 'heading (G_ "Amount"))
                         (cons 'calc-fn original-amount)
                         (cons 'reverse-column? #t)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'amount-double))
             (list (list (cons 'heading (G_ "Debit"))
                         (cons 'calc-fn original-debit-amount)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #t)
                         (cons 'friendly-heading-fn friendly-debit)
                         (cons 'merge-dual-column? #t))
                   (list (cons 'heading (G_ "Credit"))
                         (cons 'calc-fn original-credit-amount)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #t)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn friendly-credit)
                         (cons 'merge-dual-column? #f)))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'running-balance))
             (if (report-uses? 'bal-bf)
                 (list (list (cons 'heading (G_ "Running Balance"))
                             (cons 'calc-fn original-account-balance)
                             (cons 'reverse-column? #t)
                             (cons 'subtotal? #f)
                             (cons 'start-dual-column? #f)
                             (cons 'friendly-heading-fn 'original-bal-bf)
                             (cons 'merge-dual-column? #f)))
                 (list (list (cons 'heading (G_ "Account Balance"))
                             (cons 'calc-fn original-account-balance)
                             (cons 'reverse-column? #t)
                             (cons 'subtotal? #f)
                             (cons 'start-dual-column? #f)
                             (cons 'friendly-heading-fn #f)
                             (cons 'merge-dual-column? #f))))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'running-sec))
             (list (list (cons 'heading (G_ "Running Secondary Subtotal"))
                         (cons 'calc-fn original-running-sec)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'running-prime))
             (list (list (cons 'heading
                           (if (report-uses? 'secondary-key/renderer-fn)
                               (G_ "Running Primary Subtotal")
                               (G_ "Running Subtotal")))
                         (cons 'calc-fn original-running-prime)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '())

         (if (and (report-uses? 'common-currency/original)
                  (report-uses? 'running-grand-total))
             (list (list (cons 'heading
                           (if (or (report-uses? 'primary-key/renderer-fn)
                                   (report-uses? 'secondary-key/renderer-fn))
                               (G_ "Running Grand Total")
                               (G_ "Running Total")))
                         (cons 'calc-fn original-running-total)
                         (cons 'reverse-column? #f)
                         (cons 'subtotal? #f)
                         (cons 'start-dual-column? #f)
                         (cons 'friendly-heading-fn (const ""))
                         (cons 'merge-dual-column? #f)))
             '()))))

    (define calculated-cells
      ;; this part will check whether custom-calculated-cells were specified. this
      ;; describes a custom function which consumes an options list, and generates
      ;; an association list similar to default-calculated-cells as above.
      (let ((cc (if custom-calculated-cells
                    (let ((ccc (custom-calculated-cells options)))
                      (cond
                        ((not (pair? ccc)) (gnc:error "welp" ccc)
                          default-calculated-cells)
                        ((vector? (car ccc)) (upgrade-vector-to-assoclist ccc))
                        ((any invalid-cell? ccc) (gnc:error "welp" ccc)
                          default-calculated-cells)
                        (else ccc)))
                    default-calculated-cells)))
        ;; Only keep cells with subtotals when "Show subtotals only" is selected
        ;; otherwise leave all calculated-cells as is.
        (if (report-uses? 'subtotals-only) (filter cell-with-subtotals? cc) cc)))

    (define headings-left-columns
      (map (cut assq-ref <> 'heading) left-columns))

    (define headings-right-columns
      (map (cut assq-ref <> 'heading) calculated-cells))

    (define width-left-columns (length left-columns))
    (define width-right-columns (length calculated-cells))

    (define primary-indent
      (if (and (report-uses? 'indenting)
               (report-uses? 'primary-key/renderer-fn))
          1 0))

    (define secondary-indent
      (if (and (report-uses? 'indenting)
               (report-uses? 'secondary-key/renderer-fn))
          1 0))

    (define indent-level
      (+ primary-indent secondary-indent))

    (define (add-subheading data subheading-style split level)
      (let* ((sortkey (case level
                        ((primary) (report-uses? 'primary-key))
                        ((secondary) (report-uses? 'secondary-key))))
             (data (if (and (any (lambda (c) (eq? 'bal-bf (assq-ref c 'friendly-heading-fn)))
                                 calculated-cells)
                            (memq sortkey ACCOUNT-SORTING-TYPES))
                       ;; Translators: Balance b/f stands for "Balance
                       ;; brought forward".
                       (string-append data ": " (G_ "Balance b/f"))
                       data))
             (renderer-fn (keylist-get-info
                           (sortkey-list parameters)
                           sortkey 'renderer-fn))
             (left-indent (case level
                            ((primary total) 0)
                            ((secondary) primary-indent)))
             (right-indent (- indent-level left-indent)))

        (unless (report-uses? 'subtotals-only)
          (gnc:html-table-append-row/markup!
           table subheading-style
           (append
            (gnc:html-make-empty-cells left-indent)
            (if (report-uses? 'export-table)
                (cons
                 (gnc:make-html-table-cell/markup "total-label-cell" data)
                 (gnc:html-make-empty-cells
                  (+ right-indent width-left-columns -1)))
                (list
                 (gnc:make-html-table-cell/size/markup
                  1 (+ right-indent width-left-columns) "total-label-cell" data)))
            (map
             (lambda (cell)
               (match (assq-ref cell 'friendly-heading-fn)
                 (#f #f)
                 ('bal-bf
                  (let* ((acc (xaccSplitGetAccount split))
                         (bal (exchange-fn
                               (gnc:make-gnc-monetary
                                (xaccAccountGetCommodity acc)
                                (xaccAccountGetBalanceAsOfDate acc (report-uses? 'begindate)))
                               (or (report-uses? 'common-currency)
                                   (xaccAccountGetCommodity acc))
                               (time64CanonicalDayTime
                                (xaccTransGetDate (xaccSplitGetParent split))))))
                    (and (memq sortkey ACCOUNT-SORTING-TYPES)
                         (gnc:make-html-table-cell/markup
                          "number-cell"
                          (if (acc-reverse? acc) (gnc:monetary-neg bal) bal)))))
                 ('original-bal-bf
                  (let* ((acc (xaccSplitGetAccount split))
                         (bal (xaccAccountGetBalanceAsOfDate acc (report-uses? 'begindate))))
                    (and (memq sortkey ACCOUNT-SORTING-TYPES)
                         (gnc:make-html-table-cell/markup
                          "number-cell"
                          (gnc:make-gnc-monetary
                           (xaccAccountGetCommodity acc)
                           (if (acc-reverse? acc) (- bal) bal))))))
                 (fn
                  (and (report-uses? 'informal-headers)
                       (report-uses? 'amount-double)
                       (memq sortkey SORTKEY-INFORMAL-HEADERS)
                       (gnc:make-html-text
                        (gnc:html-markup-b
                         (fn (xaccSplitGetAccount split))))))))
             calculated-cells))))))

    ;; check first calculated-cell merge-dual-column status.
    (define first-column-merge?
      (and (pair? calculated-cells)
           (assq-ref (car calculated-cells) 'merge-dual-column?)))

    (define (add-subtotal-row subtotal-string subtotal-collectors
                              subtotal-style level row col)
      (let* ((left-indent (case level
                            ((total) 0)
                            ((primary) primary-indent)
                            ((secondary) (+ primary-indent secondary-indent))))
             (right-indent (- indent-level left-indent))
             (merge-list (map (cut assq-ref <> 'start-dual-column?) calculated-cells))
             (columns (map (lambda (coll)
                             (coll 'format gnc:make-gnc-monetary #f))
                           subtotal-collectors))
             (list-of-commodities
              (delete-duplicates
               (map gnc:gnc-monetary-commodity (concatenate columns))
               gnc-commodity-equal)))

        (define (retrieve-commodity list-of-monetary commodity)
          (find (lambda (mon)
                  (gnc-commodity-equal commodity (gnc:gnc-monetary-commodity mon)))
                list-of-monetary))

        (define (first-column string)
          (if (report-uses? 'export-table)
              (cons
               (gnc:make-html-table-cell/markup "total-label-cell" string)
               (gnc:html-make-empty-cells (+ right-indent width-left-columns -1)))
              (list
               (gnc:make-html-table-cell/size/markup
                1 (+ right-indent width-left-columns) "total-label-cell" string))))

        (define (data-columns commodity)
          (let loop ((merging? #f)
                     (last-column #f)
                     (columns columns)
                     (merge-list merge-list)
                     (result '()))
            (if (null? columns)
                ;; we've processed all columns. return the (reversed)
                ;; list of html-table-cells.
                (reverse result)
                (let* ((mon (retrieve-commodity (car columns) commodity))
                       (this-column (and mon (gnc:gnc-monetary-amount mon))))
                  (cond

                   ;; We're merging. If a subtotal exists, send to next loop iteration.
                   ((car merge-list)
                    (loop #t
                          this-column
                          (cdr columns)
                          (cdr merge-list)
                          result))

                   ;; We're completing merge. Display debit-credit in correct column.
                   (merging?
                    (let* ((sum (and (or last-column this-column)
                                     (- (or last-column 0) (or this-column 0))))
                           (sum-table-cell (and sum (gnc:make-html-table-cell/markup
                                                     "total-number-cell"
                                                     (gnc:make-gnc-monetary
                                                      commodity (abs sum)))))
                           (debit-col (and sum (positive? sum) sum-table-cell))
                           (credit-col (and sum (not (positive? sum)) sum-table-cell)))
                      (loop #f
                            #f
                            (cdr columns)
                            (cdr merge-list)
                            (cons* credit-col debit-col result))))

                   ;; Not merging nor completed merge. Just add amount to result.
                   (else
                    (loop #f
                          #f
                          (cdr columns)
                          (cdr merge-list)
                          (cons (gnc:make-html-table-cell/markup
                                 "total-number-cell" mon)
                                result))))))))

        (define (get-commodity-grid-amount commodity)
          (define zero (gnc:make-gnc-monetary commodity 0))
          (gnc:monetary+
           (or (retrieve-commodity (car columns) commodity) zero)
           (gnc:monetary-neg
            (or (and first-column-merge? (retrieve-commodity (cadr columns) commodity))
                zero))))

        (set! grid
          (grid-add grid row col (map get-commodity-grid-amount list-of-commodities)))

        ;; each commodity subtotal gets a separate line in the html-table
        ;; each line comprises: indenting, first-column, data-columns
        (let loop ((first-column-string subtotal-string)
                   (list-of-commodities list-of-commodities))
          (unless (null? list-of-commodities)
            (gnc:html-table-append-row/markup!
             table subtotal-style
             (append
              (gnc:html-make-empty-cells left-indent)
              (first-column first-column-string)
              (data-columns (car list-of-commodities))))
            (loop "" (cdr list-of-commodities))))))

    (define (total-string str)
      (if (opt-val pagename-sorting optname-show-subtotals-only)
        str
        (string-append (G_ "Total For ") str)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; renderers
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; display an account name depending on the options the user has set
    (define (account-namestring account show-account-code?
                                show-account-name? show-account-full-name?)
      ;;# on multi-line splits we can get an empty ('()) account
      (if (null? account)
          (G_ "Split Transaction")
          (with-output-to-string
            (lambda ()
              (when show-account-code?
                (let ((code (xaccAccountGetCode account)))
                  (unless (string-null? code)
                    (display code)
                    (display " "))))
              (when show-account-name?
                (display
                 (if show-account-full-name?
                     (gnc-account-get-full-name account)
                     (xaccAccountGetName account))))))))

    ;; retrieve date renderer from the date-subtotal-list
    (define (render-date date-subtotal-key split)
      ((keylist-get-info date-subtotal-list date-subtotal-key 'renderer-fn) split))

    ;; generate account name, optionally with anchor to account register
    (define (render-account sortkey split anchor?)
      (let* ((account ((keylist-get-info (sortkey-list parameters)
                                         sortkey 'renderer-fn) split))
             (name (account-namestring account
                                       (report-uses? 'sort-account-code)
                                       #t
                                       (report-uses? 'sort-account-full-name))))
        (if (and (report-uses? 'sort-account-description)
                 (not (string-null? (xaccAccountGetDescription account))))
            (string-append name ": " (xaccAccountGetDescription account))
            name)))

    ;; generic renderer. retrieve renderer-fn which should return a str
    (define (render-generic sortkey split)
      ((keylist-get-info (sortkey-list parameters) sortkey 'renderer-fn) split))

    (define (render-summary split level anchor?)
      (let ((sortkey (case level
                      ((primary) (report-uses? 'primary-key))
                      ((secondary) (report-uses? 'secondary-key))))
            (date-subtotal-key (case level
                                ((primary) (report-uses? 'primary-date-subtotal))
                                ((secondary) (report-uses? 'secondary-date-subtotal)))))
        (cond
         ((memq sortkey DATE-SORTING-TYPES)
          (render-date date-subtotal-key split))
         ((memq sortkey ACCOUNT-SORTING-TYPES)
          (render-account sortkey split anchor?))
         (else
          (render-generic sortkey split)))))

    (define (render-grand-total)
      (G_ "Grand Total"))

    (define primary-subtotal-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    (define secondary-subtotal-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    (define total-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-split-row
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (add-split-row split cell-calculators row-style transaction-row?)
      (let* ((account (xaccSplitGetAccount split))
             (reversible-account? (acc-reverse? account)))

        (unless (report-uses? 'subtotals-only)
          (gnc:html-table-append-row/markup!
           table row-style
           (append
            (gnc:html-make-empty-cells indent-level)
            (map (lambda (left-col)
                   ((assq-ref left-col 'renderer-fn) split transaction-row?))
                 left-columns)
            (map (lambda (cell)
                   (let* ((cell-monetary ((assq-ref cell 'calc-fn)
                                          split transaction-row?))
                          (reverse? (and (assq-ref cell 'reverse-column?)
                                         reversible-account?))
                          (cell-content (and cell-monetary
                                             (if reverse?
                                                 (gnc:monetary-neg cell-monetary)
                                                 cell-monetary))))
                     (and cell-content
                          (gnc:make-html-table-cell/markup
                           "number-cell"
                           ;; If link option is enabled, to avoid cluttering, we show links
                           ;; only on number cells that are set to show a subtotal,
                           ;; unless no columns are set to show a subtotal, in which case links
                           ;; are shown on all number cells.
                           (if (and (report-uses? 'links) (or (cell-with-subtotals? cell)
                                                              (not (any cell-with-subtotals?
                                                                        cell-calculators))))
                               (gnc:html-split-anchor split cell-content)
                               cell-content)))))
                 cell-calculators))))

        (when transaction-row?
          (for-each
           (lambda (prime-collector sec-collector tot-collector cell)
             (when (assq-ref cell 'subtotal?)
               (let ((value ((assq-ref cell 'calc-fn) split transaction-row?)))
                 (when value
                   (let ((comm (gnc:gnc-monetary-commodity value))
                         (amt (gnc:gnc-monetary-amount value)))
                     (prime-collector 'add comm amt)
                     (sec-collector 'add comm amt)
                     (tot-collector 'add comm amt))))))
           primary-subtotal-collectors
           secondary-subtotal-collectors
           total-collectors
           cell-calculators))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; do-rows-with-subtotals

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define grid (make-grid))
    (define primary-subtotal-comparator (report-uses? 'primary-key/split-sortvalue))
    (define secondary-subtotal-comparator (report-uses? 'secondary-key/split-sortvalue))

    (gnc:html-table-set-col-headers!
     table (concatenate (list
                         (gnc:html-make-empty-cells indent-level)
                         headings-left-columns
                         headings-right-columns)))

    (when (report-uses? 'primary-key/renderer-fn)
      (add-subheading (render-summary (car splits) 'primary #t)
                      def:primary-subtotal-style (car splits) 'primary))

    (when (report-uses? 'secondary-key/renderer-fn)
      (add-subheading (render-summary (car splits) 'secondary #t)
                      def:secondary-subtotal-style (car splits) 'secondary))

    (let loop ((splits splits)
               (odd-row? #t)
               (work-done 0))

      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))

      (if (null? splits)

          (when (report-uses? 'grand-total)
            (gnc:html-table-append-row/markup!
             table def:grand-total-style
             (list
              (gnc:make-html-table-cell/size
               1 (+ indent-level width-left-columns width-right-columns)
               (gnc:make-html-text (gnc:html-markup-hr)))))

            (add-subtotal-row
             (render-grand-total) total-collectors
             def:grand-total-style 'total 'row-total 'col-total))

          (let* ((current (car splits))
                 (rest (cdr splits))
                 (next (and (pair? rest) (car rest))))

            (add-split-row current calculated-cells
                           (if (or odd-row? (report-uses? 'multiline))
                               def:normal-row-style
                               def:alternate-row-style)
                           #t)

            (when (report-uses? 'multiline)
              (for-each
               (lambda (othersplit)
                 (add-split-row othersplit calculated-cells
                                def:alternate-row-style #f))
               (delete current (xaccTransGetSplitList
                                (xaccSplitGetParent current)))))

            (cond
             ((and primary-subtotal-comparator
                   (or (not next)
                       (not (equal? (primary-subtotal-comparator current)
                                    (primary-subtotal-comparator next)))))
              (when secondary-subtotal-comparator
                (add-subtotal-row (total-string
                                   (render-summary current 'secondary #f))
                                  secondary-subtotal-collectors
                                  def:secondary-subtotal-style
                                  'secondary
                                  (cons (primary-subtotal-comparator current)
                                        (render-summary current 'primary #f))
                                  (cons (secondary-subtotal-comparator current)
                                        (render-summary current 'secondary #f)))
                (for-each
                 (lambda (coll)
                   (coll 'reset #f #f))
                 secondary-subtotal-collectors))
              (add-subtotal-row (total-string
                                 (render-summary current 'primary #f))
                                primary-subtotal-collectors
                                def:primary-subtotal-style
                                'primary
                                (cons (primary-subtotal-comparator current)
                                      (render-summary current 'primary #f))
                                'col-total)
              (for-each
               (lambda (coll)
                 (coll 'reset #f #f))
               primary-subtotal-collectors)
              (when next
                (add-subheading (render-summary next 'primary #t)
                                def:primary-subtotal-style next 'primary)
                (when secondary-subtotal-comparator
                  (add-subheading (render-summary next 'secondary #t)
                                  def:secondary-subtotal-style next
                                  'secondary))
                (converted-running-prime-collector 'reset #f #f)
                (original-running-prime-collector 'reset #f #f)
                (converted-running-sec-collector 'reset #f #f)
                (original-running-sec-collector 'reset #f #f)))

             (else
              (when (and secondary-subtotal-comparator
                         (or (not next)
                             (not (equal? (secondary-subtotal-comparator current)
                                          (secondary-subtotal-comparator next)))))
                (add-subtotal-row (total-string
                                   (render-summary current 'secondary #f))
                                  secondary-subtotal-collectors
                                  def:secondary-subtotal-style
                                  'secondary
                                  (if primary-subtotal-comparator
                                      (cons (primary-subtotal-comparator current)
                                            (render-summary current 'primary #f))
                                      (cons #f ""))
                                  (cons (secondary-subtotal-comparator current)
                                        (render-summary current 'secondary #f)))
                (for-each
                 (lambda (coll)
                   (coll 'reset #f #f))
                 secondary-subtotal-collectors)
                (when next
                  (add-subheading (render-summary next 'secondary #t)
                                  def:secondary-subtotal-style next 'secondary)
                  (converted-running-sec-collector 'reset #f #f)
                  (original-running-sec-collector 'reset #f #f)))))

            (loop rest (not odd-row?) (1+ work-done)))))

    (let ((csvlist (cond
                    ((any (cut assq-ref <> 'start-dual-column?) calculated-cells)
                     ;; there are mergeable cells. don't return a list.
                     (N_ "CSV disabled for double column amounts"))

                    (else
                     (map
                      (lambda (cell coll)
                        (cons (assq-ref cell 'heading)
                              (coll 'format gnc:make-gnc-monetary #f)))
                      calculated-cells total-collectors)))))
      (values table grid csvlist))))

;; grid data structure
(define (make-grid)
  '())
(define (cell-match? cell row col)
  (and (or (not row) (equal? row (vector-ref cell 0)))
       (or (not col) (equal? col (vector-ref cell 1)))))
(define (grid-get grid row col)
  ;; grid filter - get all row/col - if #f then retrieve whole row/col
  (filter
   (lambda (cell)
     (cell-match? cell row col))
   grid))
(define (grid-rows grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 0)) grid)))
(define (grid-cols grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 1)) grid)))
(define (grid-add grid row col data)
  ;; we don't need to check for duplicate cells in a row/col because
  ;; in the trep it should never happen.
  (cons (vector row col data) grid))
(define (grid->html-table grid)
  (define (<? a b)
    (cond ((string? (car a)) (gnc:string-locale<? (car a) (car b)))
          ((number? (car a)) (< (car a) (car b)))
          (else (gnc:error "unknown sortvalue"))))
  (define list-of-rows (sort (delete 'row-total (grid-rows grid)) <?))
  (define list-of-cols (sort (delete 'col-total (grid-cols grid)) <?))
  (define row-average-enabled? (and (pair? list-of-cols) (pair? (cdr list-of-cols))))
  (define (monetary-div monetary divisor)
    (and monetary
         (let* ((amount (gnc:gnc-monetary-amount monetary))
                (currency (gnc:gnc-monetary-commodity monetary))
                (scu (gnc-commodity-get-fraction currency)))
           (gnc:make-gnc-monetary
            currency (gnc-numeric-convert
                      (/ amount divisor) scu GNC-HOW-RND-ROUND)))))
  (define (row->num-of-commodities row)
    ;; for a row, find the maximum number of commodities being stored
    (apply max
           (map (lambda (col)
                  (let ((cell (grid-get grid row col)))
                    (if (null? cell) 0
                        (length (vector-ref (car cell) 2)))))
                (cons 'col-total list-of-cols))))
  (define (make-table-cell row col commodity-idx divisor)
    (let ((cell (grid-get grid row col)))
      (if (null? cell) ""
          (gnc:make-html-table-cell/markup
           "number-cell"
           (monetary-div
            (list-ref-safe (vector-ref (car cell) 2) commodity-idx)
            divisor)))))
  (define (make-row row commodity-idx)
    (append
     (list (cond
            ((positive? commodity-idx) "")
            ((eq? row 'row-total) (G_ "Grand Total"))
            (else (cdr row))))
     (map (lambda (col) (make-table-cell row col commodity-idx 1))
          list-of-cols)
     (list (make-table-cell row 'col-total commodity-idx 1))
     (if row-average-enabled?
         (list (make-table-cell
                row 'col-total commodity-idx (length list-of-cols)))
         '())))
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-caption! table (G_ optname-grid))
    (gnc:html-table-set-col-headers!
     table (append (list "")
                   (map cdr list-of-cols)
                   (list (G_ "Total"))
                   (if row-average-enabled? (list (G_ "Average")) '())))
    (gnc:html-table-set-style!
     table "th"
     'attribute (list "class" "column-heading-right"))
    (for-each
     (lambda (row)
       (for-each
        (lambda (commodity-idx)
          (gnc:html-table-append-row!
           table (make-row row commodity-idx)))
        (iota (row->num-of-commodities row))))
     (if (memq 'row-total (grid-rows grid))
         (append list-of-rows '(row-total))
         list-of-rows))
    table))

(define* (gnc:trep-renderer
          report-obj #:key custom-calculated-cells empty-report-message
          custom-split-filter split->date split->date-include-false?
          custom-source-accounts
          export-type)
  ;; the trep-renderer is a define* function which, at minimum, takes
  ;; the report object
  ;;
  ;; the optional arguments are:
  ;; #:custom-calculated-cells - a list of pairs to define customized data columns
  ;; #:empty-report-message - a str or html-object displayed at the initial run
  ;; #:custom-split-filter - a split->bool function to add to the split filter
  ;; #:split->date - a split->time64 which overrides the default posted date filter
  ;;     if a derived report specifies this, the Date Filter option
  ;;     becomes unused and should be hidden via gnc:option-make-internal!
  ;; #:split->date-include-false? - addendum to above, specifies filter behaviour if
  ;;     split->date returns #f. useful to include unreconciled splits in reconcile
  ;;     report. it can be useful for alternative date filtering, e.g. filter by
  ;;     transaction->invoice->payment date.
  ;; #:export-type - are provided for CSV export
  ;; #:custom-source-accounts - alternate list-of-accounts to retrieve splits from

  (define options (gnc:report-options report-obj))
  (define (opt-val section name)
    (gnc-optiondb-lookup-value (gnc:optiondb options) section name))

  (gnc:report-starting (opt-val gnc:pagename-general gnc:optname-reportname))

  (let* ((document (gnc:make-html-document))
         (account-matcher (opt-val pagename-filter optname-account-matcher))
         (account-matcher-neg (opt-val pagename-filter optname-account-matcher-exclude))
         (account-matcher-regexp
          (and (opt-val pagename-filter optname-account-matcher-regex)
               (if (defined? 'make-regexp)
                   (catch 'regular-expression-syntax
                     (lambda () (make-regexp account-matcher))
                     (const 'invalid-account-regex))
                   'no-guile-regex-support)))
         (c_account_0 (or custom-source-accounts
                          (opt-val gnc:pagename-accounts optname-accounts)))
         (acct? (lambda (acc)
                  (if (regexp? account-matcher-regexp)
                      (regexp-exec account-matcher-regexp
                                   (gnc-account-get-full-name acc))
                      (string-contains (gnc-account-get-full-name acc)
                                       account-matcher))))
         (c_account_1 (if (string-null? account-matcher)
                          c_account_0
                          (filter (if account-matcher-neg (negate acct?) acct?)
                                  c_account_0)))
         (c_account_2 (opt-val gnc:pagename-accounts optname-filterby))
         (filter-mode (opt-val gnc:pagename-accounts optname-filtertype))
         (begindate (gnc:time64-start-day-time
                     (gnc:date-option-absolute-time
                      (opt-val gnc:pagename-general optname-startdate))))
         (enddate (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (opt-val gnc:pagename-general optname-enddate))))
         (date-source (if split->date
                          'custom
                          (opt-val gnc:pagename-general optname-date-source)))
         (transaction-matcher (opt-val pagename-filter optname-transaction-matcher))
         (transaction-filter-case-insensitive?
          (opt-val pagename-filter optname-transaction-matcher-caseinsensitive))
         (transaction-matcher-regexp
          (and (opt-val pagename-filter optname-transaction-matcher-regex)
               (if (defined? 'make-regexp)
                   (catch 'regular-expression-syntax
                     (lambda ()
                       (if transaction-filter-case-insensitive?
                           (make-regexp transaction-matcher regexp/icase)
                           (make-regexp transaction-matcher)))
                     (const 'invalid-transaction-regex))
                   'no-guile-regex-support)))
         (transaction-filter-exclude?
          (opt-val pagename-filter optname-transaction-matcher-exclude))
         (void-filter (opt-val pagename-filter optname-void-transactions))
         (reconcile-filter (opt-val pagename-filter optname-reconcile-status))
         (cleared-filter
          (logand
           (keylist-get-info reconcile-status-list reconcile-filter 'filter-types)
           (keylist-get-info show-void-list void-filter 'how)))
         (report-title (opt-val gnc:pagename-general gnc:optname-reportname))
         (detail-is-single? (eq? (opt-val gnc:pagename-display optname-detail-level) 'single))
         (split-action? (qof-book-use-split-action-for-num-field (gnc-get-current-book)))
         (amount-setting (opt-val gnc:pagename-display (N_ "Amount")))
         (reversed-signs (opt-val gnc:pagename-display (N_ "Sign Reverses")))
         (primary-key (opt-val pagename-sorting optname-prime-sortkey))
         (primary-order (opt-val pagename-sorting optname-prime-sortorder))
         (primary-subtotal (opt-val pagename-sorting optname-prime-subtotal))
         (primary-date-subtotal (opt-val pagename-sorting optname-prime-date-subtotal))
         (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
         (secondary-order (opt-val pagename-sorting optname-sec-sortorder))
         (secondary-subtotal (opt-val pagename-sorting optname-sec-subtotal))
         (secondary-date-subtotal (opt-val pagename-sorting optname-sec-date-subtotal))
         (closing-match (keylist-get-info
                         show-closing-list
                         (opt-val pagename-filter optname-closing-transactions)
                         'closing-match))
         (splits '())
         (subtotal-table? (and (opt-val gnc:pagename-display optname-grid)
                               (if (memq primary-key DATE-SORTING-TYPES)
                                   (keylist-get-info date-subtotal-list
                                                     primary-date-subtotal 'renderer-fn)
                                   primary-subtotal)
                               (memq (opt-val gnc:pagename-display (N_ "Amount"))
                                     '(single double))))
         (infobox-display (opt-val gnc:pagename-general optname-infobox-display))
         (query (qof-query-create-for-splits)))

    ;; define a preprocessed alist of report parameters.
    ;; each key returns either the parameter value or #f is the parameter is not used.
    (define parameters
      ;; define parameters-tail, the main set of parameters.
      ;; additional variables and parameters that require this first set of parameters
      ;; wll be prepended further down.
      (let* ((parameters-tail
              (list
                ;; parameters based on file properties
                (cons 'split-action split-action?)
                ;; parameters based on account and filter options
                (cons 'accounts-matched (or (null? c_account_1) c_account_1))
                ;; parameters based on common currency options
                (cons 'common-currency
                      (and (opt-val pagename-currency optname-common-currency)
                          (opt-val pagename-currency optname-currency)))
                (cons 'common-currency/original
                      (and (opt-val pagename-currency optname-common-currency)
                          (opt-val pagename-currency optname-orig-currency)))
                (cons 'common-currency/price-source
                      (and (opt-val pagename-currency optname-common-currency)
                          (opt-val pagename-currency optname-price-source)))
                ;; parameters based on display options
                (cons 'date (opt-val gnc:pagename-display (N_ "Date")))
                (cons 'reconciled-date (opt-val gnc:pagename-display (N_ "Reconciled Date")))
                (cons 'entered (opt-val gnc:pagename-display (N_ "Date Entered")))
                (cons 'num (if split-action?
                                (opt-val gnc:pagename-display (N_ "Num/Action"))
                                (opt-val gnc:pagename-display (N_ "Num"))))
                (cons 'description (opt-val gnc:pagename-display (N_ "Description")))
                (cons 'account-name (opt-val gnc:pagename-display (N_ "Account Name")))
                (cons 'other-account-name
                      (and detail-is-single?
                            (opt-val gnc:pagename-display (N_ "Other Account Name"))))
                (cons 'shares (opt-val gnc:pagename-display (N_ "Shares")))
                (cons 'price (opt-val gnc:pagename-display (N_ "Price")))
                (cons 'doclink (opt-val gnc:pagename-display (N_ "Link")))
                (cons 'amount-single (eq? amount-setting 'single))
                (cons 'amount-double (eq? amount-setting 'double))
                (cons 'running-balance (opt-val gnc:pagename-display "Account Balance"))
                (cons 'account-full-name
                      (opt-val gnc:pagename-display (N_ "Use Full Account Name")))
                (cons 'memo (opt-val gnc:pagename-display (N_ "Memo")))
                (cons 'notes (opt-val gnc:pagename-display (N_ "Notes")))
                (cons 'account-code (opt-val gnc:pagename-display (N_ "Account Code")))
                (cons 'other-account-code
                      (and detail-is-single?
                            (opt-val gnc:pagename-display (N_ "Other Account Code"))))
                (cons 'other-account-full-name
                      (and detail-is-single?
                            (opt-val gnc:pagename-display (N_ "Use Full Other Account Name"))))
                (cons 'trans-number (and split-action?
                                        (opt-val gnc:pagename-display (N_ "Trans Number"))))
                (cons 'links (opt-val gnc:pagename-display "Enable Links"))
                (cons 'reversed-signs (or (eq? reversed-signs 'none) reversed-signs))
                (cons 'multiline (eq? (opt-val gnc:pagename-display optname-detail-level)
                                      'multi-line))
                (cons 'grand-total (opt-val gnc:pagename-display optname-grand-total))
                (cons 'running-grand-total
                      (or (eq? (opt-val gnc:pagename-display optname-running-totals) 'grand)
                          (eq? (opt-val gnc:pagename-display optname-running-totals) 'all)))
                ;; parameters based on general options
                (cons 'begindate begindate)
                (cons 'enddate enddate)
                (cons 'export-table (opt-val gnc:pagename-general optname-table-export))
                ;; parameters based on sorting options
                (cons 'primary-key primary-key)
                (cons 'primary-date-subtotal (if (memq primary-key DATE-SORTING-TYPES)
                                                primary-date-subtotal))
                (cons 'secondary-key secondary-key)
                (cons 'secondary-date-subtotal (if (memq secondary-key DATE-SORTING-TYPES)
                                                  secondary-date-subtotal))
                (cons 'indenting (opt-val pagename-sorting optname-indenting))
                (cons 'sort-account-code (opt-val pagename-sorting (N_ "Show Account Code")))
                (cons 'sort-account-full-name
                      (opt-val pagename-sorting (N_ "Show Full Account Name")))
                (cons 'sort-account-description
                      (opt-val pagename-sorting (N_ "Show Account Description")))
                (cons 'informal-headers (opt-val pagename-sorting optname-show-informal-headers))
                ;; Parameters based on a mix of options
                ;; This parameter is set to #t if an account balance can be displayed
                ;; as a running balance with a balance forward at the top.
                ;; It implies most default options are maintained :
                ;; - Detail level is set to one transaction per line,
                ;; - Date filter is set to date posted
                ;; - Filtering on transactions is kept as per default
                ;; - The primary sort is set to account name (or code)
                ;; - The primary subtotals are displayed (to separate accounts)
                ;; - The secondary sort is set to register order or date ascending.
                (cons 'bal-bf
                  (and detail-is-single?
                      (eq? (opt-val gnc:pagename-general optname-date-source) 'posted)
                      (string-null? transaction-matcher)
                      (eq? reconcile-filter 'all)
                      (eq? void-filter 'non-void-only)
                      (memq primary-key '(account-name account-code))
                      (memq secondary-key '(register-order date))
                      primary-subtotal
                      (eq? secondary-order 'ascend)))
                ))
             ;; additional variables that call sortkey-list and therefore
             ;; whose definition requires the existing parameters-tail alist
             (primary-get-info (lambda (info)
                                (if (memq primary-key DATE-SORTING-TYPES)
                                    (keylist-get-info date-subtotal-list primary-date-subtotal info)
                                    (and (SUBTOTAL-ENABLED? primary-key parameters-tail)
                                          primary-subtotal
                                          (keylist-get-info (sortkey-list parameters-tail) primary-key info)))))
             (primary-key/renderer-fn (primary-get-info 'renderer-fn))
             (secondary-get-info (lambda (info)
                                  (if (memq secondary-key DATE-SORTING-TYPES)
                                      (keylist-get-info date-subtotal-list secondary-date-subtotal info)
                                      (and (SUBTOTAL-ENABLED? secondary-key parameters-tail)
                                          secondary-subtotal
                                          (keylist-get-info (sortkey-list parameters-tail) secondary-key info)))))
             (secondary-key/renderer-fn (secondary-get-info 'renderer-fn)))

        ;; prepend additional parameters that rely on sortkey-list and therefore
        ;; whose definition requires the existing parameters-tail alist
        (cons*
          ;; additional parameters based on display options
          (cons 'running-prime
                (and primary-key/renderer-fn
                      (or (eq? (opt-val gnc:pagename-display optname-running-totals) 'sub)
                          (eq? (opt-val gnc:pagename-display optname-running-totals) 'all))))
          (cons 'running-sec
                (and secondary-key/renderer-fn
                      (or (eq? (opt-val gnc:pagename-display optname-running-totals) 'sub)
                          (eq? (opt-val gnc:pagename-display optname-running-totals) 'all))))
          ;; additional parameters based on sorting options
          (cons 'primary-key/renderer-fn primary-key/renderer-fn)
          (cons 'primary-key/split-sortvalue (primary-get-info 'split-sortvalue))
          (cons 'secondary-key/renderer-fn secondary-key/renderer-fn)
          (cons 'secondary-key/split-sortvalue (secondary-get-info 'split-sortvalue))
          (cons 'subtotals-only
                (and (opt-val pagename-sorting optname-show-subtotals-only)
                      (or primary-key/renderer-fn secondary-key/renderer-fn)))
          ;; parameters-tail comes at the end
          parameters-tail)))

    (define custom-sort?
        (or (and (memq primary-key DATE-SORTING-TYPES)
                (not (eq? primary-date-subtotal 'none)))
            (and (memq secondary-key DATE-SORTING-TYPES)
                (not (eq? secondary-date-subtotal 'none)))
            (or (CUSTOM-SORTING? primary-key parameters)
                (CUSTOM-SORTING? secondary-key parameters))))

    (define (match? str)
      (cond
       (transaction-matcher-regexp
        (regexp-exec transaction-matcher-regexp str))
       (transaction-filter-case-insensitive?
        (string-contains-ci str transaction-matcher))
       (else
        (string-contains str transaction-matcher))))

    (define (generic-less? split-X split-Y sortkey date-subtotal-key ascend?)
      ;; compare splits X and Y, whereby
      ;; sortkey and date-subtotal-key specify the options used
      ;; ascend? specifies whether ascending or descending
      (let* ((comparator-function
              (if (memq sortkey DATE-SORTING-TYPES)
                  (let ((date (keylist-get-info
                               (sortkey-list parameters)
                               sortkey 'split-sortvalue))
                        (date-comparator
                         (keylist-get-info date-subtotal-list
                                           date-subtotal-key 'date-sortvalue)))
                    (lambda (s)
                      (and date-comparator (date-comparator (date s)))))
                  (or (keylist-get-info (sortkey-list parameters)
                                        sortkey 'split-sortvalue)
                      (lambda (s) #f))))
             (value-of-X (comparator-function split-X))
             (value-of-Y (comparator-function split-Y))
             (op (if (string? value-of-X)
                     (if ascend? gnc:string-locale<? gnc:string-locale>?)
                     (if ascend? < >))))
        (and value-of-X (op value-of-X value-of-Y))))

    (define (primary-comparator? X Y)
      (generic-less? X Y primary-key
                     primary-date-subtotal
                     (eq? primary-order 'ascend)))

    (define (secondary-comparator? X Y)
      (generic-less? X Y secondary-key
                     secondary-date-subtotal
                     (eq? secondary-order 'ascend)))

    ;; This will, by default, sort the split list by ascending posted-date.
    (define (date-comparator? X Y)
      (generic-less? X Y 'date 'none #t))

    (define (transaction-filter-match split)
      (or (match? (xaccTransGetDescription (xaccSplitGetParent split)))
          (match? (xaccTransGetNotes (xaccSplitGetParent split)))
          (match? (xaccSplitGetMemo split))))

    (define (is-filter-member split account-list)
      (define (same-split? s) (equal? s split))
      (define (from-account? s) (member (xaccSplitGetAccount s) account-list))
      (let lp ((splits (xaccTransGetSplitList (xaccSplitGetParent split))))
        (match splits
          (() #f)
          (((? same-split?) . rest) (lp rest))
          (((? from-account?) . _) #t)
          ((_ . rest) (lp rest)))))

    (cond
     ((or (null? c_account_1)
          (symbol? account-matcher-regexp)
          (symbol? transaction-matcher-regexp))

      (gnc:html-document-add-object!
       document
       (cond
        ((null? c_account_1)
         (gnc:html-make-no-account-warning report-title (gnc:report-id report-obj)))

        ((symbol? account-matcher-regexp)
         (gnc:html-make-generic-warning
          report-title (gnc:report-id report-obj)
          (string-append (G_ "Error") " " (symbol->string account-matcher-regexp))
          ""))

        ((symbol? transaction-matcher-regexp)
         (gnc:html-make-generic-warning
          report-title (gnc:report-id report-obj)
          (string-append (G_ "Error") " " (symbol->string transaction-matcher-regexp))
          ""))))

      (gnc:html-document-set-export-error document "No accounts, or regexp error")

      ;; if an empty-report-message is passed by a derived report to
      ;; the renderer, display it here.
      (when empty-report-message
        (gnc:html-document-add-object!
         document
         empty-report-message))

      (when (memq infobox-display '(always no-match))
        (gnc:html-document-add-object!
         document
         (gnc:html-render-options-changed options))))

     (else
      (qof-query-set-book query (gnc-get-current-book))
      (xaccQueryAddAccountMatch query c_account_1 QOF-GUID-MATCH-ANY QOF-QUERY-AND)
      (xaccQueryAddClearedMatch query cleared-filter QOF-QUERY-AND)
      (when (eq? date-source 'posted)
        (xaccQueryAddDateMatchTT query #t begindate #t enddate QOF-QUERY-AND))
      (when (boolean? closing-match)
        (xaccQueryAddClosingTransMatch query closing-match QOF-QUERY-AND))
      (unless custom-sort?
        (qof-query-set-sort-order
         query
         (keylist-get-info (sortkey-list parameters) primary-key 'sortkey)
         (keylist-get-info (sortkey-list parameters) secondary-key 'sortkey)
         (list QUERY-DEFAULT-SORT))
        (qof-query-set-sort-increasing
         query (eq? primary-order 'ascend) (eq? secondary-order 'ascend)
         #t))

      (if (opt-val "__trep" "unique-transactions")
          (set! splits (xaccQueryGetSplitsUniqueTrans query))
          (set! splits (qof-query-run query)))

      (qof-query-destroy query)

      ;; Combined Filter:
      ;; - include/exclude using split->date according to date options
      ;; - include/exclude splits to/from selected accounts
      ;; - substring/regex matcher for Transaction Description/Notes/Memo
      ;; - custom-split-filter, a split->bool function for derived reports
      (set! splits
        (filter
         (lambda (split)
           (let* ((trans (xaccSplitGetParent split)))
             (and (case date-source
                    ((posted) #t)
                    ((reconciled)
                     (if (char=? (xaccSplitGetReconcile split) #\y)
                         (<= begindate (xaccSplitGetDateReconciled split) enddate)
                         #t))
                    ((entered) (<= begindate (xaccTransRetDateEntered trans) enddate))
                    ((custom)
                     (let ((date (split->date split)))
                        (if date
                            (<= begindate date enddate)
                            split->date-include-false?)))
                    (else (gnc:warn "invalid date-source" date-source) #t))
                  (case filter-mode
                    ((none) #t)
                    ((include) (is-filter-member split c_account_2))
                    ((exclude) (not (is-filter-member split c_account_2))))
                  (or (string-null? transaction-matcher)
                      (if transaction-filter-exclude?
                          (not (transaction-filter-match split))
                          (transaction-filter-match split)))
                  (or (not custom-split-filter)
                      (custom-split-filter split)))))
         splits))

      (when custom-sort?
        (set! splits (stable-sort! splits date-comparator?))
        (set! splits (stable-sort! splits secondary-comparator?))
        (set! splits (stable-sort! splits primary-comparator?)))

      (cond
       ((null? splits)
        ;; error condition: no splits found
        (gnc:html-document-add-object!
         document
         (gnc:html-make-generic-warning
          report-title (gnc:report-id report-obj)
          NO-MATCHING-TRANS-HEADER NO-MATCHING-TRANS-TEXT))

        (gnc:html-document-set-export-error document "No splits found")

        (when (memq infobox-display '(always no-match))
          (gnc:html-document-add-object!
           document
           (gnc:html-render-options-changed options))))

       (else
        (let-values (((table grid csvlist)
                      (make-split-table splits options parameters
                                        custom-calculated-cells)))

          (gnc:html-document-set-title! document report-title)

          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-h3
             (format #f
                     ;; Translators: Both ~a's are dates
                     (G_ "From ~a to ~a")
                     (qof-print-date begindate)
                     (qof-print-date enddate)))))

          (when (eq? infobox-display 'always)
            (gnc:html-document-add-object!
             document
             (gnc:html-render-options-changed options)))

          (when subtotal-table?
            (gnc:html-document-add-object! document (grid->html-table grid)))

          (unless (and subtotal-table?
                       (opt-val pagename-sorting optname-show-subtotals-only))
            (gnc:html-document-add-object! document table))

          (cond
           ((eq? export-type 'csv)
            (cond
             ((pair? csvlist)
              (let ((iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO)))
                (gnc:html-document-set-export-string
                 document
                 (lists->csv
                  (cons*
                   `("from" ,(gnc-print-time64 begindate iso-date))
                   `("to" ,(gnc-print-time64 enddate iso-date))
                   csvlist)))))

             (else
              (gnc:html-document-set-export-error document csvlist))))))))))

    (gnc:report-finished)

    document))
