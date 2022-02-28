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
(use-modules (ice-9 match))

(export gnc:trep-options-generator)
(export gnc:trep-renderer)
(export gnc:lists->csv)

;; Define the strings here to avoid typos and make changes easier.

;;Accounts
(define optname-accounts (N_ "Accounts"))
(define optname-filterby (N_ "Filter By..."))
(define optname-filtertype (N_ "Filter Type"))

;;Display
(define optname-detail-level (N_ "Detail Level"))
(define optname-grid (N_ "Subtotal Table"))

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

(define (sortkey-list split-action?)
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

        (if split-action?
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

(define (SUBTOTAL-ENABLED? sortkey split-action?)
  ;; this returns whether sortkey *can* be subtotalled/grouped.
  ;; it checks whether a renderer-fn is defined.
  (keylist-get-info (sortkey-list split-action?) sortkey 'renderer-fn))

(define (CUSTOM-SORTING? sortkey split-action?)
  ;; sortkey -> bool
  ;;
  ;; this returns which sortkeys which *must* use the custom sorter.
  ;; it filters whereby a split-sortvalue is defined (i.e. the splits
  ;; can be compared according to their 'sortvalue) but the QofQuery
  ;; sortkey is not defined (i.e. their 'sortkey is #f).
  (and (keylist-get-info (sortkey-list split-action?) sortkey 'split-sortvalue)
       (not (keylist-get-info (sortkey-list split-action?) sortkey 'sortkey))))

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
  (define options (gnc:new-options))
  (define BOOK-SPLIT-ACTION
    (qof-book-use-split-action-for-num-field (gnc-get-current-book)))
  (define (gnc:register-trep-option new-option)
    (gnc:register-option options new-option))

  ;; (Feb 2018) Note to future hackers - this gnc:trep-options-generator
  ;; defines a long set of options to be assigned as an object in
  ;; the report. This long list (52 at Feb 2018 count) of options
  ;; may be modified in a derived report (see income-gst-statement.scm)
  ;; via gnc:make-internal! and gnc-unregister-option to hide
  ;; and remove options, respectively. If an option is unregistered,
  ;; don't forget to re-register them via gnc:register-option, unless
  ;; your derived report truly does not require them.

  ;; General options

  (gnc:options-add-date-interval!
   options gnc:pagename-general optname-startdate optname-enddate "a")

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    gnc:pagename-general optname-date-source
    "a5" (G_ "Specify date to filter by...")
    'posted
    (list (vector 'posted (G_ "Date Posted"))
          (vector 'reconciled (G_ "Reconciled Date"))
          (vector 'entered (G_ "Date Entered")))))

  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    pagename-currency optname-common-currency
    "a" (G_ "Convert all transactions into a common currency.") #f #f
    (lambda (x)
      (gnc-option-db-set-option-selectable-by-name
       options pagename-currency optname-currency x)
      (gnc-option-db-set-option-selectable-by-name
       options pagename-currency optname-orig-currency x)
      (gnc-option-db-set-option-selectable-by-name
       options pagename-currency optname-price-source x))))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-currency optname-orig-currency
    "b" (G_ "Also show original currency amounts") #f))

  (gnc:options-add-currency!
   options pagename-currency optname-currency "c")

  (gnc:options-add-price-source!
   options pagename-currency optname-price-source "d" 'pricedb-nearest)

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-table-export
    "g" (G_ "Formats the table suitable for cut & paste exporting with extra cells.")
    #f))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    gnc:pagename-general optname-infobox-display
    "h" (G_ "Add summary of options.")
    'no-match
    ;; This is an alist of conditions for displaying the infobox
    ;; 'no-match for empty-report
    ;; 'match for generated report
    (list (vector 'no-match (G_ "If no transactions matched"))
          (vector 'always (G_ "Always"))
          (vector 'never (G_ "Never")))))

  ;; Filtering Options

  (gnc:register-trep-option
   (gnc:make-string-option
    pagename-filter optname-account-matcher
    "a5" (G_ "Show only accounts whose full name matches this filter e.g. ':Travel' will match \
Expenses:Travel:Holiday and Expenses:Business:Travel. It can be left blank, which will \
disable the filter.")
    ""))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-account-matcher-regex
    "a6"
    (G_ "By default the account filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. 'Car|Flights' will match both \
Expenses:Car and Expenses:Flights. Use a period (.) to match a single character e.g. \
'20../.' will match 'Travel 2017/1 London'. ")
    #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-account-matcher-exclude "a7"
    (G_ "If this option is selected, accounts matching filter are excluded.")
    #f))

  (gnc:register-trep-option
   (gnc:make-string-option
    pagename-filter optname-transaction-matcher
    "i1" (G_ "Show only transactions where description, notes, or memo matches this filter.
e.g. '#gift' will find all transactions with #gift in description, notes or memo. It can be left \
blank, which will disable the filter.")
    ""))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-transaction-matcher-regex
    "i2"
    (G_ "By default the transaction filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. '#work|#family' will match both \
tags within description, notes or memo.")
    #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-transaction-matcher-exclude
    "i3"
    (G_ "If this option is selected, transactions matching filter are excluded.")
    #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-transaction-matcher-caseinsensitive
    "i4"
    (G_ "If this option is selected, transactions matching filter is not case sensitive.")
    #f))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-filter optname-reconcile-status
    "j1" (G_ "Filter by reconcile status.")
    'all
    (keylist->vectorlist reconcile-status-list)))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-filter optname-void-transactions
    "k" (N_ "How to handle void transactions.")
    'non-void-only
    (keylist->vectorlist show-void-list)))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-filter optname-closing-transactions
    "l" (G_ "By default most users should not include closing \
transactions in a transaction report. Closing transactions are \
transfers from income and expense accounts to equity, and must usually \
be excluded from periodic reporting.")
    'exclude-closing
    (keylist->vectorlist show-closing-list)))

  ;; Accounts options

  ;; account to do report on
  (gnc:register-trep-option
   (gnc:make-account-list-option
    gnc:pagename-accounts optname-accounts
    "a" (G_ "Report on these accounts.")
    ;; select, by default, no accounts! Selecting all accounts will
    ;; always imply an insanely long waiting time upon opening, and it
    ;; is almost never useful. So we instead display the normal error
    ;; message saying "Click here", and the user knows how to
    ;; continue.
    (lambda ()
      '())
    #f #t))

  (gnc:register-trep-option
   (gnc:make-account-list-option
    gnc:pagename-accounts optname-filterby
    "c1" (G_ "Filter on these accounts.")
    (lambda ()
      '())
    #f #t))

  (gnc:register-trep-option
   (gnc:make-multichoice-callback-option
    gnc:pagename-accounts optname-filtertype
    "c" (G_ "Filter account.")
    'none
    (keylist->vectorlist filter-list)
    #f
    (lambda (x)
      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-accounts optname-filterby
       (not (eq? x 'none))))))

  ;; Sorting options

  (let ((ascending-choice-list (keylist->vectorlist ascending-list))
        (key-choice-list (keylist->vectorlist (sortkey-list BOOK-SPLIT-ACTION)))
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
              (SUBTOTAL-ENABLED? prime-sortkey BOOK-SPLIT-ACTION))
             (prime-date-sortingtype-enabled (memq prime-sortkey DATE-SORTING-TYPES))
             (sec-sortkey-enabled (not (eq? sec-sortkey 'none)))
             (sec-sortkey-subtotal-enabled
              (SUBTOTAL-ENABLED? sec-sortkey BOOK-SPLIT-ACTION))
             (sec-date-sortingtype-enabled (memq sec-sortkey DATE-SORTING-TYPES)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-subtotal
         prime-sortkey-subtotal-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-sortorder
         prime-sortkey-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-subtotal
         sec-sortkey-subtotal-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-sortorder
         sec-sortkey-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-full-account-name
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-account-code
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-account-description
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-indenting
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)
             (and prime-date-sortingtype-enabled (not (eq? 'none prime-date-subtotal)))
             (and sec-date-sortingtype-enabled (not (eq? 'none sec-date-subtotal)))))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-subtotals-only
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)
             (and prime-date-sortingtype-enabled (not (eq? 'none prime-date-subtotal)))
             (and sec-date-sortingtype-enabled (not (eq? 'none sec-date-subtotal)))))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-informal-headers
         (or (memq prime-sortkey (list 'account-name 'account-code))
             (memq sec-sortkey (list 'account-name 'account-code))))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-date-subtotal
         prime-date-sortingtype-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-date-subtotal
         sec-date-sortingtype-enabled)))

    ;; primary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-prime-sortkey
      "a" (G_ "Sort by this criterion first.")
      prime-sortkey
      key-choice-list #f
      (lambda (x)
        (set! prime-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-full-account-name
      "j1"
      (G_ "Show the full account name for subtotals and subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-account-code
      "j2"
      (G_ "Show the account code for subtotals and subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-account-description
      "j3"
      (G_ "Show the account description for subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-informal-headers
      "j4"
      (G_ "Show the informal headers for debit/credit accounts?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-indenting
      "j5"
      (G_ "Add indenting columns with grouping and subtotals?")
      #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-subtotals-only
      "j6"
      (G_ "Show subtotals only, hiding transactional detail?")
      #f))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-prime-subtotal
      "e5"
      (G_ "Subtotal according to the primary key?")
      prime-sortkey-subtotal-true #f
      (lambda (x)
        (set! prime-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-prime-date-subtotal
      "e2" (G_ "Do a date subtotal.")
      prime-date-subtotal
      date-subtotal-choice-list #f
      (lambda (x)
        (set! prime-date-subtotal x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-sortorder
      "e" (G_ "Order of primary sorting.")
      'ascend
      ascending-choice-list))

    ;; Secondary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-sortkey
      "f"
      (G_ "Sort by this criterion second.")
      sec-sortkey
      key-choice-list #f
      (lambda (x)
        (set! sec-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-sec-subtotal
      "i5"
      (G_ "Subtotal according to the secondary key?")
      sec-sortkey-subtotal-true #f
      (lambda (x)
        (set! sec-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-date-subtotal
      "i2" (G_ "Do a date subtotal.")
      sec-date-subtotal
      date-subtotal-choice-list #f
      (lambda (x)
        (set! sec-date-subtotal x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-sortorder
      "i" (G_ "Order of Secondary sorting.")
      'ascend
      ascending-choice-list)))

  ;; Display options

  (let ((disp-memo? #t)
        (disp-accname? #f)
        (disp-other-accname? #t)
        (disp-detail-level? 'single)
        (amount-value 'double))

    (define (apply-selectable-by-name-display-options)
      (define detail-is-single? (eq? disp-detail-level? 'single))
      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Use Full Account Name")
       disp-accname?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Other Account Name")
       detail-is-single?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Sign Reverses")
       (eq? amount-value 'single))

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display optname-grid
       (not (eq? amount-value 'none)))

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display "Enable Links"
       (not (eq? amount-value 'none)))

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Use Full Other Account Name")
       (and disp-other-accname? detail-is-single?))

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Other Account Code")
       detail-is-single?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Notes")
       disp-memo?))

    (for-each
     (lambda (l)
       (gnc:register-trep-option
        (gnc:make-simple-boolean-option
         gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
     ;; One list per option here with: option-name, sort-tag,
     ;; help-string, default-value
     (list
      (list (N_ "Date")                         "a"  (G_ "Display the date?") #t)
      (list (N_ "Reconciled Date")              "a2" (G_ "Display the reconciled date?") #f)
      (list (N_ "Date Entered")                 "a3" (G_ "Display the entered date?") #f)
      (if BOOK-SPLIT-ACTION
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
      (list (N_ "Shares")                       "k"  (G_ "Display the number of shares?") #f)
      (list (N_ "Link")                         "l5" (G_ "Display the transaction linked document") #f)
      (list (N_ "Price")                        "l"  (G_ "Display the shares price?") #f)
      ;; note the "Amount" multichoice option in between here
      (list optname-grid                        "m5" (G_ "Display a subtotal summary table.") #f)
      (list (N_ "Running Balance")              "n"  (G_ "Display a running balance?") #f)
      (list (N_ "Totals")                       "o"  (G_ "Display the totals?") #t)))

    (when BOOK-SPLIT-ACTION
      (gnc:register-trep-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Trans Number")
        "b2" (G_ "Display the trans number?") #f)))

    ;; Add an option to display the memo, and disable the notes option
    ;; when memos are not included.
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Memo")
      "d"  (G_ "Display the memo?") disp-memo?
      disp-memo?
      (lambda (x)
        (set! disp-memo? x)
        (apply-selectable-by-name-display-options))))

    ;; Ditto for Account Name #t -> Use Full Account Name is selectable
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Account Name")
      "e"  (G_ "Display the account name?") disp-accname?
      disp-accname?
      (lambda (x)
        (set! disp-accname? x)
        (apply-selectable-by-name-display-options))))

    ;; Ditto for Other Account Name #t -> Use Full Other Account Name is selectable
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Other Account Name")
      "h5"  (G_ "Display the other account name? (if this is a split transaction, this parameter is guessed).") disp-other-accname?
      disp-other-accname?
      (lambda (x)
        (set! disp-other-accname? x)
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-display optname-detail-level
      "h" (G_ "Amount of detail to display per transaction.")
      disp-detail-level?
      (list (vector 'multi-line (G_ "One split per line"))
            (vector 'single (G_ "One transaction per line")))
      #f
      (lambda (x)
        (set! disp-detail-level? x)
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-display (N_ "Amount")
      "m" (G_ "Display the amount?")
      amount-value
      (list
       (vector 'none   (G_ "Hide"))
       (vector 'single (G_ "Single Column"))
       (vector 'double (G_ "Two Columns")))
      #f
      (lambda (x)
        (set! amount-value x)
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display (N_ "Enable Links")
      "m2" (G_ "Enable hyperlinks in amounts.") #t))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      gnc:pagename-display (N_ "Sign Reverses")
      "m1" (G_ "Reverse amount display for certain account types.")
      'global
      (keylist->vectorlist sign-reverse-list))))

  ;; this hidden option will toggle whether the default
  ;; qof-query is run, or a different query which ensures
  ;; no transaction is duplicated. It can be enabled in
  ;; a derived report (eg income-gst-statement.scm)
  (gnc:register-trep-option
   (gnc:make-internal-option "__trep" "unique-transactions" #f))

  (gnc:options-set-default-section options gnc:pagename-general)
  options)

;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the big function that builds the whole table.

(define (make-split-table splits options custom-calculated-cells
                          begindate enddate c_account_1)

  (define (opt-val section name)
    (let ((option (gnc:lookup-option options section name)))
      (if option
          (gnc:option-value option)
          (gnc:error "gnc:lookup-option error: " section "/" name))))
  (define BOOK-SPLIT-ACTION
    (qof-book-use-split-action-for-num-field (gnc-get-current-book)))

  (define (build-columns-used)
    (define detail-is-single?
      (eq? (opt-val gnc:pagename-display optname-detail-level) 'single))
    (define amount-setting (opt-val gnc:pagename-display (N_ "Amount")))
    (list (cons 'date (opt-val gnc:pagename-display (N_ "Date")))
          (cons 'reconciled-date (opt-val gnc:pagename-display (N_ "Reconciled Date")))
          (cons 'entered (opt-val gnc:pagename-display (N_ "Date Entered")))
          (cons 'num (if BOOK-SPLIT-ACTION
                         (opt-val gnc:pagename-display (N_ "Num/Action"))
                         (opt-val gnc:pagename-display (N_ "Num"))))
          (cons 'description (opt-val gnc:pagename-display (N_ "Description")))
          (cons 'account-name (opt-val gnc:pagename-display (N_ "Account Name")))
          (cons 'other-account-name
                (and detail-is-single?
                     (opt-val gnc:pagename-display (N_ "Other Account Name"))))
          (cons 'shares (opt-val gnc:pagename-display (N_ "Shares")))
          (cons 'price (opt-val gnc:pagename-display (N_ "Price")))
          (cons 'link (opt-val gnc:pagename-display (N_ "Link")))
          (cons 'amount-single (eq? amount-setting 'single))
          (cons 'amount-double (eq? amount-setting 'double))
          (cons 'common-currency (opt-val pagename-currency optname-common-currency))
          (cons 'amount-original-currency
                (and (opt-val pagename-currency optname-common-currency)
                     (opt-val pagename-currency optname-orig-currency)))
          (cons 'indenting (opt-val pagename-sorting optname-indenting))
          (cons 'subtotals-only
                (and (opt-val pagename-sorting optname-show-subtotals-only)
                     (or (primary-get-info 'renderer-fn)
                         (secondary-get-info 'renderer-fn))))
          (cons 'running-balance (opt-val gnc:pagename-display (N_ "Running Balance")))
          (cons 'account-full-name
                (opt-val gnc:pagename-display (N_ "Use Full Account Name")))
          (cons 'memo (opt-val gnc:pagename-display (N_ "Memo")))
          (cons 'account-code (opt-val gnc:pagename-display (N_ "Account Code")))
          (cons 'other-account-code
                (and detail-is-single?
                     (opt-val gnc:pagename-display (N_ "Other Account Code"))))
          (cons 'other-account-full-name
                (and detail-is-single?
                     (opt-val gnc:pagename-display (N_ "Use Full Other Account Name"))))
          (cons 'sort-account-code (opt-val pagename-sorting (N_ "Show Account Code")))
          (cons 'sort-account-full-name
                (opt-val pagename-sorting (N_ "Show Full Account Name")))
          (cons 'sort-account-description
                (opt-val pagename-sorting (N_ "Show Account Description")))
          (cons 'notes (opt-val gnc:pagename-display (N_ "Notes")))))

  (define (primary-get-info info)
    (let ((sortkey (opt-val pagename-sorting optname-prime-sortkey)))
      (if (memq sortkey DATE-SORTING-TYPES)
          (keylist-get-info
           date-subtotal-list
           (opt-val pagename-sorting optname-prime-date-subtotal) info)
          (and (SUBTOTAL-ENABLED? sortkey BOOK-SPLIT-ACTION)
               (opt-val pagename-sorting optname-prime-subtotal)
               (keylist-get-info (sortkey-list BOOK-SPLIT-ACTION) sortkey info)))))

  (define (secondary-get-info info)
    (let ((sortkey (opt-val pagename-sorting optname-sec-sortkey)))
      (if (memq sortkey DATE-SORTING-TYPES)
          (keylist-get-info
           date-subtotal-list
           (opt-val pagename-sorting optname-sec-date-subtotal) info)
          (and (SUBTOTAL-ENABLED? sortkey BOOK-SPLIT-ACTION)
               (opt-val pagename-sorting optname-sec-subtotal)
               (keylist-get-info (sortkey-list BOOK-SPLIT-ACTION) sortkey info)))))

  (let* ((work-to-do (length splits))
         (table (gnc:make-html-table))
         (used-columns (build-columns-used))
         (opt-use-links? (opt-val gnc:pagename-display "Enable Links"))
         (account-types-to-reverse
          (keylist-get-info sign-reverse-list
                            (opt-val gnc:pagename-display (N_ "Sign Reverses"))
                            'acct-types))
         (is-multiline? (eq? (opt-val gnc:pagename-display optname-detail-level)
                             'multi-line))
         (export? (opt-val gnc:pagename-general optname-table-export)))

    (define (acc-reverse? acc)
      (if account-types-to-reverse
          (memv (xaccAccountGetType acc) account-types-to-reverse)
          (gnc-reverse-balance acc)))

    (define (column-uses? param)
      (assq-ref used-columns param))

    (define exchange-fn
      (if (column-uses? 'common-currency)
          (gnc:case-exchange-time-fn
           (opt-val pagename-currency optname-price-source)
           (opt-val pagename-currency optname-currency)
           (gnc:accounts-get-commodities c_account_1 #f) enddate #f #f)
          gnc:exchange-by-pricedb-nearest))

    (define left-columns
      (let* ((add-if (lambda (pred? . items) (if pred? items '())))
             (left-cols-list
              (append
               (add-if (column-uses? 'date)
                       (vector (G_ "Date")
                               (lambda (split transaction-row?)
                                 (and transaction-row?
                                      (gnc:make-html-table-cell/markup
                                       "date-cell"
                                       (qof-print-date
                                        (xaccTransGetDate
                                         (xaccSplitGetParent split))))))))

               (add-if (column-uses? 'entered)
                       (vector (G_ "Date Entered")
                               (lambda (split transaction-row?)
                                 (and transaction-row?
                                      (gnc:make-html-table-cell/markup
                                       "date-cell" (qof-print-date
                                                    (xaccTransRetDateEntered
                                                     (xaccSplitGetParent split))))))))

               (add-if (column-uses? 'reconciled-date)
                       (vector (G_ "Reconciled Date")
                               (lambda (split transaction-row?)
                                 (let ((reconcile-date
                                        (and (char=? (xaccSplitGetReconcile split) #\y)
                                             (xaccSplitGetDateReconciled split))))
                                   (and reconcile-date
                                        (gnc:make-html-table-cell/markup
                                         "date-cell"
                                         (qof-print-date reconcile-date)))))))

               (add-if (column-uses? 'num)
                       (vector (if (and BOOK-SPLIT-ACTION
                                        (opt-val gnc:pagename-display
                                                 (N_ "Trans Number")))
                                   (G_ "Num/T-Num")
                                   (G_ "Num"))
                               (lambda (split transaction-row?)
                                 (let* ((trans (xaccSplitGetParent split))
                                        (num (gnc-get-num-action trans split))
                                        (t-num (if (and BOOK-SPLIT-ACTION
                                                        (opt-val
                                                         gnc:pagename-display
                                                         (N_ "Trans Number")))
                                                   (gnc-get-num-action trans #f)
                                                   ""))
                                        (num-string (if (string-null? t-num)
                                                        num
                                                        (string-append num "/" t-num))))
                                   (and transaction-row?
                                        (gnc:make-html-table-cell/markup
                                         "text-cell" num-string))))))

               (add-if (column-uses? 'description)
                       (vector (G_ "Description")
                               (lambda (split transaction-row?)
                                 (define trans (xaccSplitGetParent split))
                                 (and transaction-row?
                                      (gnc:make-html-table-cell/markup
                                       "text-cell"
                                       (xaccTransGetDescription trans))))))

               (add-if (column-uses? 'memo)
                       (vector (if (column-uses? 'notes)
                                   (string-append (G_ "Memo") "/" (G_ "Notes"))
                                   (G_ "Memo"))
                               (lambda (split transaction-row?)
                                 (define trans (xaccSplitGetParent split))
                                 (define memo (xaccSplitGetMemo split))
                                 (if (and (string-null? memo) (column-uses? 'notes))
                                     (xaccTransGetNotes trans)
                                     memo))))

               (add-if (or (column-uses? 'account-name) (column-uses? 'account-code))
                       (vector (G_ "Account")
                               (lambda (split transaction-row?)
                                 (account-namestring
                                  (xaccSplitGetAccount split)
                                  (column-uses? 'account-code)
                                  (column-uses? 'account-name)
                                  (column-uses? 'account-full-name)))))

               (add-if (or (column-uses? 'other-account-name)
                           (column-uses? 'other-account-code))
                       (vector (G_ "Transfer from/to")
                               (lambda (split transaction-row?)
                                 (and (< 1 (xaccTransCountSplits
                                            (xaccSplitGetParent split)))
                                      (account-namestring
                                       (xaccSplitGetAccount
                                        (xaccSplitGetOtherSplit split))
                                       (column-uses? 'other-account-code)
                                       (column-uses? 'other-account-name)
                                       (column-uses? 'other-account-full-name))))))

               (add-if (column-uses? 'shares)
                       (vector (G_ "Shares")
                               (lambda (split transaction-row?)
                                 (gnc:make-html-table-cell/markup
                                  "number-cell"
                                  (xaccSplitGetAmount split)))))

               (add-if (column-uses? 'link)
                       (vector ""
                               (lambda (split transaction-row?)
                                 (let ((url (xaccTransGetDocLink
                                             (xaccSplitGetParent split))))
                                   (and (not (string-null? url))
                                        (gnc:make-html-table-cell/markup
                                         "text-cell"
                                         (if opt-use-links?
                                             (gnc:html-transaction-doclink-anchor
                                              (xaccSplitGetParent split)
                                              ;; Translators: 'L' is short for Linked Document
                                              (C_ "Column header for 'Document Link'" "L"))
                                             (C_ "Column header for 'Document Link'" "L"))))))))

               (add-if (column-uses? 'price)
                       (vector (G_ "Price")
                               (lambda (split transaction-row?)
                                 (gnc:make-html-table-cell/markup
                                  "number-cell"
                                  (gnc:default-price-renderer
                                   (xaccTransGetCurrency (xaccSplitGetParent split))
                                   (xaccSplitGetSharePrice split)))))))))

        (if (or (column-uses? 'subtotals-only)
                (and (null? left-cols-list)
                     (or (opt-val gnc:pagename-display "Totals")
                         (primary-get-info 'renderer-fn)
                         (secondary-get-info 'renderer-fn))))
            (list (vector "" (lambda (s t) #f)))
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
           (row-currency (lambda (s) (if (column-uses? 'common-currency)
                                         (opt-val pagename-currency optname-currency)
                                         (split-currency s))))
           (friendly-debit (lambda (a) (gnc-account-get-debit-string (xaccAccountGetType a))))
           (friendly-credit (lambda (a) (gnc-account-get-credit-string (xaccAccountGetType a))))
           (header-commodity (lambda (str)
                               (string-append
                                str
                                (if (column-uses? 'common-currency)
                                    (format #f " (~a)"
                                            (gnc-commodity-get-mnemonic
                                             (opt-val pagename-currency
                                                      optname-currency)))
                                    ""))))
           ;; For conversion to row-currency. Use midday as the
           ;; transaction time so it matches a price on the same day.
           ;; Otherwise it uses midnight which will likely match a
           ;; price on the previous day
           (converted-amount (lambda (s)
                               (exchange-fn
                                (gnc:make-gnc-monetary (split-currency s)
                                                       (split-amount s))
                                (row-currency s)
                                (time64CanonicalDayTime
                                 (xaccTransGetDate (xaccSplitGetParent s))))))
           (converted-debit-amount (lambda (s) (and (positive? (split-amount s))
                                                    (converted-amount s))))
           (converted-credit-amount (lambda (s)
                                      (and (not (positive? (split-amount s)))
                                           (gnc:monetary-neg (converted-amount s)))))
           (original-amount (lambda (s)
                              (gnc:make-gnc-monetary
                               (split-currency s) (split-amount s))))
           (original-debit-amount (lambda (s)
                                    (and (positive? (split-amount s))
                                         (original-amount s))))
           (original-credit-amount (lambda (s)
                                     (and (not (positive? (split-amount s)))
                                          (gnc:monetary-neg (original-amount s)))))
           (running-balance (lambda (s)
                              (gnc:make-gnc-monetary
                               (split-currency s) (xaccSplitGetBalance s)))))
        (append
         ;; each column will be a vector
         ;; (vector heading
         ;;         calculator-function (calculator-function split) to obtain amount
         ;;         reverse-column?     #t to allow reverse signs
         ;;         subtotal?           #t to allow subtotals (ie must be #f for
         ;;                             running balance)
         ;;         start-dual-column?  #t for the debit side of a dual column
         ;;                             (i.e. debit/credit) which means the next
         ;;                             column must be the credit side
         ;;         friendly-heading-fn (friendly-heading-fn account) to retrieve
         ;;                             friendly name for account debit/credit
         ;;                             or 'bal-bf for balance-brought-forward
         ;;         start-dual-column?  #t: merge with next cell for subtotal table.

         (if (column-uses? 'amount-single)
             (list (vector (header-commodity (G_ "Amount"))
                           converted-amount #t #t #f
                           (lambda (a) "") #f))
             '())

         (if (column-uses? 'amount-double)
             (list (vector (header-commodity (G_ "Debit"))
                           converted-debit-amount #f #t #t
                           friendly-debit #t)
                   (vector (header-commodity (G_ "Credit"))
                           converted-credit-amount #f #t #f
                           friendly-credit #f))
             '())

         (if (and (column-uses? 'amount-original-currency)
                  (column-uses? 'amount-single))
             (list (vector (G_ "Amount")
                           original-amount #t #t #f
                           (lambda (a) "") #f))
             '())

         (if (and (column-uses? 'amount-original-currency)
                  (column-uses? 'amount-double))
             (list (vector (G_ "Debit")
                           original-debit-amount #f #t #t
                           friendly-debit #t)
                   (vector (G_ "Credit")
                           original-credit-amount #f #t #f
                           friendly-credit #f))
             '())

         (if (column-uses? 'running-balance)
             (list (vector (G_ "Running Balance")
                           running-balance #t #f #f
                           'bal-bf #f))
             '()))))

    (define calculated-cells
      ;; this part will check whether custom-calculated-cells were specified. this
      ;; describes a custom function which consumes an options list, and generates
      ;; a vectorlist similar to default-calculated-cells as above.
      (if custom-calculated-cells
          (custom-calculated-cells options)
          default-calculated-cells))

    (define headings-left-columns
      (map (lambda (column)
             (vector-ref column 0))
           left-columns))

    (define headings-right-columns
      (map (lambda (column)
             (vector-ref column 0))
           calculated-cells))

    (define width-left-columns (length left-columns))
    (define width-right-columns (length calculated-cells))

    (define primary-indent
      (if (and (column-uses? 'indenting)
               (primary-get-info 'renderer-fn))
          1 0))

    (define secondary-indent
      (if (and (column-uses? 'indenting)
               (secondary-get-info 'renderer-fn))
          1 0))

    (define indent-level
      (+ primary-indent secondary-indent))

    (define (add-subheading data subheading-style split level)
      (let* ((sortkey (opt-val pagename-sorting
                               (case level
                                 ((primary) optname-prime-sortkey)
                                 ((secondary) optname-sec-sortkey))))
             (data (if (and (any (lambda (c) (eq? 'bal-bf (vector-ref c 5)))
                                 calculated-cells)
                            (memq sortkey ACCOUNT-SORTING-TYPES))
                       ;; Translators: Balance b/f stands for "Balance
                       ;; brought forward".
                       (string-append data ": " (G_ "Balance b/f"))
                       data))
             (renderer-fn (keylist-get-info
                           (sortkey-list BOOK-SPLIT-ACTION)
                           sortkey 'renderer-fn))
             (left-indent (case level
                            ((primary total) 0)
                            ((secondary) primary-indent)))
             (right-indent (- indent-level left-indent)))

        (unless (column-uses? 'subtotals-only)
          (gnc:html-table-append-row/markup!
           table subheading-style
           (append
            (gnc:html-make-empty-cells left-indent)
            (if export?
                (cons
                 (gnc:make-html-table-cell data)
                 (gnc:html-make-empty-cells
                  (+ right-indent width-left-columns -1)))
                (list
                 (gnc:make-html-table-cell/size
                  1 (+ right-indent width-left-columns) data)))
            (map
             (lambda (cell)
               (match (vector-ref cell 5)
                 (#f #f)
                 ('bal-bf
                  (let* ((acc (xaccSplitGetAccount split))
                         (bal (xaccAccountGetBalanceAsOfDate acc begindate)))
                    (and (memq sortkey ACCOUNT-SORTING-TYPES)
                         (gnc:make-html-table-cell/markup
                          "number-cell"
                          (gnc:make-gnc-monetary
                           (xaccAccountGetCommodity acc)
                           (if (acc-reverse? acc) (- bal) bal))))))
                 (fn
                  (and (opt-val pagename-sorting optname-show-informal-headers)
                       (column-uses? 'amount-double)
                       (memq sortkey SORTKEY-INFORMAL-HEADERS)
                       (gnc:make-html-text
                        (gnc:html-markup-b
                         (fn (xaccSplitGetAccount split))))))))
             calculated-cells))))))

    ;; check first calculated-cell vector's 7th cell. originally these
    ;; had only 6 cells. backward-compatible upgrade. useful for the
    ;; next function, add-subtotal-row.
    (define first-column-merge?
      (let ((first-cell (and (pair? calculated-cells) (car calculated-cells))))
        (and first-cell
             (<= 7 (vector-length first-cell))
             (vector-ref first-cell 6))))

    (define (add-subtotal-row subtotal-string subtotal-collectors
                              subtotal-style level row col)
      (let* ((left-indent (case level
                            ((total) 0)
                            ((primary) primary-indent)
                            ((secondary) (+ primary-indent secondary-indent))))
             (right-indent (- indent-level left-indent))
             (merge-list (map (lambda (cell) (vector-ref cell 4)) calculated-cells))
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
          (if export?
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

    (define (total-string str) (string-append (G_ "Total For ") str))

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
                (display (xaccAccountGetCode account))
                (display " "))
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
      (let* ((account ((keylist-get-info (sortkey-list BOOK-SPLIT-ACTION)
                                         sortkey 'renderer-fn) split))
             (name (account-namestring account
                                       (column-uses? 'sort-account-code)
                                       #t
                                       (column-uses? 'sort-account-full-name)))
             (description (if (and (column-uses? 'sort-account-description)
                                   (not (string-null?
                                         (xaccAccountGetDescription account))))
                              (string-append ": " (xaccAccountGetDescription account))
                              "")))
        (if (and anchor? opt-use-links?
                 (pair? account)) ;html anchor for 2-split transactions only
            (gnc:make-html-text
             (gnc:html-markup-anchor (gnc:account-anchor-text account) name)
             description)
            name)))

    ;; generic renderer. retrieve renderer-fn which should return a str
    (define (render-generic sortkey split)
      ((keylist-get-info (sortkey-list BOOK-SPLIT-ACTION) sortkey 'renderer-fn) split))

    (define (render-summary split level anchor?)
      (let ((sortkey (opt-val pagename-sorting
                              (case level
                                ((primary) optname-prime-sortkey)
                                ((secondary) optname-sec-sortkey))))
            (date-subtotal-key (opt-val pagename-sorting
                                        (case level
                                          ((primary) optname-prime-date-subtotal)
                                          ((secondary) optname-sec-date-subtotal)))))
        (cond
         ((memq sortkey DATE-SORTING-TYPES)
          (render-date date-subtotal-key split))
         ((memq sortkey ACCOUNT-SORTING-TYPES)
          (render-account sortkey split anchor?))
         (else
          (render-generic sortkey split)))))

    (define (render-grand-total)
      (G_ "Grand Total"))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-split-row
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (add-split-row split cell-calculators row-style transaction-row?)
      (let* ((account (xaccSplitGetAccount split))
             (reversible-account? (acc-reverse? account)))

        (unless (column-uses? 'subtotals-only)
          (gnc:html-table-append-row/markup!
           table row-style
           (append
            (gnc:html-make-empty-cells indent-level)
            (map (lambda (left-col)
                   ((vector-ref left-col 1)
                    split transaction-row?))
                 left-columns)
            (map (lambda (cell)
                   (let* ((cell-monetary ((vector-ref cell 1) split))
                          (reverse? (and (vector-ref cell 2) reversible-account?))
                          (cell-content (and cell-monetary
                                             (if reverse?
                                                 (gnc:monetary-neg cell-monetary)
                                                 cell-monetary))))
                     (and cell-content
                          (gnc:make-html-table-cell/markup
                           "number-cell"
                           (if opt-use-links?
                               (gnc:html-split-anchor split cell-content)
                               cell-content)))))
                 cell-calculators))))

        (map (lambda (cell) (and (vector-ref cell 3) ((vector-ref cell 1) split)))
             cell-calculators)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; do-rows-with-subtotals

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define primary-subtotal-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    (define secondary-subtotal-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    (define total-collectors
      (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells))

    (define grid (make-grid))
    (define primary-subtotal-comparator (primary-get-info 'split-sortvalue))
    (define secondary-subtotal-comparator (secondary-get-info 'split-sortvalue))

    (gnc:html-table-set-col-headers!
     table (concatenate (list
                         (gnc:html-make-empty-cells indent-level)
                         headings-left-columns
                         headings-right-columns)))

    (when (primary-get-info 'renderer-fn)
      (add-subheading (render-summary (car splits) 'primary #t)
                      def:primary-subtotal-style (car splits) 'primary))

    (when (secondary-get-info 'renderer-fn)
      (add-subheading (render-summary (car splits) 'secondary #t)
                      def:secondary-subtotal-style (car splits) 'secondary))

    (let loop ((splits splits)
               (odd-row? #t)
               (work-done 0))

      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))

      (if (null? splits)

          (when (opt-val gnc:pagename-display "Totals")
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
                 (next (and (pair? rest) (car rest)))
                 (split-values (add-split-row
                                current
                                calculated-cells
                                (if (or odd-row? is-multiline?)
                                    def:normal-row-style
                                    def:alternate-row-style)
                                #t)))

            (when is-multiline?
              (for-each
               (lambda (othersplit)
                 (add-split-row othersplit calculated-cells
                                def:alternate-row-style #f))
               (delete current (xaccTransGetSplitList
                                (xaccSplitGetParent current)))))

            (for-each
             (lambda (prime-collector sec-collector tot-collector value)
               (when (gnc:gnc-monetary? value)
                 (let ((comm (gnc:gnc-monetary-commodity value))
                       (val (gnc:gnc-monetary-amount value)))
                 (prime-collector 'add comm val)
                 (sec-collector 'add comm val)
                 (tot-collector 'add comm val))))
             primary-subtotal-collectors
             secondary-subtotal-collectors
             total-collectors
             split-values)

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
                                  'secondary))))

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
                                  def:secondary-subtotal-style next 'secondary)))))

            (loop rest (not odd-row?) (1+ work-done)))))

    (let ((csvlist (cond
                    ((any (lambda (cell) (vector-ref cell 4)) calculated-cells)
                     ;; there are mergeable cells. don't return a list.
                     (N_ "CSV disabled for double column amounts"))

                    (else
                     (map
                      (lambda (cell coll)
                        (cons (vector-ref cell 0)
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
(define (grid-del grid row col)
  ;; grid filter - del all row/col - if #f then delete whole row/col
  (filter
   (lambda (cell)
     (not (cell-match? cell row col)))
   grid))
(define (grid-rows grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 0)) grid)))
(define (grid-cols grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 1)) grid)))
(define (grid-add grid row col data)
  ;;misonomer - we don't 'add' to existing data, we delete old data
  ;;stored at row/col and add again. this is fine because the grid
  ;;should never have duplicate data in the trep.
  (set! grid (grid-del grid row col))
  (set! grid (cons (vector row col data) grid))
  grid)
(define (grid->html-table grid list-of-rows list-of-cols)
  (define row-average-enabled? (> (length list-of-cols) 1))
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
          export-type filename)
  ;; the trep-renderer is a define* function which, at minimum, takes
  ;; the report object
  ;;
  ;; the optional arguments are:
  ;; #:custom-calculated-cells - a list of vectors to define customized data columns
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
    (gnc:option-value (gnc:lookup-option options section name)))
  (define BOOK-SPLIT-ACTION
    (qof-book-use-split-action-for-num-field (gnc-get-current-book)))
  (define (is-filter-member split account-list)
    (define (same-split? s) (equal? s split))
    (define (from-account? s) (member (xaccSplitGetAccount s) account-list))
    (let lp ((splits (xaccTransGetSplitList (xaccSplitGetParent split))))
      (match splits
        (() #f)
        (((? same-split?) . rest) (lp rest))
        (((? from-account?) . _) #t)
        ((_ . rest) (lp rest)))))

  (when filename
    (issue-deprecation-warning "trep-renderer filename is obsolete, and not \
supported for exports. please set html-document export-string instead. this \
warning will be removed in GnuCash 5.0"))

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
         (primary-key (opt-val pagename-sorting optname-prime-sortkey))
         (primary-order (opt-val pagename-sorting optname-prime-sortorder))
         (primary-date-subtotal (opt-val pagename-sorting optname-prime-date-subtotal))
         (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
         (secondary-order (opt-val pagename-sorting optname-sec-sortorder))
         (secondary-date-subtotal (opt-val pagename-sorting optname-sec-date-subtotal))
         (closing-match (keylist-get-info
                         show-closing-list
                         (opt-val pagename-filter optname-closing-transactions)
                         'closing-match))
         (splits '())
         (custom-sort? (or (and (memq primary-key DATE-SORTING-TYPES)
                                (not (eq? primary-date-subtotal 'none)))
                           (and (memq secondary-key DATE-SORTING-TYPES)
                                (not (eq? secondary-date-subtotal 'none)))
                           (or (CUSTOM-SORTING? primary-key BOOK-SPLIT-ACTION)
                               (CUSTOM-SORTING? secondary-key BOOK-SPLIT-ACTION))))
         (subtotal-table? (and (opt-val gnc:pagename-display optname-grid)
                               (if (memq primary-key DATE-SORTING-TYPES)
                                   (keylist-get-info date-subtotal-list
                                                     primary-date-subtotal 'renderer-fn)
                                   (opt-val pagename-sorting optname-prime-subtotal))
                               (memq (opt-val gnc:pagename-display (N_ "Amount"))
                                     '(single double))))
         (infobox-display (opt-val gnc:pagename-general optname-infobox-display))
         (query (qof-query-create-for-splits)))

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
                               (sortkey-list BOOK-SPLIT-ACTION)
                               sortkey 'split-sortvalue))
                        (date-comparator
                         (keylist-get-info date-subtotal-list
                                           date-subtotal-key 'date-sortvalue)))
                    (lambda (s)
                      (and date-comparator (date-comparator (date s)))))
                  (or (keylist-get-info (sortkey-list BOOK-SPLIT-ACTION)
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
         (keylist-get-info (sortkey-list BOOK-SPLIT-ACTION) primary-key 'sortkey)
         (keylist-get-info (sortkey-list BOOK-SPLIT-ACTION) secondary-key 'sortkey)
         '())
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
                      (make-split-table splits options custom-calculated-cells
                                        begindate enddate c_account_1)))

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
            (let* ((generic<?
                    (lambda (a b)
                      (cond ((string? (car a)) (gnc:string-locale<? (car a) (car b)))
                            ((number? (car a)) (< (car a) (car b)))
                            (else (gnc:error "unknown sortvalue")))))
                   (list-of-rows
                    (stable-sort! (delete 'row-total (grid-rows grid))
                                  generic<?))
                   (list-of-cols
                    (stable-sort! (delete 'col-total (grid-cols grid))
                                  generic<?)))
              (gnc:html-document-add-object!
               document (grid->html-table grid list-of-rows list-of-cols))))

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
