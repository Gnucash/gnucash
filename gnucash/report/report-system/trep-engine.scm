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

(use-modules (gnucash gettext))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

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
(define optname-table-export (N_ "Table for Exporting"))
(define optname-common-currency (N_ "Common Currency"))
(define optname-orig-currency (N_ "Show original currency amount"))
(define optname-currency (N_ "Report's currency"))
(define optname-infobox-display (N_ "Add options summary"))

;;Filtering
(define pagename-filter (N_ "Filter"))
(define optname-account-matcher (N_ "Account Name Filter"))
(define optname-account-matcher-regex
  (N_ "Use regular expressions for account name filter"))
(define optname-transaction-matcher (N_ "Transaction Filter"))
(define optname-transaction-matcher-regex
  (N_ "Use regular expressions for transaction filter"))
(define optname-transaction-matcher-exclude
  (N_ "Transaction Filter excludes matched strings"))
(define optname-reconcile-status (N_ "Reconcile Status"))
(define optname-void-transactions (N_ "Void Transactions"))
(define optname-closing-transactions (N_ "Closing transactions"))

;;Styles
(define def:grand-total-style "grand-total")
(define def:normal-row-style "normal-row")
(define def:alternate-row-style "alternate-row")
(define def:primary-subtotal-style "primary-subheading")
(define def:secondary-subtotal-style "secondary-subheading")

(define NO-MATCHING-TRANS-HEADER (_ "No matching transactions found"))
(define NO-MATCHING-TRANS-TEXT (_ "No transactions were found that \
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
  (list (cons #\n (_ "Unreconciled"))
        (cons #\c (_ "Cleared"))
        (cons #\y (_ "Reconciled"))
        (cons #\f (_ "Frozen"))
        (cons #\v (_ "Voided"))))

(define (sortkey-list split-action?)
  ;; Defines the different sorting keys, as an association-list
  ;; together with the subtotal functions. Each entry:
  ;;  'sortkey             - sort parameter sent via qof-query
  ;;  'split-sortvalue     - function retrieves number/string for comparing splits
  ;;  'text                - text displayed in Display tab
  ;;  'tip                 - tooltip displayed in Display tab
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
              (cons 'text (_ "Account Name"))
              (cons 'tip (_ "Sort & subtotal by account name."))
              (cons 'renderer-fn xaccSplitGetAccount))

        (list 'account-code
              (cons 'sortkey (list SPLIT-ACCOUNT ACCOUNT-CODE-))
              (cons 'split-sortvalue (compose xaccAccountGetCode xaccSplitGetAccount))
              (cons 'text (_ "Account Code"))
              (cons 'tip (_ "Sort & subtotal by account code."))
              (cons 'renderer-fn xaccSplitGetAccount))

        (list 'date
              (cons 'sortkey (list SPLIT-TRANS TRANS-DATE-POSTED))
              (cons 'split-sortvalue (compose xaccTransGetDate xaccSplitGetParent))
              (cons 'text (_ "Date"))
              (cons 'tip (_ "Sort by date."))
              (cons 'renderer-fn #f))

        (list 'reconciled-date
              (cons 'sortkey (list SPLIT-DATE-RECONCILED))
              (cons 'split-sortvalue xaccSplitGetDateReconciled)
              (cons 'text (_ "Reconciled Date"))
              (cons 'tip (_ "Sort by the Reconciled Date."))
              (cons 'renderer-fn #f))

        (list 'reconciled-status
              (cons 'sortkey #f)
              (cons 'split-sortvalue (lambda (s)
                                       (length (memv (xaccSplitGetReconcile s)
                                                     (map car reconcile-list)))))
              (cons 'text (_ "Reconciled Status"))
              (cons 'tip (_ "Sort by the Reconciled Status"))
              (cons 'renderer-fn (lambda (s)
                                   (assv-ref reconcile-list
                                             (xaccSplitGetReconcile s)))))

        (list 'register-order
              (cons 'sortkey (list QUERY-DEFAULT-SORT))
              (cons 'split-sortvalue #f)
              (cons 'text (_ "Register Order"))
              (cons 'tip (_ "Sort as in the register."))
              (cons 'renderer-fn #f))

        (list 'corresponding-acc-name
              (cons 'sortkey (list SPLIT-CORR-ACCT-NAME))
              (cons 'split-sortvalue xaccSplitGetCorrAccountFullName)
              (cons 'text (_ "Other Account Name"))
              (cons 'tip (_ "Sort by account transferred from/to's name."))
              (cons 'renderer-fn (compose xaccSplitGetAccount xaccSplitGetOtherSplit)))

        (list 'corresponding-acc-code
              (cons 'sortkey (list SPLIT-CORR-ACCT-CODE))
              (cons 'split-sortvalue xaccSplitGetCorrAccountCode)
              (cons 'text (_ "Other Account Code"))
              (cons 'tip (_ "Sort by account transferred from/to's code."))
              (cons 'renderer-fn (compose xaccSplitGetAccount xaccSplitGetOtherSplit)))

        (list 'amount
              (cons 'sortkey (list SPLIT-VALUE))
              (cons 'split-sortvalue xaccSplitGetValue)
              (cons 'text (_ "Amount"))
              (cons 'tip (_ "Sort by amount."))
              (cons 'renderer-fn #f))

        (list 'description
              (cons 'sortkey (list SPLIT-TRANS TRANS-DESCRIPTION))
              (cons 'split-sortvalue (compose xaccTransGetDescription
                                              xaccSplitGetParent))
              (cons 'text (_ "Description"))
              (cons 'tip (_ "Sort by description."))
              (cons 'renderer-fn (compose xaccTransGetDescription xaccSplitGetParent)))

        (if split-action?
            (list 'number
                  (cons 'sortkey (list SPLIT-ACTION))
                  (cons 'split-sortvalue xaccSplitGetAction)
                  (cons 'text (_ "Number/Action"))
                  (cons 'tip (_ "Sort by check number/action."))
                  (cons 'renderer-fn #f))

            (list 'number
                  (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
                  (cons 'split-sortvalue (compose xaccTransGetNum xaccSplitGetParent))
                  (cons 'text (_ "Number"))
                  (cons 'tip (_ "Sort by check/transaction number."))
                  (cons 'renderer-fn #f)))

        (list 't-number
              (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
              (cons 'split-sortvalue (compose xaccTransGetNum xaccSplitGetParent))
              (cons 'text (_ "Transaction Number"))
              (cons 'tip (_ "Sort by transaction number."))
              (cons 'renderer-fn #f))

        (list 'memo
              (cons 'sortkey (list SPLIT-MEMO))
              (cons 'split-sortvalue xaccSplitGetMemo)
              (cons 'text (_ "Memo"))
              (cons 'tip (_ "Sort by memo."))
              (cons 'renderer-fn xaccSplitGetMemo))

        (list 'notes
              (cons 'sortkey #f)
              (cons 'split-sortvalue (compose xaccTransGetNotes xaccSplitGetParent))
              (cons 'text (_ "Notes"))
              (cons 'tip (_ "Sort by transaction notes."))
              (cons 'renderer-fn (compose xaccTransGetNotes xaccSplitGetParent)))

        (list 'none
              (cons 'sortkey '())
              (cons 'split-sortvalue #f)
              (cons 'text (_ "None"))
              (cons 'tip (_ "Do not sort."))
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
  ;;  'tip                 - tooltip displayed in Display tab
  ;;  'renderer-fn         - func retrieves string for subtotal/subheading renderer
  ;;         #f means the date sortkey is not grouped
  ;;         otherwise it converts split->string
  (list
   (list 'none
         (cons 'split-sortvalue #f)
         (cons 'date-sortvalue #f)
         (cons 'text (_ "None"))
         (cons 'tip (_ "None."))
         (cons 'renderer-fn #f))

   (list 'daily
         (cons 'split-sortvalue (lambda (s) (time64-day (split->time64 s))))
         (cons 'date-sortvalue time64-day)
         (cons 'text (_ "Daily"))
         (cons 'tip (_ "Daily."))
         (cons 'renderer-fn (lambda (s) (qof-print-date (split->time64 s)))))

   (list 'weekly
         (cons 'split-sortvalue (lambda (s) (time64-week (split->time64 s))))
         (cons 'date-sortvalue time64-week)
         (cons 'text (_ "Weekly"))
         (cons 'tip (_ "Weekly."))
         (cons 'renderer-fn (compose gnc:date-get-week-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'monthly
         (cons 'split-sortvalue (lambda (s) (time64-month (split->time64 s))))
         (cons 'date-sortvalue time64-month)
         (cons 'text (_ "Monthly"))
         (cons 'tip (_ "Monthly."))
         (cons 'renderer-fn (compose gnc:date-get-month-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'quarterly
         (cons 'split-sortvalue (lambda (s) (time64-quarter (split->time64 s))))
         (cons 'date-sortvalue time64-quarter)
         (cons 'text (_ "Quarterly"))
         (cons 'tip (_ "Quarterly."))
         (cons 'renderer-fn (compose gnc:date-get-quarter-year-string
                                     gnc-localtime
                                     split->time64)))

   (list 'yearly
         (cons 'split-sortvalue (lambda (s) (time64-year (split->time64 s))))
         (cons 'date-sortvalue time64-year)
         (cons 'text (_ "Yearly"))
         (cons 'tip (_ "Yearly."))
         (cons 'renderer-fn (compose gnc:date-get-year-string
                                     gnc-localtime
                                     split->time64)))))

(define filter-list
  (list
   (list 'none
         (cons 'text (_ "None"))
         (cons 'tip (_ "Do not do any filtering.")))

   (list 'include
         (cons 'text (_ "Include Transactions to/from Filter Accounts"))
         (cons 'tip (_ "Include transactions to/from filter accounts only.")))

   (list 'exclude
         (cons 'text (_ "Exclude Transactions to/from Filter Accounts"))
         (cons 'tip (_ "Exclude transactions to/from all filter accounts.")))))

(define show-void-list
  (list
   (list 'non-void-only
         (cons 'text (_ "Non-void only"))
         (cons 'tip (_ "Show only non-voided transactions.")))

   (list 'void-only
         (cons 'text (_ "Void only"))
         (cons 'tip (_ "Show only voided transactions.")))

   (list 'both
         (cons 'text (_ "Both"))
         (cons 'tip (_ "Show both (and include void transactions in totals).")))))

(define show-closing-list
  (list
   (list 'exclude-closing
         (cons 'text (_ "Exclude closing transactions"))
         (cons 'tip (_ "Exclude closing transactions from report."))
         (cons 'closing-match #f))

   (list 'include-both
         (cons 'text (_ "Show both closing and regular transactions"))
         (cons 'tip (_ "Show both (and include closing transactions in totals)."))
         (cons 'closing-match 'both))

   (list 'closing-only
         (cons 'text (_ "Show closing transactions only"))
         (cons 'tip (_ "Show only closing transactions."))
         (cons 'closing-match #t))))

(define reconcile-status-list
  ;; 'filter-types must be either #f (i.e. disable reconcile filter)
  ;; or a value defined as defined in Query.c
  ;; e.g. CLEARED-NO for unreconciled
  ;;      (logior CLEARED-NO CLEARED-CLEARED) for unreconciled & cleared
  (list
   (list 'all
         (cons 'text (_ "All"))
         (cons 'tip (_ "Show All Transactions"))
         (cons 'filter-types #f))

   (list 'unreconciled
         (cons 'text (_ "Unreconciled"))
         (cons 'tip (_ "Unreconciled only"))
         (cons 'filter-types CLEARED-NO))

   (list 'cleared
         (cons 'text (_ "Cleared"))
         (cons 'tip (_ "Cleared only"))
         (cons 'filter-types CLEARED-CLEARED))

   (list 'reconciled
         (cons 'text (_ "Reconciled"))
         (cons 'tip (_ "Reconciled only"))
         (cons 'filter-types CLEARED-RECONCILED))))


(define ascending-list
  (list
   (list 'ascend
         (cons 'text (_ "Ascending"))
         (cons 'tip (_ "Smallest to largest, earliest to latest.")))
   (list 'descend
         (cons 'text (_ "Descending"))
         (cons 'tip (_ "Largest to smallest, latest to earliest.")))))

(define sign-reverse-list
  (list
   (list 'global
         (cons 'text (_ "Use Global Preference"))
         (cons 'tip (_ "Use reversing option specified in global preference."))
         (cons 'acct-types #f))
   (list 'none
         (cons 'text (_ "None"))
         (cons 'tip (_ "Don't change any displayed amounts."))
         (cons 'acct-types '()))
   (list 'income-expense
         (cons 'text (_ "Income and Expense"))
         (cons 'tip (_ "Reverse amount display for Income and Expense Accounts."))
         (cons 'acct-types (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)))
   (list 'credit-accounts
         (cons 'text (_ "Credit Accounts"))
         (cons 'tip (_ "Reverse amount display for Liability, Payable, Equity, \
Credit Card, and Income accounts."))
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
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
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

  (define max-items (apply max (map length lst)))

  (define (strify obj)
    (cond
     ((not obj) "")
     ((string? obj) (string-sanitize-csv obj))
     ((number? obj) (number->string (exact->inexact obj)))
     ((list? obj) (string-join
                   (map strify
                        (append obj
                                (make-list (- max-items (length obj)) #f)))
                   ","))
     ((gnc:gnc-monetary? obj) (strify (gnc:gnc-monetary-amount obj)))
     (else (object->string obj))))

  (string-join (map strify lst) "\n"))


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
   (gnc:make-complex-boolean-option
    gnc:pagename-general optname-common-currency
    "e" (_ "Convert all transactions into a common currency.") #f
    #f
    (lambda (x)
      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-general optname-currency x)
      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-general optname-orig-currency x))))

  (gnc:options-add-currency!
   options gnc:pagename-general optname-currency "f")

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-orig-currency
    "f1" (_ "Also show original currency amounts") #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-table-export
    "g" (_ "Formats the table suitable for cut & paste exporting with extra cells.")
    #f))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    gnc:pagename-general optname-infobox-display
    "h" (_ "Add summary of options.")
    'no-match
    ;; This is an alist of conditions for displaying the infobox
    ;; 'no-match for empty-report
    ;; 'match for generated report
    (list (vector 'no-match
                  (_ "If no transactions matched")
                  (_ "Display summary if no transactions were matched."))
          (vector 'always
                  (_ "Always")
                  (_ "Always display summary."))
          (vector 'never
                  (_ "Never")
                  (_ "Disable report summary.")))))

  ;; Filtering Options

  (gnc:register-trep-option
   (gnc:make-string-option
    pagename-filter optname-account-matcher
    "a5" (_ "Show only accounts whose full name matches this filter e.g. ':Travel' will match \
Expenses:Travel:Holiday and Expenses:Business:Travel. It can be left blank, which will \
disable the filter.")
    ""))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-account-matcher-regex
    "a6"
    (_ "By default the account filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. 'Car|Flights' will match both \
Expenses:Car and Expenses:Flights. Use a period (.) to match a single character e.g. \
'20../.' will match 'Travel 2017/1 London'. ")
    #f))

  (gnc:register-trep-option
   (gnc:make-string-option
    pagename-filter optname-transaction-matcher
    "i1" (_ "Show only transactions where description, notes, or memo matches this filter.
e.g. '#gift' will find all transactions with #gift in description, notes or memo. It can be left \
blank, which will disable the filter.")
    ""))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-transaction-matcher-regex
    "i2"
    (_ "By default the transaction filter will search substring only. Set this to true to \
enable full POSIX regular expressions capabilities. '#work|#family' will match both \
tags within description, notes or memo. ")
    #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-filter optname-transaction-matcher-exclude
    "i3"
    (_ "If this option is selected, transactions matching filter are excluded.")
    #f))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-filter optname-reconcile-status
    "j1" (_ "Filter by reconcile status.")
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
    "l" (_ "By default most users should not include closing \
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
    "a" (_ "Report on these accounts.")
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
    "c1" (_ "Filter on these accounts.")
    (lambda ()
      '())
    #f #t))

  (gnc:register-trep-option
   (gnc:make-multichoice-callback-option
    gnc:pagename-accounts optname-filtertype
    "c" (_ "Filter account.")
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
      "a" (_ "Sort by this criterion first.")
      prime-sortkey
      key-choice-list #f
      (lambda (x)
        (set! prime-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-full-account-name
      "j1"
      (_ "Show the full account name for subtotals and subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-account-code
      "j2"
      (_ "Show the account code for subtotals and subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-account-description
      "j3"
      (_ "Show the account description for subheadings?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-informal-headers
      "j4"
      (_ "Show the informal headers for debit/credit accounts?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-indenting
      "j5"
      (_ "Add indenting columns with grouping and subtotals?")
      #t))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-subtotals-only
      "j6"
      (_ "Show subtotals only, hiding transactional detail?")
      #f))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-prime-subtotal
      "e5"
      (_ "Subtotal according to the primary key?")
      prime-sortkey-subtotal-true #f
      (lambda (x)
        (set! prime-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-prime-date-subtotal
      "e2" (_ "Do a date subtotal.")
      prime-date-subtotal
      date-subtotal-choice-list #f
      (lambda (x)
        (set! prime-date-subtotal x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-sortorder
      "e" (_ "Order of primary sorting.")
      'ascend
      ascending-choice-list))

    ;; Secondary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-sortkey
      "f"
      (_ "Sort by this criterion second.")
      sec-sortkey
      key-choice-list #f
      (lambda (x)
        (set! sec-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-sec-subtotal
      "i5"
      (_ "Subtotal according to the secondary key?")
      sec-sortkey-subtotal-true #f
      (lambda (x)
        (set! sec-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-date-subtotal
      "i2" (_ "Do a date subtotal.")
      sec-date-subtotal
      date-subtotal-choice-list #f
      (lambda (x)
        (set! sec-date-subtotal x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-sortorder
      "i" (_ "Order of Secondary sorting.")
      'ascend
      ascending-choice-list)))

  ;; Display options

  (let ((disp-memo? #t)
        (disp-accname? #t)
        (disp-other-accname? #f)
        (detail-is-single? #t)
        (amount-value 'single))

    (define (apply-selectable-by-name-display-options)
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
       (eq? amount-value 'single))

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display "Enable links"
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
      (list (N_ "Date")                         "a"  (_ "Display the date?") #t)
      (list (N_ "Reconciled Date")              "a2" (_ "Display the reconciled date?") #f)
      (if BOOK-SPLIT-ACTION
          (list (N_ "Num/Action")               "b"  (_ "Display the check number?") #t)
          (list (N_ "Num")                      "b"  (_ "Display the check number?") #t))
      (list (N_ "Description")                  "c"  (_ "Display the description?") #t)
      (list (N_ "Notes")                        "d2" (_ "Display the notes if the memo is unavailable?") #t)
      ;; account name option appears here
      (list (N_ "Use Full Account Name")        "f"  (_ "Display the full account name?") #t)
      (list (N_ "Account Code")                 "g"  (_ "Display the account code?") #f)
      ;; other account name option appears here
      (list (N_ "Use Full Other Account Name")  "i"  (_ "Display the full account name?") #f)
      (list (N_ "Other Account Code")           "j"  (_ "Display the other account code?") #f)
      (list (N_ "Shares")                       "k"  (_ "Display the number of shares?") #f)
      (list (N_ "Price")                        "l"  (_ "Display the shares price?") #f)
      ;; note the "Amount" multichoice option in between here
      (list optname-grid                        "m5" (_ "Display a subtotal summary table. This requires Display/Amount being 'single") #f)
      (list (N_ "Running Balance")              "n"  (_ "Display a running balance?") #f)
      (list (N_ "Totals")                       "o"  (_ "Display the totals?") #t)))

    (when BOOK-SPLIT-ACTION
      (gnc:register-trep-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Trans Number")
        "b2" (_ "Display the trans number?") #f)))

    ;; Add an option to display the memo, and disable the notes option
    ;; when memos are not included.
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Memo")
      "d"  (_ "Display the memo?") #t
      disp-memo?
      (lambda (x)
        (set! disp-memo? x)
        (apply-selectable-by-name-display-options))))

    ;; Ditto for Account Name #t -> Use Full Account Name is selectable
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Account Name")
      "e"  (_ "Display the account name?") #t
      disp-accname?
      (lambda (x)
        (set! disp-accname? x)
        (apply-selectable-by-name-display-options))))

    ;; Ditto for Other Account Name #t -> Use Full Other Account Name is selectable
    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      gnc:pagename-display (N_ "Other Account Name")
      "h5"  (_ "Display the other account name? (if this is a split transaction, this parameter is guessed).") #f
      disp-other-accname?
      (lambda (x)
        (set! disp-other-accname? x)
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-display optname-detail-level
      "h" (_ "Amount of detail to display per transaction.")
      'single
      (list (vector 'multi-line
                    (_ "Multi-Line")
                    (_ "Display all splits in a transaction on a separate line."))
            (vector 'single
                    (_ "Single")
                    (_ "Display one line per transaction, merging multiple splits where required.")))
      #f
      (lambda (x)
        (set! detail-is-single? (eq? x 'single))
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      gnc:pagename-display (N_ "Amount")
      "m" (_ "Display the amount?")
      amount-value
      (list
       (vector 'none   (_ "None") (_ "No amount display."))
       (vector 'single (_ "Single") (_ "Single Column Display."))
       (vector 'double (_ "Double") (_ "Two Column Display.")))
      #f
      (lambda (x)
        (set! amount-value x)
        (apply-selectable-by-name-display-options))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display (N_ "Enable links")
      "m2" (_ "Enable hyperlinks in amounts.") #t))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      gnc:pagename-display (N_ "Sign Reverses")
      "m1" (_ "Reverse amount display for certain account types.")
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
                          begindate)

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
          (cons 'amount-single (eq? amount-setting 'single))
          (cons 'amount-double (eq? amount-setting 'double))
          (cons 'common-currency (opt-val gnc:pagename-general optname-common-currency))
          (cons 'amount-original-currency
                (and (opt-val gnc:pagename-general optname-common-currency)
                     (opt-val gnc:pagename-general optname-orig-currency)))
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
         (opt-use-links? (opt-val gnc:pagename-display "Enable links"))
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
      (cdr (assq param used-columns)))

    (define left-columns
      (let* ((add-if (lambda (pred? . items) (if pred? items '())))
             (left-cols-list
              (append
               (add-if (column-uses? 'date)
                       (vector (_ "Date")
                               (lambda (split transaction-row?)
                                 (and transaction-row?
                                      (gnc:make-html-table-cell/markup
                                       "date-cell"
                                       (qof-print-date
                                        (xaccTransGetDate
                                         (xaccSplitGetParent split))))))))

               (add-if (column-uses? 'reconciled-date)
                       (vector (_ "Reconciled Date")
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
                                   (_ "Num/T-Num")
                                   (_ "Num"))
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
                       (vector (_ "Description")
                               (lambda (split transaction-row?)
                                 (define trans (xaccSplitGetParent split))
                                 (and transaction-row?
                                      (gnc:make-html-table-cell/markup
                                       "text-cell"
                                       (xaccTransGetDescription trans))))))

               (add-if (column-uses? 'memo)
                       (vector (if (column-uses? 'notes)
                                   (string-append (_ "Memo") "/" (_ "Notes"))
                                   (_ "Memo"))
                               (lambda (split transaction-row?)
                                 (define trans (xaccSplitGetParent split))
                                 (define memo (xaccSplitGetMemo split))
                                 (if (and (string-null? memo) (column-uses? 'notes))
                                     (xaccTransGetNotes trans)
                                     memo))))

               (add-if (or (column-uses? 'account-name) (column-uses? 'account-code))
                       (vector (_ "Account")
                               (lambda (split transaction-row?)
                                 (account-namestring
                                  (xaccSplitGetAccount split)
                                  (column-uses? 'account-code)
                                  (column-uses? 'account-name)
                                  (column-uses? 'account-full-name)))))

               (add-if (or (column-uses? 'other-account-name)
                           (column-uses? 'other-account-code))
                       (vector (_ "Transfer from/to")
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
                       (vector (_ "Shares")
                               (lambda (split transaction-row?)
                                 (gnc:make-html-table-cell/markup
                                  "number-cell"
                                  (xaccSplitGetAmount split)))))

               (add-if (column-uses? 'price)
                       (vector (_ "Price")
                               (lambda (split transaction-row?)
                                 ;; share price is retrieved as an
                                 ;; exact rational; convert for
                                 ;; presentation to decimal, rounded
                                 ;; to the currency SCU, optionally
                                 ;; increasing precision by 2
                                 ;; significant digits.
                                 (let* ((currency (xaccTransGetCurrency
                                                   (xaccSplitGetParent split)))
                                        (scu (gnc-commodity-get-fraction currency))
                                        (price (xaccSplitGetSharePrice split))
                                        (price-decimal
                                         (gnc-numeric-convert
                                          price (min 10000 (* 100 scu))
                                          GNC-HOW-RND-ROUND)))
                                   (gnc:make-html-table-cell/markup
                                    "number-cell"
                                    (gnc:make-gnc-monetary
                                     currency price-decimal)))))))))

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
                                         (opt-val gnc:pagename-general optname-currency)
                                         (split-currency s))))
           (friendly-debit (lambda (a) (gnc:get-debit-string (xaccAccountGetType a))))
           (friendly-credit (lambda (a) (gnc:get-credit-string (xaccAccountGetType a))))
           (header-commodity (lambda (str)
                               (string-append
                                str
                                (if (column-uses? 'common-currency)
                                    (format #f " (~a)"
                                            (gnc-commodity-get-mnemonic
                                             (opt-val gnc:pagename-general
                                                      optname-currency)))
                                    ""))))
           ;; For conversion to row-currency. Use midday as the
           ;; transaction time so it matches a price on the same day.
           ;; Otherwise it uses midnight which will likely match a
           ;; price on the previous day
           (converted-amount (lambda (s)
                               (gnc:exchange-by-pricedb-nearest
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

         (if (column-uses? 'amount-single)
             (list (vector (header-commodity (_ "Amount"))
                           converted-amount #t #t #f
                           (lambda (a) "")))
             '())

         (if (column-uses? 'amount-double)
             (list (vector (header-commodity (_ "Debit"))
                           converted-debit-amount #f #t #t
                           friendly-debit)
                   (vector (header-commodity (_ "Credit"))
                           converted-credit-amount #f #t #f
                           friendly-credit))
             '())

         (if (and (column-uses? 'amount-original-currency)
                  (column-uses? 'amount-single))
             (list (vector (_ "Amount")
                           original-amount #t #t #f
                           (lambda (a) "")))
             '())

         (if (and (column-uses? 'amount-original-currency)
                  (column-uses? 'amount-double))
             (list (vector (_ "Debit")
                           original-debit-amount #f #t #t
                           friendly-debit)
                   (vector (_ "Credit")
                           original-credit-amount #f #t #f
                           friendly-credit))
             '())

         (if (column-uses? 'running-balance)
             (list (vector (_ "Running Balance")
                           running-balance #t #f #f
                           'bal-bf))
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
                       (string-append data ": " (_ "Balance b/f"))
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

        ;; take the first column of each commodity, add onto the subtotal grid
        (set! grid
          (grid-add grid row col
                    (map (lambda (commodity)
                           (retrieve-commodity (car columns) commodity))
                         list-of-commodities)))

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

    (define (total-string str) (string-append (_ "Total For ") str))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; renderers
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; display an account name depending on the options the user has set
    (define (account-namestring account show-account-code?
                                show-account-name? show-account-full-name?)
      ;;# on multi-line splits we can get an empty ('()) account
      (if (null? account)
          (_ "Split Transaction")
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
      (_ "Grand Total"))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-split-row
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (add-split-row split cell-calculators row-style transaction-row?)
      (let* ((account (xaccSplitGetAccount split))
             (reversible-account? (acc-reverse? account))
             (cells (map (lambda (cell)
                           (let ((split->monetary (vector-ref cell 1)))
                             (vector (split->monetary split)
                                     (vector-ref cell 2) ;reverse?
                                     (vector-ref cell 3) ;subtotal?
                                     )))
                         cell-calculators)))

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
                   (let* ((cell-monetary (vector-ref cell 0))
                          (reverse? (and (vector-ref cell 1)
                                         reversible-account?))
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
                 cells))))

        (map (lambda (cell)
               (let ((cell-monetary (vector-ref cell 0))
                     (subtotal? (vector-ref cell 2)))
                 (and subtotal? cell-monetary)))
             cells)))

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
            ((eq? row 'row-total) (_ "Grand Total"))
            (else (cdr row))))
     (map (lambda (col) (make-table-cell row col commodity-idx 1))
          list-of-cols)
     (list (make-table-cell row 'col-total commodity-idx 1))
     (if row-average-enabled?
         (list (make-table-cell
                row 'col-total commodity-idx (length list-of-cols)))
         '())))
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-caption! table optname-grid)
    (gnc:html-table-set-col-headers!
     table (append (list "")
                   (map cdr list-of-cols)
                   (list (_ "Total"))
                   (if row-average-enabled? (list (_ "Average")) '())))
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
  ;;     (see reconcile report)
  ;; #:split->date-include-false? - addendum to above, specifies filter behaviour if
  ;;     split->date returns #f. useful to include unreconciled splits in reconcile
  ;;     report. it can be useful for alternative date filtering, e.g. filter by
  ;;     transaction->invoice->payment date.
  ;; #:export-type and #:filename - are provided for CSV export
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

  (gnc:report-starting (opt-val gnc:pagename-general gnc:optname-reportname))

  (let* ((document (gnc:make-html-document))
         (account-matcher (opt-val pagename-filter optname-account-matcher))
         (account-matcher-regexp
          (and (opt-val pagename-filter optname-account-matcher-regex)
               (if (defined? 'make-regexp)
                   (catch 'regular-expression-syntax
                     (lambda () (make-regexp account-matcher))
                     (const 'invalid-account-regex))
                   'no-guile-regex-support)))
         (c_account_0 (or custom-source-accounts
                          (opt-val gnc:pagename-accounts optname-accounts)))
         (c_account_1 (filter
                       (lambda (acc)
                         (if (regexp? account-matcher-regexp)
                             (regexp-exec account-matcher-regexp
                                          (gnc-account-get-full-name acc))
                             (string-contains (gnc-account-get-full-name acc)
                                              account-matcher)))
                       c_account_0))
         (c_account_2 (opt-val gnc:pagename-accounts optname-filterby))
         (filter-mode (opt-val gnc:pagename-accounts optname-filtertype))
         (begindate (gnc:time64-start-day-time
                     (gnc:date-option-absolute-time
                      (opt-val gnc:pagename-general optname-startdate))))
         (enddate (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (opt-val gnc:pagename-general optname-enddate))))
         (transaction-matcher (opt-val pagename-filter optname-transaction-matcher))
         (transaction-matcher-regexp
          (and (opt-val pagename-filter optname-transaction-matcher-regex)
               (if (defined? 'make-regexp)
                   (catch 'regular-expression-syntax
                     (lambda () (make-regexp transaction-matcher))
                     (const 'invalid-transaction-regex))
                   'no-guile-regex-support)))
         (transaction-filter-exclude?
          (opt-val pagename-filter optname-transaction-matcher-exclude))
         (reconcile-status-filter
          (keylist-get-info reconcile-status-list
                            (opt-val pagename-filter optname-reconcile-status)
                            'filter-types))
         (report-title (opt-val gnc:pagename-general gnc:optname-reportname))
         (primary-key (opt-val pagename-sorting optname-prime-sortkey))
         (primary-order (opt-val pagename-sorting optname-prime-sortorder))
         (primary-date-subtotal (opt-val pagename-sorting optname-prime-date-subtotal))
         (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
         (secondary-order (opt-val pagename-sorting optname-sec-sortorder))
         (secondary-date-subtotal (opt-val pagename-sorting optname-sec-date-subtotal))
         (void-status (opt-val pagename-filter optname-void-transactions))
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
                               (eq? (opt-val gnc:pagename-display (N_ "Amount"))
                                    'single)))
         (infobox-display (opt-val gnc:pagename-general optname-infobox-display))
         (match? (lambda (str)
                   (if transaction-matcher-regexp
                       (regexp-exec transaction-matcher-regexp str)
                       (string-contains str transaction-matcher))))
         (query (qof-query-create-for-splits)))

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
                     (if ascend? string<? string>?)
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
          (string-append (_ "Error") " " (symbol->string account-matcher-regexp))
          ""))

        ((symbol? transaction-matcher-regexp)
         (gnc:html-make-generic-warning
          report-title (gnc:report-id report-obj)
          (string-append (_ "Error") " " (symbol->string transaction-matcher-regexp))
          ""))))

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
      (unless split->date
        (xaccQueryAddDateMatchTT query #t begindate #t enddate QOF-QUERY-AND))
      (case void-status
        ((non-void-only)
         (gnc:query-set-match-non-voids-only! query (gnc-get-current-book)))
        ((void-only)
         (gnc:query-set-match-voids-only! query (gnc-get-current-book)))
        (else #f))
      (when reconcile-status-filter
        (xaccQueryAddClearedMatch query reconcile-status-filter QOF-QUERY-AND))
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
             (and (or (not split->date)
                      (let ((date (split->date split)))
                        (if date
                            (<= begindate date enddate)
                            split->date-include-false?)))
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

        (when (memq infobox-display '(always no-match))
          (gnc:html-document-add-object!
           document
           (gnc:html-render-options-changed options))))

       (else
        (let-values (((table grid csvlist)
                      (make-split-table splits options custom-calculated-cells
                                        begindate)))

          (gnc:html-document-set-title! document report-title)

          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-h3
             (format #f
                     ;; Translators: Both ~a's are dates
                     (_ "From ~a to ~a")
                     (qof-print-date begindate)
                     (qof-print-date enddate)))))

          (when (eq? infobox-display 'always)
            (gnc:html-document-add-object!
             document
             (gnc:html-render-options-changed options)))

          (when subtotal-table?
            (let* ((generic<?
                    (lambda (a b)
                      (cond ((string? (car a)) (string<? (car a) (car b)))
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

          (cond
           ((and (eq? export-type 'csv)
                 (string? filename)
                 (not (string-null? filename)))
            (let ((old-date-fmt (qof-date-format-get))
                  (dummy (qof-date-format-set QOF-DATE-FORMAT-ISO))
                  (infolist
                   (list
                    (list "from" (qof-print-date begindate))
                    (list "to" (qof-print-date enddate)))))
              (qof-date-format-set old-date-fmt)
              (if (list? csvlist)
                  (catch #t
                    (lambda ()
                      (call-with-output-file filename
                        (lambda (p)
                          (display (lists->csv (append infolist csvlist)) p))))
                    (lambda (key . args)
                      ;; Translators: ~a error type, ~a filename, ~s error details
                      (let ((fmt (N_ "error ~a during csv output to ~a: ~s")))
                        (gnc:gui-error (format #f fmt key filename args)
                                       (format #f (_ fmt) key filename args)))))
                  (gnc:gui-error csvlist (_ csvlist))))))

          (unless (and subtotal-table?
                       (opt-val pagename-sorting optname-show-subtotals-only))
            (gnc:html-document-add-object! document table)))))))

    (gnc:report-finished)

    document))
