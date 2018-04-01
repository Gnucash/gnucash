;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transaction-report.scm : Report on all transactions in account(s)
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
;; - add defaults suitable for a reconciliation report
;; - add subtotal summary grid
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

(define-module (gnucash report standard-reports transaction))

(use-modules (gnucash utilities)) 
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-13))
(use-modules (ice-9 regex))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

;; Define the strings here to avoid typos and make changes easier.
(define reportname (N_ "Transaction Report"))

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
(define optname-show-subtotals-only (N_ "Show subtotals only (hide transactional data)"))
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
(define optname-account-matcher-regex (N_ "Use regular expressions for account name filter"))
(define optname-transaction-matcher (N_ "Transaction Filter"))
(define optname-transaction-matcher-regex (N_ "Use regular expressions for transaction filter"))
(define optname-reconcile-status (N_ "Reconcile Status"))
(define optname-void-transactions (N_ "Void Transactions"))

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

(define DATE-SORTING-TYPES (list 'date 'reconciled-date))

;; The option-values of the sorting key multichoice option, for
;; which a subtotal should be enabled.
(define SUBTOTAL-ENABLED (list 'account-name 'corresponding-acc-name
                               'account-code 'corresponding-acc-code
                               'reconciled-status))

(define ACCOUNT-SORTING-TYPES (list 'account-name 'corresponding-acc-name
                                    'account-code 'corresponding-acc-code))
(define CUSTOM-SORTING (list 'reconciled-status))

(define SORTKEY-INFORMAL-HEADERS (list 'account-name 'account-code))

(define sortkey-list
  ;;
  ;; Defines the different sorting keys, as an association-list
  ;; together with the subtotal functions. Each entry:
  ;;  'sortkey             - sort parameter sent via qof-query
  ;;  'split-sortvalue     - function which retrieves number/string used for comparing splits
  ;;  'text                - text displayed in Display tab
  ;;  'tip                 - tooltip displayed in Display tab
  ;;  'renderer-fn         - helper function to select subtotal/subheading renderer
  ;;       behaviour varies according to sortkey.
  ;;       account-types converts split->account
  ;;       #f means the sortkey cannot be subtotalled
  ;;       otherwise it converts split->string
  ;;
  (list (cons 'account-name  (list (cons 'sortkey (list SPLIT-ACCT-FULLNAME))
                                   (cons 'split-sortvalue (lambda (a) (gnc-account-get-full-name (xaccSplitGetAccount a))))
                                   (cons 'text (_ "Account Name"))
                                   (cons 'tip (_ "Sort & subtotal by account name."))
                                   (cons 'renderer-fn (lambda (a) (xaccSplitGetAccount a)))))

        (cons 'account-code (list (cons 'sortkey (list SPLIT-ACCOUNT ACCOUNT-CODE-))
                                  (cons 'split-sortvalue (lambda (a) (xaccAccountGetCode (xaccSplitGetAccount a))))
                                  (cons 'text (_ "Account Code"))
                                  (cons 'tip (_ "Sort & subtotal by account code."))
                                  (cons 'renderer-fn (lambda (a) (xaccSplitGetAccount a)))))

        (cons 'date         (list (cons 'sortkey (list SPLIT-TRANS TRANS-DATE-POSTED))
                                  (cons 'split-sortvalue #f)
                                  (cons 'text (_ "Date"))
                                  (cons 'tip (_ "Sort by date."))
                                  (cons 'renderer-fn #f)))

        (cons 'reconciled-date (list (cons 'sortkey (list SPLIT-DATE-RECONCILED))
                                     (cons 'split-sortvalue #f)
                                     (cons 'text (_ "Reconciled Date"))
                                     (cons 'tip (_ "Sort by the Reconciled Date."))
                                     (cons 'renderer-fn #f)))

        (cons 'reconciled-status (list (cons 'sortkey #f)
                                       (cons 'split-sortvalue (lambda (s) (length (memq (xaccSplitGetReconcile s)
                                                                                        '(#\n #\c #\y #\f #\v)))))
                                       (cons 'text (_ "Reconciled Status"))
                                       (cons 'tip (_ "Sort by the Reconciled Status"))
                                       (cons 'renderer-fn (lambda (s) (case (xaccSplitGetReconcile s)
                                                                        ((#\y) (_ "Reconciled"))
                                                                        ((#\c) (_ "Cleared"))
                                                                        ((#\n) (_ "Unreconciled"))
                                                                        ((#\f) (_ "Frozen"))
                                                                        ((#\v) (_ "Voided"))
                                                                        (else (_ "Unknown")))))))

        (cons 'register-order (list (cons 'sortkey (list QUERY-DEFAULT-SORT))
                                    (cons 'split-sortvalue #f)
                                    (cons 'text (_ "Register Order"))
                                    (cons 'tip (_ "Sort as in the register."))
                                    (cons 'renderer-fn #f)))

        (cons 'corresponding-acc-name (list (cons 'sortkey (list SPLIT-CORR-ACCT-NAME))
                                            (cons 'split-sortvalue (lambda (a) (xaccSplitGetCorrAccountFullName a)))
                                            (cons 'text (_ "Other Account Name"))
                                            (cons 'tip (_ "Sort by account transferred from/to's name."))
                                            (cons 'renderer-fn (lambda (a) (xaccSplitGetAccount (xaccSplitGetOtherSplit a))))))

        (cons 'corresponding-acc-code (list (cons 'sortkey (list SPLIT-CORR-ACCT-CODE))
                                            (cons 'split-sortvalue (lambda (a) (xaccSplitGetCorrAccountCode a)))
                                            (cons 'text (_ "Other Account Code"))
                                            (cons 'tip (_ "Sort by account transferred from/to's code."))
                                            (cons 'renderer-fn (lambda (a) (xaccSplitGetAccount (xaccSplitGetOtherSplit a))))))

        (cons 'amount        (list (cons 'sortkey (list SPLIT-VALUE))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Amount"))
                                   (cons 'tip (_ "Sort by amount."))
                                   (cons 'renderer-fn #f)))

        (cons 'description   (list (cons 'sortkey (list SPLIT-TRANS TRANS-DESCRIPTION))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Description"))
                                   (cons 'tip (_ "Sort by description."))
                                   (cons 'renderer-fn #f)))

        (if (and (gnc-current-session-exist)
                 (qof-book-use-split-action-for-num-field (gnc-get-current-book)))
            (cons 'number    (list (cons 'sortkey (list SPLIT-ACTION))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Number/Action"))
                                   (cons 'tip (_ "Sort by check number/action."))
                                   (cons 'renderer-fn #f)))

            (cons 'number    (list (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Number"))
                                   (cons 'tip (_ "Sort by check/transaction number."))
                                   (cons 'renderer-fn #f))))

        (cons 't-number      (list (cons 'sortkey (list SPLIT-TRANS TRANS-NUM))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Transaction Number"))
                                   (cons 'tip (_ "Sort by transaction number."))
                                   (cons 'renderer-fn #f)))

        (cons 'memo          (list (cons 'sortkey (list SPLIT-MEMO))
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "Memo"))
                                   (cons 'tip (_ "Sort by memo."))
                                   (cons 'renderer-fn #f)))

        (cons 'none          (list (cons 'sortkey '())
                                   (cons 'split-sortvalue #f)
                                   (cons 'text (_ "None"))
                                   (cons 'tip (_ "Do not sort."))
                                   (cons 'renderer-fn #f)))))

(define (time64-year t64)    (gnc:date-get-year (gnc-localtime t64)))
(define (time64-quarter t64) (+ (* 10 (gnc:date-get-year (gnc-localtime t64)))  (gnc:date-get-quarter (gnc-localtime t64))))
(define (time64-month t64)   (+ (* 100 (gnc:date-get-year (gnc-localtime t64))) (gnc:date-get-month (gnc-localtime t64))))
(define (time64-week t64)    (gnc:date-get-week (gnc-localtime t64)))
(define (time64-day t64)     (+ (* 500 (gnc:date-get-year (gnc-localtime t64))) (gnc:date-get-year-day (gnc-localtime t64))))
(define (time64->daily-string t) (qof-print-date t))
(define (split->time64 s) (xaccTransGetDate (xaccSplitGetParent s)))

(define date-subtotal-list
  ;; List for date option.
  ;; Defines the different date sorting keys, as an association-list. Each entry:
  ;;  'split-sortvalue     - function which retrieves number/string used for comparing splits
  ;;  'text                - text displayed in Display tab
  ;;  'tip                 - tooltip displayed in Display tab
  ;;  'renderer-fn         - func retrieve string for subtotal/subheading renderer
  ;;         #f means the date sortkey is not grouped
  ;;         otherwise it converts split->string
  (list
   (cons 'none (list
                (cons 'split-sortvalue #f)
                (cons 'text (_ "None"))
                (cons 'tip (_ "None."))
                (cons 'renderer-fn #f)))

   (cons 'daily (list
                  (cons 'split-sortvalue (lambda (s) (time64-day (split->time64 s))))
                  (cons 'text (_ "Daily"))
                  (cons 'tip (_ "Daily."))
                  (cons 'renderer-fn (lambda (s) (time64->daily-string (split->time64 s))))))

   (cons 'weekly (list
                  (cons 'split-sortvalue (lambda (s) (time64-week (split->time64 s))))
                  (cons 'text (_ "Weekly"))
                  (cons 'tip (_ "Weekly."))
                  (cons 'renderer-fn (lambda (s) (gnc:date-get-week-year-string (gnc-localtime (split->time64 s)))))))

   (cons 'monthly (list
                   (cons 'split-sortvalue (lambda (s) (time64-month (split->time64 s))))
                   (cons 'text (_ "Monthly"))
                   (cons 'tip (_ "Monthly."))
                   (cons 'renderer-fn (lambda (s) (gnc:date-get-month-year-string (gnc-localtime (split->time64 s)))))))

   (cons 'quarterly (list
                     (cons 'split-sortvalue (lambda (s) (time64-quarter (split->time64 s))))
                     (cons 'text (_ "Quarterly"))
                     (cons 'tip (_ "Quarterly."))
                     (cons 'renderer-fn (lambda (s) (gnc:date-get-quarter-year-string (gnc-localtime (split->time64 s)))))))

   (cons 'yearly (list
                  (cons 'split-sortvalue (lambda (s) (time64-year (split->time64 s))))
                  (cons 'text (_ "Yearly"))
                  (cons 'tip (_ "Yearly."))
                  (cons 'renderer-fn (lambda (s) (gnc:date-get-year-string (gnc-localtime (split->time64 s)))))))))

(define filter-list
  (list
   (cons 'none (list
                (cons 'text (_ "None"))
                (cons 'tip (_ "Do not do any filtering."))))

   (cons 'include (list
                   (cons 'text (_ "Include Transactions to/from Filter Accounts"))
                   (cons 'tip (_ "Include transactions to/from filter accounts only."))))

   (cons 'exclude (list
                   (cons 'text (_ "Exclude Transactions to/from Filter Accounts"))
                   (cons 'tip (_ "Exclude transactions to/from all filter accounts."))))))

(define show-void-list
  (list
   (cons 'non-void-only (list
                         (cons 'text (_ "Non-void only"))
                         (cons 'tip (_ "Show only non-voided transactions."))))

   (cons 'void-only (list
                     (cons 'text (_ "Void only"))
                     (cons 'tip (_ "Show only voided transactions."))))

   (cons 'both (list
                (cons 'text (_ "Both"))
                (cons 'tip (_ "Show both (and include void transactions in totals)."))))))

(define reconcile-status-list
  (list
   (cons 'all
         (list
          (cons 'text (_ "All"))
          (cons 'tip (_ "Show All Transactions"))
          (cons 'filter-types #f)))

   (cons 'unreconciled
         (list
          (cons 'text (_ "Unreconciled"))
          (cons 'tip (_ "Unreconciled only"))
          (cons 'filter-types (list #\n))))

   (cons 'cleared
         (list
          (cons 'text (_ "Cleared"))
          (cons 'tip (_ "Cleared only"))
          (cons 'filter-types (list #\c))))

   (cons 'reconciled
         (list
          (cons 'text (_ "Reconciled"))
          (cons 'tip (_ "Reconciled only"))
          (cons 'filter-types (list #\y))))))


(define ascending-list
  (list
   (cons 'ascend (list
                  (cons 'text (_ "Ascending"))
                  (cons 'tip (_ "Smallest to largest, earliest to latest."))))
   (cons 'descend (list
                   (cons 'text (_ "Descending"))
                   (cons 'tip (_ "Largest to smallest, latest to earliest."))))))

(define sign-reverse-list
  (list
   (cons 'global
         (list
          (cons 'text (_ "Use Global Preference"))
          (cons 'tip (_ "Use reversing option specified in global preference."))
          (cons 'acct-types #f)))
   (cons 'none
         (list
          (cons 'text (_ "None"))
          (cons 'tip (_ "Don't change any displayed amounts."))
          (cons 'acct-types '())))
   (cons 'income-expense
         (list
          (cons 'text (_ "Income and Expense"))
          (cons 'tip (_ "Reverse amount display for Income and Expense Accounts."))
          (cons 'acct-types (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE))))
   (cons 'credit-accounts
         (list
          (cons 'text (_ "Credit Accounts"))
          (cons 'tip (_ "Reverse amount display for Liability, Payable, Equity, \
Credit Card, and Income accounts."))
          (cons 'acct-types (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE
                                  ACCT-TYPE-EQUITY ACCT-TYPE-CREDIT
                                  ACCT-TYPE-INCOME))))))


(define (keylist-get-info keylist key info)
  (cdr (assq info (cdr (assq key keylist)))))

(define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))


;;
;; Set defaults for reconcilation report
;;
(define (reconcile-report-options-generator)
  (define options (trep-options-generator))
  (gnc:option-set-value (gnc:lookup-option options pagename-sorting optname-prime-sortkey) 'reconciled-status)
  (gnc:option-set-value (gnc:lookup-option options pagename-sorting optname-sec-sortkey)   'date)
  (gnc:option-set-value (gnc:lookup-option options pagename-sorting optname-sec-date-subtotal) 'none)
  ;; the start date should really be the last-reconcile-date but this information is not
  ;; easily accessible from scheme:
  (gnc:option-set-value (gnc:lookup-option options gnc:pagename-general optname-startdate) (cons 'relative 'start-prev-quarter))
  (gnc:option-set-value (gnc:lookup-option options gnc:pagename-general optname-enddate)   (cons 'relative 'today))
  (gnc:option-set-value (gnc:lookup-option options gnc:pagename-display (N_ "Reconciled Date")) #t)
  (gnc:option-set-value (gnc:lookup-option options gnc:pagename-display (N_ "Running Balance")) #t)
  (gnc:option-set-value (gnc:lookup-option options gnc:pagename-display (N_ "Memo")) #f)
  options)

;;
;; Default Transaction Report
;;
(define (trep-options-generator)

  (define options (gnc:new-options))
  (define BOOK-SPLIT-ACTION (qof-book-use-split-action-for-num-field (gnc-get-current-book)))
  (define (gnc:register-trep-option new-option)
    (gnc:register-option options new-option))

  ;; (Feb 2018) Note to future hackers - this trep-options-generator
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
      (begin
        (gnc-option-db-set-option-selectable-by-name options
                                                     gnc:pagename-general
                                                     optname-currency x)
        (gnc-option-db-set-option-selectable-by-name options
                                                     gnc:pagename-general
                                                     optname-orig-currency x)))))

  (gnc:options-add-currency!
   options gnc:pagename-general optname-currency "f")

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-orig-currency
    "f1" (_ "Also show original currency amounts") #f))

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-table-export
    "g" (_ "Formats the table suitable for cut & paste exporting with extra cells.") #f))

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
        (key-choice-list (keylist->vectorlist sortkey-list))
        (date-subtotal-choice-list (keylist->vectorlist date-subtotal-list))
        (prime-sortkey 'account-name)
        (prime-sortkey-subtotal-true #t)
        (sec-sortkey 'register-order)
        (sec-sortkey-subtotal-true #f))

    (define (apply-selectable-by-name-sorting-options)
      (let* ((prime-sortkey-enabled (not (eq? prime-sortkey 'none)))
             (prime-sortkey-subtotal-enabled (member prime-sortkey SUBTOTAL-ENABLED))
             (prime-date-sortingtype-enabled (member prime-sortkey DATE-SORTING-TYPES))
             (sec-sortkey-enabled (not (eq? sec-sortkey 'none)))
             (sec-sortkey-subtotal-enabled (member sec-sortkey SUBTOTAL-ENABLED))
             (sec-date-sortingtype-enabled (member sec-sortkey DATE-SORTING-TYPES)))

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
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-subtotals-only
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-informal-headers
         (or (member prime-sortkey (list 'account-name 'account-code))
             (member sec-sortkey (list 'account-name 'account-code))))

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
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-date-subtotal
      "e2" (_ "Do a date subtotal.")
      'monthly
      date-subtotal-choice-list))

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
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-date-subtotal
      "i2" (_ "Do a date subtotal.")
      'monthly
      date-subtotal-choice-list))

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
        (amount-is-single? #t))

    (define (apply-selectable-by-name-display-options)
      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Use Full Account Name")
       disp-accname?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Other Account Name")
       detail-is-single?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display (N_ "Sign Reverses")
       amount-is-single?)

      (gnc-option-db-set-option-selectable-by-name
       options gnc:pagename-display optname-grid
       amount-is-single?)

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

    (if BOOK-SPLIT-ACTION
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
      'single
      (list
       (vector 'none   (_ "None") (_ "No amount display."))
       (vector 'single (_ "Single") (_ "Single Column Display."))
       (vector 'double (_ "Double") (_ "Two Column Display.")))
      #f
      (lambda (x)
        (set! amount-is-single? (eq? x 'single))
        (apply-selectable-by-name-display-options))))

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

(define (make-split-table splits options custom-calculated-cells)

  (define (opt-val section name)
    (let ((option (gnc:lookup-option options section name)))
      (if option
          (gnc:option-value option)
          (gnc:error "gnc:lookup-option error: " section "/" name))))
  (define BOOK-SPLIT-ACTION (qof-book-use-split-action-for-num-field (gnc-get-current-book)))

  (define (build-columns-used)
    (define detail-is-single? (eq? (opt-val gnc:pagename-display optname-detail-level) 'single))
    (define amount-setting (opt-val gnc:pagename-display (N_ "Amount")))
    (list (cons 'date (opt-val gnc:pagename-display (N_ "Date")))
          (cons 'reconciled-date (opt-val gnc:pagename-display (N_ "Reconciled Date")))
          (cons 'num (if BOOK-SPLIT-ACTION
                         (opt-val gnc:pagename-display (N_ "Num/Action"))
                         (opt-val gnc:pagename-display (N_ "Num"))))
          (cons 'description (opt-val gnc:pagename-display (N_ "Description")))
          (cons 'account-name (opt-val gnc:pagename-display (N_ "Account Name")))
          (cons 'other-account-name (and detail-is-single?
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
          (cons 'subtotals-only (and (opt-val pagename-sorting optname-show-subtotals-only)
                                     (or (primary-get-info 'renderer-fn)
                                         (secondary-get-info 'renderer-fn))))
          (cons 'running-balance (opt-val gnc:pagename-display (N_ "Running Balance")))
          (cons 'account-full-name (opt-val gnc:pagename-display (N_ "Use Full Account Name")))
          (cons 'memo (opt-val gnc:pagename-display (N_ "Memo")))
          (cons 'account-code (opt-val gnc:pagename-display (N_ "Account Code")))
          (cons 'other-account-code (and detail-is-single?
                                         (opt-val gnc:pagename-display (N_ "Other Account Code"))))
          (cons 'other-account-full-name (and detail-is-single?
                                              (opt-val gnc:pagename-display (N_ "Use Full Other Account Name"))))
          (cons 'sort-account-code (opt-val pagename-sorting (N_ "Show Account Code")))
          (cons 'sort-account-full-name (opt-val pagename-sorting (N_ "Show Full Account Name")))
          (cons 'sort-account-description (opt-val pagename-sorting (N_ "Show Account Description")))
          (cons 'notes (opt-val gnc:pagename-display (N_ "Notes")))))

  (define (primary-get-info info)
    (let ((sortkey (opt-val pagename-sorting optname-prime-sortkey)))
      (if (member sortkey DATE-SORTING-TYPES)
          (keylist-get-info date-subtotal-list (opt-val pagename-sorting optname-prime-date-subtotal) info)
          (and (member sortkey SUBTOTAL-ENABLED)
               (and (opt-val pagename-sorting optname-prime-subtotal)
                    (keylist-get-info sortkey-list sortkey info))))))

  (define (secondary-get-info info)
    (let ((sortkey (opt-val pagename-sorting optname-sec-sortkey)))
      (if (member sortkey DATE-SORTING-TYPES)
          (keylist-get-info date-subtotal-list (opt-val pagename-sorting optname-sec-date-subtotal) info)
          (and (member sortkey SUBTOTAL-ENABLED)
               (and (opt-val pagename-sorting optname-sec-subtotal)
                    (keylist-get-info sortkey-list sortkey info))))))

  (let* ((work-to-do (length splits))
         (work-done 0)
         (table (gnc:make-html-table))
         (used-columns (build-columns-used))
         (account-types-to-reverse
          (keylist-get-info sign-reverse-list
                            (opt-val gnc:pagename-display (N_ "Sign Reverses"))
                            'acct-types))
         (is-multiline? (eq? (opt-val gnc:pagename-display optname-detail-level) 'multi-line))
         (export? (opt-val gnc:pagename-general optname-table-export)))

    (define (column-uses? param)
      (cdr (assq param used-columns)))

    (define left-columns
      (let* ((add-if (lambda (pred? . items) (if pred? items '())))
             (left-cols-list
              (append
               (add-if (column-uses? 'date)
                       (vector (_ "Date")
                               (lambda (split transaction-row?)
                                 (if transaction-row?
                                     (gnc:make-html-table-cell/markup
                                      "date-cell"
                                      (qof-print-date (xaccTransGetDate (xaccSplitGetParent split))))
                                     ""))))

               (add-if (column-uses? 'reconciled-date)
                       (vector (_ "Reconciled Date")
                               (lambda (split transaction-row?)
                                 (gnc:make-html-table-cell/markup
                                  "date-cell"
                                  (if (eqv? (xaccSplitGetReconcile split) #\y)
                                      (qof-print-date (xaccSplitGetDateReconciled split))
                                      "")))))

               (add-if (column-uses? 'num)
                       (vector (if (and BOOK-SPLIT-ACTION
                                        (opt-val gnc:pagename-display (N_ "Trans Number")))
                                   (_ "Num/T-Num")
                                   (_ "Num"))
                               (lambda (split transaction-row?)
                                 (let* ((trans (xaccSplitGetParent split))
                                        (num (gnc-get-num-action trans split))
                                        (t-num (if (and BOOK-SPLIT-ACTION
                                                        (opt-val gnc:pagename-display (N_ "Trans Number")))
                                                   (gnc-get-num-action trans #f)
                                                   ""))
                                        (num-string (if (string-null? t-num)
                                                        num
                                                        (string-append num "/" t-num))))
                                   (if transaction-row?
                                       (gnc:make-html-table-cell/markup "text-cell" num-string)
                                       "")))))

               (add-if (column-uses? 'description)
                       (vector (_ "Description")
                               (lambda (split transaction-row?)
                                 (define trans (xaccSplitGetParent split))
                                 (if transaction-row?
                                     (gnc:make-html-table-cell/markup
                                      "text-cell"
                                      (xaccTransGetDescription trans))
                                     ""))))

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
                                 (define account (xaccSplitGetAccount split))
                                 (account-namestring account
                                                     (column-uses? 'account-code)
                                                     (column-uses? 'account-name)
                                                     (column-uses? 'account-full-name)))))

               (add-if (or (column-uses? 'other-account-name) (column-uses? 'other-account-code))
                       (vector (_ "Transfer from/to")
                               (lambda (split transaction-row?)
                                 (define other-account (xaccSplitGetAccount (xaccSplitGetOtherSplit split)))
                                 (account-namestring other-account
                                                     (column-uses? 'other-account-code)
                                                     (column-uses? 'other-account-name)
                                                     (column-uses? 'other-account-full-name)))))

               (add-if (column-uses? 'shares)
                       (vector (_ "Shares")
                               (lambda (split transaction-row?)
                                 (gnc:make-html-table-cell/markup
                                  "number-cell"
                                  (xaccSplitGetAmount split)))))

               (add-if (column-uses? 'price)
                       (vector (_ "Price")
                               (lambda (split transaction-row?)
                                 ;; share price is retrieved as an exact rational; convert for
                                 ;; presentation to decimal, rounded to the currency SCU, optionally
                                 ;; increasing precision by 2 significant digits.
                                 (let* ((currency (xaccTransGetCurrency (xaccSplitGetParent split)))
                                        (scu (gnc-commodity-get-fraction currency))
                                        (price (xaccSplitGetSharePrice split))
                                        (price-decimal (gnc-numeric-convert price
                                                                            (if (< scu 10000)
                                                                                (* scu 100)
                                                                                scu)
                                                                            GNC-HOW-RND-ROUND)))
                                   (gnc:make-html-table-cell/markup
                                    "number-cell"
                                    (gnc:make-gnc-monetary currency price-decimal)))))))))

        (if (and (null? left-cols-list)
                 (or (opt-val gnc:pagename-display "Totals")
                     (primary-get-info 'renderer-fn)
                     (secondary-get-info 'renderer-fn)))
            (list (vector "" (lambda (s t) #f)))
            left-cols-list)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; calculated-cells
    ;;
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define default-calculated-cells
      (letrec
          ((damount (lambda (s) (if (gnc:split-voided? s)
                                    (xaccSplitVoidFormerAmount s)
                                    (xaccSplitGetAmount s))))
           (trans-date (lambda (s) (xaccTransGetDate (xaccSplitGetParent s))))
           (currency (lambda (s) (xaccAccountGetCommodity (xaccSplitGetAccount s))))
           (report-currency (lambda (s) (if (column-uses? 'common-currency)
                                            (opt-val gnc:pagename-general optname-currency)
                                            (currency s))))
           (friendly-debit (lambda (a) (gnc:get-debit-string (xaccAccountGetType a))))
           (friendly-credit (lambda (a) (gnc:get-credit-string (xaccAccountGetType a))))
           (header-commodity (lambda (str)
                               (string-append
                                str
                                (if (column-uses? 'common-currency)
                                    (string-append
                                     "<br />"
                                     (gnc-commodity-get-mnemonic
                                      (opt-val gnc:pagename-general optname-currency)))
                                    ""))))
           (convert (lambda (s num)
                      (gnc:exchange-by-pricedb-nearest
                       (gnc:make-gnc-monetary (currency s) num)
                       (report-currency s)
                       ;; Use midday as the transaction time so it matches a price
                       ;; on the same day.  Otherwise it uses midnight which will
                       ;; likely match a price on the previous day
                       (time64CanonicalDayTime (trans-date s)))))
           (split-value (lambda (s) (convert s (damount s)))) ; used for correct debit/credit
           (amount (lambda (s) (split-value s)))
           (debit-amount (lambda (s) (and (positive? (gnc:gnc-monetary-amount (split-value s)))
                                          (split-value s))))
           (credit-amount (lambda (s) (if (positive? (gnc:gnc-monetary-amount (split-value s)))
                                          #f
                                          (gnc:monetary-neg (split-value s)))))
           (original-amount (lambda (s) (gnc:make-gnc-monetary (currency s) (damount s))))
           (original-debit-amount (lambda (s) (if (positive? (damount s))
                                                  (original-amount s)
                                                  #f)))
           (original-credit-amount (lambda (s) (if (positive? (damount s))
                                                   #f
                                                   (gnc:monetary-neg (original-amount s)))))
           (running-balance (lambda (s) (gnc:make-gnc-monetary (currency s) (xaccSplitGetBalance s)))))
        (append
         ;; each column will be a vector
         ;; (vector heading
         ;;         calculator-function                          ;; (calculator-function split) to obtain amount
         ;;         reverse-column?                              ;; #t to allow reverse signs
         ;;         subtotal?                                    ;; #t to allow subtotals (ie must be #f for running balance)
         ;;         start-dual-column?                           ;; #t for the debit side of a dual column (i.e. debit/credit)
         ;;                                                      ;; which means the next column must be the credit side
         ;;         friendly-heading-fn                          ;; (friendly-heading-fn account) to retrieve friendly name for account debit/credit
         (if (column-uses? 'amount-single)
             (list (vector (header-commodity (_ "Amount"))
                           amount #t #t #f
                           (lambda (a) "")))
             '())
         (if (column-uses? 'amount-double)
             (list (vector (header-commodity (_ "Debit"))
                           debit-amount #f #t #t
                           friendly-debit)
                   (vector (header-commodity (_ "Credit"))
                           credit-amount #f #t #f
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
                           (lambda (a) "")))
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
      (let* ((row-contents '())
             (sortkey (opt-val pagename-sorting
                               (case level
                                 ((primary) optname-prime-sortkey)
                                 ((secondary) optname-sec-sortkey))))
             (left-indent (case level
                            ((primary total) 0)
                            ((secondary) primary-indent)))
             (right-indent (- indent-level left-indent)))
        (for-each (lambda (cell) (addto! row-contents cell))
                  (gnc:html-make-empty-cells left-indent))
        (if (and (opt-val pagename-sorting optname-show-informal-headers)
                 (column-uses? 'amount-double)
                 (member sortkey SORTKEY-INFORMAL-HEADERS))
            (begin
              (if export?
                  (begin
                    (addto! row-contents (gnc:make-html-table-cell data))
                    (for-each (lambda (cell) (addto! row-contents cell))
                              (gnc:html-make-empty-cells (+ right-indent width-left-columns -1))))
                  (addto! row-contents (gnc:make-html-table-cell/size
                                        1 (+ right-indent width-left-columns) data)))
              (for-each (lambda (cell)
                          (addto! row-contents
                                  (gnc:make-html-table-cell
                                   "<b>"
                                   ((vector-ref cell 5)
                                    ((keylist-get-info sortkey-list sortkey 'renderer-fn) split))
                                   "</b>")))
                        calculated-cells))
            (addto! row-contents (gnc:make-html-table-cell/size
                                  1 (+ right-indent width-left-columns width-right-columns) data)))

        (if (not (column-uses? 'subtotals-only))
            (gnc:html-table-append-row/markup! table subheading-style (reverse row-contents)))))

    (define (add-subtotal-row subtotal-string subtotal-collectors subtotal-style level row col)
      (let* ((row-contents '())
             (left-indent (case level
                            ((total) 0)
                            ((primary) primary-indent)
                            ((secondary) (+ primary-indent secondary-indent))))
             (right-indent (- indent-level left-indent))
             (merge-list (map (lambda (cell) (vector-ref cell 4)) calculated-cells))
             (columns (map (lambda (coll) (coll 'format gnc:make-gnc-monetary #f)) subtotal-collectors))
             (list-of-commodities (delete-duplicates (map gnc:gnc-monetary-commodity (concatenate columns))
                                                     gnc-commodity-equal)))

        (define (retrieve-commodity list-of-monetary commodity)
          (if (null? list-of-monetary)
              #f
              (if (gnc-commodity-equal (gnc:gnc-monetary-commodity (car list-of-monetary)) commodity)
                  (car list-of-monetary)
                  (retrieve-commodity (cdr list-of-monetary) commodity))))

        (define (add-first-column string)
          (if export?
              (begin
                (addto! row-contents (gnc:make-html-table-cell/markup "total-label-cell" string))
                (for-each (lambda (cell) (addto! row-contents cell))
                          (gnc:html-make-empty-cells (+ right-indent width-left-columns -1))))
              (addto! row-contents (gnc:make-html-table-cell/size/markup 1 (+ right-indent width-left-columns) "total-label-cell" string))))

        (define (add-columns commodity)
          (let ((start-dual-column? #f)
                (dual-subtotal #f))
            (for-each (lambda (column merge-entry)
                        (let* ((mon (retrieve-commodity column commodity))
                               (column-amount (and mon (gnc:gnc-monetary-amount mon)))
                               (merge? merge-entry))
                          (if merge?
                              ;; We're merging. If a subtotal exists, store
                              ;; it in dual-subtotal. Do NOT add column to row.
                              (begin
                                (set! dual-subtotal column-amount)
                                (set! start-dual-column? #t))
                              (if start-dual-column?
                                  (begin
                                    ;; We've completed merging. Add the negated
                                    ;; column amount and add the columns to row.
                                    (if column-amount
                                        (set! dual-subtotal
                                          (- (or dual-subtotal 0) column-amount)))
                                    (cond ((not dual-subtotal)
                                           (addto! row-contents "")
                                           (addto! row-contents ""))
                                          ((positive? dual-subtotal)
                                           (addto! row-contents
                                                   (gnc:make-html-table-cell/markup
                                                    "total-number-cell"
                                                    (gnc:make-gnc-monetary
                                                     commodity
                                                     dual-subtotal)))
                                           (addto! row-contents ""))
                                          (else
                                           (addto! row-contents "")
                                           (addto! row-contents
                                                   (gnc:make-html-table-cell/markup
                                                    "total-number-cell"
                                                    (gnc:make-gnc-monetary
                                                     commodity
                                                     (- dual-subtotal))))))
                                    (set! start-dual-column? #f)
                                    (set! dual-subtotal #f))
                                  ;; Default; not merging/completed merge. Just
                                  ;; display monetary amount
                                  (addto! row-contents
                                          (gnc:make-html-table-cell/markup "total-number-cell" mon))))))
                      columns
                      merge-list)))

        ;; we only wish to add the first column into the grid.
        (if (pair? columns)
            (set! grid (grid-add grid row col (car columns))))

        ;;first row
        (for-each (lambda (cell) (addto! row-contents cell))
                  (gnc:html-make-empty-cells left-indent))
        (add-first-column subtotal-string)
        (add-columns (if (pair? list-of-commodities)
                         (car list-of-commodities)
                         #f)) ;to account for empty-row subtotals
        (gnc:html-table-append-row/markup! table subtotal-style (reverse row-contents))

        ;;subsequent rows
        (if (pair? list-of-commodities)
            (for-each (lambda (commodity)
                        (set! row-contents '())
                        (for-each (lambda (cell) (addto! row-contents cell))
                                  (gnc:html-make-empty-cells left-indent))
                        (add-first-column "")
                        (add-columns commodity)
                        (gnc:html-table-append-row/markup! table subtotal-style (reverse row-contents)))
                      (cdr list-of-commodities)))))

    (define (total-string str) (string-append (_ "Total For ") str))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; renderers

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; display an account name depending on the options the user has set
    (define (account-namestring account show-account-code? show-account-name? show-account-full-name?)
      ;;# on multi-line splits we can get an empty ('()) account
      (if (null? account)
          (_ "Split Transaction")
          (string-append
           ;; display account code?
           (if show-account-code?
               (string-append (xaccAccountGetCode account) " ")
               "")
           ;; display account name?
           (if show-account-name?
               ;; display full account name?
               (if show-account-full-name?
                   (gnc-account-get-full-name account)
                   (xaccAccountGetName account))
               ""))))

    ;; retrieve date renderer from the date-subtotal-list
    (define (render-date date-subtotal-key split)
      ((keylist-get-info date-subtotal-list date-subtotal-key 'renderer-fn) split))

    ;; generate account name, optionally with anchor to account register
    (define (render-account sortkey split anchor?)
      (let* ((account ((keylist-get-info sortkey-list sortkey 'renderer-fn) split))
             (name (account-namestring account
                                       (column-uses? 'sort-account-code)
                                       #t
                                       (column-uses? 'sort-account-full-name)))
             (description (if (and (column-uses? 'sort-account-description)
                                   (not (string-null? (xaccAccountGetDescription account))))
                              (string-append ": " (xaccAccountGetDescription account))
                              "")))
        (if (and anchor? (not (null? account))) ;html anchor for 2-split transactions only
            (gnc:make-html-text
             (gnc:html-markup-anchor (gnc:account-anchor-text account) name)
             description)
            name)))

    ;; generic renderer. retrieve renderer-fn which should return a str
    (define (render-generic sortkey split)
      ((keylist-get-info sortkey-list sortkey 'renderer-fn) split))

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
         ((member sortkey DATE-SORTING-TYPES)
          (render-date date-subtotal-key split))
         ((member sortkey ACCOUNT-SORTING-TYPES)
          (render-account sortkey split anchor?))
         ((eq? sortkey 'reconciled-status)
          (render-generic sortkey split)))))

    (define (render-grand-total)
      (_ "Grand Total"))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; add-split-row
    ;;
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (add-split-row split cell-calculators row-style transaction-row?)
      (let* ((row-contents '())
             (trans (xaccSplitGetParent split))
             (account (xaccSplitGetAccount split)))

        (define left-cols
          (map (lambda (left-col)
                 (let* ((col-fn (vector-ref left-col 1))
                        (col-data (col-fn split transaction-row?)))
                   col-data))
               left-columns))

        (define cells
          (map (lambda (cell)
                 (let* ((calculator (vector-ref cell 1))
                        (reverse? (vector-ref cell 2))
                        (subtotal? (vector-ref cell 3))
                        (calculated (calculator split)))
                   (vector calculated
                           reverse?
                           subtotal?)))
               cell-calculators))

        (for-each (lambda (cell) (addto! row-contents cell))
                  (gnc:html-make-empty-cells indent-level))

        (for-each (lambda (col)
                    (addto! row-contents col))
                  left-cols)

        (for-each (lambda (cell)
                    (let ((cell-content (vector-ref cell 0))
                          ;; reverse? returns a bool - will check if the cell type has reversible sign,
                          ;; whether the account is also reversible according to Report Option, or
                          ;; if Report Option follows Global Settings, will retrieve bool from it.
                          (reverse? (and (vector-ref cell 1)
                                         (if account-types-to-reverse
                                             (member (xaccAccountGetType account) account-types-to-reverse)
                                             (gnc-reverse-balance account)))))
                      (if cell-content
                          (addto! row-contents
                                  (gnc:make-html-table-cell/markup
                                   "number-cell"
                                   (gnc:html-transaction-anchor
                                    trans
                                    ;; if conditions for reverse are satisfied, apply sign reverse to
                                    ;; monetary amount
                                    (if reverse?
                                        (gnc:monetary-neg cell-content)
                                        cell-content))))
                          (addto! row-contents (gnc:html-make-empty-cell)))))
                  cells)

        (if (not (column-uses? 'subtotals-only))
            (gnc:html-table-append-row/markup! table row-style (reverse row-contents)))

        (map (lambda (cell)
               (let ((cell-content (vector-ref cell 0))
                     (subtotal? (vector-ref cell 2)))
                 (and subtotal? cell-content)))
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

    (define (do-rows-with-subtotals splits odd-row?)
      (define primary-subtotal-comparator (primary-get-info 'split-sortvalue))
      (define secondary-subtotal-comparator (secondary-get-info 'split-sortvalue))

      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))

      (set! work-done (+ 1 work-done))

      (if (null? splits)

          (if (opt-val gnc:pagename-display "Totals")
              (begin
                (gnc:html-table-append-row/markup!
                 table def:grand-total-style
                 (list
                  (gnc:make-html-table-cell/size
                   1 (+ indent-level width-left-columns width-right-columns)
                   (gnc:make-html-text (gnc:html-markup-hr)))))

                (add-subtotal-row (render-grand-total) total-collectors def:grand-total-style 'total 'row-total 'col-total)))

          (let* ((current (car splits))
                 (rest (cdr splits))
                 (next (if (null? rest) #f (car rest)))
                 (split-values (add-split-row
                                current
                                calculated-cells
                                (if is-multiline? def:normal-row-style
                                    (if odd-row?
                                        def:normal-row-style
                                        def:alternate-row-style))
                                #t)))

            (if is-multiline?
                (for-each
                 (lambda (othersplits)
                   (add-split-row othersplits calculated-cells def:alternate-row-style #f))
                 (delete current (xaccTransGetSplitList (xaccSplitGetParent current)))))

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 primary-subtotal-collectors
                 split-values)

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 secondary-subtotal-collectors
                 split-values)

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 total-collectors
                 split-values)

            (if (and primary-subtotal-comparator
                     (or (not next)
                         (and next
                              (not (equal? (primary-subtotal-comparator current)
                                           (primary-subtotal-comparator next))))))

                (begin
                  (if secondary-subtotal-comparator
                      (begin
                        (add-subtotal-row (total-string
                                           (render-summary current 'secondary #f))
                                          secondary-subtotal-collectors
                                          def:secondary-subtotal-style
                                          'secondary
                                          (cons (primary-subtotal-comparator current)
                                                (render-summary current 'primary #f))
                                          (cons (secondary-subtotal-comparator current)
                                                (render-summary current 'secondary #f)))
                        (for-each (lambda (coll) (coll 'reset #f #f))
                                  secondary-subtotal-collectors)))
                  (add-subtotal-row (total-string
                                     (render-summary current 'primary #f))
                                    primary-subtotal-collectors
                                    def:primary-subtotal-style
                                    'primary
                                    (cons (primary-subtotal-comparator current)
                                          (render-summary current 'primary #f))
                                    'col-total)
                  (for-each (lambda (coll) (coll 'reset #f #f))
                            primary-subtotal-collectors)
                  (if next
                      (begin
                        (add-subheading (render-summary next 'primary #t)
                                        def:primary-subtotal-style next 'primary)
                        (if secondary-subtotal-comparator
                            (add-subheading (render-summary next 'secondary #t)
                                            def:secondary-subtotal-style next 'secondary)))))

                (if (and secondary-subtotal-comparator
                         (or (not next)
                             (and next
                                  (not (equal? (secondary-subtotal-comparator current)
                                               (secondary-subtotal-comparator next))))))
                    (begin (add-subtotal-row (total-string
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
                           (for-each (lambda (coll) (coll 'reset #f #f))
                                     secondary-subtotal-collectors)
                           (if next
                               (add-subheading (render-summary next 'secondary #t)
                                               def:secondary-subtotal-style next 'secondary)))))

            (do-rows-with-subtotals rest (not odd-row?)))))

    (define grid (make-grid))

    (gnc:html-table-set-col-headers! table (concatenate (list
                                                         (gnc:html-make-empty-cells indent-level)
                                                         headings-left-columns
                                                         headings-right-columns)))

    (if (primary-get-info 'renderer-fn)
        (add-subheading (render-summary (car splits) 'primary #t)
                        def:primary-subtotal-style (car splits) 'primary))

    (if (secondary-get-info 'renderer-fn)
        (add-subheading (render-summary (car splits) 'secondary #t)
                        def:secondary-subtotal-style (car splits) 'secondary))

    (do-rows-with-subtotals splits #t)

    (values table
            grid)))


;; grid data structure
(define (make-grid)
  '())
(define (grid-get grid row col)    ; grid filter - get all row/col - if #f then retrieve whole row/col
  (filter
   (lambda (cell)
     (and (or (not row) (equal? row (vector-ref cell 0)))
          (or (not col) (equal? col (vector-ref cell 1)))))
   grid))
(define (grid-del grid row col)    ; grid filter - del all row/col - if #f then delete whole row/col - CAREFUL!
  (filter
   (lambda (cell)
     (not (and (or (not row) (equal? row (vector-ref cell 0)))
               (or (not col) (equal? col (vector-ref cell 1))))))
   grid))
(define (grid-rows grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 0)) grid)))
(define (grid-cols grid)
  (delete-duplicates (map (lambda (cell) (vector-ref cell 1)) grid)))
(define (grid-add grid row col data)            ;-> misonomer - we don't 'add' to existing data,
  (set! grid (grid-del grid row col))           ;we simply delete old data stored at row/col and 
  (set! grid (cons (vector row col data) grid)) ;add again. this is fine because the grid should
  grid)                                         ;never have duplicate data in the trep.
(define (grid->html grid list-of-rows list-of-cols)
  (define (cell->html cell)
    (if (pair? cell)
        (string-append "<td class=\"number-cell\">"
                       (string-join (map gnc:monetary->string
                                         (vector-ref (car cell) 2))
                                    "<br/>\n")
                       "</td>\n")
        "<td></td>\n"))
  (define (row->html row list-of-cols)
    (string-append "<tr><td>"
                   (if (eq? row 'row-total)
                       (_ "Grand Total")
                       (cdr row))
                   "</td>\n"
                   (string-join (map
                                 (lambda (col) (cell->html (grid-get grid row col)))
                                 list-of-cols) "")
                   (cell->html (grid-get grid row 'col-total))
                   "</tr>\n"))
  (string-append "<table class=\"summary-table\"><caption>"
                 optname-grid
                 "</caption><thead><tr>"
                 "<th></th>\n"
                 (string-join (map (lambda (col)
                                     (string-append "<th class=\"column-heading-right\">"
                                                    (cdr col)
                                                    "</th>\n")) list-of-cols) "")
                 "<th class=\"column-heading-right\">"
                 (_ "Total")
                 "</th>\n</tr>\n</thead><tbody>"
                 (string-join (map (lambda (row)
                                     (row->html row list-of-cols))
                                   list-of-rows) "")
                 (if (memq 'row-total (grid-rows grid))
                     (row->html 'row-total list-of-cols)
                     "")
                 "</tbody></table>\n"))


;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the renderer function for this report.


(define* (trep-renderer report-obj #:key custom-calculated-cells empty-report-message custom-split-filter)
  ;; the trep-renderer is a define* function which, at minimum, takes the report object
  ;;
  ;; the optional arguments are:
  ;; #:custom-calculated-cells - a list of vectors to define customized data columns
  ;; #:empty-report-message - a str which is displayed at the initial report opening
  ;; #:custom-split-filter - a split->bool function to add to the split filter

  (define options (gnc:report-options report-obj))
  (define (opt-val section name) (gnc:option-value (gnc:lookup-option options section name)))
  (define BOOK-SPLIT-ACTION (qof-book-use-split-action-for-num-field (gnc-get-current-book)))

  (define (is-filter-member split account-list)
    (let* ((txn (xaccSplitGetParent split))
           (splitcount (xaccTransCountSplits txn))
           (other-account (xaccSplitGetAccount (xaccSplitGetOtherSplit split)))
           (splits-equal? (lambda (s1 s2) (xaccSplitEqual s1 s2 #t #f #f)))
           (other-splits (delete split (xaccTransGetSplitList txn) splits-equal?))
           (other-accounts (map xaccSplitGetAccount other-splits))
           (is-in-account-list? (lambda (acc) (member acc account-list))))
      (cond
        ;; A 2-split transaction - test separately so it can be optimized
        ;; to significantly reduce the number of splits to traverse
        ;; in guile code
        ((= splitcount 2) (is-in-account-list? other-account))
        ;; A multi-split transaction - run over all splits
        ((> splitcount 2) (or-map is-in-account-list? other-accounts))
        ;; Single transaction splits
        (else #f))))

  (gnc:report-starting reportname)

  (let* ((document (gnc:make-html-document))
         (account-matcher (opt-val pagename-filter optname-account-matcher))
         (account-matcher-regexp (and (opt-val pagename-filter optname-account-matcher-regex)
                                      (make-regexp account-matcher)))
         (c_account_0 (opt-val gnc:pagename-accounts optname-accounts))
         (c_account_1 (filter
                       (lambda (acc)
                         (if account-matcher-regexp
                             (regexp-exec account-matcher-regexp (gnc-account-get-full-name acc))
                             (string-contains (gnc-account-get-full-name acc) account-matcher)))
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
         (transaction-matcher-regexp (and (opt-val pagename-filter optname-transaction-matcher-regex)
                                          (make-regexp transaction-matcher)))
         (reconcile-status-filter (keylist-get-info reconcile-status-list
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
         (splits '())
         (custom-sort? (or (and (member primary-key DATE-SORTING-TYPES)   ; this will remain
                                (not (eq? primary-date-subtotal 'none)))  ; until qof-query
                           (and (member secondary-key DATE-SORTING-TYPES) ; is upgraded
                                (not (eq? secondary-date-subtotal 'none)))
                           (or (member primary-key CUSTOM-SORTING)
                               (member secondary-key CUSTOM-SORTING))))
         (infobox-display (opt-val gnc:pagename-general optname-infobox-display))
         (query (qof-query-create-for-splits)))

    (define (generic-less? X Y key date-subtotal ascend?)
      (define comparator-function
        (if (member key DATE-SORTING-TYPES)
            (let ((date (lambda (s)
                          (case key
                            ((date) (xaccTransGetDate (xaccSplitGetParent s)))
                            ((reconciled-date) (xaccSplitGetDateReconciled s))))))
              (case date-subtotal
                ((yearly)    (lambda (s) (time64-year (date s))))
                ((monthly)   (lambda (s) (time64-month (date s))))
                ((quarterly) (lambda (s) (time64-quarter (date s))))
                ((weekly)    (lambda (s) (time64-week (date s))))
                ((daily)     (lambda (s) (time64-day (date s))))
                ((none)      (lambda (s) (date s)))))
            (case key
              ((account-name) (lambda (s) (gnc-account-get-full-name (xaccSplitGetAccount s))))
              ((account-code) (lambda (s) (xaccAccountGetCode (xaccSplitGetAccount s))))
              ((corresponding-acc-name) (lambda (s) (xaccSplitGetCorrAccountFullName s)))
              ((corresponding-acc-code) (lambda (s) (xaccSplitGetCorrAccountCode s)))
              ((reconciled-status) (lambda (s) (length (memq (xaccSplitGetReconcile s)
                                                             '(#\n #\c #\y #\f #\v)))))
              ((amount) (lambda (s) (gnc-numeric-to-scm (xaccSplitGetValue s))))
              ((description) (lambda (s) (xaccTransGetDescription (xaccSplitGetParent s))))
              ((number) (lambda (s)
                          (if BOOK-SPLIT-ACTION
                              (xaccSplitGetAction s)
                              (xaccTransGetNum (xaccSplitGetParent s)))))
              ((t-number) (lambda (s) (xaccTransGetNum (xaccSplitGetParent s))))
              ((register-order) (lambda (s) #f))
              ((memo) (lambda (s) (xaccSplitGetMemo s)))
              ((none) (lambda (s) #f)))))
      (cond
       ((string? (comparator-function X)) ((if ascend? string<? string>?) (comparator-function X) (comparator-function Y)))
       ((comparator-function X)           ((if ascend? < >)               (comparator-function X) (comparator-function Y)))
       (else                              #f)))

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


    (if (or (null? c_account_1) (and-map not c_account_1))

        ;; error condition: no accounts specified or obtained after filtering
        (begin

          (gnc:html-document-add-object!
           document
           (gnc:html-make-no-account-warning
            report-title (gnc:report-id report-obj)))

          ;; if an empty-report-message is passed by a derived report to
          ;; the renderer, display it here.
          (if empty-report-message
              (gnc:html-document-add-object!
               document
               empty-report-message))

          (if (memq infobox-display '(always no-match))
              (gnc:html-document-add-object!
               document
               (gnc:render-options-changed options))))

        (begin

          (qof-query-set-book query (gnc-get-current-book))
          (xaccQueryAddAccountMatch query c_account_1 QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          (xaccQueryAddDateMatchTT query #t begindate #t enddate QOF-QUERY-AND)
          (case void-status
            ((non-void-only) (gnc:query-set-match-non-voids-only! query (gnc-get-current-book)))
            ((void-only)     (gnc:query-set-match-voids-only! query (gnc-get-current-book)))
            (else #f))
          (if (not custom-sort?)
              (begin
                (qof-query-set-sort-order query
                                          (keylist-get-info sortkey-list primary-key 'sortkey)
                                          (keylist-get-info sortkey-list secondary-key 'sortkey)
                                          '())
                (qof-query-set-sort-increasing query
                                               (eq? primary-order 'ascend)
                                               (eq? secondary-order 'ascend)
                                               #t)))

          (if (opt-val "__trep" "unique-transactions")
              (set! splits (xaccQueryGetSplitsUniqueTrans query))
              (set! splits (qof-query-run query)))

          (qof-query-destroy query)

          (if custom-sort?
              (begin
                (set! splits (stable-sort! splits date-comparator?))
                (set! splits (stable-sort! splits secondary-comparator?))
                (set! splits (stable-sort! splits primary-comparator?))))

          ;; Combined Filter:
          ;; - include/exclude splits to/from selected accounts
          ;; - substring/regex matcher for Transaction Description/Notes/Memo
          ;; - custom-split-filter, a split->bool function for derived reports
          ;; - by reconcile status
          (set! splits (filter
                        (lambda (split)
                          (let* ((trans (xaccSplitGetParent split))
                                 (match? (lambda (str)
                                           (if transaction-matcher-regexp
                                               (regexp-exec transaction-matcher-regexp str)
                                               (string-contains str transaction-matcher)))))
                            (and (case filter-mode
                                   ((none) #t)
                                   ((include) (is-filter-member split c_account_2))
                                   ((exclude) (not (is-filter-member split c_account_2))))
                                 (or (string-null? transaction-matcher) ; null-string = ignore filters
                                     (match? (xaccTransGetDescription trans))
                                     (match? (xaccTransGetNotes trans))
                                     (match? (xaccSplitGetMemo split)))
                                 (or (not custom-split-filter)     ; #f = ignore custom-split-filter
                                     (custom-split-filter split))
                                 (or (not reconcile-status-filter) ; #f = ignore reconcile-status-filter
                                     (memv (xaccSplitGetReconcile split)
                                           reconcile-status-filter)))))
                        splits))

          (if (null? splits)

              ;; error condition: no splits found
              (begin

                (gnc:html-document-add-object!
                 document
                 (gnc:html-make-generic-warning
                  report-title (gnc:report-id report-obj)
                  NO-MATCHING-TRANS-HEADER NO-MATCHING-TRANS-TEXT))

                (if (memq infobox-display '(always no-match))
                    (gnc:html-document-add-object!
                     document
                     (gnc:render-options-changed options))))

              (let-values (((table grid) (make-split-table splits options custom-calculated-cells)))

                (gnc:html-document-set-title! document report-title)

                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-h3
                   (format #f
                            (_ "From ~a to ~a")
                            (qof-print-date begindate)
                            (qof-print-date enddate)))))

                (if (and (opt-val gnc:pagename-display optname-grid)
                         (eq? (opt-val gnc:pagename-display (N_ "Amount")) 'single))
                    (let* ((generic<? (lambda (a b)
                                        (cond ((string? (car a)) (string<? (car a) (car b)))
                                              ((number? (car a)) (< (car a) (car b)))
                                              (else (gnc:error "unknown sortvalue")))))
                           (list-of-rows (stable-sort! (delete 'row-total (grid-rows grid)) generic<?))
                           (list-of-cols (stable-sort! (delete 'col-total (grid-cols grid)) generic<?)))
                      (gnc:html-document-add-object!
                       document (grid->html grid list-of-rows list-of-cols))))

                (if (eq? infobox-display 'always)
                    (gnc:html-document-add-object!
                     document
                     (gnc:render-options-changed options)))

                (gnc:html-document-add-object! document table)))))

    (gnc:report-finished)

    document))

(define trep-guid "2fe3b9833af044abb929a88d5a59620f")
(export trep-guid)
(export trep-renderer)
(export trep-options-generator)

;; Define the report.
(gnc:define-report
 'version 1
 'name (_ "Reconciliation Report")
 'report-guid "e45218c6d76f11e7b5ef0800277ef320"
 'options-generator reconcile-report-options-generator
 'renderer trep-renderer)

;; Define the report.
(gnc:define-report
 'version 1
 'name reportname
 'report-guid trep-guid
 'options-generator trep-options-generator
 'renderer trep-renderer)
