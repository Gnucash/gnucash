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

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-14))
(use-modules (srfi srfi-64))
(use-modules (gnucash gnc-module))
(use-modules (tests srfi64-extras))

;; Guile 2 needs to load external modules at compile time
;; otherwise the N_ syntax-rule won't be found at compile time
;; causing the test to fail
;; That's what the wrapper below is meant for:

(gnc:module-begin-syntax (gnc:module-load "gnucash/report" 0))

(use-modules (gnucash utilities)) 
(use-modules (gnucash report))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash reports standard net-charts))
(use-modules (tests test-report-extras))
(use-modules (gnucash reports standard category-barchart))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (tests test-engine-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "standard-category-report")
  (run-category-income-expense-test category-barchart-income-uuid category-barchart-expense-uuid)
  (run-category-asset-liability-test category-barchart-asset-uuid category-barchart-liability-uuid)
  (test-end "standard-category-report"))

(export run-category-income-expense-test)
(export run-category-asset-liability-test)

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (str->num str)
  (string->number
   (string-filter
    (lambda (c)
      (or (char-numeric? c)
          (memv c '(#\- #\.))))
    str)))

(define (run-category-income-expense-test income-report-uuid expense-report-uuid)
  (null-test income-report-uuid)
  (null-test expense-report-uuid)
  (single-txn-test income-report-uuid)
  (single-txn-test-average income-report-uuid)
  (multi-acct-test expense-report-uuid))

(define (run-category-asset-liability-test asset-report-uuid liability-report-uuid)
  (null-test asset-report-uuid)
  (null-test liability-report-uuid)
  (asset-test asset-report-uuid)
  (liability-test liability-report-uuid))

(define (teardown)
  (gnc-clear-current-session))

;; No real test here, just confirm that no exceptions are thrown
(define (null-test uuid)
  (let ((options (gnc:make-report-options uuid)))
    (gnc:options->render uuid options "test-standard-category-report" "null-test")))

(define (single-txn-test uuid)
  (let* ((income-options (gnc:make-report-options uuid))
         (env (create-test-env))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
                                                    (gnc-default-report-currency)))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
                                                      (gnc-default-report-currency)))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
                                                     (gnc-default-report-currency))))
    (env-create-daily-transactions env
                                   (gnc:get-start-this-month)
                                   (gnc:get-end-this-month)
                                   my-asset-account my-income-account)
    (set-option income-options gnc:pagename-display "Show table" #t)
    (set-option income-options gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
    (set-option income-options gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
    (set-option income-options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option income-options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option income-options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option income-options gnc:pagename-accounts "Accounts" (list my-income-account))
    (set-option income-options gnc:pagename-accounts "Show Accounts until level"  'all)

    (let ((sxml (gnc:options->sxml uuid income-options "test-standard-category-report"
                                   "single-txn-test" #:strip-tag "script")))
      (test-begin "single-txn-test")
      (test-assert "day=value"
        (every =
               (map
                (lambda (s)
                  (str->num (cadr (string-split s #\/))))
                (sxml->table-row-col sxml 1 #f 1))
               (map str->num (sxml->table-row-col sxml 1 #f 2))))
      (test-end "single-txn-test"))
    (teardown)))

(define (single-txn-test-average uuid)
  (let* ((income-options (gnc:make-report-options uuid))
         (env (create-test-env))
         (curr (gnc-default-report-currency))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET curr))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE curr))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME curr)))
    ;; create 52 weekly txns from 1.1.1980, amount $1.10 increase by $1.10 weekly
    (let loop ((date (gnc-dmy2time64 1 1 1980))
               (amt 11/10)
               (remaining 52))
      (unless (zero? remaining)
        (env-create-transaction env date my-asset-account my-income-account amt)
        (loop (incdate date WeekDelta)
              (+ amt 11/10)
              (1- remaining))))
    ;; and a $22.40 txn on 1.7.1980 just to throw the averages off
    (env-create-transaction env (gnc-dmy2time64 1 7 1980)
                            my-asset-account my-income-account 224/10)
    (set-option income-options gnc:pagename-display "Show table" #t)
    (set-option income-options gnc:pagename-general "Start Date"
                (cons 'absolute (gnc-dmy2time64 1 1 1980)))
    (set-option income-options gnc:pagename-general "End Date"
                (cons 'absolute (gnc-dmy2time64 31 12 1980)))
    (set-option income-options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option income-options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option income-options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option income-options gnc:pagename-accounts "Accounts" (list my-income-account))
    (set-option income-options gnc:pagename-accounts "Show Accounts until level"  'all)

    (test-begin "multiplier test")
    (set-option income-options gnc:pagename-general "Show Average" 'WeekDelta)
    (set-option income-options gnc:pagename-general "Step Size" 'MonthDelta)
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-average-week"
                                   #:strip-tag "script")))
      (test-equal "monthly chart, weekly average"
        '("$3.79" "$7.57" "$11.61" "$20.20" "$20.70" "$24.74"
          "$41.75" "$33.83" "$47.97" "$42.92" "$46.96" "$51.00")
        (sxml->table-row-col sxml 1 #f 2)))
    (set-option income-options gnc:pagename-general "Show Average" 'MonthDelta)
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-average-month"
                                   #:strip-tag "script")))
      (test-equal "monthly chart, monthly average"
        '("$16.50" "$33.00" "$50.60" "$88.00" "$90.20" "$107.80"
          "$181.90" "$147.40" "$209.00" "$187.00" "$204.60" "$222.20")
        (sxml->table-row-col sxml 1 #f 2)))
    (set-option income-options gnc:pagename-general "Show Average" 'DayDelta)
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-average-day"
                                   #:strip-tag "script")))
      (test-equal "monthly chart, daily average"
        '("$0.54" "$1.08" "$1.66" "$2.89" "$2.96" "$3.53"
          "$5.96" "$4.83" "$6.85" "$6.13" "$6.71" "$7.29")
        (sxml->table-row-col sxml 1 #f 2)))
    (set-option income-options gnc:pagename-general "Step Size" 'WeekDelta)
    (set-option income-options gnc:pagename-general "Show Average" 'DayDelta)
    (set-option income-options gnc:pagename-general "Start Date"
                (cons 'absolute (gnc-dmy2time64 1 6 1980)))
    (set-option income-options gnc:pagename-general "End Date"
                (cons 'absolute (gnc-dmy2time64 1 8 1980)))
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-weekly-average-day"
                                   #:strip-tag "script")))
      (test-equal "weekly chart, daily average"
        '("$3.61" "$3.77" "$3.93" "$4.09" "$7.44" "$4.40" "$4.56" "$4.71" "$4.87")
        (sxml->table-row-col sxml 1 #f 2)))
    (set-option income-options gnc:pagename-general "Show Average" 'WeekDelta)
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-weekly-average-week"
                                   #:strip-tag "script")))
      (test-equal "weekly chart, weekly average"
        '("$25.30" "$26.40" "$27.50" "$28.60"
          "$52.10" "$30.80" "$31.90" "$33.00" "$34.10")
        (sxml->table-row-col sxml 1 #f 2)))
    (set-option income-options gnc:pagename-general "Show Average" 'MonthDelta)
    (let ((sxml (gnc:options->sxml uuid income-options
                                   "test-standard-category-report"
                                   "single-txn-test-weekly-average-month"
                                   #:strip-tag "script")))
      (test-equal "weekly chart, monthly average"
        '("$25.30" "$26.40" "$27.50" "$28.60"
          "$52.10" "$30.80" "$31.90" "$33.00" "$34.10")
        (sxml->table-row-col sxml 1 #f 2)))
    (test-end "multiplier test"))
  (teardown))

(define (list-leaves list)
  (if (not (pair? list))
      (cons list '())
      (fold (lambda (next acc)
	      (append (list-leaves next)
		      acc))
	    '()
	    list)))

(define (multi-acct-test expense-report-uuid)
  (let* ((expense-options (gnc:make-report-options expense-report-uuid))
         (env (create-test-env))
         (expense-accounts (env-expense-account-structure env))
         (asset-accounts (env-create-account-structure
                          env
                          (list "Assets"
                                (list (cons 'type ACCT-TYPE-ASSET))
                                (list "Bank"))))
         (leaf-expense-accounts (list-leaves expense-accounts))
         (bank-account (car (car (cdr asset-accounts)))))
    (for-each (lambda (expense-account)
                (env-create-daily-transactions env
                                               (gnc:get-start-this-month)
                                               (gnc:get-end-this-month)
                                               expense-account
                                               bank-account))
              leaf-expense-accounts)
    (set-option expense-options gnc:pagename-display "Show table" #t)
    (set-option expense-options gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
    (set-option expense-options gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
    (set-option expense-options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option expense-options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option expense-options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option expense-options gnc:pagename-accounts "Accounts" leaf-expense-accounts)
    (set-option expense-options gnc:pagename-accounts "Show Accounts until level" 2)
    (let ((sxml (gnc:options->sxml expense-report-uuid expense-options "test-standard-category-report"
                                   "multi--test" #:strip-tag "script")))
      (test-begin "multi-acct-test")
      (test-equal "6 columns"
        6
        (length (sxml->table-row-col sxml 1 0 #f)))
      (test-equal "date"
        '("Date")
        (sxml->table-row-col sxml 1 0 1))
      (test-equal "auto"
        '("Auto")
        (sxml->table-row-col sxml 1 0 2))
      (test-end "multi-acct-test"))))


(define (asset-test uuid)
  (let* ((asset-options (gnc:make-report-options uuid))
         (env (create-test-env))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
                                                    (gnc-default-report-currency)))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
                                                      (gnc-default-report-currency)))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
                                                     (gnc-default-report-currency))))
    (env-create-daily-transactions env
                                   (gnc:get-start-this-month)
                                   (gnc:get-end-this-month)
                                   my-asset-account my-income-account)
    (set-option asset-options gnc:pagename-display "Show table" #t)
      (set-option asset-options gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
      (set-option asset-options gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
      (set-option asset-options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option asset-options gnc:pagename-general "Price Source" 'pricedb-nearest)
      (set-option asset-options gnc:pagename-general "Price Source" 'pricedb-nearest)
      (set-option asset-options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
      (set-option asset-options gnc:pagename-accounts "Accounts" (list my-asset-account))
      (set-option asset-options gnc:pagename-accounts "Show Accounts until level"  'all)
      (let ((sxml (gnc:options->sxml uuid asset-options "test-standard-category-report"
                                     "asset-test" #:strip-tag "script")))
        (test-begin "asset-renderer")
        (test-equal "2 columns"
          2
          (length (sxml->table-row-col sxml 1 0 #f)))
        (test-equal "account-1"
          '("account-1")
          (sxml->table-row-col sxml 1 0 2))
        (test-equal "first row $1.00"
          '("$1.00")
          (sxml->table-row-col sxml 1 1 2))
        (test-equal "28th row $406.00"
          '("$406.00")
          (sxml->table-row-col sxml 1 28 2))
        (test-end "asset-renderer"))))

(define (liability-test uuid)
  (let* ((liability-options (gnc:make-report-options uuid))
         (env (create-test-env))
         (asset--acc (env-create-root-account env ACCT-TYPE-ASSET (gnc-default-report-currency)))
         (liabil-acc (env-create-root-account env ACCT-TYPE-CREDIT (gnc-default-report-currency)))
         (income-acc (env-create-root-account env ACCT-TYPE-INCOME (gnc-default-report-currency))))
    (env-create-transaction env (gnc-dmy2time64 01 10 2016) asset--acc liabil-acc 100) ;loan
    (env-create-transaction env (gnc-dmy2time64 01 01 2017) asset--acc income-acc 10)  ;salary#1
    (env-create-transaction env (gnc-dmy2time64 02 01 2017) liabil-acc asset--acc 9)   ;repay#1
    (env-create-transaction env (gnc-dmy2time64 01 02 2017) asset--acc income-acc 10)  ;salary#2
    (env-create-transaction env (gnc-dmy2time64 02 02 2017) liabil-acc asset--acc 9)   ;repay#2
    (env-create-transaction env (gnc-dmy2time64 01 03 2017) asset--acc income-acc 10)  ;salary#3
    (env-create-transaction env (gnc-dmy2time64 02 03 2017) liabil-acc asset--acc 9)   ;repay#3
    (env-create-transaction env (gnc-dmy2time64 01 04 2017) asset--acc income-acc 10)  ;salary#4
    (env-create-transaction env (gnc-dmy2time64 02 04 2017) liabil-acc asset--acc 9)   ;repay#4
    (env-create-transaction env (gnc-dmy2time64 01 05 2017) asset--acc income-acc 10)  ;salary#5
    (env-create-transaction env (gnc-dmy2time64 02 05 2017) liabil-acc asset--acc 9)   ;repay#5
    (set-option liability-options gnc:pagename-display "Show table" #t)
    (set-option liability-options gnc:pagename-general "Start Date" (cons 'absolute (gnc-dmy2time64 01 01 2017)))
    (set-option liability-options gnc:pagename-general "End Date" (cons 'absolute (gnc-dmy2time64 31 12 2018)))
    (set-option liability-options gnc:pagename-general "Step Size" 'MonthDelta)
    (set-option liability-options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option liability-options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option liability-options gnc:pagename-accounts "Accounts" (list liabil-acc))
    (set-option liability-options gnc:pagename-accounts "Show Accounts until level"  'all)

    (let ((sxml (gnc:options->sxml uuid liability-options "test-standard-category-report"
                                   "liability-test" #:strip-tag "script")))
      (test-begin "liability-renderer")
      (test-equal "2 columns"
        2
        (length (sxml->table-row-col sxml 1 0 #f)))
      (test-equal "account-2"
        '("account-2")
        (sxml->table-row-col sxml 1 0 2))
      (test-equal "first row $100.00"
        '("$100.00")
        (sxml->table-row-col sxml 1 1 2))
      (test-equal "last row $55.00"
        '("$55.00")
        (sxml->table-row-col sxml 1 -1 2))
      (test-end "liability-renderer"))))
