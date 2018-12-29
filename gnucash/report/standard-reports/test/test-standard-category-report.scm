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
(use-modules (gnucash engine test srfi64-extras))

;; Guile 2 needs to load external modules at compile time
;; otherwise the N_ syntax-rule won't be found at compile time
;; causing the test to fail
;; That's what the wrapper below is meant for:

(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))

(use-modules (gnucash utilities)) 
(use-modules (gnucash report report-system))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash report standard-reports net-charts))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report standard-reports category-barchart))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (run-category-income-expense-test category-barchart-income-uuid category-barchart-expense-uuid)
  (run-category-asset-liability-test category-barchart-asset-uuid category-barchart-liability-uuid))

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
  (multi-acct-test expense-report-uuid))

(define (run-category-asset-liability-test asset-report-uuid liability-report-uuid)
  (null-test asset-report-uuid)
  (null-test liability-report-uuid)
  (asset-test asset-report-uuid)
  (liability-test liability-report-uuid))

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
      (test-end "single-txn-test"))))

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
