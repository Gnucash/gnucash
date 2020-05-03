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

(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report standard-reports net-charts))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "standard-net-linechart")
  (run-net-asset-test net-worth-linechart-uuid)
  (test-end "standard-net-linechart"))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (run-net-asset-test asset-report-uuid)
  (null-test asset-report-uuid)
  (single-txn-test asset-report-uuid)
  (two-txn-test asset-report-uuid)
  (two-txn-test-2 asset-report-uuid))

;; Just prove that the report exists.
(define (null-test uuid)
  (let ((options (gnc:make-report-options uuid)))
    (gnc:options->render uuid options "test-standard-net-linechart" "null-test")))

(define (single-txn-test uuid)
  (let* ((options (gnc:make-report-options uuid))
         (env (create-test-env))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
                                                    (gnc-default-report-currency)))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
                                                      (gnc-default-report-currency)))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
                                                     (gnc-default-report-currency))))
    (env-create-transaction env
                            (gnc:get-start-this-month)
                            my-income-account
                            my-asset-account
                            -1/1)
    (set-option options gnc:pagename-display "Show table" #t)
    (set-option options gnc:pagename-general "Start Date"
                (cons 'absolute (gnc:get-start-this-month)))
    (set-option options gnc:pagename-general "End Date"
                (cons 'absolute (gnc:get-start-this-month)))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list my-asset-account))

    (let ((sxml (gnc:options->sxml uuid options "test-standard-net-linechart"
                                   "single-txn-test" #:strip-tag "script")))
      (test-begin "single-txn-test")
      (test-equal "assets $1.00"
        '("$1.00")
        (sxml->table-row-col sxml 1 1 2))
      (test-equal "liability $0.00"
        '("$0.00")
        (sxml->table-row-col sxml 1 1 3))
      (test-equal "net $0.00"
        '("$1.00")
        (sxml->table-row-col sxml 1 1 4))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-end "single-txn-test"))))


(define (two-txn-test uuid)
  (let* ((options (gnc:make-report-options uuid))
         (env (create-test-env))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
                                                    (gnc-default-report-currency)))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
                                                      (gnc-default-report-currency)))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
                                                     (gnc-default-report-currency)))
         (date-0 (gnc:get-start-this-month))
         (date-1 (gnc:time64-next-day date-0))
         (date-2 (gnc:time64-next-day date-1)))
    (env-create-transaction env
                            date-1
                            my-income-account
                            my-asset-account
                            -1/1)
    (env-create-transaction env
                            date-2
                            my-income-account
                            my-asset-account
                            -5/1)

    (set-option options gnc:pagename-display "Show table" #t)
    (set-option options gnc:pagename-general "Start Date" (cons 'absolute date-0))
    (set-option options gnc:pagename-general "End Date" (cons 'absolute date-2))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list my-asset-account))

    (let ((sxml (gnc:options->sxml uuid options "test-standard-net-linechart"
                                   "two-txn-test" #:strip-tag "script")))
      (test-begin "two-txn-test")
      (test-equal "asset $0.00"
        '("$0.00")
        (sxml->table-row-col sxml 1 1 2))
      (test-equal "asset $1.00"
        '("$1.00")
        (sxml->table-row-col sxml 1 2 2))
      (test-equal "asset $6.00"
        '("$6.00")
        (sxml->table-row-col sxml 1 3 2))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-equal "3 rows"
        3
        (length (sxml->table-row-col sxml 1 #f 1)))
      (test-end "two-txn-test")
      sxml)))


(define (two-txn-test-2 uuid)
  (let* ((options (gnc:make-report-options uuid))
         (env (create-test-env))
         (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
                                                    (gnc-default-report-currency)))
         (my-liability-account (env-create-root-account env ACCT-TYPE-LIABILITY
                                                        (gnc-default-report-currency)))
         (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
                                                      (gnc-default-report-currency)))
         (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
                                                     (gnc-default-report-currency)))
         (date-0 (gnc:get-start-this-month))
         (date-1 (gnc:time64-next-day date-0))
         (date-2 (gnc:time64-next-day date-1)))
    (env-create-transaction env date-1 my-income-account my-asset-account -1/1)
    (env-create-transaction env date-1 my-expense-account my-liability-account -1/1)
    (env-create-transaction env date-2 my-income-account my-asset-account -5/1)
    (env-create-transaction env date-2 my-expense-account my-liability-account -5/1)

    (set-option options gnc:pagename-display "Show table" #t)
    (set-option options gnc:pagename-general "Start Date" (cons 'absolute date-0))
    (set-option options gnc:pagename-general "End Date" (cons 'absolute date-2))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list my-asset-account my-liability-account))

    (let ((sxml (gnc:options->sxml uuid options "test-standard-net-linechart"
                                   "two-txn-test-2" #:strip-tag "script")))
      (test-begin "two-txn-test-2")
      (test-equal "asset $0.00"
        '("$0.00")
        (sxml->table-row-col sxml 1 1 2))
      (test-equal "asset $1.00"
        '("$1.00")
        (sxml->table-row-col sxml 1 2 2))
      (test-equal "asset $6.00"
        '("$6.00")
        (sxml->table-row-col sxml 1 3 2))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-equal "3 rows"
        3
        (length (sxml->table-row-col sxml 1 #f 1)))
      (test-end "two-txn-test-2"))))

