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

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report standard-reports cashflow-barchart))
(use-modules (gnucash report stylesheets))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-in-txn)
  (test-out-txn)
  (test-null-txn))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (str->num str)
  (string->number
   (string-filter
    (lambda (c)
      (or (char-numeric? c)
          (memv c '(#\- #\.))))
    str)))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank")
              (list "Wallet"))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))))

;; Test two transactions from income to two different assets in two different days
(define (test-in-txn)
  (let* ((options (gnc:make-report-options cashflow-barchart-uuid))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank-account (cdr (assoc "Bank" account-alist)))
         (wallet-account (cdr (assoc "Wallet" account-alist)))
         (expense-account (cdr (assoc "Expenses" account-alist)))
         (income-account (cdr (assoc "Income" account-alist)))
         (date-0 (gnc:get-start-this-month))
         (date-1 (gnc:time64-next-day date-0))
         (date-2 (gnc:time64-next-day date-1)))
    (env-create-transaction env date-1 bank-account income-account 1)
    (env-create-transaction env date-2 wallet-account income-account 5)
    (set-option options gnc:pagename-display "Show Table" #t)
    (set-option options gnc:pagename-general "Start Date" (cons 'absolute date-0))
    (set-option options gnc:pagename-general "End Date" (cons 'absolute date-2))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list wallet-account bank-account))

    (let ((sxml (gnc:options->sxml cashflow-barchart-uuid options "test-cashflow-barchart"
                                   "test-in-txn" #:strip-tag "script")))
      (test-begin "test-in-txn")
      (test-assert "in = net, out=0"
        (every (lambda (in out net)
                 (and (= in net) (zero? out)))
               (map str->num (sxml->table-row-col sxml 1 #f 2))
               (map str->num (sxml->table-row-col sxml 1 #f 3))
               (map str->num (sxml->table-row-col sxml 1 #f 4))))
      (test-equal "day in"
        '(0.0 1.0 5.0 6.0)
        (map str->num (sxml->table-row-col sxml 1 #f 2)))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-equal "4 rows"
        4
        (length (sxml->table-row-col sxml 1 #f 1)))
      (test-end "test-in-txn"))))

;; Test two transactions from two different assets to expense in two different days
(define (test-out-txn)
  (let* ((options (gnc:make-report-options cashflow-barchart-uuid))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank-account (cdr (assoc "Bank" account-alist)))
         (wallet-account (cdr (assoc "Wallet" account-alist)))
         (expense-account (cdr (assoc "Expenses" account-alist)))
         (income-account (cdr (assoc "Income" account-alist)))
         (date-0 (gnc:get-start-this-month))
         (date-1 (gnc:time64-next-day date-0))
         (date-2 (gnc:time64-next-day date-1)))
    ;; large in txn to avoid negative net (hard to parse):
    (env-create-transaction env date-1 bank-account income-account 100)
    (env-create-transaction env date-1 expense-account bank-account 1)
    ;; large in txn to avoid negative net (hard to parse):
    (env-create-transaction env date-2 wallet-account income-account 100)
    (env-create-transaction env date-2 expense-account wallet-account 5)

    (set-option options gnc:pagename-display "Show Table" #t)
    (set-option options gnc:pagename-general "Start Date" (cons 'absolute date-0))
    (set-option options gnc:pagename-general "End Date" (cons 'absolute date-2))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list wallet-account bank-account))

    (let ((sxml (gnc:options->sxml cashflow-barchart-uuid options "test-cashflow-barchart"
                                   "test-out-txn" #:strip-tag "script")))
      (test-begin "test-out-txn")
      (test-assert "in - out = net"
        (every (lambda (in out net)
                 (= (- in out) net))
               (map str->num (sxml->table-row-col sxml 1 #f 2))
               (map str->num (sxml->table-row-col sxml 1 #f 3))
               (map str->num (sxml->table-row-col sxml 1 #f 4))))
      (test-equal "money out"
        '(0.0 1.0 5.0 6.0)
        (map str->num (sxml->table-row-col sxml 1 #f 3)))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-equal "4 rows"
        4
        (length (sxml->table-row-col sxml 1 #f 1)))
      (test-end "test-out-txn"))))


;; Test null transaction (transaction between assets)
;; This test is identical to test-in-txn but with an extra transaction between assets
(define (test-null-txn)
  (let* ((options (gnc:make-report-options cashflow-barchart-uuid))
         (env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank-account (cdr (assoc "Bank" account-alist)))
         (wallet-account (cdr (assoc "Wallet" account-alist)))
         (expense-account (cdr (assoc "Expenses" account-alist)))
         (income-account (cdr (assoc "Income" account-alist)))
         (date-0 (gnc:get-start-this-month))
         (date-1 (gnc:time64-next-day date-0))
         (date-2 (gnc:time64-next-day date-1)))
    (env-create-transaction env date-1 bank-account income-account 1)
    ;; the following transaction should not be counted
    (env-create-transaction env date-1 bank-account wallet-account 20)
    (env-create-transaction env date-2 wallet-account income-account 5)

    (set-option options gnc:pagename-display "Show Table" #t)
    (set-option options gnc:pagename-general "Start Date" (cons 'absolute date-0))
    (set-option options gnc:pagename-general "End Date" (cons 'absolute date-2))
    (set-option options gnc:pagename-general "Step Size" 'DayDelta)
    (set-option options gnc:pagename-general "Price Source" 'pricedb-nearest)
    (set-option options gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
    (set-option options gnc:pagename-accounts "Accounts" (list wallet-account bank-account))

    (let ((sxml (gnc:options->sxml cashflow-barchart-uuid options "test-cashflow-barchart"
                                   "test-null-txn" #:strip-tag "script")))
      (test-begin "test-null-txn")
      (test-assert "in = net, out=0"
        (every (lambda (in out net)
                 (and (= in net) (zero? out)))
               (map str->num (sxml->table-row-col sxml 1 #f 2))
               (map str->num (sxml->table-row-col sxml 1 #f 3))
               (map str->num (sxml->table-row-col sxml 1 #f 4))))
      (test-equal "day in"
        '(0.0 1.0 5.0 6.0)
        (map str->num (sxml->table-row-col sxml 1 #f 2)))
      (test-equal "4 columns"
        4
        (length (sxml->table-row-col sxml 1 1 #f)))
      (test-equal "4 rows"
        4
        (length (sxml->table-row-col sxml 1 #f 1)))
      (test-end "test-null-txn"))))
