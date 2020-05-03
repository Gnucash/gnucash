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

(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))

(use-modules (gnucash utilities))
(use-modules (gnucash report report-system))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash report standard-reports budget))
(use-modules (gnucash report standard-reports budget-income-statement))
(use-modules (gnucash report standard-reports budget-balance-sheet))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))
(use-modules (sxml xpath))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")
(define budget-uuid "810ed4b25ef0486ea43bbd3dddb32b11")
(define budget-is-uuid "583c313fcc484efc974c4c844404f454")
(define budget-bs-uuid "ecc35ea9dbfa4e20ba389fc85d59cb69")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "budget")
  (test-group-with-cleanup "budget.scm"
    (test-budget)
    (teardown))
  (test-group-with-cleanup "budget-income-statement.scm"
    (test-budget-income-statement)
    (teardown))
  (test-group-with-cleanup "budget-balance-sheet.scm"
    (test-budget-balance-sheet)
    (teardown))
  (test-end "budget"))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options uuid test-title)
  (gnc:options->sxml uuid options "test-budget" test-title))

(define (test-budget)
  (let* ((env (create-test-env))
         (account-alist (create-test-data))
         (budget (gnc:create-budget-and-transactions env account-alist))
         (options (gnc:make-report-options budget-uuid))
         (bank (cdr (assoc "Bank" account-alist))))

    (display "\nbudget.scm\n")
    (set-option options "Accounts" "Account Display Depth" 'all)

    (set-option options "Display" "Show Difference" #f)
    (set-option options "Display" "Show Budget" #f)
    (set-option options "Display" "Show Actual" #f)
    (let ((sxml (options->sxml options budget-uuid "basic all display off")))
      (test-equal "all display OFF, table has 15 cells"
        15
        (length (sxml->table-row-col sxml 1 #f #f))))

    (set-option options "Display" "Show Difference" #t)
    (set-option options "Display" "Show Budget" #t)
    (set-option options "Display" "Show Actual" #t)
    (set-option options "Display" "Show Column with Totals" #t)
    (let ((sxml (options->sxml options budget-uuid "basic")))
      (test-equal "all display ON, table has 226 cells"
        226
        (length (sxml->table-row-col sxml 1 #f #f)))
      (test-equal "bank"
        '("Bank" "$20.00" "$35.00" "-$15.00" "$40.00" "-$20.00" "$60.00"
          "." "$67.00" "-$67.00" "$60.00" "$77.00" "-$17.00" "." "$0.00"
          "." "." "$0.00" "." "$120.00" "$159.00" "-$39.00")
        (sxml->table-row-col sxml 1 5 #f))
      (test-equal "income"
        '("Income" "-$55.00" "-$55.00" "$0.00" "." "$0.00" "." "-$65.00"
          "-$67.00" "-$2.00" "-$75.00" "-$77.00" "-$2.00" "." "$0.00" "."
          "." "$0.00" "." "-$195.00" "-$199.00" "-$4.00")
        (sxml->table-row-col sxml 1 9 #f))
      (test-equal "expense"
        '("Expenses" "." "$20.00" "-$20.00" "$30.00" "$20.00" "$10.00"
          "$20.00" "$0.00" "$20.00" "$40.00" "$0.00" "$40.00" "." "$0.00"
          "." "." "$0.00" "." "$90.00" "$40.00" "$50.00")
        (sxml->table-row-col sxml 1 11 #f)))

    (set-option options "General" "Report for range of budget periods" #t)
    (set-option options "General" "Range start" 'current)
    (set-option options "General" "Range end" 'next)
    (let ((sxml (options->sxml options budget-uuid "only next period")))
      (test-equal "only next period - 133 cells"
        133
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "only next period- bank"
        '("Bank" "$20.00" "$35.00" "-$15.00" "$40.00" "-$20.00" "$60.00"
          "$60.00" "$144.00" "-$84.00" "$120.00" "$159.00" "-$39.00")
        (sxml->table-row-col sxml 1 5 #f)))

    (set-option options "General" "Range start" 'last)
    (set-option options "General" "Range end" 'last)
    (let ((sxml (options->sxml options budget-uuid "only last period")))
      (test-equal "only last period - 102 cells"
        102
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "only last period - bank"
        '("Bank" "$120.00" "$159.00" "-$39.00" "." "$0.00" "."
          "$120.00" "$159.00" "-$39.00")
        (sxml->table-row-col sxml 1 5 #f)))

    (set-option options "General" "Range start" 'manual)
    (set-option options "General" "Range end" 'manual)
    (set-option options "General" "Exact start period" 2)
    (set-option options "General" "Exact end period" 4)
    (set-option options "General" "Include collapsed periods before selected." #f)
    (set-option options "General" "Include collapsed periods after selected." #f)
    (let ((sxml (options->sxml options budget-uuid "exact periods")))
      (test-equal "exact periods - 133 cells"
        133
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "exact periods - bank"
        '("Bank" "$40.00" "-$20.00" "$60.00" "." "$67.00" "-$67.00"
          "$60.00" "$77.00" "-$17.00" "$100.00" "$124.00" "-$24.00")
        (sxml->table-row-col sxml 1 5 #f)))

    (set-option options "General" "Use accumulated amounts" #t)
    (let ((sxml (options->sxml options budget-uuid "Use accumulated amounts")))
      (test-equal "use accumulated amounts"
        '("Bank" "$60.00" "$15.00" "$45.00" "$60.00" "$82.00" "-$22.00"
          "$120.00" "$159.00" "-$39.00" "$120.00" "$159.00" "-$39.00")
        (sxml->table-row-col sxml 1 5 #f)))
    ))

(define (test-budget-income-statement)
  (let* ((env (create-test-env))
         (account-alist (create-test-data))
         (budget (gnc:create-budget-and-transactions env account-alist))
         (options (gnc:make-report-options budget-is-uuid))
         (bank (assoc-ref account-alist "Bank")))

    (display "\nbudget-income-statement.scm\n")
    (let ((sxml (options->sxml options budget-is-uuid "budget-is-basic")))
      (test-equal "basic test"
        72
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "budgeted income amounts"
        '("$195.00" "Income")
        ((sxpath '(// table // (tr 1) // table // (tr 3) // *text*))
         sxml))

      (test-equal "net loss for budget"
        '("Net loss for Budget test budget" "$285.00")
        ((sxpath '(// table // (tr 2) // table // (tr 5) // *text*))
         sxml)))))

(define (test-budget-balance-sheet)
  (let* ((env (create-test-env))
         (account-alist (create-test-data))
         (budget (gnc:create-budget-and-transactions env account-alist))
         (options (gnc:make-report-options budget-bs-uuid))
         (bank (assoc-ref account-alist "Bank")))

    (display "\nbudget-balance-sheet.scm\n")
    (let ((sxml (options->sxml options budget-bs-uuid "budget-bs-basic")))
      (test-equal "basic test"
        52
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "existing assets"
        '("Existing Assets" "$3,118.00")
        (sxml->table-row-col sxml 1 8 #f))

      (test-equal "allocated assets"
        '("Allocated Assets" "$120.00")
        (sxml->table-row-col sxml 1 9 #f))

      (test-equal "unallocated assets"
        '("Unallocated Assets" "-$405.00")
        (sxml->table-row-col sxml 1 10 #f))

      (test-equal "total assets"
        '("Total Assets" "$2,833.00")
        (sxml->table-row-col sxml 1 11 #f))

      (test-equal "existing liab"
        '("Existing Liabilities" "$3.00")
        (sxml->table-row-col sxml 1 16 #f))

      (test-equal "new liab"
        '("New Liabilities" "$0.00")
        (sxml->table-row-col sxml 1 17 #f))

      (test-equal "total liab"
        '("Total Liabilities" "$3.00")
        (sxml->table-row-col sxml 1 18 #f))

      (test-equal "retained earnings"
        '("Existing Retained Earnings" "$3,227.00")
        (sxml->table-row-col sxml 1 22 #f))

      (test-equal "retained losses"
        '("New Retained Losses" "$285.00")
        (sxml->table-row-col sxml 1 23 #f))

      (test-equal "unrealized losses"
        '("Unrealized Losses" "$1.00")
        (sxml->table-row-col sxml 1 24 #f))

      (test-equal "existing equity"
        '("Existing Equity" "$3,115.00")
        (sxml->table-row-col sxml 1 25 #f))

      (test-equal "new equity"
        '("New Equity" "-$285.00")
        (sxml->table-row-col sxml 1 26 #f))

      (test-equal "total equity"
        '("Total Equity" "$2,830.00")
        (sxml->table-row-col sxml 1 27 #f))

      (test-equal "total liab and equity"
        '("Total Liabilities & Equity" "$2,833.00")
        (sxml->table-row-col sxml 1 29 #f)))))

