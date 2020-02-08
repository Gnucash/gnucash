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
(use-modules (gnucash report standard-reports equity-statement))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")
(define uuid "c2a996c8970f43448654ca84f17dda24")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "equity-statement")
  (test-equity-statement)
  (test-end "equity-statement"))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-equity-statement" test-title))

(define (test-equity-statement)
  (let* ((options (gnc:make-report-options uuid))
         (account-alist (create-test-data))
         (gbp-bank (assoc-ref account-alist "GBP Bank"))
         (usd-bank (assoc-ref account-alist "Bank"))
         (expense (assoc-ref account-alist "Expenses"))
         (equity (assoc-ref account-alist "Equity"))
         (income (assoc-ref account-alist "Income"))
         (bank (assoc-ref account-alist "Bank")))

    (gnc-commodity-set-user-symbol
     (xaccAccountGetCommodity gbp-bank)
     "#")

    (let ((closing-txn (env-transfer #f 30 06 2003 expense equity
                                     111 #:description "Closing Entries")))
      (xaccTransSetIsClosingTxn closing-txn #t))

    (env-transfer #f 01 06 2003 expense equity
                  33 #:description "Adjusting Entries")
    (env-transfer #f 01 07 2003 income equity
                  -2500 #:description "Adjusting Entries")

    (set-option options "General" "Start Date"
                (cons 'absolute (gnc-dmy2time64 01 01 1970)))
    (set-option options "General" "End Date"
                (cons 'absolute (gnc-dmy2time64 01 01 2005)))
    (let ((sxml (options->sxml options "current")))
      (test-equal "current table has 22 cells"
        14
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "capital"
        '("Capital, 01/01/70" "$29.00")
        (sxml->table-row-col sxml 1 2 #f))

      (test-equal "income"
        '("Net income for Period" "$620.00")
        (sxml->table-row-col sxml 1 3 #f))

      (test-equal "investments"
        '("Investments for Period" "$2,722.00")
        (sxml->table-row-col sxml 1 4 #f))

      (test-equal "withdrawals"
        '("Withdrawals for Period" "$255.00")
        (sxml->table-row-col sxml 1 5 #f))

      (test-equal "unrealized"
        '("Unrealized Losses" "$1.00")
        (sxml->table-row-col sxml 1 6 #f))

      (test-equal "inc/dec in capital"
        '("Increase in capital" "$3,086.00")
        (sxml->table-row-col sxml 1 7 #f))

      (test-equal "capital end"
        '("Capital, 01/01/05" "$3,115.00")
        (sxml->table-row-col sxml 1 8 #f)))))
