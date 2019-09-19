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
(use-modules (gnucash report standard-reports trial-balance))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")
(define uuid "216cd0cf6931453ebcce85415aba7082")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "trial-balance")
  (test-trial-balance)
  (test-end "trial-balance"))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-trial-balance" test-title))

(define (test-trial-balance)
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

    (let ((sxml (options->sxml options "current")))
      (test-equal "current table has 22 cells"
        22
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "current accounts"
        '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities"
          "Income" "Income-GBP" "Expenses" "Equity" "Unrealized Losses")
        (sxml->table-row-col sxml 1 #f 1))

      (test-equal "current debits"
        '("$2,186.00" "$912.00" "$20.00" "$120.00" "$1.00" "$3,239.00")
        (sxml->table-row-col sxml 1 #f 2))

      (test-equal "current credits = $401"
        '("$3.00" "$73.00" "$918.00" "$2,245.00" "$3,239.00")
        (sxml->table-row-col sxml 1 #f 3)))

    (set-option options "General" "Start of Adjusting/Closing"
                (cons 'absolute (gnc-dmy2time64 01 01 2000)))
    (set-option options "General" "Report variation" 'pre-adj)
    (let ((sxml (options->sxml options "pre-adj")))
      (test-equal "pre-adj table has 22 cells"
        22
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "pre-adj accounts"
        '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities"
          "Income" "Income-GBP" "Expenses" "Equity" "Unrealized Losses")
        (sxml->table-row-col sxml 1 #f 1))

      (test-equal "pre-adj debits"
        ' ("$2,186.00" "$912.00" "$20.00" "$264.00" "$111.00" "$1.00" "$3,494.00")
        (sxml->table-row-col sxml 1 #f 2))

      (test-equal "pre-adj credits = $401"
        '("$3.00" "$2,573.00" "$918.00" "$3,494.00")
        (sxml->table-row-col sxml 1 #f 3)))

    (set-option options "General" "Report variation" 'work-sheet)
    (let ((sxml (options->sxml options "work-sheet")))
      (test-equal "work-sheet table has 58 cells"
        58
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "work-sheet accounts"
        '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities" "Income"
          "Income-GBP" "Expenses" "Equity" "Unrealized Losses" "Net Income")
        (sxml->table-row-col sxml 1 #f 1))

      (test-equal "work-sheet tb debits"
        '("$2,186.00" "$912.00" "$20.00" "$264.00" "$111.00" "$1.00" "$3,494.00")
        (sxml->table-row-col sxml 1 #f 2))

      (test-equal "work-sheet tb credits"
        '("$3.00" "$2,573.00" "$918.00" "$3,494.00")
        (sxml->table-row-col sxml 1 #f 3))

      (test-equal "work-sheet adj debits"
        '("$2,500.00" "$2,500.00" "$760.00")
        (sxml->table-row-col sxml 1 #f 4))

      (test-equal "work-sheet adj credits"
        '("$33.00" "$2,467.00" "$2,500.00")
        (sxml->table-row-col sxml 1 #f 5))

      (test-equal "work-sheet atb debits"
        '("$2,186.00" "$912.00" "$20.00" "$231.00" "$1.00" "$3,350.00")
        (sxml->table-row-col sxml 1 #f 6))

      (test-equal "work-sheet atb credits"
        '("$3.00" "$73.00" "$918.00" "$2,356.00" "$3,350.00")
        (sxml->table-row-col sxml 1 #f 7))

      (test-equal "work-sheet is debits"
        '("$231.00" "$231.00" "$991.00")
        (sxml->table-row-col sxml 1 #f 8))

      (test-equal "work-sheet is credits"
        '("$73.00" "$918.00" "$991.00" "$991.00")
        (sxml->table-row-col sxml 1 #f 9))

      (test-equal "work-sheet bs debits"
        '("$2,186.00" "$912.00" "$20.00" "$1.00" "$3,119.00" "$3,119.00")
        (sxml->table-row-col sxml 1 #f 10))

      (test-equal "work-sheet bs credits"
        '("$3.00" "$2,356.00" "$2,359.00" "$760.00" "$3,119.00")
        (sxml->table-row-col sxml 1 #f 11)))

    ;; A couple of transactions which involve foreign currency
    ;; conversions. We'll set the currencies to GBP and USD.
    (env-transfer-foreign #f 15 01 2000 gbp-bank usd-bank
                          10 14 #:description "GBP 10 to USD 14")
    (env-transfer-foreign #f 15 02 2000 usd-bank gbp-bank
                          9  8 #:description "USD 9 to GBP 8")

    (set-option options "General" "Report variation" 'current)
    (let ((sxml (options->sxml options "test-unrealized-gain")))
      (test-equal "unrealized losses"
        '("Unrealized Gains" "$3.25")
        (sxml->table-row-col sxml 1 -2 #f)))))
