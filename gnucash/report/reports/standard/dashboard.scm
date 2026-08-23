;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dashboard.scm : dasboard style report based on Bill's welcome
;;                 to gnucash demo report
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
;; Copyright 2019 Geert Janssens <geert@kobaltwit.be>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard dashboard))
(export gnc:make-dashboard)
(export gnc:make-dashboard-v2)

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils)) ; for gnc:version and (G_ ...)
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define multicolumn-guid "d8ba4a2e89e8479ca9f6eccdeb164588")
(define acct-summary-guid "3298541c236b494998b236dfad6ad752")
(define exp-piechart-guid "9bf1892805cb4336be6320fe48ce5446")
(define inc-piechart-guid "e1bd09b8a1dd49dd85760db9d82b045c")
(define inc-exp-chart-guid "80769921e87943adade887b9835a7685")

(define (gnc:make-dashboard)
  (let* ((view (gnc:make-report multicolumn-guid))
         (sub-accounts (gnc:make-report acct-summary-guid))
         (sub-expense-pie (gnc:make-report exp-piechart-guid))
         (sub-income-pie (gnc:make-report inc-piechart-guid))
         (sub-bar (gnc:make-report inc-exp-chart-guid))
         (options (gnc:report-options (gnc-report-find view))))

    (define (set-option! section name value)
      (gnc-set-option (gnc:optiondb options) section name value))

    (set-option! "General" "Report name" (G_ "Dashboard"))
    (set-option! "General" "Number of columns" 2)

    ;; mark the reports as needing to be saved
    (gnc:report-set-needs-save?! (gnc-report-find sub-accounts) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-expense-pie) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-income-pie) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-bar) #t)

    (set-option! "__general" "report-list"
                 (list (list sub-expense-pie 1 1 #f)
                       (list sub-income-pie 1 1 #f)
                       (list sub-bar 2 1 #f)
                       (list sub-accounts 2 1 #f)))
    view))




;;----------------------------------------------------------------------------
;;
;; Dashboard v2.0 - Revamped
;;
;;----------------------------------------------------------------------------


(define multicolumn-guid "d8ba4a2e89e8479ca9f6eccdeb164588")
(define cash-flow-guid "f8748b813fab4220ba26e743aedf38da")
(define inc-exp-chart-guid "44f81bee049b4b3ea908f8dac9a9474e")
(define acct-summary-guid "3298541c236b494998b236dfad6ad752")
(define exp-piechart-guid "80769921e87943adade887b9835a7685")
(define inc-piechart-guid "5c7fd8a1fe9a4cd38884ff54214aa88a")
(define assets-piechart-guid "5c7fd8a1fe9a4cd38884ff54214aa88a")
(define money-piechart-guid "5c7fd8a1fe9a4cd38884ff54214aa88a")
(define transactions-guid "2fe3b9833af044abb929a88d5a59620f")


(define (set-option! option-db section name value)
      (gnc-set-option (gnc:optiondb option-db) section name value))

(define (get-cash-accounts)
    (let* ((book (gnc-get-current-book))
           (root (gnc-book-get-root-account book))
           (all-accounts (gnc-account-get-descendants root))
           (cash-accounts (filter
                            (lambda (acc) (or (= (xaccAccountGetType acc) ACCT-TYPE-BANK)
                                (= (xaccAccountGetType acc) ACCT-TYPE-CASH)))
                                    all-accounts)))
           cash-accounts
        )
)

(define (setup-cash-flow option-db)
    (set-option! option-db "General" "Show Full Account Names" #t)
    (set-option! option-db "General" "Show Selected Accounts" #f)
)

(define (setup-inc-exp-chart option-db name)
    (set-option! option-db "General" "Report name" (G_ name))
    (set-option! option-db "General" "Price Source" 'pricedb-nearest)
    (set-option! option-db "Display" "Show table" #t)
    (set-option! option-db "Display" "Plot Width" '(pixels . 500))
    (set-option! option-db "Display" "Plot Height" '(pixels . 360))
    (set-option! option-db "tooltips" "mode" 'index)
)

(define (setup-pie-chart option-db name cashOnly)
    (set-option! option-db "General" "Report name" (G_ name))
    (set-option! option-db "Display" "Plot Width" '(pixels . 600))
    (set-option! option-db "Display" "Plot Height" '(pixels . 300))
    (set-option! option-db "Display" "Show Percents" #t)
    (set-option! option-db "Display" "Show Totals" #t)
    (set-option! option-db "Display" "Maximum Slices" 10)
    (set-option! option-db "Accounts" "Levels of Subaccounts" '2)
    (if cashOnly (set-option! option-db "Accounts" "Accounts" (get-cash-accounts)))
    (if cashOnly (set-option! option-db "Accounts" "Levels of Subaccounts" '3))
)

(define (setup-accounts-chart option-db)
    (set-option! option-db "Display" "Include accounts with zero total balances" #f)
    (set-option! option-db "Display" "Omit zero balance figures" #t)
    (set-option! option-db "Display" "Parent account balances" 'recursive-bal)
    (set-option! option-db "Display" "Parent account subtotals" 't)
    (set-option! option-db "Display" "Show accounting-style rules" #t)
    (set-option! option-db "Display" "Account Balance" #f)
    (set-option! option-db "Display" "Account Code" #f)
    (set-option! option-db "Accounts" "Levels of Subaccounts" '2)
)

(define (setup-transactions option-db)
    (set-option! option-db "General" "Report name" "<hr style=\"border: 1px solid orange; margin-top: 75px;\"> Transaction Report")
    (set-option! option-db "General" "Table for Exporting" #t)
    (set-option! option-db "Display" "Num" #f)
    (set-option! option-db "Display" "Account Balance" #t)
    (set-option! option-db "Display" "Subtotal Table" #t)
    (set-option! option-db "Accounts" "Accounts" (get-cash-accounts))
)

;; Main Dashboard's Definition

(define (gnc:make-dashboard-v2)
  (let* ((view (gnc:make-report multicolumn-guid))
         (sub-cash-flow (gnc:make-report cash-flow-guid))
         (sub-inc-expense (gnc:make-report exp-piechart-guid))
         (sub-inc-time (gnc:make-report inc-exp-chart-guid))
         (sub-income-pie (gnc:make-report inc-piechart-guid))
         (sub-accounts (gnc:make-report acct-summary-guid))
         (sub-assets (gnc:make-report assets-piechart-guid))
         (sub-money (gnc:make-report money-piechart-guid))
         (sub-transactions (gnc:make-report transactions-guid))
         (options (gnc:report-options (gnc-report-find view)))
    )

    (set-option! options "General" "Report name" (G_ "Dashboard 2.0"))
    (set-option! options "General" "Number of columns" 3)
    (set-option! options "General" "Stylesheet" "Easy")

    ;; mark the reports as needing to be saved
    (gnc:report-set-needs-save?! (gnc-report-find sub-cash-flow) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-inc-expense) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-inc-time) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-income-pie) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-accounts) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-assets) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-money) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-transactions) #t)

    (setup-cash-flow (gnc:report-options (gnc-report-find sub-cash-flow)))
    (setup-transactions (gnc:report-options (gnc-report-find sub-transactions)))
    (setup-accounts-chart (gnc:report-options (gnc-report-find sub-accounts)))
    (setup-inc-exp-chart (gnc:report-options (gnc-report-find sub-inc-expense)) "Income / Expense")
    (setup-inc-exp-chart (gnc:report-options (gnc-report-find sub-inc-time)) "Income Only")
    (setup-pie-chart (gnc:report-options (gnc-report-find sub-money)) "Surplus Money" #t)
    (setup-pie-chart (gnc:report-options (gnc-report-find sub-assets)) "Assets" #f)

    (set-option! options "__general" "report-list"
                 (list
                       (list sub-cash-flow 1 3 #f)
                       (list sub-inc-expense 1 1 #f)
                       (list sub-inc-time 1 1 #f)
                       (list sub-assets 1 1 #f)
                       (list sub-accounts 1 2 #f)
                       (list sub-money 1 1 #f)
                       (list sub-transactions 3 1 #f)
                ))
  view))
