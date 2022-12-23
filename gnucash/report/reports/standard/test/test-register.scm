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
(use-modules (tests srfi64-extras))

(use-modules (gnucash utilities))
(use-modules (gnucash report))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (gnucash reports standard register))
(use-modules (tests test-report-extras))
(use-modules (gnucash report stylesheets plain)) ; For the default stylesheet, required for rendering
(use-modules (tests test-engine-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")
(define uuid "22104e02654c4adba844ee75a3f8d173")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "register")
  (test-register)
  (test-end "register"))

(define (set-option options page tag value)
  (gnc-set-option (gnc:optiondb options) page tag value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-register" test-title))

(define (test-register)
  (let* ((account-alist (create-test-data))
         (options (gnc:make-report-options uuid))
         (bank (cdr (assoc "Bank" account-alist))))

    (gnc-commodity-set-user-symbol
     (xaccAccountGetCommodity (assoc-ref account-alist "GBP Bank"))
     "#")

    (let ((query (qof-query-create-for-splits)))
      (qof-query-set-book query (gnc-get-current-book))
      (xaccQueryAddAccountMatch query (list bank)
                                QOF-GUID-MATCH-ANY QOF-QUERY-AND)
      (set-option options "__reg" "query" (gnc-query2scm query))
      (qof-query-destroy query))

    (let ((sxml (options->sxml options "basic")))
      ;; this is a simplistic test - counts the number of populated
      ;; html-table-cells in the register table.
      (test-equal "table has 232 cells"
        232
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = $2587"
        '("Total Debits" "$2,587.00")
        (sxml->table-row-col sxml 1 -3 #f))

      (test-equal "total credits = $401"
        '("Total Credits" "$401.00")
        (sxml->table-row-col sxml 1 -1 #f)))

    (set-option options "__reg" "journal" #t)
    (let ((sxml (options->sxml options "journal")))
      ;; this is a simplistic test - counts the number of populated
      ;; html-table-cells in the register table.
      (test-equal "table has 329 cells"
        329
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = #6"
        '("Total Debits" "#6.00")
        (sxml->table-row-col sxml 1 135 #f))

      (test-equal "total debit = $2979"
        '("Total Debits" "$2,979.00")
        (sxml->table-row-col sxml 1 136 #f))

      (test-equal "total credits = #10"
        '("Total Credits" "#10.00")
        (sxml->table-row-col sxml 1 138 #f))

      (test-equal "total credits = 2974"
        '("Total Credits" "$2,974.00")
        (sxml->table-row-col sxml 1 139 #f)))

    (set-option options "__reg" "ledger-type" #t)
    (let ((sxml (options->sxml options "ledger-type")))
      ;; this is a simplistic test - counts the number of populated
      ;; html-table-cells in the register table.
      (test-equal "table has 333 cells"
        333
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = #6"
        '("Total Debits" "#6.00")
        (sxml->table-row-col sxml 1 135 #f))

      (test-equal "total debit = $2979"
        '("Total Debits" "$2,979.00")
        (sxml->table-row-col sxml 1 136 #f))

      (test-equal "total credits = #10"
        '("Total Credits" "#10.00")
        (sxml->table-row-col sxml 1 138 #f))

      (test-equal "total credits = $2974"
        '("Total Credits" "$2,974.00")
        (sxml->table-row-col sxml 1 139 #f))

      (test-equal "net change = #4"
        '("Net Change" "#4.00")
        (sxml->table-row-col sxml 1 141 #f))

      (test-equal "net change = $5"
        '("Net Change" "$5.00")
        (sxml->table-row-col sxml 1 142 #f)))

    (set-option options "__reg" "double" #t)
    (let ((sxml (options->sxml options "double")))
      ;; this is a simplistic test - counts the number of populated
      ;; html-table-cells in the register table.
      (test-equal "table has 337 cells"
        337
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = #6"
        '("Total Debits" "#6.00")
        (sxml->table-row-col sxml 1 179 #f))

      (test-equal "total debit = $2979"
        '("Total Debits" "$2,979.00")
        (sxml->table-row-col sxml 1 180 #f))

      (test-equal "total credits = #10"
        '("Total Credits" "#10.00")
        (sxml->table-row-col sxml 1 182 #f))

      (test-equal "total credits = $2974"
        '("Total Credits" "$2,974.00")
        (sxml->table-row-col sxml 1 183 #f))

      (test-equal "net change = #4"
        '("Net Change" "#4.00")
        (sxml->table-row-col sxml 1 185 #f))

      (test-equal "net change = $5"
        '("Net Change" "$5.00")
        (sxml->table-row-col sxml 1 186 #f)))
    ))
