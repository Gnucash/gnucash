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
(use-modules (gnucash report standard-reports register))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash engine test test-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")
(define uuid "22104e02654c4adba844ee75a3f8d173")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "register")
  (test-register)
  (test-end "register"))

(define (set-option options page tag value)
  ((gnc:option-setter (gnc:lookup-option options page tag)) value))

(define (teardown)
  (gnc-clear-current-session))

(define (options->sxml options test-title)
  (gnc:options->sxml uuid options "test-register" test-title))

(define (test-register)
  (let* ((options (gnc:make-report-options uuid))
         (account-alist (create-test-data))
         (bank (cdr (assoc "Bank" account-alist))))

    (let ((query (qof-query-create-for-splits)))
      (qof-query-set-book query (gnc-get-current-book))
      (xaccQueryAddAccountMatch query (list bank)
                                QOF-GUID-MATCH-ANY QOF-QUERY-AND)
      (set-option options "__reg" "query" (gnc-query2scm query)))

    (let ((sxml (options->sxml options "basic")))
      (test-equal "table has 231 cells"
        231
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = 2587"
        '("Total Debits" "$2,587.00")
        (sxml->table-row-col sxml 1 -3 #f))

      (test-equal "total credits = 401"
        '("Total Credits" "$401.00")
        (sxml->table-row-col sxml 1 -1 #f)))

    (set-option options "__reg" "journal" #t)
    (let ((sxml (options->sxml options "journal")))
      (test-equal "table has 333 cells"
        333
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = 2587"
        '("Total Debits" "$2,587.00")
        (sxml->table-row-col sxml 1 -3 #f))

      (test-equal "total credits = 401"
        '("Total Credits" "$401.00")
        (sxml->table-row-col sxml 1 -1 #f)))

    (set-option options "__reg" "ledger-type" #t)
    (let ((sxml (options->sxml options "ledger-type")))
      (test-equal "table has 335 cells"
        335
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = 2587"
        '("Total Debits" "$2,587.00")
        (sxml->table-row-col sxml 1 -5 #f))

      (test-equal "total credits = 401"
        '("Total Credits" "$401.00")
        (sxml->table-row-col sxml 1 -3 #f))

      (test-equal "net change = 401"
        '("Net Change" "$2,186.00")
        (sxml->table-row-col sxml 1 -1 #f)))

    (set-option options "__reg" "double" #t)
    (let ((sxml (options->sxml options "double")))
      (test-equal "table has 339 cells"
        339
        (length (sxml->table-row-col sxml 1 #f #f)))

      (test-equal "total debit = 2587"
        '("Total Debits" "$2,587.00")
        (sxml->table-row-col sxml 1 -5 #f))

      (test-equal "total credits = 401"
        '("Total Credits" "$401.00")
        (sxml->table-row-col sxml 1 -3 #f))

      (test-equal "net change = 401"
        '("Net Change" "$2,186.00")
        (sxml->table-row-col sxml 1 -1 #f)))
    ))
