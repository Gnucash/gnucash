;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Balance Assertion Report
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard balance-assertion-report))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))
(use-modules (srfi srfi-26))

(use-modules (gnucash core-utils))
(use-modules (gnucash gnc-module))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define optname-verbose (N_ "Verbose report"))
(define opthelp-verbose (N_ "Verbose report"))

(define (reconcile-report-options-generator)
  (define options (gnc:new-options))
  (define add-option (cut gnc:register-option options <>))

  (add-option
   (gnc:make-account-list-option
    gnc:pagename-accounts "Accounts"
    "a" (G_ "Report on these accounts.")
    (const '()) #f #t))

  (add-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-verbose
    "a" opthelp-verbose #f))

  (gnc:options-set-default-section options gnc:pagename-general)
  options)

(define (reconcile-audit-report-renderer report-obj)
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define (split->reconciled-date split)
    (if (eqv? (xaccSplitGetReconcile split) #\y)
        (xaccSplitGetDateReconciled split)
        +inf.0))

  (let* ((report-title (get-option gnc:pagename-general gnc:optname-reportname))
         (accounts-option (get-option gnc:pagename-accounts "Accounts"))
         (accounts (if (null? accounts-option)
                       (gnc-account-get-descendants (gnc-get-current-root-account))
                       accounts-option))
         (verbose? (get-option gnc:pagename-general optname-verbose))
         (document (gnc:make-html-document)))

    (gnc:report-starting report-title)

    (let lp ((accounts accounts) (has-data? #f))
      (match accounts

        (()
         (unless has-data?
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
             report-title (gnc:report-id report-obj))))

         (gnc:report-finished)

         document)

        ((account . rest-accounts)
         (define table (gnc:make-html-table))
         (define assertion-dates (xaccAccountGetBalanceAssertionDates account))
         (define has-assertions? (pair? assertion-dates))
         (define split->elt
           (let ((bal 0))
             (lambda (s)
               (set! bal (+ bal (xaccSplitGetAmount s)))
               bal)))

         (let lp1 ((assertion-dates assertion-dates)
                   (acc-balances (gnc:account-accumulate-at-dates
                                  account assertion-dates
                                  #:split->date split->reconciled-date
                                  #:split->elt split->elt))
                   (splits (xaccAccountGetSplitList account))
                   (has-data? has-data?))

           (match assertion-dates
             (()
              (let ((assertions-valid? (gnc-account-assertions-are-valid account)))
                (when (or verbose? has-assertions?)
                  (gnc:html-document-add-object!
                   document
                   (gnc:make-html-text
                    (gnc:html-markup-h3
                     "Account "
                     (gnc:html-markup-i (gnc-account-get-full-name account))
                     (if assertions-valid? "PASS" "FAIL"))))

                  (gnc:html-table-set-col-headers!
                   table (list "Reconcile Date" "Post Date"
                               "Description" "Amount" "Reconciled Balance"))

                  (gnc:html-document-add-object! document table))

                (lp rest-accounts (or has-data? has-assertions?))))

             ((assertion-date . rest-dates)
              (let* ((acc-balance (car acc-balances))
                     (assertion-bal (xaccAccountGetBalanceAssertionAtDate
                                     account assertion-date)))

                (define (in-date? s)
                  (<= (xaccSplitGetDateReconciled s) assertion-date))

                (define (add-split-row s)
                  (define t (xaccSplitGetParent s))
                  (gnc:html-table-append-row!
                   table
                   (append
                    (list (qof-print-date (xaccSplitGetDateReconciled s))
                          (qof-print-date (xaccTransGetDate t))
                          (xaccTransGetDescription t))
                    (map (cut gnc:make-html-table-cell/markup "number-cell" <>)
                         (list (xaccSplitGetAmount s)
                               (xaccSplitGetReconciledBalance s))))))

                (receive (acc-splits rest-splits) (partition in-date? splits)
                  (when (or verbose? (not (equal? acc-balance assertion-bal)))
                    (for-each add-split-row acc-splits))

                  (gnc:html-table-append-row!
                   table
                   (append
                    (map (cut gnc:make-html-table-cell/markup "total-label-cell" <>)
                         (list (qof-print-date assertion-date) #f #f #f))
                    (map (cut gnc:make-html-table-cell/markup "total-number-cell" <>)
                         (list acc-balance
                               (if (equal? acc-balance assertion-bal) "PASS"
                                   (format #f "FAIL (expected bal = ~a)"
                                           (gnc:monetary->string
                                            (gnc:make-gnc-monetary
                                             (xaccAccountGetCommodity account)
                                             assertion-bal))))))))

                  (lp1 (cdr assertion-dates) (cdr acc-balances) rest-splits
                       #t)))))))))))

(gnc:define-report
 'version 1
 'name (G_ "Balance Assertion Audit Report")
 'report-guid "63e4a56c0e86478482dc6fcf515e9228"
 'options-generator reconcile-report-options-generator
 'renderer reconcile-audit-report-renderer)

