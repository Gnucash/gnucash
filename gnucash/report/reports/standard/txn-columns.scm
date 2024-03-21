;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define-module (gnucash reports standard txn-columns))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (gnucash utilities))
(use-modules (gnucash report))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))

(define reportname (N_ "Transaction Breakdown Report"))
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-account (N_ "Account"))
(define optname-max-columns (N_ "Limit for number of columns"))
(define opthelp-max-columns (N_ "Set the upper limit for number of columns"))
(define optname-desc-filter "Description Filter")

(define (options-generator)
  (let ((options (gnc-new-optiondb)))

    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-from-date optname-to-date "a")

    (gnc-register-account-sel-limited-option
     options gnc:pagename-general optname-account "b" "Account" '() '())

    (gnc-register-string-option
     options gnc:pagename-general optname-desc-filter "c" "Description Filter" "")

    (gnc-register-number-range-option
     options gnc:pagename-general optname-max-columns "d" opthelp-max-columns 10 1 100 1)

    options))


;; gets an account's fullname as a list eg: '("Asset" "Investments" "Shares")
(define (acct-get-fullname-list acct)
  (define root (gnc-book-get-root-account (gnc-get-current-book)))
  (let lp ((acct acct) (accum '()))
    (cond
     ((equal? acct root) accum)
     (else (lp (gnc-account-get-parent acct)
               (cons (xaccAccountGetName acct) accum))))))

(define (txn-columns-renderer report-obj)

  (define options (gnc:report-options report-obj))

  (define get-option
    (cut gnc-optiondb-lookup-value options <> <>))

  (define document (gnc:make-html-document))

  (define to-date
    (gnc:time64-end-day-time
     (gnc:date-option-absolute-time
      (get-option gnc:pagename-general optname-to-date))))

  (define from-date
    (gnc:time64-start-day-time
     (gnc:date-option-absolute-time
      (get-option gnc:pagename-general optname-from-date))))

  (define account
    (get-option gnc:pagename-general optname-account))

  (define desc-filter
    (get-option gnc:pagename-general optname-desc-filter))

  (define opt-max-columns
    (get-option gnc:pagename-general optname-max-columns))

  (define currency (xaccAccountGetCommodity account))

  (define (desc-filter? txn)
    (string-contains (xaccTransGetDescription txn) desc-filter))

  (define transactions
    (cond
     ((null? account) '())
     (else (let ((query (qof-query-create-for-splits)))
             (qof-query-set-book query (gnc-get-current-book))
             (xaccQueryAddClearedMatch
              query (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)
             (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
             (xaccQueryAddDateMatchTT query #t from-date #t to-date QOF-QUERY-AND)
             (let ((result (filter-map (lambda (split) (let ((txn (xaccSplitGetParent split)))
                                                         (and (desc-filter? txn) txn)))
                                       (xaccQueryGetSplitsUniqueTrans query))))
               (qof-query-destroy query)
               result)))))

  (define (account<? a b)
    (< (xaccAccountOrder a b) 0))

  (define (account->sort-index a)
    (list-index
     (cut equal? <> (xaccAccountTypeGetFundamental (xaccAccountGetType a)))
     (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY ACCT-TYPE-EQUITY ACCT-TYPE-EXPENSE ACCT-TYPE-INCOME)))

  (define (account-type<? a b)
    (< (account->sort-index a) (account->sort-index b)))

  (define accounts
    (let lp ((transactions transactions) (rv '()))
      (match transactions
        (() (stable-sort (sort rv account<?) account-type<?))
        ((txn . rest)
         (let lp1 ((txn-splits (xaccTransGetSplitList txn)) (rv rv))
           (match txn-splits
             (() (lp rest rv))
             (((= xaccSplitGetAccount s-acc) . rest-splits)
              (lp1 rest-splits (if (or (equal? (xaccAccountGetType s-acc) ACCT-TYPE-TRADING)
                                       (member s-acc rv))
                                   rv
                                   (cons s-acc rv))))))))))

  (define num-accounts-found (length accounts))

  (cond
   ((null? transactions)
    (gnc:html-document-add-object!
     document (gnc:html-make-empty-data-warning
               reportname (gnc:report-id report-obj))))

   ((> num-accounts-found opt-max-columns)
    (gnc:html-document-add-object!
     document
     (gnc:html-make-generic-warning
      reportname (gnc:report-id report-obj)
      (G_ "Too many accounts")
      (G_ "The number of accounts linked by the transactions found exceeds the limit. \
Select a different subset of transactions, or increase the limit in the options."))))

   (else
    (let ((table (gnc:make-html-table))
          (max-depth (fold (lambda (a b) (max (gnc-account-get-current-depth a) b)) 0 accounts)))

      (define (add-padding-right lst)
        (let lp ((remaining max-depth) (lst lst) (accum '()))
          (if (zero? remaining)
              (reverse accum)
              (match lst
                ((head . tail) (lp (1- remaining) tail (cons head accum)))
                (_ (lp (1- remaining) #f (cons #f accum)))))))

      (gnc:html-table-set-multirow-col-headers!
       table (apply zip (cons* (add-padding-right '("Date"))
                               (add-padding-right '("Num"))
                               (add-padding-right '("Desc"))
                               (map (compose add-padding-right acct-get-fullname-list) accounts))))

      (let lp ((transactions transactions)
               (totals (make-list num-accounts-found 0)))
        (match transactions
          (()
           (gnc:html-table-append-row!
            table
            (cons* (gnc:make-html-table-cell/size/markup
                    1 3 "total-label-cell"
                    (gnc:make-html-text
                     (G_ "Total For ")
                     (format #f "~a to ~a" (qof-print-date from-date) (qof-print-date to-date))))
                   (map (lambda (acc total)
                          (gnc:make-html-table-cell/markup
                           "total-number-cell"
                           (gnc:make-gnc-monetary (xaccAccountGetCommodity acc) total)))
                        accounts totals))))
          ((txn . rest-txns)
           (let lp1 ((accounts accounts)
                     (totals totals)
                     (columns '())
                     (new-totals '())
                     (txn-splits (xaccTransGetSplitList txn)))
             (match accounts
               (()
                (gnc:html-table-append-row!
                 table
                 (cons* (qof-print-date (xaccTransGetDate txn))
                        (xaccTransGetNum txn)
                        (xaccTransGetDescription txn)
                        (reverse columns)))
                (lp rest-txns (reverse new-totals)))
               ((acc . rest-accts)
                (define (is-acc? split)
                  (equal? acc (xaccSplitGetAccount split)))
                (define (maybe-negate split->num split)
                  (let ((num (split->num split)))
                    (if (gnc-reverse-balance acc) (- num) num)))
                (let lp2 ((txn-splits txn-splits)
                          (next-txn-splits '())
                          (bal-value #f)
                          (bal-amount #f)
                          (first-split #f))
                  (match txn-splits
                    (()
                     (lp1 rest-accts
                          (cdr totals)
                          (cons (and bal-value
                                     (gnc:make-html-table-cell/markup
                                      "number-cell"
                                      (let* ((txn-currency (xaccTransGetCurrency txn))
                                             (acc-commodity (xaccAccountGetCommodity acc))
                                             (value-text (gnc:make-gnc-monetary txn-currency bal-value)))
                                        (if (equal? acc-commodity txn-currency)
                                            (gnc:make-html-text (gnc:html-markup-anchor
                                                                 (gnc:split-anchor-text first-split)
                                                                 value-text))
                                            (gnc:make-html-text "[" value-text "] "
                                                                (gnc:html-markup-anchor
                                                                 (gnc:split-anchor-text first-split)
                                                                 (gnc:make-gnc-monetary acc-commodity bal-amount)))))))
                                columns)
                          (cons (+ (car totals) (or bal-amount 0)) new-totals)
                          (reverse next-txn-splits)))
                    (((? is-acc? split) . rest)
                     (lp2 rest next-txn-splits
                          (+ (or bal-value 0) (maybe-negate xaccSplitGetValue split))
                          (+ (or bal-amount 0) (maybe-negate xaccSplitGetAmount split))
                          (or first-split split)))
                    ((this . rest) (lp2 rest (cons this next-txn-splits) bal-value bal-amount first-split))))))))))

      (gnc:html-document-set-title! document (G_ reportname))

      (gnc:html-document-add-object! document (gnc:html-render-options-changed options))

      (gnc:html-document-add-object! document table))))

  document)


;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "03603f0443494a6fa790b417f35d90d4"
 'menu-path (list gnc:menuname-experimental)
 'options-generator options-generator
 'renderer txn-columns-renderer)
