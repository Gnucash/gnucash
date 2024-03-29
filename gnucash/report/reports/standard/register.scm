;; -*-scheme-*-
;; register.scm
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


(define-module (gnucash reports standard register))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))

(define (date-col columns-used)
  (vector-ref columns-used 0))
(define (num-col columns-used)
  (vector-ref columns-used 1))
(define (description-col columns-used)
  (vector-ref columns-used 2))
(define (memo-col columns-used)
  (vector-ref columns-used 3))
(define (account-col columns-used)
  (vector-ref columns-used 4))
(define (shares-col columns-used)
  (vector-ref columns-used 5))
(define (price-col columns-used)
  (vector-ref columns-used 6))
(define (amount-single-col columns-used)
  (vector-ref columns-used 7))
(define (debit-col columns-used)
  (vector-ref columns-used 8))
(define (credit-col columns-used)
  (vector-ref columns-used 9))
(define (balance-col columns-used)
  (vector-ref columns-used 10))
(define (value-single-col columns-used)
  (vector-ref columns-used 11))
(define (value-debit-col columns-used)
  (vector-ref columns-used 12))
(define (value-credit-col columns-used)
  (vector-ref columns-used 13))
(define (lot-col columns-used)
  (vector-ref columns-used 14))

(define columns-used-size 15)
(define reportname (N_ "Register"))

(define (num-columns-required columns-used)
  (do ((i 0 (+ i 1))
       (col-req 0 col-req))
      ((>= i columns-used-size) col-req)
    (if (vector-ref columns-used i)
        (set! col-req (+ col-req 1)))))

(define (build-column-used options)
  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))
  (define (make-set-col col-vector)
    (let ((col 0))
      (lambda (used? index)
        (if used?
            (begin
              (vector-set! col-vector index col)
              (set! col (+ col 1)))
            (vector-set! col-vector index #f)))))

  (let* ((col-vector (make-vector columns-used-size #f))
         (set-col (make-set-col col-vector)))
    (set-col (opt-val "Display" "Date") 0)
    (set-col (if (gnc-lookup-option options "Display" "Num")
                 (opt-val "Display" "Num")
                 (opt-val "Display" "Num/Action")) 1)
    (set-col
     (if (opt-val "__reg" "journal")
         (or (opt-val "Display" "Memo")
             (opt-val "Display" "Description")
             (opt-val "__reg" "double") )
         (opt-val "Display" "Description"))
     2)
    (set-col
     (if (opt-val "__reg" "journal")
         #f
         (opt-val "Display" "Memo"))
     3)
    (set-col (opt-val "Display" "Account") 4)
    (set-col (opt-val "Display" "Shares") 5)
    (set-col (opt-val "Display" "Lot") 14)
    (set-col (opt-val "Display" "Price") 6)
    (let ((amount-setting (opt-val "Display" "Amount")))
      (if (eq? amount-setting 'single)
          (set-col #t 7)
          (begin
            (set-col #t 8)
            (set-col #t 9))))
    (if (opt-val "Display" "Value")
        (if (amount-single-col col-vector)
            (set-col #t 11)
            (begin
              (set-col #t 12)
              (set-col #t 13))))
    (set-col (opt-val "Display" "Running Balance") 10)

    col-vector))

(define (make-heading-list column-vector
                           debit-string credit-string amount-string
                           multi-rows? action-for-num? ledger-type?)
  (let ((heading-list '()))
    (gnc:debug "Column-vector" column-vector)
    (if (date-col column-vector)
        (addto! heading-list (G_ "Date")))
    (if (num-col column-vector)
        (addto! heading-list (if action-for-num?
                                 (if ledger-type?
                                     (G_ "T-Num")
                                     (G_ "Num/Action"))
                                 (G_ "Num"))))
    (if (description-col column-vector)
        (addto! heading-list (G_ "Description")))
    (if (memo-col column-vector)
        (addto! heading-list (G_ "Memo")))
    (if (account-col column-vector)
        (addto! heading-list (if multi-rows?
                                 (G_ "Account")
                                 (G_ "Transfer"))))
    (if (shares-col column-vector)
        (addto! heading-list (G_ "Shares")))
    (if (lot-col column-vector)
        (addto! heading-list (G_ "Lot")))
    (if (price-col column-vector)
        (addto! heading-list (G_ "Price")))
    (if (amount-single-col column-vector)
        (addto! heading-list amount-string))
    (if (debit-col column-vector)
        (addto! heading-list debit-string))
    (if (credit-col column-vector)
        (addto! heading-list credit-string))
    (if (value-single-col column-vector)
        (addto! heading-list (G_ "Value")))
    (if (value-debit-col column-vector)
        (addto! heading-list (G_ "Debit Value")))
    (if (value-credit-col column-vector)
        (addto! heading-list (G_ "Credit Value")))
    (if (balance-col column-vector)
        (addto! heading-list (G_ "Balance")))
    (reverse heading-list)))

(define (add-split-row table split column-vector row-style transaction-info?
                       split-info? action-for-num? ledger-type? double? memo?
                       description? total-collector)
  (let* ((row-contents '())
         (parent (xaccSplitGetParent split))
         (account (xaccSplitGetAccount split))
         (reverse? (gnc-reverse-balance account))
         (currency (xaccAccountGetCommodity account))
         (trans-currency (xaccTransGetCurrency parent))
         (damount (xaccSplitGetAmount split))
         (dvalue (xaccSplitGetValue split))
         (split-abs-amount (gnc:make-gnc-monetary currency (abs damount)))
         (split-abs-value (gnc:make-gnc-monetary trans-currency (abs dvalue))))

    (if (date-col column-vector)
        (addto! row-contents
                (and transaction-info?
                     (gnc:make-html-table-cell/markup
                      "date-cell" (qof-print-date (xaccTransGetDate parent))))))
    (if (num-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "text-cell"
                 (if transaction-info?
                     (if (and action-for-num? ledger-type?)
                         (gnc-get-num-action parent #f)
                         (gnc-get-num-action parent split))
                     (and split-info? (gnc-get-action-num  #f split))))))
    (if (description-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "text-cell"
                 (if transaction-info?
                     (and description? (xaccTransGetDescription parent))
                     (and split-info? memo? (xaccSplitGetMemo split))))))
    (if (memo-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "text-cell" (and transaction-info? (xaccSplitGetMemo split)))))
    (if (account-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "text-cell"
                 (cond
                  ((not split-info?) #f)
                  ((not transaction-info?) (gnc-account-get-full-name account))
                  (else (case (xaccTransCountSplits (xaccSplitGetParent split))
                          ((2) (gnc-account-get-full-name
                                (xaccSplitGetAccount
                                 (xaccSplitGetOtherSplit split))))
                          ((1) (G_ "None"))
                          (else (G_ "-- Split Transaction --"))))))))
    (if (shares-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "number-cell"
                 (and split-info? (xaccSplitGetAmount split)))))
    (if (lot-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "text-cell"
                 (if split-info? (gnc-lot-get-title (xaccSplitGetLot split))))))
    (if (price-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
                 "number-cell"
                 (and split-info?
                      (gnc:default-price-renderer
                       trans-currency (xaccSplitGetSharePrice split))))))
    (if (amount-single-col column-vector)
        (addto! row-contents
                (and split-info?
                     (gnc:make-html-table-cell/markup
                      "number-cell"
                      (gnc:html-split-anchor
                       split (gnc:make-gnc-monetary
                              currency (if reverse? (- damount) damount)))))))
    (if (debit-col column-vector)
        (addto! row-contents
                (and split-info? (positive? damount)
                     (gnc:make-html-table-cell/markup
                      "number-cell"
                      (gnc:html-split-anchor split split-abs-amount)))))
    (if (credit-col column-vector)
        (addto! row-contents
                (and split-info? (not (positive? damount))
                     (gnc:make-html-table-cell/markup
                      "number-cell"
                      (gnc:html-split-anchor split split-abs-amount)))))
    (if (value-single-col column-vector)
        (addto! row-contents
                (and split-info?
                     (gnc:make-html-table-cell/markup
                      "number-cell"
                      (gnc:make-gnc-monetary
                       trans-currency (if reverse? (- dvalue) dvalue))))))
    (if (value-debit-col column-vector)
        (addto! row-contents
                (and split-info? (positive? dvalue)
                     (gnc:make-html-table-cell/markup
                      "number-cell" split-abs-value))))
    (if (value-credit-col column-vector)
        (addto! row-contents
                (and split-info? (not (positive? dvalue))
                     (gnc:make-html-table-cell/markup
                      "number-cell" split-abs-value))))
    ;; For single account registers, use the split's cached balance to remain
    ;; consistent with the balances shown in the register itself
    ;; For others, use the cumulated balance from the totals-collector
    (if (balance-col column-vector)
        (addto! row-contents
                (if transaction-info?
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:html-split-anchor
                      split
                      (gnc:make-gnc-monetary
                       currency
                       (cond
                        (ledger-type? (cadr (total-collector 'getpair currency #f)))
                        ((gnc-reverse-balance account) (- (xaccSplitGetBalance split)))
                        (else (xaccSplitGetBalance split))))))
                    " ")))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    (if (and double? transaction-info?)
        (if (or (num-col column-vector) (description-col column-vector))
            (begin
              (let ((count 0))
                (set! row-contents '())
                (if (date-col column-vector)
                    (begin
                      (set! count (+ count 1))
                      (addto! row-contents " ")))
                (if (and (num-col column-vector) (description-col column-vector))
                    (begin
                      (set! count (+ count 1))
                      (addto! row-contents
                              (gnc:make-html-table-cell/markup
                               "text-cell"
                               (if (and action-for-num? (not ledger-type?))
                                   (gnc-get-num-action parent #f)
                                   " ")))))
                (if (description-col column-vector)
                    (addto! row-contents ;;
                            (gnc:make-html-table-cell/size
                             1 (- (num-columns-required column-vector) count)
                             (xaccTransGetNotes parent)))
                    (gnc:make-html-table-cell/size
                     1 (- (num-columns-required column-vector) (- count 1))
                     (if (and action-for-num? (not ledger-type?))
                         (gnc-get-num-action parent #f)
                         " ")))
                (gnc:html-table-append-row/markup! table row-style
                                                   (reverse row-contents))))))))


(define (options-generator)
  (let ((options (gnc-new-optiondb)))

  (gnc-register-query-option options
    "__reg" "query" '())
  (gnc-register-internal-option options
    "__reg" "journal" #f)
  (gnc-register-internal-option options
    "__reg" "ledger-type" #f)
  (gnc-register-internal-option options
    "__reg" "double" #f)
  (gnc-register-internal-option options
    "__reg" "debit-string" (G_ "Debit"))
  (gnc-register-internal-option options
    "__reg" "credit-string" (G_ "Credit"))

  (gnc-register-string-option options
    (N_ "General") (N_ "Title")
    "a" (N_ "The title of the report.")
    (N_ "Register Report"))

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Date")
    "b" (N_ "Display the date?") #t)

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc-register-simple-boolean-option options
        (N_ "Display") (N_ "Num/Action")
        "c" (N_ "Display the check number/action?") #t)
      (gnc-register-simple-boolean-option options
        (N_ "Display") (N_ "Num")
        "c" (N_ "Display the check number?") #t))

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Description")
    "d" (N_ "Display the description?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Memo")
    "e" (N_ "Display the memo?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Account")
    "g" (N_ "Display the account?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Shares")
    "ha" (N_ "Display the number of shares?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Lot")
    "hb" (N_ "Display the name of lot the shares are in?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Price")
    "hc" (N_ "Display the shares price?") #f)

  (gnc-register-multichoice-option options
    (N_ "Display") (N_ "Amount")
    "ia" (N_ "Display the amount?")
    "double"
    (list
     (vector 'single (N_ "Single Column"))
     (vector 'double (N_ "Two Columns"))))

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Value")
    "ib" (N_ "Display the value in transaction currency?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Running Balance")
    "k" (N_ "Display a running balance?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Totals")
    "l" (N_ "Display the totals?") #t)


  (gnc:options-set-default-section options "General")

  options))

;; -----------------------------------------------------------------
;; create the report result
;; -----------------------------------------------------------------

(define (make-split-table splits options
                          debit-string credit-string amount-string)
  ;; ----------------------------------
  ;; local helper
  ;; ----------------------------------
  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))
  (define (reg-report-journal?)
    (opt-val "__reg" "journal"))
  (define (reg-report-ledger-type?)
    (opt-val "__reg" "ledger-type"))
  (define (reg-report-double?)
    (opt-val "__reg" "double"))
  (define (reg-report-show-totals?)
    (opt-val "Display" "Totals"))

  (define (add-subtotal-row label leader table used-columns
                            subtotal-collector subtotal-style
                            value?)
    (let ((currency-totals (subtotal-collector
                            'format gnc:make-gnc-monetary #f))
          (single-col (if value?
                          (value-single-col used-columns)
                          (amount-single-col used-columns)))
          (credit-col (if value?
                          (value-credit-col used-columns)
                          (credit-col used-columns)))
          (debit-col (if value?
                         (value-debit-col used-columns)
                         (debit-col used-columns))))

      (define (colspan monetary)
        (cond
         (single-col single-col)
         ((negative? (gnc:gnc-monetary-amount monetary)) credit-col)
         (else debit-col)))

      (define (display-subtotal monetary)
        (if single-col
            (if (and leader (gnc-reverse-balance leader))
                (gnc:monetary-neg monetary)
                monetary)
            (if (negative? (gnc:gnc-monetary-amount monetary))
                (gnc:monetary-neg monetary)
                monetary)))

      (when (or single-col credit-col debit-col)
        (gnc:html-table-append-row!
         table
         (list
          (gnc:make-html-table-cell/size
           1 (num-columns-required used-columns)
           (gnc:make-html-text (gnc:html-markup-hr)))))

        (for-each
         (lambda (monetary)
           (gnc:html-table-append-row/markup!
            table subtotal-style
            (list (gnc:make-html-table-cell/markup "total-label-cell" label)
                  (gnc:make-html-table-cell/size/markup
                   1 (colspan monetary) "total-number-cell"
                   (display-subtotal monetary)))))
         currency-totals))))

  (define (accumulate-totals split total-amount total-value
                             debit-amount debit-value
                             credit-amount credit-value)
    (let* ((parent (xaccSplitGetParent split))
           (account (xaccSplitGetAccount split))
           (split-currency (xaccAccountGetCommodity account))
           (split-amount (xaccSplitGetAmount split))
           (trans-currency (xaccTransGetCurrency parent))
           (split-value (xaccSplitGetValue split)))
      (if (positive? split-amount)
          (debit-amount 'add split-currency split-amount)
          (credit-amount 'add split-currency split-amount))
      (if (positive? split-value)
          (debit-value 'add trans-currency split-value)
          (credit-value 'add trans-currency split-value))
      (total-amount 'add split-currency split-amount)
      (total-value 'add trans-currency split-value)))

  (define (splits-leader splits)
    (let ((accounts (map xaccSplitGetAccount splits)))
      (and (pair? accounts)
           (apply equal? accounts)
           (car accounts))))

  ;; ----------------------------------
  ;; make the split table
  ;; ----------------------------------
  (let* ((table (gnc:make-html-table))
         (used-columns (build-column-used options))
         (width (num-columns-required used-columns))
         (multi-rows? (reg-report-journal?))
         (ledger-type? (reg-report-ledger-type?))
         (double? (reg-report-double?))
         (leader (splits-leader splits))
         (total-collector (gnc:make-commodity-collector))
         (debit-collector (gnc:make-commodity-collector))
         (credit-collector (gnc:make-commodity-collector))
         (total-value (gnc:make-commodity-collector))
         (debit-value (gnc:make-commodity-collector))
         (credit-value (gnc:make-commodity-collector))
         (work-to-do (length splits))
         (action-for-num? (qof-book-use-split-action-for-num-field
                           (gnc-get-current-book))))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns
                        debit-string credit-string amount-string
                        multi-rows? action-for-num? ledger-type?))

    (let loop ((splits splits)
               (work-done 0)
               (odd-row? #t))

      (cond

       ;; ----------------------------------
       ;; exit condition reached
       ;; add debit/credit totals to the table
       ;; ----------------------------------
       ((null? splits)
        (gnc:report-percent-done 100)

        (when reg-report-show-totals?
          (add-subtotal-row (G_ "Total Debits") leader table used-columns
                            debit-collector "grand-total" #f)
          (add-subtotal-row (G_ "Total Credits") leader table used-columns
                            credit-collector "grand-total" #f)
          (add-subtotal-row (G_ "Total Value Debits") leader table used-columns
                            debit-value "grand-total" #t)
          (add-subtotal-row (G_ "Total Value Credits") leader table used-columns
                            credit-value "grand-total" #t))
        (when ledger-type?
          (add-subtotal-row (G_ "Net Change") leader table used-columns
                            total-collector "grand-total" #f))
        (add-subtotal-row (G_ "Value Change") leader table used-columns
                          total-value "grand-total" #t))

       ;; The general journal has a split that doesn't have an account
       ;; set yet (the new entry transaction).
       ;; This split should be skipped or the report errors out.  See
       ;; bug #639082
       ((null? (xaccSplitGetAccount (car splits)))
        (loop (cdr splits) (1+ work-done) (not odd-row?)))

       ;; ----------------------------------
       ;; process the splits list
       ;; ----------------------------------
       (else
        (when (zero? (modulo work-done 200))
          (gnc:report-percent-done (* 100 (/ work-done work-to-do))))

        (let* ((current (car splits))
               (current-row-style (if (or multi-rows? odd-row?)
                                      "normal-row"
                                      "alternate-row")))
          ;; ----------------------------------------------
          ;; update totals, but don't add them to the table
          ;; ----------------------------------------------
          (for-each
           (lambda (split)
             (accumulate-totals split
                                total-collector total-value
                                debit-collector debit-value
                                credit-collector credit-value))
           (if multi-rows?
               (xaccTransGetSplitList (xaccSplitGetParent current))
               (list current)))
          ;; ----------------------------------
          ;; add the splits to the table
          ;; ----------------------------------
          (add-split-row table current used-columns
                         current-row-style #t (not multi-rows?)
                         action-for-num?  ledger-type?
                         double? (opt-val "Display" "Memo")
                         (opt-val "Display" "Description")
                         total-collector)
          (when multi-rows?
            (for-each
             (lambda (split)
               (add-split-row table split used-columns "alternate-row"
                              #f #t action-for-num? ledger-type? #f
                              (opt-val "Display" "Memo")
                              (opt-val "Display" "Description") total-collector))
             (xaccTransGetSplitList (xaccSplitGetParent current))))

          (loop (cdr splits)
                (1+ work-done)
                (not odd-row?))))))
    table))

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc-optiondb-lookup-value (gnc:report-options report-obj) section name))

  (let* ((document (gnc:make-html-document))
         (query-scm (opt-val "__reg" "query"))
         (journal? (opt-val "__reg" "journal"))
         (debit-string (opt-val "__reg" "debit-string"))
         (credit-string (opt-val "__reg" "credit-string"))
         (title (opt-val "General" "Title"))
         (query (gnc-scm2query query-scm)))

    (gnc:report-starting (G_ reportname))

    (qof-query-set-book query (gnc-get-current-book))

    (let* ((splits (if journal?
                       (xaccQueryGetSplitsUniqueTrans query)
                       (qof-query-run query)))
           (table (make-split-table splits
                                    (gnc:report-options report-obj)
                                    debit-string credit-string
                                    (G_ "Amount"))))
      (gnc:html-document-set-title! document title)
      (gnc:html-document-add-object! document table)
      (qof-query-destroy query))

    (gnc:report-finished)

    document))

(define register-report-guid "22104e02654c4adba844ee75a3f8d173")

;; we get called from elsewhere... but this doesn't work FIX-ME, find
;; out how to get report-guid's exported from report into the report
;; system at large. might have to define this at the report
;; level to get them read by other reports. Look at the aging reports
;; for suggestions, perhaps
(export register-report-guid)

(gnc:define-report
 'version 1
 'name reportname
 'report-guid register-report-guid
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #f)

(define (gnc:register-report-create-internal invoice? query journal? ledger-type?
                                             double? title debit-string credit-string)
  (define options (gnc:make-report-options register-report-guid))
  (define (set-option section name val)
    (gnc-set-option options section name val))

  (when invoice?
    (issue-deprecation-warning "gnc:register-report-create-internal: invoice \
option is obsolete")
    (set! journal? #f)
    (set-option "Display" "Account" #f))

  (set-option "General" "Title" title)
  (set-option "__reg" "query" query)
  (set-option "__reg" "journal" journal?)
  (set-option "__reg" "ledger-type" ledger-type?)
  (set-option "__reg" "double" double?)
  (set-option "__reg" "debit-string" debit-string)
  (set-option "__reg" "credit-string" credit-string)
  (gnc:make-report register-report-guid options))

(export gnc:register-report-create-internal)
