;; -*-scheme-*-
;; owner-report.scm -- Print out a detailed owner report, which is a
;;                     summary of invoices and payments for a particular
;;                     company (the owner) applied to an account.
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;; Copyright (c) 2002, 2003 Derek Atkins <warlord@MIT.EDU>
;; Modified by AMM to show tax figures of invoice.
;; Modified by Christopher Lam to combine job/owner-report
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


(define-module (gnucash report new-owner-report))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))
(use-modules (srfi srfi-11))             ;for let-values
(use-modules (gnucash gnc-module))
(use-modules (gnucash utilities))        ; for gnc:debug
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

;; Option names
(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-date-driver (N_ "Due or Post Date"))

(define owner-page gnc:pagename-general)
(define date-header (N_ "Date"))
(define due-date-header (N_ "Due Date"))
(define reference-header (N_ "Reference"))
(define type-header (N_ "Type"))
(define desc-header (N_ "Description"))
(define sale-header (N_ "Sale"))
(define tax-header (N_ "Tax"))
(define credit-header (N_ "Credits"))
(define debit-header (N_ "Debits"))
(define amount-header (N_ "Balance"))
(define linked-txns-header (N_ "Links"))

;; Depending on the report type we want to set up some lists/cases
;; with strings to ease overview and translation
(define owner-string-alist
  (list
   (list GNC-OWNER-CUSTOMER
         (N_ "Customer")
         (_ "No valid customer selected.")
         (_ "This report requires a customer to be selected."))

   (list GNC-OWNER-EMPLOYEE
         (N_ "Employee")
         (_ "No valid employee selected.")
         (_ "This report requires a employee to be selected."))

   (list GNC-OWNER-JOB
         (N_ "Job")
         (_ "No valid job selected.")
         (_ "This report requires a job to be selected."))

   (list GNC-OWNER-VENDOR
         (N_ "Vendor")
         (_ "No valid vendor selected.")
         (_ "This report requires a vendor to be selected."))))

(define (get-info key)
  (assv-ref owner-string-alist key))

;; Names in Option panel (Untranslated! Because it is used for option
;; naming and lookup only, and the display of the option name will be
;; translated somewhere else.)
(define (owner-string owner-type)
  (car (get-info owner-type)))

(define (date-col columns-used)
  (vector-ref columns-used 0))
(define (date-due-col columns-used)
  (vector-ref columns-used 1))
(define (num-col columns-used)
  (vector-ref columns-used 2))
(define (type-col columns-used)
  (vector-ref columns-used 3))
(define (memo-col columns-used)
  (vector-ref columns-used 4))
(define (sale-col columns-used)
  (vector-ref columns-used 5))
(define (tax-col columns-used)
  (vector-ref columns-used 6))
(define (credit-col columns-used)
  (vector-ref columns-used 7))
(define (debit-col columns-used)
  (vector-ref columns-used 8))
(define (value-col columns-used)
  (vector-ref columns-used 9))

(define columns-used-size 10)

(define (build-column-used options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
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
    (set-col (opt-val "Display Columns" date-header) 0)
    (set-col (opt-val "Display Columns" due-date-header) 1)
    (set-col (opt-val "Display Columns" reference-header) 2)
    (set-col (opt-val "Display Columns" type-header) 3)
    (set-col (opt-val "Display Columns" desc-header) 4)
    (set-col (opt-val "Display Columns" sale-header) 5)
    (set-col (opt-val "Display Columns" tax-header) 6)
    (set-col (opt-val "Display Columns" credit-header) 7)
    (set-col (opt-val "Display Columns" debit-header) 8)
    (set-col (opt-val "Display Columns" amount-header) 9)
    col-vector))

(define (make-heading-list column-vector link-option)
  (let ((heading-list '()))
    (if (date-col column-vector)
        (addto! heading-list (_ date-header)))
    (if (date-due-col column-vector)
        (addto! heading-list (_ due-date-header)))
    (if (num-col column-vector)
        (addto! heading-list (_ reference-header)))
    (if (type-col column-vector)
        (addto! heading-list (_ type-header)))
    (if (memo-col column-vector)
        (addto! heading-list (_ desc-header)))
    (if (sale-col column-vector)
        (addto! heading-list (_ sale-header)))
    (if (tax-col column-vector)
        (addto! heading-list (_ tax-header)))
    (if (credit-col column-vector)
        (addto! heading-list (_ credit-header)))
    (if (debit-col column-vector)
        (addto! heading-list (_ debit-header)))
    (if (value-col column-vector)
        (addto! heading-list (_ amount-header)))
    (case link-option
      ((simple)
       (addto! heading-list (_ linked-txns-header)))
      ((detailed)
       (addto! heading-list (_ "Date"))
       (addto! heading-list (_ "Details"))
       (addto! heading-list (_ "Amount"))))
    (reverse heading-list)))

(define num-buckets 6)
(define (new-bucket-vector)
  (make-vector num-buckets 0))

(define (txn-is-invoice? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-INVOICE))
(define (txn-is-payment? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-PAYMENT))

(define (make-aging-table splits to-date payable? date-type currency)
  (let ((table (gnc:make-html-table))
        (aging-list (gnc:owner-splits->aging-list
                     splits num-buckets to-date date-type (not payable?))))

    (gnc:html-table-set-col-headers!
     table (list (_ "Prepayments")
                 (_ "Current")
                 (_ "0-30 days")
                 (_ "31-60 days")
                 (_ "61-90 days")
                 (_ "91+ days")
                 (_ "Total")))

    (gnc:html-table-append-row!
     table (map (lambda (entry) (gnc:make-gnc-monetary currency entry))
                (reverse (cons (apply + aging-list) aging-list))))
    table))

;; addif is a macro; a simple procedure will always evaluate the
;; arguments pred? and elt which is not desirable; a macro will ensure
;; elt is only evaluated if pred? is not #f
(define-syntax-rule (addif pred? elt)
  (if pred? (list elt) '()))

(define (make-cell elt) (gnc:make-html-table-cell/markup "number-cell" elt))

;;
;; Make a row list based on the visible columns
;;
(define (add-row table odd-row? column-vector date due-date num type-str
                 memo currency amt credit debit sale tax link-rows)
  (define empty-cols
    (count identity
           (map (lambda (f) (f column-vector))
                (list date-col date-due-col num-col type-col
                      memo-col sale-col tax-col credit-col
                      debit-col value-col))))
  (define (cell amt)
    (and amt (make-cell (gnc:make-gnc-monetary currency amt))))
  (let lp ((link-rows link-rows)
           (first-row? #t))
    (unless (null? link-rows)
      (if first-row?
          (gnc:html-table-append-row/markup!
           table (if odd-row? "normal-row" "alternate-row")
           (append
            (addif (date-col column-vector) (qof-print-date date))
            (addif (date-due-col column-vector)
                   (and due-date (qof-print-date due-date)))
            (addif (num-col column-vector)    (gnc:html-string-sanitize num))
            (addif (type-col column-vector)   type-str)
            (addif (memo-col column-vector)   (gnc:html-string-sanitize memo))
            (addif (sale-col column-vector)   (cell sale))
            (addif (tax-col column-vector)    (cell tax))
            (addif (credit-col column-vector) (cell credit))
            (addif (debit-col column-vector)  (cell (and debit (- debit))))
            (addif (value-col column-vector)  (cell amt))
            (car link-rows)))
          (gnc:html-table-append-row/markup!
           table (if odd-row? "normal-row" "alternate-row")
           (cons
            (gnc:make-html-table-cell/size 1 empty-cols #f)
            (car link-rows))))
      (lp (cdr link-rows) #f))))

(define (add-owner-table table splits acc start-date end-date date-type
                         used-columns payable? link-option)
  (define currency (xaccAccountGetCommodity acc))
  (define link-cols (assq-ref '((none . 0) (simple . 1) (detailed . 3)) link-option))
  (define (print-totals total debit credit tax sale)
    (define (total-cell cell)
      (gnc:make-html-table-cell/markup "total-number-cell" cell))
    (define (make-cell amt)
      (total-cell (gnc:make-gnc-monetary currency amt)))
    (define span
      (count identity (map (lambda (f) (f used-columns))
                           (list memo-col type-col num-col date-due-col date-col))))
    ;; print period totals
    (if (or (sale-col used-columns) (tax-col used-columns)
            (credit-col used-columns) (debit-col used-columns))
        (gnc:html-table-append-row/markup!
         table "grand-total"
         (append
          (list (gnc:make-html-table-cell/markup
                 "total-label-cell" (_ "Period Totals")))
          (addif (>= span 2) (gnc:make-html-table-cell/size 1 (1- span) ""))
          (addif (sale-col used-columns)   (make-cell sale))
          (addif (tax-col used-columns)    (make-cell tax))
          (addif (credit-col used-columns) (make-cell credit))
          (addif (debit-col used-columns)  (make-cell (- debit)))
          (addif (value-col used-columns) (make-cell (+ credit debit)))
          (addif (> link-cols 0) (gnc:make-html-table-cell/size 1 link-cols #f)))))

    ;; print grand total
    (if (value-col used-columns)
        (gnc:html-table-append-row/markup!
         table "grand-total"
         (append
          (list (gnc:make-html-table-cell/markup
                 "total-label-cell"
                 (if (negative? total)
                     (_ "Total Credit")
                     (_ "Total Due")))
                (gnc:make-html-table-cell/size/markup
                 1 (value-col used-columns)
                 "total-number-cell"
                 (gnc:make-gnc-monetary currency total)))
          (addif (> link-cols 0)
                 (gnc:make-html-table-cell/size 1 link-cols #f)))))

    ;; print aging table
    (gnc:html-table-append-row/markup!
     table "grand-total"
     (list (gnc:make-html-table-cell/size
            1 (+ columns-used-size link-cols)
            (make-aging-table splits
                              end-date
                              payable? date-type currency)))))

  (define (add-balance-row odd-row? total)
    (add-row table odd-row? used-columns start-date #f "" (_ "Balance") ""
             currency total #f #f #f #f (list (make-list link-cols #f))))

  (define (make-invoice->payments-table invoice invoice-splits currency txn)
    (let lp ((invoice-splits invoice-splits) (result '()))
      (cond
       ((null? invoice-splits)
        (reverse
         (if (gncInvoiceIsPaid invoice)
             result
             (cons (list (gnc:make-html-table-cell/size 1 2 (_ "Outstanding"))
                         (make-cell
                          (gnc:make-gnc-monetary
                           currency (gnc-lot-get-balance
                                     (gncInvoiceGetPostedLot invoice)))))
                   result))))
       (else
        (let* ((lot-split (car invoice-splits))
               (lot-txn (xaccSplitGetParent lot-split))
               (tfr-splits (xaccTransGetPaymentAcctSplitList lot-txn)))
          (let lp1 ((tfr-splits tfr-splits) (result result))
            (cond
             ((equal? lot-txn txn) (lp (cdr invoice-splits) result))
             ((null? tfr-splits) (lp (cdr invoice-splits) result))
             (else
              (let* ((tfr-split (car tfr-splits))
                     (tfr-acct (xaccSplitGetAccount tfr-split))
                     (tfr-curr (xaccAccountGetCommodity tfr-acct))
                     (tfr-amt (xaccSplitGetAmount tfr-split)))
                (lp1 (cdr tfr-splits)
                     (cons (list
                            (qof-print-date (xaccTransGetDate lot-txn))
                            (let ((num (gnc-get-num-action lot-txn lot-split)))
                              (if (string-null? num) (_ "Payment") num))
                            (make-cell
                             (gnc:make-html-text
                              (gnc:html-markup-anchor
                               (gnc:split-anchor-text tfr-split)
                               (gnc:make-gnc-monetary tfr-curr tfr-amt)))))
                           result)))))))))))

  (define (make-payment->invoices-list invoice payment-splits)
    (list
     (list
      (apply
       gnc:make-html-text
       (map
        (lambda (inv-splits)
          (gnc:html-markup-anchor
           (gnc:invoice-anchor-text (car inv-splits))
           (gnc-get-num-action
            (gncInvoiceGetPostedTxn (car inv-splits))
            #f)))
        payment-splits)))))

  (define (make-payment->invoices-table amount payment-splits currency)
    (let lp ((payment-splits payment-splits)
             (amount (- amount))
             (result '()))
      (cond
       ((null? payment-splits)
        (reverse
         (if (positive? amount)
             (cons (list (gnc:make-html-table-cell/size 1 2 (_ "Prepayments"))
                         (make-cell (gnc:make-gnc-monetary currency amount)))
                   result)
             result)))
       (else
        (let* ((payment-split (car payment-splits))
               (inv (car payment-split))
               (inv-split (cadr payment-split))
               (inv-amount (xaccSplitGetAmount inv-split)))
          (lp (cdr payment-splits)
              (- amount inv-amount)
              (cons (list
                     (qof-print-date (gncInvoiceGetDatePosted inv))
                     (gnc:make-html-text
                      (gnc:html-markup-anchor
                       (gnc:invoice-anchor-text inv)
                       (gnc-get-num-action (gncInvoiceGetPostedTxn inv) #f)))
                     (make-cell (gnc:make-gnc-monetary currency inv-amount)))
                    result)))))))

  (define (split->type-str split)
    (let* ((txn (xaccSplitGetParent split))
           (invoice (gncInvoiceGetInvoiceFromTxn txn)))
      (cond
       ((and (txn-is-invoice? txn)
             (not (null? invoice)))
        (gnc:make-html-text
         (gnc:html-markup-anchor
          (gnc:invoice-anchor-text invoice)
          (gncInvoiceGetTypeString invoice))))
       ((txn-is-payment? txn)
        (apply gnc:make-html-text
               (map (lambda (pmt-split)
                      (gnc:html-markup-anchor
                       (gnc:split-anchor-text pmt-split)
                       (_ "Payment")))
                    (xaccTransGetPaymentAcctSplitList txn))))
       (else (_ "Unknown")))))

  (define (invoice->sale invoice)
    (and (not (null? invoice))
         ((if (gncInvoiceGetIsCreditNote invoice) - identity)
          (gncInvoiceGetTotalSubtotal invoice))))

  (define (invoice->tax invoice)
    (and (not (null? invoice))
         ((if (gncInvoiceGetIsCreditNote invoice) - identity)
          (gncInvoiceGetTotalTax invoice))))

  (define (invoice->due-date invoice)
    (and (not (null? invoice))
         (gncInvoiceIsPosted invoice)
         (gncInvoiceGetDateDue invoice)))

  (let lp ((printed? #f)
           (odd-row? #t)
           (splits splits)
           (total 0)
           (debit 0)
           (credit 0)
           (tax 0)
           (sale 0)
           (links '()))
    (cond

     ((null? splits)
      ;;Balance row may not have been added if all transactions were before
      ;;start-date (and no other rows would be added either) so add it now
      (when (and (not printed?) (value-col used-columns) (not (zero? total)))
        (add-balance-row odd-row? total))
      (print-totals total debit credit tax sale)
      (gnc:html-table-set-style!
       table "table"
       'attribute (list "border" 1)
       'attribute (list "cellspacing" 0)
       'attribute (list "cellpadding" 4))
      table)

     ;; not an invoice/payment. skip transaction.
     ((not (or (txn-is-invoice? (xaccSplitGetParent (car splits)))
               (txn-is-payment? (xaccSplitGetParent (car splits)))))
      (lp printed? odd-row? (cdr splits) total debit credit tax sale links))

     ;; invalid case: txn-type-invoice but no associated invoice, nor lot
     ((let* ((txn (xaccSplitGetParent (car splits)))
             (invoice (gncInvoiceGetInvoiceFromTxn txn)))
        (and (txn-is-invoice? txn)
             (or (null? invoice)
                 (null? (gncInvoiceGetPostedLot invoice)))))
      (gnc:warn "sanity check fail" txn)
      (lp printed? odd-row? (cdr splits) total debit credit tax sale links))

     ;; start printing txns.
     (else
      (let* ((split (car splits))
             (txn (xaccSplitGetParent split))
             (date (xaccTransGetDate txn))
             (value (xaccTransGetAccountAmount txn acc))
             (value (if payable? (- value) value))
             (invoice (gncInvoiceGetInvoiceFromTxn txn))
             (invoice-splits
              (and (txn-is-invoice? txn)
                   (gnc-lot-get-split-list
                    (gncInvoiceGetPostedLot invoice))))
             (payment-splits
              (and (txn-is-payment? txn)
                   (filter
                    (lambda (inv-split)
                      (member txn (map xaccSplitGetParent (cdr inv-split))))
                    links))))

        (cond
         ;; txn-date < start-date. skip display, accumulate amounts
         ((< date start-date)
          (lp printed? odd-row? (cdr splits) (+ total value)
              (if (negative? value) (+ debit value) debit)
              (if (negative? value) credit (+ credit value))
              tax sale (if (null? invoice) links
                           (acons invoice invoice-splits links))))

         ;; if balance row hasn't been rendered, consider
         ;; adding here.  skip if value=0.
         ((not printed?)
          (let ((print? (and (value-col used-columns) (not (zero? total)))))
            (if print? (add-balance-row odd-row? total))
            (lp #t (not print?) splits total debit credit tax sale links)))

         (else
          (add-row
           table odd-row? used-columns date (invoice->due-date invoice)
           (gnc-get-num-action txn split) (split->type-str split)
           (xaccSplitGetMemo split) currency (+ total value)
           (and (>= value 0) value) (and (< value 0) value)
           (invoice->sale invoice) (invoice->tax invoice)
           (cond
            ((and invoice-splits (eq? link-option 'simple))
             (if (gnc-lot-is-closed (gncInvoiceGetPostedLot invoice))
                 (list (list (_ "Paid")))
                 (list (list #f))))
            ((and invoice-splits (eq? link-option 'detailed))
             (make-invoice->payments-table invoice invoice-splits currency txn))
            ((and payment-splits (eq? link-option 'simple))
             (make-payment->invoices-list invoice payment-splits))
            ((and payment-splits (eq? link-option 'detailed))
             (make-payment->invoices-table value payment-splits currency))
            ;; some error occurred, show 1 line containing empty-list
            (else '(()))))

          (lp printed? (not odd-row?) (cdr splits) (+ total value)
              (if (negative? value) (+ debit value) debit)
              (if (negative? value) credit (+ credit value))
              (+ tax (or (invoice->tax invoice) 0))
              (+ sale (or (invoice->sale invoice) 0))
              (if (null? invoice) links
                  (acons invoice invoice-splits links))))))))))

(define (options-generator owner-type)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-owner-option
    owner-page (owner-string owner-type) "v"
    (N_ "The company for this report.")
    (lambda () '()) #f owner-type))

  (gnc:options-add-date-interval!
   gnc:*report-options* gnc:pagename-general
   optname-from-date optname-to-date "a")

  ;; Use a default report date of 'today'
  (gnc:option-set-default-value
   (gnc:lookup-option gnc:*report-options* gnc:pagename-general optname-to-date)
   (cons 'relative 'today))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") date-header
    "b" (N_ "Display the transaction date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") due-date-header
    "c" (N_ "Display the transaction date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") reference-header
    "d" (N_ "Display the transaction reference?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") type-header
    "g" (N_ "Display the transaction type?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") desc-header
    "ha" (N_ "Display the transaction description?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") sale-header
    "haa" (N_ "Display the sale amount column?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") tax-header
    "hab" (N_ "Display the tax column?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") credit-header
    "hac" (N_ "Display the period credits column?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") debit-header
    "had" (N_ "Display a period debits column?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") amount-header
    "hb" (N_ "Display the transaction amount?") #t))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Display Columns") linked-txns-header
    "hc" (N_ "Show linked transactions") 'none
    (list (vector 'none
                  (N_ "Disabled")
                  (N_ "Linked transactions are hidden."))
          (vector 'simple
                  (N_ "Simple")
                  (N_ "Invoices show if paid, payments show invoice numbers."))
          (vector 'detailed
                  (N_ "Detailed")
                  (N_ "Invoices show list of payments, payments show list of \
invoices and amounts.")))))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    gnc:pagename-general optname-date-driver "k"
    (N_ "Leading date.") 'duedate
    (list
     ;; Should be using standard label for due date?
     (vector 'duedate
             (N_ "Due Date")
             (N_ "Due date is leading."))
     ;; Should be using standard label for post date?
     (vector 'postdate
             (N_ "Post Date")
             (N_ "Post date is leading.")))))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (multiline-to-html-text str)
  ;; simple function - splits string containing #\newline into
  ;; substrings, and convert to a gnc:make-html-text construct which
  ;; adds gnc:html-markup-br after each substring.
  (let loop ((list-of-substrings (string-split str #\newline))
             (result '()))
    (if (null? list-of-substrings)
        (apply gnc:make-html-text (if (null? result) '() (reverse (cdr result))))
        (loop (cdr list-of-substrings)
              (cons* (gnc:html-markup-br) (car list-of-substrings) result)))))

(define (setup-query q owner accounts end-date job?)
  (let ((guid (gncOwnerReturnGUID (if job? owner (gncOwnerGetEndOwner owner))))
        (last-param (if job? QOF-PARAM-GUID OWNER-PARENTG)))
    (qof-query-add-guid-match
     q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER last-param)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT OWNER-FROM-LOT last-param)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER last-param)
     guid QOF-QUERY-OR)
    (xaccQueryAddAccountMatch q accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT q #f end-date #t end-date QOF-QUERY-AND)
    (qof-query-set-book q (gnc-get-current-book))
    (qof-query-set-sort-order q (list SPLIT-TRANS TRANS-DATE-POSTED) '() '())
    q))

(define (make-owner-table owner)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0)
     'attribute (list "valign" "top"))
    (gnc:html-table-append-row!
     table (multiline-to-html-text (gnc:owner-get-name-and-address-dep owner)))
    table))

(define (make-myname-table book date-format)
  (let* ((table (gnc:make-html-table))
         (name (gnc:company-info book gnc:*company-name*))
         (addy (gnc:company-info book gnc:*company-addy*)))

    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "align" "right")
     'attribute (list "valign" "top")
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))

    (when name
      (gnc:html-table-append-row! table (list name)))
    (when addy
      (gnc:html-table-append-row! table (multiline-to-html-text addy)))
    (gnc:html-table-append-row!
     table (list (gnc-print-time64 (gnc:get-today) date-format)))
    table))

(define (make-break! document)
  (gnc:html-document-add-object!
   document
   (gnc:make-html-text
    (gnc:html-markup-br))))

(define (reg-renderer report-obj type)
  (define options (gnc:report-options report-obj))
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let* ((accounts (filter (compose xaccAccountIsAPARType xaccAccountGetType)
                           (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account))))
         (start-date (gnc:time64-start-day-time
                      (gnc:date-option-absolute-time
                       (opt-val gnc:pagename-general optname-from-date))))
         (end-date (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (opt-val gnc:pagename-general optname-to-date))))
         (book (gnc-get-current-book))
         (date-format (gnc:options-fancy-date (gnc-get-current-book)))
         (used-columns (build-column-used options))
         (link-option
          (gnc:option-value
           (gnc:lookup-option options "Display Columns" linked-txns-header)))
         (owner-descr (owner-string type))
         (date-type (opt-val gnc:pagename-general optname-date-driver))
         (owner (opt-val owner-page owner-descr))
         (payable? (memv (gncOwnerGetType (gncOwnerGetEndOwner owner))
                         (list GNC-OWNER-VENDOR GNC-OWNER-EMPLOYEE)))
         (query (qof-query-create-for-splits))
         (document (gnc:make-html-document))
         (table (gnc:make-html-table))
         (headings (make-heading-list used-columns link-option))
         (report-title (string-append (owner-string type) " " (_ "Report"))))

    (cond
     ((not (gncOwnerIsValid owner))
      (gnc:html-document-add-object!
       document (gnc:html-make-generic-warning
                 report-title (gnc:report-id report-obj)
                 (cadr (get-info type)) (caddr (get-info type)))))

     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-generic-warning
        (string-append report-title ": " (gncOwnerGetName owner))
        (gnc:report-id report-obj)
        (_ "No valid account found")
        (_ "This report requires a valid AP/AR account to be available."))))

     (else
      (setup-query query owner accounts end-date (eqv? GNC-OWNER-JOB type))

      (let ((splits (xaccQueryGetSplitsUniqueTrans query)))
        (qof-query-destroy query)

        (gnc:html-document-set-headline!
         document (gnc:html-markup
                   "span" (owner-string type) " " (_ "Report:") " "
                   (gnc:html-markup-anchor
                    (if (eqv? GNC-OWNER-JOB type)
                        (gnc:job-anchor-text (gncOwnerGetJob owner))
                        (gnc:owner-anchor-text owner))
                    (gncOwnerGetName owner))))

        (cond
         ((null? splits)

          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-h2 (_ "No transactions found."))
            (gnc:html-markup-p
             (format #f (_ "No transactions were found associated with the ~a.")
                     (string-downcase (car (get-info type)))))
            (gnc:html-make-options-link (gnc:report-id report-obj)))))

         (else
          ;; loops in 2 passes: 1st loop. for each APAR account, filter
          ;; splits into each account. accumulate non-null results into
          ;; accounts-and-splits accumulator.
          (let loop ((accounts accounts)
                     (accounts-and-splits '())
                     (splits splits))
            (cond
             ((null? accounts)

              ;; 2nd loop: for-each accounts-and-splits accumulator, add
              ;; owner-txns into the html-table. only show header if >1
              ;; account has splits.
              (for-each
               (lambda (acc-splits-pair)
                 (let* ((account (car acc-splits-pair))
                        (splits (cdr acc-splits-pair)))

                   (when (> (length accounts-and-splits) 1)
                     (gnc:html-table-append-row!
                      table (gnc:make-html-table-cell/size
                             1 (length headings)
                             (gnc:make-html-text
                              (gnc:html-markup-h3
                               (string-append (_ "Account") ": "
                                              (xaccAccountGetName account)))))))

                   (add-owner-table table splits account start-date end-date
                                    date-type used-columns payable? link-option)))
               accounts-and-splits))

             (else
              ;; each 1st loop will slice splits into account-splits and
              ;; non-account splits, add to accounts-and-splits; and send
              ;; the non-account splits to be processed in the next loop
              ;; iteration.
              (let-values (((acc-splits other-acc-splits)
                            (partition
                             (lambda (split)
                               (equal? (car accounts) (xaccSplitGetAccount split)))
                             splits)))

                (loop (cdr accounts)
                      (if (null? acc-splits)
                          accounts-and-splits
                          (cons (cons (car accounts) acc-splits)
                                accounts-and-splits))
                      other-acc-splits)))))

          (gnc:html-document-add-object!
           document (make-myname-table book date-format))

          (gnc:html-document-add-object!
           document (make-owner-table owner))

          (gnc:html-document-add-object!
           document (gnc:make-html-text
                     (string-append (_ "Date Range") ": " (qof-print-date start-date)
                                    " - " (qof-print-date end-date))))

          (make-break! document)

          (gnc:html-table-set-col-headers! table headings)

          (gnc:html-document-add-object! document table))))))

    document))

(define (customer-renderer obj)
  (reg-renderer obj GNC-OWNER-CUSTOMER))

(define (vendor-renderer  obj)
  (reg-renderer obj GNC-OWNER-VENDOR))

(define (employee-renderer obj)
  (reg-renderer obj GNC-OWNER-EMPLOYEE))

(define (job-renderer obj)
  (reg-renderer obj GNC-OWNER-JOB))

(gnc:define-report
 'version 1
 'name (N_ "Customer Report (beta)")
 'report-guid "c146317be32e4948a561ec7fc89d15c1-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (options-generator GNC-OWNER-CUSTOMER))
 'renderer customer-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Vendor Report (beta)")
 'report-guid "d7d1e53505ee4b1b82efad9eacedaea0-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (options-generator GNC-OWNER-VENDOR))
 'renderer vendor-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Employee Report (beta)")
 'report-guid "08ae9c2e884b4f9787144f47eacd7f44-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (options-generator GNC-OWNER-EMPLOYEE))
 'renderer employee-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Job Report (beta)")
 'report-guid "5518ac227e474f47a34439f2d4d049de-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator (lambda () (options-generator GNC-OWNER-JOB))
 'renderer job-renderer
 'in-menu? #t)
