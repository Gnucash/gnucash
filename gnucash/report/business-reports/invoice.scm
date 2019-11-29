;; -*-scheme-*-
;; invoice.scm -- an Invoice Report, used to print a GncInvoice
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;; Copyright (c) 2002, 2003 Derek Atkins <warlord@MIT.EDU>
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


(define-module (gnucash report invoice))

(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash utilities))

(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(define (addif pred . data) (if pred data '()))

(define base-css "/* advanced users only */
.div-align-right { float: right; }
.div-align-right .maybe-align-right { text-align: right }
.entries-table * { border-width: 1px; border-style:solid; border-collapse: collapse}
.entries-table > table { width: 100% }
.company-table > table * { padding: 0px; }
.client-table > table * { padding: 0px; }
.invoice-details-table > table * { padding: 0px; }
@media print { .main-table > table { width: 100%; }}
")

(define (date-col columns-used)
  (vector-ref columns-used 0))
(define (description-col columns-used)
  (vector-ref columns-used 1))
(define (action-col columns-used)
  (vector-ref columns-used 2))
(define (quantity-col columns-used)
  (vector-ref columns-used 3))
(define (price-col columns-used)
  (vector-ref columns-used 4))
(define (discount-col columns-used)
  (vector-ref columns-used 5))
(define (tax-col columns-used)
  (vector-ref columns-used 6))
(define (taxvalue-col columns-used)
  (vector-ref columns-used 7))
(define (value-col columns-used)
  (vector-ref columns-used 8))

(define (num-columns-required columns-used)
  ;; count number of columns where (vector-ref columns-used col) is #t
  (count identity (vector->list columns-used)))

(define (build-column-used options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (vector
   (opt-val "Display Columns" "Date")
   (opt-val "Display Columns" "Description")
   (opt-val "Display Columns" "Action")
   (opt-val "Display Columns" "Quantity")
   (opt-val "Display Columns" "Price")
   (opt-val "Display Columns" "Discount")
   (opt-val "Display Columns" "Taxable")
   (opt-val "Display Columns" "Tax Amount")
   (opt-val "Display Columns" "Total")))

(define (make-heading-list column-vector)
  (append
   (addif (date-col column-vector)
          (_ "Date"))
   (addif (description-col column-vector)
          (_ "Description"))
   (addif (action-col column-vector)
          (_ "Action"))
   (addif (quantity-col column-vector)
          (_ "Quantity"))
   (addif (price-col column-vector)
          (_ "Unit Price"))
   (addif (discount-col column-vector)
          (_ "Discount"))
   (addif (tax-col column-vector)
          (_ "Taxable"))
   (addif (taxvalue-col column-vector)
          (_ "Tax Amount"))
   (addif (value-col column-vector)
          (_ "Total"))))

(define (monetary-or-percent numeric currency entry-type)
  (if (eqv? entry-type GNC-AMT-TYPE-PERCENT)
      (string-append (gnc:default-html-gnc-numeric-renderer numeric #f) " " (_ "%"))
      (gnc:make-gnc-monetary currency numeric)))

(define layout-key-list
  ;; Translators: "Their details" refer to the invoice 'other party' details i.e. client/vendor name/address/ID
  (list (cons 'client (list (cons 'text (_ "Their details"))
                            (cons 'tip (_ "Client or vendor name, address and ID"))))

        ;; Translators: "Our details" refer to the book owner's details i.e. name/address/tax-ID
        (cons 'company (list (cons 'text (_ "Our details"))
                             (cons 'tip (_ "Company name, address and tax-ID"))))

        (cons 'invoice (list (cons 'text (_ "Invoice details"))
                             (cons 'tip (_ "Invoice date, due date, billing ID, terms, job details"))))

        (cons 'today (list (cons 'text (_ "Today's date"))
                           (cons 'tip (_ "Today's date"))))

        (cons 'picture (list (cons 'text (_ "Picture"))
                             (cons 'tip (_ "Picture"))))

        ;; Translators: "(empty)" refers to invoice header section being left blank
        (cons 'none (list (cons 'text (_ "(empty)"))
                          (cons 'tip (_ "Empty space"))))))

(define variant-list
  (list
   (cons 'invoice (list (cons '1a 'none)
                        (cons '1b 'invoice)
                        (cons '2a 'client)
                        (cons '2b 'company)
                        (cons '3a 'none)
                        (cons '3b 'today)
                        (cons 'css base-css)))

   (cons 'easy-invoice (list (cons '1a 'none)
                             (cons '1b 'invoice)
                             (cons '2a 'client)
                             (cons '2b 'company)
                             (cons '3a 'none)
                             (cons '3b 'today)
                             (cons 'css (string-append base-css "
.invoice-in-progress { color:red }
.invoice-title { font-weight: bold; text-decoration: underline }
.main-table > table { margin: auto }
.invoice-details-table > table { display: block; }
.invoice-notes { margin-top: 20px }
.entries-table > table { min-width: 600px }"))))

   (cons 'fancy-invoice (list (cons '1a 'company)
                              (cons '1b 'invoice)
                              (cons '2a 'client)
                              (cons '2b 'company)
                              (cons '3a 'none)
                              (cons '3b 'none)
                              (cons 'css (string-append base-css "
.company-name {font-size: x-large; }
.client-name {font-size: x-large; }"))))))

(define (keylist-get-info keylist key info)
  (cdr (assq info (cdr (assq key keylist)))))

(define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))

(define (multiline-to-html-text str)
  (gnc:multiline-to-html-text str))

(define (options-generator variant)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-invoice-option gnc:pagename-general gnc:optname-invoice-number "x" ""
                            (lambda () '()) #f))

  (gnc:register-inv-option
   (gnc:make-string-option
    gnc:pagename-general (N_ "Custom Title")
    "z" (N_ "A custom string to replace Invoice, Bill or Expense Voucher.")
    ""))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Layout") (N_ "CSS") "zz" (N_ "CSS code. This field specifies the CSS code \
for styling the invoice. Please see the exported report for the CSS class names.")
    (keylist-get-info variant-list variant 'css)))

  (gnc:register-inv-option
   (gnc:make-pixmap-option
    (N_ "Layout") (N_ "Picture Location") "zy" (N_ "Location for Picture")
    ""))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Date")
    "b" (N_ "Display the date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Description")
    "d" (N_ "Display the description?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Action")
    "g" (N_ "Display the action?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Quantity")
    "ha" (N_ "Display the quantity of items?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Price")
    "hb" (N_ "Display the price per item?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Discount")
    "k" (N_ "Display the entry's discount?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Taxable")
    "l" (N_ "Display the entry's taxable status?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Tax Amount")
    "m" (N_ "Display each entry's total total tax?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Total")
    "n" (N_ "Display the entry's value?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Due Date")
    "c" (N_ "Display due date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Subtotal")
    "d" (N_ "Display the subtotals?") #t))

  (gnc:register-inv-option
   (gnc:make-complex-boolean-option
    (N_ "Display") (N_ "Payable to")
    "ua1" (N_ "Display the Payable to: information.") #f #f
    (lambda (x)
      (gnc-option-db-set-option-selectable-by-name
       gnc:*report-options* "Display" "Payable to string" x))))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Display") (N_ "Payable to string")
    "ua2" (N_ "The phrase for specifying to whom payments should be made.")
    (_ "Please make all checks payable to")))

  (gnc:register-inv-option
   (gnc:make-complex-boolean-option
    (N_ "Display") (N_ "Company contact")
    "ub1" (N_ "Display the Company contact information.") #f #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
                 gnc:*report-options* "Display" "Company contact string" x))))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Display") (N_ "Company contact string")
    "ub2" (N_ "The phrase used to introduce the company contact.")
    (_ "Please direct all enquiries to")))

  (gnc:register-inv-option
   (gnc:make-number-range-option
    (N_ "Display") (N_ "Minimum # of entries")
    "zz" (N_ "The minimum number of invoice entries to display.") 1
    0 23 0 1))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Use Detailed Tax Summary")
    "o" (N_ "Display all tax categories separately (one per line) instead of one single tax line.?") #f))

  (gnc:register-inv-option (gnc:make-internal-option "Display" "Totals" #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "References")
    "s" (N_ "Display the invoice references?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Billing Terms")
    "t" (N_ "Display the invoice billing terms?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Billing ID")
    "ta" (N_ "Display the billing id?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Invoice owner ID")
    "tam" (N_ "Display the customer/vendor id?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Invoice Notes")
    "tb" (N_ "Display the invoice notes?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Payments")
    "tc" (N_ "Display the payments applied to this invoice?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Job Details")
    "td" (N_ "Display the job name for this invoice?") #f))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Display") (N_ "Extra Notes")
    "u" (N_ "Extra notes to put on the invoice.")
    (_ "Thank you for your patronage!")))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 1 Left")
    "1a" "1st row, left"
    (keylist-get-info variant-list variant '1a)
    (keylist->vectorlist layout-key-list)))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 1 Right")
    "1b" "1st row, right"
    (keylist-get-info variant-list variant '1b)
    (keylist->vectorlist layout-key-list)))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 2 Left")
    "2a" "2nd row, left"
    (keylist-get-info variant-list variant '2a)
    (keylist->vectorlist layout-key-list)))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 2 Right")
    "2b" "2nd row, right"
    (keylist-get-info variant-list variant '2b)
    (keylist->vectorlist layout-key-list)))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 3 Left")
    "3a" "3rd row, left"
    (keylist-get-info variant-list variant '3a)
    (keylist->vectorlist layout-key-list)))

  (gnc:register-inv-option
   (gnc:make-multichoice-option
    (N_ "Layout") (N_ "Row 3 Right")
    "3b" "3rd row, right"
    (keylist-get-info variant-list variant '3b)
    (keylist->vectorlist layout-key-list)))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)


(define (make-entry-table invoice options cust-doc? credit-note?)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let ((show-payments (opt-val "Display" "Payments"))
        (display-all-taxes (opt-val "Display" "Use Detailed Tax Summary"))
        (display-subtotal? (opt-val "Display" "Subtotal"))
        (lot (gncInvoiceGetPostedLot invoice))
        (txn (gncInvoiceGetPostedTxn invoice))
        (currency (gncInvoiceGetCurrency invoice))
        (reverse-payments? (not (gncInvoiceAmountPositive invoice))))

    (define (display-subtotal monetary used-columns)
      (if (value-col used-columns)
          monetary
          (let ((amt (gnc:gnc-monetary-amount monetary)))
            (if amt
                (if (negative? amt)
                    (gnc:monetary-neg monetary)
                    monetary)
                monetary))))

    (define (add-payment-row table used-columns split total-collector reverse-payments?)
      (let* ((t (xaccSplitGetParent split))
             (currency (xaccTransGetCurrency t))
             ;; Depending on the document type, the payments may need to be sign-reversed
             (amt (gnc:make-gnc-monetary currency
                                         (if reverse-payments?
                                             (- (xaccSplitGetValue split))
                                             (xaccSplitGetValue split)))))

        (total-collector 'add
                         (gnc:gnc-monetary-commodity amt)
                         (gnc:gnc-monetary-amount amt))

        (gnc:html-table-append-row/markup!
         table "grand-total"
         (append
          (addif (date-col used-columns)
                 (qof-print-date (xaccTransGetDate t)))

          (addif (description-col used-columns)
                 (_ "Payment, thank you!"))

          (list (gnc:make-html-table-cell/size/markup
                 1 (- (max 3 (num-columns-required used-columns))
                      (if (date-col used-columns) 1 0)
                      (if (description-col used-columns) 1 0))
                 "total-number-cell"
                 (display-subtotal amt used-columns)))))))

    (let* ((table (gnc:make-html-table))
           (used-columns (build-column-used options))
           (entries (gncInvoiceGetEntries invoice)))

      (define (add-entry-row entry row-style)
        (gnc:html-table-append-row/markup!
         table row-style
         (append
          (addif (date-col used-columns)
                 (qof-print-date (gncEntryGetDate entry)))

          (addif (description-col used-columns)
                 (gncEntryGetDescription entry))

          (addif (action-col used-columns)
                 (gncEntryGetAction entry))

          (addif (quantity-col used-columns)
                 (gnc:make-html-table-cell/markup
                  "number-cell"
                  (gncEntryGetDocQuantity entry credit-note?)))

          (addif (price-col used-columns)
                 (gnc:make-html-table-cell/markup
                  "number-cell"
                  (gnc:make-gnc-monetary
                   currency (if cust-doc?
                                (gncEntryGetInvPrice entry)
                                (gncEntryGetBillPrice entry)))))

          (addif (discount-col used-columns)
                 (if cust-doc?
                     (gnc:make-html-table-cell/markup
                      "number-cell"
                      (monetary-or-percent (gncEntryGetInvDiscount entry)
                                           currency
                                           (gncEntryGetInvDiscountType entry)))
                     ""))

          (addif (tax-col used-columns)
                 (if (if cust-doc?
                         (and (gncEntryGetInvTaxable entry)
                              (gncEntryGetInvTaxTable entry))
                         (and (gncEntryGetBillTaxable entry)
                              (gncEntryGetBillTaxTable entry)))
                     ;; Translators: This "T" is displayed in the taxable column, if this entry contains tax
                     (_ "T") ""))

          (addif (taxvalue-col used-columns)
                 (gnc:make-html-table-cell/markup
                  "number-cell"
                  (gnc:make-gnc-monetary
                   currency (gncEntryGetDocTaxValue entry #t cust-doc? credit-note?))))

          (addif (value-col used-columns)
                 (gnc:make-html-table-cell/markup
                  "number-cell"
                  (gnc:make-gnc-monetary
                   currency (gncEntryGetDocValue entry #t cust-doc? credit-note?)))))))

      (define (add-subtotal-row subtotal subtotal-style subtotal-label)
        (gnc:html-table-append-row/markup!
         table subtotal-style
         (list (gnc:make-html-table-cell/markup
                "total-label-cell" subtotal-label)
               (gnc:make-html-table-cell/size/markup
                1 (max 3 (num-columns-required used-columns))
                "total-number-cell"
                (display-subtotal (gnc:make-gnc-monetary currency subtotal) used-columns)))))

      (gnc:html-table-set-col-headers! table
                                       (make-heading-list used-columns))

      (let do-rows-with-subtotals ((entries entries)
                                   (odd-row? #t)
                                   (num-entries 0))
        (if (null? entries)

            ;; all entries done, add subtotals
            (let ((total-collector (gnc:make-commodity-collector)))

              ;; minimum number of entries- replicating fancy-invoice option
              (let loop ((num-entries-left (- (opt-val "Display" "Minimum # of entries" ) num-entries))
                         (odd-row? odd-row?))
                (when (positive? num-entries-left)
                  (gnc:html-table-append-row/markup!
                   table (if odd-row? "normal-row" "alternate-row")
                   (gnc:html-make-empty-cells (num-columns-required used-columns)))
                  (loop (1- num-entries-left)
                        (not odd-row?))))

              (if display-subtotal?
                  (add-subtotal-row (gncInvoiceGetTotalSubtotal invoice)
                                    "grand-total" (_ "Net Price")))

              (if display-all-taxes
                  (for-each
                   (lambda (parm)
                     (let ((value (cdr parm))
                           (acct (car parm)))
                       (add-subtotal-row value
                                         "grand-total" (xaccAccountGetName acct))))
                   (gncInvoiceGetTotalTaxList invoice))

                  ;; nope, just show the total tax.
                  (add-subtotal-row (gncInvoiceGetTotalTax invoice)
                                    "grand-total" (_ "Tax")))

              (add-subtotal-row (gncInvoiceGetTotal invoice)
                                "grand-total" (_ "Total Price"))

              (total-collector 'add currency (gncInvoiceGetTotal invoice))

              (if (and show-payments (not (null? lot)))
                  (let ((splits (sort-list!
                                 (gnc-lot-get-split-list lot)
                                 (lambda (s1 s2)
                                   (let ((t1 (xaccSplitGetParent s1))
                                         (t2 (xaccSplitGetParent s2)))
                                     (< (xaccTransOrder t1 t2) 0))))))
                    (for-each
                     (lambda (split)
                       (if (not (equal? (xaccSplitGetParent split) txn))
                           (add-payment-row table used-columns
                                            split total-collector
                                            reverse-payments?)))
                     splits)))

              (add-subtotal-row (cadr (total-collector 'getpair currency #f))
                                "grand-total" (_ "Amount Due")))

            (begin

              (add-entry-row (car entries)
                             (if odd-row? "normal-row" "alternate-row"))

              (do-rows-with-subtotals (cdr entries)
                                      (not odd-row?)
                                      (1+ num-entries)))))

      table)))

(define (make-invoice-details-table invoice options)
  ;; dual-column. invoice date/due, billingID, terms, job name/number
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (let* ((invoice-details-table (gnc:make-html-table))
         (book (gncInvoiceGetBook invoice))
         (date-format (gnc:options-fancy-date book))
         (jobnumber (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner invoice))))
         (jobname (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner invoice)))))

    (if (gncInvoiceIsPosted invoice)

        (begin
          (gnc:html-table-append-row!
           invoice-details-table
           (make-date-row (_ "Date") (gncInvoiceGetDatePosted invoice) date-format))

          (if (opt-val "Display" "Due Date")
              (gnc:html-table-append-row!
               invoice-details-table
               (make-date-row (_ "Due Date") (gncInvoiceGetDateDue invoice) date-format))))

        (gnc:html-table-append-row! invoice-details-table
                                    (gnc:make-html-table-cell/size
                                     1 2 (gnc:make-html-span/markup
                                          "invoice-in-progress"
                                          (gnc:make-html-text
                                           (_ "Invoice in progress..."))))))

    (if (opt-val "Display" "Billing ID")
        (let ((billing-id (gncInvoiceGetBillingID invoice)))
          (if (and billing-id (not (string-null? billing-id)))
              (begin
                (gnc:html-table-append-row! invoice-details-table
                                            (list
                                             (_ "Reference:")
                                             (gnc:make-html-div/markup
                                              "div-align-right"
                                              (multiline-to-html-text billing-id))))
                (gnc:html-table-append-row! invoice-details-table '())))))

    (if (opt-val "Display" "Billing Terms")
        (let* ((term (gncInvoiceGetTerms invoice))
               (terms (gncBillTermGetDescription term)))
          (if (and terms (not (string-null? terms)))
              (gnc:html-table-append-row! invoice-details-table
                                          (list
                                           (_ "Terms:")
                                           (gnc:make-html-div/markup
                                            "div-align-right"
                                            (multiline-to-html-text terms)))))))

    ;; Add job number and name to invoice if requested and if it exists
    (if (and (opt-val "Display" "Job Details")
             (not (string-null? jobnumber)))
        (begin
          (gnc:html-table-append-row! invoice-details-table
                                      (list (_ "Job number:")
                                            (gnc:make-html-div/markup
                                             "div-align-right"
                                             jobnumber)))
          (gnc:html-table-append-row! invoice-details-table
                                      (list (_ "Job name:")
                                            (gnc:make-html-div/markup
                                             "div-align-right"
                                             jobname)))))
    invoice-details-table))

(define (make-img img-url)
  ;; just an image
  (gnc:make-html-text
   (gnc:html-markup-img
    (make-file-url img-url))))

(define (make-client-table owner orders options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  ;; this is a single-column table.
  (let ((table (gnc:make-html-table)))

    (gnc:html-table-append-row! table
                                (list
                                 (gnc:make-html-div/markup
                                  "maybe-align-right client-name"
                                  (gnc:owner-get-name-dep owner))))

    (gnc:html-table-append-row! table
                                (list
                                 (gnc:make-html-div/markup
                                  "maybe-align-right client-address"
                                  (multiline-to-html-text
                                   (gnc:owner-get-address-dep owner)))))

    (if (opt-val "Display" "Invoice owner ID")
        (gnc:html-table-append-row! table
                                    (list
                                     (gnc:make-html-div/markup
                                      "maybe-align-right client-id"
                                      (multiline-to-html-text
                                       (gnc:owner-get-owner-id owner))))))

    (for-each
     (lambda (order)
       (let ((reference (gncOrderGetReference order)))
         (if (and reference (not (string-null? reference)))
             (gnc:html-table-append-row! table
                                         (list (string-append
                                                (_ "REF") " "
                                                reference))))))
     orders)

    table))

(define (make-date-row label date date-format)
  (list
   (string-append label ":")
   (gnc:make-html-div/markup
    "div-align-right"
    (gnc-print-time64 date date-format))))

(define (make-company-table book)
  ;; single-column table. my name, address, and printdate
  (let* ((table (gnc:make-html-table))
         (name (gnc:company-info book gnc:*company-name*))
         (addy (gnc:company-info book gnc:*company-addy*))
         (phone (gnc:company-info book gnc:*company-phone*))
         (fax (gnc:company-info book gnc:*company-fax*))
         (email (gnc:company-info book gnc:*company-email*))
         (url (gnc:company-info book gnc:*company-url*))
         (taxnr (gnc:option-get-value book gnc:*tax-label* gnc:*tax-nr-label*))
         (taxid (gnc:company-info book gnc:*company-id*)))

    (if (and name (not (string-null? name)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-name" name))))

    (if (and addy (not (string-null? addy)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-address" (multiline-to-html-text addy)))))

    (if (and phone (not (string-null? phone)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                           "maybe-align-right company-phone" phone))))

    (if (and fax (not (string-null? fax)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-fax" fax))))

    (if (and email (not (string-null? email)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-email" email))))

    (if (and url (not (string-null? url)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-url" url))))

    (if (and taxid (not (string-null? taxid)))
        (gnc:html-table-append-row! table (list
                                           (gnc:make-html-div/markup
                                            "maybe-align-right company-tax-id" taxid))))

    (if (and taxnr (not (string-null? taxnr)))
        (gnc:html-table-append-row!
         table (list (gnc:make-html-div/markup
                      "maybe-align-right company-tax-nr" taxnr))))

    table))

(define (reg-renderer report-obj)
  (let* ((document (gnc:make-html-document))
         (options (gnc:report-options report-obj))
         (opt-val (lambda (section name) (gnc:option-value (gnc:lookup-option options section name))))
         (invoice (opt-val gnc:pagename-general gnc:optname-invoice-number))
         (references? (opt-val "Display" "References"))
         (custom-title (opt-val gnc:pagename-general "Custom Title")))

    (if (null? invoice)
        (gnc:html-document-add-object! document
                                       (gnc:make-html-text
                                        (_ "No valid invoice selected. Click on the Options button and select the invoice to use.")))
        (let* ((book (gncInvoiceGetBook invoice))
               (owner (gncInvoiceGetOwner invoice))
               (type (gncInvoiceGetType invoice))
               (orders (if references? (delete-duplicates (map gncEntryGetOrder (gncInvoiceGetEntries invoice))) '()))
               (cust-doc? (memv type (list GNC-INVOICE-CUST-INVOICE GNC-INVOICE-CUST-CREDIT-NOTE)))
               (credit-note? (memv type (list GNC-INVOICE-CUST-CREDIT-NOTE GNC-INVOICE-VEND-CREDIT-NOTE GNC-INVOICE-EMPL-CREDIT-NOTE)))
               (default-title (cond
                               ((eqv? type GNC-INVOICE-VEND-INVOICE)
                                (_ "Bill"))
                               ((eqv? type GNC-INVOICE-EMPL-INVOICE)
                                (_ "Expense Voucher"))
                               ((memv type (list GNC-INVOICE-CUST-CREDIT-NOTE
                                                 GNC-INVOICE-VEND-CREDIT-NOTE
                                                 GNC-INVOICE-EMPL-CREDIT-NOTE))
                                (_ "Credit Note"))
                               (else
                                (_ "Invoice"))))
               (title (if (string-null? custom-title) default-title custom-title))
               ;; Translators: This is the format of the invoice title.
               ;; The first ~a is "Invoice", "Credit Note"... and the second the number.
               ;; Replace " #" by whatever is common as number abbreviation, i.e. "~a Nr. ~a"
               (invoice-title (format #f (_"~a #~a") title (gncInvoiceGetID invoice)))
               (layout-lookup-table (list (cons 'none #f)
                                          (cons 'picture (gnc:make-html-div/markup
                                                          "picture"
                                                          (make-img (opt-val "Layout" "Picture Location"))))
                                          (cons 'invoice (gnc:make-html-div/markup
                                                          "invoice-details-table"
                                                          (make-invoice-details-table
                                                           invoice options)))
                                          (cons 'client (gnc:make-html-div/markup
                                                         "client-table"
                                                         (make-client-table
                                                          owner orders options)))
                                          (cons 'company (gnc:make-html-div/markup
                                                          "company-table"
                                                          (make-company-table book)))
                                          (cons 'today (gnc:make-html-div/markup
                                                        "invoice-print-date"
                                                        (qof-print-date (current-time))))))
               (layout-lookup (lambda (loc) (cdr (assq (opt-val "Layout" loc) layout-lookup-table)))))

          (gnc:html-document-set-style-text! document (opt-val "Layout" "CSS"))

          (let ((main-table (gnc:make-html-table)))

            (gnc:html-table-append-row! main-table
                                        (gnc:make-html-table-cell/size
                                         1 2 (gnc:make-html-div/markup
                                              "invoice-title" invoice-title)))

            (gnc:html-table-append-row! main-table
                                        (list (layout-lookup "Row 1 Left")
                                              (gnc:make-html-div/markup
                                               "div-align-right"
                                               (layout-lookup "Row 1 Right"))))

            (gnc:html-table-append-row! main-table
                                        (list (layout-lookup "Row 2 Left")
                                              (gnc:make-html-div/markup
                                               "div-align-right"
                                               (layout-lookup "Row 2 Right"))))

            (gnc:html-table-append-row! main-table
                                        (list (layout-lookup "Row 3 Left")
                                              (gnc:make-html-div/markup
                                               "div-align-right"
                                               (layout-lookup "Row 3 Right"))))

            (gnc:html-table-append-row! main-table
                                        (gnc:make-html-table-cell/size
                                         1 2 (gnc:make-html-div/markup
                                              "entries-table"
                                              (make-entry-table invoice options
                                                                cust-doc? credit-note?))))

            (if (opt-val "Display" "Invoice Notes")
                (let ((notes (gncInvoiceGetNotes invoice)))
                  (gnc:html-table-append-row! main-table
                                              (gnc:make-html-table-cell/size
                                               1 2 (gnc:make-html-div/markup
                                                    "invoice-notes"
                                                    (multiline-to-html-text notes))))))

            (if (opt-val "Display" "Payable to")
                (let* ((name (gnc:company-info book gnc:*company-name*))
                       (name-str (opt-val "Display" "Payable to string")))
                  (if (and name (not (string-null? name)))
                      (gnc:html-table-append-row!
                       main-table
                       (gnc:make-html-div/markup
                        "invoice-footer-payable-to"
                        (multiline-to-html-text
                         (string-append name-str ": " name)))))))

            (if (opt-val "Display" "Company contact")
                (let* ((contact (gnc:company-info book gnc:*company-contact*))
                       (contact-str (opt-val "Display" "Company contact string")))
                  (if (and contact (not (string-null? contact)))
                      (gnc:html-table-append-row!
                       main-table
                       (gnc:make-html-div/markup
                        "invoice-footer-company-contact"
                        (multiline-to-html-text
                         (string-append contact-str  ": " contact)))))))

            (gnc:html-table-append-row! main-table
                                        (gnc:make-html-table-cell/size
                                         1 2 (gnc:make-html-div/markup
                                              "invoice-notes"
                                              (multiline-to-html-text
                                               (opt-val "Display" "Extra Notes")))))

            (gnc:html-document-add-object! document (gnc:make-html-div/markup
                                                     "main-table" main-table)))))

    document))

(define invoice-report-guid "5123a759ceb9483abf2182d01c140e8d")
(define easy-invoice-guid "67112f318bef4fc496bdc27d106bbda4")
(define fancy-invoice-guid "3ce293441e894423a2425d7a22dd1ac6")

(gnc:define-report
 'version 1
 'name (N_ "Printable Invoice")
 'report-guid invoice-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator (lambda () (options-generator 'invoice))
 'renderer reg-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Easy Invoice")
 'report-guid easy-invoice-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator (lambda () (options-generator 'easy-invoice))
 'renderer reg-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Fancy Invoice")
 'report-guid fancy-invoice-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator (lambda () (options-generator 'fancy-invoice))
 'renderer reg-renderer
 'in-menu? #t)

(define (gnc:easy-invoice-report-create-internal invoice)
  (issue-deprecation-warning
   "gnc:easy-invoice-report-create-internal is unused")
  (let* ((options (gnc:make-report-options easy-invoice-guid))
         (invoice-op (gnc:lookup-option options gnc:pagename-general gnc:optname-invoice-number)))
    (gnc:option-set-value invoice-op invoice)
    (gnc:make-report easy-invoice-guid options)))
(export gnc:easy-invoice-report-create-internal)

(define (gnc:fancy-invoice-report-create-internal invoice)
  (issue-deprecation-warning
   "gnc:fancy-invoice-report-create-internal is unused")
  (let* ((options (gnc:make-report-options fancy-invoice-guid))
         (invoice-op (gnc:lookup-option options gnc:pagename-general gnc:optname-invoice-number)))
    (gnc:option-set-value invoice-op invoice)
    (gnc:make-report fancy-invoice-guid options)))
(export gnc:fancy-invoice-report-create-internal)
