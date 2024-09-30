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


(define-module (gnucash reports standard invoice))

(use-modules (gnucash engine))
(use-modules (gnucash core-utils))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))

(define (addif pred . data) (if pred data '()))

(define base-css "/* advanced users only */
.div-align-right { float: right; }
.div-align-right .maybe-align-right { text-align: right }
.entries-table * { border-width: 1px; border-style:solid; border-collapse: collapse}
.entries-table > table { width: 100% }
.company-table > table * { padding: 0px; }
.client-table > table * { padding: 0px; }
.invoice-details-table > table * { padding: 0px; text-indent: 0.2em; }
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
    (gnc-optiondb-lookup-value options section name))
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
          (G_ "Date"))
   (addif (description-col column-vector)
          (G_ "Description"))
   (addif (action-col column-vector)
          (G_ "Action"))
   (addif (quantity-col column-vector)
          (G_ "Quantity"))
   (addif (price-col column-vector)
          (G_ "Unit Price"))
   (addif (discount-col column-vector)
          (G_ "Discount"))
   (addif (tax-col column-vector)
          (G_ "Taxable"))
   (addif (taxvalue-col column-vector)
          (G_ "Tax Amount"))
   (addif (value-col column-vector)
          (G_ "Total"))))

(define (monetary-or-percent numeric currency entry-type)
  (if (eqv? entry-type GNC-AMT-TYPE-PERCENT)
      (string-append (gnc:default-html-gnc-numeric-renderer numeric #f) " " (G_ "%"))
      (gnc:make-gnc-monetary currency numeric)))


(define (make-client-table options)

  (define (get-orders invoice)
    (fold
     (lambda (a b)
       (let ((order (gncEntryGetOrder a)))
         (if (member order b) b (cons order b))))
     '() (gncInvoiceGetEntries invoice)))

  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))

  ;; this is a single-column table.
  (let* ((invoice (opt-val gnc:pagename-general gnc:optname-invoice-number))
         (owner (gncInvoiceGetOwner invoice))
         (references? (opt-val "Display" "References"))
         (orders (if references? (get-orders invoice) '()))
         (table (gnc:make-html-table)))

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
                                                (G_ "REF") " "
                                                reference))))))
     orders)

    (gnc:make-html-div/markup "client-table" table)))

(define (make-company-table options)

  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))

  ;; single-column table. my name, address, and printdate
  (let* ((table (gnc:make-html-table))
         (book (gncInvoiceGetBook (opt-val gnc:pagename-general gnc:optname-invoice-number)))
         (name (gnc:company-info book gnc:*company-name*))
         (addy (gnc:company-info book gnc:*company-addy*))
         (phone (gnc:company-info book gnc:*company-phone*))
         (fax (gnc:company-info book gnc:*company-fax*))
         (email (gnc:company-info book gnc:*company-email*))
         (url (gnc:company-info book gnc:*company-url*))
         (taxnr (gnc:book-get-option-value book gnc:*tax-label* gnc:*tax-nr-label*))
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

    (gnc:make-html-div/markup "company-table" table)))


(define (make-date-row label date date-format)
  (list
   (string-append label ":")
   (gnc:make-html-div/markup
    "div-align-right"
    (gnc-print-time64 date date-format))))

(define (make-invoice-details-table options)
  ;; dual-column. invoice date/due, billingID, terms, job name/number
  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))
  (let* ((invoice-details-table (gnc:make-html-table))
         (invoice (opt-val gnc:pagename-general gnc:optname-invoice-number))
         (book (gncInvoiceGetBook invoice))
         (date-format (gnc:options-fancy-date book))
         (jobnumber (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner invoice))))
         (jobname (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner invoice)))))

    (if (gncInvoiceIsPosted invoice)

        (begin
          (gnc:html-table-append-row!
           invoice-details-table
           (make-date-row (G_ "Date") (gncInvoiceGetDatePosted invoice) date-format))

          (if (opt-val "Display" "Due Date")
              (gnc:html-table-append-row!
               invoice-details-table
               (make-date-row (G_ "Due Date") (gncInvoiceGetDateDue invoice) date-format))))

        (gnc:html-table-append-row! invoice-details-table
                                    (gnc:make-html-table-cell/size
                                     1 2 (gnc:make-html-span/markup
                                          "invoice-in-progress"
                                          (gnc:make-html-text
                                           (G_ "Invoice in progress…"))))))

    (if (opt-val "Display" "Billing ID")
        (let ((billing-id (gncInvoiceGetBillingID invoice)))
          (if (and billing-id (not (string-null? billing-id)))
              (begin
                (gnc:html-table-append-row! invoice-details-table
                                            (list
                                             (G_ "Reference:")
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
                                           (G_ "Terms:")
                                           (gnc:make-html-div/markup
                                            "div-align-right"
                                            (multiline-to-html-text terms)))))))

    ;; Add job number and name to invoice if requested and if it exists
    (if (and (opt-val "Display" "Job Details")
             (not (string-null? jobnumber)))
        (begin
          (gnc:html-table-append-row! invoice-details-table
                                      (list (G_ "Job number:")
                                            (gnc:make-html-div/markup
                                             "div-align-right"
                                             jobnumber)))
          (gnc:html-table-append-row! invoice-details-table
                                      (list (G_ "Job name:")
                                            (gnc:make-html-div/markup
                                             "div-align-right"
                                             jobname)))))

    (gnc:make-html-div/markup "invoice-details-table" invoice-details-table)))

(define (make-picture options)
  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))
  (let ((img-url (opt-val "Layout" "Picture Location")))
    (gnc:make-html-div/markup
     "picture"
     (gnc:make-html-text
      (gnc:html-markup-img
       (make-file-url img-url))))))

(define (make-today options)
  (gnc:make-html-div/markup
   "invoice-print-date" (qof-print-date (current-time))))

(define layout-key-list
  (list (list 'client
              (cons 'renderer make-client-table)
              (cons 'text (G_ "Client or vendor name, address and ID")))

        (list 'company
              (cons 'renderer make-company-table)
              (cons 'text (G_ "Company name, address and tax-ID")))

        (list 'invoice
              (cons 'renderer make-invoice-details-table)
              (cons 'text (G_ "Invoice date, due date, billing ID, terms, job details")))

        (list 'today
              (cons 'renderer make-today)
              (cons 'text (G_ "Today's date")))

        (list 'picture
              (cons 'renderer make-picture)
              (cons 'text (G_ "Picture")))

        ;; Translators: "Empty space" refers to invoice header section being left blank
        (list 'none
              (cons 'renderer (const #f))
              (cons 'text (G_ "Empty space")))))

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

   (cons 'fancy-invoice (list (cons '1a 'none)
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
      (keylist-get-info keylist (car item) 'text)))
   keylist))

(define (multiline-to-html-text str)
  (gnc:multiline-to-html-text str))

(define (options-generator variant)
  (let ((options (gnc-new-optiondb)))

    (gnc-register-invoice-option options
                                 gnc:pagename-general
                                 gnc:optname-invoice-number "x" "" '())

  (gnc-register-string-option options
    gnc:pagename-general (N_ "Custom Title")
    "z" (N_ "A custom string to replace Invoice, Bill or Expense Voucher.")
    "")

  (gnc-register-text-option options
    (N_ "Layout") (N_ "CSS") "zz" (N_ "CSS code. This field specifies the CSS code \
for styling the invoice. Please see the exported report for the CSS class names.")
    (keylist-get-info variant-list variant 'css))

  (gnc-register-pixmap-option options
    (N_ "Layout") (N_ "Picture Location") "zy" (N_ "Location for Picture")
    "")

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Date")
    "b" (N_ "Display the date?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Description")
    "d" (N_ "Display the description?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Action")
    "g" (N_ "Display the action?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Quantity")
    "ha" (N_ "Display the quantity of items?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Price")
    "hb" (N_ "Display the price per item?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Discount")
    "k" (N_ "Display the entry's discount?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Taxable")
    "l" (N_ "Display the entry's taxable status?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Tax Amount")
    "m" (N_ "Display each entry's total total tax?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display Columns") (N_ "Total")
    "n" (N_ "Display the entry's value?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Due Date")
    "c" (N_ "Display due date?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Subtotal")
    "d" (N_ "Display the subtotals?") #t)

  (gnc-register-complex-boolean-option options
    (N_ "Display") (N_ "Payable to")
    "ua1" (N_ "Display the Payable to: information.") #f
    (lambda (x)
      (gnc-optiondb-set-option-selectable-by-name
       options "Display" "Payable to string" x)))

  (gnc-register-text-option options
    (N_ "Display") (N_ "Payable to string")
    "ua2" (N_ "The phrase for specifying to whom payments should be made.")
    (G_ "Please make all checks payable to"))

  (gnc-register-complex-boolean-option options
    (N_ "Display") (N_ "Company contact")
    "ub1" (N_ "Display the Company contact information.") #f
    (lambda (x) (gnc-optiondb-set-option-selectable-by-name
                 options "Display" "Company contact string" x)))

  (gnc-register-text-option options
    (N_ "Display") (N_ "Company contact string")
    "ub2" (N_ "The phrase used to introduce the company contact.")
    (G_ "Please direct all enquiries to"))

  (gnc-register-number-range-option options
    (N_ "Display") (N_ "Minimum # of entries")
    "zz" (N_ "The minimum number of invoice entries to display.") 1
    0 23 1)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Use Detailed Tax Summary")
    "o" (N_ "Display all tax categories separately (one per line) instead of one single tax line.?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "References")
    "s" (N_ "Display the invoice references?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Billing Terms")
    "t" (N_ "Display the invoice billing terms?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Billing ID")
    "ta" (N_ "Display the billing id?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Invoice owner ID")
    "tam" (N_ "Display the customer/vendor id?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Invoice Notes")
    "tb" (N_ "Display the invoice notes?") #f)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Payments")
    "tc" (N_ "Display the payments applied to this invoice?") #t)

  (gnc-register-simple-boolean-option options
    (N_ "Display") (N_ "Job Details")
    "td" (N_ "Display the job name for this invoice?") #f)

  (gnc-register-text-option options
    (N_ "Display") (N_ "Extra Notes")
    "u" (N_ "Extra notes to put on the invoice.")
    (G_ "Thank you for your patronage!"))

  (gnc-register-multichoice-callback-option
   options
   (N_ "Display") (N_ "QR Code")
   "te" "QR Code"
   "none"
   (list
    (vector 'none (N_ "None") (N_ "None"))
    (vector 'epc  (N_ "EPC QR Code") (N_ "EPC QR Code")))
   (lambda _
     (gnc-optiondb-set-option-selectable-by-name
      options "Display" "QR Code" (gnc-qrcode-available))))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 1 Left")
    "1a" "1st row, left"
    (symbol->string (keylist-get-info variant-list variant '1a))
    (keylist->vectorlist layout-key-list))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 1 Right")
    "1b" "1st row, right"
    (symbol->string (keylist-get-info variant-list variant '1b))
    (keylist->vectorlist layout-key-list))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 2 Left")
    "2a" "2nd row, left"
    (symbol->string (keylist-get-info variant-list variant '2a))
    (keylist->vectorlist layout-key-list))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 2 Right")
    "2b" "2nd row, right"
    (symbol->string (keylist-get-info variant-list variant '2b))
    (keylist->vectorlist layout-key-list))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 3 Left")
    "3a" "3rd row, left"
    (symbol->string (keylist-get-info variant-list variant '3a))
    (keylist->vectorlist layout-key-list))

  (gnc-register-multichoice-option options
    (N_ "Layout") (N_ "Row 3 Right")
    "3b" "3rd row, right"
    (symbol->string (keylist-get-info variant-list variant '3b))
    (keylist->vectorlist layout-key-list))

  (gnc:options-set-default-section options "General")

  options))

(define (make-entry-table invoice options cust-doc? credit-note?)
  (define (opt-val section name)
    (gnc-optiondb-lookup-value options section name))

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

    (define (add-payment-row table used-columns currency split total-collector reverse-payments?)
      (let* ((t (xaccSplitGetParent split))
             ;; Depending on the document type, the payments may need to be sign-reversed
             (amt (gnc:make-gnc-monetary currency
                                         (if reverse-payments?
                                             (- (xaccSplitGetAmount split))
                                             (xaccSplitGetAmount split)))))

        (total-collector 'add
                         (gnc:gnc-monetary-commodity amt)
                         (gnc:gnc-monetary-amount amt))

        (gnc:html-table-append-row/markup!
         table "grand-total"
         (append
          (addif (date-col used-columns)
                 (qof-print-date (xaccTransGetDate t)))

          (addif (description-col used-columns)
                 (G_ "Payment, thank you!"))

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
                  (xaccPrintAmount
                   (gncEntryGetDocQuantity entry credit-note?)
                   (gnc-default-print-info #f))))

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
                     (G_ "T") ""))

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
                                    "grand-total" (G_ "Net Price")))

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
                                    "grand-total" (G_ "Tax")))

              (add-subtotal-row (gncInvoiceGetTotal invoice)
                                "grand-total" (G_ "Total Price"))

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
                           (add-payment-row table used-columns currency
                                            split total-collector
                                            reverse-payments?)))
                     splits)))

              (add-subtotal-row (cadr (total-collector 'getpair currency #f))
                                "grand-total" (G_ "Amount Due")))

            (begin

              (add-entry-row (car entries)
                             (if odd-row? "normal-row" "alternate-row"))

              (do-rows-with-subtotals (cdr entries)
                                      (not odd-row?)
                                      (1+ num-entries)))))

      table)))


(define (reg-renderer report-obj)
  (let* ((document (gnc:make-html-document))
         (options (gnc:report-options report-obj))
         (opt-val (lambda (section name) (gnc-optiondb-lookup-value options section name)))
         (invoice (opt-val gnc:pagename-general gnc:optname-invoice-number))
         (custom-title (opt-val gnc:pagename-general "Custom Title")))

    (if (null? invoice)

        (gnc:html-document-add-object!
         document
         (gnc:html-make-generic-warning
          (G_ "Invoice") (gnc:report-id report-obj) ""
          (G_ "No valid invoice selected. Click on the Options button and select the invoice to use.")))

        (let* ((book (gncInvoiceGetBook invoice))
               (type (gncInvoiceGetType invoice))
               (cust-doc? (memv type (list GNC-INVOICE-CUST-INVOICE GNC-INVOICE-CUST-CREDIT-NOTE)))
               (credit-note? (memv type (list GNC-INVOICE-CUST-CREDIT-NOTE GNC-INVOICE-VEND-CREDIT-NOTE GNC-INVOICE-EMPL-CREDIT-NOTE)))
               (default-title (cond
                               ((eqv? type GNC-INVOICE-VEND-INVOICE)
                                (G_ "Bill"))
                               ((eqv? type GNC-INVOICE-EMPL-INVOICE)
                                (G_ "Expense Voucher"))
                               ((memv type (list GNC-INVOICE-CUST-CREDIT-NOTE
                                                 GNC-INVOICE-VEND-CREDIT-NOTE
                                                 GNC-INVOICE-EMPL-CREDIT-NOTE))
                                (G_ "Credit Note"))
                               (else
                                (G_ "Invoice"))))
               (title (if (string-null? custom-title) default-title custom-title))
               (opt-qrcode (opt-val "Display" "QR Code"))
               ;; Translators: This is the format of the invoice
               ;; title.  The first ~a is one of "Invoice", "Credit
               ;; Note", and so on and the second the number.  Replace
               ;; " #" by whatever is common as number abbreviation,
               ;; i.e. "~a Nr. ~a"
               (invoice-title (format #f (G_"~a #~a") title (gncInvoiceGetID invoice)))
               (layout-lookup (lambda (loc)
                                (let* ((key (opt-val "Layout" loc))
                                       (renderer (keylist-get-info layout-key-list key 'renderer)))
                                  (renderer options)))))

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

            (cond
             ((eq? opt-qrcode 'none) #f)
             ((not (gnc-qrcode-available))
              (gnc:warn "invoice QR: libqrencode not available"))
             ((eq? opt-qrcode 'epc)
              (gnc:html-table-append-row!
               main-table
               (gnc:make-html-table-cell/size
                1 2 (apply gnc:make-html-div/markup (gnc:make-epc-qrcode invoice)))))
             (else (gnc:warn "QR option invalid")))

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
 'hook 'invoice
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Easy Invoice")
 'report-guid easy-invoice-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator (lambda () (options-generator 'easy-invoice))
 'renderer reg-renderer
 'hook 'invoice
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Fancy Invoice")
 'report-guid fancy-invoice-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator (lambda () (options-generator 'fancy-invoice))
 'renderer reg-renderer
 'hook 'invoice
 'in-menu? #t)

