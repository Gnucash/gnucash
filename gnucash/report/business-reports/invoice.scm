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

(define columns-used-size 9)

(define (num-columns-required columns-used)
  (do ((i 0 (1+ i))
       (col-req 0 col-req))
      ((>= i columns-used-size) col-req)
    (if (vector-ref columns-used i)
        (set! col-req (1+ col-req)))))

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
              (set! col (1+ col)))
            (vector-set! col-vector index #f)))))

  (let* ((col-vector (make-vector columns-used-size #f))
         (set-col (make-set-col col-vector)))
    (set-col (opt-val "Display Columns" "Date") 0)
    (set-col (opt-val "Display Columns" "Description") 1)
    (set-col (opt-val "Display Columns" "Action") 2)
    (set-col (opt-val "Display Columns" "Quantity") 3)
    (set-col (opt-val "Display Columns" "Price") 4)
    (set-col (opt-val "Display Columns" "Discount") 5)
    (set-col (opt-val "Display Columns" "Taxable") 6)
    (set-col (opt-val "Display Columns" "Tax Amount") 7)
    (set-col (opt-val "Display Columns" "Total") 8)
    col-vector))

(define (make-heading-list column-vector)
  (append
   (addif (date-col column-vector)
          (_ "Date"))
   (addif (description-col column-vector)
          (_ "Description"))
   (addif (action-col column-vector)
          (_ "Charge Type"))
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
  (if (gnc:entry-type-percent-p entry-type)
      (string-append (gnc:default-html-gnc-numeric-renderer numeric #f) " " (_ "%"))
      (gnc:make-gnc-monetary currency numeric)))

(define (add-entry-row table currency entry column-vector row-style cust-doc? credit-note?)
  (let* ((entry-value (gnc:make-gnc-monetary currency
                                             (gncEntryGetDocValue entry #t cust-doc? credit-note?)))
         (entry-tax-value (gnc:make-gnc-monetary currency
                                                 (gncEntryGetDocTaxValue entry #t cust-doc? credit-note?))))

    (gnc:html-table-append-row/markup!
     table row-style
     (append
      (addif (date-col column-vector)
             (qof-print-date (gncEntryGetDate entry)))

      (addif (description-col column-vector)
             (gncEntryGetDescription entry))

      (addif (action-col column-vector)
             (gncEntryGetAction entry))

      (addif (quantity-col column-vector)
             (gnc:make-html-table-cell/markup "number-cell"
                                              (gncEntryGetDocQuantity entry credit-note?)))

      (addif (price-col column-vector)
             (gnc:make-html-table-cell/markup "number-cell"
                                              (gnc:make-gnc-monetary
                                               currency (if cust-doc?
                                                            (gncEntryGetInvPrice entry)
                                                            (gncEntryGetBillPrice entry)))))

      (addif (discount-col column-vector)
             (if cust-doc?
                 (gnc:make-html-table-cell/markup
                  "number-cell"
                  (monetary-or-percent (gncEntryGetInvDiscount entry)
                                       currency
                                       (gncEntryGetInvDiscountType entry)))
                 ""))

      (addif (tax-col column-vector)
             (if (if cust-doc?
                     (and (gncEntryGetInvTaxable entry)
                          (gncEntryGetInvTaxTable entry))
                     (and (gncEntryGetBillTaxable entry)
                          (gncEntryGetBillTaxTable entry)))
                 ;; Translators: This "T" is displayed in the taxable column, if this entry contains tax
                 (_ "T") ""))

      (addif (taxvalue-col column-vector)
             (gnc:make-html-table-cell/markup
              "number-cell"
              entry-tax-value))

      (addif (value-col column-vector)
             (gnc:make-html-table-cell/markup
              "number-cell"
              entry-value))))

    (cons entry-value entry-tax-value)))

(define (options-generator)

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
    (N_ "Display") (N_ "Individual Taxes")
    "o" (N_ "Display all the individual taxes?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Totals")
    "p" (N_ "Display the totals?") #t))

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
    (N_ "Display") (N_ "Invoice Notes")
    "tb" (N_ "Display the invoice notes?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Payments")
    "tc" (N_ "Display the payments applied to this invoice?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Job Details")
    "td" (N_ "Display the job name for this invoice?") #f))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Display") (N_ "Extra Notes")
    "u" (N_ "Extra notes to put on the invoice.")
    (_ "Thank you for your patronage!")))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (make-entry-table invoice options cust-doc? credit-note?)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let ((show-payments (opt-val "Display" "Payments"))
        (display-all-taxes (opt-val "Display" "Individual Taxes"))
        (lot (gncInvoiceGetPostedLot invoice))
        (txn (gncInvoiceGetPostedTxn invoice))
        (job? (opt-val "Display" "Job Details"))
        (currency (gncInvoiceGetCurrency invoice))
        (jobnumber  (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner  invoice))))
        (jobname    (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner  invoice))))
        (reverse-payments? (not (gncInvoiceAmountPositive invoice))))

    (define (colspan monetary used-columns)
       (or (value-col used-columns)
           (taxvalue-col used-columns)
           (price-col used-columns)))

    (define (display-subtotal monetary used-columns)
      (if (value-col used-columns)
          monetary
          (let ((amt (gnc:gnc-monetary-amount monetary)))
            (if amt
                (if (negative? amt)
                    (gnc:monetary-neg monetary)
                    monetary)
                monetary))))

    (define (add-subtotal-row table used-columns
                              subtotal subtotal-style subtotal-label)
      (let ((subtotal-mon (gnc:make-gnc-monetary currency subtotal)))

        (gnc:html-table-append-row/markup!
         table subtotal-style
         (list (gnc:make-html-table-cell/markup
                "total-label-cell" subtotal-label)
               (gnc:make-html-table-cell/size/markup
                1 (colspan subtotal-mon used-columns)
                "total-number-cell"
                (display-subtotal subtotal-mon used-columns))))))

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
                 (_ "Payment, thank you"))

          (list (gnc:make-html-table-cell/size/markup
                 1 (colspan currency used-columns)
                 "total-number-cell"
                 (display-subtotal amt used-columns)))))))

    (define (do-rows-with-subtotals entries
                                    table
                                    used-columns
                                    width
                                    odd-row?)
      (if (null? entries)

          ;; all entries done, add subtotals
          (let ((total-collector (gnc:make-commodity-collector)))

            (add-subtotal-row table used-columns (gncInvoiceGetTotalSubtotal invoice)
                              "grand-total" (_ "Net Price"))

            (if display-all-taxes
                (for-each
                 (lambda (parm)
                   (let ((value (cdr parm))
                         (acct (car parm)))
                     (add-subtotal-row table used-columns value
                                       "grand-total" (xaccAccountGetName acct))))
                 (gncInvoiceGetTotalTaxList invoice))

                ;; nope, just show the total tax.
                (add-subtotal-row table used-columns (gncInvoiceGetTotalTax invoice)
                                  "grand-total" (_ "Tax")))

            (add-subtotal-row table used-columns (gncInvoiceGetTotal invoice)
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

            (add-subtotal-row table used-columns (cadr (total-collector 'getpair currency #f))
                              "grand-total" (_ "Amount Due")))

          ;;
          ;; End of BEGIN -- now here's the code to handle all the entries!
          ;;
          (begin

            (add-entry-row table
                           currency
                           (car entries)
                           used-columns
                           (if odd-row? "normal-row" "alternate-row")
                           cust-doc? credit-note?)

            (do-rows-with-subtotals (cdr entries)
                                    table
                                    used-columns
                                    width
                                    (not odd-row?)))))

    (let* ((table (gnc:make-html-table))
           (used-columns (build-column-used options))
           (width (num-columns-required used-columns))
           (entries (gncInvoiceGetEntries invoice)))

      (gnc:html-table-set-col-headers! table
                                       (make-heading-list used-columns))

      (do-rows-with-subtotals entries
                              table
                              used-columns
                              width
                              #t)
      table)))

(define (string-expand string character replace-string)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (display
          (if (eqv? c character)
              replace-string
              c)))
       string))))

(define (make-client-table owner orders)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style! table "table"
                               'attribute (list "border" 0)
                               'attribute (list "cellspacing" 0)
                               'attribute (list "cellpadding" 0))

    (gnc:html-table-append-row! table
                                (list
                                 (string-expand (gnc:owner-get-name-and-address-dep owner) #\newline "<br/>")))

    (gnc:html-table-append-row! table
                                (list
                                 (gnc:make-html-text
                                  (gnc:html-markup-br))))

    (for-each
     (lambda (order)
       (let ((reference (gncOrderGetReference order)))
         (if (and reference (not (string-null? reference)))
             (gnc:html-table-append-row! table
                                         (list
                                          (string-append (_ "REF") ":&nbsp;" reference))))))
     orders)

    (gnc:html-table-set-last-row-style! table "td"
                                        'attribute (list "valign" "top"))
    table))

(define (make-date-row! table label date date-format)
  (gnc:html-table-append-row! table
                              (list
                               (string-append label ":&nbsp;")
                               (string-expand (strftime date-format
                                                        (localtime date))
                                              #\space "&nbsp;"))))

(define (make-date-table)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style! table "table"
                               'attribute (list "border" 0)
                               'attribute (list "cellpadding" 0))

    (gnc:html-table-set-last-row-style! table "td"
                                        'attribute (list "valign" "top"))

    table))

(define (make-myname-table book date-format)
  (let* ((table (gnc:make-html-table))
         (name (gnc:company-info book gnc:*company-name*))
         (addy (gnc:company-info book gnc:*company-addy*)))

    (gnc:html-table-set-style! table "table"
                               'attribute (list "border" 0)
                               'attribute (list "align" "right")
                               'attribute (list "valign" "top")
                               'attribute (list "cellspacing" 0)
                               'attribute (list "cellpadding" 0))

    (gnc:html-table-append-row! table (list (or name "")))

    (gnc:html-table-append-row! table (list (string-expand (or addy "")
                                                           #\newline "<br/>")))
    (gnc:html-table-append-row! table (list (qof-print-date (current-time))))

    table))

(define (make-break! document)
  (gnc:html-document-add-object! document
                                 (gnc:make-html-text
                                  (gnc:html-markup-br))))

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define (title-string title custom-title)
    (if (not (string-null? custom-title))
        (string-expand custom-title
                       #\space "&nbsp;")
        title))

  (let* ((document (gnc:make-html-document))
         (invoice (opt-val gnc:pagename-general gnc:optname-invoice-number))
         (references? (opt-val "Display" "References"))
         (job? (opt-val "Display" "Job Details"))
         (jobnumber  (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner  invoice))))
         (jobname    (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner  invoice))))
         (custom-title (opt-val gnc:pagename-general "Custom Title")))

    (if (null? invoice)

        (gnc:html-document-add-object! document
                                       (gnc:make-html-text
                                        (_ "No valid invoice selected. Click on the Options button and select the invoice to use.")))

        (let* ((book (gncInvoiceGetBook invoice))
               (date-format (gnc:options-fancy-date book))
               (owner (gncInvoiceGetOwner invoice))
               (type (gncInvoiceGetType invoice))
               (orders (if references? (delete-duplicates (map gncEntryGetOrder (gncInvoiceGetEntries invoice))) '()))
               (cust-doc? (memq type (list GNC-INVOICE-CUST-INVOICE GNC-INVOICE-CUST-CREDIT-NOTE)))
               (credit-note? (memq type (list GNC-INVOICE-CUST-CREDIT-NOTE GNC-INVOICE-VEND-CREDIT-NOTE
                                              GNC-INVOICE-EMPL-CREDIT-NOTE)))
               (default-title (case type
                                ((GNC-INVOICE-VEND-INVOICE)
                                 (_ "Bill"))
                                ((GNC-INVOICE-EMPL-INVOICE)
                                 (_ "Expense Voucher"))
                                ((GNC-INVOICE-CUST-CREDIT-NOTE GNC-INVOICE-VEND-CREDIT-NOTE GNC-INVOICE-EMPL-CREDIT-NOTE)
                                 (_ "Credit Note"))
                                (else
                                 (_ "Invoice"))))
               (title (title-string default-title custom-title))
               (table (make-entry-table invoice
                                        (gnc:report-options report-obj)
                                        cust-doc? credit-note?)))

          (gnc:html-document-set-title! document (format #f (_"~a #~a") title
                                                   (gncInvoiceGetID invoice)))

          (gnc:html-table-set-style! table "table"
                                     'attribute (list "border" 1)
                                     'attribute (list "cellspacing" 0)
                                     'attribute (list "cellpadding" 4))

          (gnc:html-document-add-object! document
                                         (make-myname-table book date-format))

          (if (gncInvoiceIsPosted invoice)
              (let ((date-table (make-date-table))
                    (post-date (gncInvoiceGetDatePosted invoice))
                    (due-date (gncInvoiceGetDateDue invoice)))
                (make-date-row! date-table (string-append title " " (_ "Date")) post-date date-format)
                (make-date-row! date-table (_ "Due Date") due-date date-format)
                (gnc:html-document-add-object! document date-table))
              (gnc:html-document-add-object! document
                                             (gnc:make-html-text
                                              (_ "Invoice in progress..."))))

          (make-break! document)
          (make-break! document)

          (gnc:html-document-add-object! document
                                         (make-client-table owner orders))

          (make-break! document)
          (make-break! document)

          (if (opt-val "Display" "Billing ID")
              (let ((billing-id (gncInvoiceGetBillingID invoice)))
                (if (and billing-id (not (string-null? billing-id)))
                    (begin
                      (gnc:html-document-add-object! document
                                                     (gnc:make-html-text
                                                      (string-append
                                                       (_ "Reference") ":&nbsp;"
                                                       (string-expand billing-id #\newline "<br/>"))))
                      (make-break! document)))))

          (if (opt-val "Display" "Billing Terms")
              (let* ((term (gncInvoiceGetTerms invoice))
                     (terms (gncBillTermGetDescription term)))
                (if (and terms (not (string-null? terms)))
                    (begin
                      (gnc:html-document-add-object! document
                                                     (gnc:make-html-text
                                                      (string-append
                                                       (_ "Terms") ":&nbsp;"
                                                       (string-expand terms #\newline "<br/>"))))
                      (make-break! document)))))

          ;; Add job number and name to invoice if requested and if it exists
          (if (and job?
                   (not (string-null? jobnumber)))
              (begin
                (gnc:html-document-add-object! document
                                               (gnc:make-html-text
                                                (string-append
                                                 (_ "Job number") ":&nbsp;"
                                                 (string-expand jobnumber #\newline "<br/>"))))
                (make-break! document)
                (gnc:html-document-add-object! document
                                               (gnc:make-html-text
                                                (string-append
                                                 (_ "Job name") ":&nbsp;"
                                                 (string-expand jobname #\newline "<br/>"))))
                (make-break! document)
                (make-break! document)))

          (gnc:html-document-add-object! document table)

          (make-break! document)
          (make-break! document)

          (if (opt-val "Display" "Invoice Notes")
              (let ((notes (gncInvoiceGetNotes invoice)))
                (gnc:html-document-add-object! document
                                               (gnc:make-html-text
                                                (string-expand notes #\newline "<br/>")))))

          (make-break! document)

          (gnc:html-document-add-object! document
                                         (gnc:make-html-text
                                          (gnc:html-markup-br)
                                          (string-expand (opt-val "Display" "Extra Notes") #\newline "<br/>")
                                          (gnc:html-markup-br)))))

    document))

(define invoice-report-guid "5123a759ceb9483abf2182d01c140e8d")

(gnc:define-report
 'version 1
 'name (N_ "Printable Invoice")
 'report-guid invoice-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #t)
