;; -*-scheme-*-
;; fancy-invoice.scm -- a Fancy Invoice Report, used to print a GncInvoice
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;; Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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


;; Fancy Invoice customized from "invoice.scm"
;; Customized by:  Oliver Jones <gnucash at oliverstech dot com>
;;
;; WARNING: customizations are hard-coded, some translations might be
;; broken and it won't work for bills/expense vouchers
;;
;; Customizations are marked with "oli-custom".
;;
;; Hint: you may set your default options here until a way to save report
;; options will be implemented.
;;
;; You will need to upgrade to gtkhtml-1.1 for the latest features or
;; it won't look right.  gtkhtml doesn't have support for table
;; colgroup, tbody, thead and rules tags yet. When it will, the
;; invoice will look even better.
;;
;; This is a quick and dirty hack. The proper way to do this (when I
;; or someone else will have time) is to have the user supply an HTML
;; template. The most common used templates will be distributed with
;; gnucash.

;; Modifed to use settable options instead of the hard coded ones.
;; modified by Brian Dolbec <dol-sen at telus dot net> Feb. 6, 2006

(define-module (gnucash report fancy-invoice))

(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'hash-table)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-utils" 0)

(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(define invoice-page gnc:pagename-general)
(define invoice-name (N_ "Invoice Number"))

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

(define (set-last-row-style! table tag . rest)
  (let ((arg-list 
         (cons table 
               (cons (- (gnc:html-table-num-rows table) 1)
                     (cons tag rest)))))
    (apply gnc:html-table-set-row-style! arg-list)))

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
  (do ((i 0 (+ i 1)) 
       (col-req 0 col-req)) 
      ((>= i columns-used-size) col-req)
    (if (vector-ref columns-used i)
        (set! col-req (+ col-req 1)))))

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

  (let ((heading-list '()))
    (if (date-col column-vector)
        (addto! heading-list (_ "Date")))
    (if (description-col column-vector)
        (addto! heading-list (_ "Description")))
    (if (action-col column-vector)
	(addto! heading-list (_ "Charge Type")))
    (if (quantity-col column-vector)
	(addto! heading-list (_ "Quantity")))
    (if (price-col column-vector)
	(addto! heading-list (string-expand (_ "Unit Price") #\space "&nbsp;")))
    (if (discount-col column-vector)
	(addto! heading-list (_ "Discount")))
    (if (tax-col column-vector)
	(addto! heading-list (_ "Taxable")))
    (if (taxvalue-col column-vector)
	(addto! heading-list (_ "Tax Amount")))
    (if (value-col column-vector)
	(addto! heading-list (_ "Total")))
    (reverse heading-list)))

(define (make-account-hash) (make-hash-table 23))

(define (update-account-hash hash values)
  (for-each
   (lambda (item)
     (let* ((acct (car item))
	    (val (cdr item))
	    (ref (hash-ref hash acct)))

       (hash-set! hash acct (if ref (gnc-numeric-add-fixed ref val) val))))
   values))

(define (monetary-or-percent numeric currency entry-type)
  (if (gnc:entry-type-percent-p entry-type)
      ;; oli-custom - make a string instead of a table
      (string-append (gnc:default-html-gnc-numeric-renderer numeric #f) " " (_ "%"))
      (gnc:make-gnc-monetary currency numeric)))

(define (add-entry-row table currency entry column-vector row-style invoice?)
  (let* ((row-contents '())
	 (entry-value (gnc:make-gnc-monetary
		       currency
		       (gncEntryReturnValue entry invoice?)))
	 (entry-tax-value (gnc:make-gnc-monetary
			   currency
			   (gncEntryReturnTaxValue entry invoice?))))

    (if (date-col column-vector)
        (addto! row-contents
                (gnc-print-date (gncEntryGetDate entry))))

    (if (description-col column-vector)
        (addto! row-contents
		(gncEntryGetDescription entry)))

    (if (action-col column-vector)
        (addto! row-contents
		(gncEntryGetAction entry)))

    (if (quantity-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (gncEntryGetQuantity entry))))

    (if (price-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (gnc:make-gnc-monetary
		  currency (if invoice? (gncEntryGetInvPrice entry)
			       (gncEntryGetBillPrice entry))))))

    (if (discount-col column-vector)
	(addto! row-contents
		(if invoice?
		    (gnc:make-html-table-cell/markup
		     "number-cell"
		     (monetary-or-percent (gncEntryGetInvDiscount entry)
					  currency
					  (gncEntryGetInvDiscountType entry)))
		    "")))

    (if (tax-col column-vector)
	(addto! row-contents
		(if (if invoice?
			(and (gncEntryGetInvTaxable entry)
			     (gncEntryGetInvTaxTable entry))
			(and (gncEntryGetBillTaxable entry)
			     (gncEntryGetBillTaxTable entry)))
		    (_ "T") "")))

    (if (taxvalue-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 entry-tax-value)))

    (if (value-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 entry-value)))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    
    (cons entry-value entry-tax-value)))

;; oli-custom - here you can set your default options

(define (options-generator)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-invoice-option invoice-page invoice-name "x" ""
			    (lambda () '()) #f))

  (gnc:register-inv-option
   (gnc:make-string-option 
    invoice-page (N_ "Custom Title") 
    "z" (N_ "A custom string to replace Invoice, Bill or Expense Voucher") 
    ""))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Date")
    "b" (N_ "Display the date?") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Description")
    "d" (N_ "Display the description?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Action")
    "g" (N_ "Display the action?") #f))

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
    "k" (N_ "Display the entry's discount") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Taxable")
    "l" (N_ "Display the entry's taxable status") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Tax Amount")
    "m" (N_ "Display each entry's total total tax") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Total")
    "n" (N_ "Display the entry's value") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Individual Taxes")
    "o" (N_ "Display all the individual taxes?") #t))

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
   (gnc:make-number-range-option
    (N_ "Display") (N_ "Minimum # of entries")
    "u" (N_ "The minimum number of invoice entries to display. (-1)") 23
    4 23 3 1))

  (gnc:register-inv-option
   (gnc:make-text-option
    (N_ "Display") (N_ "Extra Notes")
     "u" (N_ "Extra notes to put on the invoice")
     ""))

  (gnc:register-inv-option
   (gnc:make-complex-boolean-option
    (N_ "Display") (N_ "Payable to")
     "ua1" (N_ "Display the Payable to: information") #t #f
     (lambda (x) (gnc-option-db-set-option-selectable-by-name
		  gnc:*report-options* "Display" "Payable to string" x))))

  (gnc:register-inv-option
   (gnc:make-string-option
    (N_ "Display") (N_ "Payable to string")
    "ua2" (N_ "The phrase for specifying to whom payments should be made")
    (_ "Make all cheques Payable to")))
     
  (gnc:register-inv-option
   (gnc:make-complex-boolean-option
    (N_ "Display") (N_ "Company contact")
     "ub1" (N_ "Display the Company contact information") #t #f
     (lambda (x) (gnc-option-db-set-option-selectable-by-name
		  gnc:*report-options* "Display" "Company contact string" x))))

  (gnc:register-inv-option
   (gnc:make-string-option
    (N_ "Display") (N_ "Company contact string")
    "ub2" (N_ "The phrase used to introduce the company contact")
    (_ "Direct all inquiries to")))

; not used
;  (gnc:register-inv-option
;   (gnc:make-string-option
;    (N_ "Display") (N_ "Today Date Format")
;    "v" (N_ "The format for the date->string conversion for today's date.")
;    (gnc-default-strftime-date-format)))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (make-entry-table invoice options add-order invoice?)
  (define (opt-val section name)
    (gnc:option-value 
     (gnc:lookup-option options section name)))

  (let ((show-payments (opt-val "Display" "Payments"))
	(display-all-taxes (opt-val "Display" "Individual Taxes"))
	(lot (gncInvoiceGetPostedLot invoice))
	(txn (gncInvoiceGetPostedTxn invoice))
	(currency (gncInvoiceGetCurrency invoice))
	(entries-added 0))

    (define (colspan monetary used-columns)
      (cond
       ((value-col used-columns) (value-col used-columns))
       ((taxvalue-col used-columns) (taxvalue-col used-columns))
       (else (price-col used-columns))))

    (define (display-subtotal monetary used-columns)
      (if (value-col used-columns)
	  monetary
	  (let ((amt (gnc:gnc-monetary-amount monetary)))
	    (if amt
		(if (gnc-numeric-negative-p amt)
		    (gnc:monetary-neg monetary)
		    monetary)
		monetary))))

    (define (add-subtotal-row table used-columns
			      subtotal-collector subtotal-style subtotal-label)
      (let ((currency-totals (subtotal-collector
			      'format gnc:make-gnc-monetary #f)))

	(for-each (lambda (currency)
		    (gnc:html-table-append-row/markup! 
		     table
		     subtotal-style
		     ;; oli-custom modified to colspan the subtotal labels
		     ;; instead of the data fields
		     (append (cons (gnc:make-html-table-cell/size/markup
				    0 (colspan currency used-columns)
				    "total-label-cell" subtotal-label)
				   '())
			     (list (gnc:make-html-table-cell/markup
				    ;; 1 (colspan currency used-columns)
				    "total-number-cell"
				    (display-subtotal currency used-columns))))))
		  currency-totals)))

    (define (add-payment-row table used-columns split total-collector)
      (let* ((t (xaccSplitGetParent split))
	     (currency (xaccTransGetCurrency t))
	     ;; XXX Need to know when to reverse the value
	     (amt (gnc:make-gnc-monetary currency (xaccSplitGetValue split)))
	     (payment-style "grand-total")
	     (row '()))
	
	(total-collector 'add 
			 (gnc:gnc-monetary-commodity amt)
			 (gnc:gnc-monetary-amount amt))

	(if (date-col used-columns)
	    (addto! row
		    (gnc-print-date (gnc-transaction-get-date-posted t))))

	(if (description-col used-columns)
	    (addto! row (_ "Payment, thank you")))
		    
	(gnc:html-table-append-row/markup! 
	 table
	 payment-style
	 (append (reverse row)
		 (list (gnc:make-html-table-cell/size/markup
			1 (colspan currency used-columns)
			"total-number-cell"
			(display-subtotal amt used-columns)))))))

    (define (do-rows-with-subtotals entries
				    table
				    used-columns
				    width
				    odd-row?
				    value-collector
				    tax-collector
				    total-collector
				    acct-hash)
      (if (null? entries)
	  (begin
	    ;; oli-custom - modified to have a minimum of entries per table,
	    ;; currently defaults to 24
	    ;; also, doesn't count payment rows and stuff
	    (do ((entries-added entries-added (+ entries-added 1))
		 (odd-row? odd-row? (not odd-row?)))
		((> entries-added (opt-val "Display" "Minimum # of entries" )))
		(gnc:html-table-append-row/markup!
		 table (if odd-row? "normal-row" "alternate-row")
		 (string->list (make-string (num-columns-required used-columns)
					    #\space)))
		)
	    (add-subtotal-row table used-columns value-collector
			      "grand-total" (_ "Subtotal"))

	    (if display-all-taxes
		(hash-for-each
		 (lambda (acct value)
		   (let ((collector (gnc:make-commodity-collector))
			 (commodity (xaccAccountGetCommodity acct))
			 (name (xaccAccountGetName acct)))
		     (collector 'add commodity value)
		     (add-subtotal-row table used-columns collector
				       "grand-total" (string-expand
						      name #\space "&nbsp;"))))
		 acct-hash)

		; nope, just show the total tax.
		(add-subtotal-row table used-columns tax-collector
				  "grand-total" (_ "Tax")))

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
					  split total-collector)))
		   splits)))

	    (add-subtotal-row table used-columns total-collector
			      "grand-total" (string-expand (_ "Amount Due")
							   #\space "&nbsp;")))

	  ;;
	  ;; End of BEGIN -- now here's the code to handle all the entries!
	  ;;
	  (let* ((current (car entries))
		 (current-row-style (if odd-row? "normal-row" "alternate-row"))
		 (rest (cdr entries))
		 (next (if (null? rest) #f
			   (car rest)))
		 (entry-values (add-entry-row table
					      currency
					      current
					      used-columns
					      current-row-style
					      invoice?)))

	    (if display-all-taxes
		(let ((tax-list (gncEntryReturnTaxValues current invoice?)))
		  (update-account-hash acct-hash tax-list))
		(tax-collector 'add
			       (gnc:gnc-monetary-commodity (cdr entry-values))
			       (gnc:gnc-monetary-amount (cdr entry-values))))

	    (value-collector 'add
			     (gnc:gnc-monetary-commodity (car entry-values))
			     (gnc:gnc-monetary-amount (car entry-values)))

	    (total-collector 'add
			     (gnc:gnc-monetary-commodity (car entry-values))
			     (gnc:gnc-monetary-amount (car entry-values)))
	    (total-collector 'add
			     (gnc:gnc-monetary-commodity (cdr entry-values))
			     (gnc:gnc-monetary-amount (cdr entry-values)))

	    (let ((order (gncEntryGetOrder current)))
	      (if (not (null? order)) (add-order order)))

	    (set! entries-added (+ entries-added 1))
	    
	    (do-rows-with-subtotals rest
				    table
				    used-columns
				    width
				    (not odd-row?)
				    value-collector
				    tax-collector
				    total-collector
				    acct-hash))))

    (let* ((table (gnc:make-html-table))
	   (used-columns (build-column-used options))
	   (width (num-columns-required used-columns))
	   (entries (gncInvoiceGetEntries invoice))
	   (totals (gnc:make-commodity-collector)))

      (gnc:html-table-set-col-headers!
       table
       (make-heading-list used-columns))

      (do-rows-with-subtotals entries
			      table
			      used-columns
			      width
			      #t
			      (gnc:make-commodity-collector)
			      (gnc:make-commodity-collector)
			      totals
			      (make-account-hash))
      table)))

(define (string-expand string character replace-string)
  (define (car-line chars)
    (take-while (lambda (c) (not (eqv? c character))) chars))
  (define (cdr-line chars)
    (let ((rest (drop-while (lambda (c) (not (eqv? c character))) chars)))
      (if (null? rest)
          '()
          (cdr rest))))
  (define (line-helper chars)
    (if (null? chars)
        ""
        (let ((first (car-line chars))
              (rest (cdr-line chars)))
          (string-append (list->string first)
                         (if (null? rest) "" replace-string)
                         (line-helper rest)))))
  (line-helper (string->list string)))

(define (make-client-table owner orders)
  (let ((table (gnc:make-html-table))
	(name-cell (gnc:make-html-table-cell)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-cell-append-objects!
     name-cell (gnc:owner-get-name-dep owner))
    (gnc:html-table-cell-set-style!
     name-cell "td"
     'font-size "+2")
    (gnc:html-table-append-row! table (list name-cell #\newline "<br>"))
    (gnc:html-table-append-row!
     table
     (list
      (string-expand (gnc:owner-get-address-dep owner) #\newline "<br>")))
    (gnc:html-table-append-row!
     table
     (list "<br>"))
    (for-each
     (lambda (order)
       (let* ((reference (gncOrderGetReference order)))
	 (if (and reference (> (string-length reference) 0))
	     (gnc:html-table-append-row!
	      table
	      (list
	       (string-append (_ "REF") ":&nbsp;" reference))))))
     orders)
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (make-date-row! table label date)
  (gnc:html-table-append-row!
   table
   (list
    (string-append label ":&nbsp;")
    ;; oli-custom - modified to display a custom format
    ;; for the invoice date/due date fields
    ;; I could have taken the format from the report options, but... ;)
    (string-expand (strftime (gnc-default-strftime-date-format)
                             (localtime (car date)))
                   #\space "&nbsp;")
    ;;(string-expand (gnc-print-date date) #\space "&nbsp;")
    )))

(define (make-date-table)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellpadding" 0))
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (make-myname-table book date-format title)
  (let* ((table (gnc:make-html-table))
	 (slots (gnc-book-get-slots book))
	 (name (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-name*))))
;;	 (contact (kvp-frame-get-slot-path-gslist
;;		slots (append gnc:*kvp-option-path*
;;			      (list gnc:*business-label* gnc:*company-contact*))))
	 (addy (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-addy*))))
	 (id (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-id*))))
	 (phone (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-phone*))))
	 (fax (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-fax*))))
	 (url (kvp-frame-get-slot-path-gslist
		slots (append gnc:*kvp-option-path*
			      (list gnc:*business-label* gnc:*company-url*))))
	 (invoice-cell (gnc:make-html-table-cell))
	 (name-cell (gnc:make-html-table-cell))

	)
    ;; oli-custom - modified the name table to increase the
    ;; font size of the company name
    ;; and add an "INVOICE" title to the upper right, also,
    ;; put some contact information in the middle
    ;; FIXME: "INVOICE" should be translated and support bills/expense vouchers
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0)
     'attribute (list "width" "100%"))
    (gnc:html-table-cell-append-objects!
	invoice-cell title)
    (gnc:html-table-cell-set-style!
	invoice-cell "td"
	'font-size "+2")
    (gnc:html-table-cell-append-objects!
	name-cell (if name name ""))
    (gnc:html-table-cell-set-style!
	name-cell "td"
	'font-size "+2")
    (gnc:html-table-append-row! table (list name-cell "" invoice-cell))
    (gnc:html-table-set-col-style!
	table 1 "td"
	'attribute (list "align" "center")
	'attribute (list "width" "33%"))
    (gnc:html-table-set-col-style!
	table 2 "td"
	'attribute (list "align" "right")
	'attribute (list "width" "33%"))
    (gnc:html-table-append-row!
     table (list (string-expand (string-append (if addy addy "") (if id (string-append "\n" id) "")) #\newline "<br>")
		 (string-expand
		  (string-append (if phone
				     (string-append (_ "Phone:") " " phone)
				     "")
				 (if fax (string-append (if phone "\n" "")
							(_ "Fax:") " " fax)
				     ""))
		  #\newline "<br>" )
		 (if url (string-append (_ "Web:") " " url) "")))

;; oli-custom - I didn't want today's date on the invoice.
;; The invoice already has a date.
;; Today's date can be in the email, fax or letter accompanying the invoice.
;;    (gnc:html-table-append-row! table (list
;;				       (strftime
;;					date-format
;;					(localtime (car (gnc:get-today))))))
    table))

(define (make-break! document)
  (gnc:html-document-add-object!
   document
   (gnc:make-html-text
    (gnc:html-markup-br))))

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define (title-string title custom-title)
    (if (not (equal? "" custom-title))
	(string-expand custom-title
		       #\space "&nbsp;")
	title))

  (let* ((document (gnc:make-html-document))
	 (table '())
	 (orders '())
	 (invoice (opt-val invoice-page invoice-name))
	 (owner '())
	 (references? (opt-val "Display" "References"))
	 (default-title (_ "Invoice"))
	 (custom-title (opt-val invoice-page "Custom Title"))
	 (invoice? #f))


    (define (add-order o)
      (if (and references? (not (member o orders)))
	  (addto! orders o)))

    (if (not (null? invoice))
	(begin
	  (set! owner (gncInvoiceGetOwner invoice))
	  (let ((type (gncOwnerGetType
                       (gncOwnerGetEndOwner owner))))
	    (cond
	      ((eqv? type GNC-OWNER-CUSTOMER)
	       (set! invoice? #t))
	      ((eqv? type GNC-OWNER-VENDOR)
	       (set! default-title (_ "Bill")))
	      ((eqv? type GNC-OWNER-EMPLOYEE)
	       (set! default-title (_ "Expense Voucher")))))
	  ))

    ;; oli-custom - title redundant, "Invoice" moved to myname-table,
    ;; invoice number moved below
    ;;(gnc:html-document-set-title! document title)

    
    (if (not (null? invoice))
	(let* ((book (gncInvoiceGetBook invoice))
	      (slots (gnc-book-get-slots book))
	      (date-object #f)
	      (helper-table (gnc:make-html-table))
	      (title (title-string default-title custom-title)))
	  (set! table (make-entry-table invoice
					(gnc:report-options report-obj)
					add-order invoice?))

	  (gnc:html-table-set-style!
	   table "table"
	   'attribute (list "border" 1)
	   'attribute (list "cellspacing" 0)
	   'attribute (list "cellpadding" 4)
	   ;; oli-custom - make table as wide as possible
	   ;; works fine with simple style sheet templates,
	   ;; doesn't work quite right with fancy ones
	   ;; probably supplying the style sheet with a wide image
	   ;; for the header (even if transparent/white) would fix it
	   'attribute (list "width" "100%"))

	  ;; oli-custom - make the description column big
	  ;; 50% or 60%, depending on whether the first column is
	  ;; displayed or not
	  ;; should actually be something more complicated,
	  ;; it's a really ugly hack right now :)
	  (gnc:html-table-set-col-style!
	   table (if (opt-val "Display Columns" "Date") 1 0) "td"
	   'attribute (list "width" (if (opt-val "Display Columns" "Date")
					"50%" "60%")))

	  (gnc:html-document-add-object!
	   document (make-myname-table
		     book ;;(opt-val "Display" "Today Date Format")))
		     "" title))

	  (make-break! document)
	  (make-break! document)
	  (make-break! document)

	  ;; oli-custom - client table and table with invoice
	  ;; number/date/due date both inserted into a table
	  (gnc:html-table-set-style!
	   helper-table "table"
	   'attribute (list "border" 0)
	   'attribute (list "cellspacing" 0)
	   'attribute (list "cellpadding" 0)
	   'attribute (list "width" "100%"))

	  (set! date-object (let ((date-table #f)
		(post-date (gncInvoiceGetDatePosted invoice))
		(due-date (gncInvoiceGetDateDue invoice)))

	    (if (not (equal? post-date (cons 0 0)))
		(begin
		  (set! date-table (make-date-table))
		  ;; oli-custom - moved invoice number here
		  (gnc:html-table-append-row!
		   date-table (list (sprintf #f "%s&nbsp;#" title) (gncInvoiceGetID invoice)))
		  (make-date-row! date-table (string-append title "&nbsp;" (_ "Date")) post-date)
		  (make-date-row! date-table (_ "Due Date") due-date)
		  date-table)
		(gnc:make-html-text
		  ;; oli-custom - FIXME: I have a feeling I broke a
		 ;; translation by not using string-expand for &nbsp;
		  (string-append title "<br>" (N_ "Invoice in progress..."))))))
	  
	  (gnc:html-table-append-row!
	  	helper-table
		(list (make-client-table owner orders) date-object))
	
	  (gnc:html-table-set-col-style!
	  	helper-table 0 "td"
		'attribute (list "valign" "top"))
		
	  (gnc:html-table-set-col-style!
	  	helper-table 1 "td"
		'attribute (list "valign" "top")
		'attribute (list "align" "right")
		;; oli-custom - "squeeze" the date table,
		;; or else it's spaced out
		'attribute (list "width" "1%"))

	  (gnc:html-document-add-object!
	   document
	   helper-table)

	  (make-break! document)

	  (if (opt-val "Display" "Billing ID")
	      (let ((billing-id (gncInvoiceGetBillingID invoice)))
		(if (and billing-id (> (string-length billing-id) 0))
		    (begin
		      (gnc:html-document-add-object!
		       document
		       (gnc:make-html-text
			(string-append
			 (_ "Reference") ":&nbsp;" 
			 (string-expand billing-id #\newline "<br>"))))
		      (make-break! document)))))

	  (if (opt-val "Display" "Billing Terms")
	      (let* ((term (gncInvoiceGetTerms invoice))
		     (terms (gncBillTermGetDescription term)))
		(if (and terms (> (string-length terms) 0))
		    (gnc:html-document-add-object!
		     document
		     (gnc:make-html-text
		      (string-append
		       (_ "Terms") ":&nbsp;" 
		       (string-expand terms #\newline "<br>")))))))

	  (make-break! document)

	  (gnc:html-document-add-object! document table)

	  (make-break! document)
	  (make-break! document)

	  (if (opt-val "Display" "Invoice Notes")
	      (let ((notes (gncInvoiceGetNotes invoice)))
		(gnc:html-document-add-object!
		 document
		 (gnc:make-html-text
		  (string-expand notes #\newline "<br>")))))
	  
	  (make-break! document)

	  (if (opt-val "Display" "Payable to")
	      (let* ((name (kvp-frame-get-slot-path-gslist
			    slots (append gnc:*kvp-option-path*
					  (list gnc:*business-label*
						gnc:*company-name*))))
		     (name-str (opt-val "Display" "Payable to string")))
		(if (and name (> (string-length name) 0))
		(gnc:html-document-add-object!
		 document
		 (gnc:make-html-text
		  (string-append name-str  ":&nbsp;"
		  (string-expand name #\newline "<br>")))))))

	  (make-break! document)

	  (if (opt-val "Display" "Company contact")
	      (let* ((contact (kvp-frame-get-slot-path-gslist
			       slots (append gnc:*kvp-option-path*
					     (list gnc:*business-label*
						   gnc:*company-contact*))))
		     (contact-str (opt-val "Display" "Company contact string")))
		(if (and contact (> (string-length contact) 0))
	        (gnc:html-document-add-object!
		 document
		 (gnc:make-html-text
		  (string-append contact-str  ":&nbsp;"
		  (string-expand contact #\newline "<br>")))))))

	  (gnc:html-document-add-object!
	   document
	   (gnc:make-html-text
	    (gnc:html-markup-br)
	    (string-expand (opt-val "Display" "Extra Notes") #\newline "<br>")
	    (gnc:html-markup-br))))

	; else
	(gnc:html-document-add-object!
	 document
	 (gnc:make-html-text
	  (_ "No valid invoice selected.  Click on the Options button and select the invoice to use."))))

    document))

(define fancy-invoice-guid "3ce293441e894423a2425d7a22dd1ac6")

(gnc:define-report
 'version 1
 'name (N_ "Fancy Invoice")
 'report-guid fancy-invoice-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #t)

(define (gnc:fancy-invoice-report-create-internal invoice)
  (let* ((options (gnc:make-report-options fancy-invoice-guid))
         (invoice-op (gnc:lookup-option options invoice-page invoice-name)))

    (gnc:option-set-value invoice-op invoice)
    (gnc:make-report fancy-invoice-guid options)))

(export gnc:fancy-invoice-report-create-internal)
