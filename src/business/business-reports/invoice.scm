;; -*-scheme-*-
;; invoice.scm -- an Invoice Report, used to print a GncInvoice
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;;

(define-module (gnucash report invoice))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'record)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-gnome" 0)

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
    (set-col (opt-val "Display Columns" "Tax") 6)
    (set-col (opt-val "Display Columns" "Tax Value") 7)
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
	(addto! heading-list (_ "Unit Price")))
    (if (discount-col column-vector)
	(addto! heading-list (_ "Discount")))
    (if (tax-col column-vector)
	(addto! heading-list (_ "Tax")))
    (if (taxvalue-col column-vector)
	(addto! heading-list (_ "Tax Amount")))
    (if (value-col column-vector)
	(addto! heading-list (_ "Total")))
    (reverse heading-list)))

(define (monetary-or-percent numeric currency entry-type)
  (if (gnc:entry-type-percent-p entry-type)
      (let ((table (gnc:make-html-table)))
	(gnc:html-table-set-style!
	 table "table"
	 'attribute (list "border" 0)
	 'attribute (list "cellspacing" 1)
	 'attribute (list "cellpadding" 0))
	(gnc:html-table-append-row!
	 table
	 (list numeric (_ "%")))
	(set-last-row-style!
	 table "td"
	 'attribute (list "valign" "top"))
	table)
      (gnc:make-gnc-monetary currency numeric)))      

(define (add-entry-row table entry column-vector row-style)
  (let* ((row-contents '())
         (currency (gnc:default-currency)) ; XXX: FIXME
	 (entry-value (gnc:make-gnc-monetary
		       currency
		       (gnc:entry-get-value entry)))
	 (entry-tax-value (gnc:make-gnc-monetary
			   currency
			   (gnc:entry-get-tax-value entry))))

    (if (date-col column-vector)
        (addto! row-contents
                (gnc:print-date (gnc:entry-get-date entry))))

    (if (description-col column-vector)
        (addto! row-contents
		(gnc:entry-get-description entry)))

    (if (action-col column-vector)
        (addto! row-contents
		(gnc:entry-get-action entry)))

    (if (quantity-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (gnc:entry-get-quantity entry))))

    (if (price-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (gnc:make-gnc-monetary
		  currency (gnc:entry-get-price entry)))))

    (if (discount-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (monetary-or-percent (gnc:entry-get-discount entry)
				      currency
				      (gnc:entry-get-discount-type entry)))))

    (if (tax-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup
		 "number-cell"
		 (monetary-or-percent (gnc:entry-get-tax entry)
				      currency
				      (gnc:entry-get-tax-type entry)))))

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

(define (options-generator)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-invoice-option invoice-page invoice-name "x" ""
			    (lambda () #f) #f))

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
    "hb" "Display the price per item?" #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Discount")
    "k" (N_ "Display the entry's discount") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Tax")
    "l" (N_ "Display the entry's tax") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Tax Value")
    "m" (N_ "Display the entry's monetary tax") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Total")
    "n" (N_ "Display the entry's value") #t))

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
    (N_ "Display") (N_ "Invoice Terms")
    "t" (N_ "Display the invoice terms?") #t))

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
   (gnc:make-text-option
    (N_ "Display") (N_ "Extra Notes")
     "u" (N_ "Extra notes to put on the invoice")
     "Thank you for your patronage"))

  (gnc:register-inv-option
   (gnc:make-string-option
    (N_ "Display") (N_ "Today Date Format")
    "v" (N_ "The format for the date->string conversion for today's date.")
    "~B ~e, ~Y"))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (make-entry-table invoice options add-order)
  (let ((show-payments (gnc:option-value
			(gnc:lookup-option options (N_ "Display")
					   (N_ "Payments"))))
	(lot (gnc:invoice-get-posted-lot invoice))
	(txn (gnc:invoice-get-posted-txn invoice)))

                          
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
		(if (gnc:numeric-negative-p amt)
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
		     (append (cons (gnc:make-html-table-cell/markup
				    "total-label-cell" subtotal-label)
				   '())
			     (list (gnc:make-html-table-cell/size/markup
				    1 (colspan currency used-columns)
				    "total-number-cell"
				    (display-subtotal currency used-columns))))))
		  currency-totals)))

    (define (add-payment-row table used-columns split total-collector)
      (let* ((t (gnc:split-get-parent split))
	     (currency (gnc:transaction-get-currency t))
	     ;; XXX Need to know when to reverse the value
	     (amt (gnc:make-gnc-monetary currency (gnc:split-get-value split)))
	     (payment-style "grand-total")
	     (row '()))
	
	(total-collector 'add 
			 (gnc:gnc-monetary-commodity amt)
			 (gnc:gnc-monetary-amount amt))

	(if (date-col used-columns)
	    (addto! row
		    (gnc:print-date (gnc:transaction-get-date-posted t))))

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
				    total-collector)
      (if (null? entries)
	  (begin
	    (add-subtotal-row table used-columns value-collector
			      "grand-total" (_ "Subtotal"))
	    (add-subtotal-row table used-columns tax-collector
			      "grand-total" (_ "Tax"))

	    (if (and show-payments lot)
		(let ((splits (sort-list!
			       (gnc:lot-get-splits lot)
			       (lambda (s1 s2)
				 (let ((t1 (gnc:split-get-parent s1))
				       (t2 (gnc:split-get-parent s2)))
				   (< (gnc:transaction-order t1 t2) 0))))))
		  (for-each
		   (lambda (split)
		     (if (not (equal? (gnc:split-get-parent split) txn))
			 (add-payment-row table used-columns
					  split total-collector)))
		   splits)))

	    (add-subtotal-row table used-columns total-collector
			      "grand-total" (_ "Amount Due")))
	  
	  (let* ((current (car entries))
		 (current-row-style (if odd-row? "normal-row" "alternate-row"))
		 (rest (cdr entries))
		 (next (if (null? rest) #f
			   (car rest)))
		 (entry-values (add-entry-row table
					      current
					      used-columns
					      current-row-style)))

	    (value-collector 'add
			     (gnc:gnc-monetary-commodity (car entry-values))
			     (gnc:gnc-monetary-amount (car entry-values)))

	    (tax-collector 'add
                           (gnc:gnc-monetary-commodity (cdr entry-values))
                           (gnc:gnc-monetary-amount (cdr entry-values)))

	    (total-collector 'add
			     (gnc:gnc-monetary-commodity (car entry-values))
			     (gnc:gnc-monetary-amount (car entry-values)))
	    (total-collector 'add
			     (gnc:gnc-monetary-commodity (cdr entry-values))
			     (gnc:gnc-monetary-amount (cdr entry-values)))

	    (let ((order (gnc:entry-get-order current)))
	      (if order (add-order order)))

	    (do-rows-with-subtotals rest
				    table
				    used-columns
				    width
				    (not odd-row?)
				    value-collector
				    tax-collector
				    total-collector))))

    (let* ((table (gnc:make-html-table))
	   (used-columns (build-column-used options))
	   (width (num-columns-required used-columns))
	   (entries (gnc:invoice-get-entries invoice))
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
			      totals)
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
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (string-expand (gnc:owner-get-address-dep owner) #\newline "<br>")))
    (gnc:html-table-append-row!
     table
     (list "<br>"))
    (for-each
     (lambda (order)
       (let* ((reference (gnc:order-get-reference order)))
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
    (string-expand (gnc:print-date date) #\space "&nbsp;"))))

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

(define (make-myname-table date-format)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "align" "right")
     'attribute (list "valign" "top")
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (gnc:option-value
       (gnc:lookup-global-option "User Info" "User Name"))))
    (gnc:html-table-append-row!
     table
     (list
      (string-expand
       (gnc:option-value
	(gnc:lookup-global-option "User Info" "User Address"))
       #\newline "<br>")))
    (gnc:html-table-append-row!
     table
     (list (date->string (current-date) date-format)))
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

  (let* ((document (gnc:make-html-document))
	 (table '())
	 (orders '())
	 (invoice (opt-val invoice-page invoice-name))
	 (owner #f)
	 (references? (opt-val "Display" "References"))
	 (title (_ "Invoice")))

    (define (add-order o)
      (if (and references? (not (member o orders)))
	  (addto! orders o)))

    (if invoice
	(begin
	  (set! owner (gnc:invoice-get-owner invoice))
	  (set! title (string-append title " #"
				     (gnc:invoice-get-id invoice)))))

    (gnc:html-document-set-title! document title)

    (if invoice
	(begin
	  (set! table (make-entry-table invoice
					(gnc:report-options report-obj)
					add-order))

	  (gnc:html-table-set-style!
	   table "table"
	   'attribute (list "border" 1)
	   'attribute (list "cellspacing" 0)
	   'attribute (list "cellpadding" 4))

	  (gnc:html-document-add-object!
	   document
	   (make-myname-table (opt-val "Display" "Today Date Format")))

	  (let ((date-table #f)
		(post-date (gnc:invoice-get-date-posted invoice))
		(due-date (gnc:invoice-get-date-due invoice)))

	    (if (not (equal? post-date (cons 0 0)))
		(begin
		  (set! date-table (make-date-table))
		  (make-date-row! date-table (_ "Invoice Date") post-date)
		  (make-date-row! date-table (_ "Due Date") due-date)
		  (gnc:html-document-add-object! document date-table))
		(gnc:html-document-add-object!
		 document
		 (gnc:make-html-text
		  (N_ "Invoice in progress....")))))

	  (make-break! document)
	  (make-break! document)

	  (gnc:html-document-add-object!
	   document
	   (make-client-table owner orders))

	  (make-break! document)
	  (make-break! document)

	  (if (opt-val "Display" "Billing ID")
	      (let ((billing-id (gnc:invoice-get-billing-id invoice)))
		(if (and billing-id (> (string-length billing-id) 0))
		    (begin
		      (gnc:html-document-add-object!
		       document
		       (gnc:make-html-text
			(string-append
			 (_ "Reference") ":&nbsp;" 
			 (string-expand billing-id #\newline "<br>"))))
		      (make-break! document)))))

	  (if (opt-val "Display" "Invoice Terms")
	      (let* ((term (gnc:invoice-get-terms invoice))
		     (terms (gnc:bill-term-get-description term)))
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
	      (let ((notes (gnc:invoice-get-notes invoice)))
		(gnc:html-document-add-object!
		 document
		 (gnc:make-html-text
		  (string-expand notes #\newline "<br>")))))
	  
	  (make-break! document)

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
	  (N_ "No Valid Invoice Selected"))))

    document))

(gnc:define-report
 'version 1
 'name (N_ "Print Invoice")
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #t)

(define (gnc:invoice-report-create-internal invoice)
  (let* ((options (gnc:make-report-options (N_ "Print Invoice")))
         (invoice-op (gnc:lookup-option options invoice-page invoice-name)))

    (gnc:option-set-value invoice-op invoice)
    (gnc:make-report (N_ "Print Invoice") options)))

(export gnc:invoice-report-create-internal)
