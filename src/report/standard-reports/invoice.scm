;; -*-scheme-*-
;; invoice.scm

(define-module (gnucash report invoice))

(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'record)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-core" 0)

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
    (set-col (opt-val "Display" "Date") 0)
    (set-col (opt-val "Display" "Description") 1)
    (set-col (opt-val "Display" "Action") 2)
    (set-col (opt-val "Display" "Quantity") 3)
    (set-col (opt-val "Display" "Price") 4)
    (set-col (opt-val "Display" "Discount") 5)
    (set-col (opt-val "Display" "Tax") 6)
    (set-col (opt-val "Display" "Tax Value") 7)
    (set-col (opt-val "Display" "Value") 8)
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
	(addto! heading-list (_ "Total Charge")))
    (reverse heading-list)))

(define (add-entry-row table entry column-vector row-style)
  (let* ((row-contents '())
         (currency (gnc:default-currency)) ; XXX: FIXME
	 (entry-value (gnc:make-gnc-monetary currency
					     (gnc:entry-get-value entry))))

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
		(gnc:entry-get-quantity entry)))

    (if (price-col column-vector)
	(addto! row-contents
		(gnc:make-gnc-monetary
		 currency (gnc:entry-get-price entry))))

    (if (discount-col column-vector)
	(addto! row-contents
		(gnc:entry-get-discount entry)))

    (if (tax-col column-vector)
	(addto! row-contents
		(gnc:entry-get-tax entry)))

    (if (taxvalue-col column-vector)
	(addto! row-contents
		(gnc:make-gnc-monetary
		 currency (gnc:entry-get-tax-value entry))))

    (if (value-col column-vector)
	(addto! row-contents entry-value))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    
    entry-value))

(define (options-generator)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-internal-option "__reg" "invoice" #f))

  (gnc:register-inv-option
   (gnc:make-string-option
    (N_ "General") (N_ "Title")
    "a" (N_ "The title of the report")
    (N_ "Invoice Report")))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Date")
    "b" (N_ "Display the date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Description")
    "d" (N_ "Display the description?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Action")
    "g" (N_ "Display the action?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Quantity")
    "ha" (N_ "Display the quantity of items?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Price")
    "hb" "Display the price per item?" #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Discount")
    "k" (N_ "Display the entry's discount") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Tax")
    "k" (N_ "Display the entry's tax") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Tax Value")
    "k" (N_ "Display the entry's monetary tax") #f))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Value")
    "k" (N_ "Display the entry's value") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Totals")
    "l" (N_ "Display the totals?") #t))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (make-entry-table entries options)
                          
  (define (add-subtotal-row leader table used-columns
                            subtotal-collector subtotal-style)
    (let ((currency-totals (subtotal-collector
                            'format gnc:make-gnc-monetary #f)))

      (define (colspan monetary)
        (cond
	 ((value-col used-columns) (value-col used-columns))
         ((taxvalue-col used-columns) (taxvalue-col used-columns))
         (else (price-col used-columns))))

      (define (display-subtotal monetary)
        (if (value-col used-columns)
            (if (and leader (gnc:account-reverse-balance? leader))
                (gnc:monetary-neg monetary)
                monetary)
	    (let ((amt (gnc:gnc-monetary-amount monetary)))
	      (if amt
		  (if (gnc:numeric-negative-p amt)
		      (gnc:monetary-neg monetary)
		      monetary)
		  monetary))))

      (for-each (lambda (currency)
                  (gnc:html-table-append-row/markup! 
                   table
                   subtotal-style
                   (append (cons (gnc:make-html-table-cell/markup
                                  "total-label-cell" (_ "Total"))
                                 '())
                           (list (gnc:make-html-table-cell/size/markup
                                  1 (colspan currency)
                                  "total-number-cell"
                                  (display-subtotal currency))))))
                currency-totals)))

  (define (do-rows-with-subtotals leader
                                  entries
                                  table
                                  used-columns
                                  width
                                  odd-row?
                                  total-collector)
    (if (null? entries)
        (add-subtotal-row leader table used-columns
                          total-collector "grand-total")

        (let* ((current (car entries))
               (current-row-style (if odd-row? "normal-row" "alternate-row"))
               (rest (cdr entries))
               (next (if (null? rest) #f
                         (car rest)))
               (entry-value (add-entry-row table
                                           current
                                           used-columns
                                           current-row-style)))

          (total-collector 'add
                           (gnc:gnc-monetary-commodity entry-value)
                           (gnc:gnc-monetary-amount entry-value))

          (do-rows-with-subtotals leader
                                  rest
                                  table
                                  used-columns
                                  width 
                                  (not odd-row?)                       
                                  total-collector))))

  (define (entries-leader entries) #f)

  (let* ((table (gnc:make-html-table))
         (used-columns (build-column-used options))
         (width (num-columns-required used-columns)))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns))

    (do-rows-with-subtotals (entries-leader entries)
                            entries
                            table
                            used-columns
                            width
                            #t
                            (gnc:make-commodity-collector))
    table))

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

(define (make-client-table address)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (string-append (_ "Customer") ":&nbsp;")
      (string-expand address #\newline "<br>")))
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (make-info-table address)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 20)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (string-append
       (_ "Date") ":&nbsp;"
       (string-expand (gnc:print-date (cons (current-time) 0))
                      #\space "&nbsp;"))
      (make-client-table address)))
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let ((document (gnc:make-html-document))
        (table '())
	(owner #f)
	(entries '())
        (invoice (opt-val "__reg" "invoice"))
        (title (opt-val "General" "Title")))

    (set! owner (gnc:invoice-get-owner invoice))
    (set! entries (gnc:invoice-get-entries invoice))
    (set! table (make-entry-table entries (gnc:report-options report-obj)))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text
      (gnc:html-markup-br)
      (gnc:option-value
       (gnc:lookup-global-option "User Info" "User Name"))
      (gnc:html-markup-br)
      (string-expand
       (gnc:option-value
	(gnc:lookup-global-option "User Info" "User Address"))
       #\newline
       "<br>")
      (gnc:html-markup-br)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 1)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 4))
    (gnc:html-document-add-object!
     document
     (make-info-table (gnc:owner-get-address owner)))

    (gnc:html-document-set-title! document title)
    (gnc:html-document-add-object! document table)

    document))

(gnc:define-report
 'version 1
 'name (N_ "Invoice")
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #f)

(define (gnc:invoice-report-create-internal invoice title)
  (let* ((options (gnc:make-report-options "Invoice"))
         (invoice-op (gnc:lookup-option options "__reg" "invoice"))
         (title-op (gnc:lookup-option options "General" "Title")))

    (gnc:option-set-value invoice-op invoice)
    (gnc:option-set-value title-op title)
    (gnc:make-report "Invoice" options)))

(export gnc:invoice-report-create-internal)
