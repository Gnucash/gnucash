;; -*-scheme-*-
;; owner-report.scm -- Print out a detailed owner report, which is a
;;		       summary of invoices and payments for a particular
;;		       company (the owner) applied to an account.
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;;

(define-module (gnucash report owner-report))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))
(use-modules (gnucash main))		; for gnc:debug

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-gnome" 0)
(use-modules (gnucash report standard-reports))

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
(define (num-col columns-used)
  (vector-ref columns-used 1))
(define (type-col columns-used)
  (vector-ref columns-used 2))
(define (memo-col columns-used)
  (vector-ref columns-used 3))
(define (value-col columns-used)
  (vector-ref columns-used 4))

(define columns-used-size 5)

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
    (set-col (opt-val "Display Columns" "Num") 1)
    (set-col (opt-val "Display Columns" "Type") 2)
    (set-col (opt-val "Display Columns" "Memo") 3)
    (set-col (opt-val "Display Columns" "Value") 4)
    col-vector))

(define (make-heading-list column-vector)
  (let ((heading-list '()))
    (if (date-col column-vector)
        (addto! heading-list (_ "Date")))
    (if (num-col column-vector)
        (addto! heading-list (_ "Reference")))
    (if (type-col column-vector)
	(addto! heading-list (_ "Type")))
    (if (memo-col column-vector)
	(addto! heading-list (_ "Description")))
    (if (value-col column-vector)
	(addto! heading-list (_ "Amount")))
    (reverse heading-list)))


(define num-buckets 4)
(define (new-bucket-vector)
  (make-vector num-buckets (gnc:numeric-zero)))

(define (make-interval-list to-date)
  (let ((begindate to-date))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))


(define (make-aging-table options query bucket-intervals)
  (let ((lots (gnc:query-get-lots query 'query-txn-match-any))
	(buckets (new-bucket-vector))
	(payments (gnc:numeric-zero))
	(currency (gnc:default-currency)) ;XXX
	(table (gnc:make-html-table)))

    (define (in-interval this-date current-bucket)
      (gnc:timepair-lt this-date current-bucket))

    (define (find-bucket current-bucket bucket-intervals date)
      (begin
	(if (>= current-bucket (vector-length bucket-intervals))
	    (gnc:error "sanity check failed in find-bucket")
	    (if (in-interval date (vector-ref bucket-intervals current-bucket))
		current-bucket
		(find-bucket (+ current-bucket 1) bucket-intervals date)))))

    (define (apply-invoice date value)
      (let* ((bucket-index (find-bucket 0 bucket-intervals date))
	     (new-value (gnc:numeric-add-fixed
			 value
			 (vector-ref buckets bucket-index))))
	(vector-set! buckets bucket-index new-value)))

    (define (apply-payment value)
      (set! payments (gnc:numeric-add-fixed value payments)))

    (for-each
     (lambda (lot)
       (let* ((bal (gnc:lot-get-balance lot))
	      (invoice (gnc:invoice-get-invoice-from-lot lot))
	      (post-date (gnc:invoice-get-date-posted invoice)))

	 (if (not (gnc:numeric-zero-p bal))
	     (if invoice
		 (begin
		   (apply-invoice post-date bal))
		 (apply-payment bal)))))
     lots)

    (gnc:html-table-set-col-headers!
     table
     (list (N_ "0-30 days")
	   (N_ "31-60 days")
	   (N_ "61-90 days")
	   (N_ "91+ days")))

    (gnc:html-table-append-row!
     table
     (reverse (map (lambda (entry)
		     (gnc:make-gnc-monetary currency entry))
		   (vector->list buckets))))

    table))
		 
;;
;; Make sure the caller checks the type first and only calls us with
;; invoice and payment transactions.  we don't verify it here.
;;
;; Return a pair of (date . value)
;;
(define (add-txn-row table txn acc column-vector row-style)
  (let* ((type (gnc:transaction-get-txn-type txn))
	 (date (gnc:transaction-get-date-posted txn))
	 (value (gnc:transaction-get-account-value txn acc))
	 (split (gnc:transaction-get-split txn 0))
	 (invoice (gnc:invoice-get-invoice-from-txn txn))
	 (currency (gnc:transaction-get-currency txn))
	 (type-str
	  (cond
	   ((equal? type gnc:transaction-type-invoice)
	    (if invoice
		(gnc:make-html-text
		 (gnc:html-markup-anchor
		  (gnc:invoice-anchor-text invoice)
		  (N_ "Invoice")))
		(N_ "Invoice")))
	   ((equal? type gnc:transaction-type-payment) (N_ "Payment, thank you"))
	   (else (N_ "UNK"))))
	 (row-contents '()))

    (if (date-col column-vector)
	(addto! row-contents (gnc:print-date date)))
    (if (num-col column-vector)
	(addto! row-contents (gnc:transaction-get-num txn)))
    (if (type-col column-vector)
	(addto! row-contents type-str))
    (if (memo-col column-vector)
	(addto! row-contents (gnc:split-get-memo split)))
    (if (value-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup "number-cell"
		 (gnc:make-gnc-monetary
		  currency value))))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    (cons date value)
    ))


(define (make-txn-table options query acc report-date)
  (let ((txns (gnc:query-get-transactions query 'query-txn-match-any))
	(used-columns (build-column-used options))
	(odd-row? #t)
	(total (gnc:numeric-zero))
	(currency (gnc:default-currency)) ;XXX
	(table (gnc:make-html-table)))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns))

    ; Order the transactions properly
    (set! txns (sort txns (lambda (a b) (> 0 (gnc:transaction-order a b)))))

    (for-each
     (lambda (txn)
       (let ((type (gnc:transaction-get-txn-type txn))
	     (row-style (if odd-row? "normal-row" "alternate-row")))
	 (if
	  (or (equal? type gnc:transaction-type-invoice)
	      (equal? type gnc:transaction-type-payment))
	  (let ((dv (add-txn-row table txn acc used-columns row-style)))

	    (set! odd-row? (not odd-row?))
	    (set! total (gnc:numeric-add-fixed total (cdr dv)))
	    ))))
     txns)

    (gnc:html-table-append-row/markup! 
     table
     "grand-total"
     (append (cons (gnc:make-html-table-cell/markup
		    "total-label-cell"
		    (if (gnc:numeric-negative-p total)
			(N_ "Total Credit")
			(N_ "Total Due")))
		   '())
	     (list (gnc:make-html-table-cell/size/markup
		    1 (value-col used-columns)
		    "total-number-cell"
		    (gnc:make-gnc-monetary currency total)))))

    (let* ((interval-vec (list->vector (make-interval-list report-date))))
      (gnc:html-table-append-row/markup!
       table
       "grand-total"
       (list (gnc:make-html-table-cell/size/markup
	      0 (value-col used-columns)
	      "total-number-cell"
	      (make-aging-table options query interval-vec)))))

    table))

(define (options-generator acct-type-list)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-owner-option "__reg" "owner" "" ""
			    (lambda () #f) #f))

  (gnc:register-inv-option
   (gnc:make-account-list-limited-option "Account" "Account" "" ""
			    (lambda () '()) #f #f acct-type-list))

  (gnc:options-add-report-date!
   gnc:*report-options* gnc:pagename-general
   (N_ "To") "a")

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Date")
    "b" (N_ "Display the transaction date?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Num")
    "d" (N_ "Display the transaction reference?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Type")
    "g" (N_ "Display the transaction type?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Memo")
    "ha" (N_ "Display the transaction description?") #t))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option
    (N_ "Display Columns") (N_ "Value")
    "hb" "Display the transaction amount?" #t))

  (gnc:register-inv-option
   (gnc:make-string-option
    (N_ "Display") (N_ "Today Date Format")
    "v" (N_ "The format for the date->string conversion for today's date.")
    "~B ~e, ~Y"))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)
	     
(define (customer-options-generator)
  (options-generator '(receivable)))

(define (vendor-options-generator)
  (options-generator '(payable)))

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

(define (setup-query q owner account)
  (let* ((guid (gnc:owner-get-guid (gnc:owner-get-end-owner owner))))

    (gnc:query-add-guid-match
     q 
     (list gnc:split-trans gnc:invoice-from-txn gnc:invoice-owner
	   gnc:owner-parentg)
     guid 'query-or)
    (gnc:query-add-guid-match
     q
     (list gnc:split-lot gnc:owner-from-lot gnc:owner-parentg)
     guid 'query-or)
    (gnc:query-add-guid-match
     q
     (list gnc:split-lot gnc:invoice-from-lot gnc:invoice-owner
	   gnc:owner-parentg)
     guid 'query-or)

    (gnc:query-add-single-account-match q account 'query-and)
    (gnc:query-set-book q (gnc:get-current-book))
    q))

(define (make-owner-table owner)
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
	 (query (gnc:malloc-query))
	 (account-list (opt-val "Account" "Account"))
	 (account #f)
	 (owner (opt-val "__reg" "owner"))
	 (report-date (gnc:timepair-end-day-time 
		       (gnc:date-option-absolute-time
			(opt-val gnc:pagename-general (N_ "To")))))
	 (title #f))

    (if (not (null? account-list))
	(set! account (car account-list)))

    (if owner
	(begin
	  (setup-query query owner account)

	  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
		       (gnc:owner-get-type (gnc:owner-get-end-owner owner))
		       #f))
		(type-str ""))
	    (case type
	      ((gnc-owner-customer)
	       (set! type-str (N_ "Customer")))
	      
	      ((gnc-owner-vendor)
	       (set! type-str (N_ "Vendor"))))

	    (set! title (gnc:html-markup
			 "!" 
			 type-str
			 (N_ " Report: ")
			 (gnc:html-markup-anchor
			  (gnc:owner-anchor-text owner)
			  (gnc:owner-get-name owner)))))
	  
	  (if account
	      (begin
		(set! table (make-txn-table (gnc:report-options report-obj)
					    query account report-date))
		(gnc:html-table-set-style!
		 table "table"
		 'attribute (list "border" 1)
		 'attribute (list "cellspacing" 0)
		 'attribute (list "cellpadding" 4)))

	      (set! table (gnc:make-html-text "No Valid Account Selected")))

	  (gnc:html-document-set-title! document title)

	  (gnc:html-document-add-object!
	   document
	   (make-myname-table (opt-val "Display" "Today Date Format")))

	  (gnc:html-document-add-object!
	   document
	   (make-owner-table owner))

	  (make-break! document)
	  (make-break! document)

	  (gnc:html-document-add-object! document table))

	;; else....
	(gnc:html-document-add-object!
	 document
	 (gnc:make-html-text
	  "No Valid Company Selected")))

    (gnc:free-query query)
    document))

(define (find-first-account type)
  (define (find-first group num index)
    (if (>= index num)
	#f
	(let* ((this-account (gnc:group-get-account group index))
	       (account-type (gw:enum-<gnc:AccountType>-val->sym
			      (gnc:account-get-type this-account) #f)))
	  (if (eq? account-type type)
	      this-account
	      (find-first group num (+ index 1))))))

  (let* ((current-group (gnc:get-current-group))
	 (num-accounts (gnc:group-get-num-accounts
			current-group)))
    (if (> num-accounts 0)
	(find-first current-group num-accounts 0)
	#f)))

(define (find-first-account-for-owner owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type (gnc:owner-get-end-owner owner)) #f)))
    (case type
      ((gnc-owner-customer)
       (find-first-account 'receivable))

      ((gnc-owner-vendor)
       (find-first-account 'payable))

      ((gnc-owner-job)
       (find-first-account-for-owner (gnc:owner-get-end-owner owner)))

      (else
       #f))))

(gnc:define-report
 'version 1
 'name (N_ "Customer Report")
 'options-generator customer-options-generator
 'renderer reg-renderer
 'in-menu? #f)

(gnc:define-report
 'version 1
 'name (N_ "Vendor Report")
 'options-generator vendor-options-generator
 'renderer reg-renderer
 'in-menu? #f)

(define (owner-report-create-internal report-name owner account)
  (let* ((options (gnc:make-report-options report-name))
	 (owner-op (gnc:lookup-option options "__reg" "owner"))
	 (account-op (gnc:lookup-option options "Account" "Account")))

    (gnc:option-set-value owner-op owner)
    (gnc:option-set-value account-op (list account))
    (gnc:make-report report-name options)))

(define (owner-report-create owner account)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type (gnc:owner-get-end-owner owner)) #f)))
    (case type
      ((gnc-owner-customer)
       (owner-report-create-internal "Customer Report" owner account))

      ((gnc-owner-vendor)
       (owner-report-create-internal "Vendor Report" owner account))

      (else #f))))

(define (gnc:owner-report-create owner account)
  ; Figure out an account to use if nothing exists here.
  (if (not account)
      (set! account (find-first-account-for-owner owner)))

  (owner-report-create owner account))

(define (gnc:owner-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)

  (let* ((temp-owner (gnc:owner-create))
	 (owner (gnc:owner-from-split split temp-owner))
	 (res (gnc:owner-report-create owner account)))

    (gnc:owner-destroy temp-owner)
    res))

(gnc:register-report-hook 'receivable #t
			  gnc:owner-report-create-internal)

(gnc:register-report-hook 'payable #t
			  gnc:owner-report-create-internal)

(export gnc:owner-report-create)
