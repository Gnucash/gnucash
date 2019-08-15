;; -*-scheme-*-
;; owner-report.scm -- Print out a detailed owner report, which is a
;;		       summary of invoices and payments for a particular
;;		       company (the owner) applied to an account.
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


(define-module (gnucash reports standard job-report))

(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash utilities))		; for gnc:debug
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report" 0)

(define acct-string (N_ "Account"))
(define owner-string (N_ "Job"))
(define owner-page gnc:pagename-general)

(define date-header (N_ "Date"))
(define due-date-header (N_ "Due Date"))
(define reference-header (N_ "Reference"))
(define type-header (N_ "Type"))
(define desc-header (N_ "Description"))
(define amount-header (N_ "Amount"))

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
(define (value-col columns-used)
  (vector-ref columns-used 5))

(define columns-used-size 6)

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
    (set-col (opt-val "Display Columns" amount-header) 5)
    col-vector))

(define (make-heading-list column-vector)
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
    (if (value-col column-vector)
	(addto! heading-list (_ amount-header)))
    (reverse heading-list)))


(define num-buckets 4)
(define (new-bucket-vector)
  (make-vector num-buckets (gnc-numeric-zero)))

(define (make-interval-list to-date)
  (let ((begindate to-date))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))


(define (make-aging-table options query bucket-intervals reverse? currency)
  (let ((lots (xaccQueryGetLots query QUERY-TXN-MATCH-ANY))
	(buckets (new-bucket-vector))
	(payments (gnc-numeric-zero))
        (table (gnc:make-html-table)))

    (define (in-interval this-date current-bucket)
      (< this-date current-bucket))

    (define (find-bucket current-bucket bucket-intervals date)
      (begin
	(if (>= current-bucket (vector-length bucket-intervals))
	    (gnc:error "sanity check failed in find-bucket")
	    (if (in-interval date (vector-ref bucket-intervals current-bucket))
		current-bucket
		(find-bucket (+ current-bucket 1) bucket-intervals date)))))

    (define (apply-invoice date value)
      (let* ((bucket-index (find-bucket 0 bucket-intervals date))
	     (new-value (gnc-numeric-add-fixed
			 value
			 (vector-ref buckets bucket-index))))
	(vector-set! buckets bucket-index new-value)))

    (define (apply-payment value)
      (set! payments (gnc-numeric-add-fixed value payments)))

    (for-each
     (lambda (lot)
       (let* ((bal (gnc-lot-get-balance lot))
	      (invoice (gncInvoiceGetInvoiceFromLot lot))
	      (post-date (gncInvoiceGetDatePosted invoice)))

	 (if (not (gnc-numeric-zero-p bal))
	     (begin
	       (if reverse?
		   (set! bal (gnc-numeric-neg bal)))
	       (if (not (null? invoice))
		   (begin
		     (apply-invoice post-date bal))
		   (apply-payment bal))))))
     lots)

    (gnc:html-table-set-col-headers!
     table
     (list (_ "0-30 days")
	   (_ "31-60 days")
	   (_ "61-90 days")
	   (_ "91+ days")))

    (gnc:html-table-append-row!
     table
     (reverse (map (lambda (entry)
		     (gnc:make-gnc-monetary currency entry))
		   (vector->list buckets))))

    table))
		 
;;
;; Make a row list based on the visible columns
;;
(define (make-row column-vector date due-date num type-str memo monetary)
  (let ((row-contents '()))
    (if (date-col column-vector)
	(addto! row-contents (qof-print-date date)))
    (if (date-due-col column-vector)
	(addto! row-contents 
                (if due-date
                    (qof-print-date due-date)
                    "")))
    (if (num-col column-vector)
	(addto! row-contents num))
    (if (type-col column-vector)
	(addto! row-contents type-str))
    (if (memo-col column-vector)
	(addto! row-contents memo))
    (if (value-col column-vector)
	(addto! row-contents
		(gnc:make-html-table-cell/markup "number-cell" monetary)))
    row-contents))

;;
;; Adds the 'Balance' row to the table if it has not been printed and
;; total is not zero
;;
;; Returns printed? 
;;
(define (add-balance-row table column-vector txn odd-row? printed? start-date total)
  (if (not printed?)
      (begin
	(set! printed? #t)
	(if (not (gnc-numeric-zero-p total))
	    (let ((row (make-row column-vector start-date #f "" (_ "Balance") ""
				 (gnc:make-gnc-monetary (xaccTransGetCurrency txn) total)))
		  (row-style (if odd-row? "normal-row" "alternate-row")))
	      (gnc:html-table-append-row/markup! table row-style (reverse row))
	      (set! odd-row? (not odd-row?))
	      (set! row-style (if odd-row? "normal-row" "alternate-row")))
	    )))
	printed?)

;;
;; Make sure the caller checks the type first and only calls us with
;; invoice and payment transactions.  we don't verify it here.
;;
;; Return a list of (printed? value odd-row?)
;;
(define (add-txn-row table txn acc column-vector odd-row? printed?
		     inv-str reverse? start-date total)
  (let* ((type (xaccTransGetTxnType txn))
	 (date (xaccTransGetDate txn))
	 (due-date #f)
	 (value (xaccTransGetAccountValue txn acc))
	 (split (xaccTransGetSplit txn 0))
	 (invoice (gncInvoiceGetInvoiceFromTxn txn))
	 (currency (xaccTransGetCurrency txn))
	 (type-str
	  (cond
	   ((equal? type TXN-TYPE-INVOICE)
	    (if (not (null? invoice))
		(gnc:make-html-text
		 (gnc:html-markup-anchor
		  (gnc:invoice-anchor-text invoice)
		  inv-str))
		inv-str))
	   ((equal? type TXN-TYPE-PAYMENT) (_ "Payment, thank you!"))
	   (else (_ "Unknown"))))
	 )

    (if reverse?
	(set! value (gnc-numeric-neg value)))

    (if (< start-date date)
	(begin
	  
	  ; Adds 'balance' row if needed
	  (set! printed? (add-balance-row table column-vector txn odd-row? printed? start-date total))
	  
	  ; Now print out the invoice row
          (if (and (not (null? invoice))
                   (gncInvoiceIsPosted invoice))
              (set! due-date (gncInvoiceGetDateDue invoice)))

	  (let ((row (make-row column-vector date due-date (gnc-get-num-action txn split)
			       type-str (xaccSplitGetMemo split)
			       (gnc:make-gnc-monetary currency value)))
		(row-style (if odd-row? "normal-row" "alternate-row")))

	    (gnc:html-table-append-row/markup! table row-style
					       (reverse row)))

	  (set! odd-row? (not odd-row?))
	  ))

    (list printed? value odd-row?)
    ))


(define (make-txn-table options query acc start-date end-date)
  (let ((txns (xaccQueryGetTransactions query QUERY-TXN-MATCH-ANY))
	(used-columns (build-column-used options))
	(total (gnc-numeric-zero))
        (currency (xaccAccountGetCommodity acc))
	(table (gnc:make-html-table))
	(inv-str (gnc:option-value (gnc:lookup-option options "__reg"
						      "inv-str")))
	(reverse? (gnc:option-value (gnc:lookup-option options "__reg"
						      "reverse?"))))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns))

    ; Order the transactions properly
    (set! txns (sort txns (lambda (a b) (> 0 (xaccTransOrder a b)))))

    (let ((printed? #f)
	  (odd-row? #t))
      (for-each
       (lambda (txn)
	 (let ((type (xaccTransGetTxnType txn)))
	   (if
	    (or (equal? type TXN-TYPE-INVOICE)
		(equal? type TXN-TYPE-PAYMENT))
	    (let ((result (add-txn-row table txn acc used-columns odd-row? printed?
				       inv-str reverse? start-date total)))

	      (set! printed? (car result))
	      (set! total (gnc-numeric-add-fixed total (cadr result)))
	      (set! odd-row? (caddr result))
	      ))))
       txns)
	  ;Balance row may not have been added if all transactions were before
	  ;start-date (and no other rows would be added either) so add it now
      (if (not (null? txns))
	  (add-balance-row table used-columns (car txns) odd-row? printed? start-date total)
		))

    (gnc:html-table-append-row/markup! 
     table
     "grand-total"
     (append (cons (gnc:make-html-table-cell/markup
		    "total-label-cell"
		    (if (gnc-numeric-negative-p total)
			(_ "Total Credit")
			(_ "Total Due")))
		   '())
	     (list (gnc:make-html-table-cell/size/markup
		    1 (value-col used-columns)
		    "total-number-cell"
		    (gnc:make-gnc-monetary currency total)))))

    (let* ((interval-vec (list->vector (make-interval-list end-date))))
      (gnc:html-table-append-row/markup!
       table
       "grand-total"
       (list (gnc:make-html-table-cell/size/markup
	      1 (+ 1 (value-col used-columns))
	      "centered-label-cell"
	      (make-aging-table options query interval-vec reverse? currency)))))

    table))

(define (options-generator acct-type-list owner-type inv-str reverse?)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-internal-option "__reg" "inv-str" inv-str))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option "__reg" "reverse?" "" "" reverse?))

  (gnc:register-inv-option
   (gnc:make-owner-option owner-page owner-string "v"
			  (N_ "The job for this report.")
			  (lambda () '()) #f owner-type))

  (gnc:register-inv-option
   (gnc:make-internal-option "__reg" "owner-type" owner-type))

  (gnc:register-inv-option
   (gnc:make-account-sel-limited-option owner-page acct-string "w"
					(N_ "The account to search for transactions.")
					#f #f acct-type-list))

  (gnc:options-add-date-interval!
   gnc:*report-options* gnc:pagename-general
   (N_ "From") (N_ "To") "a")

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
    (N_ "Display Columns") amount-header
    "hb" (N_ "Display the transaction amount?") #t))

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)
	     
(define (job-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE) GNC-OWNER-JOB
                     (_ "Invoice") #f))

(define (customer-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE) GNC-OWNER-CUSTOMER
                     (_ "Invoice") #f))

(define (vendor-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-VENDOR
                     (_ "Bill") #t))

(define (employee-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-EMPLOYEE
                     (_ "Expense Report") #t))

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

(define (setup-query q owner account end-date)
  (let* ((guid (gncOwnerReturnGUID owner)))

    (qof-query-add-guid-match
     q 
     (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER
	   QOF-PARAM-GUID)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q
     (list SPLIT-LOT OWNER-FROM-LOT QOF-PARAM-GUID)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q
     (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER
	   QOF-PARAM-GUID)
     guid QOF-QUERY-OR)

    (xaccQueryAddSingleAccountMatch q account QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT q #f end-date #t end-date QOF-QUERY-AND)
    (qof-query-set-book q (gnc-get-current-book))
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
      (string-expand (gnc:owner-get-name-and-address-dep owner) #\newline "<br/>")))
    (gnc:html-table-append-row!
     table
     (list "<br/>"))
    (gnc:html-table-set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (make-date-row! table label date)
  (gnc:html-table-append-row!
   table
   (list
    (string-append label " ")
    (qof-print-date date))))

(define (make-date-table)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
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

    (gnc:html-table-append-row! table (list (if name name "")))
    (gnc:html-table-append-row! table (list (string-expand
					     (if addy addy "")
					     #\newline "<br/>")))
    (gnc:html-table-append-row!
     table (list (gnc-print-time64 (current-time) date-format)))
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
	 (query (qof-query-create-for-splits))
	 (account (opt-val owner-page acct-string))
	 (owner (opt-val owner-page owner-string))
	 (start-date (gnc:time64-start-day-time 
		       (gnc:date-option-absolute-time
			(opt-val gnc:pagename-general (N_ "From")))))
	 (end-date (gnc:time64-end-day-time 
		       (gnc:date-option-absolute-time
			(opt-val gnc:pagename-general (N_ "To")))))
	 (book (gnc-get-current-book))
         (date-format (gnc:options-fancy-date book))
	 (type (opt-val "__reg" "owner-type"))
	 (type-str "")
         (report-title-str ""))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (set! type-str (N_ "Customer"))
       (set! report-title-str (_ "Customer Report")))
      ((eqv? type GNC-OWNER-JOB)
       (set! type-str (N_ "Job"))
       (set! report-title-str (_ "Job Report")))
      ((eqv? type GNC-OWNER-VENDOR)
       (set! type-str (N_ "Vendor"))
       (set! report-title-str (_ "Vendor Report")))
      ((eqv? type GNC-OWNER-EMPLOYEE)
       (set! type-str (N_ "Employee"))
       (set! report-title-str (_ "Employee Report"))))

    (gnc:html-document-set-title! document report-title-str)

    (if (gncOwnerIsValid owner)
	(begin
	  (setup-query query owner account end-date)

	  (gnc:html-document-set-title!
	   document
           (string-append report-title-str ": " (gncOwnerGetName owner)))

           (gnc:html-document-set-headline!
            document (gnc:html-markup
                      "span"
                      report-title-str ": "
                      (gnc:html-markup-anchor
					   (gnc:job-anchor-text (gncOwnerGetJob owner))
                       (gncOwnerGetName owner))))
	  
	  (if (not (null? account))
	      (begin
		(set! table (make-txn-table (gnc:report-options report-obj)
					    query account start-date end-date))
		(gnc:html-table-set-style!
		 table "table"
		 'attribute (list "border" 1)
		 'attribute (list "cellspacing" 0)
		 'attribute (list "cellpadding" 4)))

	      (set!
	       table
	       (gnc:make-html-text
		(_ "No valid account selected. Click on the Options button and select the account to use."))))

	  (gnc:html-document-add-object!
	   document
	   (make-myname-table book date-format))

	  (gnc:html-document-add-object!
	   document
	   (make-owner-table owner))

	  (make-break! document)

	  (gnc:html-document-add-object!
	   document
	   (gnc:make-html-text
	    (string-append
	     (_ "Date Range")
	     ": "
	     (qof-print-date start-date)
	     " - "
	     (qof-print-date end-date))))

	  (make-break! document)

	  (gnc:html-document-add-object! document table))

	;; else....
	(gnc:html-document-add-object!
         document
         (gnc:make-html-text
          (string-append
           (cond
            ((eqv? type GNC-OWNER-CUSTOMER)
             (_ "No valid customer selected."))
            ((eqv? type GNC-OWNER-JOB)
             (_ "No valid job selected."))
            ((eqv? type GNC-OWNER-VENDOR)
             (_ "No valid vendor selected."))
            ((eqv? type GNC-OWNER-EMPLOYEE)
             (_ "No valid employee selected."))
            (else ""))
           " "
           (_ "Click on the \"Options\" button to select a company.")))))

    (qof-query-destroy query)
    document))

(define (find-first-account type)
  (define (find-first account num index)
    (if (>= index num)
	'()
	(let* ((this-child (gnc-account-nth-child account index))
	       (account-type (xaccAccountGetType this-child)))
	  (if (eq? account-type type)
	      this-child
	      (find-first account num (+ index 1))))))

  (let* ((current-root (gnc-get-current-root-account))
	 (num-accounts (gnc-account-n-children current-root)))
    (if (> num-accounts 0)
	(find-first current-root num-accounts 0)
	'())))

(define (find-first-account-for-owner owner)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (find-first-account ACCT-TYPE-RECEIVABLE))

      ((eqv? type GNC-OWNER-VENDOR)
       (find-first-account ACCT-TYPE-PAYABLE))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (find-first-account ACCT-TYPE-PAYABLE))

      ((eqv? type GNC-OWNER-JOB)
       (find-first-account-for-owner (gncOwnerGetEndOwner owner)))

      (else
       '()))))

(gnc:define-report
 'version 1
 'name (N_ "Job Report")
 'report-guid "5518ac227e474f47a34439f2d4d049de"
 'menu-path (list gnc:menuname-business-reports)
 'options-generator job-options-generator
 'renderer reg-renderer
 'in-menu? #t)
