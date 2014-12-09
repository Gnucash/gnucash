;; -*-scheme-*-
;; owner-report.scm -- Print out a detailed owner report, which is a
;;                     summary of invoices and payments for a particular
;;                     company (the owner) applied to an account.
;;
;; Created by:  Derek Atkins <warlord@MIT.EDU>
;; Copyright (c) 2002, 2003 Derek Atkins <warlord@MIT.EDU>
;; Modified by AMM to show tax figures of invoice.
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


(define-module (gnucash report owner-report))

(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash main))        ; for gnc:debug
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

;; Option names
(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-date-driver (N_ "Due or Post Date"))

;; let's define a name for the report-guid's, much prettier
(define employee-report-guid "08ae9c2e884b4f9787144f47eacd7f44")
(define vendor-report-guid "d7d1e53505ee4b1b82efad9eacedaea0")
(define customer-report-guid "c146317be32e4948a561ec7fc89d15c1")

(define acct-string (N_ "Account"))
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
(define amount-header (N_ "Amount"))

;; Depending on the report type we want to set up some lists/cases 
;; with strings to ease overview and translation
;; note: we default to company

;; owner-string & doctype-str are nearly equivalent, report is vendor report
;; but default option naming was Company. 

;; Names in Option panel (Untranslated! Because it is used for option
;; naming and lookup only, and the display of the option name will be
;; translated somewhere else.)
(define (owner-string owner-type)
  (cond ((eqv? owner-type GNC-OWNER-CUSTOMER) (N_ "Customer"))
        ((eqv? owner-type GNC-OWNER-EMPLOYEE) (N_ "Employee"))
        ;; FALL THROUGH
        (else (N_ "Company")))) 

;; Error strings in case there is no (valid) selection (translated)
(define (invalid-selection-title-string owner-type)
  (cond ((eqv? owner-type GNC-OWNER-CUSTOMER) (_ "No valid customer selected."))
        ((eqv? owner-type GNC-OWNER-EMPLOYEE) (_ "No valid employee selected."))
        ;; FALL THROUGH
        (else (_ "No valid company selected."))))

(define (invalid-selection-string owner-type)
  (cond ((eqv? owner-type GNC-OWNER-CUSTOMER) (_ "This report requires a customer to be selected."))
        ((eqv? owner-type GNC-OWNER-EMPLOYEE) (_ "This report requires a employee to be selected."))
        ;; FALL THROUGH
        (else (_ "This report requires a company to be selected."))))

;; Html formatted error message documents
(define (gnc:html-make-no-owner-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    invalid-selection-title-string
    invalid-selection-string))

(define (gnc:html-make-no-valid-account-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    (_ "No valid account selected")
    (_ "This report requires a valid account to be selected.")))


;; Document names, used in report names (translated)
(define (doctype-str owner-type)
  (cond ((eqv? owner-type GNC-OWNER-CUSTOMER) (_ "Customer"))
        ((eqv? owner-type GNC-OWNER-EMPLOYEE) (_ "Employee"))
        ;; FALL THROUGH
        (else (_ "Vendor")))) 

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
    (reverse heading-list)))


(define num-buckets 5)
(define (new-bucket-vector)
  (make-vector num-buckets (gnc-numeric-zero)))

(define (make-interval-list to-date)
  (let ((begindate to-date))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (set! begindate (decdate begindate ThirtyDayDelta))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))

;; Have make-list create a stepped list, then add a date in the future for the "current" bucket 
(define (make-extended-interval-list to-date) 
    (define dayforcurrent (incdate to-date YearDelta)) ;; MAGIC CONSTANT 
    (define oldintervalreversed (reverse (make-interval-list to-date)))          
    (reverse (cons dayforcurrent oldintervalreversed))) 

(define (make-aging-table options query bucket-intervals reverse? date-type)
  (let ((lots (xaccQueryGetLots query QUERY-TXN-MATCH-ANY))
    (buckets (new-bucket-vector))
    (payments (gnc-numeric-zero))
    (currency (gnc-default-currency)) ;XXX
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
              (date (if (eq? date-type 'postdate)
               (gncInvoiceGetDatePosted invoice) 
               (gncInvoiceGetDateDue invoice)))
              )
         
     (if (not (gnc-numeric-zero-p bal))
         (begin
           (if reverse?
           (set! bal (gnc-numeric-neg bal)))
           (if (not (null? invoice))
           (begin
             (apply-invoice date bal))
           (apply-payment bal))))))
     lots)

    (gnc:html-table-set-col-headers!
     table
     (list (_ "Current")
           (_ "0-30 days")
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
(define (make-row column-vector date due-date num type-str memo monetary credit debit sale tax)
  (let ((row-contents '()))
    (if (date-col column-vector)
        (addto! row-contents (gnc-print-date date)))
    (if (date-due-col column-vector)
        (addto! row-contents 
         (if (and due-date
              (not (equal? due-date (cons 0 0))))
             (gnc-print-date due-date)
             "")))
    (if (num-col column-vector)
        (addto! row-contents num))
    (if (type-col column-vector)
        (addto! row-contents type-str))
    (if (memo-col column-vector)
        (addto! row-contents memo))
    (if (sale-col column-vector)
        (addto! row-contents
         (gnc:make-html-table-cell/markup "number-cell" sale)))
    (if (tax-col column-vector)
        (addto! row-contents
         (gnc:make-html-table-cell/markup "number-cell" tax)))
    (if (credit-col column-vector)
        (addto! row-contents
         (gnc:make-html-table-cell/markup "number-cell" credit)))
    (if (debit-col column-vector)
        (addto! row-contents
         (gnc:make-html-table-cell/markup "number-cell" debit)))
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
    (if (and (value-col column-vector) (not (gnc-numeric-zero-p total)))
        (let ((row (make-row column-vector start-date #f "" (_ "Balance") ""
                 (gnc:make-gnc-monetary (xaccTransGetCurrency txn) total) "" "" "" ""))
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
             reverse? start-date total)
  (let* ((type (xaccTransGetTxnType txn))
     (date (gnc-transaction-get-date-posted txn))
     (due-date #f)
     (value (xaccTransGetAccountValue txn acc))
     (sale (gnc-numeric-zero))
     (tax (gnc-numeric-zero))
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
          (gncInvoiceGetTypeString invoice)))
        (_ "Unknown")))
       ((equal? type TXN-TYPE-PAYMENT)
        (gnc:make-html-text
	 (gnc:html-markup-anchor
	  (gnc:split-anchor-text split) (_ "Payment"))))
       (else (_ "Unknown"))))
     )

   (if reverse?
    (set! value (gnc-numeric-neg value)))

   (if (gnc:timepair-le start-date date)
    (begin
      
      ; Adds 'balance' row if needed
      (set! printed? (add-balance-row table column-vector txn odd-row? printed? start-date total))
      
      ; Now print out the invoice row
      (if (not (null? invoice))
        (begin
          (set! due-date (gncInvoiceGetDateDue invoice))
          (set! sale (gncInvoiceGetTotalSubtotal invoice))
          (set! tax (gncInvoiceGetTotalTax invoice))))

      (if (gncInvoiceGetIsCreditNote invoice)
        (begin
          (set! tax (gnc-numeric-neg tax))
          (set! sale (gnc-numeric-neg sale))))

      (let ((row (make-row column-vector date due-date (gnc-get-num-action txn split)
                   type-str (xaccSplitGetMemo split)
                   (gnc:make-gnc-monetary currency value)
           (if (not (gnc-numeric-negative-p value))
               (gnc:make-gnc-monetary currency value) "")
           (if (gnc-numeric-negative-p value)
               (gnc:make-gnc-monetary currency value) "")
           (if (not (null? invoice))
               (gnc:make-gnc-monetary currency sale) "")
           (if (not (null? invoice))
               (gnc:make-gnc-monetary currency tax) "")
        ))
        (row-style (if odd-row? "normal-row" "alternate-row")))

        (gnc:html-table-append-row/markup! table row-style
                           (reverse row)))

      (set! odd-row? (not odd-row?))
      ))

    (list printed? value odd-row? sale tax)
    ))


(define (make-txn-table options query acc start-date end-date date-type)
  (let ((txns (xaccQueryGetTransactions query QUERY-TXN-MATCH-ANY))
    (used-columns (build-column-used options))
    (total (gnc-numeric-zero))
    (debit (gnc-numeric-zero))
    (credit (gnc-numeric-zero))
    (tax (gnc-numeric-zero))
    (sale (gnc-numeric-zero))
    (currency (gnc-default-currency)) ;XXX
    (table (gnc:make-html-table))
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
                       reverse? start-date total)))

          (set! printed? (car result))
          (if (and printed? total)
            (begin
              (set! sale (gnc-numeric-add-fixed sale (cadddr result)))
              (set! tax (gnc-numeric-add-fixed tax (car (cddddr result))))
              (if (gnc-numeric-negative-p (cadr result))
                (set! debit (gnc-numeric-add-fixed debit (cadr result)))
                (set! credit (gnc-numeric-add-fixed credit (cadr result))))))
          (set! total (gnc-numeric-add-fixed total (cadr result)))
          (set! odd-row? (caddr result))
          ))))
       txns)
      ;Balance row may not have been added if all transactions were before
      ;start-date (and no other rows would be added either) so add it now
      (if (not (null? txns))
      (add-balance-row table used-columns (car txns) odd-row? printed? start-date total)
        ))

    (if (or (sale-col used-columns) (tax-col used-columns) (credit-col used-columns) (debit-col used-columns))
    (gnc:html-table-append-row/markup! 
     table
     "grand-total"
     (append (cons (gnc:make-html-table-cell/markup
            "total-label-cell"
            (_ "Period Totals"))
           '())

     (let ((row-contents '())
         (pre-span 0))

      ; HTML gets generated in reverse order
      (if (value-col used-columns) (addto! row-contents
          (gnc:make-html-table-cell/size/markup
          1 1 "total-number-cell"
          (gnc:make-gnc-monetary currency (gnc-numeric-add-fixed credit debit)))))
      (if (debit-col used-columns) (addto! row-contents
          (gnc:make-html-table-cell/size/markup
          1 1 "total-number-cell"
          (gnc:make-gnc-monetary currency debit))))
      (if (credit-col used-columns) (addto! row-contents
          (gnc:make-html-table-cell/size/markup
          1 1 "total-number-cell"
          (gnc:make-gnc-monetary currency credit))))
      (if (tax-col used-columns) (addto! row-contents
          (gnc:make-html-table-cell/size/markup
          1 1 "total-number-cell"
          (gnc:make-gnc-monetary currency tax))))
      (if (sale-col used-columns) (addto! row-contents
          (gnc:make-html-table-cell/size/markup
          1 1 "total-number-cell"
          (gnc:make-gnc-monetary currency sale))))
      (if (memo-col used-columns) (set! pre-span (+ pre-span 1)))
      (if (type-col used-columns) (set! pre-span (+ pre-span 1)))
      (if (num-col used-columns) (set! pre-span (+ pre-span 1)))
      (if (date-due-col used-columns) (set! pre-span (+ pre-span 1)))
      (if (date-col used-columns) (set! pre-span (+ pre-span 1)))
      (if (>= pre-span 2) (addto! row-contents (gnc:make-html-table-cell/size 1 (- pre-span 1) "")))
     row-contents))))

    (if (value-col used-columns)
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
            (gnc:make-gnc-monetary currency total))))))

    (let* ((interval-vec (list->vector (make-extended-interval-list end-date))))
      (gnc:html-table-append-row/markup!
       table
       "grand-total"
       (list (gnc:make-html-table-cell/size
          1 columns-used-size
          (make-aging-table options query interval-vec reverse? date-type)))))

    table))

(define (options-generator acct-type-list owner-type reverse?)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
   (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option "__reg" "reverse?" "" "" reverse?))

  (gnc:register-inv-option
   (gnc:make-owner-option owner-page (owner-string owner-type) "v"
              (N_ "The company for this report.")
              (lambda () '()) #f owner-type))

  (gnc:register-inv-option
   (gnc:make-internal-option "__reg" "owner-type" owner-type))

  (gnc:register-inv-option
   (gnc:make-account-sel-limited-option owner-page acct-string "w"
                    (N_ "The account to search for transactions.")
                    #f #f acct-type-list))

  (gnc:options-add-date-interval!
   gnc:*report-options* gnc:pagename-general
   optname-from-date optname-to-date "a")
  ;; Use a default report date of 'today'
  (gnc:option-set-value (gnc:lookup-option gnc:*report-options*
                                           gnc:pagename-general
                                           optname-to-date)
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
   (gnc:make-string-option
    gnc:pagename-general (N_ "Today Date Format")
    "p" (N_ "The format for the date->string conversion for today's date.")
    (gnc-default-strftime-date-format)))
  
  (gnc:register-inv-option 
   (gnc:make-multichoice-option 
    gnc:pagename-general 
    optname-date-driver 
    "k" 
    (N_ "Leading date.") 
    'duedate 
    (list 
     (vector 'duedate (N_ "Due Date") (N_ "Due date is leading.")) ;; Should be using standard label for due date? 
     (vector 'postdate (N_ "Post Date") (N_ "Post date is leading."))))) ;; Should be using standard label for post date? 

  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

(define (customer-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE) GNC-OWNER-CUSTOMER #f))

(define (vendor-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-VENDOR #t))

(define (employee-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-EMPLOYEE #t))

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
  (let* ((guid (gncOwnerReturnGUID (gncOwnerGetEndOwner owner))))

    (qof-query-add-guid-match
     q 
     (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER
       OWNER-PARENTG)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q
     (list SPLIT-LOT OWNER-FROM-LOT OWNER-PARENTG)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q
     (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER
       OWNER-PARENTG)
     guid QOF-QUERY-OR)

    (xaccQueryAddSingleAccountMatch q account QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS q #f end-date #t end-date QOF-QUERY-AND)
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
      (string-expand (gnc:owner-get-name-and-address-dep owner) #\newline "<br>")))
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
    (string-expand (gnc-print-date date) #\space "&nbsp;"))))

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

(define (make-myname-table book date-format)
  (let* ((table (gnc:make-html-table))
     (slots (qof-book-get-slots book))
     (name (kvp-frame-get-slot-path-gslist
        slots (append gnc:*kvp-option-path*
                  (list gnc:*business-label* gnc:*company-name*))))
     (addy (kvp-frame-get-slot-path-gslist
        slots (append gnc:*kvp-option-path*
                  (list gnc:*business-label* gnc:*company-addy*)))))

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
                         #\newline "<br>")))
    (gnc:html-table-append-row! table (list
                       (strftime
                    date-format
                    (localtime (car (gnc:get-today))))))
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
     (start-date (gnc:timepair-start-day-time 
      (gnc:date-option-absolute-time
       (opt-val gnc:pagename-general optname-from-date))))
     (end-date (gnc:timepair-end-day-time 
               (gnc:date-option-absolute-time
               (opt-val gnc:pagename-general optname-to-date))))
     (book (gnc-get-current-book)) ;XXX Grab this from elsewhere
     (type (opt-val "__reg" "owner-type"))
     (owner-descr (owner-string type))
     (date-type (opt-val gnc:pagename-general optname-date-driver)) 
     (owner (opt-val owner-page owner-descr))
     (report-title (string-append (doctype-str type) " " (_ "Report"))))

    (if (not (gncOwnerIsValid owner))
     (gnc:html-document-add-object!
      document
      (gnc:html-make-no-owner-warning
       report-title (gnc:report-id report-obj)))

    ;; else....
     (begin
      (set! report-title (string-append report-title ": " (gncOwnerGetName owner)))
      (if (null? account)
       (gnc:html-document-add-object!
        document
        (gnc:html-make-no-valid-account-warning
         report-title (gnc:report-id report-obj)))

      ;; else....
       (begin
        (setup-query query owner account end-date)
        (gnc:html-document-set-title! document report-title)

        (gnc:html-document-set-headline!
         document (gnc:html-markup
                   "!" 
                   (doctype-str type)
                   " " (_ "Report:") " "
                   (gnc:html-markup-anchor
                    (gnc:owner-anchor-text owner)
                    (gncOwnerGetName owner))))
      
        (set! table (make-txn-table (gnc:report-options report-obj)
                        query account start-date end-date date-type))
        (gnc:html-table-set-style!
         table "table"
         'attribute (list "border" 1)
         'attribute (list "cellspacing" 0)
         'attribute (list "cellpadding" 4))

        (gnc:html-document-add-object!
         document
         (make-myname-table book (opt-val gnc:pagename-general (N_ "Today Date Format"))))

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
           (gnc-print-date start-date)
           " - "
           (gnc-print-date end-date))))

        (make-break! document)

        (gnc:html-document-add-object! document table)
        (qof-query-destroy query)))))
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
 'name (N_ "Customer Report")
 'report-guid customer-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator customer-options-generator
 'renderer reg-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Vendor Report")
 'report-guid vendor-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator vendor-options-generator
 'renderer reg-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Employee Report")
 'report-guid employee-report-guid 
 'menu-path (list gnc:menuname-business-reports)
 'options-generator employee-options-generator
 'renderer reg-renderer
 'in-menu? #t)

(define (owner-report-create-internal report-guid owner account owner-type)
  (let* ((options (gnc:make-report-options report-guid))
     (owner-op (gnc:lookup-option options owner-page (owner-string owner-type))) 
     (account-op (gnc:lookup-option options owner-page acct-string)))

    (gnc:option-set-value owner-op owner)
    (gnc:option-set-value account-op account)
    (gnc:make-report report-guid options)))

(define (owner-report-create owner account)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (owner-report-create-internal customer-report-guid owner account type)) ;; Not sure whether to pass type, or to use the guid in the report function

      ((eqv? type GNC-OWNER-VENDOR)
       (owner-report-create-internal vendor-report-guid owner account type))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (owner-report-create-internal employee-report-guid owner account type))

      (else #f))))

(define (gnc:owner-report-create-internal
     account split query journal? double? title
     debit-string credit-string)

  (let* ((temp-owner (gncOwnerNew))
     (owner (gnc:owner-from-split split temp-owner))
     (res -1)) ;; XXX -- in this case we should create an error report

    (if (not (null? owner))
    (set! res (gnc:owner-report-create owner account)))

    (gncOwnerFree temp-owner)
    res))

(gnc:register-report-hook ACCT-TYPE-RECEIVABLE #t
              gnc:owner-report-create-internal)

(gnc:register-report-hook ACCT-TYPE-PAYABLE #t
              gnc:owner-report-create-internal)

(export find-first-account-for-owner owner-report-create)
