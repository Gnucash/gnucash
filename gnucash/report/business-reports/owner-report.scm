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
(use-modules (srfi srfi-8))
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

;; let's define a name for the report-guid's, much prettier
(define employee-report-guid "08ae9c2e884b4f9787144f47eacd7f44")
(define vendor-report-guid "d7d1e53505ee4b1b82efad9eacedaea0")
(define customer-report-guid "c146317be32e4948a561ec7fc89d15c1")
(define job-report-guid "5518ac227e474f47a34439f2d4d049de")

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
  (make-vector num-buckets 0))

;; Have make-list create a stepped list, then add a date in the
;; infinite future for the "current" bucket
(define (make-extended-interval-list to-date)
  (let* ((begindate to-date)
         (begindate (decdate begindate ThirtyDayDelta))
         (begindate (decdate begindate ThirtyDayDelta))
         (begindate (decdate begindate ThirtyDayDelta)))
    (append (gnc:make-date-list begindate to-date ThirtyDayDelta)
            (list +inf.0))))

(define (make-aging-table options query bucket-intervals reverse? date-type currency)
  (let ((lots (xaccQueryGetLots query QUERY-TXN-MATCH-ANY))
        (buckets (new-bucket-vector))
        (table (gnc:make-html-table)))

    (for-each
     (lambda (lot)
       (let* ((bal ((if reverse? - identity) (gnc-lot-get-balance lot)))
              (invoice (gncInvoiceGetInvoiceFromLot lot))
              (date (if (eq? date-type 'postdate)
                        (gncInvoiceGetDatePosted invoice)
                        (gncInvoiceGetDateDue invoice))))

         (unless (null? invoice)
           (let loop ((idx 0))
             (if (< date (vector-ref bucket-intervals idx))
                 (vector-set! buckets idx (+ bal (vector-ref buckets idx)))
                 (loop (1+ idx)))))))
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
     (map
      (lambda (entry)
        (gnc:make-gnc-monetary currency entry))
      (reverse (vector->list buckets))))
    table))

(define (addif pred? elt) (if pred? (list elt) '()))

(define (make-cell elt) (gnc:make-html-table-cell/markup "number-cell" elt))

;;
;; Make a row list based on the visible columns
;;
(define (make-row column-vector date due-date num type-str
                  memo monetary credit debit sale tax)
  (append
   (addif (date-col column-vector) (qof-print-date date))
   (addif (date-due-col column-vector) (and due-date (qof-print-date due-date)))
   (addif (num-col column-vector)    (gnc:html-string-sanitize num))
   (addif (type-col column-vector)   type-str)
   (addif (memo-col column-vector)   (gnc:html-string-sanitize memo))
   (addif (sale-col column-vector)   (make-cell sale))
   (addif (tax-col column-vector)    (make-cell tax))
   (addif (credit-col column-vector) (make-cell credit))
   (addif (debit-col column-vector)  (make-cell debit))
   (addif (value-col column-vector)  (make-cell monetary))))

(define (make-txn-table options query acc start-date end-date date-type)
  (let ((txns (sort (xaccQueryGetTransactions query QUERY-TXN-MATCH-ANY)
                    (lambda (a b)
                      (negative? (xaccTransOrder a b)))))
        (used-columns (build-column-used options))
        (currency (xaccAccountGetCommodity acc))
        (reverse? (gnc:option-value (gnc:lookup-option options "__reg" "reverse?")))
        (table (gnc:make-html-table)))

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
            (list (total-cell (_ "Period Totals")))
            (addif (>= span 2) (gnc:make-html-table-cell/size 1 (1- span) ""))
            (addif (sale-col used-columns)   (make-cell sale))
            (addif (tax-col used-columns)    (make-cell tax))
            (addif (credit-col used-columns) (make-cell credit))
            (addif (debit-col used-columns)  (make-cell debit))
            (addif (value-col used-columns) (make-cell (+ credit debit)))
            (addif (> link-cols 0) (gnc:make-html-table-cell/size 1 link-cols #f)))))

      ;; print grand total
      (if (value-col used-columns)
          (gnc:html-table-append-row/markup!
           table "grand-total"
           (list (gnc:make-html-table-cell/markup
                  "total-label-cell"
                  (if (negative? total)
                      (_ "Total Credit")
                      (_ "Total Due")))
                 (gnc:make-html-table-cell/size/markup
                  1 (value-col used-columns)
                  "total-number-cell"
                  (gnc:make-gnc-monetary currency total)))))

      ;; print aging table
      (gnc:html-table-append-row/markup!
       table "grand-total"
       (list (gnc:make-html-table-cell/size
              1 columns-used-size
              (make-aging-table options query
                                (list->vector (make-extended-interval-list end-date))
                                reverse? date-type currency)))))

    (define (add-balance-row odd-row? total)
      (gnc:html-table-append-row/markup!
       table (if odd-row? "normal-row" "alternate-row")
       (make-row used-columns start-date #f "" (_ "Balance") ""
                 (gnc:make-gnc-monetary currency total)
                 "" "" "" "")))

    (gnc:html-table-set-col-headers!
     table (make-heading-list used-columns))

    (let lp ((printed? #f)
             (odd-row? #t)
             (txns txns)
             (total 0)
             (debit 0)
             (credit 0)
             (tax 0)
             (sale 0))
      (cond

       ((null? txns)
        ;;Balance row may not have been added if all transactions were before
        ;;start-date (and no other rows would be added either) so add it now
        (when (and (not printed?) (value-col used-columns) (not (zero? total)))
          (add-balance-row odd-row? total))

        (print-totals total debit credit tax sale))

       ;; transactions left to print.
       (else
        (let* ((txn (car txns))
               (date (xaccTransGetDate txn))
               (type (xaccTransGetTxnType txn)))
          (cond

           ;; not an invoice/payment. skip transaction.
           ((not (memv type (list TXN-TYPE-INVOICE TXN-TYPE-PAYMENT)))
            (lp printed?
                odd-row?
                (cdr txns)
                total
                debit
                credit
                tax
                sale))

           ;; txn-date < start-date. skip display, accumulate amounts
           ((< date start-date)
            (let ((value ((if reverse? - identity)
                          (xaccTransGetAccountValue txn acc))))
              (lp printed?
                  odd-row?
                  (cdr txns)
                  (+ total value)
                  (if (negative? value) (+ debit value) debit)
                  (if (negative? value) credit (+ credit value))
                  tax
                  sale)))

           ;; if balance row hasn't been rendered, consider printing
           ;; here. skip if value=0.
           ((not printed?)
            (let ((print? (and (value-col used-columns) (not (zero? total)))))
              (if print? (add-balance-row odd-row? total))
              (lp #t
                  (logxor (if odd-row? 1 0) (if print? 1 0))
                  txns total debit credit tax sale links)))

           ;; start printing txns.
           (else
            (let* ((value ((if reverse? - identity)
                           (xaccTransGetAccountValue txn acc)))
                   (split (xaccTransGetSplit txn 0))
                   (invoice (gncInvoiceGetInvoiceFromTxn txn))
                   (credit-note? (gncInvoiceGetIsCreditNote invoice))
                   (type-str (cond
                              ((and (eqv? type TXN-TYPE-INVOICE)
                                    (not (null? invoice)))
                               (gnc:make-html-text
                                (gnc:html-markup-anchor
                                 (gnc:invoice-anchor-text invoice)
                                 (gncInvoiceGetTypeString invoice))))
                              ((eqv? type TXN-TYPE-PAYMENT)
                               (gnc:make-html-text
	                        (gnc:html-markup-anchor
	                         (gnc:split-anchor-text split)
                                 (_ "Payment"))))
                              (else (_ "Unknown"))))
                   (txn-sale (cond
                              ((null? invoice) 0)
                              (credit-note? (- (gncInvoiceGetTotalSubtotal invoice)))
                              (else (gncInvoiceGetTotalSubtotal invoice))))
                   (txn-tax (cond
                             ((null? invoice) 0)
                             (credit-note? (- (gncInvoiceGetTotalTax invoice)))
                             (else (gncInvoiceGetTotalTax invoice))))
                   (due-date (and (not (null? invoice))
                                  (gncInvoiceIsPosted invoice)
                                  (gncInvoiceGetDateDue invoice))))

              (gnc:html-table-append-row/markup!
               table (if odd-row? "normal-row" "alternate-row")
               (make-row used-columns date due-date (gnc-get-num-action txn split)
                         type-str (xaccSplitGetMemo split)
                         (gnc:make-gnc-monetary currency value)
                         (if (negative? value)
                             ""
                             (gnc:make-gnc-monetary currency value))
                         (if (negative? value)
                             (gnc:make-gnc-monetary currency value)
                             "")
                         (if (null? invoice)
                             ""
                             (gnc:make-gnc-monetary currency txn-sale))
                         (if (null? invoice)
                             ""
                             (gnc:make-gnc-monetary currency txn-tax))))

              (lp printed?
                  (not odd-row?)
                  (cdr txns)
                  (+ total value)
                  (if (negative? value) (+ debit value) debit)
                  (if (negative? value) credit (+ credit value))
                  (+ tax txn-tax)
                  (+ sale txn-sale)))))))))
    table))

(define (options-generator acct-type-list owner-type reverse?)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-inv-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-inv-option
   (gnc:make-simple-boolean-option "__reg" "reverse?" "" "" reverse?))

  (gnc:register-inv-option
   (gnc:make-owner-option
    owner-page (owner-string owner-type) "v"
    (N_ "The company for this report.")
    (lambda () '()) #f owner-type))

  (gnc:register-inv-option
   (gnc:make-internal-option "__reg" "owner-type" owner-type))

  (gnc:register-inv-option
   (gnc:make-account-sel-limited-option
    owner-page acct-string "w"
    (N_ "The account to search for transactions.")
    #f #f acct-type-list))

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

(define (customer-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE) GNC-OWNER-CUSTOMER #f))

(define (vendor-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-VENDOR #t))

(define (employee-options-generator)
  (options-generator (list ACCT-TYPE-PAYABLE) GNC-OWNER-EMPLOYEE #t))

(define (job-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE ACCT-TYPE-PAYABLE)
                     GNC-OWNER-JOB #f))

(define (string-expand string character replace-string)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (display
          (if (char=? c character)
              replace-string
              c)))
       string))))

(define (setup-job-query q owner account end-date)
  (let ((guid (gncOwnerReturnGUID owner)))
    (qof-query-add-guid-match
     q  (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER QOF-PARAM-GUID)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT OWNER-FROM-LOT QOF-PARAM-GUID)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER QOF-PARAM-GUID)
     guid QOF-QUERY-OR)
    (xaccQueryAddSingleAccountMatch q account QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT q #f end-date #t end-date QOF-QUERY-AND)
    (qof-query-set-book q (gnc-get-current-book))
    q))

(define (setup-query q owner account end-date)
  (let ((guid (gncOwnerReturnGUID (gncOwnerGetEndOwner owner))))
    (qof-query-add-guid-match
     q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER OWNER-PARENTG)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT OWNER-FROM-LOT OWNER-PARENTG)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER OWNER-PARENTG)
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
    (string-append label ": ")
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

    (when name
      (gnc:html-table-append-row! table (list name)))
    (when addy
      (gnc:html-table-append-row! table (list (string-expand addy #\newline "<br/>"))))

    (gnc:html-table-append-row!
     table (list (gnc-print-time64 (gnc:get-today) date-format)))
    table))

(define (make-break! document)
  (gnc:html-document-add-object!
   document
   (gnc:make-html-text
    (gnc:html-markup-br))))

(define (reg-renderer report-obj)
  (define options (gnc:report-options report-obj))
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let* ((document (gnc:make-html-document))
         (query (qof-query-create-for-splits))
         (account (opt-val owner-page acct-string))
         (start-date (gnc:time64-start-day-time
                      (gnc:date-option-absolute-time
                       (opt-val gnc:pagename-general optname-from-date))))
         (end-date (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (opt-val gnc:pagename-general optname-to-date))))
         (book (gnc-get-current-book))
         (date-format (gnc:options-fancy-date (gnc-get-current-book)))
         (type (opt-val "__reg" "owner-type"))
         (owner-descr (owner-string type))
         (date-type (opt-val gnc:pagename-general optname-date-driver))
         (owner (opt-val owner-page owner-descr))
         (report-title (string-append (owner-string type) " " (_ "Report"))))

    (cond
     ((not (gncOwnerIsValid owner))
      (gnc:html-document-add-object!
       document
       (gnc:html-make-generic-warning
        report-title
        (gnc:report-id report-obj)
        (cadr (get-info type))
        (caddr (get-info type)))))

     ((null? account)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-generic-warning
        (string-append report-title ": " (gncOwnerGetName owner))
        (gnc:report-id report-obj)
        (_ "No valid account selected")
        (_ "This report requires a valid account to be selected."))))

     (else
      (if (eqv? GNC-OWNER-JOB type)
          (setup-job-query query owner account end-date)
          (setup-query query owner account end-date))

      (gnc:html-document-set-title!
       document (string-append report-title ": " (gncOwnerGetName owner)))

      (gnc:html-document-set-headline!
       document (gnc:html-markup
                 "span" (owner-string type) " " (_ "Report:") " "
                 (gnc:html-markup-anchor
                  (gnc:owner-anchor-text owner)
                  (gncOwnerGetName owner))))

      (let ((table (make-txn-table
                    options query account start-date end-date date-type)))

        (qof-query-destroy query)

        (gnc:html-table-set-style!
         table "table"
         'attribute (list "border" 1)
         'attribute (list "cellspacing" 0)
         'attribute (list "cellpadding" 4))

        (gnc:html-document-add-object!
         document (make-myname-table book date-format))

        (gnc:html-document-add-object!
         document (make-owner-table owner))

        (make-break! document)

        (gnc:html-document-add-object!
         document
         (gnc:make-html-text
          (string-append (_ "Date Range") ": " (qof-print-date start-date)
                         " - " (qof-print-date end-date))))

        (make-break! document)

        (gnc:html-document-add-object! document table))))
    document))

(define* (find-first-account type #:key currency)
  (or (find
       (lambda (acc)
         (and (eqv? type (xaccAccountGetType acc))
              (or (not currency)
                  (gnc-commodity-equiv currency (xaccAccountGetCommodity acc)))))
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
      '()))

(define* (find-first-account-for-owner owner #:key currency)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (find-first-account ACCT-TYPE-RECEIVABLE #:currency currency))

      ((eqv? type GNC-OWNER-VENDOR)
       (find-first-account ACCT-TYPE-PAYABLE #:currency currency))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (find-first-account ACCT-TYPE-PAYABLE #:currency currency))

      ((eqv? type GNC-OWNER-JOB)
       (find-first-account-for-owner (gncOwnerGetEndOwner owner)
                                     #:currency currency))
      (else '()))))

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

(gnc:define-report
 'version 1
 'name (N_ "Job Report")
 'report-guid job-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator job-options-generator
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
     ;; Not sure whether to pass type, or to use the guid in the report function
     ((eqv? type GNC-OWNER-CUSTOMER)
      (owner-report-create-internal customer-report-guid owner account type))

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
         (res (if (null? owner)
                  -1
                  (gnc:owner-report-create owner account))))
    (gncOwnerFree temp-owner)
    res))

(gnc:register-report-hook ACCT-TYPE-RECEIVABLE #t gnc:owner-report-create-internal)

(gnc:register-report-hook ACCT-TYPE-PAYABLE #t gnc:owner-report-create-internal)

(export find-first-account-for-owner owner-report-create)
