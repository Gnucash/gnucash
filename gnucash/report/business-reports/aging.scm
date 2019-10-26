;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aging.scm : accounts payable/receivable aging report utilities
;;  
;; By Derek Atkins <warlord@MIT.EDU> taken from the original...
;; By Robert Merkel (rgmerk@mira.net) 
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report aging))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))            ;let-values
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(define optname-to-date (N_ "To"))
(define optname-sort-order (N_ "Sort Order"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-zeros (N_ "Show zero balance items"))
(define optname-date-driver (N_ "Due or Post Date"))

;; Display tab options
(define optname-addr-source (N_ "Address Source")) ;; Billing or Shipping addresses
(define optname-disp-addr-name (N_ "Address Name"))
(define optname-disp-addr1 (N_ "Address 1"))
(define optname-disp-addr2 (N_ "Address 2"))
(define optname-disp-addr3 (N_ "Address 3"))
(define optname-disp-addr4 (N_ "Address 4"))
(define optname-disp-addr-phone (N_ "Address Phone"))
(define optname-disp-addr-fax (N_ "Address Fax"))
(define optname-disp-addr-email (N_ "Address Email"))
(define optname-disp-active (N_ "Active"))

(define no-APAR-account (_ "No valid A/Payable or A/Receivable \
account found. Please ensure valid AP/AR account exists."))

(define empty-APAR-accounts (_ "A/Payable or A/Receivable accounts \
exist but have no suitable transactions."))

(export optname-show-zeros)

(define num-buckets 6)

(define (setup-query query accounts date)
  (qof-query-set-book query (gnc-get-current-book))
  (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
  (xaccQueryAddAccountMatch query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
  (xaccQueryAddDateMatchTT query #f 0 #t date QOF-QUERY-AND)
  (qof-query-set-sort-order query (list SPLIT-TRANS TRANS-DATE-POSTED) '() '())
  (qof-query-set-sort-increasing query #t #t #t))

(define (aging-options-generator options)
  (let* ((add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (gnc:options-add-report-date!
     options gnc:pagename-general
     optname-to-date "a")

    ;; Use a default report date of 'today'
    (gnc:option-set-default-value
     (gnc:lookup-option options gnc:pagename-general optname-to-date)
     (cons 'relative 'today))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general
      optname-sort-order
      "ia"
      (N_ "Sort order.")
      'increasing
      (list
       (vector 'increasing (N_ "Increasing") (N_ "Alphabetical order"))
       (vector 'decreasing (N_ "Decreasing") (N_ "Reverse alphabetical order")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general
      optname-show-zeros
      "j"
      (N_ "Show all vendors/customers even if they have a zero balance.")
      #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general
      optname-date-driver
      "k"
      (N_ "Leading date.")
      'duedate
      (list
       ;; Should be using standard label for due date?
       (vector 'duedate
               (N_ "Due Date")
               (N_ "Due date is leading.")) 
       ;; Should be using standard label for post date?
       (vector 'postdate
               (N_ "Post Date")
               (N_ "Post date is leading.")))))

    ;; display tab options

    ;; option optname-addr-source is added in receivables.scm
    ;; as cannot access the value of an option in aging-options-generator

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr-name
      "b"
      (N_ "Display Address Name. This, and other fields, may be useful if \
copying this report to a spreadsheet for use in a mail merge.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr1
      "c"
      (N_ "Display Address 1.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr2
      "d"
      (N_ "Display Address 2.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr3
      "e"
      (N_ "Display Address 3.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr4
      "f"
      (N_ "Display Address 4.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr-phone
      "g"
      (N_ "Display Phone.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr-fax
      "h"
      (N_ "Display Fax.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-addr-email
      "i"
      (N_ "Display Email.")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      optname-disp-active
      "j"
      (N_ "Display Active status.")
      #f))

    (gnc:options-set-default-section options "General")
    options))

(define (make-interval-list to-date)
  (let* ((begindate to-date)
         (begindate (decdate begindate ThirtyDayDelta))
         (begindate (decdate begindate ThirtyDayDelta))
         (begindate (decdate begindate ThirtyDayDelta)))
    (gnc:make-date-list begindate to-date ThirtyDayDelta)))

;; Have make-list create a stepped list, then add a date in the future for the "current" bucket
(define (make-extended-interval-list to-date)
  (append (make-interval-list to-date)
          (list +inf.0)))

(define (txn-is-invoice? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-INVOICE))

(define (txn-is-payment? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-PAYMENT))

;; this is required because (equal? owner-a owner-b) doesn't always
;; return #t even if owner-a and owner-b refer to the same owner
(define (gnc-owner-equal? a b)
  (string=? (gncOwnerReturnGUID a) (gncOwnerReturnGUID b)))

;; simpler version of gnc:owner-from-split.
;; must be gncOwnerFree after use!
(define (split->owner split)
  (let* ((lot (xaccSplitGetLot (gnc-lot-get-earliest-split (xaccSplitGetLot split))))
         (owner (gncOwnerNew))
         (use-lot-owner? (gncOwnerGetOwnerFromLot lot owner)))
    (unless use-lot-owner?
      (gncOwnerCopy (gncOwnerGetEndOwner
                     (gncInvoiceGetOwner (gncInvoiceGetInvoiceFromLot lot)))
                    owner))
    owner))

(define (owner-splits->aging-list splits to-date date-type reverse?)
  (gnc:debug 'processing: (qof-print-date to-date) date-type 'reverse? reverse?)
  (for-each gnc:debug splits)
  (let ((bucket-dates (make-extended-interval-list to-date))
        (buckets (make-vector num-buckets 0))
        (lots (map (compose gncInvoiceGetPostedLot gncInvoiceGetInvoiceFromTxn)
                   (filter txn-is-invoice? (map xaccSplitGetParent splits)))))
    (for-each
     (lambda (lot)
       (let* ((invoice (gncInvoiceGetInvoiceFromLot lot))
              (bal (gnc-lot-get-balance lot))
              ;; (bal ((if reverse? - identity) bal))
              ;; (bal ((if (gncInvoiceGetIsCreditNote invoice) - identity) bal))
              (date (if (eq? date-type 'postdate)
                        (gncInvoiceGetDatePosted invoice)
                        (gncInvoiceGetDateDue invoice))))
         (unless (null? invoice)
           (let loop ((idx 0)
                      (bucket-dates bucket-dates))
             (gnc:debug idx buckets bal invoice date)
             (if (< date (car bucket-dates))
                 (vector-set! buckets idx (+ bal (vector-ref buckets idx)))
                 (loop (1+ idx) (cdr bucket-dates)))))
         (gnc:debug '* buckets bal invoice date)))
     lots)

    ;; process prepayments, i.e. payments without associated invoices
    (let lp ((splits (filter (compose txn-is-payment? xaccSplitGetParent) splits))
             (prepayments 0))
      (cond
       ((null? splits)
        (vector-set! buckets (1- num-buckets) prepayments))
       ((null? (gncInvoiceGetInvoiceFromLot (xaccSplitGetLot (car splits))))
        (lp (cdr splits) (+ prepayments (xaccSplitGetAmount (car splits)))))
       (else
        (lp (cdr splits) prepayments))))

    (vector->list buckets)))

(define (aging-renderer report-obj reportname APARaccount reverse?)
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  ;; XXX: This is a hack - will be fixed when we move to a
  ;; more general interval scheme in this report
  (define make-heading-list
    (list
     ""
     (_ "Company")
     (_ "Prepayments")
     (_ "Current")
     (_ "0-30 days")
     (_ "31-60 days")
     (_ "61-90 days")
     (_ "91+ days")
     (_ "Total")))

  (when (or reportname APARaccount)
    (issue-deprecation-warning "reportname and account are no longer in use."))

  (let* ((accounts (filter (compose xaccAccountIsAPARType xaccAccountGetType)
                           (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account))))
         (receivable (eq? (op-value "__hidden" "receivable-or-payable") 'R))
         (accounts (filter
                    (lambda (acc)
                      (eqv? (xaccAccountGetType acc)
                            (if receivable ACCT-TYPE-RECEIVABLE ACCT-TYPE-PAYABLE)))
                    accounts))
         (report-title (op-value gnc:pagename-general gnc:optname-reportname))
         ;; document will be the HTML document that we return.
         (report-date (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (op-value gnc:pagename-general optname-to-date))))
         ;; (sort-pred (get-sort-pred
         ;;             (op-value gnc:pagename-general optname-sort-by)
         ;;             (op-value gnc:pagename-general optname-sort-order)))
         (sort-order (op-value gnc:pagename-general optname-sort-order))
         (show-zeros (op-value gnc:pagename-general optname-show-zeros))
         (date-type (op-value gnc:pagename-general optname-date-driver))
         (disp-addr-source (if receivable
                               (op-value gnc:pagename-display optname-addr-source)
                               'billing))
         (disp-addr-name (op-value gnc:pagename-display optname-disp-addr-name))
         (disp-addr1 (op-value gnc:pagename-display optname-disp-addr1))
         (disp-addr2 (op-value gnc:pagename-display optname-disp-addr2))
         (disp-addr3 (op-value gnc:pagename-display optname-disp-addr3))
         (disp-addr4 (op-value gnc:pagename-display optname-disp-addr4))
         (disp-addr-phone (op-value gnc:pagename-display optname-disp-addr-phone))
         (disp-addr-fax (op-value gnc:pagename-display optname-disp-addr-fax))
         (disp-addr-email (op-value gnc:pagename-display optname-disp-addr-email))
         (disp-active (op-value gnc:pagename-display optname-disp-active))
         (query (qof-query-create-for-splits))
         (document (gnc:make-html-document)))

    
    ;; return pointer to either billing or shipping address
    ;;  note customers have a shipping address but not vendors
    (define (get-addr owner disp-addr-source)
      (if (and receivable (eq? disp-addr-source 'shipping))
          (gncCustomerGetShipAddr (gncOwnerGetCustomer owner)) ;; shipping
          (gncOwnerGetAddr owner)))                            ;; billing

    ;; set default title
    (gnc:html-document-set-title! document report-title)

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:make-html-text no-APAR-account)))

     (else
      (setup-query query accounts report-date)
      (let* ((splits (qof-query-run query))
             (accounts (delete-duplicates (map xaccSplitGetAccount splits)))
             (table (gnc:make-html-table)))
        (qof-query-destroy query)
        (let loop ((accounts accounts)
                   (splits (filter
                            (lambda (split)
                              (or (txn-is-invoice? (xaccSplitGetParent split))
                                  (txn-is-payment? (xaccSplitGetParent split))))
                            splits)))
          (cond
           ((null? accounts)
            (gnc:html-table-set-col-headers! table make-heading-list)
            (gnc:html-document-add-object!
             document
             (if (null? (gnc:html-table-data table))
                 (gnc:make-html-text empty-APAR-accounts)
                 table)))
           (else
            (let ((account (car accounts)))
              (let-values (((acc-splits other-acc-splits)
                            (partition
                             (lambda (split)
                               (equal? account (xaccSplitGetAccount split)))
                             splits)))
                (gnc:debug 'account account)
                (gnc:html-table-append-row!
                 table
                 (list
                  (gnc:make-html-table-cell/size
                   1 (+ 2 num-buckets)
                   (xaccAccountGetName account))))
                (let* ((acc-owners
                        (sort-and-delete-duplicates
                         (map (compose gncOwnerGetEndOwner gncInvoiceGetOwner
                                       gncInvoiceGetInvoiceFromTxn xaccSplitGetParent)
                              (filter (compose txn-is-invoice? xaccSplitGetParent)
                                      acc-splits))
                         (lambda (a b)
                           ((if (eq? sort-order 'increasing) string<? string>?)
                            (gncOwnerGetName a) (gncOwnerGetName b)))
                         gnc-owner-equal?)))
                  (gnc:debug 'owners acc-owners)
                  (let lp ((acc-owners acc-owners)
                           (acc-splits acc-splits)
                           (acc-totals (make-list (1+ num-buckets) 0)))
                    (cond
                     ((null? acc-owners)
                      (gnc:html-table-append-row!
                       table
                       (cons*
                        #f
                        (gnc:make-html-table-cell/markup
                         "total-label-cell" (_ "Total"))
                        (map
                         (lambda (amt)
                           (gnc:make-html-table-cell/markup
                            "total-number-cell" (gnc:make-gnc-monetary
                                                 (xaccAccountGetCommodity account)
                                                 amt)))
                         acc-totals)))
                      (loop (cdr accounts)
                            other-acc-splits))
                     (else
                      (let-values (((owner-splits other-owner-splits)
                                    (partition
                                     (lambda (split)
                                       (let* ((owner (split->owner split))
                                              (match? (gnc-owner-equal? (car acc-owners) owner)))
                                         (gncOwnerFree owner)
                                         match?))
                                     acc-splits)))
                        (let* ((owner (car acc-owners))
                               (aging (owner-splits->aging-list
                                       owner-splits report-date date-type reverse?))
                               (aging-total (apply + aging)))
                          (when (or show-zeros (not (every zero? aging)))
                            (gnc:html-table-append-row!
                             table
                             (append
                              (list #f)
                              (cons
                               (gnc:make-html-text
                                (gnc:html-markup-anchor
                                 (gnc:owner-anchor-text owner)
                                 (gncOwnerGetName owner)))
                               (map
                                (lambda (amt)
                                  (gnc:make-html-table-cell/markup
                                   "number-cell"
				   (gnc:make-gnc-monetary
                                    (xaccAccountGetCommodity account)
                                    amt)))
                                (reverse aging)))
                              (list
                               (gnc:make-html-table-cell/markup
                                "number-cell"
                                (gnc:make-html-text
                                 (gnc:html-markup-anchor
			          (gnc:owner-report-text owner account)
			          (gnc:make-gnc-monetary
                                   (xaccAccountGetCommodity account)
                                   aging-total))))))))
                          (lp (cdr acc-owners)
                              other-owner-splits
                              (map + acc-totals
                                   (reverse (cons aging-total aging))))))))))))))))))
    (gnc:report-finished)
    document))

(export aging-options-generator)
(export aging-renderer)
