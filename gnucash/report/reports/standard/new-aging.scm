;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-aging.scm : accounts payable/receivable aging report
;;
;; By Christopher Lam, rewrite and debug
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

(define-module (gnucash reports standard new-aging))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))            ;let-values
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report" 0)

(use-modules (gnucash reports))

(define optname-to-date (N_ "To"))
(define optname-sort-order (N_ "Sort Order"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-zeros (N_ "Show zero balance items"))
(define optname-date-driver (N_ "Due or Post Date"))

;; Display tab options
(define optname-addr-source (N_ "Address Source"))

(define addr-options-list
  (list (list (N_ "Address Name") "b"
              (N_ "Display Address Name. This, and other fields, may be useful if \
copying this report to a spreadsheet for use in a mail merge."))
        (list (N_ "Address 1") "c" (N_ "Display Address 1."))
        (list (N_ "Address 2") "d" (N_ "Display Address 2."))
        (list (N_ "Address 3") "e" (N_ "Display Address 3."))
        (list (N_ "Address 4") "f" (N_ "Display Address 4."))
        (list (N_ "Address Phone") "g" (N_ "Display Phone."))
        (list (N_ "Address Fax") "h" (N_ "Display Fax."))
        (list (N_ "Address Email") "i" (N_ "Display Email."))
        (list (N_ "Active") "j" (N_ "Display Active status."))))

(define no-APAR-account (_ "No valid A/Payable or A/Receivable \
account found. Please ensure valid AP/AR account exists."))

(define empty-APAR-accounts (_ "A/Payable or A/Receivable accounts \
exist but have no suitable transactions."))

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
     options gnc:pagename-general optname-to-date "a")

    ;; Use a default report date of 'today'
    (gnc:option-set-default-value
     (gnc:lookup-option options gnc:pagename-general optname-to-date)
     (cons 'relative 'today))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-sort-order "ia" (N_ "Sort order.") 'increasing
      (list
       (vector 'increasing (N_ "Increasing") (N_ "Alphabetical order"))
       (vector 'decreasing (N_ "Decreasing") (N_ "Reverse alphabetical order")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-zeros "j"
      (N_ "Show all vendors/customers even if they have a zero balance.")
      #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-date-driver "k" (N_ "Leading date.") 'duedate
      (list
       ;; Should be using standard label for due date?
       (vector 'duedate
               (N_ "Due Date")
               (N_ "Due date is leading."))
       ;; Should be using standard label for post date?
       (vector 'postdate
               (N_ "Post Date")
               (N_ "Post date is leading.")))))

    (gnc:options-set-default-section options "General")

    (for-each
     (lambda (opt)
       (add-option
        (gnc:make-simple-boolean-option
         gnc:pagename-display (car opt) (cadr opt) (caddr opt) #f)))
     addr-options-list)

    options))

(define (options->address options receivable? owner)
  (define (op-value name)
    (gnc:option-value (gnc:lookup-option options gnc:pagename-display name)))
  (let* ((address-list-names (map car addr-options-list))
         (address-list-options (map op-value address-list-names))
         (addr-source (if receivable? (op-value optname-addr-source) 'billing))
         (result-list
          (cond
           (owner
            (let ((addr (if (eq? addr-source 'shipping)
                            (gncCustomerGetShipAddr (gncOwnerGetCustomer owner))
                            (gncOwnerGetAddr owner))))
              (list (gncAddressGetName addr)
                    (gncAddressGetAddr1 addr)
                    (gncAddressGetAddr2 addr)
                    (gncAddressGetAddr3 addr)
                    (gncAddressGetAddr4 addr)
                    (gncAddressGetPhone addr)
                    (gncAddressGetFax addr)
                    (gncAddressGetEmail addr)
                    (if (gncOwnerGetActive owner) (_ "Y") (_ "N")))))
           (else address-list-names))))
    (fold-right (lambda (opt elt prev) (if opt (cons elt prev) prev))
                '() address-list-options result-list)))

(define (txn-is-invoice? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-INVOICE))

(define (txn-is-payment? txn)
  (eqv? (xaccTransGetTxnType txn) TXN-TYPE-PAYMENT))

(define (gnc-owner-equal? a b)
  (string=? (gncOwnerReturnGUID a) (gncOwnerReturnGUID b)))

(define (split-has-owner? split owner)
  (let* ((split-owner (split->owner split))
         (retval (gnc-owner-equal? split-owner owner)))
    (gncOwnerFree split-owner)
    retval))

(define (split-from-acct? split acct)
  (equal? acct (xaccSplitGetAccount split)))

(define (list-split lst fn cmp)
  (let-values (((list-yes list-no) (partition (lambda (elt) (fn elt cmp)) lst)))
    (cons list-yes list-no)))

;; simpler version of gnc:owner-from-split. must be gncOwnerFree after
;; use! see split-has-owner? above...
(define (split->owner split)
  (let* ((lot (xaccSplitGetLot (gnc-lot-get-earliest-split (xaccSplitGetLot split))))
         (owner (gncOwnerNew))
         (use-lot-owner? (gncOwnerGetOwnerFromLot lot owner)))
    (unless use-lot-owner?
      (gncOwnerCopy (gncOwnerGetEndOwner
                     (gncInvoiceGetOwner (gncInvoiceGetInvoiceFromLot lot)))
                    owner))
    owner))

(define (aging-renderer report-obj receivable)
  (define options (gnc:report-options report-obj))
  (define (op-value section name)
    (gnc:option-value (gnc:lookup-option options section name)))

  (define make-heading-list
    (list ""
          (_ "Company")
          (_ "Prepayments")
          (_ "Current")
          (_ "0-30 days")
          (_ "31-60 days")
          (_ "61-90 days")
          (_ "91+ days")
          (_ "Total")))

  (let* ((type (if receivable ACCT-TYPE-RECEIVABLE ACCT-TYPE-PAYABLE))
         (accounts (filter (lambda (acc) (eqv? (xaccAccountGetType acc) type))
                           (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account))))
         (report-title (op-value gnc:pagename-general gnc:optname-reportname))
         (report-date (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (op-value gnc:pagename-general optname-to-date))))
         (sort-order (op-value gnc:pagename-general optname-sort-order))
         (show-zeros (op-value gnc:pagename-general optname-show-zeros))
         (date-type (op-value gnc:pagename-general optname-date-driver))
         (query (qof-query-create-for-splits))
         (document (gnc:make-html-document)))

    ;; for sorting and delete-duplicates. compare GUIDs
    (define (ownerGUID<? a b)
      (string<? (gncOwnerGetGUID a) (gncOwnerGetGUID b)))

    ;; for presentation. compare names.
    (define (owner<? a b)
      ((if (eq? sort-order 'increasing) string<? string>?)
       (gncOwnerGetName a) (gncOwnerGetName b)))

    ;; set default title
    (gnc:html-document-set-title! document report-title)

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document (gnc:make-html-text no-APAR-account)))

     (else
      (setup-query query accounts report-date)
      (let* ((splits (qof-query-run query))
             (accounts (sort-and-delete-duplicates (map xaccSplitGetAccount splits)
                                                   gnc:account-path-less-p equal?))
             (table (gnc:make-html-table)))
        (qof-query-destroy query)

        ;; loop into each APAR account
        (let loop ((accounts accounts)
                   (splits (filter
                            (lambda (split)
                              (or (txn-is-invoice? (xaccSplitGetParent split))
                                  (txn-is-payment? (xaccSplitGetParent split))))
                            splits)))
          (cond
           ((null? accounts)
            (gnc:html-table-set-col-headers!
             table (append make-heading-list
                           (options->address options receivable #f)))
            (gnc:html-document-add-object!
             document (if (null? (gnc:html-table-data table))
                          (gnc:make-html-text empty-APAR-accounts)
                          table)))

           (else
            (let* ((account (car accounts))
                   (comm (xaccAccountGetCommodity account))
                   (splits-acc-others (list-split splits split-from-acct? account))
                   (acc-splits (car splits-acc-others))
                   (other-acc-splits (cdr splits-acc-others)))

              (gnc:debug 'account account)
              (gnc:html-table-append-row!
               table (list (gnc:make-html-table-cell/size
                            1 (+ 2 num-buckets) (xaccAccountGetName account))))

              (let* ((split-owners (map split->owner acc-splits))
                     (acc-owners (sort (sort-and-delete-duplicates
                                        split-owners ownerGUID<? gnc-owner-equal?)
                                       owner<?)))

                (gnc:debug 'owners acc-owners)

                ;; loop into each APAR account split
                (let lp ((acc-owners acc-owners)
                         (acc-splits acc-splits)
                         (acc-totals (make-list (1+ num-buckets) 0)))
                  (cond
                   ((null? acc-owners)
                    (for-each gncOwnerFree split-owners)
                    (gnc:html-table-append-row!
                     table
                     (cons* #f
                            (gnc:make-html-table-cell/markup
                             "total-label-cell" (_ "Total"))
                            (map
                             (lambda (amt)
                               (gnc:make-html-table-cell/markup
                                "total-number-cell" (gnc:make-gnc-monetary comm amt)))
                             acc-totals)))
                    (loop (cdr accounts)
                          other-acc-splits))

                   (else
                    (let* ((owner (car acc-owners))
                           (splits-own-others (list-split acc-splits split-has-owner?
                                                          owner))
                           (owner-splits (car splits-own-others))
                           (other-owner-splits (cdr splits-own-others))
                           (aging (gnc:owner-splits->aging-list
                                   owner-splits num-buckets report-date
                                   date-type receivable))
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
                               "number-cell" (gnc:make-gnc-monetary comm amt)))
                            (reverse aging)))
                          (list
                           (gnc:make-html-table-cell/markup
                            "number-cell"
                            (gnc:make-html-text
                             (gnc:html-markup-anchor
                              (gnc:owner-report-text owner account)
                              (gnc:make-gnc-monetary comm aging-total)))))
                          (options->address options receivable owner))))
                      (lp (cdr acc-owners)
                          other-owner-splits
                          (map + acc-totals
                               (reverse (cons aging-total aging))))))))))))))))
    (gnc:report-finished)
    document))

(define (payable-options-generator)
  (aging-options-generator (gnc:new-options)))

(define (receivable-options-generator)
  (let ((options (aging-options-generator (gnc:new-options))))
    (define (add-option new-option)
      (gnc:register-option options new-option))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-addr-source "a" (N_ "Address source.") 'billing
      (list
       (vector 'billing
               (N_ "Billing")
               (N_ "Address fields from billing address."))
       (vector 'shipping
               (N_ "Shipping")
               (N_ "Address fields from shipping address.")))))
    options))

(define (payables-renderer report-obj)
  (aging-renderer report-obj #f))

(define (receivables-renderer report-obj)
  (aging-renderer report-obj #t))

(gnc:define-report
 'version 1
 'name (N_ "Payable Aging (beta)")
 'report-guid "e57770f2dbca46619d6dac4ac5469b50-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator payable-options-generator
 'renderer payables-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Receivable Aging (beta)")
 'report-guid "9cf76bed17f14401b8e3e22d0079cb98-new"
 'menu-path (list gnc:menuname-experimental)
 'options-generator receivable-options-generator
 'renderer receivables-renderer
 'in-menu? #t)
