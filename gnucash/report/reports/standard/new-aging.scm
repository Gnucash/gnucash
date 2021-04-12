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
(use-modules (ice-9 match))
(use-modules (gnucash utilities))
(use-modules (gnucash engine))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define optname-to-date (N_ "To"))
(define optname-sort-by (N_ "Sort By"))
(define optname-sort-order (N_ "Sort Order"))
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

(define no-APAR-account (G_ "No valid A/Payable or A/Receivable \
account found. Please ensure valid AP/AR account exists."))

(define empty-APAR-accounts (G_ "A/Payable or A/Receivable accounts \
exist but have no suitable transactions."))

(define num-buckets 6)

(define (setup-query query accounts date)
  (qof-query-set-book query (gnc-get-current-book))
  (xaccQueryAddClearedMatch
   query (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)
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
      gnc:pagename-general optname-sort-by "i" (N_ "Sort companies by.") 'name
      (list
       (vector 'name (N_ "Name of the company"))
       (vector 'total (N_ "Total amount owed to/from Company"))
       (vector 'oldest-bracket (N_ "Bracket Total Owed")))))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-sort-order "ia" (N_ "Sort order.") 'increasing
      (list
       (vector 'increasing (N_ "Ascending"))
       (vector 'decreasing (N_ "Descending")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-zeros "j"
      (N_ "Show all vendors/customers even if they have a zero balance.")
      #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-date-driver "k" (N_ "Leading date.") 'duedate
      (list
       (vector 'duedate (N_ "Due Date"))
       (vector 'postdate (N_ "Post Date")))))

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
                    (if (gncOwnerGetActive owner) (C_ "One-letter indication for 'yes'" "Y") (C_ "One-letter indication for 'no'" "N")))))
           (else address-list-names))))
    (fold-right (lambda (opt elt prev) (if opt (cons elt prev) prev))
                '() address-list-options result-list)))

(define (split-is-not-business? split)
  (let ((type (xaccTransGetTxnType (xaccSplitGetParent split))))
    (not (or (eqv? type TXN-TYPE-INVOICE)
             (eqv? type TXN-TYPE-PAYMENT)))))

(define (split-has-owner? split owner)
  (gncOwnerEqual (gnc:split->owner split) owner))

(define (split-owner-is-invalid? split)
  (not (gncOwnerIsValid (gnc:split->owner split))))

(define (split-from-acct? split acct)
  (equal? acct (xaccSplitGetAccount split)))

(define (list-split lst fn cmp)
  (let-values (((list-yes list-no) (partition (lambda (elt) (fn elt cmp)) lst)))
    (cons list-yes list-no)))

(define (aging-renderer report-obj receivable)
  (define options (gnc:report-options report-obj))
  (define (op-value section name)
    (gnc:option-value (gnc:lookup-option options section name)))

  (define make-heading-list
    (list (G_ "Company")
          (G_ "Pre-Payment")
          (G_ "Current")
          (G_ "0-30 days")
          (G_ "31-60 days")
          (G_ "61-90 days")
          (G_ "91+ days")
          (G_ "Total")))

  (let* ((type (if receivable ACCT-TYPE-RECEIVABLE ACCT-TYPE-PAYABLE))
         (accounts (filter (lambda (acc) (eqv? (xaccAccountGetType acc) type))
                           (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account))))
         (report-title (op-value gnc:pagename-general gnc:optname-reportname))
         (report-date (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (op-value gnc:pagename-general optname-to-date))))
         (sort-order (op-value gnc:pagename-general optname-sort-order))
         (sort-by (op-value gnc:pagename-general optname-sort-by))
         (show-zeros (op-value gnc:pagename-general optname-show-zeros))
         (date-type (op-value gnc:pagename-general optname-date-driver))
         (query (qof-query-create-for-splits))
         (document (gnc:make-html-document)))

    (define (sort-aging<? a b)
      (match-let* (((own1 aging1 aging-total1) a)
                   ((own2 aging2 aging-total2) b)
                   (increasing? (eq? sort-order 'increasing))
                   (op-str (if increasing? gnc:string-locale<? gnc:string-locale>?))
                   (op-num (if increasing? < >)))
        (case sort-by
          ((name)  (op-str (gncOwnerGetName own1) (gncOwnerGetName own2)))
          ((total) (op-num aging-total1 aging-total2))
          (else
           (let lp ((aging1 aging1) (aging2 aging2))
             (cond
              ((null? aging1) (op-str (gncOwnerGetName own1) (gncOwnerGetName own2)))
              ((= (car aging1) (car aging2)) (lp (cdr aging1) (cdr aging2)))
              (else (op-num (car aging1) (car aging2)))))))))

    ;; set default title
    (gnc:html-document-set-title!
     document
     (format #f "~a - ~a" report-title (qof-print-date report-date)))

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document (gnc:make-html-text no-APAR-account)))

     (else
      (setup-query query accounts report-date)
      (let* ((splits (xaccQueryGetSplitsUniqueTrans query)))
        (qof-query-destroy query)

        ;; split->owner hashtable should be empty at the start of
        ;; report renderer. clear it anyway.
        (gnc:split->owner #f)

        ;; loop into each APAR account
        (let loop ((accounts accounts)
                   (splits splits)
                   (accounts-and-owners '())
                   (invalid-splits '()))
          (cond
           ((null? accounts)

            (cond
             ((null? accounts-and-owners)
              (gnc:html-document-add-object!
               document (gnc:make-html-text empty-APAR-accounts)))

             (else
              (let ((table (gnc:make-html-table))
                    (accounts>1? (> (length accounts-and-owners) 1)))

                (gnc:html-table-set-col-headers!
                 table (append (if accounts>1? '(#f) '())
                               make-heading-list
                               (options->address options receivable #f)))

                (for-each
                 (lambda (account-and-owners)
                   (let* ((account (car account-and-owners))
                          (owners-and-aging (cadr account-and-owners))
                          (acc-totals (caddr account-and-owners))
                          (comm (xaccAccountGetCommodity account)))

                     (when accounts>1?
                       (gnc:html-table-append-row!
                        table (list (gnc:make-html-table-cell/size
                                     1 (+ 2 num-buckets)
                                     (gnc:make-html-text
                                      (gnc:html-markup-anchor
                                       (gnc:account-anchor-text account)
                                       (xaccAccountGetName account)))))))

                     (for-each
                      (lambda (owner-and-aging)
                        (let ((owner (car owner-and-aging))
                              (aging (cadr owner-and-aging))
                              (aging-total (caddr owner-and-aging)))

                          (gnc:html-table-append-row!
                           table
                           (append
                            (if accounts>1? '(#f) '())
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
                                (gnc:owner-report-text owner account report-date)
                                (gnc:make-gnc-monetary comm aging-total)))))
                            (options->address options receivable owner)))))
                      (sort owners-and-aging sort-aging<?))

                     (gnc:html-table-append-row!
                      table
                      (append
                       (if accounts>1? '(#f) '())
                       (list (gnc:make-html-table-cell/markup
                              "total-label-cell" (G_ "Total")))
                       (map
                        (lambda (amt)
                          (gnc:make-html-table-cell/markup
                           "total-number-cell" (gnc:make-gnc-monetary comm amt)))
                        acc-totals)))))
                 (reverse accounts-and-owners))

                (gnc:split->owner #f)       ;free the gncOwners
                (gnc:html-document-add-object! document table)

                (unless (null? invalid-splits)
                  (gnc:html-document-add-object!
                   document (gnc:make-html-text (gnc:html-markup-br)))

                  (gnc:html-document-add-object!
                   document
                   (gnc:make-html-text
                    (G_ "Please note some transactions were not processed")
                    (gnc:html-markup-ol
                     (map
                      (lambda (invalid-split)
                        (gnc:html-markup-anchor
                         (gnc:split-anchor-text (cadr invalid-split))
                         (car invalid-split)))
                      invalid-splits)))))))))

           (else
            (let* ((account (car accounts))
                   (splits-acc-others (list-split splits split-from-acct? account)))

              (let lp ((acc-splits (car splits-acc-others))
                       (acc-totals (make-list (1+ num-buckets) 0))
                       (invalid-splits invalid-splits)
                       (owners-and-aging '()))

                (match acc-splits
                  (()
                   (loop (cdr accounts)
                         (cdr splits-acc-others)
                         (if (null? owners-and-aging)
                             accounts-and-owners
                             (cons (list account owners-and-aging acc-totals)
                                   accounts-and-owners))
                         invalid-splits))

                  ;; txn type != TXN_TYPE_INVOICE or TXN_TYPE_PAYMENT.
                  (((? split-is-not-business? this) . rest)
                   (let ((type (xaccTransGetTxnType (xaccSplitGetParent this))))
                     (lp rest
                         acc-totals
                         (cons (list (format #f (G_ "Invalid Txn Type ~a") type) this)
                               invalid-splits)
                         owners-and-aging)))

                  ;; some payment splits may have no owner in this
                  ;; account. skip. see bug 797506.
                  (((? split-owner-is-invalid? this) . rest)
                   (gnc:warn "split " this " has no owner")
                   (lp rest
                       acc-totals
                       (cons (list (G_ "Payment has no owner") this) invalid-splits)
                       owners-and-aging))

                  ((this . _)
                   (match-let* ((owner (gnc:split->owner this))
                                ((owner-splits . other-owner-splits)
                                 (list-split acc-splits split-has-owner? owner))
                                (aging (gnc:owner-splits->aging-list
                                        owner-splits num-buckets report-date
                                        date-type receivable))
                                (aging-total (apply + aging)))
                     (lp other-owner-splits
                         (map + acc-totals (reverse (cons aging-total aging)))
                         invalid-splits
                         (if (or show-zeros (any (negate zero?) aging))
                             (cons (list owner aging aging-total) owners-and-aging)
                             owners-and-aging)))))))))))))
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
       (vector 'billing (N_ "Billing address"))
       (vector 'shipping (N_ "Shipping address")))))
    options))

(define (payables-renderer report-obj)
  (aging-renderer report-obj #f))

(define (receivables-renderer report-obj)
  (aging-renderer report-obj #t))

(define payables-aging-guid "e57770f2dbca46619d6dac4ac5469b50")
(define receivables-aging-guid "9cf76bed17f14401b8e3e22d0079cb98")

(gnc:define-report
 'version 1
 'name (N_ "Payable Aging")
 'report-guid payables-aging-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator payable-options-generator
 'renderer payables-renderer
 'in-menu? #t)

(gnc:define-report
 'version 1
 'name (N_ "Receivable Aging")
 'report-guid "9cf76bed17f14401b8e3e22d0079cb98"
 'menu-path (list gnc:menuname-business-reports)
 'options-generator receivable-options-generator
 'renderer receivables-renderer
 'in-menu? #t)

(define (receivables-report-create-internal acct title show-zeros?)
  (let* ((options (gnc:make-report-options receivables-aging-guid))
         (zero-op (gnc:lookup-option options gnc:pagename-general optname-show-zeros))
         (title-op (gnc:lookup-option options gnc:pagename-general gnc:optname-reportname)))
    (when title (gnc:option-set-value title-op title))
    (gnc:option-set-value zero-op show-zeros?)
    (gnc:make-report receivables-aging-guid options)))

(define (payables-report-create-internal acct title show-zeros?)
  (let* ((options (gnc:make-report-options payables-aging-guid))
         (zero-op (gnc:lookup-option options gnc:pagename-general optname-show-zeros))
         (title-op (gnc:lookup-option options gnc:pagename-general gnc:optname-reportname)))
    (when title (gnc:option-set-value title-op title))
    (gnc:option-set-value zero-op show-zeros?)
    (gnc:make-report payables-aging-guid options)))

(define (gnc:receivables-create-internal
         account split query journal? double? title debit-string credit-string)
  (receivables-report-create-internal account #f #f))

(define (gnc:payables-create-internal
         account split query journal? double? title debit-string credit-string)
  (payables-report-create-internal account #f #f))

(gnc:register-report-hook ACCT-TYPE-RECEIVABLE #f gnc:receivables-create-internal)
(gnc:register-report-hook ACCT-TYPE-PAYABLE #f gnc:payables-create-internal)

(export payables-report-create-internal)
(export receivables-report-create-internal)
