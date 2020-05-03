;; -*-scheme-*-
;; customer-summary.scm -- Print a summary of profit per customer
;;
;; Created by:  Christian Stimming
;; Copyright (c) 2010 Christian Stimming <christian@cstimming.de>
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

;; This report is based on the code in owner-report.scm, but it does
;; not only print a summary for one single owner (here: only
;; customers), but instead a table showing all customers.

(define-module (gnucash report customer-summary))

(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash utilities))                ; for gnc:debug
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

;; Option names
(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))

;; let's define a name for the report-guid's, much prettier
(define customer-report-guid "4166a20981985fd2b07ff8cb3b7d384e")
(define owner-report-guid "c146317be32e4948a561ec7fc89d15c1")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pagename-incomeaccounts (N_ "Income Accounts"))
(define optname-incomeaccounts (N_ "Income Accounts"))
(define opthelp-incomeaccounts
  (N_ "The income accounts where the sales and income was recorded."))

;; The line break in the next expressions will suppress above comment as translator comments.

(define pagename-expenseaccounts
  (N_ "Expense Accounts"))
(define optname-expenseaccounts (N_ "Expense Accounts"))

;; The line break in the next expressions will suppress above comment as translator comments.
(define opthelp-expenseaccounts
  (N_ "The expense accounts where the expenses are recorded which are subtracted from the sales to give the profit."))

(define optname-show-column-expense (N_ "Show Expense Column"))
(define opthelp-show-column-expense (N_ "Show the column with the expenses per customer."))
(define optname-show-own-address (N_ "Show Company Address"))
(define opthelp-show-own-address (N_ "Show your own company's address and the date of printing."))

(define pagename-columndisplay (N_ "Display Columns"))
(define date-header (N_ "Date"))
(define reference-header (N_ "Reference"))
(define type-header (N_ "Type"))
(define desc-header (N_ "Description"))
(define amount-header (N_ "Amount"))

;; The line break in the next expression will suppress above comments as translator comments.

(define optname-show-zero-lines
  (N_ "Show Lines with All Zeros"))
(define opthelp-show-zero-lines (N_ "Show the table lines with customers which did not have any transactions in the reporting period, hence would show all zeros in the columns."))
(define optname-show-inactive (N_ "Show Inactive Customers"))
(define opthelp-show-inactive (N_ "Include customers that have been marked inactive."))

(define optname-sortkey (N_ "Sort Column"))
(define opthelp-sortkey (N_ "Choose the column by which the result table is sorted."))
(define optname-sortascending (N_ "Sort Order"))
(define opthelp-sortascending (N_ "Choose the ordering of the column sort: Either ascending or descending."))


(define (options-generator)
  (define options (gnc:new-options))

  (define (add-option new-option)
    (gnc:register-option options new-option))

  (gnc:options-add-date-interval!
   options gnc:pagename-general optname-from-date optname-to-date "b")

  (add-option
   (gnc:make-account-list-option
    pagename-incomeaccounts optname-incomeaccounts
    "b" opthelp-incomeaccounts
    (lambda ()
      (gnc:filter-accountlist-type
       (list ACCT-TYPE-INCOME)
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
    #f #t))

  (add-option
   (gnc:make-account-list-option
    pagename-expenseaccounts optname-expenseaccounts
    "b" opthelp-expenseaccounts
    (lambda ()
      (gnc:filter-accountlist-type
       (list ACCT-TYPE-EXPENSE)
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
    #f #t))

  (add-option
   (gnc:make-multichoice-option
    gnc:pagename-display optname-sortkey
    "a" opthelp-sortkey
    'customername
    (list
     (vector 'customername
             (N_ "Customer Name")
             (N_ "Sort alphabetically by customer name."))
     (vector 'profit
             (N_ "Profit")
             (N_ "Sort by profit amount."))
     (vector 'markup
             ;; Translators: "Markup" is profit amount divided by sales amount
             (N_ "Markup")
             (N_ "Sort by markup (which is profit amount divided by sales)."))
     (vector 'sales
             (N_ "Sales")
             (N_ "Sort by sales amount."))
     (vector 'expense
             (N_ "Expense")
             (N_ "Sort by expense amount.")))))

  (add-option
   (gnc:make-multichoice-option
    gnc:pagename-display optname-sortascending
    "b" opthelp-sortascending
    'ascend
    (list
     (vector 'ascend
             (N_ "Ascending")
             (N_ "A to Z, smallest to largest."))
     (vector 'descend
             (N_ "Descending")
             (N_ "Z to A, largest to smallest.")))))

  (add-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display optname-show-own-address
    "d" opthelp-show-own-address #t))

  (add-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display optname-show-zero-lines
    "e" opthelp-show-zero-lines #f))

  (add-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display optname-show-inactive
    "f" opthelp-show-inactive #f))

  (add-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display optname-show-column-expense
    "g" opthelp-show-column-expense #t))

  (gnc:options-set-default-section options gnc:pagename-general)

  options)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (query owner account-list start-date end-date)
  (let* ((q (qof-query-create-for-splits))
         (guid (and owner
                    (gncOwnerReturnGUID (gncOwnerGetEndOwner owner)))))
    (when owner
      (qof-query-add-guid-match
       q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER OWNER-PARENTG)
       guid QOF-QUERY-OR)
      (qof-query-add-guid-match
       q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-BILLTO OWNER-PARENTG)
       guid QOF-QUERY-OR))
    ;; Apparently those query terms are unneeded because we never take
    ;; lots into account?!?
    ;; (qof-query-add-guid-match
    ;;  q (list SPLIT-LOT OWNER-FROM-LOT OWNER-PARENTG)
    ;;  guid QOF-QUERY-OR)
    ;; (qof-query-add-guid-match
    ;;  q (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-OWNER OWNER-PARENTG)
    ;;  guid QOF-QUERY-OR)
    ;; (qof-query-add-guid-match
    ;;  q (list SPLIT-LOT INVOICE-FROM-LOT INVOICE-BILLTO OWNER-PARENTG)
    ;;  guid QOF-QUERY-OR)
    (xaccQueryAddAccountMatch q account-list QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT q #t start-date #t end-date QOF-QUERY-AND)
    (xaccQueryAddClosingTransMatch q #f QOF-QUERY-AND)
    (qof-query-set-book q (gnc-get-current-book))
    (let ((result (qof-query-run q)))
      (qof-query-destroy q)
      result)))

(define (make-myname-table book date-format)
  (let* ((table (gnc:make-html-table))
         (name (gnc:company-info book gnc:*company-name*))
         (addy (gnc:company-info book gnc:*company-addy*)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "width" "") ;; this way we force the override of the "100%" below
     'attribute (list "align" "right")
     'attribute (list "valign" "top")
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (if name (gnc:html-table-append-row! table (list name)))
    (if addy (gnc:html-table-append-row! table (gnc:multiline-to-html-text addy)))
    (gnc:html-table-append-row!
     table (list (gnc-print-time64 (gnc:get-today) date-format)))
    (let ((table-outer (gnc:make-html-table)))
      (gnc:html-table-set-style!
       table-outer "table"
       'attribute (list "border" 0)
       'attribute (list "width" "100%")
       'attribute (list "valign" "top")
       'attribute (list "cellspacing" 0)
       'attribute (list "cellpadding" 0))
      (gnc:html-table-append-row! table-outer (list table))
      table-outer)))

(define (markup-percent profit sales)
  (if (zero? sales) 0
      (* 100 (/ profit sales))))

(define (filter-splits splits accounts)
  (apply gnc:monetaries-add
         (map (lambda (s)
                (gnc:make-gnc-monetary
                 (xaccTransGetCurrency (xaccSplitGetParent s))
                 (xaccSplitGetValue s)))
              (filter
               (lambda (s)
                 (member (xaccSplitGetAccount s) accounts))
               splits))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let* ((document (gnc:make-html-document))
         (report-title (opt-val gnc:pagename-general gnc:optname-reportname))
         (start-date (gnc:time64-start-day-time
                      (gnc:date-option-absolute-time
                       (opt-val gnc:pagename-general optname-from-date))))
         (end-date (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (opt-val gnc:pagename-general optname-to-date))))
         (sort-order (opt-val gnc:pagename-display optname-sortascending))
         (sort-key (opt-val gnc:pagename-display optname-sortkey))
         (show-zero-lines? (opt-val gnc:pagename-display optname-show-zero-lines))
         (show-column-expense?
          (opt-val gnc:pagename-display optname-show-column-expense))
         (show-own-address? (opt-val gnc:pagename-display optname-show-own-address))
         (expense-accounts (opt-val pagename-expenseaccounts optname-expenseaccounts))
         (sales-accounts (opt-val pagename-incomeaccounts optname-incomeaccounts))
         (all-accounts (append sales-accounts expense-accounts))
         (commodities (gnc:accounts-get-commodities all-accounts #f))
         (commodities>1? (> (length commodities) 1))
         (book (gnc-get-current-book))
         (date-format (gnc:options-fancy-date book))
         (ownerlist (gncBusinessGetOwnerList
                     book
                     (gncOwnerTypeToQofIdType GNC-OWNER-CUSTOMER)
                     (opt-val gnc:pagename-display optname-show-inactive)))
         (toplevel-total-sales (gnc:make-commodity-collector))
         (toplevel-total-expense (gnc:make-commodity-collector))
         (type-str (N_ "Customer")))

    (gnc:html-document-set-title!
     document (string-append (_ type-str) " " (_ "Report")))

    (gnc:html-document-set-title!
     document (format #f (_ "~a ~a - ~a")
                      report-title
                      (qof-print-date start-date)
                      (qof-print-date end-date)))

    (when show-own-address?
      (gnc:html-document-add-object!
       document (make-myname-table book date-format)))

    (cond
     ((null? sales-accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((null? ownerlist)
      (gnc:html-document-add-object!
       document
       (gnc:make-html-text
        (_ "No valid customer found."))))

     (else
      (let ((all-splits (query #f all-accounts start-date end-date))
            (table (gnc:make-html-table))
            (total-sales (gnc:make-commodity-collector))
            (total-expense (gnc:make-commodity-collector))
            (headings (cons* (_ "Customer")
                             (_ "Profit")
                             (_ "Markup")
                             (_ "Sales")
                             (if show-column-expense?
                                 (list (_ "Expense"))
                                 '())))
            (results (map
                      (lambda (owner)
                        (let* ((splits (query owner all-accounts start-date end-date))
                               (sales (gnc:commodity-collector-get-negated
                                       (filter-splits splits sales-accounts)))
                               (expense (filter-splits splits expense-accounts))
                               (profit (gnc:collector- sales expense)))
                          (list owner profit sales expense)))
                      ownerlist))
            (sortingtable '()))

        (define (add-row str curr markup profit sales expense url)
          (gnc:html-table-append-row!
           table (cons* (if url
                            (gnc:make-html-text (gnc:html-markup-anchor url str))
                            str)
                        (map
                         (lambda (cell)
                           (gnc:make-html-table-cell/markup "number-cell" cell))
                         (cons* profit
                                (and markup (format #f "~a%" (round markup)))
                                sales
                                (if show-column-expense?
                                    (list expense)
                                    '()))))))

        (let ((sales (gnc:commodity-collector-get-negated
                      (filter-splits all-splits sales-accounts)))
              (expense (filter-splits all-splits expense-accounts)))
          (toplevel-total-sales 'merge sales #f)
          (toplevel-total-expense 'merge expense #f))

        ;; The actual content - add onto sortingtable
        (for-each
         (lambda (row)
           (let* ((owner (car row))
                  (profit (cadr row))
                  (sales (caddr row))
                  (expense (cadddr row)))
             (total-sales 'merge sales #f)
             (total-expense 'merge expense #f)
             (for-each
              (lambda (comm)
                (let* ((comm-profit (cadr (profit 'getpair comm #f)))
                       (comm-sales (cadr (sales 'getpair comm #f)))
                       (comm-expense (cadr (expense 'getpair comm #f)))
                       (markup (markup-percent comm-profit comm-sales)))
                  (when (or show-zero-lines?
                            (not (and (zero? comm-profit) (zero? comm-sales))))
                    (set! sortingtable
                      (cons (vector
                             (gncOwnerGetName owner) comm markup
                             comm-profit comm-sales comm-expense
                             (gnc:report-anchor-text
                              (gnc:owner-report-create owner '() #:currency comm)))
                            sortingtable)))))
              commodities)))
         results)

        ;; Add the "No Customer" lines to the sortingtable for sorting
        ;; as well
        (let* ((other-sales (gnc:collector- toplevel-total-sales total-sales))
               (other-expense (gnc:collector- toplevel-total-expense
                                                  total-expense))
               (other-profit (gnc:collector- other-sales other-expense)))
          (for-each
           (lambda (comm)
             (let* ((profit (cadr (other-profit 'getpair comm #f)))
                    (sales (cadr (other-sales 'getpair comm #f)))
                    (expense (cadr (other-expense 'getpair comm #f)))
                    (markup (markup-percent profit sales)))
               (unless (and (zero? profit) (zero? sales))
                 (set! sortingtable
                   (cons (vector
                          (_ "No Customer") comm markup profit sales expense #f)
                         sortingtable)))))
           commodities))

        ;; Stable-sort the sortingtable according to column, then
        ;; stable-sort according to currency. This results in group-by
        ;; currency then sort by columns.
        (let* ((str-op (if (eq? sort-order 'descend) string>? string<?))
               (op (if (eq? sort-order 'descend) > <)))
          (define (<? key)
            (case key
              ;; customername sorting is handled differently; this
              ;; conditional ensures "No Customer" entries,
              ;; i.e. without owner-report url, are printed last.
              ((customername)
               (lambda (a b)
                 (cond
                  ((not (vector-ref b 6)) #t)
                  ((not (vector-ref a 6)) #f)
                  (else (str-op (vector-ref a 0) (vector-ref b 0))))))
              ;; currency sorting always alphabetical a-z
              ((currency)
               (lambda (a b) (string<?
                              (gnc-commodity-get-mnemonic (vector-ref a 1))
                              (gnc-commodity-get-mnemonic (vector-ref b 1)))))
              ((markup)
               (lambda (a b) (op (vector-ref a 2) (vector-ref b 2))))
              ((profit)
               (lambda (a b) (op (vector-ref a 3) (vector-ref b 3))))
              ((sales)
               (lambda (a b) (op (vector-ref a 4) (vector-ref b 4))))
              ((expense)
               (lambda (a b) (op (vector-ref a 5) (vector-ref b 5))))))
          (set! sortingtable (stable-sort! sortingtable (<? sort-key)))
          (when (memq sort-key '(profit sales expense))
            (set! sortingtable (stable-sort! sortingtable (<? 'currency)))))

        ;; After sorting, add the entries to the resultant table
        (let lp ((sortingtable sortingtable)
                 (last-comm #f))
          (unless (null? sortingtable)
            (let* ((elt (car sortingtable))
                   (comm (vector-ref elt 1)))
              (when (and commodities>1?
                         (memq sort-key '(profit sales expense))
                         (not (and last-comm (gnc-commodity-equiv last-comm comm))))
                (add-row (gnc-commodity-get-mnemonic comm) #f #f #f #f #f #f))
              (add-row (vector-ref elt 0)
                       comm
                       (vector-ref elt 2)
                       (gnc:make-gnc-monetary comm (vector-ref elt 3))
                       (gnc:make-gnc-monetary comm (vector-ref elt 4))
                       (gnc:make-gnc-monetary comm (vector-ref elt 5))
                       (vector-ref elt 6))
              (lp (cdr sortingtable) comm))))

        ;; One horizontal ruler before the summary
        (gnc:html-table-append-row!
         table (list
                (gnc:make-html-table-cell/size
                 1 (length headings)
                 (gnc:make-html-text (gnc:html-markup/attr/no-end "hr" "noshade")))))

        ;; Summary lines - 1 per currency
        (let ((total-profit (gnc:collector- toplevel-total-sales
                                            toplevel-total-expense)))
          (for-each
           (lambda (comm)
             (let* ((profit (cadr (total-profit 'getpair comm #f)))
                    (sales (cadr (toplevel-total-sales 'getpair comm #f)))
                    (expense (cadr (toplevel-total-expense 'getpair comm #f)))
                    (markup (markup-percent profit sales)))
               (add-row (if commodities>1?
                            (format #f "~a (~a)"
                                    (_ "Total")
                                    (gnc-commodity-get-mnemonic comm))
                            (_ "Total"))
                        comm markup
                        (gnc:make-gnc-monetary comm profit)
                        (gnc:make-gnc-monetary comm sales)
                        (gnc:make-gnc-monetary comm expense)
                        #f)))
           commodities))

        ;; Heading line
        (gnc:html-table-set-col-headers! table headings)

        ;; Set the formatting styles
        (gnc:html-table-set-style!
         table "td"
         'attribute '("align" "right")
         'attribute '("valign" "top"))

        (gnc:html-table-set-col-style!
         table 0 "td"
         'attribute '("align" "left"))

        (gnc:html-table-set-style!
         table "table"
         ;;'attribute (list "border" 1)
         'attribute (list "cellspacing" 2)
         'attribute (list "cellpadding" 4))

        ;; And add the table to the document
        (gnc:html-document-add-object! document table))))

    document))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:define-report
 'version 1
 'name (N_ "Customer Summary")
 'report-guid customer-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #t)

