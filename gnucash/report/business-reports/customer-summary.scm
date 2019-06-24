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


(define (options-generator acct-type-list owner-type inv-str reverse?)

  (define options (gnc:new-options))

  (define (add-option new-option)
    (gnc:register-option options new-option))

  (add-option
   (gnc:make-internal-option "__reg" "inv-str" inv-str))

  (add-option
   (gnc:make-simple-boolean-option "__reg" "reverse?" "" "" reverse?))

  (add-option
   (gnc:make-internal-option "__reg" "owner-type" owner-type))

  (gnc:options-add-date-interval!
   options
   gnc:pagename-general optname-from-date optname-to-date
   "b")

  (add-option
   (gnc:make-account-list-option
    pagename-incomeaccounts optname-incomeaccounts
    "b"
    opthelp-incomeaccounts
    ;; This default-getter finds the first account of this type. TODO:
    ;; Find not only the first one, but all of them!
    (lambda ()
      (gnc:filter-accountlist-type
       (list ACCT-TYPE-INCOME)
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
    #f #t))

  (add-option
   (gnc:make-account-list-option
    pagename-expenseaccounts optname-expenseaccounts
    "b"
    opthelp-expenseaccounts
    ;; This default-getter finds the first account of this type. TODO:
    ;; Find not only the first one, but all of them!
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

(define (customer-options-generator)
  (options-generator (list ACCT-TYPE-RECEIVABLE) GNC-OWNER-CUSTOMER
                     (_ "Invoice") #t)) ;; FIXME: reverse?=#t but originally #f


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (query-setup q owner account-list start-date end-date)
  (let* ((guid (gncOwnerReturnGUID (gncOwnerGetEndOwner owner))))
    (qof-query-add-guid-match
     q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-OWNER OWNER-PARENTG)
     guid QOF-QUERY-OR)
    (qof-query-add-guid-match
     q (list SPLIT-TRANS INVOICE-FROM-TXN INVOICE-BILLTO OWNER-PARENTG)
     guid QOF-QUERY-OR)
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
    (qof-query-set-book q (gnc-get-current-book))
    q))

(define (make-myname-table book date-format)
  (let* ((table (gnc:make-html-table))
         (table-outer (gnc:make-html-table))
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

    (gnc:html-table-append-row! table (list (if name name "")))
    (gnc:html-table-append-row! table (list (string-expand
                                             (if addy addy "")
                                             #\newline "<br/>")))
    (gnc:html-table-append-row!
     table (list (gnc-print-time64 (gnc:get-today) date-format)))

    (gnc:html-table-set-style!
     table-outer "table"
     'attribute (list "border" 0)
     'attribute (list "width" "100%")
     'attribute (list "valign" "top")
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))

    (gnc:html-table-append-row! table-outer (list table))

    table-outer))

(define (make-break! document)
  (gnc:html-document-add-object!
   document
   (gnc:make-html-text
    (gnc:html-markup-br))))

(define (markup-percent profit sales)
  (if (zero? sales) 0
      (* 100 (/ profit sales))))

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
         (show-zero-lines? (opt-val gnc:pagename-display optname-show-zero-lines))
         (show-column-expense? (opt-val gnc:pagename-display optname-show-column-expense))
         (table-num-columns (if show-column-expense? 5 4))
         (show-own-address? (opt-val gnc:pagename-display optname-show-own-address))
         (expense-accounts (opt-val pagename-expenseaccounts optname-expenseaccounts))
         (income-accounts (opt-val pagename-incomeaccounts optname-incomeaccounts))
         (all-accounts (append income-accounts expense-accounts))
         (commodities (delete-duplicates
                       (map xaccAccountGetCommodity all-accounts)
                       gnc-commodity-equiv))
         (book (gnc-get-current-book))
         (date-format (gnc:options-fancy-date book))
         (type (opt-val "__reg" "owner-type"))
         (reverse? (opt-val "__reg" "reverse?"))
         (ownerlist (gncBusinessGetOwnerList
                     book
                     (gncOwnerTypeToQofIdType type)
                     (opt-val gnc:pagename-display optname-show-inactive)))
         (toplevel-total-income (gnc:make-commodity-collector))
         (toplevel-total-expense (gnc:make-commodity-collector))
         (any-valid-owner? #f)
         (type-str (cond
                    ((eqv? type GNC-OWNER-CUSTOMER) (N_ "Customer"))
                    ((eqv? type GNC-OWNER-VENDOR)   (N_ "Vendor"))
                    ((eqv? type GNC-OWNER-EMPLOYEE) (N_ "Employee"))
                    (else "")))
         (currency (gnc-default-currency)))

    (gnc:html-document-set-title!
     document (string-append (_ type-str) " " (_ "Report")))

    (for-each
     (lambda (acc)
       (toplevel-total-income
        'add (xaccAccountGetCommodity acc)
        ((if reverse? - identity)
         (- (xaccAccountGetBalanceAsOfDate acc end-date)
            (xaccAccountGetBalanceAsOfDate acc start-date)))))
     income-accounts)

    (for-each
     (lambda (acc)
       (toplevel-total-expense
        'add (xaccAccountGetCommodity acc)
        ((if reverse? - identity)
         (- (xaccAccountGetBalanceAsOfDate acc end-date)
            (xaccAccountGetBalanceAsOfDate acc start-date)))))
     expense-accounts)

    ;; Continue if we have non-null accounts
    (if (null? income-accounts)

        ;; error condition: no accounts specified
        ;; is this *really* necessary??  i'd be fine with an all-zero
        ;; account summary that would, technically, be correct....
        (gnc:html-document-add-object!
         document
         (gnc:html-make-no-account-warning
          report-title (gnc:report-id report-obj)))

        ;; otherwise, generate the report...

        (let ((resulttable
               ;; Loop over all owners
               (map
                (lambda (owner)
                  ;; Now create the line for one single owner
                  (let ((total-income 0)
                        (total-expense 0)
                        (owner-query (qof-query-create-for-splits))
                        (currency (gncOwnerGetCurrency owner)))
                        ;; Run one query on all accounts
                    (query-setup owner-query owner all-accounts start-date end-date)
                    (set! any-valid-owner? #t)

                    (let ((splits (qof-query-run owner-query)))

                      (set! total-income
                        (gnc:make-gnc-monetary
                         currency
                         ((if reverse? - identity)
                          (apply + (map xaccSplitGetValue
                                        (filter
                                         (lambda (s)
                                           (member (xaccSplitGetAccount s)
                                                   income-accounts))
                                         splits))))))

                      (set! total-expense
                        (gnc:make-gnc-monetary
                         currency
                         ((if reverse? - identity)
                          (apply + (map xaccSplitGetValue
                                        (filter
                                         (lambda (s)
                                           (member (xaccSplitGetAccount s)
                                                   expense-accounts))
                                         splits)))))))

                    ;; Clean up the query
                    (qof-query-destroy owner-query)

                    ;; We print the summary now
                    (let* ((profit (gnc:monetary+ total-income total-expense))
                           (markupfloat (markup-percent
                                         (gnc:gnc-monetary-amount profit)
                                         (gnc:gnc-monetary-amount total-income))))

                      ;; Result of this customer
                      (list owner profit markupfloat total-income total-expense))))
                ownerlist)))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;; If asked for, we also print the company name
          (if show-own-address?
              (gnc:html-document-add-object!
               document
               (make-myname-table book date-format)))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;; Now print the resulttable here:
          (let ((table (gnc:make-html-table))
                (sort-descending? (eq? (opt-val gnc:pagename-display optname-sortascending) 'descend))
                (sort-key (opt-val gnc:pagename-display optname-sortkey))
                (total-sales (gnc:make-commodity-collector))
                (total-expense (gnc:make-commodity-collector))
                (heading-list
                 (list (_ "Customer")
                       (_ "Profit")
                       ;; Translators: "Markup" is profit amount divided by sales amount
                       (_ "Markup")
                       (_ "Sales"))))

            ;; helper for sorting an owner list
            (define (owner-name<? a b)
              (string<? (gncOwnerGetName a) (gncOwnerGetName b)))

            ;; Heading line
            (if show-column-expense?
                (set! heading-list (append heading-list (list (_ "Expense")))))
            (gnc:html-table-set-col-headers! table heading-list)

            ;; Sorting: First sort everything alphabetically
            ;; (ascending) so that we have one stable sorting order
            (set! resulttable
              (sort resulttable
                    (lambda (a b)
                      (owner-name<? (car a) (car b)))))

            ;; Secondly sort by the actual sort key
            (let ((cmp (if sort-descending? > <))
                  (strcmp (if sort-descending? string>? string<?)))
              (set! resulttable
                (sort resulttable
                      (cond
                       ((eq? sort-key 'customername)
                        (lambda (a b)
                          (strcmp (gncOwnerGetName (car a)) (gncOwnerGetName (car b)))))
                       ((eq? sort-key 'profit)
                        (lambda (a b)
                          (cmp (gnc-numeric-compare (cadr a) (cadr b)) 0)))
                       ((eq? sort-key 'markup)
                        (lambda (a b)
                          (cmp (list-ref a 2) (list-ref b 2))))
                       ((eq? sort-key 'sales)
                        (lambda (a b)
                          (cmp (gnc-numeric-compare (list-ref a 3) (list-ref b 3)) 0)))
                       ((eq? sort-key 'expense)
                        (lambda (a b)
                          (cmp (gnc-numeric-compare (list-ref a 4) (list-ref b 4)) 0)))))))

            ;; The actual content
            (for-each
             (lambda (row)
               (if (eq? (length row) 5)
                   (let ((owner (list-ref row 0))
                         (profit (list-ref row 1))
                         (markupfloat (list-ref row 2))
                         (sales (list-ref row 3))
                         (expense (list-ref row 4)))

                     (total-sales 'add (gnc:gnc-monetary-commodity sales)
                                  (gnc:gnc-monetary-amount sales))
                     (total-expense 'add (gnc:gnc-monetary-commodity expense)
                                    (gnc:gnc-monetary-amount expense))
                     (if (or show-zero-lines?
                             (not (and (zero? (gnc:gnc-monetary-amount profit))
                                       (zero? (gnc:gnc-monetary-amount sales)))))
                         (gnc:html-table-append-row!
                          table (append
                                 (list (gncOwnerGetName owner)
                                       profit
                                       (format #f  "~2,0f%" markupfloat)
                                       sales)
                                 (if show-column-expense?
                                     (list expense) '())))))
                   (gnc:warn "Oops, encountered a row with wrong length=" (length row))))
             resulttable) ;; END for-each row

            ;; The "No Customer" lines
            (let* ((other-sales (let ((coll (gnc:make-commodity-collector)))
                                  (coll 'merge toplevel-total-income #f)
                                  (coll 'minusmerge total-sales #f)
                                  coll))
                   (other-expense (let ((coll (gnc:make-commodity-collector)))
                                    (coll 'merge toplevel-total-expense #f)
                                    (coll 'minusmerge total-expense #f)
                                    coll))
                   (other-profit (let ((coll (gnc:make-commodity-collector)))
                                    (coll 'merge other-sales #f)
                                    (coll 'merge other-expense #f)
                                    coll)))
              (for-each
               (lambda (comm)
                 (let* ((profit (cadr (other-profit 'getpair comm #f)))
                        (sales (cadr (other-sales 'getpair comm #f)))
                        (expense (cadr (other-expense 'getpair comm #f)))
                        (markupfloat (markup-percent profit sales)))
                   (if (or show-zero-lines?
                           (not (and (zero? profit) (zero? sales))))
                       (gnc:html-table-append-row!
                        table
                        (append
                         (list (string-append (_ "No Customer")
                                              " "
                                              (gnc-commodity-get-mnemonic comm))
                               (gnc:make-gnc-monetary comm profit)
                               (format #f  "~2,0f%" markupfloat)
                               (gnc:make-gnc-monetary comm sales))
                         (if show-column-expense?
                             (list (gnc:make-gnc-monetary comm (- expense)))
                             '()))))))
               commodities))

            ;; One horizontal ruler before the summary
            ;;;(gnc:html-table-append-ruler!
            ;;; table table-num-columns) ;; better use the "noshade" attribute:
            (gnc:html-table-append-row!
             table
             (list
              (gnc:make-html-table-cell/size
               1 table-num-columns
               (gnc:make-html-text (gnc:html-markup/attr/no-end "hr" "noshade")))))

            ;; One summary line
            (let* ((total-profit (let ((coll (gnc:make-commodity-collector)))
                                   (coll 'merge toplevel-total-income #f)
                                   (coll 'merge toplevel-total-expense #f)
                                   coll)))
              (for-each
               (lambda (comm)
                 (let* ((profit (cadr (total-profit 'getpair comm #f)))
                        (sales (cadr (toplevel-total-income 'getpair comm #f)))
                        (expense (cadr (toplevel-total-expense 'getpair comm #f)))
                        (markupfloat (markup-percent profit sales)))
                   (gnc:html-table-append-row!
                    table
                    (append (list (string-append (_ "Total") " "
                                                 (gnc-commodity-get-mnemonic comm))
                                  (gnc:make-gnc-monetary comm profit)
                                  (format #f  "~2,0f%" markupfloat)
                                  (gnc:make-gnc-monetary comm sales))
                            (if show-column-expense?
                                (list (gnc:make-gnc-monetary currency (- expense)))
                                '())))))
               commodities))

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
            (gnc:html-document-add-object!
             document table))))

    (if any-valid-owner?
        ;; Report contains valid data
        (let ((headline (format #f (_ "~a ~a - ~a")
                                report-title
                                (qof-print-date start-date)
                                (qof-print-date end-date))))
          (gnc:html-document-set-title! document headline))

        ;; else....
        (gnc:html-document-add-object!
         document
         (gnc:make-html-text
          (string-append
           (cond
            ((eqv? type GNC-OWNER-CUSTOMER)
             (_ "No valid customer selected."))
            ((eqv? type GNC-OWNER-VENDOR)
             (_ "No valid vendor selected."))
            ((eqv? type GNC-OWNER-EMPLOYEE)
             (_ "No valid employee selected.")))
           " "
           (_ "Click on the \"Options\" button to select a company.")))))

    document))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:define-report
 'version 1
 'name (N_ "Customer Summary")
 'report-guid customer-report-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator customer-options-generator
 'renderer reg-renderer
 'in-menu? #t)

