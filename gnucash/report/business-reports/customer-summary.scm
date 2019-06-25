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

(define (query owner account-list start-date end-date)
  (let* ((q (qof-query-create-for-splits))
         (guid (gncOwnerReturnGUID (gncOwnerGetEndOwner owner))))
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
    (if addy (gnc:html-table-append-row!
              table (list (string-expand addy #\newline "<br/>"))))
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
  (apply + (map xaccSplitGetAmount
                (filter
                 (lambda (s)
                   (member (xaccSplitGetAccount s) accounts))
                 splits))))

(define (coll-minus minuend subtrahend)
  (let ((coll (gnc:make-commodity-collector)))
    (coll 'merge minuend #f)
    (coll 'minusmerge subtrahend #f)
    coll))

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
         (show-zero-lines? (opt-val gnc:pagename-display optname-show-zero-lines))
         (show-column-expense?
          (opt-val gnc:pagename-display optname-show-column-expense))
         (show-own-address? (opt-val gnc:pagename-display optname-show-own-address))
         (expense-accounts (opt-val pagename-expenseaccounts optname-expenseaccounts))
         (sales-accounts (opt-val pagename-incomeaccounts optname-incomeaccounts))
         (all-accounts (append sales-accounts expense-accounts))
         (commodities (delete-duplicates
                       (map xaccAccountGetCommodity all-accounts)
                       gnc-commodity-equiv))
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

    (for-each
     (lambda (acc)
       (toplevel-total-sales
        'add (xaccAccountGetCommodity acc)
        (- (xaccAccountGetBalanceAsOfDate acc start-date)
           (xaccAccountGetBalanceAsOfDate acc end-date))))
     sales-accounts)

    (for-each
     (lambda (acc)
       (toplevel-total-expense
        'add (xaccAccountGetCommodity acc)
        (- (xaccAccountGetBalanceAsOfDate acc end-date)
           (xaccAccountGetBalanceAsOfDate acc start-date))))
     expense-accounts)

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
        (string-append
         (_ "No valid customer selected.")
         " " (_ "Click on the \"Options\" button to select a company.")))))

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
                               (currency (gncOwnerGetCurrency owner))
                               (sales (- (filter-splits splits sales-accounts)))
                               (expense (filter-splits splits expense-accounts))
                               (profit (- sales expense)))
                          (list owner
                                (gnc:make-gnc-monetary currency profit)
                                (markup-percent profit sales)
                                (gnc:make-gnc-monetary currency sales)
                                (gnc:make-gnc-monetary currency expense))))
                      ownerlist)))

        (define (add-row owner markup profit sales expense)
          (gnc:html-table-append-row!
           table (cons owner
                       (map
                        (lambda (cell)
                          (gnc:make-html-table-cell/markup "number-cell" cell))
                        (cons* profit
                               (format #f "~a%" (round markup))
                               sales
                               (if show-column-expense?
                                   (list expense)
                                   '()))))))

        ;; Heading line
        (gnc:html-table-set-col-headers! table headings)

        (let* ((owner<? (lambda (a b)
                          ((if (eq? sort-order 'descend) string>? string<?)
                           (gncOwnerGetName (car a))
                           (gncOwnerGetName (car b)))))
               (op (if (eq? sort-order 'descend) > <))
               (<? (case sort-key
                     ((profit)  (lambda (a b) (op (gnc:gnc-monetary-amount (cadr a))
                                                  (gnc:gnc-monetary-amount (cadr b)))))
                     ((markup) (lambda (a b) (op (caddr a) (caddr b))))
                     ((sales) (lambda (a b) (op (gnc:gnc-monetary-amount (cadddr a))
                                                (gnc:gnc-monetary-amount (cadddr b)))))
                     ((expense) (lambda (a b) (op (gnc:gnc-monetary-amount (last a))
                                                  (gnc:gnc-monetary-amount (last b)))))
                     (else #f))))
          (set! results (sort results owner<?))
          (if <? (set! results (sort results <?))))

        ;; The actual content
        (for-each
         (lambda (row)
           (let* ((owner (car row))
                  (curr (gncOwnerGetCurrency owner))
                  (profit (cadr row))
                  (markupfloat (caddr row))
                  (sales (cadddr row))
                  (expense (last row)))
             (total-sales 'add curr (gnc:gnc-monetary-amount sales))
             (total-expense 'add curr (gnc:gnc-monetary-amount expense))
             (if (or show-zero-lines?
                     (not (and (zero? (gnc:gnc-monetary-amount profit))
                               (zero? (gnc:gnc-monetary-amount sales)))))
                 (add-row (gncOwnerGetName owner) markupfloat profit sales expense))))
         results)

        ;; The "No Customer" lines
        (let* ((other-sales (coll-minus toplevel-total-sales total-sales))
               (other-expense (coll-minus toplevel-total-expense total-expense))
               (other-profit (coll-minus other-sales  other-expense)))
          (for-each
           (lambda (comm)
             (let* ((profit (cadr (other-profit 'getpair comm #f)))
                    (sales (cadr (other-sales 'getpair comm #f)))
                    (expense (cadr (other-expense 'getpair comm #f)))
                    (markupfloat (markup-percent profit sales)))
               (unless (and (zero? profit) (zero? sales))
                 (add-row (string-append (_ "No Customer") " "
                                         (gnc-commodity-get-mnemonic comm))
                          markupfloat
                          (gnc:make-gnc-monetary comm profit)
                          (gnc:make-gnc-monetary comm sales)
                          (gnc:make-gnc-monetary comm expense)))))
           commodities))

        ;; One horizontal ruler before the summary
        (gnc:html-table-append-row!
         table (list
                (gnc:make-html-table-cell/size
                 1 (length headings)
                 (gnc:make-html-text (gnc:html-markup/attr/no-end "hr" "noshade")))))

        ;; Summary lines - 1 per currency
        (let ((total-profit (coll-minus toplevel-total-sales toplevel-total-expense)))
          (for-each
           (lambda (comm)
             (let* ((profit (cadr (total-profit 'getpair comm #f)))
                    (sales (cadr (toplevel-total-sales 'getpair comm #f)))
                    (expense (cadr (toplevel-total-expense 'getpair comm #f)))
                    (markupfloat (markup-percent profit sales)))
               (add-row (string-append (_ "Total") " "
                                       (gnc-commodity-get-mnemonic comm))
                        markupfloat
                        (gnc:make-gnc-monetary comm profit)
                        (gnc:make-gnc-monetary comm sales)
                        (gnc:make-gnc-monetary comm expense))))
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

