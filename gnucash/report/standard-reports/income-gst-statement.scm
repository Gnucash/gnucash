;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Income-GST-Statement.scm : Produce report suitable for
;; annual income tax returns and periodic VAT/GST reporting.
;;
;; Original transaction.scm report by Robert Merkel <rgmerk@mira.net>
;; Contributions by Bryan Larsen <blarsen@ada-works.com>
;; More contributions for new report generation code by Robert Merkel
;; More contributions by Christian Stimming <stimming@tuhh.de>
;; Modified to support the intersection of two account lists by
;; Michael T. Garrison Stuber
;; Modified account names display by Tomas Pospisek
;; <tpo_deb@sourcepole.ch> with a lot of help from "warlord"
;; Heavily amended by Christopher Lam to add calculations
;; appropriate for GST/VAT, building on efforts by Doug Doughty.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports income-gst-statement))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash printf))
(gnc:module-load "gnucash/report/report-system" 0)

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

;; Define the strings here to avoid typos and make changes easier.

(define reportname (N_ "Income & GST Statement"))
(define pagename-sorting (N_ "Sorting"))
(define optname-prime-sortkey (N_ "Primary Key"))
(define optname-prime-subtotal (N_ "Primary Subtotal"))
(define optname-prime-sortorder (N_ "Primary Sort Order"))
(define optname-prime-date-subtotal (N_ "Primary Subtotal for Date Key"))
(define optname-full-account-name (N_ "Show Full Account Name"))
(define optname-show-account-code (N_ "Show Account Code"))
(define optname-sec-sortkey (N_ "Secondary Key"))
(define optname-sec-subtotal (N_ "Secondary Subtotal"))
(define optname-sec-sortorder  (N_ "Secondary Sort Order"))
(define optname-sec-date-subtotal (N_ "Secondary Subtotal for Date Key"))
(define optname-void-transactions (N_ "Void Transactions"))
(define optname-table-export (N_ "Table for Exporting"))
(define optname-common-currency (N_ "Common Currency"))
(define TAX-SETUP-DESC "From the Report Options, you will need to select the accounts which will \
hold the GST/VAT taxes collected or paid. These accounts must contain splits which document the \
monies which are wholly sent or claimed from tax authorities during periodic GST/VAT returns. These \
accounts must be of type ASSET for taxes paid on expenses, and type LIABILITY for taxes collected on sales.")
(define optname-currency (N_ "Report's currency"))
(define def:grand-total-style "grand-total")
(define def:normal-row-style "normal-row")
(define def:alternate-row-style "alternate-row")
(define def:primary-subtotal-style "primary-subheading")
(define def:secondary-subtotal-style "secondary-subheading")
;; The option-values of the sorting key multichoice option, for
;; which a subtotal should be enabled.
(define subtotal-enabled '(account-name
                           account-code
                           corresponding-acc-name
                           corresponding-acc-code))

(define (split-account-full-name-same-p a b)
  (= (xaccSplitCompareAccountFullNames a b) 0))

(define (split-account-code-same-p a b)
  (= (xaccSplitCompareAccountCodes a b) 0))

(define (split-same-corr-account-full-name-p a b)
  (= (xaccSplitCompareOtherAccountFullNames a b) 0))

(define (split-same-corr-account-code-p a b)
  (= (xaccSplitCompareOtherAccountCodes a b) 0))

(define (timepair-same-year tp-a tp-b)
  (= (gnc:timepair-get-year tp-a)
     (gnc:timepair-get-year tp-b)))

(define (timepair-same-quarter tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-quarter tp-a)
          (gnc:timepair-get-quarter tp-b))))

(define (timepair-same-month tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-month tp-a)
          (gnc:timepair-get-month tp-b))))

(define (timepair-same-week tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-week tp-a)
          (gnc:timepair-get-week tp-b))))

(define (split-same-week-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-week tp-a tp-b)))

(define (split-same-month-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-month tp-a tp-b)))

(define (split-same-quarter-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-quarter tp-a tp-b)))

(define (split-same-year-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-year tp-a tp-b)))

(define (set-last-row-style! table tag . rest)
  (let ((arg-list
         (cons table
               (cons (- (gnc:html-table-num-rows table) 1)
                     (cons tag rest)))))
    (apply gnc:html-table-set-row-style! arg-list)))

(define (add-subheading-row data table width subheading-style)
  (let ((heading-cell (gnc:make-html-table-cell data)))
    (gnc:html-table-cell-set-colspan! heading-cell width)
    (gnc:html-table-append-row/markup!
     table
     subheading-style
     (list heading-cell))))

;; display an account name depending on the options the user has set
(define (account-namestring account show-account-code show-account-name show-account-full-name)
  ;;# on multi-line splits we can get an empty ('()) account
  (if (null? account)
      (_ "Split Transaction")
      (string-append
       ;; display account code?
       (if show-account-code
           (string-append (xaccAccountGetCode account) " ")
           "")
       ;; display account name?
       (if show-account-name
           ;; display full account name?
           (if show-account-full-name
               (gnc-account-get-full-name account)
               (xaccAccountGetName account))
           ""))))

;; render an account subheading - column-vector determines what is displayed
(define (render-account-subheading
         split table width subheading-style column-vector)
  (let ((account (xaccSplitGetAccount split)))
    (add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                          (gnc:account-anchor-text account)
                          (account-namestring account
                                              (used-sort-account-code      column-vector)
                                              #t
                                              (used-sort-account-full-name column-vector))))
                        table width subheading-style)))

(define (render-corresponding-account-subheading
         split table width subheading-style column-vector)
  (let ((account (xaccSplitGetAccount (xaccSplitGetOtherSplit split))))
    (add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                          (if (not (null? account))
                              (gnc:account-anchor-text account)
                              "")
                          (account-namestring account
                                              (used-sort-account-code      column-vector)
                                              #t
                                              (used-sort-account-full-name column-vector))))
                        table width subheading-style)))

(define (render-week-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-week-year-string
                       (gnc:timepair->date
                        (gnc-transaction-get-date-posted
                         (xaccSplitGetParent split))))
                      table width subheading-style))

(define (render-month-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-month-year-string
                       (gnc:timepair->date
                        (gnc-transaction-get-date-posted
                         (xaccSplitGetParent split))))
                      table width subheading-style))

(define (render-quarter-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-quarter-year-string
                       (gnc:timepair->date
                        (gnc-transaction-get-date-posted
                         (xaccSplitGetParent split))))
                      table width subheading-style))

(define (render-year-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-year-string
                       (gnc:timepair->date
                        (gnc-transaction-get-date-posted
                         (xaccSplitGetParent split))))
                      table width subheading-style))


(define (add-subtotal-row table width subtotal-string subtotal-collectors
                          subtotal-style export?)
  (let* ((row-contents '())
         (columns (map (lambda (coll) (coll 'format gnc:make-gnc-monetary #f)) subtotal-collectors))
         (list-of-commodities (delete-duplicates (map gnc:gnc-monetary-commodity (apply append columns)))))

    (define (retrieve-commodity list-of-monetary commodity)
      (if (null? list-of-monetary)
          #f
          (if (gnc-commodity-equal (gnc:gnc-monetary-commodity (car list-of-monetary)) commodity)
              (car list-of-monetary)
              (retrieve-commodity (cdr list-of-monetary) commodity))))

    (define (add-first-column string)
      (if export?
          (begin
            (addto! row-contents (gnc:make-html-table-cell/markup "total-label-cell" string))
            (for-each (lambda (cell) (addto! row-contents cell))
                      (gnc:html-make-empty-cells (- width 1))))
          (addto! row-contents (gnc:make-html-table-cell/size/markup 1 width "total-label-cell" string))))

    (define (add-columns commodity)
      (for-each (lambda (column)
                  (addto! row-contents
                          (gnc:make-html-table-cell/markup
                           "total-number-cell"
                           (retrieve-commodity column commodity))))
                columns))

    ;first row
    (add-first-column subtotal-string)
    (add-columns (if (pair? list-of-commodities)
                     (car list-of-commodities)
                     #f)) ;to account for empty-row subtotals
    (gnc:html-table-append-row/markup! table subtotal-style (reverse row-contents))

    ;subsequent rows
    (if (pair? list-of-commodities)
        (for-each (lambda (commodity)
                    (set! row-contents '())
                    (add-first-column "")
                    (add-columns commodity)
                    (gnc:html-table-append-row/markup! table subtotal-style (reverse row-contents)))
                  (cdr list-of-commodities)))))

(define (total-string str) (string-append (_ "Total For ") str))

(define (render-account-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (add-subtotal-row table width
                    (total-string (account-namestring (xaccSplitGetAccount split)
                                                      (used-sort-account-code      column-vector)
                                                      #t
                                                      (used-sort-account-full-name column-vector)))
                    total-collector subtotal-style export?))

(define (render-corresponding-account-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (add-subtotal-row table width
                    (total-string (account-namestring (xaccSplitGetAccount
                                                       (xaccSplitGetOtherSplit split))
                                                      (used-sort-account-code      column-vector)
                                                      #t
                                                      (used-sort-account-full-name column-vector)))
                    total-collector subtotal-style export?))

(define (render-week-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (gnc:date-get-week-year-string tm))
                      total-collector subtotal-style export?)))

(define (render-month-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (gnc:date-get-month-year-string tm))
                      total-collector subtotal-style export?)))


(define (render-quarter-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (gnc:date-get-quarter-year-string tm))
                      total-collector subtotal-style export?)))

(define (render-year-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (strftime "%Y" tm))
                      total-collector subtotal-style export?)))


(define (render-grand-total
         table width total-collector export?)
  (add-subtotal-row table width
                    (_ "Grand Total")
                    total-collector def:grand-total-style export?))

(define account-types-to-reverse-assoc-list
  (list (cons 'none '())
        (cons 'income-expense
              (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE))
        (cons 'credit-accounts
              (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-EQUITY
                    ACCT-TYPE-CREDIT ACCT-TYPE-INCOME))))

(define (used-date columns-used)
  (vector-ref columns-used 0))
(define (used-reconciled-date columns-used)
  (vector-ref columns-used 1))
(define (used-num columns-used)
  (vector-ref columns-used 2))
(define (used-description columns-used)
  (vector-ref columns-used 3))
(define (used-account-name columns-used)
  (vector-ref columns-used 4))
(define (used-other-account-name columns-used)
  (vector-ref columns-used 5))
(define (used-shares columns-used)
  (vector-ref columns-used 6))
(define (used-price columns-used)
  (vector-ref columns-used 7))
(define (used-amount-single columns-used)
  (vector-ref columns-used 8))
(define (used-amount-double-positive columns-used)
  (vector-ref columns-used 9))
(define (used-amount-double-negative columns-used)
  (vector-ref columns-used 10))
(define (used-account-full-name columns-used)
  (vector-ref columns-used 12))
(define (used-memo columns-used)
  (vector-ref columns-used 13))
(define (used-account-code columns-used)
  (vector-ref columns-used 14))
(define (used-other-account-code columns-used)
  (vector-ref columns-used 15))
(define (used-other-account-full-name columns-used)
  (vector-ref columns-used 16))
(define (used-sort-account-code columns-used)
  (vector-ref columns-used 17))
(define (used-sort-account-full-name columns-used)
  (vector-ref columns-used 18))
(define (used-notes columns-used)
  (vector-ref columns-used 19))

(define columns-used-size 20)

(define (num-columns-required columns-used)
  (do ((i 0 (+ i 1))
       (col-req 0 col-req))
    ((>= i columns-used-size) col-req)
    ; If column toggle is true, increase column count. But attention:
    ; some toggles only change the meaning of another toggle. Don't count these modifier toggles
    (if (and (not (= i 12)) ; Skip Account Full Name toggle - modifies Account Name column
             (not (= i 16)) ; Skip Other Account Full Name toggle - modifies Other Account Name column
             (not (= i 17)) ; Skip Sort Account Code - modifies Account Name subheading
             (not (= i 18)) ; Skip Sort Account Full Name - modifies Account Name subheading
             (not (= i 19)) ; Skip Note toggle - modifies Memo column
             (vector-ref columns-used i))
        (set! col-req (+ col-req 1)))
    ; Account Code and Account Name share one column so if both were ticked the
    ; the check above would have set up one column too much. The check below
    ; will compensate these again.
    (if (or (and (= i 14) (vector-ref columns-used 14) (vector-ref columns-used 4)) ; Account Code and Name
            (and (= i 15) (vector-ref columns-used 15) (vector-ref columns-used 5))) ; Other Account Code and Name
        (set! col-req (- col-req 1)))))

(define (build-column-used options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (let ((column-list (make-vector columns-used-size #f)))
    (if (opt-val gnc:pagename-display (N_ "Date"))
        (vector-set! column-list 0 #t))
    (if (opt-val gnc:pagename-display (N_ "Reconciled Date"))
        (vector-set! column-list 1 #t))
    (if (if (gnc:lookup-option options gnc:pagename-display (N_ "Num"))
            (opt-val gnc:pagename-display (N_ "Num"))
            (opt-val gnc:pagename-display (N_ "Num/Action")))
        (vector-set! column-list 2 #t))
    (if (opt-val gnc:pagename-display (N_ "Description"))
        (vector-set! column-list 3 #t))
    (if (opt-val gnc:pagename-display (N_ "Account Name"))
        (vector-set! column-list 4 #t))
    (if (opt-val gnc:pagename-display (N_ "Other Account Name"))
        (vector-set! column-list 5 #t))
    ;(if (opt-val gnc:pagename-display (N_ "Shares"))
    ;    (vector-set! column-list 6 #t))
    ;(if (opt-val gnc:pagename-display (N_ "Price"))
    ;    (vector-set! column-list 7 #t))
    ;(let ((amount-setting (opt-val gnc:pagename-display (N_ "Amount"))))
    ;  (if (eq? amount-setting 'single)
    ;      (vector-set! column-list 8 #t))
    ;  (if (eq? amount-setting 'double)
    ;      (begin (vector-set! column-list 9 #t)
    ;             (vector-set! column-list 10 #t))))

    (if (opt-val gnc:pagename-display  (N_ "Use Full Account Name"))
        (vector-set! column-list 12 #t))
    (if (opt-val gnc:pagename-display (N_ "Memo"))
        (vector-set! column-list 13 #t))
    (if (opt-val gnc:pagename-display (N_ "Account Code"))
        (vector-set! column-list 14 #t))
    (if (opt-val gnc:pagename-display (N_ "Other Account Code"))
        (vector-set! column-list 15 #t))
    (if (opt-val gnc:pagename-display (N_ "Use Full Other Account Name"))
        (vector-set! column-list 16 #t))
    (if (opt-val pagename-sorting (N_ "Show Account Code"))
        (vector-set! column-list 17 #t))
    (if (opt-val pagename-sorting (N_ "Show Full Account Name"))
        (vector-set! column-list 18 #t))
    (if (opt-val gnc:pagename-display (N_ "Notes"))
        (vector-set! column-list 19 #t))
    column-list))

(define (make-heading-list column-vector calculated-cells options)
  (let ((heading-list '()))
    (if (used-date column-vector)
        (addto! heading-list (_ "Date")))
    (if (used-reconciled-date column-vector)
        (addto! heading-list (_ "Reconciled Date")))
    (if (used-num column-vector)
        (addto! heading-list (if (and (qof-book-use-split-action-for-num-field
                                       (gnc-get-current-book))
                                      (if (gnc:lookup-option options
                                                             gnc:pagename-display
                                                             (N_ "Trans Number"))
                                          (gnc:option-value
                                           (gnc:lookup-option options
                                                              gnc:pagename-display
                                                              (N_ "Trans Number")))
                                          #f))
                                 (_ "Num/T-Num")
                                 (_ "Num"))))
    (if (used-description column-vector)
        (addto! heading-list (_ "Description")))
    (if (used-memo column-vector)
        (if (used-notes column-vector)
            (addto! heading-list (string-append (_ "Memo") "/" (_ "Notes")))
            (addto! heading-list (_ "Memo"))))
    (if (or (used-account-name column-vector) (used-account-code column-vector))
        (addto! heading-list (_ "Account")))
    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
        (addto! heading-list (_ "Transfer from/to")))
    ;(if (used-shares column-vector)
    ;    (addto! heading-list (_ "Shares")))
    ;(if (used-price column-vector)
    ;    (addto! heading-list (_ "Price")))
    ;(if (used-amount-single column-vector)
    ;    (addto! heading-list (_ "Amount")))
    ;; FIXME: Proper labels: what?
    (if (used-amount-double-positive column-vector)
        (addto! heading-list (_ "Debit")))
    (if (used-amount-double-negative column-vector)
        (addto! heading-list (_ "Credit")))

    (for-each (lambda (arg)
                (addto! heading-list
                        (gnc:make-html-table-cell/markup
                         "column-heading-right"
                         (car arg))))
              calculated-cells)

    (reverse heading-list)))

(define (add-split-row table split column-vector cell-calculators options
                       row-style account-types-to-reverse transaction-row?)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let* ((row-contents '())
         (dummy  (gnc:debug "split is originally" split))
         (parent (xaccSplitGetParent split))
         (account (xaccSplitGetAccount split))
         (account-type (xaccAccountGetType account))
         (currency (xaccTransGetCurrency parent))
         ;the following cannot be used, because we're using each split's tax currency
         ;(if (not (null? account))
         ;             (xaccAccountGetCommodity account)
         ;             (gnc-default-currency)))
         (report-currency (if (opt-val gnc:pagename-general optname-common-currency)
                              (opt-val gnc:pagename-general optname-currency)
                              currency))
         (trans-date (gnc-transaction-get-date-posted parent))
         (converted (lambda (num)
                      (gnc:exchange-by-pricedb-nearest
                       (gnc:make-gnc-monetary currency num)
                       report-currency
                       (timespecCanonicalDayTime trans-date)))))

    (define cells
      (map (lambda (cell)
             (converted ((cdr cell) split)))
           cell-calculators))

    (if (used-date column-vector)
        (addto! row-contents
                (if transaction-row?
                    (gnc:make-html-table-cell/markup "date-cell"
                                                     (gnc-print-date (gnc-transaction-get-date-posted parent)))
                    " ")))
    (if (used-reconciled-date column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup "date-cell"
                                                 (let ((date (gnc-split-get-date-reconciled split)))
                                                   (if (equal? date (cons 0 0))
                                                       " "
                                                       (gnc-print-date date))))))
    (if (used-num column-vector)
        (addto! row-contents
                (if transaction-row?
                    (if (qof-book-use-split-action-for-num-field
                         (gnc-get-current-book))
                        (let* ((num (gnc-get-num-action parent split))
                               (t-num (if (if (gnc:lookup-option options
                                                                 gnc:pagename-display
                                                                 (N_ "Trans Number"))
                                              (opt-val gnc:pagename-display
                                                       (N_ "Trans Number"))
                                              #f)
                                          (gnc-get-num-action parent #f)
                                          ""))
                               (num-string (if (equal? t-num "")
                                               num
                                               (string-append num "/" t-num))))
                          (gnc:make-html-table-cell/markup "text-cell"
                                                           num-string))
                        (gnc:make-html-table-cell/markup "text-cell"
                                                         (gnc-get-num-action parent split)))
                    " ")))

    (if (used-description column-vector)
        (addto! row-contents
                (if transaction-row?
                    (gnc:make-html-table-cell/markup "text-cell"
                                                     (xaccTransGetDescription parent))
                    " ")))

    (if (used-memo column-vector)
        (let ((memo (xaccSplitGetMemo split)))
          (if (and (equal? memo "") (used-notes column-vector))
              (addto! row-contents (xaccTransGetNotes parent))
              (addto! row-contents memo))))

    (if (or (used-account-name column-vector) (used-account-code column-vector))
        (addto! row-contents (account-namestring account
                                                 (used-account-code      column-vector)
                                                 (used-account-name      column-vector)
                                                 (used-account-full-name column-vector))))

    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
        (addto! row-contents (account-namestring (xaccSplitGetAccount
                                                  (xaccSplitGetOtherSplit split))
                                                 (used-other-account-code      column-vector)
                                                 (used-other-account-name      column-vector)
                                                 (used-other-account-full-name column-vector))))

    ;(if (used-shares column-vector)
    ;    (addto! row-contents (xaccSplitGetAmount split)))
    ;(if (used-price column-vector)
    ;    (addto!
    ;     row-contents
    ;     (gnc:make-gnc-monetary (xaccTransGetCurrency parent)
    ;                            (xaccSplitGetSharePrice split))))
    ;(if (used-amount-single column-vector)
    ;    (addto! row-contents
    ;            (gnc:make-html-table-cell/markup "number-cell"
    ;                                             (gnc:html-transaction-anchor parent split-value))))
    ;(if (used-amount-double-positive column-vector)
    ;    (if (gnc-numeric-positive-p (gnc:gnc-monetary-amount split-value))
    ;        (addto! row-contents
    ;                (gnc:make-html-table-cell/markup "number-cell"
    ;                                                 (gnc:html-transaction-anchor parent split-value)))
    ;        (addto! row-contents " ")))
    ;(if (used-amount-double-negative column-vector)
    ;    (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount split-value))
    ;        (addto! row-contents
    ;                (gnc:make-html-table-cell/markup
    ;                 "number-cell" (gnc:html-transaction-anchor parent (gnc:monetary-neg split-value))))
    ;        (addto! row-contents " ")))

    (for-each (lambda (cell)
                (if cell
                    (addto! row-contents
                            (gnc:make-html-table-cell/markup
                             "number-cell"
                             (gnc:html-transaction-anchor
                              parent
                              (gnc:make-gnc-monetary report-currency cell))))
                    (addto! row-contents (gnc:html-make-empty-cell))))
              cells)

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    cells))

(define date-sorting-types (list 'date 'register-order))

(define (trep-options-generator)
  (define gnc:*transaction-report-options* (gnc:new-options))
  (define (gnc:register-trep-option new-option)
    (gnc:register-option gnc:*transaction-report-options* new-option))

  ;; General options

  (gnc:options-add-date-interval!
   gnc:*transaction-report-options*
   gnc:pagename-general (N_ "Start Date") (N_ "End Date") "a")

  ;(gnc:register-trep-option
  ; (gnc:make-multichoice-option
  ;  gnc:pagename-general (N_ "Style")
  ;  "d" (N_ "Report style.")
  ;  'single
  ;  (list (vector 'multi-line
  ;                (N_ "Multi-Line")
  ;                (N_ "Display N lines."))
  ;        (vector 'single
  ;                (N_ "Single")
  ;                (N_ "Display 1 line.")))))

  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    gnc:pagename-general optname-common-currency
    "e" (N_ "Convert all transactions into a common currency.") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
                 gnc:*transaction-report-options*
                 gnc:pagename-general
                 optname-currency
                 x))))

  (gnc:options-add-currency!
   gnc:*transaction-report-options* gnc:pagename-general optname-currency "f")

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-table-export
    "g" (N_ "Formats the table suitable for cut & paste exporting with extra cells.") #f))

  ;; Accounts options

  ;; account to do report on

  (gnc:register-trep-option
   (gnc:make-account-list-limited-option
    gnc:pagename-accounts (N_ "Accounts")
    "b1" (N_ "Report on these accounts.")
    ;; select, by default, no accounts! Selecting all accounts will
    ;; always imply an insanely long waiting time upon opening, and it
    ;; is almost never useful. So we instead display the normal error
    ;; message saying "Click here", and the user knows how to
    ;; continue.
    (lambda ()
      '())
    #f #t
    (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE)))

  (gnc:register-trep-option
   (gnc:make-string-option
    gnc:pagename-accounts (N_ "Account Substring")
    "b15" (N_ "Match only above accounts whose fullname contains substring e.g. ':Travel' will \
match Expenses:Travel:Holiday and Expenses:Business:Travel. Can be left blank, which will \
disable the substring filter. This filter is case-sensitive.")
    ""))

  (gnc:register-trep-option
   (gnc:make-account-list-limited-option
    gnc:pagename-accounts (N_ "Tax Accounts")
    "b17" (N_ "Please find and select the accounts which will hold the tax collected or paid. \
These accounts must contain splits which document the monies which are wholly sent or claimed \
from tax authorities during periodic GST/VAT returns. These accounts must be of type ASSET \
for taxes paid on expenses, and type LIABILITY for taxes collected on sales.")
    (lambda ()
      '())
    #f #t
    (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY)))

  (gnc:register-trep-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Filter By...")
    "b2" (N_ "Filter on these accounts.")
    (lambda ()
      ;; FIXME : gnc:get-current-accounts disappeared.
      (let* ((current-accounts '())
             (root (gnc-get-current-root-account))
             (num-accounts (gnc-account-n-children root))
             (first-account (gnc-account-nth-child root 0)))
        (cond ((not (null? current-accounts))
               (list (car current-accounts)))
              ((> num-accounts 0) (list first-account))
              (else '()))))
    #f #t))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    gnc:pagename-accounts (N_ "Filter Type")
    "c" (N_ "Filter account.")
    'none
    (list (vector 'none
                  (N_ "None")
                  (N_ "Do not do any filtering."))
          (vector 'include
                  (N_ "Include Transactions to/from Filter Accounts")
                  (N_ "Include transactions to/from filter accounts only."))
          (vector 'exclude
                  (N_ "Exclude Transactions to/from Filter Accounts")
                  (N_ "Exclude transactions to/from all filter accounts."))
          )))

  ;;

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    gnc:pagename-accounts optname-void-transactions
    "d" (N_ "How to handle void transactions.")
    'non-void-only
    (list (vector
           'non-void-only
           (N_ "Non-void only")
           (N_ "Show only non-voided transactions."))
          (vector
           'void-only
           (N_ "Void only")
           (N_ "Show only voided transactions."))
          (vector
           'both
           (N_ "Both")
           (N_ "Show both (and include void transactions in totals).")))))

  ;; Sorting options

  (let ((options gnc:*transaction-report-options*)

        (key-choice-list
         (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
             (list (vector 'none
                           (N_ "None")
                           (N_ "Do not sort."))

                   (vector 'account-name
                           (N_ "Account Name")
                           (N_ "Sort & subtotal by account name."))

                   (vector 'account-code
                           (N_ "Account Code")
                           (N_ "Sort & subtotal by account code."))

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'reconciled-date
                           (N_ "Reconciled Date")
                           (N_ "Sort by the Reconciled Date."))

                   (vector 'register-order
                           (N_ "Register Order")
                           (N_ "Sort as in the register."))

                   (vector 'corresponding-acc-name
                           (N_ "Other Account Name")
                           (N_ "Sort by account transferred from/to's name."))

                   (vector 'corresponding-acc-code
                           (N_ "Other Account Code")
                           (N_ "Sort by account transferred from/to's code."))

                   (vector 'amount
                           (N_ "Amount")
                           (N_ "Sort by amount."))

                   (vector 'description
                           (N_ "Description")
                           (N_ "Sort by description."))

                   (vector 'number
                           (N_ "Number/Action")
                           (N_ "Sort by check number/action."))

                   (vector 't-number
                           (N_ "Transaction Number")
                           (N_ "Sort by transaction number."))

                   (vector 'memo
                           (N_ "Memo")
                           (N_ "Sort by memo.")))
             (list (vector 'none
                           (N_ "None")
                           (N_ "Do not sort."))

                   (vector 'account-name
                           (N_ "Account Name")
                           (N_ "Sort & subtotal by account name."))

                   (vector 'account-code
                           (N_ "Account Code")
                           (N_ "Sort & subtotal by account code."))

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'reconciled-date
                           (N_ "Reconciled Date")
                           (N_ "Sort by the Reconciled Date."))

                   (vector 'register-order
                           (N_ "Register Order")
                           (N_ "Sort as in the register."))

                   (vector 'corresponding-acc-name
                           (N_ "Other Account Name")
                           (N_ "Sort by account transferred from/to's name."))

                   (vector 'corresponding-acc-code
                           (N_ "Other Account Code")
                           (N_ "Sort by account transferred from/to's code."))

                   (vector 'amount
                           (N_ "Amount")
                           (N_ "Sort by amount."))

                   (vector 'description
                           (N_ "Description")
                           (N_ "Sort by description."))

                   (vector 'number
                           (N_ "Number")
                           (N_ "Sort by check/transaction number."))

                   (vector 'memo
                           (N_ "Memo")
                           (N_ "Sort by memo.")))))

        (ascending-choice-list
         (list
          (vector 'ascend
                  (N_ "Ascending")
                  (N_ "Smallest to largest, earliest to latest."))
          (vector 'descend
                  (N_ "Descending")
                  (N_ "Largest to smallest, latest to earliest."))))

        (subtotal-choice-list
         (list
          (vector 'none (N_ "None") (N_ "None."))
          (vector 'weekly (N_ "Weekly") (N_ "Weekly."))
          (vector 'monthly (N_ "Monthly") (N_ "Monthly."))
          (vector 'quarterly (N_ "Quarterly") (N_ "Quarterly."))
          (vector 'yearly (N_ "Yearly") (N_ "Yearly."))))

        (prime-sortkey 'account-name)
        (prime-sortkey-subtotal-true #t)
        (sec-sortkey 'register-order)
        (sec-sortkey-subtotal-true #f))

    (define (apply-selectable-by-name-sorting-options)
      (let* ((prime-sortkey-enabled (not (eq? prime-sortkey 'none)))
             (prime-sortkey-subtotal-enabled (member prime-sortkey subtotal-enabled))
             (prime-date-sortingtype-enabled (member prime-sortkey date-sorting-types))
             (sec-sortkey-enabled (not (eq? sec-sortkey 'none)))
             (sec-sortkey-subtotal-enabled (member sec-sortkey subtotal-enabled))
             (sec-date-sortingtype-enabled (member sec-sortkey date-sorting-types)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-subtotal
         prime-sortkey-subtotal-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-sortorder
         prime-sortkey-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-subtotal
         sec-sortkey-subtotal-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-sortorder
         sec-sortkey-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-full-account-name
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-show-account-code
         (or (and prime-sortkey-subtotal-enabled prime-sortkey-subtotal-true)
             (and sec-sortkey-subtotal-enabled sec-sortkey-subtotal-true)))

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-date-subtotal
         prime-date-sortingtype-enabled)

        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-date-subtotal
         sec-date-sortingtype-enabled)))

    ;; primary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-prime-sortkey
      "a" (N_ "Sort by this criterion first.")
      prime-sortkey
      key-choice-list #f
      (lambda (x)
        (set! prime-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-full-account-name
      "j1"
      (N_ "Show the full account name for subtotals and subtitles?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-show-account-code
      "j2"
      (N_ "Show the account code for subtotals and subtitles?")
      #f))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-prime-subtotal
      "e5"
      (N_ "Subtotal according to the primary key?")
      prime-sortkey-subtotal-true #f
      (lambda (x)
        (set! prime-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-date-subtotal
      "e2" (N_ "Do a date subtotal.")
      'monthly
      subtotal-choice-list))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-sortorder
      "e" (N_ "Order of primary sorting.")
      'ascend
      ascending-choice-list))

    ;; Secondary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-sortkey
      "f"
      (N_ "Sort by this criterion second.")
      sec-sortkey
      key-choice-list #f
      (lambda (x)
        (set! sec-sortkey x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-complex-boolean-option
      pagename-sorting optname-sec-subtotal
      "i5"
      (N_ "Subtotal according to the secondary key?")
      sec-sortkey-subtotal-true #f
      (lambda (x)
        (set! sec-sortkey-subtotal-true x)
        (apply-selectable-by-name-sorting-options))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-date-subtotal
      "i2" (N_ "Do a date subtotal.")
      'none
      subtotal-choice-list))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-sortorder
      "i" (N_ "Order of Secondary sorting.")
      'ascend
      ascending-choice-list)))

  ;; Display options

  (for-each
   (lambda (l)
     (gnc:register-trep-option
      (gnc:make-simple-boolean-option
       gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
   ;; One list per option here with: option-name, sort-tag,
   ;; help-string, default-value
   (list
    (list (N_ "Date")                         "a"  (N_ "Display the date?") #t)
    (list (N_ "Reconciled Date")              "a2" (N_ "Display the reconciled date?") #f)
    (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
        (list (N_ "Num/Action")               "b"  (N_ "Display the check number?") #t)
        (list (N_ "Num")                      "b"  (N_ "Display the check number?") #t))
    (list (N_ "Description")                  "c"  (N_ "Display the description?") #t)
    (list (N_ "Notes")                        "d2" (N_ "Display the notes if the memo is unavailable?") #t)
    ;; account name option appears here
    (list (N_ "Use Full Account Name")        "f"  (N_ "Display the full account name?") #t)
    (list (N_ "Account Code")                 "g"  (N_ "Display the account code?") #f)
    ;; other account name option appears here
    (list (N_ "Use Full Other Account Name")  "i"  (N_ "Display the full account name?") #f)
    (list (N_ "Other Account Code")           "j"  (N_ "Display the other account code?") #f)
    ;(list (N_ "Shares")                       "k"  (N_ "Display the number of shares?") #f)
    ;(list (N_ "Price")                        "l"  (N_ "Display the shares price?") #f)
    ;; note the "Amount" multichoice option in between here
    (list (N_ "Totals")                       "o"  (N_ "Display the totals?") #t)
    (list (N_ "Individual income columns")    "p" (N_ "Display individual income columns rather than their sum") #f)
    (list (N_ "Individual expense columns")   "q" (N_ "Display individual expense columns rather than their sum") #f)
    (list (N_ "Individual tax columns")       "r"  (N_ "Display individual tax columns rather than their sum") #f)
    (list (N_ "Remittance amount")            "s"  (N_ "Display the remittance amount (total sales - total purchases)") #f)
    (list (N_ "Net Income")                   "t"  (N_ "Display the net income (sales without tax - purchases without tax)") #f)
    (list (N_ "Tax payable")                  "t"  (N_ "Display the tax payable (tax on sales - tax on purchases)") #f)
    ))

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc:register-trep-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Trans Number")
        "b2" (N_ "Display the trans number?") #f)))

  ;; Add an option to display the memo, and disable the notes option
  ;; when memos are not included.
  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    gnc:pagename-display (N_ "Memo")
    "d"  (N_ "Display the memo?") #t
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
                 gnc:*transaction-report-options*
                 gnc:pagename-display
                 (N_ "Notes")
                 x))))

  ;; Ditto for Account Name #t -> Use Full Account Name is selectable
  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    gnc:pagename-display (N_ "Account Name")
    "e"  (N_ "Display the account name?") #t
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
                 gnc:*transaction-report-options*
                 gnc:pagename-display
                 (N_ "Use Full Account Name")
                 x))))

  ;; Ditto for Other Account Name #t -> Use Full Other Account Name is selectable
  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    gnc:pagename-display (N_ "Other Account Name")
    "h5"  (N_ "Display the other account name? (if this is a split transaction, this parameter is guessed).") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
                 gnc:*transaction-report-options*
                 gnc:pagename-display
                 (N_ "Use Full Other Account Name")
                 x))))

  ;(gnc:register-trep-option
  ; (gnc:make-multichoice-option
  ;  gnc:pagename-display (N_ "Amount")
  ;  "m" (N_ "Display the amount?")
  ;  'single
  ;  (list
  ;   (vector 'none (N_ "None") (N_ "No amount display."))
  ;   (vector 'single (N_ "Single") (N_ "Single Column Display."))
  ;   (vector 'double (N_ "Double") (N_ "Two Column Display."))
  ;   )))

  ;(gnc:register-trep-option
  ; (gnc:make-multichoice-option
  ;  gnc:pagename-display (N_ "Sign Reverses")
  ;  "p" (N_ "Reverse amount display for certain account types.")
  ;  'none
  ;  (list
  ;   (vector 'none (N_ "None") (N_ "Don't change any displayed amounts."))
  ;   (vector 'income-expense (N_ "Income and Expense")
  ;           (N_ "Reverse amount display for Income and Expense Accounts."))
  ;   (vector 'credit-accounts (N_ "Credit Accounts")
  ;           (N_ "Reverse amount display for Liability, Payable, Equity, Credit Card, and Income accounts.")))))

  (gnc:options-set-default-section gnc:*transaction-report-options*
                                   gnc:pagename-general)

  gnc:*transaction-report-options*)


(define (display-date-interval begin end)
  (let ((begin-string (gnc-print-date begin))
        (end-string (gnc-print-date end)))
    (sprintf #f (_ "From %s To %s") begin-string end-string)))

(define (get-primary-subtotal-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Primary Subtotals/headings"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-secondary-subtotal-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Secondary Subtotals/headings"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-grand-total-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Grand Total"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-odd-row-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Split Odd"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-even-row-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Split Even"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))


;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the big function that builds the whole table.
(define (make-split-table splits options
                          accounts-tax-paid
                          accounts-tax-collected
                          accounts-sales
                          accounts-purchases
                          primary-subtotal-pred
                          secondary-subtotal-pred
                          primary-subheading-renderer
                          secondary-subheading-renderer
                          primary-subtotal-renderer
                          secondary-subtotal-renderer)

  (let ((work-to-do (length splits))
        (work-done 0)
        (used-columns (build-column-used options)))

    (define calculated-cells
      (letrec
          ((myadd (lambda (X Y) (if X (if Y (gnc-numeric-add X Y GNC-DENOM-AUTO GNC-RND-ROUND) X) Y)))
           (myneg (lambda (X) (if X (gnc-numeric-neg X) #f)))
           (split-adder (lambda (split accountlist typefilter)
                          (let* ((transaction (xaccSplitGetParent split))
                                 (splits-in-transaction (xaccTransGetSplitList transaction))
                                 (sum #f))
                            (for-each (lambda (s)
                                        (let* ((splitAcc (xaccSplitGetAccount s))
                                               (splitVal (if (xaccTransGetVoidStatus transaction)
                                                             (xaccSplitVoidFormerValue s)
                                                             (xaccSplitGetValue s)))
                                               (splitCommodity (xaccAccountGetCommodity splitAcc))
                                               (splitAccType (xaccAccountGetType splitAcc))
                                               (splitAccName (xaccAccountGetName splitAcc)))
                                          (if accountlist
                                              (if (member splitAcc accountlist)
                                                  (set! sum (myadd sum splitVal))))
                                          (if typefilter
                                              (if (eq? typefilter splitAccType)
                                                  (set! sum (myadd sum splitVal))))))
                                      splits-in-transaction)
                            sum)))
           ;(sales-without-tax (lambda (s) (split-adder s #f ACCT-TYPE-INCOME)))
           ;(purchases-without-tax (lambda (s) (split-adder s #f ACCT-TYPE-EXPENSE)))
           (tax-on-sales (lambda (s) (split-adder s accounts-tax-collected #f)))
           (tax-on-purchases (lambda (s) (split-adder s accounts-tax-paid #f)))
           (sales-without-tax (lambda (s) (split-adder s accounts-sales #f)))
           (purchases-without-tax (lambda (s) (split-adder s accounts-purchases #f)))
           (account-adder (lambda (acc) (lambda (s) (split-adder s (list acc) #f))))
           (total-sales (lambda (s) (myadd (tax-on-sales s) (sales-without-tax s))))
           (total-purchases (lambda (s) (myadd (tax-on-purchases s) (purchases-without-tax s))))
           (bank-remittance (lambda (s) (myneg (myadd (total-sales s) (total-purchases s)))))
           (net-income (lambda (s) (myneg (myadd (sales-without-tax s) (purchases-without-tax s)))))
           (tax-payable (lambda (s) (myneg (myadd (tax-on-purchases s) (tax-on-sales s))))))
        (append
         (list (cons "Total Sales" total-sales))
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Individual income columns")))
             (map (lambda (acc) (cons (xaccAccountGetName acc) (account-adder acc)))
                  accounts-sales)
             (list (cons "Net Sales" sales-without-tax)))
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Individual tax columns")))
             (map (lambda (acc) (cons (xaccAccountGetName acc) (account-adder acc)))
                  accounts-tax-collected)
             (list (cons "Tax on Sales" tax-on-sales)))
         (list (cons "Total Purchases" total-purchases))
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Individual expense columns")))
             (map (lambda (acc) (cons (xaccAccountGetName acc) (account-adder acc)))
                  accounts-purchases)
             (list (cons "Net Purchases" purchases-without-tax)))
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Individual tax columns")))
             (map (lambda (acc) (cons (xaccAccountGetName acc) (account-adder acc)))
                  accounts-tax-paid)
             (list (cons "Tax on Purchases" tax-on-purchases)))
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Remittance amount")))
             (list (cons "Remittance" bank-remittance))
             '())
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Net Income")))
             (list (cons "Net Income" net-income))
             '())
         (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display (N_ "Tax payable")))
             (list (cons "Tax Payable" tax-payable))
             '())
         )))


    ;(define (get-account-types-to-reverse options)
    ;  (cdr (assq (gnc:option-value
    ;              (gnc:lookup-option options
    ;                                 gnc:pagename-display
    ;                                 (N_ "Sign Reverses")))
    ;             account-types-to-reverse-assoc-list)))

    ;(define (transaction-report-multi-rows-p options)
    ;  (eq? (gnc:option-value
    ;        (gnc:lookup-option options gnc:pagename-general (N_ "Style")))
    ;       'multi-line))

    (define (transaction-report-export-p options)
      (gnc:option-value
       (gnc:lookup-option options gnc:pagename-general
                          optname-table-export)))

    (define (add-other-split-rows split table used-columns
                                  row-style account-types-to-reverse)
      (define (other-rows-driver split parent table used-columns i)
        (let ((current (xaccTransGetSplit parent i)))
          (cond ((null? current) #f)
                ((equal? current split)
                 (other-rows-driver split parent table used-columns (+ i 1)))
                (else (begin
                        (add-split-row table current used-columns calculated-cells options
                                       row-style account-types-to-reverse #f)
                        (other-rows-driver split parent table used-columns
                                           (+ i 1)))))))

      (other-rows-driver split (xaccSplitGetParent split)
                         table used-columns 0))

    (define (do-rows-with-subtotals splits
                                    table
                                    used-columns
                                    width
                                    multi-rows?
                                    odd-row?
                                    export?
                                    account-types-to-reverse
                                    primary-subtotal-pred
                                    secondary-subtotal-pred
                                    primary-subheading-renderer
                                    secondary-subheading-renderer
                                    primary-subtotal-renderer
                                    secondary-subtotal-renderer
                                    primary-subtotal-collectors
                                    secondary-subtotal-collectors
                                    total-collectors)

      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
      (set! work-done (+ 1 work-done))
      (if (null? splits)
          (begin
            (gnc:html-table-append-row/markup!
             table
             def:grand-total-style
             (list
              (gnc:make-html-table-cell/size
               1 width (gnc:make-html-text (gnc:html-markup-hr)))))
            (if (gnc:option-value (gnc:lookup-option options "Display" "Totals"))
                (render-grand-total table width total-collectors export?)))

          (let* ((current (car splits))
                 (current-row-style (if multi-rows? def:normal-row-style
                                        (if odd-row? def:normal-row-style
                                            def:alternate-row-style)))
                 (rest (cdr splits))
                 (next (if (null? rest) #f
                           (car rest)))
                 (split-values (add-split-row
                                table
                                current
                                used-columns
                                calculated-cells
                                options
                                current-row-style
                                account-types-to-reverse
                                #t)))
            (if multi-rows?
                (add-other-split-rows
                 current table used-columns def:alternate-row-style
                 account-types-to-reverse))

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 primary-subtotal-collectors
                 split-values)

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 secondary-subtotal-collectors
                 split-values)

            (map (lambda (collector value)
                   (if value
                       (collector 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))))
                 total-collectors
                 split-values)

            (if (and primary-subtotal-pred
                     (or (not next)
                         (and next
                              (not (primary-subtotal-pred current next)))))
                (begin
                  (if secondary-subtotal-pred
                      (begin
                        (secondary-subtotal-renderer
                         table width current
                         secondary-subtotal-collectors
                         def:secondary-subtotal-style used-columns export?)
                        (for-each (lambda (coll) (coll 'reset #f #f))
                                  secondary-subtotal-collectors)))

                  (primary-subtotal-renderer table width current
                                             primary-subtotal-collectors
                                             def:primary-subtotal-style used-columns
                                             export?)

                  (for-each (lambda (coll) (coll 'reset #f #f))
                            primary-subtotal-collectors)

                  (if next
                      (begin
                        (primary-subheading-renderer
                         next table width def:primary-subtotal-style used-columns)
                        (if secondary-subtotal-pred
                            (secondary-subheading-renderer
                             next
                             table
                             width def:secondary-subtotal-style used-columns)))))

                (if (and secondary-subtotal-pred
                         (or (not next)
                             (and next
                                  (not (secondary-subtotal-pred
                                        current next)))))
                    (begin (secondary-subtotal-renderer
                            table width current
                            secondary-subtotal-collectors
                            def:secondary-subtotal-style used-columns export?)
                           (for-each (lambda (coll) (coll 'reset #f #f))
                                     secondary-subtotal-collectors)
                           (if next
                               (secondary-subheading-renderer
                                next table width
                                def:secondary-subtotal-style used-columns)))))

            (do-rows-with-subtotals rest
                                    table
                                    used-columns
                                    width
                                    multi-rows?
                                    (not odd-row?)
                                    export?
                                    account-types-to-reverse
                                    primary-subtotal-pred
                                    secondary-subtotal-pred
                                    primary-subheading-renderer
                                    secondary-subheading-renderer
                                    primary-subtotal-renderer
                                    secondary-subtotal-renderer
                                    primary-subtotal-collectors
                                    secondary-subtotal-collectors
                                    total-collectors))))

    (let* ((table (gnc:make-html-table))
           (width (num-columns-required used-columns))
           (multi-rows? #f) ;disable. (transaction-report-multi-rows-p options))
           (export?  (transaction-report-export-p options))
           (account-types-to-reverse '())) ;disabled.            (get-account-types-to-reverse '()))options)))

      (gnc:html-table-set-col-headers!
       table
       (make-heading-list used-columns calculated-cells options))

      (if (not (null? splits))
          (begin
            (if primary-subheading-renderer
                (primary-subheading-renderer
                 (car splits) table width def:primary-subtotal-style used-columns))
            (if secondary-subheading-renderer
                (secondary-subheading-renderer
                 (car splits) table width def:secondary-subtotal-style used-columns))

            (do-rows-with-subtotals splits table used-columns width
                                    multi-rows? #t
                                    export?
                                    account-types-to-reverse
                                    primary-subtotal-pred
                                    secondary-subtotal-pred
                                    primary-subheading-renderer
                                    secondary-subheading-renderer
                                    primary-subtotal-renderer
                                    secondary-subtotal-renderer
                                    (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells)
                                    (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells)
                                    (map (lambda (x) (gnc:make-commodity-collector)) calculated-cells)
                                    ;(gnc:make-commodity-collector)
                                    ;(gnc:make-commodity-collector)
                                    ;(gnc:make-commodity-collector)
                                    )))
      table)))

;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the renderer function for this report.
(define (trep-renderer report-obj)
  (define options (gnc:report-options report-obj))
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (define comp-funcs-assoc-list
    ;; Defines the different sorting keys, together with the
    ;; subtotal functions. Each entry: (cons
    ;; 'sorting-key-option-value (vector 'query-sorting-key
    ;; subtotal-function subtotal-renderer))
    ;;  (let* ((used-columns (build-column-used options))) ;; tpo: gives unbound variable options?
    (let* ((used-columns (build-column-used (gnc:report-options report-obj))))
      (list (cons 'account-name  (vector
                                  (list SPLIT-ACCT-FULLNAME)
                                  split-account-full-name-same-p
                                  render-account-subheading
                                  render-account-subtotal))
            (cons 'account-code  (vector
                                  (list SPLIT-ACCOUNT ACCOUNT-CODE-)
                                  split-account-code-same-p
                                  render-account-subheading
                                  render-account-subtotal))
            (cons 'date          (vector
                                  (list SPLIT-TRANS TRANS-DATE-POSTED)
                                  #f #f #f))
            (cons 'reconciled-date (vector
                                    (list SPLIT-DATE-RECONCILED)
                                    #f #f #f))
            (cons 'register-order (vector
                                   (list QUERY-DEFAULT-SORT)
                                   #f #f #f))
            (cons 'corresponding-acc-name
                  (vector
                   (list SPLIT-CORR-ACCT-NAME)
                   split-same-corr-account-full-name-p
                   render-corresponding-account-subheading
                   render-corresponding-account-subtotal))
            (cons 'corresponding-acc-code
                  (vector
                   (list SPLIT-CORR-ACCT-CODE)
                   split-same-corr-account-code-p
                   render-corresponding-account-subheading
                   render-corresponding-account-subtotal))
            (cons 'amount        (vector (list SPLIT-VALUE) #f #f #f))
            (cons 'description   (vector (list SPLIT-TRANS TRANS-DESCRIPTION) #f #f #f))
            (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
                (cons 'number    (vector (list SPLIT-ACTION) #f #f #f))
                (cons 'number    (vector (list SPLIT-TRANS TRANS-NUM) #f #f #f)))
            (cons 't-number      (vector (list SPLIT-TRANS TRANS-NUM) #f #f #f))
            (cons 'memo          (vector (list SPLIT-MEMO) #f #f #f))
            (cons 'none          (vector '() #f #f #f)))))

  (define date-comp-funcs-assoc-list
    ;; Extra list for date option. Each entry: (cons
    ;; 'date-subtotal-option-value (vector subtotal-function
    ;; subtotal-renderer))
    (list
     (cons 'none (vector #f #f #f))
     (cons 'weekly (vector split-same-week-p render-week-subheading
                           render-week-subtotal))
     (cons 'monthly (vector split-same-month-p render-month-subheading
                            render-month-subtotal))
     (cons 'quarterly (vector split-same-quarter-p render-quarter-subheading
                              render-quarter-subtotal))
     (cons 'yearly (vector split-same-year-p render-year-subheading
                           render-year-subtotal))))

  (define (get-subtotalstuff-helper
           name-sortkey name-subtotal name-date-subtotal
           comp-index date-index)
    ;; The value of the sorting-key multichoice option.
    (let ((sortkey (opt-val pagename-sorting name-sortkey)))
      (if (member sortkey date-sorting-types)
          ;; If sorting by date, look up the value of the
          ;; date-subtotalling multichoice option and return the
          ;; corresponding funcs in the assoc-list.
          (vector-ref
           (cdr (assq (opt-val pagename-sorting name-date-subtotal)
                      date-comp-funcs-assoc-list))
           date-index)
          ;; For everything else: 1. check whether sortkey has
          ;; subtotalling enabled at all, 2. check whether the
          ;; enable-subtotal boolean option is #t, 3. look up the
          ;; appropriate funcs in the assoc-list.
          (and (member sortkey subtotal-enabled)
               (and (opt-val pagename-sorting name-subtotal)
                    (vector-ref
                     (cdr (assq sortkey comp-funcs-assoc-list))
                     comp-index))))))

  (define (get-query-sortkey sort-option-value)
    (vector-ref
     (cdr (assq sort-option-value comp-funcs-assoc-list))
     0))

  (define (get-subtotal-pred
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     1 0))

  (define (get-subheading-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     2 1))

  (define (get-subtotal-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     3 2))

  ;;(define (get-other-account-names account-list)
  ;;  ( map (lambda (acct)  (gnc-account-get-full-name acct)) account-list))

  (define (splits-filter-unique-transactions splits)
    (let ((have-trans-hash (make-hash-table)))
      (define (only-one-copy? split)
        (let* ((parent (xaccSplitGetParent split))
               (trans-guid (gncTransGetGUID parent)))
          (if (hash-ref have-trans-hash trans-guid #f)
              #f  ; already have a copy of this transaction
              (begin
                (hash-set! have-trans-hash trans-guid #t)
                #t))))
      (filter only-one-copy? splits)))

  (define (is-filter-member split account-list)
    (let* ((txn (xaccSplitGetParent split))
           (splitcount (xaccTransCountSplits txn)))

      (cond
        ;; A 2-split transaction - test separately so it can be optimized
        ;; to significantly reduce the number of splits to traverse
        ;; in guile code
        ((= splitcount 2)
         (let* ((other      (xaccSplitGetOtherSplit split))
                (other-acct (xaccSplitGetAccount other)))
           (member other-acct account-list)))

        ;; A multi-split transaction - run over all splits
        ((> splitcount 2)
         (let ((splits (xaccTransGetSplitList txn)))

           ;; Walk through the list of splits.
           ;; if we reach the end, return #f
           ;; if the 'this' != 'split' and the split->account is a member
           ;; of the account-list, then return #t, else recurse
           (define (is-member splits)
             (if (null? splits)
                 #f
                 (let* ((this (car splits))
                        (rest (cdr splits))
                        (acct (xaccSplitGetAccount this)))
                   (if (and (not (eq? this split))
                            (member acct account-list))
                       #t
                       (is-member rest)))))

           (is-member splits)))

        ;; Single transaction splits
        (else #f))))

  (gnc:report-starting reportname)

  (let* ((document (gnc:make-html-document))
         (c_account_0 (opt-val gnc:pagename-accounts "Accounts"))
         (c_account_substring (opt-val gnc:pagename-accounts "Account Substring"))
         (c_account_1 (filter
                       (lambda (acc)
                         (string-contains (gnc-account-get-full-name acc) c_account_substring))
                       c_account_0))
         (c_account_2 (opt-val gnc:pagename-accounts "Filter By..."))
         (tax-accounts (opt-val gnc:pagename-accounts "Tax Accounts"))
         (accounts-tax-collected (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-LIABILITY)) tax-accounts))
         (accounts-tax-paid (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-ASSET)) tax-accounts))
         (accounts-sales (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-INCOME)) c_account_1))
         (accounts-purchases (filter (lambda (acc) (eq? (xaccAccountGetType acc) ACCT-TYPE-EXPENSE)) c_account_1))
         (filter-mode (opt-val gnc:pagename-accounts "Filter Type"))
         (begindate (gnc:timepair-start-day-time
                     (gnc:date-option-absolute-time
                      (opt-val gnc:pagename-general "Start Date"))))
         (enddate (gnc:timepair-end-day-time
                   (gnc:date-option-absolute-time
                    (opt-val gnc:pagename-general "End Date"))))
         (report-title (opt-val
                        gnc:pagename-general
                        gnc:optname-reportname))
         (primary-key (opt-val pagename-sorting optname-prime-sortkey))
         (primary-order (opt-val pagename-sorting "Primary Sort Order"))
         (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
         (secondary-order (opt-val pagename-sorting "Secondary Sort Order"))
         (void-status (opt-val gnc:pagename-accounts optname-void-transactions))
         (splits '())
         (query (qof-query-create-for-splits)))

    ;(gnc:warn "c1 is " c_account_1)
    ;(gnc:warn "c2 is " c_account_2)
    ;(gnc:warn "first c1 is " (xaccAccountGetName (car c_account_1)))

    (if (not (or (null? c_account_1) (and-map not c_account_1)))
        (begin
          (qof-query-set-book query (gnc-get-current-book))
          ;;(gnc:warn "query is:" query)
          (xaccQueryAddAccountMatch query
                                    c_account_1
                                    QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          (xaccQueryAddDateMatchTS
           query #t begindate #t enddate QOF-QUERY-AND)
          (qof-query-set-sort-order query
                                    (get-query-sortkey primary-key)
                                    (get-query-sortkey secondary-key)
                                    '())

          (qof-query-set-sort-increasing query
                                         (eq? primary-order 'ascend)
                                         (eq? secondary-order 'ascend)
                                         #t)

          (case void-status
            ((non-void-only)
             (gnc:query-set-match-non-voids-only! query (gnc-get-current-book)))
            ((void-only)
             (gnc:query-set-match-voids-only! query (gnc-get-current-book)))
            (else #f))

          (set! splits (qof-query-run query))

          ;;(gnc:warn "Splits in trep-renderer:" splits)

          ;;(gnc:warn "Filter account names:" (get-other-account-names c_account_2))

          ;;This should probably a cond or a case to allow for different filter types.
          ;;(gnc:warn "Filter Mode: " filter-mode)
          (if (eq? filter-mode 'include)
              (begin
                ;;(gnc:warn "Including Filter Accounts")
                (set! splits (filter (lambda (split)
                                       (is-filter-member split c_account_2))
                                     splits))))

          (if (eq? filter-mode 'exclude)
              (begin
                ;;(gnc:warn "Excluding Filter Accounts")
                (set! splits (filter (lambda (split)
                                       (not (is-filter-member split c_account_2)))
                                     splits))))

          ; We have to remove duplicates because the report will *sum* amounts in a transaction
          ; otherwise it will double count where transaction contains 2 splits in same account
          (set! splits (splits-filter-unique-transactions splits))

          ; For each split, we will only keep those which contain useful data
          ; e.g. show invoices & regular transactions. We will also disallow closing txns.
          (set! splits (filter
                        (lambda (split)
                          (let* ((trans (xaccSplitGetParent split))
                                 (txn-type (xaccTransGetTxnType trans)))
                            (and (member txn-type (list TXN-TYPE-NONE TXN-TYPE-INVOICE))
                                 (not (xaccTransGetIsClosingTxn trans)))))
                        splits))

          (if (not (null? splits))
              (let ((table
                     (make-split-table
                      splits
                      options
                      accounts-tax-paid
                      accounts-tax-collected
                      accounts-sales
                      accounts-purchases
                      (get-subtotal-pred optname-prime-sortkey
                                         optname-prime-subtotal
                                         optname-prime-date-subtotal)
                      (get-subtotal-pred optname-sec-sortkey
                                         optname-sec-subtotal
                                         optname-sec-date-subtotal)
                      (get-subheading-renderer optname-prime-sortkey
                                               optname-prime-subtotal
                                               optname-prime-date-subtotal)
                      (get-subheading-renderer optname-sec-sortkey
                                               optname-sec-subtotal
                                               optname-sec-date-subtotal)
                      (get-subtotal-renderer   optname-prime-sortkey
                                               optname-prime-subtotal
                                               optname-prime-date-subtotal)
                      (get-subtotal-renderer   optname-sec-sortkey
                                               optname-sec-subtotal
                                               optname-sec-date-subtotal))))

                (gnc:html-document-set-title! document
                                              report-title)
                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-h3
                   (display-date-interval begindate enddate))))

                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-p
                   "Input Tax accounts: "
                   (string-join (map gnc-account-get-full-name accounts-tax-paid) ", "))))

                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-p
                   "Output Tax accounts: "
                   (string-join (map gnc-account-get-full-name accounts-tax-collected) ", "))))

                (if (null? (append accounts-tax-collected accounts-tax-paid))
                    (gnc:html-document-add-object!
                     document
                     (gnc:make-html-text
                      (gnc:html-markup-p
                       "There are no input/output tax accounts set up. This is probably not what"
                       " you want. "
                       TAX-SETUP-DESC))))

                (gnc:html-document-add-object!
                 document
                 table)
                (qof-query-destroy query))
              ;; error condition: no splits found
              (let ((p (gnc:make-html-text)))
                (gnc:html-text-append!
                 p
                 (gnc:html-markup-h2
                  (_ "No matching transactions found"))
                 (gnc:html-markup-p
                  (_ "No transactions were found that match the time interval and account selection specified in the Options panel.")))
                (gnc:html-document-add-object! document p))))

        ;; error condition: no accounts specified

        (begin
          (gnc:html-document-add-object!
           document
           (gnc:html-make-no-account-warning
            report-title (gnc:report-id report-obj)))

          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-p
             "This report is useful to calculate periodic business tax payable/receivable from"
             " authorities. From <i>Edit report options</i> above, choose your Business Income and Business Expense accounts."
             " Each transaction may contain, in addition to the accounts payable/receivable or bank accounts,"
             " a split to a tax account, e.g. Income:Sales -$1000, Liability:GST on Sales -$100, Asset:Bank $1100.")
            (gnc:html-markup-p
             " These tax accounts can either be populated using the standard register, or from Business Invoices and Bills "
             " which will require Business > Sales Tax Tables to be set up correctly. Please see the documentation.")))

          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-p TAX-SETUP-DESC)))))

    (gnc:report-finished)
    document))

;; Define the report.
(gnc:define-report
 'version 1
 'menu-path (list gnc:menuname-income-expense)
 'name reportname
 'report-guid "5bf27f249a0d11e7abc4cec278b6b50a"
 'options-generator trep-options-generator
 'renderer trep-renderer)