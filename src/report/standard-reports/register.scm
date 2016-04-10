;; -*-scheme-*-
;; register.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash report standard-reports register))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

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
(define (num-col columns-used)
  (vector-ref columns-used 1))
(define (description-col columns-used)
  (vector-ref columns-used 2))
(define (memo-col columns-used)
  (vector-ref columns-used 3))
(define (account-col columns-used)
  (vector-ref columns-used 4))
(define (shares-col columns-used)
  (vector-ref columns-used 5))
(define (price-col columns-used)
  (vector-ref columns-used 6))
(define (amount-single-col columns-used)
  (vector-ref columns-used 7))
(define (debit-col columns-used)
  (vector-ref columns-used 8))
(define (credit-col columns-used)
  (vector-ref columns-used 9))
(define (balance-col columns-used)
  (vector-ref columns-used 10))
(define (value-single-col columns-used)
  (vector-ref columns-used 11))
(define (value-debit-col columns-used)
  (vector-ref columns-used 12))
(define (value-credit-col columns-used)
  (vector-ref columns-used 13))
(define (lot-col columns-used)
  (vector-ref columns-used 14))

(define columns-used-size 15)

(define (num-columns-required columns-used)  
  (do ((i 0 (+ i 1)) 
       (col-req 0 col-req)) 
      ((>= i columns-used-size) col-req)
    (if (vector-ref columns-used i)
        (set! col-req (+ col-req 1)))))

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
    (set-col (opt-val "Display" "Date") 0)
    (set-col (if (gnc:lookup-option options "Display" "Num")
                 (opt-val "Display" "Num")
                 (opt-val "Display" "Num/Action")) 1)
    (set-col 
        (if (opt-val "__reg" "journal")
        (or (opt-val "Display" "Memo") (opt-val "Display" "Description") (opt-val "__reg" "double") )
        (opt-val "Display" "Description") 
        )
        2)
    (set-col 
        (if (opt-val "__reg" "journal")
        #f
        (opt-val "Display" "Memo") 
        )
        3)
    (set-col (opt-val "Display" "Account") 4)
    (set-col (opt-val "Display" "Shares") 5)
    (set-col (opt-val "Display" "Lot") 14)
    (set-col (opt-val "Display" "Price") 6)
    (let ((invoice? #f)
          (amount-setting (opt-val "Display" "Amount")))
      (if (or invoice? (eq? amount-setting 'single))
          (set-col #t 7)
          (begin
            (set-col #t 8)
            (set-col #t 9))))
    (if (opt-val "Display" "Value")
        (if (amount-single-col col-vector)
            (set-col #t 11)
            (begin
              (set-col #t 12)
              (set-col #t 13))))
    (set-col (opt-val "Display" "Running Balance") 10)

    col-vector))

(define (make-heading-list column-vector
                           debit-string credit-string amount-string
                           multi-rows? action-for-num? ledger-type?)
  (let ((heading-list '()))
    (gnc:debug "Column-vector" column-vector)
    (if (date-col column-vector)
        (addto! heading-list (_ "Date")))
    (if (num-col column-vector)
        (addto! heading-list (if action-for-num?
                                 (if ledger-type?
                                     (_ "T-Num")
                                     (_ "Num/Action"))
                                 (_ "Num"))))
    (if (description-col column-vector)
        (addto! heading-list (_ "Description")))
    (if (memo-col column-vector)
        (addto! heading-list (_ "Memo")))
    (if (account-col column-vector)
        (addto! heading-list (if multi-rows?
                                 (_ "Account")
                                 (_ "Transfer"))))
    (if (shares-col column-vector)
        (addto! heading-list (_ "Shares")))
    (if (lot-col column-vector)
        (addto! heading-list (_ "Lot")))
    (if (price-col column-vector)
        (addto! heading-list (_ "Price")))
    (if (amount-single-col column-vector)
        (addto! heading-list amount-string))
    (if (debit-col column-vector)
        (addto! heading-list debit-string))
    (if (credit-col column-vector)
        (addto! heading-list credit-string))
    (if (value-single-col column-vector)
        (addto! heading-list (_ "Value")))
    (if (value-debit-col column-vector)
        (addto! heading-list (_ "Debit Value")))
    (if (value-credit-col column-vector)
        (addto! heading-list (_ "Credit Value")))
    (if (balance-col column-vector)
        (addto! heading-list (_ "Balance")))
    (reverse heading-list)))

(define (gnc:split-get-balance-display split-info? split)
  (let* ((account (xaccSplitGetAccount split))
         (balance
          (if split-info?
              (xaccSplitGetBalance split)
              (xaccTransGetAccountBalance
               (xaccSplitGetParent split) account))))
    (if (and (not (null? account)) (gnc-reverse-balance account))
        (gnc-numeric-neg balance)
        balance)))

(define (add-split-row table split column-vector row-style transaction-info?
                       split-info? action-for-num? ledger-type? double? memo?
                       description? total-collector)
  (let* ((row-contents '())
         (parent (xaccSplitGetParent split))
         (account (xaccSplitGetAccount split))
         (currency (if (not (null? account))
                       (xaccAccountGetCommodity account)
                       (gnc-default-currency)))
         (trans-currency (xaccTransGetCurrency parent))
         (damount (xaccSplitGetAmount split))
         (split-value (gnc:make-gnc-monetary currency damount)))

    (if (date-col column-vector)
        (addto! row-contents
                (if transaction-info?
                    (gnc:make-html-table-cell/markup
					    "date-cell"
                        (gnc-print-date
                             (gnc-transaction-get-date-posted parent)))
                        " ")))
    (if (num-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if transaction-info?
                        (if (and action-for-num? ledger-type?)
                            (gnc-get-num-action parent #f)
                            (gnc-get-num-action parent split))
                        (if split-info?
                            (gnc-get-action-num  #f split)
                            " ")))))
    (if (description-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if transaction-info?
                        (if description?
                            (xaccTransGetDescription parent)
                            " " )
                        (if split-info?
                            (if memo?
                                (xaccSplitGetMemo split)
                                " ")
                            " ")))))
    (if (memo-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if transaction-info?
                        (xaccSplitGetMemo split)
                        " "))))
    (if (account-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if split-info?
                        (if transaction-info?
                            (let ((other-split
                                   (xaccSplitGetOtherSplit split)))
                              (if (not (null? other-split))
                                  (gnc-account-get-full-name
                                   (xaccSplitGetAccount other-split))
                                  (_ "-- Split Transaction --")))
                            (gnc-account-get-full-name account))
                        " "))))
    (if (shares-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if split-info?
                        (xaccSplitGetAmount split)
                        " "))))
    (if (lot-col column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup
					"text-cell"
                        (if split-info?
                            (gnc-lot-get-title (xaccSplitGetLot split))
                            " "))))
    (if (price-col column-vector)
        (addto! row-contents 
                (gnc:make-html-table-cell/markup
					"text-cell"
                    (if split-info?
                        (gnc:make-gnc-monetary
                         currency (xaccSplitGetSharePrice split))
                        " "))))
    (if (amount-single-col column-vector)
        (addto! row-contents
                (if split-info?
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:html-split-anchor split split-value))
                    " ")))
    (if (debit-col column-vector)
        (if (gnc-numeric-positive-p (gnc:gnc-monetary-amount split-value))
            (addto! row-contents
                    (if split-info?
                        (gnc:make-html-table-cell/markup
                         "number-cell"
                         (gnc:html-split-anchor split split-value))
                        " "))
            (addto! row-contents " ")))
    (if (credit-col column-vector)
        (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount split-value))
            (addto! row-contents
                    (if split-info?
                        (gnc:make-html-table-cell/markup
                         "number-cell"
                         (gnc:html-split-anchor
                          split (gnc:monetary-neg split-value)))
                        " "))
            (addto! row-contents " ")))
    (if (value-single-col column-vector)
        (addto! row-contents
                (if split-info?
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:make-gnc-monetary trans-currency
                                             (xaccSplitGetValue split)))
                    " ")))
    (if (value-debit-col column-vector)
        (addto! row-contents
                (if (and split-info? (gnc-numeric-positive-p (xaccSplitGetValue split)))
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:make-gnc-monetary trans-currency
                                            (xaccSplitGetValue split)))
                    " ")))
    (if (value-credit-col column-vector)
        (addto! row-contents
                (if (and split-info? (gnc-numeric-negative-p (xaccSplitGetValue split)))
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:make-gnc-monetary trans-currency
                                            (gnc-numeric-neg (xaccSplitGetValue split))))
                    " ")))
    ; For single account registers, use the split's cached balance to remain
    ; consistent with the balances shown in the register itself
    ; For others, use the cumulated balance from the totals-collector
    (if (balance-col column-vector)
        (addto! row-contents
                (if transaction-info?
                    (gnc:make-html-table-cell/markup
                     "number-cell"
                     (gnc:html-split-anchor
                      split
                      (gnc:make-gnc-monetary
                        currency
                        (if ledger-type?
                            (cadr (total-collector 'getpair currency #f))
                            (xaccSplitGetBalance split)))))
                    " ")))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    (if (and double? transaction-info?)
        (if (or (num-col column-vector) (description-col column-vector))
        (begin
          (let ((count 0))
            (set! row-contents '())
            (if (date-col column-vector)
                (begin
                  (set! count (+ count 1))
                  (addto! row-contents " ")))
            (if (and (num-col column-vector) (description-col column-vector))
                (begin
                  (set! count (+ count 1))
                  (addto! row-contents
                    (gnc:make-html-table-cell/markup
					  "text-cell"
                      (if (and action-for-num? (not ledger-type?))
                          (gnc-get-num-action parent #f)
                          " ")))))
            (if (description-col column-vector)
                (addto! row-contents ;; 
                    (gnc:make-html-table-cell/size
                     1 (- (num-columns-required column-vector) count)
                     (xaccTransGetNotes parent)))
                (gnc:make-html-table-cell/size
                     1 (- (num-columns-required column-vector) (- count 1))
                     (if (and action-for-num? (not ledger-type?))
                         (gnc-get-num-action parent #f)
                         " ")))
            (gnc:html-table-append-row/markup! table row-style
                                               (reverse row-contents))))))
    split-value))

(define (lookup-sort-key sort-option)
  (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 0))
(define (lookup-subtotal-pred sort-option)
  (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 1))

(define (options-generator)

  (define gnc:*report-options* (gnc:new-options))

  (define (gnc:register-reg-option new-option)
    (gnc:register-option gnc:*report-options* new-option))

  (gnc:register-reg-option
   (gnc:make-query-option "__reg" "query" '()))
  (gnc:register-reg-option
   (gnc:make-internal-option "__reg" "journal" #f))
  (gnc:register-reg-option
   (gnc:make-internal-option "__reg" "ledger-type" #f))
  (gnc:register-reg-option
   (gnc:make-internal-option "__reg" "double" #f))
  (gnc:register-reg-option
   (gnc:make-internal-option "__reg" "debit-string" (_ "Debit")))
  (gnc:register-reg-option
   (gnc:make-internal-option "__reg" "credit-string" (_ "Credit")))

  (gnc:register-reg-option
   (gnc:make-string-option
    (N_ "General") (N_ "Title")
    "a" (N_ "The title of the report.")
    (N_ "Register Report")))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Date")
    "b" (N_ "Display the date?") #t))

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc:register-reg-option
       (gnc:make-simple-boolean-option
        (N_ "Display") (N_ "Num/Action")
        "c" (N_ "Display the check number/action?") #t))
      (gnc:register-reg-option
       (gnc:make-simple-boolean-option
        (N_ "Display") (N_ "Num")
        "c" (N_ "Display the check number?") #t)))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Description")
    "d" (N_ "Display the description?") #t))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Memo")
    "e" (N_ "Display the memo?") #t))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Account")
    "g" (N_ "Display the account?") #t))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Shares")
    "ha" (N_ "Display the number of shares?") #f))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Lot")
    "hb" (N_ "Display the name of lot the shares are in?") #f))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Price")
    "hc" (N_ "Display the shares price?") #f))

  (gnc:register-reg-option
   (gnc:make-multichoice-option
    (N_ "Display") (N_ "Amount")
    "ia" (N_ "Display the amount?")  
    'double
    (list
     (vector 'single (N_ "Single") (N_ "Single Column Display."))
     (vector 'double (N_ "Double") (N_ "Two Column Display.")))))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Value")
    "ib" (N_ "Display the value in transaction currency?") #f))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Running Balance")
    "k" (N_ "Display a running balance?") #t))

  (gnc:register-reg-option
   (gnc:make-simple-boolean-option
    (N_ "Display") (N_ "Totals")
    "l" (N_ "Display the totals?") #t))


  (gnc:options-set-default-section gnc:*report-options* "General")

  gnc:*report-options*)

;; -----------------------------------------------------------------
;; create the report result
;; -----------------------------------------------------------------

(define (make-split-table splits options
                          debit-string credit-string amount-string)
  ;; ----------------------------------
  ;; local helper
  ;; ----------------------------------
  (define (opt-val section name)
    (gnc:option-value (gnc:lookup-option options section name)))
  (define (reg-report-journal?)
    (opt-val "__reg" "journal"))
  (define (reg-report-ledger-type?)
    (opt-val "__reg" "ledger-type"))
  (define (reg-report-double?)
    (opt-val "__reg" "double"))
  (define (reg-report-invoice?)
    #f)
  (define (reg-report-show-totals?)
    (opt-val "Display" "Totals"))

  (define (add-subtotal-row label leader table used-columns
                            subtotal-collector subtotal-style
                            value?)
    (let ((currency-totals (subtotal-collector
                            'format gnc:make-gnc-monetary #f))
          (single-col (if value?
                          (value-single-col used-columns)
                          (amount-single-col used-columns)))
          (credit-col (if value?
                          (value-credit-col used-columns)
                          (credit-col used-columns)))
          (debit-col (if value?
                         (value-debit-col used-columns)
                         (debit-col used-columns))))

      (define (colspan monetary)
        (cond
         (single-col single-col)
         ((gnc-numeric-negative-p (gnc:gnc-monetary-amount monetary)) credit-col)
         (else debit-col)))

      (define (display-subtotal monetary)
        (if single-col
            (if (and (not (null? leader)) (gnc-reverse-balance leader))
                (gnc:monetary-neg monetary)
                monetary)
            (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount monetary))
                (gnc:monetary-neg monetary)
                monetary)))

      (if (or single-col credit-col debit-col)
          (begin
            (if (not (reg-report-invoice?))
                (gnc:html-table-append-row!
                 table
                 (list
                  (gnc:make-html-table-cell/size
                   1 (num-columns-required used-columns)
                   (gnc:make-html-text (gnc:html-markup-hr))))))

            (for-each (lambda (currency)
                        (gnc:html-table-append-row/markup! 
                         table
                         subtotal-style
                         (append (cons (gnc:make-html-table-cell/markup
                                        "total-label-cell" label)
                                       '())
                                 (list (gnc:make-html-table-cell/size/markup
                                        1 (colspan currency)
                                        "total-number-cell"
                                        (display-subtotal currency))))))
                      currency-totals)))))

  (define (accumulate-totals split total-amount total-value
                             debit-amount debit-value
                             credit-amount credit-value)
    (let* ((parent (xaccSplitGetParent split))
           (account (xaccSplitGetAccount split))
           (split-currency (if (not (null? account))
                               (xaccAccountGetCommodity account)
                               (gnc-default-currency)))
           (split-amount (xaccSplitGetAmount split))
           (trans-currency (xaccTransGetCurrency parent))
           (split-value (xaccSplitGetValue split)))
      (if (gnc-numeric-positive-p split-amount)
          (debit-amount 'add split-currency split-amount)
          (credit-amount 'add split-currency split-amount))
      (if (gnc-numeric-positive-p split-value)
          (debit-value 'add trans-currency split-value)
          (credit-value 'add trans-currency split-value))
      (total-amount 'add split-currency split-amount)
      (total-value 'add trans-currency split-value)))

  (define (add-other-split-rows split table used-columns row-style
                                action-for-num? ledger-type? total-collector)
    (define (other-rows-driver split parent table used-columns i)
      (let ((current (xaccTransGetSplit parent i)))
        (if (not (null? current))
            (begin
              (add-split-row table current used-columns row-style #f #t
                                action-for-num? ledger-type? #f
                                (opt-val "Display" "Memo")
                                (opt-val "Display" "Description")
                                total-collector)
              (other-rows-driver split parent table
                                 used-columns (+ i 1))))))

    (other-rows-driver split (xaccSplitGetParent split)
                       table used-columns 0))

  ;; ----------------------------------
  ;; main loop
  ;; ----------------------------------
  (define (do-rows-with-subtotals leader
                                  splits
                                  table
                                  used-columns
                                  width
                                  multi-rows?
                                  action-for-num?
                                  ledger-type?
                                  double?
                                  odd-row?
                                  total-collector
				  debit-collector
				  credit-collector
                                  total-value
                                  debit-value
                                  credit-value)
    (if (null? splits)
      ;; ----------------------------------
      ;; exit condition reached
      ;; ----------------------------------
	(begin
          ;; ------------------------------------
	  ;; add debit/credit totals to the table
          ;; ------------------------------------
	  (if (reg-report-show-totals?)
	      (begin
		(add-subtotal-row (_ "Total Debits") leader table used-columns
				  debit-collector "grand-total" #f)
		(add-subtotal-row (_ "Total Credits") leader table used-columns
				  credit-collector "grand-total" #f)
                (add-subtotal-row (_ "Total Value Debits") leader table used-columns
                                  debit-value "grand-total" #t)
                (add-subtotal-row (_ "Total Value Credits") leader table used-columns
                                  credit-value "grand-total" #t)))
          (if ledger-type?
            (add-subtotal-row (_ "Net Change") leader table used-columns
			    total-collector "grand-total" #f)
          )
          (add-subtotal-row (_ "Value Change") leader table used-columns
                            total-value "grand-total" #t))

      ;; ----------------------------------
      ;; process the splits list
      ;; ----------------------------------
        (let* ((current (car splits))
               (current-row-style (if multi-rows? "normal-row"
                                      (if odd-row? "normal-row"
                                          "alternate-row")))
               (rest (cdr splits))
               (next (if (null? rest) #f
                         (car rest)))
               (valid-split? (not (null? (xaccSplitGetAccount current)))))
          ;; ----------------------------------------------
          ;; update totals, but don't add them to the table
          ;; ----------------------------------------------
          (if (and multi-rows? valid-split?)
              (for-each (lambda (split)
                          (if (string=? (gncAccountGetGUID
                                         (xaccSplitGetAccount current))
                                        (gncAccountGetGUID
                                         (xaccSplitGetAccount split)))
                              (accumulate-totals split
                                                 total-collector total-value
                                                 debit-collector debit-value
                                                 credit-collector credit-value)))
                        (xaccTransGetSplitList (xaccSplitGetParent current)))
              (accumulate-totals current
                                 total-collector total-value
                                 debit-collector debit-value
                                 credit-collector credit-value))
          ;; ----------------------------------
          ;; add the splits to the table
          ;; ----------------------------------
          ;; The general journal has a split that doesn't have an account
          ;; set yet (the new entry transaction).
          ;; This split should be skipped or the report errors out.
          ;; See bug #639082
          (if valid-split?
            (add-split-row
              table
              current
              used-columns
              current-row-style
              #t
              (not multi-rows?)
              action-for-num?
              ledger-type?
              double?
              (opt-val "Display" "Memo")
              (opt-val "Display" "Description")
              total-collector
            )
          )
          (if (and multi-rows? valid-split?)
            (add-other-split-rows
              current
              table used-columns
              "alternate-row"
              action-for-num?
              ledger-type?
              total-collector
            )
          )

          (do-rows-with-subtotals leader
                                  rest
                                  table
                                  used-columns
                                  width 
                                  multi-rows?
                                  action-for-num?
                                  ledger-type?
                                  double?
                                  (not odd-row?)                       
                                  total-collector
				  debit-collector
				  credit-collector
                                  total-value
                                  debit-value
                                  credit-value))))
  ;; -----------------------------------------------
  ;; needed for the call to (do-rows-with-subtotals)
  ;; -----------------------------------------------
  (define (splits-leader splits)
    (let ((accounts (map xaccSplitGetAccount splits)))
      (if (null? accounts) '()
          (begin
            (set! accounts (cons (car accounts)
                                 (delete (car accounts) (cdr accounts))))
            (if (not (null? (cdr accounts))) '()
                (car accounts))))))
  ;; ----------------------------------
  ;; make the split table
  ;; ----------------------------------
  (let* ((table (gnc:make-html-table))
         (used-columns (build-column-used options))
         (width (num-columns-required used-columns))
         (multi-rows? (reg-report-journal?))
         (ledger-type? (reg-report-ledger-type?))
         (double? (reg-report-double?))
         (action-for-num? (qof-book-use-split-action-for-num-field
                                                      (gnc-get-current-book))))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns
                        debit-string credit-string amount-string
                        multi-rows? action-for-num? ledger-type?))

    (do-rows-with-subtotals (splits-leader splits)
                            splits
                            table
                            used-columns
                            width
                            multi-rows?
                            action-for-num?
                            ledger-type?
                            double?
                            #t
                            (gnc:make-commodity-collector)
                            (gnc:make-commodity-collector)
                            (gnc:make-commodity-collector)
                            (gnc:make-commodity-collector)
                            (gnc:make-commodity-collector)
                            (gnc:make-commodity-collector))
    table))
;; -----------------------------------------------------------------
;; misc
;; -----------------------------------------------------------------
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

(define (make-client-table address)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 0)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (string-append (_ "Client") ":&nbsp;")
      (string-expand address #\newline "<br>")))
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (make-info-table address)
  (let ((table (gnc:make-html-table)))
    (gnc:html-table-set-style!
     table "table"
     'attribute (list "border" 0)
     'attribute (list "cellspacing" 20)
     'attribute (list "cellpadding" 0))
    (gnc:html-table-append-row!
     table
     (list
      (string-append
       (_ "Date") ":&nbsp;"
       (string-expand (gnc-print-date (cons (current-time) 0))
                      #\space "&nbsp;"))
      (make-client-table address)))
    (set-last-row-style!
     table "td"
     'attribute (list "valign" "top"))
    table))

(define (reg-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let ((document (gnc:make-html-document))
        (splits '())
        (table '())
        (query-scm (opt-val "__reg" "query"))
        (query #f)
        (journal? (opt-val "__reg" "journal"))
        (debit-string (opt-val "__reg" "debit-string"))
        (credit-string (opt-val "__reg" "credit-string"))
        (invoice? #f)
        (title (opt-val "General" "Title")))

    (if invoice?
        (set! title (_ "Invoice")))

    (set! query (gnc-scm2query query-scm))

    (qof-query-set-book query (gnc-get-current-book))

    (set! splits (if journal?
                     (xaccQueryGetSplitsUniqueTrans query)
                     (qof-query-run query)))

    (set! table (make-split-table splits
                                  (gnc:report-options report-obj)
                                  debit-string credit-string
                                  (if invoice? (_ "Charge") (_ "Amount"))))

    (if invoice?
        (begin
          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-br)
            "User Name"
            (gnc:html-markup-br)
            (string-expand
             "User Address"
             #\newline
             "<br>")
            (gnc:html-markup-br)))
          (gnc:html-table-set-style!
           table "table"
           'attribute (list "border" 1)
           'attribute (list "cellspacing" 0)
           'attribute (list "cellpadding" 4))
          (gnc:html-document-add-object!
           document
           (make-info-table
             ""))))
    
    (gnc:html-document-set-title! document title)
    (gnc:html-document-add-object! document table)

    (qof-query-destroy query)

    document))

(define register-report-guid "22104e02654c4adba844ee75a3f8d173")

;; we get called from elsewhere... but this doesn't work FIX-ME, find
;; out how to get report-guid's exported from report into the report
;; system at large. might have to define this at the report-system
;; level to get them read by other reports. Look at the aging reports
;; for suggestions, perhaps
(export register-report-guid)

(gnc:define-report
 'version 1
 'name (N_ "Register")
 'report-guid register-report-guid
 'options-generator options-generator
 'renderer reg-renderer
 'in-menu? #f)

(define (gnc:register-report-create-internal invoice? query journal? ledger-type?
                                             double? title debit-string credit-string)
  (let* ((options (gnc:make-report-options register-report-guid))
         (query-op (gnc:lookup-option options "__reg" "query"))
         (journal-op (gnc:lookup-option options "__reg" "journal"))
         (ledger-type-op (gnc:lookup-option options "__reg" "ledger-type"))
         (double-op (gnc:lookup-option options "__reg" "double"))
         (title-op (gnc:lookup-option options "General" "Title"))
         (debit-op (gnc:lookup-option options "__reg" "debit-string"))
         (credit-op (gnc:lookup-option options "__reg" "credit-string"))
         (account-op (gnc:lookup-option options "Display" "Account")))

    (if invoice?
        (begin
          (set! journal? #f)
          (gnc:option-set-value account-op #f)))

    (gnc:option-set-value query-op query)
    (gnc:option-set-value journal-op journal?)
    (gnc:option-set-value ledger-type-op ledger-type?)
    (gnc:option-set-value double-op double?)
    (gnc:option-set-value title-op title)
    (gnc:option-set-value debit-op debit-string)
    (gnc:option-set-value credit-op credit-string)
    (gnc:make-report register-report-guid options)))

(export gnc:register-report-create-internal)
