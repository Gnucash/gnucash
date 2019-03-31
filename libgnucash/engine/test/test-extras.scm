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

(define-module (gnucash engine test test-extras))

(use-modules (gnucash gnc-module))

(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (srfi srfi-1))
(use-modules (sw_app_utils))
(use-modules (sw_engine))

(export test)

(export with-account)
(export with-transaction)

(export create-test-env)
(export env-random-amount)
(export env-random)
(export env-counter-next)
(export env-string)
(export env-select-price-source)
(export env-any-date)
(export env-transfer)
(export env-transfer-foreign)
(export env-create-multisplit-transaction)
(export env-create-transaction)
(export env-create-account)
(export env-create-root-account)
(export env-create-test-accounts)
(export env-create-daily-transactions)
(export env-create-account-structure)
(export env-create-account-structure-alist)
(export env-expense-account-structure)
(export gnc-pricedb-create)

;;
;; Random test related syntax and the like
;;

(define (test the-test)
  (format #t "(Running ~a " the-test)
  (let ((result (the-test)))
    (format #t "~a Completed)\n" result)
    result))

;;
;; Gnucash specifics
;;

;; Really could do with generalising and making into a 'with' macro
(define (with-account account fn)
  (begin (xaccAccountBeginEdit account)
	 (let ((result (fn)))
	   (xaccAccountCommitEdit account)
	   result)))

(define (with-accounts accounts fn)
  (begin (map xaccAccountBeginEdit accounts)
	 (let ((result (fn)))
	   (map xaccAccountCommitEdit accounts)
	   result)))

(define (with-transaction txn fn)
  (begin (xaccTransBeginEdit txn)
	 (let ((result (fn)))
	   (xaccTransCommitEdit txn)
	   result)))

;; Test environments.. an environment is just an alist with some well
;; known names.  The idea is that we can use it to pass around
;; "suitable" sets of values for things

(define (make-counter)
  (let ((x 0))
    (lambda ()
      (begin (set! x (+ x 1))
	     x))))

(define (create-test-env)
  (list (cons 'random (seed->random-state (random 1000)))
	(cons 'counter (make-counter))))

(define (env-random-amount env n)
  (/ (env-random env n) 1))

(define (env-random env n)
  (random n (assoc-ref env 'random)))

(define (env-counter-next env)
  ((assoc-ref env 'counter)))

(define (env-string env prefix)
  (format #f "~a-~a" prefix (env-counter-next env)))

(define (env-select-price-source env)
  'pricedb-nearest)

(define (env-any-date env) (gnc:get-today))

(define (env-create-transaction env date credit debit aaa)
  (let ((txn (xaccMallocTransaction (gnc-get-current-book)))
	(split-1 (xaccMallocSplit  (gnc-get-current-book)))
	(split-2 (xaccMallocSplit  (gnc-get-current-book)))
	(datevec (gnc-localtime date)))
    (with-transaction txn
		      (lambda ()
			(xaccTransSetDescription txn (env-string env "ponies"))
			(xaccTransSetCurrency txn (gnc-default-report-currency))
			(xaccTransSetDate txn
					  (gnc:date-get-month-day datevec)
					  (gnc:date-get-month datevec)
					  (gnc:date-get-year datevec))
			(xaccSplitSetParent split-1 txn)
			(xaccSplitSetParent split-2 txn)
			(xaccSplitSetAccount split-1 credit)
			(xaccSplitSetAccount split-2 debit)
			(xaccSplitSetAmount split-1 aaa)
			(xaccSplitSetAmount split-2 (gnc-numeric-neg aaa))
			(xaccSplitSetValue split-1 aaa)
			(xaccSplitSetValue split-2 (gnc-numeric-neg aaa))
			))
    ;(format #t "tx ~a\n" (map xaccSplitGetAmount (list split-1 split-2)))
    ;(format #t "tx ~a\n" (map xaccSplitGetValue (list split-1 split-2)))
    txn))

(define (gnc-pricedb-create currency commodity time64 value)
  ;; does not check for pre-existing pricedb data on date
  (unless (gnc-commodity-equiv currency commodity)
    (let ((price (gnc-price-create (gnc-get-current-book)))
          (pricedb (gnc-pricedb-get-db (gnc-get-current-book))))
      (gnc-price-begin-edit price)
      (gnc-price-set-commodity price commodity)
      (gnc-price-set-currency price currency)
      (gnc-price-set-time64 price time64)
      (gnc-price-set-source price PRICE-SOURCE-XFER-DLG-VAL)
      (gnc-price-set-source-string price "test-price")
      (gnc-price-set-typestr price "test")
      (gnc-price-set-value price value)
      (gnc-price-commit-edit price)
      (gnc-pricedb-add-price pricedb price))))

;; When creating stock transactions always put the stock account and the number
;; of shares second, using negative numbers for a sale. e.g., to buy 100 shares
;; of IBM:
;;    (env-transfer-foreign env 15 01 2012 cash-a ibm-a 3583200/100 200
;;                          #:description "Buy IBM 200") ;;200 @ $179.16
;; and to sell them:
;;    (env-transfer-foreign env 8 8 2014 cash-a ibm-a -3732600/100 -200
;;                          #:description "Sell IBM 200") ;;-200 @ $186.63
;;    (env-transfer-foreign env 8 8 2014 capgain ibm-a 149400/100 0
;;                          #:description "IBM 200 G/L")

(define* (env-transfer-foreign
          env
          DD MM YY         ; day/month/year
          debit            ; account-from
          credit           ; account-to
          amount1          ; amount-from
          amount2          ; amount-to
          #:key            ; - the following are optional -
          description      ; string: description (def = "ponies")
          void-reason      ; string: void-reason (def = not-voided)
          reconcile        ; pair  : (cons reconciled reconciled-date)
          num              ; string: num field   (def = null)
          notes            ; string: notes       (def = null)
          memo             ; string: memo        (def = null)
          )
  (env-create-multisplit-transaction
   env
   DD MM YY
   (list (vector debit (- amount1) (- amount1))
         (vector credit amount1 amount2))
   #:description description
   #:void-reason void-reason
   #:reconcile reconcile
   #:num num
   #:memo memo
   #:notes notes))

(define* (env-transfer
          env
          DD MM YY         ; day/month/year
          debit            ; account-from
          credit           ; account-to
          amount           ; amount
          #:key            ; - the following are optional -
          description      ; string: description (def = "ponies")
          void-reason      ; string: void-reason (def = not-voided)
          reconcile        ; char  : reconciled  (default = n)
          num              ; string: num field   (def = null)
          notes            ; string: notes       (def = null)
          memo)            ; string: memo        (def = null)
  (env-create-multisplit-transaction
   env
   DD MM YY
   (list
    (vector debit (- amount) (- amount))
    (vector credit amount amount))
   #:description description
   #:void-reason void-reason
   #:reconcile reconcile
   #:num num
   #:memo memo
   #:notes notes))

;; creates multisplit transaction.
;;
;; input: DD/MM/YY - posting date of transaction
;;
;;        list-of-splits, a list of vectors, whose components are
;;        (vector account value amount). The total of sum of values
;;        must be zero, otherwise an imbalance split will be
;;        created. It must contain at least 1 split. The transaction
;;        currency is set as currency of the first split.
;;
;; returns: the transaction created, or #f
(define* (env-create-multisplit-transaction
          env DD MM YY list-of-splits
          #:key            ; - the following are optional -
          description      ; string: description (def = "ponies")
          (pricedb? #t)    ; boolean: add pricedb entry?
          void-reason      ; string: void-reason (def = not-voided)
          reconcile        ; pair  : (cons reconciled reconciled-date)
          num              ; string: num field   (def = null)
          notes            ; string: notes       (def = null)
          memo)            ; string: memo        (def = null)
  (and (pair? list-of-splits)
       (let* ((book (gnc-get-current-book))
              (txn (xaccMallocTransaction book))
              (first-split (vector-ref (car list-of-splits) 0)))
         (xaccTransBeginEdit txn)
         (xaccTransSetDescription txn (or description (env-string env "ponies")))
         (xaccTransSetCurrency txn (xaccAccountGetCommodity first-split))
         (xaccTransSetDate txn DD MM YY)
         (for-each
          (lambda (split)
            (let ((acc (vector-ref split 0))
                  (val (vector-ref split 1))
                  (amt (vector-ref split 2))
                  (newsplit (xaccMallocSplit book)))
              (xaccSplitSetParent newsplit txn)
              (xaccSplitSetAccount newsplit acc)
              (xaccSplitSetValue newsplit val)
              (xaccSplitSetAmount newsplit amt)
              (if num (gnc-set-num-action txn newsplit num num))
              (if memo (xaccSplitSetMemo newsplit memo))
              (when reconcile
                (xaccSplitSetReconcile newsplit (car reconcile))
                (xaccSplitSetDateReconciledSecs newsplit (cdr reconcile)))
              (if (and pricedb?
                       (not (zero? amt))
                       (not (gnc-commodity-equiv
                             (xaccAccountGetCommodity first-split)
                             (xaccAccountGetCommodity acc))))
                  (gnc-pricedb-create (xaccAccountGetCommodity first-split)
                                      (xaccAccountGetCommodity acc)
                                      (gnc-dmy2time64 DD MM YY)
                                      (/ val amt)))))
          list-of-splits)
         (if void-reason (xaccTransVoid txn void-reason))
         (if notes (xaccTransSetNotes txn notes))
         (xaccTransCommitEdit txn)
         txn)))

(define (env-create-root-account env type commodity)
  (env-create-account env type commodity (gnc-get-current-root-account)))

(define (env-create-account env type commodity parent-account)
  (let ((new-account (xaccMallocAccount (gnc-get-current-book))))
    (with-accounts (list new-account parent-account)
		  (lambda ()
		    (xaccAccountSetCommodity new-account commodity)
		    (xaccAccountSetName new-account (env-string env "account"))
		    (xaccAccountSetType new-account type)
		    (gnc-account-append-child parent-account new-account)
		    new-account))))

;; Spend '1' on the 1st, '2' on the 2nd, etc.  Makes for pretty graphs
(define (env-create-daily-transactions env start-date end-date to-account from-account)
  (let ((dates-this-month (gnc:make-date-list start-date
					      end-date
					      DayDelta)))
      (for-each (lambda (date)
		  (env-create-transaction env date to-account
					  from-account
					  (/
					   (gnc:date-get-month-day (gnc-localtime date))
					   1)))
		(cdr (reverse dates-this-month)))))

(define (env-create-account-structure env account-structure)
  (define (lookup-options list)
    (if (null? list) (cons '() '())
	(if (not (pair? (car list)))
	    (cons '() list)
	    (if (not (pair? (car (car list))))
		(cons '() list)
		list))))

  (define (create-substructure parent options account-structure)
    ;;(format #t "Creating subaccounts for ~a ~a\n"
    ;; (xaccAccountGetName parent) account-structure)
    (let* ((account-name (car account-structure))
	   (options-pair (lookup-options (cdr account-structure)))
	   (options (append (car options-pair) options)))
      ;;(format #t "New Account ~a\n" account-name)
      ;;(format #t "Options ~a\n" (car options-pair))
      ;;(format #t "Child list ~a\n" (cdr options-pair))
      (let ((new-account (env-create-account env (assoc-ref options 'type)
					     (assoc-ref options 'commodity)
					     parent)))
	(with-accounts (list new-account)
		       (lambda ()
			 (xaccAccountSetName new-account account-name)))

	(cons new-account
	      (map (lambda (child)
		     (create-substructure new-account options child))
		   (cdr options-pair))))))
  (let ((options (list (cons 'commodity (gnc-default-report-currency))
		       (cons 'type '()))))
    (create-substructure (gnc-get-current-root-account)
			 options
			 account-structure)))

(define (env-create-account-structure-alist env account-structure)
  (let ((accounts (env-create-account-structure env account-structure)))
    (define (flatten l)
      (if (null? l) '()
	  (if (not (pair? l)) (list l)
	      (append (flatten (car l)) (flatten (cdr l))))))
    (map (lambda (acct) (cons (xaccAccountGetName acct) acct))
	 (flatten accounts))))

(define (env-expense-account-structure env)
  (env-create-account-structure
   env
   (list "Expenses"
	 (list (cons 'type ACCT-TYPE-EXPENSE))
	 (list "Groceries")
	 (list "Rent")
	 (list "Auto"
	       (list "Tax")
	       (list "Parking")
	       (list "Petrol")))))

(define (env-create-test-accounts env)
  (env-create-account-structure-alist
   env
   (list "Root"
	 (list (cons 'type ACCT-TYPE-ASSET))
	 (list "Bank")
	 (list "Wallet")
	 (list "Other")
	 (list "Expenses"
	       (list (cons 'type ACCT-TYPE-EXPENSE))))))

(define (mnemonic->commodity sym)
    (gnc-commodity-table-lookup
     (gnc-commodity-table-get-table (gnc-get-current-book))
     (gnc-commodity-get-namespace (gnc-default-report-currency))
     sym))

(define-public (create-test-data)
  (let* ((env (create-test-env))
         (GBP (mnemonic->commodity "GBP"))
         (structure
          (list "Root" (list
                        (cons 'type ACCT-TYPE-ASSET))
                (list "Asset"
                      (list "Bank")
                      (list "GBP Bank" (list
                                        (cons 'commodity GBP)))
                      (list "Wallet"))
                (list "Income" (list
                                (cons 'type ACCT-TYPE-INCOME)))
                (list "Income-GBP" (list
                                    (cons 'type ACCT-TYPE-INCOME)
                                    (cons 'commodity GBP)))
                (list "Expenses" (list
                                  (cons 'type ACCT-TYPE-EXPENSE)))
                (list "Liabilities" (list (cons 'type ACCT-TYPE-LIABILITY)))
                (list "Equity" (list
                                (cons 'type ACCT-TYPE-EQUITY)))))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (gbp-bank (cdr (assoc "GBP Bank" account-alist)))
         (wallet (cdr (assoc "Wallet" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (gbp-income (cdr (assoc "Income-GBP" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (liability (cdr (assoc "Liabilities" account-alist)))
         (equity (cdr (assoc "Equity" account-alist))))

    ;; populate datafile with old transactions
    (env-transfer env 01 01 1970 bank expense       5
                  #:description "desc-1" #:num "trn1"
                  #:memo "memo-3")

    (env-transfer env 31 12 1969 income bank       10
                  #:description "desc-2" #:num "trn2"
                  #:void-reason "void" #:notes "notes3")

    (env-transfer env 31 12 1969 income bank       29
                  #:description "desc-3" #:num "trn3"
                  #:reconcile (cons #\c (gnc-dmy2time64 01 03 1970)))

    (env-transfer env 01 02 1970 bank expense      15
                  #:description "desc-4" #:num "trn4"
                  #:notes "notes2" #:memo "memo-1")

    (env-transfer env 10 01 1970 liability expense 10
                  #:description "desc-5" #:num "trn5"
                  #:void-reason "any")

    (env-transfer env 10 01 1970 liability expense 11
                  #:description "desc-6" #:num "trn6"
                  #:notes "notes1")

    (env-transfer env 10 02 1970 bank liability     8
                  #:description "desc-7" #:num "trn7"
                  #:notes "notes1" #:memo "memo-2"
                  #:reconcile (cons #\y (gnc-dmy2time64 01 03 1970)))

    (env-create-multisplit-transaction
     env 14 02 1971
     (list (vector bank  -100 -100)
           (vector expense 80   80)
           (vector wallet  20   20))
     #:description "$100bank -> $80expenses + $20wallet"
     #:notes "multisplit")

    (let ((closing-txn (env-transfer
                        env 31 12 1977 expense equity
                        111 #:description "Closing")))
      (xaccTransSetIsClosingTxn closing-txn #t))

    (env-transfer-foreign env 15 01 2000 gbp-bank bank
                          10 14 #:description "GBP 10 to USD 14")

    (env-transfer-foreign env 15 02 2000 bank gbp-bank
                          9  6 #:description "USD 9 to GBP 6")

    (for-each
     (lambda (m)
       (env-transfer env 08 (1+ m) 1978
                     gbp-income gbp-bank 51 #:description "#51 income")
       (env-transfer env 03 (1+ m) 1978
                     income bank  103 #:description "$103 income")
       (env-transfer env 15 (1+ m) 1978
                     bank expense  22 #:description "$22 expense")
       (env-transfer env 09 (1+ m) 1978
                     income bank  109 #:description "$109 income"))
     (iota 12))
    account-alist))
