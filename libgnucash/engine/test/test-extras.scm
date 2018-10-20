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
  (let ((txn (xaccMallocTransaction (gnc-get-current-book)))
	(split-1 (xaccMallocSplit  (gnc-get-current-book)))
	(split-2 (xaccMallocSplit  (gnc-get-current-book))))
    (xaccTransBeginEdit txn)
    (xaccTransSetDescription txn (or description "ponies"))
    (xaccTransSetCurrency txn (xaccAccountGetCommodity debit))
    (xaccTransSetDate txn DD MM YY)
    (xaccSplitSetParent split-1 txn)
    (xaccSplitSetParent split-2 txn)
    (xaccSplitSetAccount split-1 debit)
    (xaccSplitSetAccount split-2 credit)
    (xaccSplitSetValue split-1 (- amount1))
    (xaccSplitSetValue split-2 amount1)
    (xaccSplitSetAmount split-1 (- amount1))
    (xaccSplitSetAmount split-2 amount2)
    (if reconcile
        (begin
          (xaccSplitSetReconcile split-1 (car reconcile))
          (xaccSplitSetReconcile split-2 (car reconcile))
          (xaccSplitSetDateReconciledSecs split-1 (cdr reconcile))
          (xaccSplitSetDateReconciledSecs split-2 (cdr reconcile))))
    (if num
        (begin
          (gnc-set-num-action txn split-1 num num)
          (gnc-set-num-action txn split-2 num num)))
    (if void-reason (xaccTransVoid txn void-reason))
    (if notes (xaccTransSetNotes txn notes))
    (if memo
        (begin
          (xaccSplitSetMemo split-1 memo)
          (xaccSplitSetMemo split-2 memo)))
    (if (> amount2 0)
        (gnc-pricedb-create (xaccAccountGetCommodity debit)
                        (xaccAccountGetCommodity credit)
                        (gnc-dmy2time64 DD MM YY)
                        (/ amount1 amount2)))
    (xaccTransCommitEdit txn)
    txn))

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
          memo             ; string: memo        (def = null)
          )
  (env-transfer-foreign
   env DD MM YY debit credit amount amount
   #:description description
   #:void-reason void-reason
   #:reconcile reconcile
   #:num num
   #:memo memo
   #:notes notes))

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
  (env-create-account-structure-alist env
				      (list "Root"
					    (list (cons 'type ACCT-TYPE-ASSET))
					    (list "Bank")
					    (list "Wallet")
					    (list "Other")
					    (list "Expenses"
						  (list (cons 'type ACCT-TYPE-EXPENSE))))))


