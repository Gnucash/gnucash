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

;; creates 8 invoices. (1) customer-invoice (2) customer's job's
;; invoice (3) vendor bill (4) employee bill (5) customer credit-note
;; (6) vendor credit-note (7) employee credit-note (8)
;; customer-invoice with various combinations of entries. in addition,
;; this function will return the vector-list of invoices created.
(define-public (create-test-invoice-data)
  (define USD (mnemonic->commodity "USD"))
  (define structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET)
                       (cons 'commodity USD))
          (list "Asset"
                (list "Bank"))
          (list "VAT"
                (list "VAT-on-Purchases")
                (list "VAT-on-Sales" (list (cons 'type ACCT-TYPE-LIABILITY))))
          (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
          (list "Expense" (list (cons 'type ACCT-TYPE-EXPENSE)))
          (list "A/Receivable" (list (cons 'type ACCT-TYPE-RECEIVABLE)))
          (list "A/Payable" (list (cons 'type ACCT-TYPE-PAYABLE)))))
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expense" account-alist)))
         (vat-sales (cdr (assoc "VAT-on-Sales" account-alist)))
         (vat-purchases (cdr (assoc "VAT-on-Purchases" account-alist)))
         (receivable (cdr (assoc "A/Receivable" account-alist)))
         (payable (cdr (assoc "A/Payable" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today)))

         (cust-1 (let ((cust-1 (gncCustomerCreate (gnc-get-current-book))))
                   (gncCustomerSetID cust-1 "cust-1-id")
                   (gncCustomerSetName cust-1 "cust-1-name")
                   (gncCustomerSetNotes cust-1 "cust-1-notes")
                   (gncCustomerSetCurrency cust-1 USD)
                   (gncCustomerSetTaxIncluded cust-1 1) ;1 = GNC-TAXINCLUDED-YES
                   cust-1))

         (owner-1 (let ((owner-1 (gncOwnerNew)))
                    (gncOwnerInitCustomer owner-1 cust-1)
                    owner-1))

         ;; inv-1 is generated for a customer
         (inv-1 (let ((inv-1 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-1 owner-1)
                  (gncInvoiceSetNotes inv-1 "inv-1-notes")
                  (gncInvoiceSetBillingID inv-1 "inv-1-billing-id")
                  (gncInvoiceSetCurrency inv-1 USD)
                  inv-1))

         (job-1 (let ((job-1 (gncJobCreate (gnc-get-current-book))))
                  (gncJobSetID job-1 "job-1-id")
                  (gncJobSetName job-1 "job-1-name")
                  (gncJobSetOwner job-1 owner-1)
                  job-1))

         (owner-2 (let ((owner-2 (gncOwnerNew)))
                    (gncOwnerInitJob owner-2 job-1)
                    owner-2))

         ;; inv-2 is generated from a customer's job
         (inv-2 (let ((inv-2 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-2 owner-2)
                  (gncInvoiceSetNotes inv-2 "inv-2-notes")
                  (gncInvoiceSetCurrency inv-2 USD)
                  inv-2))

         (vend-1 (let ((vend-1 (gncVendorCreate (gnc-get-current-book))))
                   (gncVendorSetID vend-1 "vend-1-id")
                   (gncVendorSetName vend-1 "vend-1-name")
                   (gncVendorSetNotes vend-1 "vend-1-notes")
                   (gncVendorSetCurrency vend-1 USD)
                   (gncVendorSetTaxIncluded vend-1 1) ;1 = GNC-TAXINCLUDED-YES
                   vend-1))

         (owner-3 (let ((owner-3 (gncOwnerNew)))
                    (gncOwnerInitVendor owner-3 vend-1)
                    owner-3))

         ;; inv-3 is generated from a vendor
         (inv-3 (let ((inv-3 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-3 owner-3)
                  (gncInvoiceSetNotes inv-3 "inv-3-notes")
                  (gncInvoiceSetCurrency inv-3 USD)
                  inv-3))

         (emp-1 (let ((emp-1 (gncEmployeeCreate (gnc-get-current-book))))
                  (gncEmployeeSetID emp-1 "emp-1-id")
                  (gncEmployeeSetCurrency emp-1 USD)
                  (gncEmployeeSetName emp-1 "emp-1-name")
                  emp-1))

         (owner-4 (let ((owner-4 (gncOwnerNew)))
                    (gncOwnerInitEmployee owner-4 emp-1)
                    owner-4))

         ;; inv-4 is generated for an employee
         (inv-4 (let ((inv-4 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-4 owner-4)
                  (gncInvoiceSetNotes inv-4 "inv-4-notes")
                  (gncInvoiceSetCurrency inv-4 USD)
                  inv-4))

         ;; inv-5 cust-credit-note
         (inv-5 (let ((inv-5 (gncInvoiceCopy inv-1)))
                  (gncInvoiceSetIsCreditNote inv-5 #t)
                  (gncInvoiceSetCurrency inv-5 USD)
                  inv-5))

         ;; inv-6 vend-credit-note
         (inv-6 (let ((inv-6 (gncInvoiceCopy inv-3)))
                  (gncInvoiceSetIsCreditNote inv-6 #t)
                  (gncInvoiceSetCurrency inv-6 USD)
                  inv-6))

         ;; inv-7 emp-credit-note
         (inv-7 (let ((inv-7 (gncInvoiceCopy inv-4)))
                  (gncInvoiceSetIsCreditNote inv-7 #t)
                  (gncInvoiceSetCurrency inv-7 USD)
                  inv-7))

         (inv-8 (let ((inv-8 (gncInvoiceCreate (gnc-get-current-book))))
                  (gncInvoiceSetOwner inv-8 owner-1)
                  (gncInvoiceSetCurrency inv-8 USD)
                  inv-8))

         (standard-vat-sales-tt
          (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
            (gncTaxTableIncRef tt)
            (gncTaxTableSetName tt "10% vat on sales")
            (let ((entry (gncTaxTableEntryCreate)))
              (gncTaxTableEntrySetAccount entry vat-sales)
              (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
              (gncTaxTableEntrySetAmount entry 10)
              (gncTaxTableAddEntry tt entry))
            tt))

         (standard-vat-purchases-tt
          (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
            (gncTaxTableIncRef tt)
            (gncTaxTableSetName tt "10% vat on purchases")
            (let ((entry (gncTaxTableEntryCreate)))
              (gncTaxTableEntrySetAccount entry vat-purchases)
              (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
              (gncTaxTableEntrySetAmount entry 10)
              (gncTaxTableAddEntry tt entry))
            tt)))

    ;; entry-1  2 widgets of $3 = $6
    (let ((entry-1 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-1 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-1 "entry-1-desc")
      (gncEntrySetAction entry-1 "entry-1-action")
      (gncEntrySetNotes entry-1 "entry-1-notes")
      (gncEntrySetInvAccount entry-1 income)
      (gncEntrySetDocQuantity entry-1 2 #f)
      (gncEntrySetInvPrice entry-1 3)
      (gncInvoiceAddEntry inv-1 entry-1))

    ;; entry-inv-2  2 widgets of $3 = $6
    (let ((entry-inv-2 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-2 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-2 "entry-inv-2-desc")
      (gncEntrySetAction entry-inv-2 "entry-inv-2-action")
      (gncEntrySetNotes entry-inv-2 "entry-inv-2-notes")
      (gncEntrySetInvAccount entry-inv-2 income)
      (gncEntrySetDocQuantity entry-inv-2 2 #f)
      (gncEntrySetInvPrice entry-inv-2 3)
      (gncInvoiceAddEntry inv-2 entry-inv-2))

    ;; entry-inv-3  2 widgets of $3 = $6
    (let ((entry-inv-3 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-3 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-3 "entry-inv-3-desc")
      (gncEntrySetAction entry-inv-3 "entry-inv-3-action")
      (gncEntrySetNotes entry-inv-3 "entry-inv-3-notes")
      (gncEntrySetBillAccount entry-inv-3 expense)
      (gncEntrySetDocQuantity entry-inv-3 2 #f)
      (gncEntrySetBillPrice entry-inv-3 3)
      (gncInvoiceAddEntry inv-3 entry-inv-3))

    ;; entry-inv-4  2 widgets of $3 = $6
    (let ((entry-inv-4 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-4 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-4 "entry-inv-4-desc")
      (gncEntrySetAction entry-inv-4 "entry-inv-4-action")
      (gncEntrySetNotes entry-inv-4 "entry-inv-4-notes")
      (gncEntrySetBillAccount entry-inv-4 expense)
      (gncEntrySetDocQuantity entry-inv-4 2 #f)
      (gncEntrySetBillPrice entry-inv-4 3)
      (gncInvoiceAddEntry inv-4 entry-inv-4))

    ;; entry-5  2 widgets of $3 = $6
    (let ((entry-5 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-5 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-5 "entry-5-desc")
      (gncEntrySetAction entry-5 "entry-5-action")
      (gncEntrySetNotes entry-5 "entry-5-notes")
      (gncEntrySetInvAccount entry-5 income)
      (gncEntrySetDocQuantity entry-5 2 #t)
      (gncEntrySetInvPrice entry-5 3)
      (gncInvoiceAddEntry inv-5 entry-5))

    (let ((entry-inv-6 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-6 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-6 "entry-inv-6-desc")
      (gncEntrySetAction entry-inv-6 "entry-inv-6-action")
      (gncEntrySetNotes entry-inv-6 "entry-inv-6-notes")
      (gncEntrySetBillAccount entry-inv-6 expense)
      (gncEntrySetDocQuantity entry-inv-6 2 #t)
      (gncEntrySetBillPrice entry-inv-6 3)
      (gncInvoiceAddEntry inv-6 entry-inv-6))

    ;; entry-inv-7  2 widgets of $3 = $6
    (let ((entry-inv-7 (gncEntryCreate (gnc-get-current-book))))
      (gncEntrySetDateGDate entry-inv-7 (time64-to-gdate (current-time)))
      (gncEntrySetDescription entry-inv-7 "entry-inv-7-desc")
      (gncEntrySetAction entry-inv-7 "entry-inv-7-action")
      (gncEntrySetNotes entry-inv-7 "entry-inv-7-notes")
      (gncEntrySetBillAccount entry-inv-7 expense)
      (gncEntrySetDocQuantity entry-inv-7 2 #t)
      (gncEntrySetBillPrice entry-inv-7 3)
      (gncInvoiceAddEntry inv-7 entry-inv-7))

    (gncInvoicePostToAccount inv-1 receivable
                             (gnc-dmy2time64 1 9 1980)
                             (gnc-dmy2time64 1 9 1980)
                             "cust-invoice"
                             #t #f)

    (gncInvoicePostToAccount inv-2 receivable
                             (gnc-dmy2time64 2 9 1980)
                             (gnc-dmy2time64 3 9 1980)
                             "job-invoice"
                             #t #f)

    (gncInvoicePostToAccount inv-3 payable
                             (gnc-dmy2time64 3 9 1980)
                             (gnc-dmy2time64 3 9 1980)
                             "vendor-bill"
                             #t #f)

    (gncInvoicePostToAccount inv-4 payable
                             (gnc-dmy2time64 4 9 1980)
                             (gnc-dmy2time64 4 9 1980)
                             "emp-bill"
                             #t #f)

    (gncInvoicePostToAccount inv-5 receivable
                             (gnc-dmy2time64 5 9 1980)
                             (gnc-dmy2time64 5 9 1980)
                             "cust-credit-note"
                             #t #f)

    (gncInvoicePostToAccount inv-6 payable
                             (gnc-dmy2time64 6 9 1980)
                             (gnc-dmy2time64 6 9 1980)
                             "vend-credit-note"
                             #t #f)

    (gncInvoicePostToAccount inv-7 payable
                             (gnc-dmy2time64 7 9 1980)
                             (gnc-dmy2time64 7 9 1980)
                             "emp-credit-note"
                             #t #f)

    (let* ((taxrate 109/10)
           (discount 7/2)
           (unitprice 777/4)
           (quantity 11)
           (combo-vat-sales-tt
            (let ((tt (gncTaxTableCreate (gnc-get-current-book))))
              (gncTaxTableIncRef tt)
              (gncTaxTableSetName tt (format #f "~a% vat on sales" taxrate))
              (let ((entry (gncTaxTableEntryCreate)))
                (gncTaxTableEntrySetAccount entry vat-sales)
                (gncTaxTableEntrySetType entry GNC-AMT-TYPE-PERCENT)
                (gncTaxTableEntrySetAmount entry taxrate)
                (gncTaxTableAddEntry tt entry))
              tt))
           (order (let ((order (gncOrderCreate (gnc-get-current-book))))
                    (gncOrderSetID order "order-id")
                    (gncOrderSetOwner order owner-1)
                    (gncOrderSetReference order "order-ref")
                    (gncOrderSetActive order #t)
                    order))
           (billterm (let ((term (gncBillTermCreate (gnc-get-current-book))))
                       (gncBillTermSetName term "billterm-name")
                       (gncBillTermSetDescription term "billterm-desc")
                       (gncBillTermSetType term 1) ;1 = GNC-TERM-TYPE-DAYS
                       (gncBillTermSetDueDays term 8)
                       term)))
      (gncInvoiceSetTerms inv-8 billterm)
      (for-each
       (lambda (combo)
         (let* ((each-entry (gncEntryCreate (gnc-get-current-book)))
                (taxable? (= (vector-ref combo 0) 1))
                (tax-included? (= (vector-ref combo 1) 1))
                (discount-type (vector-ref combo 2))
                (discount-how (vector-ref combo 3))
                (desc (format #f "taxable=~a tax-included=~a discount-type=~a discount-how=~a"
                              (if taxable? "Y" "N")
                              (if tax-included? "Y" "N")
                              (gncAmountTypeToString discount-type)
                              (gncEntryDiscountHowToString discount-how))))
           (gncEntrySetDateGDate each-entry (time64-to-gdate (current-time)))
           (gncEntrySetDescription each-entry desc)
           (gncEntrySetAction each-entry "action")
           (gncEntrySetInvAccount each-entry income)
           (gncEntrySetDocQuantity each-entry quantity #f)
           (gncEntrySetInvPrice each-entry unitprice)
           (gncEntrySetInvDiscount each-entry discount)
           (gncEntrySetInvDiscountType each-entry discount-type)
           (gncEntrySetInvDiscountHow each-entry discount-how)
           (gncEntrySetInvTaxable each-entry taxable?)
           (gncEntrySetInvTaxIncluded each-entry tax-included?)
           (gncEntrySetInvTaxTable each-entry combo-vat-sales-tt)
           (gncOrderAddEntry order each-entry)
           ;; FIXME: Note: The following function hides a subtle
           ;; bug. It aims to retrieve & dump the entry description
           ;; and amount. Unfortunately the (gncEntryGetDocValue)
           ;; function will subtly modify the entry amounts by a
           ;; fraction; this means that the subsequent invoice payment
           ;; will not make the invoice amount completely zero. If the
           ;; following statement is uncommented, test-invoice will
           ;; fail because the (gncInvoiceIsPaid) final test will
           ;; fail.
           ;; (format #t "inv-8: adding ~a to invoice, entry amount is ~a\n"
           ;;         desc
           ;;         (exact->inexact (gncEntryGetDocValue each-entry #f #t #f)))
           (gncInvoiceAddEntry inv-8 each-entry)))
       (list
        ;; the following list specifies combinations to test gncEntry options
        ;; thanks to rgmerk and to jenny for idea how to generate list of options
        ;; (vector Taxable?(1=#t) Tax-included?(1=#t) DiscountType DiscountHow)
        (vector 1 2 1 1)
        (vector 2 1 2 2)
        (vector 1 1 2 3)
        (vector 2 2 1 3)
        (vector 2 1 1 1)
        (vector 1 2 2 2)
        (vector 1 2 1 2)
        (vector 1 1 2 1)))

      (gncInvoiceSetNotes
       inv-8 (format #f "tax=~a%, discount=~a, qty=~a, price=~a"
                     taxrate discount quantity unitprice))

      (gncInvoicePostToAccount inv-8 receivable
                               (gnc-dmy2time64 8 9 1980)
                               (gnc-dmy2time64 8 9 1980)
                               "trans-posting-memo"
                               #t #f)

      (gncInvoiceApplyPayment inv-8 '() bank 1747918/100 1
                              (gnc-dmy2time64 10 9 1980)
                              "trans-payment-memo-1"
                              "trans-payment-num-1"))

    (vector inv-1 inv-2 inv-3 inv-4 inv-5 inv-6 inv-7 inv-8)))

(define-public (gnc:create-budget-and-transactions env account-alist)
  (let* ((book (gnc-get-current-book))
         (budget (gnc-budget-new book))
         (bank (cdr (assoc "Bank" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist))))
    (gnc-budget-set-name budget "test budget")
    (gnc-budget-begin-edit budget)
    (gnc-budget-set-num-periods budget 6)
    (gnc-budget-set-account-period-value budget bank 0 20)
    (gnc-budget-set-account-period-value budget bank 1 40)
    (gnc-budget-set-account-period-value budget bank 3 60)
    (gnc-budget-set-account-period-value budget expense 1 30)
    (gnc-budget-set-account-period-value budget expense 2 20)
    (gnc-budget-set-account-period-value budget expense 3 40)
    (gnc-budget-set-account-period-value budget income 0 -55)
    (gnc-budget-set-account-period-value budget income 2 -65)
    (gnc-budget-set-account-period-value budget income 3 -75)
    (gnc-budget-commit-edit budget)
    (let ((midperiod (lambda (period)
                       (floor (/ (+ (gnc-budget-get-period-start-date budget period)
                                    (gnc-budget-get-period-end-date budget period))
                                 2)))))
      (env-create-transaction env (midperiod 0) bank income 55)
      (env-create-transaction env (midperiod 2) bank income 67)
      (env-create-transaction env (midperiod 3) bank income 77)
      (env-create-transaction env (midperiod 0) expense bank 20)
      (env-create-transaction env (midperiod 1) expense bank 20)
      (let ((clos (env-create-transaction env (midperiod 1) income equity 55)))
        (xaccTransSetIsClosingTxn clos #t)))
    budget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various stock transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function aims to replicate the stock-split process in
;; gnc_stock_split_assistant_finish in assistant-stock-split.c. It
;; creates a 1 or 3-split transaction, and possibly a pricedb entry.
(define (stock-split account date shares description
                     ;; price-amount may be #f
                     price-amount pricecurrency
                     ;; cash-in-lieu, cash-amount may be #f
                     cash-amount cash-memo cash-income cash-asset)
  (let* ((book (gnc-get-current-book))
         (accounts '())
         (trans (xaccMallocTransaction book)))
    (xaccTransBeginEdit trans)
    (xaccTransSetCurrency trans (gnc-default-currency))
    (xaccTransSetDatePostedSecsNormalized trans date)
    (xaccTransSetDescription trans description)

    (let ((stocksplit (xaccMallocSplit book)))
      (xaccAccountBeginEdit account)
      (set! accounts (cons account accounts))
      (xaccSplitSetAccount stocksplit account)
      (xaccSplitSetAmount stocksplit shares)
      (xaccSplitMakeStockSplit stocksplit)
      (xaccSplitSetAction stocksplit "Split")
      (xaccSplitSetParent stocksplit trans))

    ;; add pricedb
    (when price-amount
      (let ((price (gnc-price-create book)))
        (gnc-price-begin-edit price)
        (gnc-price-set-commodity price (xaccAccountGetCommodity account))
        (gnc-price-set-currency price pricecurrency)
        (gnc-price-set-time64 price date)
        (gnc-price-set-source price PRICE-SOURCE-STOCK-SPLIT)
        (gnc-price-set-typestr price "unknown")
        (gnc-price-set-value price price-amount)
        (gnc-price-commit-edit price)
        (gnc-pricedb-add-price (gnc-pricedb-get-db book) price)))

    ;; cash-in-lieu
    (when cash-amount
      (let ((asset-split (xaccMallocSplit book)))
        (xaccAccountBeginEdit cash-asset)
        (set! accounts (cons cash-asset accounts))
        (xaccSplitSetAccount asset-split cash-asset)
        (xaccSplitSetParent asset-split trans)
        (xaccSplitSetAmount asset-split cash-amount)
        (xaccSplitSetValue asset-split cash-amount)
        (xaccSplitSetMemo asset-split cash-memo))

      (let ((income-split (xaccMallocSplit book)))
        (xaccAccountBeginEdit cash-income)
        (set! accounts (cons cash-income accounts))
        (xaccSplitSetAccount income-split cash-income)
        (xaccSplitSetParent income-split trans)
        (xaccSplitSetAmount income-split (- cash-amount))
        (xaccSplitSetValue income-split (- cash-amount))
        (xaccSplitSetMemo income-split cash-memo)))

    (xaccTransCommitEdit trans)
    (for-each xaccAccountCommitEdit accounts)
    trans))

(define-public (create-stock-test-data)
  (define structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
          (list "Asset"
                (list "Bank"))
          (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
          (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
          (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))
          (list "Broker"
                (list "AAPL" (list (cons 'type ACCT-TYPE-STOCK)))
                (list "MSFT" (list (cons 'type ACCT-TYPE-STOCK)))
                (list "TSLA" (list (cons 'type ACCT-TYPE-STOCK))))))
  (let* ((env (create-test-env))
         (book (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (MSFT (gnc-commodity-new book "Microsoft" "NASDAQ" "MSFT" "" 1))
         (TSLA (gnc-commodity-new book "Tesla Motors" "NASDAQ" "TSLA" "" 1))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (inco (cdr (assoc "Income" account-alist)))
         (expe (cdr (assoc "Expenses" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (aapl (cdr (assoc "AAPL" account-alist)))
         (msft (cdr (assoc "MSFT" account-alist)))
         (tsla (cdr (assoc "TSLA" account-alist)))
         (YEAR (gnc:time64-get-year (gnc:get-today))))

    ;; Set account commodities
    (gnc-commodity-table-insert comm-table AAPL)
    (gnc-commodity-table-insert comm-table MSFT)
    (gnc-commodity-table-insert comm-table TSLA)
    (xaccAccountSetCommodity aapl AAPL)
    (xaccAccountSetCommodity msft MSFT)
    (xaccAccountSetCommodity tsla TSLA)

    (env-transfer env 01 01 1980 equity bank 10000 #:description "seed money")

    (env-create-multisplit-transaction
     env 01 02 1980
     (list (vector bank  -100 -100)
           (vector aapl   100    1))
     #:description "buy 1 AAPL @ $100")

    (env-create-multisplit-transaction
     env 01 03 1980
     (list (vector bank  -200 -200)
           (vector aapl   200    1))
     #:description "buy 1 AAPL @ $200")

    (env-create-multisplit-transaction
     env 01 05 1980
     (list (vector bank   390  390)
           (vector aapl  -400   -1)
           (vector inco  -300 -300)
           (vector expe    10   10)
           (vector aapl   300    0))
     #:description "sell 1 AAPL @ $400 FIFO, brokerage fee = $10, into bank = $390")

    ;; until 1.5.1980 the account has usual buy/sell txns only, no stock splits
    ;; there's only 1 AAPL left, price $400

    ;; on 1.10.1980: stock split, 1 AAPL -> 10 AAPL
    ;; prev price was $400, now is $40
    (stock-split aapl
                 (gnc-dmy2time64 1 10 1980)
                 9 "first 1:10 stock split"
                 40 (gnc-account-get-currency-or-parent aapl)
                 #f #f #f #f)

    ;; on 1.11.1980: another stock split, 10 AAPL -> 100 AAPL
    ;; prev price was $40, now is $4
    (stock-split aapl
                 (gnc-dmy2time64 1 11 1980)
                 90 "another 1:10 stock split"
                 4 (gnc-account-get-currency-or-parent aapl)
                 #f #f #f #f)

    ;; on 1.12.1980: 3:1 stock split, 100 AAPL -> 33 AAPL
    ;; prev price was $4, now is $12, with cash-in-lieu $4
    (stock-split aapl
                 (gnc-dmy2time64 1 12 1980)
                 -67 "3:1 stock split with cash-in-lieu $4"
                 12 (gnc-account-get-currency-or-parent aapl)
                 4 "cash-in-lieu" inco bank)

    (env-create-multisplit-transaction
     env 01 01 1981
     (list (vector bank -500 -500)
           (vector aapl  500   10))
     #:description "buy 10 AAPL @ $5")

    (env-create-multisplit-transaction
     env 1 3 1981
     (list (vector bank     3    3)
           (vector aapl    -3 -1/2)
           (vector inco  -5/2 -5/2)
           (vector aapl   5/2    0))
     #:description "sell 1/2 AAPL @ $6 FIFO, capgain = $2.50 into bank = $200")

    ;; FIXME: spin off $150 from AAPL is coded correctly? there's no
    ;; INCOME split?
    (env-create-multisplit-transaction
     env 1 4 1981
     (list (vector bank   150  150)
           (vector aapl  -150    0))
     #:description "spin-off $150")

    account-alist))

