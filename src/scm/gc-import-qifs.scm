;;; $Id$
(display "Started gc-impor.scm")
(newline)
(define (gnc:get-account-list account-group)
  (if testing?
      gc-accts
      (let ((fullacclist 
	     (flatten 
	      (gnc:group-map-accounts get-names-of-accounts
				      account-group))))
	(display "acclist:")
	(display fullacclist)
	(newline)
	(filteroutnulls fullacclist))))

(define gnc:account-types (initialize-lookup))

(define (account-type->number symbol)
  (let
      ((s (lookup symbol gnc:account-types)))
    (if s
	(cdr s)
	#f)))

(display (account-type->number 'INCOME)) (newline)
(define (gnc:get-incomes-list account-group)
  (if testing?
      gc-cats
      (filteroutnulls
       (flatten 
	(gnc:group-map-accounts 
	 get-names-of-incomes
	 account-group)))))

(define gnc-asset-account-types
  '(0 1 2 3 4 7))
;  (map account-type->number 
;       '(CASH CREDIT ASSET LIABILITY CURRENCY)))

(if testing?
    (begin
      (display "gnc-asset-account-types:") 
      (display gnc-asset-account-types)
      (newline)))
;;; '(1 2 3 4 7))
;;;;;;;;;;;;;;;;;;;;;;; add, eventually, 11 12 13 14))
;;;                  aka CHECKING SAVINGS MONEYMRKT CREDITLINE))
;(define gnc-income-account-types '(8 9))
(define gnc-income-account-types 
  (map account-type->number '(INCOME EXPENSE)))

(if testing?
    (begin
      (display "gnc-income-account-types:")
      (display gnc-income-account-types)
      (newline)))

(define gnc-invest-account-types '(5 6 10))

(define gnc-invest-account-types 
  (map account-type->number '(EQUITY STOCK MUTUAL)))

(if testing?
    (begin
      (display "gnc-invest-account-types:")
      (display gnc-invest-account-types)
      (newline)))

(define (get-names-of-accounts a)
  (list
   (if (member (gnc:account-get-type a) gnc-asset-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-accounts
			  (gnc:account-get-children a)))

(define (get-names-of-incomes a)
  (list
   (if (member (gnc:account-get-type a) gnc-income-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-incomes
			  (gnc:account-get-children a)))

(define (get-names-of-expenses a)
  (list
   (if (member (gnc:account-get-type a) gnc-expense-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-expenses
			  (gnc:account-get-children a)))

(define (gnc:import-file-into-account-group account-group)
  ;(sample-dialog)
  (let ((file-name 
	 (gnc:file-selection-dialog "Select file for QIF import" "*.qif")))
    (if file-name
	(begin
	  (gnc:debug "Loading data from file " file-name)
	  (let* ((txn-list (read-qif-file file-name account-group))
		 (category-analysis (analyze-qif-transaction-categories txn-list)))
	    ;;; Now, take steps:
	    (qif-to-gnucash txn-list file-name)
	    (list txn-list category-analysis))))))

;;; Set up QIF Category

(define (get-all-types)
  (set! gnc:account-types (initialize-lookup))
  (let loop 
      ((i 0))
    (let ((typesymbol (gnc:account-type->symbol i)))
      (set! gnc:account-types
	    (lookup-set! gnc:account-types typesymbol i))
      (if (< i 14)
	  (loop (+ i 1))))))

(define (gnc:create-account AccPtr  name description notes type)
  (gnc:init-account AccPtr)
  (gnc:account-begin-edit AccPtr 0)
  (gnc:account-set-name AccPtr name)
  (gnc:account-set-description AccPtr description)
  (gnc:account-set-notes AccPtr notes)
  (gnc:account-set-type AccPtr type)
  (gnc:account-commit-edit AccPtr))

;;;;;;;;;;;  This one's REAL IMPORTANT!!! ;;;;;;;;;;;;
(display (account-type->number 'CASH))
(display (account-type->number 'INCOME))

(define (gnc:create-transaction Account txnlist)
  (define (associt type)
    (let
	((result (lookup type txnlist)))
      (if result
	  (cdr result)
	  #f)))
  (let
      ((Txn (gnc:transaction-create))
       (Category  (associt 'category)) 
       (Payee  (associt 'payee)) 
       (Id  (associt 'id)) 
       (Date  (associt 'date)) 
       (Status  (associt 'status)) 
       (Amount  (associt 'amount)) 
       (Memo  (associt 'memo)) 
       (Splits  (associt 'splits)))
    (gnc:trans-begin-edit Txn 1)
    (let ((source-split (gnc:transaction-get-split Txn 0))
	  (build-split-entry
	   (lambda (splitentry)
	     (define (assocsplit type)
	       (let
		   ((result (assoc type splitentry)))
		 (if result
		     (cdr result)
		     #f)))
	     (let
		 ((Split (gnc:split-create))
		  (Category (assocsplit 'category))
		  (Amount   (assocsplit 'amount))
		  (Memo     (assocsplit 'memo)))
	       (if Category
		   (gnc:account-insert-split 
		    (gnc:xaccGetXferQIFAccount Account Category)
		    Split))
	       (if Amount
		   (gnc:split-set-value Split (- Amount)))
	       (if Memo
		   (gnc:split-set-memo Split Memo))))))
      (if Category
	  (gnc:account-insert-split
	   (gnc:xaccGetXFerQIFAccount Account Category)
	   source-split))
      (if Payee
	  (gnc:transaction-set-description Txn Payee))
      (if Id
	  (gnc:transaction-set-xnum Txn Id))
      (if Status
	  (gnc:split-set-reconcile source-split (string-ref Status 0)))
      (if Date
	  (gnc:trans-set-datesecs 
	   Txn
	   (gnc:gnc_dmy2timespec (caddr Date) (cadr Date) (car Date))))
      (if Amount
	  (gnc:split-set-value source-split Amount))
      (if Memo
	  (gnc:transaction-set-memo Txn Memo))
      (if Splits
	  ;;;; Do something with split
	  (for-each build-split-entry Splits)))
    (gnc:trans-commit-edit Txn)))
