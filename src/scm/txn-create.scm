(define (gnc:create-transaction Account txnlist)
  (define (associt type)
    (let
	((result (hashv-ref type txnlist)))
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

(define (gnc:test-load-txns accg)
  #f)diff -u 'pristine/gnucash/src/scm/utilities.scm' 'working/gnucash/src/scm/utilities.scm'
