;;; $Id$
;;;;; Category management

(define qif-cat-list (initialize-lookup))

(define qif-category-structure
  (define-mystruct '(name count value)))

(define (analyze-qif-categories)
  (define (analyze-qif-category item)
    (let* 
	((id (car item))
	 (q (cdr item))
	 (gc (build-mystruct-instance gnc-account-structure))
	 (positive? (<= 0 (q 'get 'amount)))
	 (balance-sheet? (char=? (string-ref id 0) #\[))
	 (propername 	  (if balance-sheet?
			      (substring 1 (- (string-length id) 1))
			      id)))
      (gc 'put 'type 
	  (if positive?
	      (if balance-sheet?
		  'BANK
		  'CREDIT)
	      (if balance-sheet?
		  'INCOME
		  'EXPENSE)))
      (gc 'put 'description id)
      (gc 'put 'currency favorite-currency)))
  (set! qif-analysis (initialize-lookup))
  (for-each analyze-qif-category qif-category-list))

(define (analyze-qif-transaction-categories qif-txn-list)
  (define (analyze-qif-txn-category txn)
    (collect-cat-stats (txn 'get 'category)
		       (txn 'get 'amount))
    (let ((splits (txn 'get 'splitlist)))
      (if splits
	  (for-each analyze-qif-split-category splits))))
  (set! qif-cat-list (initialize-lookup))
  (for-each analyze-qif-txn-category qif-txn-list)
  qif-cat-list)

(define (analyze-qif-split-category split)
  (collect-cat-stats (split 'get 'category) (split 'get 'amount)))

(define (collect-cat-stats category amount)
  (let* ((s (lookup category qif-cat-list)))
    (if s   ;;; Did we find it in qif-cat-list?
	(let ((sc (cdr s)))
	  (sc 'put 'value (+ amount (sc 'get 'value)))
	  (sc 'put 'count (+ 1 (sc 'get 'count))))
	(begin   ;;; Nope; need to add new entry to qif-cat-list
	  (let ((nc (build-mystruct-instance qif-category-structure)))
	    (nc 'put 'name category)
	    (nc 'put 'count 1)
	    (nc 'put 'value amount)
	    (set! qif-cat-list (lookup-set! qif-cat-list category nc)))))))
