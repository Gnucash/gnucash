;;; $Id$
;;;;; Category management

(define qif-cat-list (initialize-hashtable))

(define qif-category-structure
  (make-record-type "qif-category-structure" '(name count value)))

(define (qif-category-update cat field value)
  ((record-modifier qif-category-structure field) cat value))

(define (qif-category-get cat field)
  ((record-accessor qif-category-structure field) cat))

(define (analyze-qif-categories)
  (define (analyze-qif-category item)
    (let* 
	((id (car item))
	 (q (cdr item))
	 (gc ((record-constructor gnc-account-structure) 
	      #f #f #f #f #f #f #f #f #f #f #f #f))
	 (positive? (<= 0 (q 'get 'amount)))
	 (balance-sheet? (char=? (string-ref id 0) #\[))
	 (propername 	  (if balance-sheet?
			      (substring 1 (- (string-length id) 1))
			      id)))
      (gnc-account-update gc 'type 
	  (if positive?
	      (if balance-sheet?
		  'BANK
		  'CREDIT)
	      (if balance-sheet?
		  'INCOME
		  'EXPENSE)))
      (gnc-account-update gc  'description id)
      (gnc-account-update gc  'currency favorite-currency)))
  (set! qif-analysis (initialize-hashtable))
  (for-each analyze-qif-category qif-category-list))

(define (analyze-qif-transaction-categories qif-txn-list)
  (define (analyze-qif-txn-category txn)
    (collect-cat-stats (txnget txn 'category)
		       (txnget txn 'amount))
    (let ((splits (txnget txn 'splitlist)))
      (if splits
	  (for-each analyze-qif-split-category splits))))
  (set! qif-cat-list (initialize-hashtable))
  (for-each analyze-qif-txn-category qif-txn-list)
  qif-cat-list)

(define (analyze-qif-split-category split)
  (collect-cat-stats (qif-split-get split 'category)
		     (qif-split-get split 'amount)))

(define (collect-cat-stats category amount)
  (let* ((s (hash-ref qif-cat-list category)))
    (if s   ;;; Did we find it in qif-cat-list?
	(begin   ;;; Yes; found an existing entry so update it's attributes
	  (qif-category-update s 'value (+ amount (qif-category-get s 'value)))
	  (qif-category-update s 'count (+ 1 (qif-category-get s 'count))))
	(begin   ;;; Nope; need to add new entry to qif-cat-list
	  (let ((nc ((record-constructor qif-category-structure) #f #f #f)))
	    (qif-category-update nc 'name category)
	    (qif-category-update nc 'count 1)
	    (qif-category-update nc 'value amount)
	    (hash-set! qif-cat-list category nc))))))

