;;; $Id$
;;;;  Take the set of stuff from a QIF file, and turn it into the
;;;;  structures expected by GnuCash.

;;; In each of these, "gncpointer" should be populated with the
;;; address of the object.  This way the object can be maintained
;;; on both sides of the Lisp<==>C boundary
;;; For instance:
; (define (initialize-split)   ;;; Returns a gnc-split-structure
;    (let ((ptr (gnc:split-create))
;  	(splitstruct (build-mystruct-instance gnc-split-structure)))
;      (splitstruct 'put 'gncpointer ptr)
;      splitstruct))

(define gnc-account-structure 
  (define-mystruct '(id name flags type code description
			notes currency security splitlist
			parentaccountgroup
			childrenaccountgroup)))

(define gnc-account-group-structure 
  (define-mystruct '(parentaccount peercount
				   peerlist)))

(define gnc-txn-structure 
  (define-mystruct '(num date-posted date-entered description
			 docref splitlist)))

(define gnc-split-structure 
  (define-mystruct '(memo action reconcile-state
			  reconciled-date docref share-amount
			  share-price account parenttransaction)))

(define gnc-txn-list (initialize-lookup))
(define gnc-acc-list (initialize-lookup))
(define gnc-split-list (initialize-lookup))

(define (add-qif-transaction-to-gnc-lists txn curtxn cursplitlist)
  (define txnref (gensym))
  (set! gnc-txn-list (lookup-set! gnc-txn-list txnref curtxn))
    ;;; Fill in gnc-txn-list, gnc-acc-list, gnc-split-list
    ;;; First, let's fill in curtxn with some values from txn
  (curtxn 'put 'num (txn 'get 'id))
  (curtxn 'put 'date-posted (txn 'get 'date))
  (curtxn 'put 'date-entered '(1999 0903)) ;;; Which should get replaced!
  (curtxn 'put 'description (txn 'get 'memo))
  (curtxn 'put 'docref (txn 'get 'id))
    ;;; Now, set up the list of splits...
  (let ((mainref (gensym))
	(mainsplit (build-mystruct-instance gnc-split-structure)))
    (mainsplit 'put 'memo (txn 'get 'memo))
    (mainsplit 'put 'share-amount (txn 'get 'amount))
    (mainsplit 'put 'reconcile-state (txn 'get 'status))
    (mainsplit 'put 'reconcile-state 
	       (if  (string=? (txn 'get 'status) "*")
		    '(1999 09 03) #f))
    (mainsplit 'put 'docref (txn 'get 'id))
    (mainsplit 'put 'parenttransaction txnref)
    (mainsplit 'put 'account accountname)
    (set! gnc-split-list (lookup-set! gnc-split-list mainref mainsplit)))
    
    ;;;; Chunk of missing code:
    ;;;; ---> Take a look at the split list in (txn 'get 'splitlist)
    ;;;;      Add a split for each one of these
    ;;;;      Alternatively, add a split for (txn 'get 'category)
    ;;;; ---> Attach all the accounts to the corresponding splits
  (curtxn 'put 'splitlist lookup-keys cursplitlist))

(define (qif-to-gnucash txnlist accountname)
  (letrec 
      ((curtxn (build-mystruct-instance gnc-txn-structure))
       (cursplitlist (initialize-lookup))
       (process-txn (lambda (x) (add-qif-transaction-to-gnc-lists x curtxn cursplitlist))))
    (for-each process-txn txnlist)))

; QIF essentially provides a structure that sort of looks like
; (chequing
;  (deposit 500 salary)
;  (withdraw 300 rent)
;  (transfer 200 mastercard))

; Asset account
;    --> Bunch of transactions, implicitly associated with it
;    --> That are also associated with income/expense accounts

; This must be transformed to something more like:
;;; Account points to vector of splits, each split points to a transaction

; Accounts look like:
; ('chequing
;   (500 'chequing 'deposit)
;   (-300 'chequing 'withdraw)
;   (-200 'chequing 'transfer))

; ('mastercard
;   (200 'mastercard 'transfer))

; ('salary
;   (-500 'salary 'deposit))

; ('rent
;   (-500 'rent 'withdraw))

; Transactions look like:
; ('deposit
;   (500 'chequing 'deposit)
;   (-500 'salary 'deposit))

; (withdraw
;   (-300 'chequing 'withdraw)
;   (-500 'rent 'withdraw))

; (transfer
;   (200 'mastercard 'transfer)
;   (-200 'chequing 'transfer))

; And the splits are the subordinates in both cases...

;;; Thus, the approach should be:
; -- For each QIF transaction QT
;   -- Create transaction
;   -- Construct the splits for the current transaction
;      If there's no QIF split, then there's two:
;          - One for the [current account]
;          - Offset by the [category]
;      Alternatively:
;          - One for the [current account]
;          - Offset by the set of QIF split items
;    - Link splits to transaction
;          - Link transaction to split list
;      - Link each splits to appropriate account
;      - Add each split to the account-to-splits list for the account

(define (initialize-split)   ;;; Returns a gnc-split-structure
   (let ((ptr (gnc:split-create))
 	(splitstruct (build-mystruct-instance gnc-split-structure)))
     (splitstruct 'put 'gncpointer ptr)
     splitstruct))

(define (gnc:set-split-values q-txn q-split)
  (let ((g:split (initialize-split))
	(g:memo  (q-split 'get 'memo))
	(g:amount (q-split 'get 'amount))
	(g:docref (q-split 'get 'id))
	(g:action (q-txn 'get 'status)))
    (if g:amount (gnc:split-set-value g:split g:amount))
    (if g:memo (gnc:split-set-memo g:split g:memo))
    (if g:action (gnc:split-set-action g:split g:action))
    (if g:docref (gnc:split-set-docref g:split g:docref))))

(define (gnc:link-split-to-parents g:split g:account g:transaction)
  (gnc:transaction-append-split g:transaction g:split)
  (gnc:account-insert-split g:account g:split))

(define (initialize-account)   ;;; Returns a gnc-split-structure
   (let ((ptr (gnc:malloc-account))
 	(accstruct (build-mystruct-instance gnc-account-structure)))
     (accstruct 'put 'gncpointer ptr)
     accstruct))

(define (initialize-txn)   ;;; Returns a gnc-split-structure
   (let ((ptr (gnc:transaction-create))
 	(txnstruct (build-mystruct-instance gnc-transaction-structure)))
     (txnstruct 'put 'gncpointer ptr)
     txnstruct))

(if testing?
    (begin
      (display "need test scripts in qif2gc.scm")))

(define best-guesses (initialize-lookup))

(define (add-best-guess qif gnc)
  (set! best-guesses (lookup-set! best-guesses qif gnc)))

(define (find-best-guess qif)
  (lookup qif best-guesses))

(define qif-to-gnc-acct-xlation-table (initialize-lookup))

(define (improve-qif-to-gnc-translation qif gnc)
  (set! qif-to-gnc-acct-xlation-table 
	(lookup-set! qif-to-gnc-acct-xlation-table 
		     qif gnc)))diff -u /dev/null 'gnucash/src/scm/qifcats.scm'
