;;; $Id$
;;;;  Take the set of stuff from a QIF file, and turn it into the
;;;;  structures expected by GnuCash.

;;; In each of these, "gncpointer" should be populated with the
;;; address of the object.  This way the object can be maintained
;;; on both sides of the Lisp<==>C boundary
;;; For instance:

(define gnc-account-structure 
  (make-record-type "gnucash-account-structure" 
		    '(id name flags type code description
			 notes currency security splitlist
			 parentaccountgroup
			 childrenaccountgroup)))

(define (gnc-account-update acc field value)
  ((record-modifier gnc-account-structure field) acc value))

(define (gnc-account-get acc field)
  ((record-accessor gnc-account-structure field) acc))

(define gnc-account-group-structure 
  (make-record-type "gnucash-account-group-structure" 
		    '(parentaccount peercount
				    peerlist)))

(define gnc-txn-structure 
  (make-record-type "gnucash-txn-structure"
		    '(num date-posted date-entered description
			  docref splitlist)))

(define (gnc-txn-update txn field value)
  ((record-modifier gnc-txn-structure field) txn value))

(define (gnc-txn-get txn field)
  ((record-accessor gnc-txn-structure field) txn))

(define gnc-split-structure 
  (make-record-type "gnucash-split-structure" 
		    '(memo action reconcile-state
			   reconciled-date docref share-amount
			   share-price account parenttransaction)))

(define (gnc-split-update split field value)
  ((record-modifier gnc-split-structure field) split value))

(define (gnc-split-get split field)
  ((record-accessor gnc-split-structure field) split))

(define gnc-txn-list (initialize-hashtable))
(define gnc-acc-list (initialize-hashtable))
(define gnc-split-list (initialize-hashtable))

(define (add-qif-transaction-to-gnc-lists txn curtxn cursplitlist accountname)
  (define txnref (gensym))
  (hashv-set! gnc-txn-list txnref curtxn)
    ;;; Fill in gnc-txn-list, gnc-acc-list, gnc-split-list
    ;;; First, let's fill in curtxn with some values from txn
  (gnc-txn-update curtxn 'num (txn 'get 'id))
  (gnc-txn-update curtxn 'date-posted (txn 'get 'date))
  (gnc-txn-update curtxn 'date-entered '(1999 0903)) ;;; Which should get replaced!
  (gnc-txn-update curtxn 'description (txn 'get 'memo))
  (gnc-txn-update curtxn 'docref (txn 'get 'id))
    ;;; Now, set up the list of splits...
  (let ((mainref (gensym))
	(mainsplit ((record-constructor gnc-split-structure) 
		    #f #f #f #f #f #f #f #f #f))) 
    (gnc-split-update mainsplit 'memo (txnget txn 'memo))
    (gnc-split-update mainsplit 'share-amount (txnget txn 'amount))
    (gnc-split-update mainsplit 'reconcile-state (txnget txn 'status))
    (gnc-split-update mainsplit 'reconciled-date
		      (if  (string=? (txnget txn 'date) "*")
			   '(1999 09 03) #f))
    (gnc-split-update mainsplit 'docref (txnget txn 'id))
    (gnc-split-update mainsplit 'parenttransaction txnref)
    (gnc-split-update mainsplit 'account accountname)
    (hashv-set! gnc-split-list mainref mainsplit))
  
    ;;;; Chunk of missing code:
    ;;;; ---> Take a look at the split list in (txnget txn 'splitlist)
    ;;;;      Add a split for each one of these
    ;;;;      Alternatively, add a split for (txnget txn 'category)
    ;;;; ---> Attach all the accounts to the corresponding splits
  (display "Now, update txn with set of split...")
  (gnc-txn-update curtxn 'splitlist lookup-keys cursplitlist)
  (display "done.") (newline)
  )

(define (qif-to-gnucash txnlist accountname)
  (letrec 
      ((curtxn ((record-constructor gnc-txn-structure) #f #f #f #f #f #f))
       (cursplitlist (initialize-hashtable 19))  ;;; Doesn't need to be large
       (process-txn (lambda (x) 
		      (add-qif-transaction-to-gnc-lists 
		       x curtxn cursplitlist accountname))))
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
 	(splitstruct ((record-constructor gnc-split-structure) 
		      #f #f #f #f #f #f #f #f #f)))
     (gnc-split-structure splitstruct 'gncpointer ptr)
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
	 (accstruct ((record-constructor gnc-account-structure)
		     #f  #f #f #f #f #f #f #f #f #f #f #f)))
     (gnc-account-update accstruct 'gncpointer ptr)
     accstruct))

(define (initialize-txn)   ;;; Returns a gnc-split-structure
   (let ((ptr (gnc:transaction-create))
	 (txnstruct ((record-constructor gnc-transaction-structure) 
		      #f #f #f #f #f #f)))
     (gnc-account-update txnstruct 'gncpointer ptr)
     txnstruct))

(if testing?
    (begin
      (display "need test scripts in qif2gc.scm")))

(define best-guesses (initialize-hashtable 19))  ;; Need not be a big list

(define (add-best-guess qif gnc)
 (hashv-set! best-guesses qif gnc))

(define (find-best-guess qif)
  (hashv-ref qif best-guesses))

(define qif-to-gnc-acct-xlation-table (initialize-hashtable))

(define (improve-qif-to-gnc-translation qif gnc)
  (hashv-set! qif-to-gnc-acct-xlation-table 
	      qif gnc))
