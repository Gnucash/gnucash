;;; $Id$
;;;;;;;;;;;  QIF Split Management ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;  Variables used to handle splits  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define splits? #f)
(define splitlist '())
(define qif-split-structure 
  (make-record-type "qif-split-structure" 
		    '(category memo amount percent)))

(define (qif-split-update split field value)
	((record-modifier qif-split-structure field) split value))

(define (create-qif-split-structure) 
  ((record-constructor qif-split-structure) #f #f #f #f))

(define thesplit (create-qif-split-structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And functions to nuke out the splits ;;;;
;;;; at the start/end of each transaction ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resetsplits)   ;;; Do this at end of whole txn
  (set! splits? #f)
  (set! splitlist '())
  (set! thesplit (create-qif-split-structure)))

;;;;  This function *should* validate that a split adds up to 
;;;;  the same value as the transaction, and gripe if it's not.
;;;;  I'm not sure how to usefully gripe, so I leave this as a stub.
(define (ensure-split-adds-up)
  (let*
      ((txnamount (txnget thetxn 'amount))
       (find-amount (lambda (splitstructure) 
		      ((record-accessor qif-split-structure  
					'amount) splitstructure)))
       (null (begin (display "splitlist") (display splitlist) (display (map find-amount splitlist))))
       (total-of-split
	(apply + (map find-amount splitlist))))
    (if
     (< (abs (- txnamount total-of-split)) 0.01)  ; Difference tiny
     #t         ;;; OK - adds up to near enough zero.
     (begin     ;;; Problem: Doesn't add up
       (display 
	(string-append "Error - Transaction amount, " 
		       (number->string txnamount)
		       " not equal to sum of split amount, "
		       (number->string total-of-split)))
       (newline)
       (display splitlist)
       (newline)
       #f))))

(define (transsplitamt line)
  (set! splits? #T)
  (qif-split-update thesplit 'amount (numerizeamount (strip-qif-header line)))
  ;;; And now, add amount and memo to splitlist
;  (display (thesplit 'what 'what)) (newline)
  (set! splitlist (cons thesplit splitlist))
  (set! thesplit (create-qif-split-structure)))
  
;;;; percentages only occur as parts of memorized transactions
(define (transsplitpercent line)
  (set! splits? #T)
  #f)   ;;;; Do nothing; percentages only occur in memorized transactions

(define (transsplitmemo line)
  (set! splits? #T)
  (qif-split-update thesplit 'memo (strip-qif-header line)))

(define (transsplitcategory line)
  (set! splits? #T)
  (qif-split-update thesplit 'category (strip-qif-header line)))
