;;; $Id$
;;;;;;;;;;;  QIF Split Management ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;  Variables used to handle splits  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define splits? #f)
(define splitlist '())
(define qif-split-structure 
  (define-mystruct '(category memo amount percent)))

(define thesplit (build-mystruct-instance qif-split-structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And functions to nuke out the splits ;;;;
;;;; at the start/end of each transaction ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resetsplits)   ;;; Do this at end of whole txn
  (set! splits? #f)
  (set! splitlist '())
  (set! thesplit (build-mystruct-instance qif-split-structure)))

;;;;  This function *should* validate that a split adds up to 
;;;;  the same value as the transaction, and gripe if it's not.
;;;;  I'm not sure how to usefully gripe, so I leave this as a stub.
(define (ensure-split-adds-up)
  (let*
      ((txnamount (thetxn 'get 'amount))
       (find-amount (lambda (splitstructure) (splitstructure 'get 'amount)))
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
  (thesplit 'put 'amount (numerizeamount (strip-qif-header line)))
  ;;; And now, add amount and memo to splitlist
  (display (thesplit 'what 'what)) (newline)
  (set! splitlist (cons thesplit splitlist))
  (set! thesplit (build-mystruct-instance qif-split-structure)))

;;;; percentages only occur as parts of memorized transactions
(define (transsplitpercent line)
  (set! splits? #T)
  #f)   ;;;; Do nothing; percentages only occur in memorized transactions

(define (transsplitmemo line)
  (set! splits? #T)
  (thesplit 'put 'memo (strip-qif-header line)))

(define (transsplitcategory line)
  (set! splits? #T)
  (thesplit 'put 'category (strip-qif-header line)))
