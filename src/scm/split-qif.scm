;;;;;;;;;;;  QIF Split Management ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;  Variables used to handle splits  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define splits? #f)
(define splitlist '())
(define splitcategory #f)
(define splitamount #f)
(define splitmemo #f)
(define splitpercent #f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And functions to nuke out the splits ;;;;
;;;; at the start/end of each transaction ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resetsplits)   ;;; Do this at end of whole txn
  (set! splits? #f)
  (set! splitlist '())
  (resetsplit))

(define (resetsplit)     ;;;  After each split item
  (set! splitcategory #f)
  (set! splitmemo #f)
  (set! splitpercent #f))

;;;;  This function *should* validate that a split adds up to 
;;;;  the same value as the transaction, and gripe if it's not.
;;;;  I'm not sure how to usefully gripe, so I leave this as a stub.
(define (ensure-split-adds-up)
  (let*
      ((txnamount (cdr (assoc 'amount atrans)))
       (find-amount (lambda (txnlist) (cdr (assoc 'amount txnlist))))
       (total-of-split
	(apply + (map find-amount splitlist))))
    (if
     (< (abs (- txnamount total-of-split)) 0.01)  ; Difference tiny
     #t
     (begin
       (display 
	(string-append "Error - Transaction amount, " 
		       (number->string  txnamount)
		       " not equal to sum of split amount, "
		       (number->string total-of-split)))
       #f))))

(define (transsplitamt line)
  (set! splits? #T)
  (let*
      ((linelen (string-length line))
       (amount    (numerizeamount (substring line 1 linelen)))
       (amtlist   (cons 'amount  amount))
       (catlist   (cons 'category splitcategory))
       (entry     (list amtlist catlist)))
    ;;; And now, add amount and memo to splitlist
    (set! splitlist 
	  (cons entry splitlist))))

;;;; percentages only occur as parts of memorized transactions
(define (transsplitpercent line)
  (set! splits? #T)
  #f)   ;;;; Do nothing; percentages only occur in memorized transactions

(define (transsplitmemo line)
  (set! splits? #T)
  (let*
      ((linelen (string-length line))
       (memo    (substring line 1 linelen)))
    (set! splitmemo memo)))

(define (transsplitcategory line)
  (set! splits? #T)
  (let*
      ((linelen (string-length line))
       (category    (substring line 1 linelen)))
    (keep-category-for-summary category)
    (set! splitcategory category)))
