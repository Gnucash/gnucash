(define (guess-cat inputcat gnucash-cats gnucash-accs)
;;; Need a bunch of metrics, and probably to vectorize this...
;;; 1. Braces --> pick gnucash entries from account list
;;;    No braces --> pick gnucash entries from category list
;;; 2. Exact match of names --> 
;;; 3. a contains b, b contains a --> end of list
;;; 4. First 2 letters match --> end of list
;;; 5. First letter matches --> end of list
;;; g) I'd like a "similarity match" of some sort
;;; h) Is it in old-matches?  If so, toss that to front of list.
;;; Lastly, shorten the list to no more than 4 items.
  (let*
      ((picklist '())
       (size-of-list 4)  ;; Find the 4 lowest items...
       (lowestn '())
       (catlength (string-length inputcat))  ; How long is incat?
       (is-acct? (and                          ; as the list to compare to
		  (>= catlength 2)
		  (string=? (substring inputcat 0 1) "[") 
		  (string=? (substring inputcat (- catlength 1) catlength) "]")))
       (acctlist                          ; Pick either gnucash-cats/gnucash-accs
	(if 
	 is-acct?
	 gnucash-accs
	 gnucash-cats))
       
       (incat (if is-acct?
		  (substring inputcat 1 (- catlength 1))
		  inputcat))
       ;       (null    (if is-acct? (write (string-append "Account!" incat))))
       (add-to-picklist 
	(lambda (string value)
	  (let
	      ((inlist? (assoc string picklist)))
	    (if
	     inlist?
	     (let
		 ((oldvalue (cdr inlist?)))
	       (if
		(> oldvalue value)
		(set-cdr! inlist? value)))
	     (set! picklist (cons (cons string value) picklist))))))
       
       (match-against-list 
	(lambda (itemstring)
	  (if (string=? itemstring incat)      ;;; Exact match
	      (add-to-picklist itemstring 1))
	  (if (or ((substring-search-maker incat) itemstring)    ;;; Inclusion
		  ((substring-search-maker itemstring) incat))
	      (add-to-picklist itemstring 3))
	  (if (string=? 
	       (substring incat 0 (min 2 (string-length incat)))  ;; Match first 2 chars
	       (substring itemstring 0 (min 2 (string-length itemstring))) )
	      (add-to-picklist itemstring 5))
	  (if (string=? 
	       (substring incat 0 (min 1 (string-length incat)))  ;; Match first 1 char
	       (substring itemstring 0 (min 1 (string-length itemstring))) )
	      (add-to-picklist itemstring 7)))))
    
      ;;;;;;;;  Now, apply the matching...
    (for-each match-against-list acctlist)
    (write (string-append "Match-against list: " incat)) (write picklist) (newline)
    
      ;;;;;;;;  Shorten picklist, keeping top 4 items
    (let ((shortened '()))
      (let loop ((count size-of-list))
	(if (> count 0)
	    (let
		((bestitem (find-min-cdr picklist)))
	      (if bestitem
		  (begin
		    (if (> 99 (cdr bestitem))
			(set! shortened (cons (car bestitem) shortened)))
		    (set-cdr! bestitem 999)   ;;;; Force off list...
		    (loop (- count 1)))))))
      shortened)))

(define (find-min-cdr mlist)
  (if
   (null? mlist)
   #f
   (let
       ((first (car mlist))
	(rest (find-min-cdr (cdr mlist))))
     (if
      rest   ;;; Found a value for rest
      (if (> (cdr first) (cdr rest))
	  rest
	  first)
      first))))

(define (guess-corresponding-categories import-cats gnucash-cats
					gnucash-accs)
  (define (apply-guess-cat incat)
    (guess-cat (car incat) gnucash-cats gnucash-accs))
  (map apply-guess-cat import-cats))

  ;;; Make use of "old-matches," which is an association list
  ;;; containing the correspondences that have been used previously.
  ;;; These are almost sure-fire "best matches"

;;;;; (define best-guesses 
;;;;;         (guess-corresponding-categories
;;;;;                kept-categories categories-from-gnucash))
;;;;;
;;;;; The next step would be to ask the user to verify the category
;;;;; matching, thus establishing an association list to be used to
;;;;; translate from QIF to GnuCash.  This alist should be merged with
;;;;; whatever is out on disk from "last time," and will become
;;;;; "old-matches" to provide a high quality set of "best guesses"
;;;;; for next time.
;;;;; (define (fix-category-translation best-guesses))
;;;;; which is used thus:
;;;;; (define category-translations (fix-category-translation
;;;;;                                best-guesses))
;;;;; category-translations is then an alist that is then used to pick
;;;;; off categories for use thus:
;;;;;    (let  ((use-category (assoc (assoc 'category transaction)
;;;;;                               category-translations))
;;;;;	   (date (assoc 'date transaction))
;;;;;	   (amount (assoc 'amount transaction)))
;;;;;       (add-transaction use-category date amount)  
;;;;;
;;;;; - Transactions should not be marked off as being finally reconciled on
;;;;;   the GnuCash side, as the reconciliation hasn't been done there.  
;;;;;
;;;;;   Bad Things would happen if we double-load a batch of QIF transactions, 
;;;;;   and treat it as if it were fully reconciled.


;;;;;  This returns the "thunk" that should be used to translate statuses
(define (status-handling tlist)
  (define cleared? #f)
  (define (look-for-cleared txn)
    (if
     (string=? "X" (cdr (assoc 'status txn)))
     (set! cleared #t)))
  (for-each look-for-cleared tlist)
  (if cleared?
      (begin
	(display "Warning:  This transaction list includes transactions marked as cleared.")
	(display "Are you *completely* confident of the correctness of that")
	(display "reconciliation, and that it is *truly* safe to mark them as reconciled")
	(display "in GnuCash?")
	(display "It is suggested that you indicate ``No,'' which will result in those")
	(display "transactions being statused as ``marked,'' which should make the")
	(display "reconciliation in GnuCash take place reasonably quickly.")
        ;;;; Now ask if the user is certain...
	;;;; Need some code here...
	(if (not certain?)
	    (set! cleared #f))))
  (let* 
	((cleared-to-what (if cleared? 'cleared 'marked))
	 (ttable
;;;  QIF Status translation table
;;;  The CARs are values expected from Quicken.
;;;  The CDRs are the values that gnc:transaction-put-status requires...
	  '(("X" cleared-to-what)
	    ("*" 'marked)
	    ("?" 'budgeted-new)
	    ("!" 'budgeted-old)
	    (""  'unmarked))))

;;;  And here's the "thunk" that is to be returned.  It translates QIF statuses
;;;  into the form GnuCash expects to pass to gnc:transaction-put-status
    (lambda (status)
      (let
	  ((a (assoc status ttable)))
	(if
	 a
	 (cdr a)                  ;;; If the value was found, use it..
	 (cdr (assoc "" ttable))))))) ;;; No value?  Take the null value from ttable

