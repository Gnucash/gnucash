;;; $Id$
;;; Need a bunch of metrics, and probably to vectorize this...
;;; 1. Braces --> pick gnucash entries from account list
;;;    No braces --> pick gnucash entries from category list
;;; 2. Exact match of names --> 
;;; 3. a contains b, b contains a --> end of list
;;; 4. First 2 letters match --> end of list
;;; 5. First letter matches --> end of list
;;; 6. I'd like a "similarity match" of some sort
;;; 7. Is it in old-matches?  If so, toss that to front of list.
;;; Lastly, shorten the list to no more than 4 items.

(define (guess-gnucash-category 
	 inputcat gc-income-categories gc-account-categories)
  (let*
      ((picklist (initialize-hashtable))
       (qifname (inputcat 'get 'name))
       (catlength (string-length (qifname)))
       (is-acct? (and
		  (>= catlength 2)
		  (string=? (substring inputcat 0 1) "[") 
		  (string=? (substring inputcat 
				       (- catlength 1) catlength) "]")))
       (netdebit? (< (inputcat 'get 'value)))
       (acctlist  ; Pick either gc-income-categories/gc-account-categories
	(if 
	 is-acct?
	 gc-account-categories
	 gc-income-categories))
       (incat (if is-acct?
		  (substring inputcat 1 (- catlength 1))
		  inputcat))
       (add-to-picklist 
	(lambda (string value)
	   (hashv-set! picklist string value)))
       (match-against-list 
	(lambda (itemstring)
	  (if (string=? itemstring incat)      ;;; Exact match
	      (add-to-picklist itemstring 1))
	  (if (or ((substring-search-maker incat) itemstring) ;;; Inclusion
		  ((substring-search-maker itemstring) incat))
	      (add-to-picklist itemstring 3))
	  (if (string=? 
	       (substring incat 0 
			  (min 2 (string-length incat))) ;; Match first 2 chars
	       (substring itemstring 0 (min 2 (string-length itemstring))) )
	      (add-to-picklist itemstring 5))
	  (if (string=? 
	       (substring incat 0 
			  (min 1 (string-length incat)));; Match first 1 char
	       (substring itemstring 0 (min 1 (string-length itemstring))))
	      (add-to-picklist itemstring 7)))))
    
      ;;;;;;;;  Now, apply the matching...
    (for-each match-against-list acctlist)
    
      ;;;;;;;;  Shorten picklist, keeping top 4 items
    (shorten-to-best 4 picklist)))

(define (guess-corresponding-categories
	 import-categories 
	 gc-income-categories gc-account-categories)
  (define apply-guess-category
    (lambda (incat)
      (list incat 
	    (guess-gnucash-category (car incat)
				    gc-income-categories
				    gc-account-categories))))
  
  (map apply-guess-category import-categories))

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

(define (guess-results account-group kept-categories)
  (guess-corresponding-categories 
   kept-categories 
   (gnc:get-incomes-list account-group)
   (gnc:get-account-list account-group)))