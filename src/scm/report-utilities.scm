;; report-utilities.scm -- Reporting utilities
;;
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(gnc:support "report-utilities.scm")
(gnc:depend "engine-utilities.scm")

(define (gnc:amount->string amount info)
  (d-gnc:amount->string-helper (exact->inexact amount) info))

(define (gnc:commodity-amount->string amount info)
  (gnc:amount->string-helper amount info))

(define (gnc:account-has-shares? account)
  (let ((type (gw:enum-GNCAccountType-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(stock mutual-fund currency))))

(define (gnc:account-separator-char)
  (let ((option (gnc:lookup-option gnc:*options-entries*
                                   "General" "Account Separator")))
    (if option
        (case (gnc:option-value option)
          ((colon) ":")
          ((slash) "/")
          ((backslash) "\\")
          ((dash) "-")
          ((period) ".")
          (else ":"))
        ":")))

;; get a full account name
(define (gnc:account-get-full-name account)
  (let ((separator (gnc:account-separator-char)))
    (if (not account)
	""
	(let ((parent-name
	       (gnc:account-get-full-name 
		(gnc:group-get-parent
		 (gnc:account-get-parent account)))))
	  (if (string=? parent-name "")
	      (gnc:account-get-name account)
	      (string-append
	       parent-name
	       separator
	       (gnc:account-get-name account)))))))

;; get children that are the direct descendant of this acct
(define (gnc:account-get-immediate-subaccounts acct)
  (define (acctptr-eq? a1 a2)
    (let ((a1-str 
           (with-output-to-string (lambda () (write a1))))
          (a2-str 
           (with-output-to-string (lambda () (write a2)))))
      (string=? a1-str a2-str)))
  
  (let* ((group (gnc:account-get-children acct))
         (children (gnc:group-get-subaccounts group))
         (retval '()))
    (for-each 
     (lambda (child)
       (if (acctptr-eq? acct (gnc:account-get-parent-account child))
           (begin 
             (set! retval (cons child retval)))))
     children)
    (reverse retval)))

;; get all children of this account 
(define (gnc:account-get-all-subaccounts acct)
  (let ((group (gnc:account-get-children acct)))
    (gnc:group-get-subaccounts group)))
                            
;; returns a list contains elements of the-list for which predicate is true
(define (gnc:filter-list the-list predicate)
  (let loop ((rest the-list)
             (collected '()))
    (cond ((null? rest) (reverse collected))
          (else (loop (cdr rest)
                      (if (predicate (car rest))
                          (cons (car rest) collected)
                          collected))))))

;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.
;; note: map-in-order is in a SRFI.
(define (gnc:inorder-map the-list fn)
  (let loop ((rest the-list)
             (collected '()))
    (cond ((null? rest) (reverse collected))
          (else (loop (cdr rest)
                      (cons (fn (car rest)) collected))))))

(define (gnc:for-loop thunk first last step)
  (if (< first last) 
      (begin
	(thunk first)
	(gnc:for-loop thunk (+ first step) last step))
      #f))

(define (gnc:map-for thunk first last step)
  (if (< first last)
      (cons
	(thunk first)
	(gnc:map-for thunk (+ first step) last step))
      '()))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
                0 (gnc:account-get-split-count account) 1))

;;; applies thunk to each split in account account
(define (gnc:map-splits-in-account thunk account)
  (gnc:map-for (lambda (x) 
		 (thunk (gnc:account-get-split account x)))
	       0 (gnc:account-get-split-count account) 1))

; (define (gnc:account-transactions-for-each thunk account)
;   ;; You must call gnc:group-reset-write-flags on the account group
;   ;; before using this...
;
;   (let loop ((num-splits (gnc:account-get-split-count account))
;              (i 0))
;     (if (< i num-splits)
;         (let* ((split (gnc:account-get-split account i))
;                (transaction (gnc:split-get-parent split)))
;           ;; We don't use the flags just like FileIO does (only 1 pass here).
;           (if (= (gnc:transaction-get-write-flag transaction) 0)
;               (begin
;                 (thunk transaction)
;                 (gnc:transaction-set-write-flag transaction 2)))
;           (loop num-splits (+ i 1))))))

(define (gnc:transaction-map-splits thunk transaction)
  (let loop ((num-splits (gnc:transaction-get-split-count transaction))
             (i 0))
    (if (< i num-splits)
        (cons
         (thunk (gnc:transaction-get-split transaction i))
         (loop num-splits (+ i 1)))
        '())))

;;; Here's a statistics collector...  Collects max, min, total, and makes
;;; it easy to get at the mean.

;;; It would be a logical extension to throw in a "slot" for x^2 so
;;; that you could also extract the variance and standard deviation

(define (make-stats-collector)
  (let ;;; values
      ((value 0)
       (totalitems 0)
       (max -10E9)
       (min 10E9))
    (let ;;; Functions to manipulate values
	((adder (lambda (amount)
		  (if (number? amount) 
		      (begin
			(set! value (+ amount value))
			(if (> amount max)
			    (set! max amount))
			(if (< amount min)
			    (set! min amount))
			(set! totalitems (+ 1 totalitems))))))
	 (gettotal (lambda () value))
	 (getaverage (lambda () (/ value totalitems)))
	 (getmax (lambda () max))
	 (getmin (lambda () min))
	 (reset-all (lambda ()
		    (set! value 0)
		    (set! max -10E9)
		    (set! min 10E9)
		    (set! totalitems 0))))
      (lambda (action value)  ;;; Dispatch function
	(case action
	  ('add (adder value))
	  ('total (gettotal))
	  ('average (getaverage))
	  ('getmax (getmax))
	  ('getmin (getmin))
	  ('reset (reset-all))
          (else (gnc:warn "bad stats-collector action: " action)))))))

(define (make-drcr-collector)
  (let ;;; values
      ((debits 0)
       (credits 0)
       (totalitems 0))
    (let ;;; Functions to manipulate values
	((adder (lambda (amount)
		 (if (> 0 amount)
		     (set! credits (- credits amount))
		     (set! debits (+ debits amount)))
		 (set! totalitems (+ 1 totalitems))))
	 (getdebits (lambda () debits))
	 (getcredits (lambda () credits))
	 (setdebits (lambda (amount)
		      (set! debits amount)))
	 (getitems (lambda () totalitems))
	 (reset-all (lambda ()
		    (set! credits 0)
		    (set! debits 0)
		    (set! totalitems 0))))
      (lambda (action value)  ;;; Dispatch function
	(case action
	  ('add (adder value))
	  ('debits (getdebits))
	  ('credits (getcredits))
	  ('items (getitems))
	  ('reset (reset-all))
          (else (gnc:warn "bad dr-cr-collector action: " action)))))))

;; This is a collector of values -- works similar to the stats-collector but
;; has much less overhead. It is used by the currency-collector (see below).
(define (make-value-collector)
  (let ;;; values
      ((value 0))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	('add (if (number? amount) 
		  (set! value (+ amount value))))
	('total value)
	(else (gnc:warn "bad value-collector action: " action))))))

;; Same as above but with gnc:numeric
(define (make-numeric-collector)
  (let ;;; values
      ((value (gnc:numeric-zero)))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	('add (if (gnc:gnc-numeric? amount) 
		  (set! value (gnc:numeric-add-fixed amount value))))
	('total value)
	(else (gnc:warn "bad numeric-collector action: " action))))))

;; A commodity collector. This is intended to handle multiple
;; currencies' amounts. The amounts are accumulated via 'add, the
;; result can be fetched via 'format.  Used to work with strings as
;; currencies and doubles as values, but now it uses <gnc:commodity*>
;; as commodity and <gnc:numeric> as value. 
;; Old Example: (define a (make-commodity-collector)) ... 
;; (a 'add 'USD 12) ...  (a 'format (lambda(x y)(list x y)) #f) 
;; used to give you something like 
;; ((USD 123.4) (DEM 12.21) (FRF -23.32))
;;
;; The functions:
;;   'add <commodity> <amount>: Add the given amount to the 
;;       appropriate currencies' total amount.
;;   'format <fn> #f: Call the function <fn> (where fn takes two
;;       arguments) for each commodity with the arguments <commodity>
;;       and the corresponding total <amount>. The results is a list
;;       of each call's result.
;;   'merge <commodity-collector> #f: Merge the given other
;;       commodity-collector into this one, adding all currencies'
;;       amounts, respectively.
;;   'minusmerge <commodity-collector> #f: Merge the given other 
;;       commodity-collector into this one (like above) but subtract
;;       the other's currencies' amounts from this one's amounts,
;;       respectively.
;;   'reset #f #f: Delete everything that has been accumulated 
;;       (even the fact that any commodity showed up at all).
;;   (internal) 'list #f #f: get the association list of 
;;        commodity->numeric-collector

(define (make-commodity-collector)
  (let 
      ;; the association list of (commodity -> value-collector) pairs.
      ((commoditylist '()))
    
    ;; helper function to add a commodity->value pair to our list. 
    ;; If no pair with this commodity exists, we will create one.
    (define (add-commodity-value commodity value)
      ;; lookup the corresponding pair
      (let ((pair (assoc commodity commoditylist)))
	(if (not pair)
	    (begin
	      ;; create a new pair, using the gnc:numeric-collector
	      (set! pair (list commodity (make-numeric-collector)))
	      ;; and add it to the alist
	      (set! commoditylist (cons pair commoditylist))))
	;; add the value
	((cadr pair) 'add value)))
    
    ;; helper function to walk an association list, adding each
    ;; (commodity -> collector) pair to our list at the appropriate 
    ;; place
    (define (add-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value (caar clist) 
				       ((cadar clist) 'total #f))
		  (add-commodity-clist (cdr clist)))))

    (define (minus-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value (caar clist) 
				       (gnc:numeric-sub-fixed 
					gnc:numeric-zero 
					((cadar clist) 'total #f)))
		  (minus-commodity-clist (cdr clist)))))

    ;; helper function walk the association list doing a callback on
    ;; each key-value pair.
    (define (process-commodity-list fn clist)
      (cond ((null? clist) '())
	    (else (cons (fn (caar clist) ((cadar clist) 'total #f))
			(process-commodity-list fn (cdr clist))))))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a list (gnc-commodity gnc:numeric) 
    (define (getpair c)
      (let ((pair (assoc c commoditylist)))
	(list c 
	      (if (not pair)
		  (gnc:numeric-zero)
		  ((cadr pair) 'total #f)))))

    ;; Dispatch function
    (lambda (action commodity amount)
      (case action
	('add (add-commodity-value commodity amount))
	('merge (add-commodity-clist (commodity 'list #f #f)))
	('minusmerge (minus-commodity-clist (commodity 'list #f #f)))
	('format (process-commodity-list commodity commoditylist))
	('reset (set! commoditylist '()))
	('getpair (getpair commodity))
	('list commoditylist) ; this one is only for internal use
	(else (gnc:warn "bad commodity-collector action: " action))))))

;; Add x to list lst if it is not already in there
(define (addunique lst x)
  (if (null? lst)  
      (list x)		; all checked add it
      (if (equal? x (car lst))
	  lst	; found, quit search and don't add again
	  (cons (car lst) (addunique (cdr lst) x))))) ; keep searching

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

;; get transaction date from split - needs to be done indirectly
;; as it's stored in the parent transaction
(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
(define (gnc:account-get-balance-at-date account date include-children?)
  (let ((children-balance
         (if include-children?
             (gnc:group-get-balance-at-date
              (gnc:account-get-children account) date)
             0))
        (balance #f)
        (query (gnc:malloc-query))
        (splits #f))
    
    (gnc:query-set-group query (gnc:get-current-group))
    (gnc:query-add-single-account-match query account 'query-and)
    (gnc:query-add-date-match-timepair query #f date #t date 'query-and) 
    (gnc:query-set-sort-order query 'by-date 'by-standard 'by-none)
    (gnc:query-set-sort-increasing query #t)
    (gnc:query-set-max-splits query 1)
    
    (set! splits (gnc:glist->list 
                  (gnc:query-get-splits query) 
                  <gnc:Split*>))
    (if (and splits (not (null? splits)))
        (set! balance (gnc:numeric-to-double 
                       (gnc:split-get-balance (car splits))))
        (set! balance 0.0))
    (if include-children? 
        (+ balance children-balance)
        balance)))    

;; This works similar as above but returns a commodity-collector, 
;; thus takes care of children accounts with different currencies.
;;
;; Note that the commodity-collector contains <gnc:numeric> values
;; rather than double values.
(define (gnc:account-get-comm-balance-at-date account 
					      date include-children?)
  (let ((balance-collector
         (if include-children?
             (gnc:group-get-comm-balance-at-date
              (gnc:account-get-children account) date)
             (make-commodity-collector)))
        (query (gnc:malloc-query))
        (splits #f))

    (gnc:query-set-group query (gnc:get-current-group))
    (gnc:query-add-single-account-match query account 'query-and)
    (gnc:query-add-date-match-timepair query #f date #t date 'query-and) 
    (gnc:query-set-sort-order query 'by-date 'by-standard 'by-none)
    (gnc:query-set-sort-increasing query #t)
    (gnc:query-set-max-splits query 1)

    (set! splits (gnc:glist->list 
                  (gnc:query-get-splits query) 
                  <gnc:Split*>))
    (if (and splits (not (null? splits)))
	(balance-collector 'add (gnc:account-get-commodity account)
			   (gnc:split-get-balance (car splits))))
    balance-collector))

;; get the balance of a group of accounts at the specified date.
;; all children are included in the calculation
(define (gnc:group-get-balance-at-date group date)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (gnc:account-get-balance-at-date account date #t)) 
          group)))

;; returns a commodity-collector
(define (gnc:group-get-comm-balance-at-date group date)
  (let ((this-collector (make-commodity-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-accounts
	       (lambda (account)
		 (gnc:account-get-comm-balance-at-date 
		  account date #t)) 
	       group))
    this-collector))

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (d-gnc:account-get-balance-interval account from to include-children?)
  (- (d-gnc:account-get-balance-at-date account to include-children?)
     (d-gnc:account-get-balance-at-date account from include-children?)))

;; the version which returns a commodity-collector
(define (gnc:account-get-comm-balance-interval 
	 account from to include-children?)
  (let ((this-collector (gnc:account-get-comm-balance-at-date 
			 account to include-children?)))
    (this-collector 'minusmerge (gnc:account-get-comm-balance-at-date 
				 account from include-children?) #f)
    this-collector))

(define (d-gnc:group-get-balance-interval group from to)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (d-gnc:account-get-balance-interval account from to #t)) group)))

;; the version which returns a commodity-collector
(define (gnc:group-get-comm-balance-interval group from to)
  (let ((this-collector (make-commodity-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-accounts
	       (lambda (account)
		 (gnc:account-get-comm-balance-interval 
		  account from to #t)) group))
    this-collector))

(define (gnc:transaction-get-splits transaction)
  (let* ((num-splits (gnc:transaction-get-split-count transaction)))
    (let loop ((index 0))
      (if (= index num-splits)
	  '()
	  (cons
	   (gnc:transaction-get-split transaction index)
	   (loop (+ index 1)))))))

;; given one split, return the other splits in a transaction
(define (gnc:split-get-other-splits split)
  (let loop ((splits 
	      (gnc:transaction-get-splits (gnc:split-get-parent split))))
    (if (null? splits) 
	'()
	(if (equal? (car splits) split)
	    (loop (cdr splits))
	    (cons (car splits) (loop (cdr splits)))))))
