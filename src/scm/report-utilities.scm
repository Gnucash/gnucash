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

;; pair is a list of one gnc:commodity and one gnc:numeric
;; value. Deprecated -- use <gnc-monetary> instead.
(define (gnc:commodity-value->string pair)
  (gnc:commodity-amount->string 
   (cadr pair) (gnc:commodity-print-info (car pair) #t)))

;; Just for convenience. But in reports you should rather stick to the
;; style-info mechanism and simple plug the <gnc-monetary> into the
;; html-renderer.
(define (gnc:monetary->string value)
  (gnc:amount->string-helper 
   (gnc:gnc-monetary-amount value) 
   (gnc:commodity-print-info (gnc:gnc-monetary-amount value) #t)))

;; True if the account is of type currency, stock, or mutual-fund
(define (gnc:account-has-shares? account)
  ;; FYI: The val->sym function used to be called 
  ;; gw:enum-GNCAccountType-val->sym
  (let ((type (gw:enum-<gnc:AccountType>-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(stock mutual-fund currency))))

;; True if the account is of type income or expense
(define (gnc:account-is-inc-exp? account)
  (let ((type (gw:enum-<gnc:AccountType>-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(income expense))))

;; Returns the depth of the current account heirarchy, that is, the
;; maximum level of subaccounts in the current-group.
(define (gnc:get-current-group-depth)
  ;; Given a list of accounts, this function determines the maximum
  ;; sub-account level that there is.
  (define (accounts-get-children-depth accounts)
    (apply max
	   (map (lambda (acct)
		  (let ((children 
			 (gnc:account-get-immediate-subaccounts acct)))
		    (if (null? children)
			1
			(+ 1 (accounts-get-children-depth children)))))
		accounts)))
  (accounts-get-children-depth 
   (gnc:group-get-account-list (gnc:get-current-group))))

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

;; AFAIK the following stuff exists in SRFI. 02/01/2001
;
;;; returns a list contains elements of the-list for which predicate is true
;(define (gnc:filter-list the-list predicate)
;  (let loop ((rest the-list)
;             (collected '()))
;    (cond ((null? rest) (reverse collected))
;          (else (loop (cdr rest)
;                      (if (predicate (car rest))
;                          (cons (car rest) collected)
;                          collected))))))
;
;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.
;; note: map-in-order is in a SRFI.
;(define (gnc:inorder-map the-list fn)
;  (let loop ((rest the-list)
;             (collected '()))
;    (cond ((null? rest) (reverse collected))
;          (else (loop (cdr rest)
;                      (cons (fn (car rest)) collected))))))
;
;(define (gnc:for-loop thunk first last step)
;  (if (< first last) 
;      (begin
;	(thunk first)
;	(gnc:for-loop thunk (+ first step) last step))
;      #f))
;
;(define (gnc:map-for thunk first last step)
;  (if (< first last)
;      (cons
;	(thunk first)
;	(gnc:map-for thunk (+ first step) last step))
;      '()))

;; The following tasks shall be done with the query-api. 02/01/2001
;
;;; applies thunk to each split in account account
;(define (gnc:for-each-split-in-account account thunk)
;  (gnc:for-loop (lambda (x) 
;		  (thunk (gnc:account-get-split account x)))
;                0 (gnc:account-get-split-count account) 1))
;
;;; applies thunk to each split in account account
;(define (gnc:map-splits-in-account thunk account)
;  (gnc:map-for (lambda (x) 
;		 (thunk (gnc:account-get-split account x)))
;	       0 (gnc:account-get-split-count account) 1))

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
		  (set! value (gnc:numeric-add-fixed amount value))
		  (gnc:warn 
		   "numeric-collector called with wrong argument: " amount)))
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
;; But now USD is a <gnc:commodity*> and 123.4 a <gnc:numeric>, so
;; there is no simple example anymore.
;;
;; The functions:
;;   'add <commodity> <amount>: Add the given amount to the 
;;       appropriate currencies' total balance.
;;   'format <fn> #f: Call the function <fn> (where fn takes two
;;       arguments) for each commodity with the arguments <commodity>
;;       and the corresponding total <amount>. The results is a list
;;       of each call's result.
;;   'merge <commodity-collector> #f: Merge the given other
;;       commodity-collector into this one, adding all currencies'
;;       balances, respectively.
;;   'minusmerge <commodity-collector> #f: Merge the given other 
;;       commodity-collector into this one (like above) but subtract
;;       the other's currencies' balance from this one's balance,
;;       respectively.
;;   'reset #f #f: Delete everything that has been accumulated 
;;       (even the fact that any commodity showed up at all).
;;   'getpair <commodity> signreverse?: Returns the two-element-list
;;       with the <commodity> and its corresponding balance. If
;;       <commodity> doesn't exist, the balance will be
;;       (gnc:numeric-zero). If signreverse? is true, the result's
;;       sign will be reversed.
;;   (internal) 'list #f #f: get the association list of 
;;       commodity->numeric-collector

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
				       (gnc:numeric-neg
					((cadar clist) 'total #f)))
		  (minus-commodity-clist (cdr clist)))))

    ;; helper function walk the association list doing a callback on
    ;; each key-value pair.
    (define (process-commodity-list fn clist)
      (map 
       (lambda (pair) (fn (car pair) ((cadr pair) 'total #f)))
       clist))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a list (gnc:commodity gnc:numeric) 
    (define (getpair c sign?)
      (let ((pair (assoc c commoditylist)))
	(cons c (cons 
	      (if (not pair)
		  (gnc:numeric-zero)
		  (if sign?
		      (gnc:numeric-neg ((cadr pair) 'total #f))
		      ((cadr pair) 'total #f)))
	      '()))))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a <gnc:monetary> value.
    (define (getmonetary c sign?)
      (let ((pair (assoc c commoditylist)))
	(gnc:make-gnc-monetary
	 c (if (not pair)
	       (gnc:numeric-zero)
	       (if sign?
		   (gnc:numeric-neg ((cadr pair) 'total #f))
		   ((cadr pair) 'total #f))))))
    
    ;; Dispatch function
    (lambda (action commodity amount)
      (case action
	('add (add-commodity-value commodity amount))
	('merge (add-commodity-clist (commodity 'list #f #f)))
	('minusmerge (minus-commodity-clist (commodity 'list #f #f)))
	('format (process-commodity-list commodity commoditylist))
	('reset (set! commoditylist '()))
	('getpair (getpair commodity amount))
	('getmonetary (getmonetary commodity amount))
	('list commoditylist) ; this one is only for internal use
	(else (gnc:warn "bad commodity-collector action: " action))))))

;; Nobody uses the following functions. 02/01/2001
;;; Add x to list lst if it is not already in there
;(define (addunique lst x)
;  (if (null? lst)  
;      (list x)		; all checked add it
;      (if (equal? x (car lst))
;	  lst	; found, quit search and don't add again
;	  (cons (car lst) (addunique (cdr lst) x))))) ; keep searching
;
;;; get transaction date from split - needs to be done indirectly
;;; as it's stored in the parent transaction
;(define (gnc:split-get-transaction-date split)
;  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

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
    (gnc:free-query query)

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
    (gnc:free-query query)

    (if (and splits (not (null? splits)))
	(balance-collector 'add (gnc:account-get-commodity account)
			   (gnc:split-get-balance (car splits))))
    balance-collector))

;; get the balance of a group of accounts at the specified date.
;; The childrens are NOT included in the calculation since
;; account-get-children already returned ALL children, whether
;; they are immediate children or not.
(define (gnc:group-get-balance-at-date group date)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (gnc:account-get-balance-at-date account date #f)) 
          group)))

;; returns a commodity-collector
(define (gnc:group-get-comm-balance-at-date group date)
  (let ((this-collector (make-commodity-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-accounts
	       (lambda (account)
		 (gnc:account-get-comm-balance-at-date 
		  account date #f)) 
	       group))
    this-collector))

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (gnc:account-get-balance-interval account from to include-children?)
  ;; FIXME: the get-balance-interval function uses this date
  ;; rightaway, but since it calculates a difference it should
  ;; rather take the end-day-time of one day before that. This
  ;; needs to be fixed in report-utilities.scm.
  (- (gnc:account-get-balance-at-date account to include-children?)
     (gnc:account-get-balance-at-date account from include-children?)))

;; the version which returns a commodity-collector
(define (gnc:account-get-comm-balance-interval 
	 account from to include-children?)
  (let ((this-collector (gnc:account-get-comm-balance-at-date 
			 account to include-children?)))
    (this-collector 'minusmerge (gnc:account-get-comm-balance-at-date 
				 account from include-children?) #f)
    this-collector))

(define (gnc:group-get-balance-interval group from to)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (gnc:account-get-balance-interval account from to #t)) group)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In progress: A suggested function to calculate the weighted average
;; exchange rate between all commodities and the
;; report-commodity. Uses all currency transactions up until the
;; end-date. Returns an alist, see sumlist.
(define (gnc:get-exchange-totals report-commodity end-date)
  (let ((curr-accounts 
	 (filter gnc:account-has-shares? (gnc:group-get-subaccounts
					  (gnc:get-current-group))))
	(query (gnc:malloc-query))
	(splits #f)
	;; sumlist: a multilevel association list. Each element has a
	;; commodity as key, and another alist as a value. The
	;; value-alist's elements consist of a commodity as a key, and
	;; a pair of two value-collectors as value, e.g. with only the
	;; report-commodity DEM in the outer alist: ( {DEM ( [USD (400
	;; .  1000)] [FRF (300 . 100)] ) } ) where DEM,USD,FRF are
	;; <gnc:commodity> and the numbers are a value-collector which
	;; in turn store a <gnc:numeric>. In the example, USD 400 were
	;; bought for an amount of DEM 1000, FRF 300 were bought for
	;; DEM 100. The reason for the outer alist is that there might
	;; be commodity transactions which do not involve the
	;; report-commodity, but which can still be calculated after
	;; *all* transactions are processed.
	(sumlist (list (list report-commodity '()))))

    (define (make-newrate unknown-coll un->known-coll known-pair)
      (let ((a (make-numeric-collector))
	    (b (make-numeric-collector)))
	(a 'add (unknown-coll 'total #f))
	(b 'add 
	   (gnc:numeric-div
	    (gnc:numeric-mul 
	     (un->known-coll 'total #f) 
	     ((cdadr known-pair) 'total #f)
	     0 GNC-DENOM-REDUCE)
	    ((caadr known-pair) 'total #f)
	    0 GNC-DENOM-REDUCE))
	;; in other words: (/ (* (caadr c) (cdadr pair-b)) (caadr
	;; pair-b) ))
	(cons a b)))

    (if (not (null? curr-accounts))
	(begin
	  (gnc:query-set-group query (gnc:get-current-group))
	  (gnc:query-add-account-match 
	   query (gnc:list->glist curr-accounts)
	   'acct-match-any 'query-and)
	  (gnc:query-add-date-match-timepair 
	   query #f end-date #t end-date 'query-and) 
	  
	  ;; Get the query result, i.e. all splits in currency
	  ;; accounts.
	  (set! splits (filter 
			;; Filter such that we get only those splits
			;; which have two *different* commodities
			;; involved.
			(lambda (s) (not (gnc:commodity-equiv? 
					  (gnc:transaction-get-commodity 
					   (gnc:split-get-parent s))
					  (gnc:account-get-commodity 
					   (gnc:split-get-account s)))))
			(gnc:glist->list 
			 (gnc:query-get-splits query) 
			 <gnc:Split*>)))
	  (gnc:free-query query)

;	  (warn "report-commodity is " 
;		(gnc:commodity-value->string 
;		 (list report-commodity (gnc:numeric-zero)))
;		report-commodity)
	  
	  ;; Now go through all splits and add up all value-amounts
	  ;; and share-amounts
	  (for-each 
	   (lambda (a)
	     (let* ((transaction-comm (gnc:transaction-get-commodity 
				       (gnc:split-get-parent a)))
		    (account-comm (gnc:account-get-commodity 
				   (gnc:split-get-account a)))
		    (share-amount (gnc:split-get-share-amount a))
		    (value-amount (gnc:split-get-value a))
		    (tmp (assoc transaction-comm sumlist))
		    (comm-list 
		     (if (not tmp) 
			 (begin
;			   (warn "not found " 
;				 (gnc:commodity-value->string 
;				  (list account-comm share-amount))
;				 ", trying "
;				 (gnc:commodity-value->string 
;				  (list transaction-comm value-amount))
;				 sumlist)
			   (assoc account-comm sumlist))
			 tmp)))
;	       (if comm-list
;		   (warn "looking for toplevel comm: " 
;			 (gnc:commodity-value->string 
;			  (list (car comm-list) (gnc:numeric-zero))))
;		   (warn "not comm-list"))
		   
	       
	       ;; entry exists already in comm-list?
	       (if (not comm-list)
		   ;; no, create sub-alist from scratch
		   (let ((pair (list transaction-comm
				     (cons (make-numeric-collector)
					   (make-numeric-collector)))))
;		     (warn "XX " (gnc:commodity-value->string 
;			    (list transaction-comm value-amount))
;			   (gnc:commodity-value->string 
;			    (list account-comm share-amount)))
		     ((caadr pair) 'add value-amount)
		     ((cdadr pair) 'add share-amount)
		     (set! comm-list (list account-comm (list pair)))
		     (set! sumlist (cons comm-list sumlist)))
		   ;; yes, check for second currency.
		   (let* 
		       ((foreignlist 
			 ;; this will adjust the signs appropriately
			 (if (gnc:commodity-equiv? transaction-comm
						   (car comm-list))
			     (list account-comm 
				   (gnc:numeric-neg share-amount)
				   (gnc:numeric-neg value-amount))
			     (list transaction-comm 
				   value-amount 
				   share-amount)))
			;; second commodity already existing in comm-list?
			(pair (assoc (car foreignlist) (cadr comm-list))))
;		     (warn "current transaction "
;			   (gnc:commodity-value->string 
;			    (list (car foreignlist) (cadr foreignlist)))
;			   (gnc:commodity-value->string 
;			    (list (car comm-list) (caddr foreignlist))))
		     ;; if not, create a new entry in comm-list.
		     (if (not pair)
			 (begin
			   ;;(warn "ZZ ")
			   (set! 
			    pair (list (car foreignlist)
				       (cons (make-numeric-collector) 
					     (make-numeric-collector))))
			   (set! 
			    comm-list (list (car comm-list) 
					    (cons pair (cadr comm-list))))
			   (set! 
			    sumlist (cons comm-list 
					  (alist-delete 
					   (car comm-list) sumlist)))))
		     ;;(display (gnc:commodity-value->string (list (car
		     ;;foreignlist) (cadr foreignlist)))
		     ;;(gnc:commodity-value->string (list report-commodity
		     ;;(caddr foreignlist)))))))
		     ((caadr pair) 'add (cadr foreignlist))
		     ;;(warn "ZZ6 " sumlist)
		     ((cdadr pair) 'add (caddr foreignlist))))))
	   splits)))

    ;; Now go through all additional toplevel non-report-commodity
    ;; balances and add them to report-commodity, if possible.
    (let ((reportlist (cadr (assoc report-commodity sumlist))))
      (for-each
       (lambda (l)
	 (if (not (gnc:commodity-equiv? (car l) report-commodity))
	     (for-each
	      (lambda (c)
		(let ((pair-a (assoc (car l) reportlist))
		       (pair-b (assoc (car c) reportlist))
		       (rate (gnc:numeric-zero)))
		  (if (and (not pair-a) (not pair-b))
		      (warn "can't calculate rate for"
			    (gnc:commodity-value->string 
			     (list (car c) (caadr c)))
			    " = "
			    (gnc:commodity-value->string 
			     (list (car l) (cdadr c)))
			    " to "
			    (gnc:commodity-value->string 
			     (list report-commodity (gnc:numeric-zero))))
		      (if (and pair-a pair-b)
			  (warn "Oops - what went wrong? Both are found:"
				(gnc:commodity-value->string 
				 (list (car c) (caadr c)))
				" = "
				(gnc:commodity-value->string 
				 (list (car l) (cdadr c))))
			  (let 
			      ((newrate
				(if (not pair-a)
				    (list (car l)
					  (make-newrate (cdadr c) 
							(caadr c) pair-b))
				    (list (car c)
					  (make-newrate (caadr c) 
							(cdadr c) pair-a)))))
;			    (warn "created new rate"
;				  (gnc:commodity-value->string 
;				   (list (car newrate) 
;					 ((caadr newrate) 'total #f)))
;				  " = "
;				  (gnc:commodity-value->string 
;				   (list report-commodity 
;					 ((cdadr newrate) 'total #f))))
			    (set! reportlist (cons newrate reportlist)))))))
	      (cadr l))))
       sumlist)

      reportlist)))

;; Anybody feel free to reimplement any of these functions, either in
;; scheme or in C. -- cstim

(define (gnc:make-exchange-alist report-commodity end-date)
  ;; This returns the alist with the actual exchange rates, i.e. the
  ;; total balances from get-exchange-totals are divided by each
  ;; other.
  (map 
   (lambda (e)
     (begin
       ;;(display (gnc:commodity-value->string (list (car e) ((caadr
       ;;e) 'total #f))) (gnc:commodity-value->string (list
       ;;report-commodity ((cdadr e) 'total #f))))
;       (warn "rate"
;	     (gnc:commodity-value->string 
;	      (list (car e) 
;		    ((caadr e) 'total #f)))
;	     " = "
;	     (gnc:commodity-value->string 
;	      (list report-commodity 
;		    ((cdadr e) 'total #f))))
       
       (list (car e) 
	     (gnc:numeric-abs
	      (gnc:numeric-div ((cdadr e) 'total #f) 
			       ((caadr e) 'total #f)
			       ;; 0 stands for GNC_DENOM_AUTO
			       0
			       GNC-DENOM-REDUCE)))))
   (gnc:get-exchange-totals report-commodity end-date)))

;; This one returns the ready-to-use function for calculation of the
;; exchange rates. The returned function in turn returns a pair
;; commodity - value which instantly can be plugged into
;; gnc:commodity-amount->string .
(define (gnc:make-exchange-function exchange-alist)
  (let ((exchangelist exchange-alist))
    (lambda (foreign-pair domestic)
      (cons domestic
	    (cons 
	     (let ((pair (assoc (car foreign-pair) exchangelist)))
	       (if (not pair)
		   (gnc:numeric-zero)
		   (gnc:numeric-mul (cadr foreign-pair) (cadr pair)
				    ;; FIXME: the constant 100 here is
				    ;; not a durable solution
				    100 GNC-RND-ROUND)))
	     '())))))

;; Adds all different commodities in the commodity-collector <foreign>
;; by using the exchange rates of <exchange-fn> to calculate the
;; exchange rates to the commodity <domestic>. Returns the
;; two-element-list with the domestic commodity and its corresponding
;; balance, like (gnc:commodity* gnc:numeric).
(define (gnc:sum-collector-commodity foreign domestic exchange-fn)
  (let ((balance (make-commodity-collector)))
    (foreign
     'format 
     (lambda (curr val) 
       (if (gnc:commodity-equiv? domestic curr)
	   (balance 'add domestic val)
	   (balance 'add domestic 
		    (cadr (exchange-fn (list curr val) domestic)))))
     #f)
    (balance 'getmonetary domestic #f)))


;; These are just a bunch of options which were useful in several
;; reports and hence they got defined in a seperate function.

;; This is one single end-date of a report.
(define (gnc:options-add-report-date!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-date-option
     pagename optname 
     sort-tag (_ "Select a date to report on")
     (lambda ()
       (cons 'absolute 
	     (gnc:timepair-end-day-time     
	      (gnc:secs->timepair 
	       (car (mktime (localtime (current-time))))))))
     #f 'absolute #f)))

;; This is a date-interval for a report.
(define (gnc:options-add-date-interval!
	 options pagename name-from name-to sort-tag)
  (begin
    (gnc:register-option 
     options  
     (gnc:make-date-option
      pagename name-from 
      (string-append sort-tag "a")
      (_ "Start of reporting period")
      (lambda ()
	(cons 'absolute 
	      (gnc:get-start-cal-year)))
      #f 'absolute #f))
    (gnc:register-option 
     options  
     (gnc:make-date-option
      pagename name-to
      (string-append sort-tag "b")
      (_ "End of reporting period")
      (lambda ()
	(cons 'absolute 
	      (gnc:timepair-end-day-time     
	       (gnc:secs->timepair 
		(car (mktime (localtime (current-time))))))))
      #f 'absolute #f))))

;; These help for selecting a bunch of accounts.
(define (gnc:options-add-account-selection! 
	 options pagename 
	 name-display-depth name-show-subaccounts name-accounts
	 sort-tag default-depth default-accounts)
  (begin
    (gnc:register-option 
     options  
     (gnc:make-multichoice-option
      pagename name-display-depth
      (string-append sort-tag "a")
      (_ "Show accounts to this depth, overriding any other option.") 
      default-depth
      (list (list->vector
	     (list 'all (_ "All") (_ "Show all accounts")))
	    (list->vector
	     (list 1 "1" (_ "Top-level")))
	    (list->vector
	     (list 2 "2" (_ "Second-level")))
	    (list->vector
	     (list 3 "3" (_ "Third-level")))
	    (list->vector
	     (list 4 "4" (_ "Fourth-level")))
	    (list->vector
	     (list 5 "5" (_ "Fifth-level"))))))
    
    (gnc:register-option 
     options  
     (gnc:make-simple-boolean-option
      pagename name-show-subaccounts
      (string-append sort-tag "b")
      (_ "Override account-selection and show sub-accounts of all selected accounts?") 
      #t))

    ;; Semantics of the account selection, as used in the
    ;; gnc:html-build-acct-table: An account shows up if ( the
    ;; tree-depth is large enough AND ( it is selected in the account
    ;; selector OR ( always show sub-accounts is selected AND one of
    ;; the parents is selected in the account selector. )))
    (gnc:register-option 
     options  
     (gnc:make-account-list-option
      pagename name-accounts
      (string-append sort-tag "c")
      (_ "Report on these accounts, if display depth allows.")
      default-accounts
      #f #t))))

;; The single checkbox whether to include the sub-account balances
;; into the other balances.
(define (gnc:options-add-include-subaccounts!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-simple-boolean-option
     pagename optname
     sort-tag (_ "Include sub-account balances in printed balance?") #t)))

;; These are common options for the selection of the report's
;; currency/commodity.
(define (gnc:options-add-currency-selection!
	 options pagename 
	 name-show-foreign name-report-currency sort-tag)
  (begin
    (gnc:register-option 
     options 
     (gnc:make-simple-boolean-option
      pagename name-show-foreign
      (string-append sort-tag "a")
      (_ "Display the account's foreign currency amount?") #f))
    
    (gnc:register-option 
     options 
     (gnc:make-currency-option 
      pagename name-report-currency
      (string-append sort-tag "b")
      (_ "All other currencies will get converted to this currency.")
      (gnc:locale-default-currency)))))

