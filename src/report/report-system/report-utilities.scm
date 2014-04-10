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

(define (list-ref-safe list elt)
  (if (> (length list) elt)
      (list-ref list elt)
      #f))

(define (list-set-safe! l elt val)
  (if (and (list? l) (> (length l) elt))
      (list-set! l elt val)
      (let ((filler (list val)))
        (if (not (list? l))
            (set! l '()))
        (let loop ((i (length l)))
          (if (< i elt)
              (begin 
                (set! filler (cons #f filler))
                (loop (+ 1 i)))))
        (set! l (append! l filler))))
  l)

;; pair is a list of one gnc:commodity and one gnc:numeric
;; value. Deprecated -- use <gnc-monetary> instead.
(define (gnc:commodity-value->string pair)
  (gnc:amount->string 
   (cadr pair) (gnc:commodity-print-info (car pair) #t)))

;; Just for convenience. But in reports you should rather stick to the
;; style-info mechanism and simple plug the <gnc-monetary> into the
;; html-renderer.
(define (gnc:monetary->string value)
  (gnc:amount->string 
   (gnc:gnc-monetary-amount value) 
   (gnc:commodity-print-info (gnc:gnc-monetary-commodity value) #t)))

;; True if the account is of type currency, stock, or mutual-fund
(define (gnc:account-has-shares? account)
  ;; FYI: The val->sym function used to be called 
  ;; gw:enum-GNCAccountType-val->sym
  (let ((type (gw:enum-<gnc:AccountType>-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(stock mutual-fund currency))))

;; True if the account is of type stock or mutual-fund
(define (gnc:account-is-stock? account)
  (let ((type (gw:enum-<gnc:AccountType>-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(stock mutual-fund))))

;; True if the account is of type income or expense

(define (gnc:account-is-inc-exp? account)
  (let ((type (gw:enum-<gnc:AccountType>-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(income expense))))

;; Returns only those accounts out of the list <accounts> which have
;; one of the type identifiers in typelist.
(define (gnc:filter-accountlist-type typelist accounts)
  (filter (lambda (a) 
	    (member (gw:enum-<gnc:AccountType>-val->sym
		     (gnc:account-get-type a) #f)
		    typelist) )
	  accounts))

;; Decompose a given list of accounts 'accounts' into an alist
;; according to their types. Each element of alist is a list, whose
;; first element is the type-symbol, e.g. 'assets, and the rest (cdr)
;; of the element is the list of accounts which belong to that
;; category.
(define (gnc:decompose-accountlist accounts)
  (map (lambda (x) (cons
		    (car x)
		    (gnc:filter-accountlist-type (cdr x) accounts)))
       (list
	(cons 'asset
	      '(asset bank cash checking savings money-market 
		      stock mutual-fund currency))
	(cons 'liability '(liability credit credit-line))
	(cons 'equity '(equity))
	(cons 'income '(income))
	(cons 'expense '(expense)))))

;; Returns the name of the account type as a string, and in its plural
;; form (as opposed to gnc:account-get-type-string which gives the
;; singular form of the word).
(define (gnc:account-get-type-string-plural type)
  (assoc-ref
   (list 
    (cons 'bank (_ "Bank"))
    (cons 'cash (_ "Cash"))
    (cons 'credit (_ "Credits"))
    (cons 'asset (_ "Assets"))
    (cons 'liability (_ "Liabilities"))
    (cons 'stock (_ "Stocks"))
    (cons 'mutual-fund (_ "Mutual Funds"))
    (cons 'currency (_ "Currencies"))
    (cons 'income (_ "Income"))
    (cons 'expense (_ "Expenses"))
    (cons 'equity (_ "Equities"))
    (cons 'checking (_ "Checking"))
    (cons 'savings (_ "Savings"))
    (cons 'money-market (_ "Money Market"))
    (cons 'credit-line (_ "Credit Lines")))
   type))

;; Get the list of all different commodities that are used within the
;; 'accounts', excluding the 'exclude-commodity'.
(define (gnc:accounts-get-commodities accounts exclude-commodity)
  (delete exclude-commodity
	  (delete-duplicates
	   (sort (map gnc:account-get-commodity accounts) 
		 (lambda (a b) 
		   (string<? (gnc:commodity-get-mnemonic a)
			     (gnc:commodity-get-mnemonic b)))))))


;; Returns the depth of the current account hierarchy, that is, the
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

(define (gnc:split-get-corr-account-full-name split)
  (let ((separator (string-ref (gnc:account-separator-char) 0)))
    (gnc:split-get-corr-account-full-name-internal split separator)))


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

;; Get all children of this list of accounts.
(define (gnc:acccounts-get-all-subaccounts accountlist)
  (append-map 
   (lambda (a)
     (gnc:group-get-subaccounts
      (gnc:account-get-children a)))
   accountlist))


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

;; An IRC discussion on the performance of this: <cstim> rlb: I was
;; wondering which one would perform better: The directly stored
;; lambda in make-{stats,commodity}-collector, or just a value
;; somewhere with an exhaustive set of functions on it?  <rlb> cstim:
;; my guess in the long run, a goops object would be most appropriate,
;; and barring that, a record with a suitable set of functions defined
;; to manipulate it would be faster, but in the short run (i.e. until
;; we switch to requiring goops), it might not be worth changing.
;; However, a record for the data (or vector) and a set of 7 functions
;; would still be faster, if for no other reason than because you
;; don't have to do the "case" lookup.  That penalty can be avoided if
;; you follow the other strategy where passing in 'adder simply
;; returns the function, rather than calling it.  Then the user's code
;; can cache the value when repeated lookups would be expensive.  Also
;; everyone should note that in some tests Bill and I did here, plain
;; old alists were faster than hash tables until you got to a
;; reasonable size (i.e. greater than 10 elements, maybe greater than
;; 30...)  <cstim> rlb: hm, that makes sense. However, any change
;; would break existing code, so if I would go for speed optimization
;; I might just go for the record-and-function-set way.  <rlb> cstim:
;; yes.  I think that would still be faster.

(define (gnc:make-stats-collector)
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
	 (getnumitems (lambda () totalitems))
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
	  ('numitems (getnumitems))
	  ('getmax (getmax))
	  ('getmin (getmin))
	  ('reset (reset-all))
          (else (gnc:warn "bad stats-collector action: " action)))))))

(define (gnc:make-drcr-collector)
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
(define (gnc:make-value-collector)
  (let ;;; values
      ((value 0))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	('add (if (number? amount) 
		  (set! value (+ amount value))))
	('total value)
	(else (gnc:warn "bad value-collector action: " action))))))

;; Same as above but with gnc:numeric
(define (gnc:make-numeric-collector)
  (let ;;; values
      ((value (gnc:numeric-zero)))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	('add (if (gnc:gnc-numeric? amount) 
		  (set! value (gnc:numeric-add-fixed amount value))
		  (gnc:warn 
		   "gnc:numeric-collector called with wrong argument: " amount)))
	('total value)
	(else (gnc:warn "bad gnc:numeric-collector action: " action))))))

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

(define (gnc:make-commodity-collector)
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
	      (set! pair (list commodity (gnc:make-numeric-collector)))
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
    ;; existing, a list (gnc:commodity gnc:numeric). If the second
    ;; argument was #t, the sign gets reversed.
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
    ;; existing, a <gnc:monetary> value. If the second argument was
    ;; #t, the sign gets reversed.
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

;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
(define (gnc:account-get-balance-at-date account date include-children?)
  (let ((collector (gnc:account-get-comm-balance-at-date
                    account date include-children?)))
    (cadr (collector 'getpair (gnc:account-get-commodity account) #f))))

;; This works similar as above but returns a commodity-collector, 
;; thus takes care of children accounts with different currencies.
;;
;; Also note that the commodity-collector contains <gnc:numeric>
;; values rather than double values.
(define (gnc:account-get-comm-balance-at-date account 
					      date include-children?)
  (let ((balance-collector
         (if include-children?
             (gnc:group-get-comm-balance-at-date
              (gnc:account-get-children account) date)
             (gnc:make-commodity-collector)))
	  (query (gnc:malloc-query))
	  (splits #f))
      
      (gnc:query-set-group query (gnc:get-current-group))
      (gnc:query-add-single-account-match query account 'query-and)
      (gnc:query-add-date-match-timepair query #f date #t date 'query-and) 
      (gnc:query-set-sort-order query 'by-date 'by-standard 'by-none)
      (gnc:query-set-sort-increasing query #t #t #t)
      (gnc:query-set-max-splits query 1)
      
      (set! splits (gnc:glist->list 
		    (gnc:query-get-splits query) 
                  <gnc:Split*>))
      (gnc:free-query query)

      (if (and splits (not (null? splits)))
	(balance-collector 'add (gnc:account-get-commodity account)
			   (gnc:split-get-balance (car splits))))
      balance-collector))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. The reverse-balance-fn
;; (e.g. gnc:account-reverse-balance?) should return #t if the
;; account's balance sign should get reversed. Returns a
;; commodity-collector.
(define (gnc:accounts-get-balance-helper 
	 accounts get-balance-fn reverse-balance-fn)
  (let ((collector (gnc:make-commodity-collector)))
    (for-each 
     (lambda (acct)
       (collector (if (reverse-balance-fn acct)
		      'minusmerge 
		      'merge) 
		  (get-balance-fn acct) #f))
     accounts)
    collector))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Intended for usage with a profit and loss
;; report, hence a) only the income/expense accounts are regarded, and
;; b) the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-profit accounts 
					    get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type '(income expense) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the income accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-income accounts 
					    get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type '(income) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the expense accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-expense accounts 
                                             get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type '(expense) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Intended for usage with a balance sheet, hence
;; a) the income/expense accounts are ignored, and b) no signs are
;; reversed at all. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-assets accounts 
					    get-balance-fn)
  (gnc:accounts-get-balance-helper
   (filter (lambda (a) (not (gnc:account-is-inc-exp? a)))
	   accounts)
   get-balance-fn
   (lambda(x) #f)))

;; returns a commodity-collector
(define (gnc:group-get-comm-balance-at-date group date)
  (let ((this-collector (gnc:make-commodity-collector)))
    (for-each 
     (lambda (x) (this-collector 'merge x #f))
     (gnc:group-map-all-accounts
      (lambda (account)
	(gnc:account-get-comm-balance-at-date 
	 account date #f)) 
      group))
    this-collector))

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (gnc:account-get-balance-interval account from to include-children?)
  (let ((collector (gnc:account-get-comm-balance-interval
                    account from to include-children?)))
    (cadr (collector 'getpair (gnc:account-get-commodity account) #f))))

;; the version which returns a commodity-collector
(define (gnc:account-get-comm-balance-interval 
	 account from to include-children?)
  ;; Since this function calculates a balance difference it has to
  ;; subtract the balance of the previous day's end (from-date)
  ;; instead of the plain date.
  (let ((this-collector (gnc:account-get-comm-balance-at-date 
			 account to include-children?)))
    (this-collector
     'minusmerge (gnc:account-get-comm-balance-at-date
		  account
		  (gnc:timepair-end-day-time (gnc:timepair-previous-day from))
		  include-children?) #f)
    this-collector))

;; the version which returns a commodity-collector
(define (gnc:group-get-comm-balance-interval group from to)
  (let ((this-collector (gnc:make-commodity-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-all-accounts
	       (lambda (account)
		 (gnc:account-get-comm-balance-interval 
		  account from to #t)) group))
    this-collector))

;; utility function - ensure that a query matches only non-voids.  Destructive.
(define (gnc:query-set-match-non-voids-only! query group)
  (let ((temp-query (gnc:malloc-query)))
     (gnc:query-set-group temp-query group)
     
     (gnc:query-add-cleared-match
	     temp-query
	     'cleared-match-voided
	     'query-and)

     (set! temp-query (gnc:query-invert temp-query))

     (set! query (gnc:query-merge query temp-query 'query-and))))

;; utility function - ensure that a query matches only voids.  Destructive

(define (gnc:query-set-match-voids-only! query group)
  (let ((temp-query (gnc:malloc-query)))
     (gnc:query-set-group temp-query group)
     
     (gnc:query-add-cleared-match
	     temp-query
	     'cleared-match-voided
	     'query-and)

     (set! query (gnc:query-merge query temp-query 'query-and))))

(define (gnc:split-voided? split)
  (let ((trans (gnc:split-get-parent split)))
    (gnc:transaction-get-void-status trans)))