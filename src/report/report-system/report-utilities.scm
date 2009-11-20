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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org

(use-modules (srfi srfi-13))

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
(define (gnc-commodity-value->string pair)
  (xaccPrintAmount
   (cadr pair) (gnc-commodity-print-info (car pair) #t)))

;; Just for convenience. But in reports you should rather stick to the
;; style-info mechanism and simple plug the <gnc-monetary> into the
;; html-renderer.
(define (gnc:monetary->string value)
  (xaccPrintAmount
   (gnc:gnc-monetary-amount value) 
   (gnc-commodity-print-info (gnc:gnc-monetary-commodity value) #t)))

;; True if the account is of type currency, stock, or mutual-fund
(define (gnc:account-has-shares? account)
  (let ((type (xaccAccountGetType account)))
    (member type (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL ACCT-TYPE-CURRENCY))))

;; True if the account is of type stock or mutual-fund
(define (gnc:account-is-stock? account)
  (let ((type (xaccAccountGetType account)))
    (member type (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL))))

;; True if the account is of type income or expense

(define (gnc:account-is-inc-exp? account)
  (let ((type (xaccAccountGetType account)))
    (member type (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE))))

;; Returns only those accounts out of the list <accounts> which have
;; one of the type identifiers in typelist.
(define (gnc:filter-accountlist-type typelist accounts)
  (filter (lambda (a) 
	    (member (xaccAccountGetType a) typelist))
	  accounts))

;; Decompose a given list of accounts 'accounts' into an alist
;; according to their types. Each element of alist is a list, whose
;; first element is the type, e.g. ACCT-TYPE-ASSET, and the rest (cdr)
;; of the element is the list of accounts which belong to that
;; category.
(define (gnc:decompose-accountlist accounts)
  (map (lambda (x) (cons
		    (car x)
		    (gnc:filter-accountlist-type (cdr x) accounts)))
       (list
	(cons ACCT-TYPE-ASSET
	      (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH
                    ACCT-TYPE-CHECKING ACCT-TYPE-SAVINGS
                    ACCT-TYPE-MONEYMRKT ACCT-TYPE-RECEIVABLE
                    ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
                    ACCT-TYPE-CURRENCY))
	(cons ACCT-TYPE-LIABILITY
              (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
                    ACCT-TYPE-CREDITLINE))
	(cons ACCT-TYPE-EQUITY (list ACCT-TYPE-EQUITY))
	(cons ACCT-TYPE-INCOME (list ACCT-TYPE-INCOME))
	(cons ACCT-TYPE-EXPENSE (list ACCT-TYPE-EXPENSE))
	(cons ACCT-TYPE-TRADING (list ACCT-TYPE-TRADING)))))

;; Returns the name of the account type as a string, and in its plural
;; form (as opposed to xaccAccountGetTypeStr which gives the
;; singular form of the word).
(define (gnc:account-get-type-string-plural type)
  (assoc-ref
   (list 
    (cons ACCT-TYPE-BANK (_ "Bank"))
    (cons ACCT-TYPE-CASH (_ "Cash"))
    (cons ACCT-TYPE-CREDIT (_ "Credits"))
    (cons ACCT-TYPE-ASSET (_ "Assets"))
    (cons ACCT-TYPE-LIABILITY (_ "Liabilities"))
    (cons ACCT-TYPE-STOCK (_ "Stocks"))
    (cons ACCT-TYPE-MUTUAL (_ "Mutual Funds"))
    (cons ACCT-TYPE-CURRENCY (_ "Currencies"))
    (cons ACCT-TYPE-INCOME (_ "Income"))
    (cons ACCT-TYPE-EXPENSE (_ "Expenses"))
    (cons ACCT-TYPE-EQUITY (_ "Equities"))
    (cons ACCT-TYPE-CHECKING (_ "Checking"))
    (cons ACCT-TYPE-SAVINGS (_ "Savings"))
    (cons ACCT-TYPE-MONEYMRKT (_ "Money Market"))
    (cons ACCT-TYPE-RECEIVABLE (_ "Accounts Receivable"))
    (cons ACCT-TYPE-PAYABLE (_ "Accounts Payable"))
    (cons ACCT-TYPE-CREDITLINE (_ "Credit Lines"))
    (cons ACCT-TYPE-TRADING (_ "Trading Accounts")))
   type))

;; Get the list of all different commodities that are used within the
;; 'accounts', excluding the 'exclude-commodity'.
(define (gnc:accounts-get-commodities accounts exclude-commodity)
  (delete exclude-commodity
	  (delete-duplicates
	   (sort (map xaccAccountGetCommodity accounts)
		 (lambda (a b) 
		   (string<? (or (gnc-commodity-get-mnemonic a) "")
			     (or (gnc-commodity-get-mnemonic b) "")))))))


;; Returns the depth of the current account hierarchy, that is, the
;; maximum level of subaccounts in the tree
(define (gnc:get-current-account-tree-depth)
  (let ((root (gnc-get-current-root-account)))
    (gnc-account-get-tree-depth root)))

(define (gnc:split-get-corr-account-full-name split)
  (xaccSplitGetCorrAccountFullName split))


;; Get all children of this list of accounts.
(define (gnc:acccounts-get-all-subaccounts accountlist)
  (append-map 
   (lambda (a)
     (gnc-account-get-descendants-sorted a))
   accountlist))

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
	  ((add) (adder value))
	  ((total) (gettotal))
	  ((average) (getaverage))
	  ((numitems) (getnumitems))
	  ((getmax) (getmax))
	  ((getmin) (getmin))
	  ((reset) (reset-all))
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
	  ((add) (adder value))
	  ((debits) (getdebits))
	  ((credits) (getcredits))
	  ((items) (getitems))
	  ((reset) (reset-all))
          (else (gnc:warn "bad dr-cr-collector action: " action)))))))

;; This is a collector of values -- works similar to the stats-collector but
;; has much less overhead. It is used by the currency-collector (see below).
(define (gnc:make-value-collector)
  (let ;;; values
      ((value 0))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	((add) (if (number? amount) 
		  (set! value (+ amount value))))
	((total) value)
	(else (gnc:warn "bad value-collector action: " action))))))
;; Bah. Let's get back to normal data types -- this procedure thingy
;; from above makes every code almost unreadable. First step: replace
;; all 'action function calls by the normal functions below.
(define (gnc:value-collector-add collector amount)
  (collector 'add amount))
(define (gnc:value-collector-total collector)
  (collector 'total #f))


;; Same as above but with gnc:numeric
(define (gnc:make-numeric-collector)
  (let ;;; values
      ((value (gnc-numeric-zero)))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	((add) (if (gnc:gnc-numeric? amount) 
                   (set! value (gnc-numeric-add amount value
                                                GNC-DENOM-AUTO GNC-DENOM-LCD))
                   (gnc:warn 
                    "gnc:numeric-collector called with wrong argument: "
                    amount)))
	((total) value)
	(else (gnc:warn "bad gnc:numeric-collector action: " action))))))

;; Replace all 'action function calls by the normal functions below.
(define (gnc:numeric-collector-add collector amount)
  (collector 'add amount))
(define (gnc:numeric-collector-total collector)
  (collector 'total #f))

;; A commodity collector. This is intended to handle multiple
;; currencies' amounts. The amounts are accumulated via 'add, the
;; result can be fetched via 'format.  This used to work with strings
;; as currencies and doubles as values, but now it uses
;; <gnc:commodity*> as commodity and <gnc:numeric> as value.
;;
;; Old Example: (define a (make-commodity-collector)) ...  (a 'add 'USD
;; 12) ...  (a 'format (lambda(x y)(list x y)) #f) used to give you
;; something like ((USD 123.4) (DEM 12.21) (FRF -23.32))
;;
;; New Example: But now USD is a <gnc:commodity*> and 123.4 a
;; <gnc:numeric>, so there is no simple example anymore.
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
;;       (gnc-numeric-zero). If signreverse? is true, the result's
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
	(gnc:numeric-collector-add (cadr pair) value)))
    
    ;; helper function to walk an association list, adding each
    ;; (commodity -> collector) pair to our list at the appropriate 
    ;; place
    (define (add-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value 
		   (caar clist) 
		   (gnc:numeric-collector-total (cadar clist)))
		  (add-commodity-clist (cdr clist)))))

    (define (minus-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value 
		   (caar clist) 
		   (gnc-numeric-neg
		    (gnc:numeric-collector-total (cadar clist))))
		  (minus-commodity-clist (cdr clist)))))

    ;; helper function walk the association list doing a callback on
    ;; each key-value pair.
    (define (process-commodity-list fn clist)
      (map 
       (lambda (pair) (fn (car pair) 
			  (gnc:numeric-collector-total (cadr pair))))
       clist))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a list (gnc:commodity gnc:numeric). If the second
    ;; argument was #t, the sign gets reversed.
    (define (getpair c sign?)
      (let ((pair (assoc c commoditylist)))
	(cons c (cons 
	      (if (not pair)
		  (gnc-numeric-zero)
		  (if sign?
		      (gnc-numeric-neg
		       (gnc:numeric-collector-total (cadr pair)))
		      (gnc:numeric-collector-total (cadr pair))))
	      '()))))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a <gnc:monetary> value. If the second argument was
    ;; #t, the sign gets reversed.
    (define (getmonetary c sign?)
      (let ((pair (assoc c commoditylist)))
	(gnc:make-gnc-monetary
	 c (if (not pair)
	       (gnc-numeric-zero)
	       (if sign?
		   (gnc-numeric-neg
		    (gnc:numeric-collector-total (cadr pair)))
		   (gnc:numeric-collector-total (cadr pair)))))))
    
    ;; Dispatch function
    (lambda (action commodity amount)
      (case action
	((add) (add-commodity-value commodity amount))
	((merge) (add-commodity-clist 
		 (gnc-commodity-collector-list commodity)))
	((minusmerge) (minus-commodity-clist
		      (gnc-commodity-collector-list commodity)))
	((format) (process-commodity-list commodity commoditylist))
	((reset) (set! commoditylist '()))
	((getpair) (getpair commodity amount))
	((getmonetary) (getmonetary commodity amount))
	((list) commoditylist) ; this one is only for internal use
	(else (gnc:warn "bad commodity-collector action: " action))))))

(define (gnc:commodity-collector-get-negated collector)
  (let
    ((negated (gnc:make-commodity-collector)))
    (negated 'minusmerge collector #f)
    negated))

(define (gnc:commodity-collectorlist-get-merged collectorlist)
  (let
    ((merged (gnc:make-commodity-collector)))
    (for-each (lambda (collector) (merged 'merge collector #f)) collectorlist)
    merged))

;; Bah. Let's get back to normal data types -- this procedure thingy
;; from above makes every code almost unreadable. First step: replace
;; all 'action function calls by the normal functions below.

;; CAS: ugh.  Having two usages is even *more* confusing, so let's
;; please settle on one or the other.  What's Step 2?  How 'bout
;; documenting the new functions?
(define (gnc-commodity-collector-add collector commodity amount)
  (collector 'add commodity amount))
(define (gnc-commodity-collector-merge collector other-collector)
  (collector 'merge other-collector #f))
(define (gnc-commodity-collector-minusmerge collector other-collector)
  (collector 'minusmerge other-collector #f))
(define (gnc-commodity-collector-map collector function)
  (collector 'format function #f))
(define (gnc-commodity-collector-assoc collector commodity sign?)
  (collector 'getmonetary commodity sign?))
(define (gnc-commodity-collector-assoc-pair collector commodity sign?)
  (collector 'getpair commodity sign?))
(define (gnc-commodity-collector-list collector)
  (collector 'list #f #f))

;; Returns zero if all entries in this collector are zero.
(define (gnc-commodity-collector-allzero? collector)
  (let ((result #t))
    (gnc-commodity-collector-map
     collector
     (lambda (commodity amount)
       (if (not (gnc-numeric-zero-p amount))
	   (set! result #f))))
    result))


;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
(define (gnc:account-get-balance-at-date account date include-children?)
  (let ((collector (gnc:account-get-comm-balance-at-date
                    account date include-children?)))
    (cadr (gnc-commodity-collector-assoc-pair
	   collector (xaccAccountGetCommodity account) #f))))

;; This works similar as above but returns a commodity-collector, 
;; thus takes care of children accounts with different currencies.
;;
;; Also note that the commodity-collector contains <gnc:numeric>
;; values rather than double values.
(define (gnc:account-get-comm-balance-at-date account 
					      date include-children?)
  (let ((balance-collector (gnc:make-commodity-collector))
	(query (qof-query-create-for-splits))
	(splits #f))

      (if include-children?
	  (for-each 
	   (lambda (x) 
	     (gnc-commodity-collector-merge balance-collector x))
	   (gnc:account-map-descendants
	    (lambda (child)
	      (gnc:account-get-comm-balance-at-date child date #f))
	    account)))

      (qof-query-set-book query (gnc-get-current-book))
      (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
      (xaccQueryAddDateMatchTS query #f date #t date QOF-QUERY-AND)
      (qof-query-set-sort-order query
				(list SPLIT-TRANS TRANS-DATE-POSTED)
				(list QUERY-DEFAULT-SORT)
				'())
      (qof-query-set-sort-increasing query #t #t #t)
      (qof-query-set-max-results query 1)
      
      (set! splits (qof-query-run query))
      (qof-query-destroy query)

      (if (and splits (not (null? splits)))
	  (gnc-commodity-collector-add balance-collector
				       (xaccAccountGetCommodity account)
				       (xaccSplitGetBalance (car splits))))
      balance-collector))

;; Calculate the increase in the balance of the account in terms of
;; "value" (as opposed to "amount") between the specified dates.
;; If include-children? is true, the balances of all children (not
;; just direct children) are are included in the calculation. The results
;; are returned in a commodity collector.
(define (gnc:account-get-comm-value-interval account start-date end-date
                                                include-children?)
  (let ((value-collector (gnc:make-commodity-collector))
	(query (qof-query-create-for-splits))
	(splits #f))

    (if include-children?
        (for-each
         (lambda (x)
           (gnc-commodity-collector-merge value-collector x))
         (gnc:account-map-descendants
          (lambda (d)
            (gnc:account-get-comm-value-interval d start-date end-date #f))
          account)))

    ;; Build a query to find all splits between the indicated dates.
    (qof-query-set-book query (gnc-get-current-book))
    (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS query
                             (and start-date #t) start-date
                             (and end-date #t) end-date
                             QOF-QUERY-AND)

    ;; Get the query results.
    (set! splits (qof-query-run query))
    (qof-query-destroy query)

    ;; Add the "value" of each split returned (which is measured
    ;; in the transaction currency).
    (for-each
     (lambda (split)
       (value-collector 'add
                        (xaccTransGetCurrency (xaccSplitGetParent split))
                        (xaccSplitGetValue split)))
     splits)

    value-collector))

;; Calculate the balance of the account in terms of "value" (rather
;; than "amount") at the specified date. If include-children? is
;; true, the balances of all children (not just direct children) are
;; are included in the calculation. The results are returned in a
;; commodity collector.
(define (gnc:account-get-comm-value-at-date account date include-children?)
  (gnc:account-get-comm-value-interval account #f date include-children?))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. The reverse-balance-fn
;; (e.g. gnc-reverse-balance) should return #t if the
;; account's balance sign should get reversed. Returns a
;; commodity-collector.
(define (gnc:accounts-get-balance-helper 
	 accounts get-balance-fn reverse-balance-fn)
  (let ((collector (gnc:make-commodity-collector)))
    (for-each 
     (lambda (acct)
       ((if (reverse-balance-fn acct)
	    gnc-commodity-collector-minusmerge
	    gnc-commodity-collector-merge)
	collector (get-balance-fn acct)))
     accounts)
    collector))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Intended for usage with a profit and loss
;; report, hence a) only the income/expense accounts are regarded, and
;; b) the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-profit accounts 
					    get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the income accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-income accounts 
					    get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type (list ACCT-TYPE-INCOME) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the expense accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-expense accounts 
                                             get-balance-fn)
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type (list ACCT-TYPE-EXPENSE) accounts)
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

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (gnc:account-get-balance-interval account from to include-children?)
  (let ((collector (gnc:account-get-comm-balance-interval
                    account from to include-children?)))
    (cadr (gnc-commodity-collector-assoc-pair
	   collector (xaccAccountGetCommodity account) #f))))

;; the version which returns a commodity-collector
(define (gnc:account-get-comm-balance-interval 
	 account from to include-children?)
  (let ((account-list (if include-children?
                          (let ((sub-accts (gnc-account-get-descendants-sorted account)))
                            (if sub-accts
                                (append (list account) sub-accts)
                                (list account)))
                          (list account))))
    (gnc:account-get-trans-type-balance-interval account-list #f from to)))

;; This calculates the increase in the balance(s) of all accounts in
;; <accountlist> over the period from <from-date> to <to-date>.
;; Returns a commodity collector.
(define (gnc:accountlist-get-comm-balance-interval accountlist from to)
  (gnc:account-get-trans-type-balance-interval accountlist #f from to))

(define (gnc:accountlist-get-comm-balance-at-date accountlist date)
  (gnc:account-get-trans-type-balance-interval accountlist #f #f date))

;; utility function - ensure that a query matches only non-voids.  Destructive.
(define (gnc:query-set-match-non-voids-only! query book)
  (let ((temp-query (qof-query-create-for-splits)))
    (qof-query-set-book temp-query book)

    (xaccQueryAddClearedMatch
     temp-query
     CLEARED-VOIDED
     QOF-QUERY-AND)

    (let ((inv-query (qof-query-invert temp-query)))
      (qof-query-merge-in-place query inv-query QOF-QUERY-AND)
      (qof-query-destroy inv-query)
      (qof-query-destroy temp-query))))

;; utility function - ensure that a query matches only voids.  Destructive

(define (gnc:query-set-match-voids-only! query book)
  (let ((temp-query (qof-query-create-for-splits)))
    (qof-query-set-book temp-query book)

    (xaccQueryAddClearedMatch
     temp-query
     CLEARED-VOIDED
     QOF-QUERY-AND)

    (qof-query-merge-in-place query temp-query QOF-QUERY-AND)
    (qof-query-destroy temp-query)))

(define (gnc:split-voided? split)
  (let ((trans (xaccSplitGetParent split)))
    (xaccTransGetVoidStatus trans)))

(define (gnc:report-starting report-name)
  (gnc-window-show-progress (sprintf #f
				     (_ "Building '%s' report ...")
				     (gnc:gettext report-name))
			    0))

(define (gnc:report-render-starting report-name)
  (gnc-window-show-progress (sprintf #f
				     (_ "Rendering '%s' report ...")
				     (if (string-null? report-name)
					 (gnc:gettext "Untitled")
					 (gnc:gettext report-name)))
			    0))

(define (gnc:report-percent-done percent)
  (gnc-window-show-progress "" percent))

(define (gnc:report-finished)
  (gnc-window-show-progress "" -1))

;; function to count the total number of splits to be iterated
(define (gnc:accounts-count-splits accounts)
  (if (not (null? accounts))
      (+ (length (xaccAccountGetSplitList (car accounts)))
	 (gnc:accounts-count-splits (cdr accounts)))
      0))

;; Sums up any splits of a certain type affecting a set of accounts.
;; the type is an alist '((str "match me") (cased #f) (regexp #f))
;; If type is #f, sums all splits in the interval
(define (gnc:account-get-trans-type-balance-interval
	 account-list type start-date-tp end-date-tp)
  (let* ((total (gnc:make-commodity-collector)))
    (map (lambda (split)
           (let* ((shares (xaccSplitGetAmount split))
                  (acct-comm (xaccAccountGetCommodity
                              (xaccSplitGetAccount split)))
                  )
             (gnc-commodity-collector-add total acct-comm shares)
             )
           )
	 (gnc:account-get-trans-type-splits-interval
          account-list type start-date-tp end-date-tp)
	 )
    total
    )
  )

;; similar, but only counts transactions with non-negative shares and
;; *ignores* any closing entries
(define (gnc:account-get-pos-trans-total-interval
	 account-list type start-date-tp end-date-tp)
  (let* ((str-query (qof-query-create-for-splits))
	 (sign-query (qof-query-create-for-splits))
	 (total-query #f)
         (splits #f)
	 (get-val (lambda (alist key)
		    (let ((lst (assoc-ref alist key)))
		      (if lst (car lst) lst))))
	 (matchstr (get-val type 'str))
	 (case-sens (if (get-val type 'cased) #t #f))
	 (regexp (if (get-val type 'regexp) #t #f))
	 (pos? (if (get-val type 'positive) #t #f))
         (total (gnc:make-commodity-collector))
         )
    (qof-query-set-book str-query (gnc-get-current-book))
    (qof-query-set-book sign-query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! str-query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! sign-query (gnc-get-current-book))
    (xaccQueryAddAccountMatch str-query account-list QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddAccountMatch sign-query account-list QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS
     str-query
     (and start-date-tp #t) start-date-tp
     (and end-date-tp #t) end-date-tp QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS
     sign-query
     (and start-date-tp #t) start-date-tp
     (and end-date-tp #t) end-date-tp QOF-QUERY-AND)
    (xaccQueryAddDescriptionMatch
     str-query matchstr case-sens regexp QOF-QUERY-AND)
    (set! total-query
          ;; this is a tad inefficient, but its a simple way to accomplish
          ;; description match inversion...
          (if pos?
              (qof-query-merge-in-place sign-query str-query QOF-QUERY-AND)
              (let ((inv-query (qof-query-invert str-query)))
                (qof-query-merge-in-place
                 sign-query inv-query QOF-QUERY-AND)
                qof-query-destroy inv-query)))
    (qof-query-destroy str-query)

    (set! splits (qof-query-run total-query))
    (map (lambda (split)
	   (let* ((shares (xaccSplitGetAmount split))
		  (acct-comm (xaccAccountGetCommodity
			      (xaccSplitGetAccount split)))
		  )
	     (or (gnc-numeric-negative-p shares)
		 (gnc-commodity-collector-add total acct-comm shares)
		 )
	     )
	   )
         splits
         )
    (qof-query-destroy total-query)
    total
    )
  )

;; Return the splits that match an account list, date range, and (optionally) type
;; where type is defined as an alist '((str "match me") (cased #f) (regexp #f))
(define (gnc:account-get-trans-type-splits-interval
         account-list type start-date-tp end-date-tp)
  (let* ((query (qof-query-create-for-splits))
	 (splits #f)
	 (get-val (lambda (alist key)
		    (let ((lst (assoc-ref alist key)))
		      (if lst (car lst) lst))))
	 (matchstr (get-val type 'str))
	 (case-sens (if (get-val type 'cased) #t #f))
	 (regexp (if (get-val type 'regexp) #t #f))
	 )
    (qof-query-set-book query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
    (xaccQueryAddAccountMatch query account-list QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS
     query
     (and start-date-tp #t) start-date-tp
     (and end-date-tp #t) end-date-tp QOF-QUERY-AND)
    (if type (xaccQueryAddDescriptionMatch
              query matchstr case-sens regexp QOF-QUERY-AND))
    
    (set! splits (qof-query-run query))
    (qof-query-destroy query)
    splits
    )
  )

;; utility to assist with double-column balance tables
;; a request is made with the <req> argument
;; <req> may currently be 'entry|'debit-q|'credit-q|'zero-q|'debit|'credit
;; 'debit-q|'credit-q|'zero-q tests the sign of the balance
;; 'side returns 'debit or 'credit, the column in which to display
;; 'debt|'credit return the entry, if appropriate, or #f
(define (gnc:double-col
	 req signed-balance report-commodity exchange-fn show-comm?)
  (let* ((sum (and signed-balance
		   (gnc:sum-collector-commodity
		    signed-balance
		    report-commodity
		    exchange-fn)))
	 (amt (and sum (gnc:gnc-monetary-amount sum)))
	 (neg? (and amt (gnc-numeric-negative-p amt)))
	 (bal (if neg?
		  (let ((bal (gnc:make-commodity-collector)))
		    (bal 'minusmerge signed-balance #f)
		    bal)
		  signed-balance))
	 (bal-sum (gnc:sum-collector-commodity
		   bal
		   report-commodity
		   exchange-fn))
	 (balance
	  (if (gnc:uniform-commodity? bal report-commodity)
	      (if (gnc-numeric-zero-p amt) #f bal-sum)
	      (if show-comm?
		  (gnc-commodity-table bal report-commodity exchange-fn)
		  bal-sum)
	      ))
	 )
    (car (assoc-ref
	  (list
	   (list 'entry balance)
	   (list 'debit (if neg? #f balance))
	   (list 'credit (if neg? balance #f))
	   (list 'zero-q (if neg? #f (if balance #f #t)))
	   (list 'debit-q (if neg? #f (if balance #t #f)))
	   (list 'credit-q (if neg? #t #f))
	   )
	  req
	  ))
    )
  )

;; Returns the start date of the first period (period 0) of the budget.
(define (gnc:budget-get-start-date budget)
  (gnc-budget-get-period-start-date budget 0))

;; Returns the end date of the last period of the budget.
(define (gnc:budget-get-end-date budget)
  (gnc-budget-get-period-end-date budget (- (gnc-budget-get-num-periods budget) 1)))


(define (gnc:budget-accountlist-helper accountlist get-fn)
  (let
    (
      (net (gnc:make-commodity-collector)))
    (for-each
      (lambda (account)
        (net 'merge
          (get-fn account)
          #f))
      accountlist)
    net))

;; Sums budget values for a single account from start-period (inclusive) to
;; end-period (exclusive).
;;
;; start-period may be #f to specify the start of the budget
;; end-period may be #f to specify the end of the budget
;;
;; Returns a commodity-collector.
(define (gnc:budget-account-get-net budget account start-period end-period)
  (if (not start-period) (set! start-period 0))
  (if (not end-period) (set! end-period (gnc-budget-get-num-periods budget)))
  (let*
    (
      (period start-period)
      (net (gnc:make-commodity-collector))
      (acct-comm (xaccAccountGetCommodity account)))
    (while (< period end-period)
      (net 'add acct-comm
          (gnc-budget-get-account-period-value budget account period))
      (set! period (+ period 1)))
    net))

;; Sums budget values for accounts in accountlist from start-period (inclusive)
;; to end-period (exclusive).
;;
;; Note that budget values are never sign-reversed, so accountlist should
;; contain only income accounts, only expense accounts, etc.  It would not be
;; meaningful to include both income and expense accounts, or both asset and
;; liability accounts.
;;
;; start-period may be #f to specify the start of the budget
;; end-period may be #f to specify the end of the budget
;;
;; Returns a commodity-collector.
(define (gnc:budget-accountlist-get-net budget accountlist start-period end-period)
  (gnc:budget-accountlist-helper accountlist (lambda (account)
    (gnc:budget-account-get-net budget account start-period end-period))))

;; Finds the balance for an account at the start date of the budget.  The
;; resulting balance is not sign-adjusted.
;;
;; Returns a commodity-collector.
(define (gnc:budget-account-get-initial-balance budget account)
  (gnc:account-get-comm-balance-at-date
    account
    (gnc:budget-get-start-date budget)
    #f))

;; Sums the balances of all accounts in accountlist at the start date of the
;; budget.  The resulting balance is not sign-adjusted.
;;
;; Returns a commodity-collector.
(define (gnc:budget-accountlist-get-initial-balance budget accountlist)
  (gnc:budget-accountlist-helper accountlist (lambda (account)
    (gnc:budget-account-get-initial-balance budget account))))

;; Calculate the sum of all budgets of all children of an account for a specific period
;;
;; Parameters:
;;   budget - budget to use
;;   children - list of children
;;   period - budget period to use
;;
;; Return value:
;;   budget value to use for account for specified period.
(define (budget-account-sum budget children period)
  (let* ((sum
           (cond
             ((null? children) (gnc-numeric-zero))
             (else
               (gnc-numeric-add
                 (gnc:get-account-period-rolledup-budget-value budget (car children) period)
                 (budget-account-sum budget (cdr children) period)
                 GNC-DENOM-AUTO GNC-RND-ROUND))
               )
        ))
  sum)
)

;; Calculate the value to use for the budget of an account for a specific period.
;; - If the account has a budget value set for the period, use it
;; - If the account has children, use the sum of budget values for the children
;; - Otherwise, use 0.
;;
;; Parameters:
;;   budget - budget to use
;;   acct - account
;;   period - budget period to use
;;
;; Return value:
;;   sum of all budgets for list of children for specified period.
(define (gnc:get-account-period-rolledup-budget-value budget acct period)
  (let* ((bgt-set? (gnc-budget-is-account-period-value-set budget acct period))
        (children (gnc-account-get-children acct))
        (amount (cond
                  (bgt-set? (gnc-budget-get-account-period-value budget acct period))
          ((not (null? children)) (budget-account-sum budget children period))
          (else (gnc-numeric-zero)))
        ))
  amount)
)

;; Sums rolled-up budget values for a single account from start-period (inclusive) to
;; end-period (exclusive).
;; - If the account has a budget value set for the period, use it
;; - If the account has children, use the sum of budget values for the children
;; - Otherwise, use 0.
;;
;; start-period may be #f to specify the start of the budget
;; end-period may be #f to specify the end of the budget
;;
;; Returns a gnc-numeric value
(define (gnc:budget-account-get-rolledup-net budget account start-period end-period)
  (if (not start-period) (set! start-period 0))
  (if (not end-period) (set! end-period (gnc-budget-get-num-periods budget)))
  (let*
    (
      (period start-period)
      (net (gnc-numeric-zero))
      (acct-comm (xaccAccountGetCommodity account)))
    (while (< period end-period)
      (set! net (gnc-numeric-add net
          (gnc:get-account-period-rolledup-budget-value budget account period)
	  GNC-DENOM-AUTO GNC-RND-ROUND))
      (set! period (+ period 1)))
    net))

(define (gnc:get-assoc-account-balances accounts get-balance-fn)
  (let*
    (
      (initial-balances (list)))
    (for-each
      (lambda (account)
        (set! initial-balances
          (append initial-balances
            (list (list account (get-balance-fn account))))))
      accounts)
    initial-balances))

(define (gnc:select-assoc-account-balance account-balances account)
  (let*
    (
      (account-balance (car account-balances))
      (result
        (if
          (equal? account-balance '())
          #f
          (if
            (equal? (car account-balance) account)
            (car (cdr account-balance))
            (gnc:select-assoc-account-balance
              (cdr account-balances)
              account)))))
    result))

(define (gnc:get-assoc-account-balances-total account-balances)
  (let
    (
      (total (gnc:make-commodity-collector)))
    (for-each
      (lambda (account-balance)
        (total 'merge (car (cdr account-balance)) #f))
      account-balances)
    total))

;; Adds "file://" to the beginning of a URL if it doesn't already exist
;;
;; @param url URL
;; @return URL with "file://" as the URL type if it isn't already there
(define (make-file-url url)
  (if (string-prefix? "file://" url)
     url
     (string-append "file://" url)))
