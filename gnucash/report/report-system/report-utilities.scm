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
(use-modules (ice-9 format))

(define (list-ref-safe list elt)
  (and (> (length list) elt)
       (list-ref list elt)))

(define (list-set-safe! l elt val)
  (unless (list? l)
    (set! l '()))
  (if (> (length l) elt)
      (list-set! l elt val)
      (let loop ((filler (list val))
                 (i (length l)))
        (if (< i elt)
            (loop (cons #f filler) (1+ i))
            (set! l (append! l filler)))))
  l)

;; pair is a list of one gnc:commodity and one gnc:numeric
;; value. Deprecated -- use <gnc-monetary> instead.
(define (gnc-commodity-value->string pair)
  (issue-deprecation-warning "gnc-commodity-value->string deprecated. \
construct gnc:make-gnc-monetary and use gnc:monetary->string instead.")
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
	    (and (not (null? a)) (member (xaccAccountGetType a) typelist)))
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


;; Get all children of this list of accounts.
(define (gnc:acccounts-get-all-subaccounts accountlist)
  (append-map gnc-account-get-descendants-sorted
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
  (let ((value 0)
        (totalitems 0)
        (maximum -inf.0)
        (minimum +inf.0))
    (let ((adder (lambda (amount)
                   (when (number? amount)
                     (set! value (+ amount value))
                     (if (> amount maximum) (set! maximum amount))
                     (if (< amount minimum) (set! minimum amount))
                     (set! totalitems (1+ totalitems)))))
          (getnumitems (lambda () totalitems))
          (gettotal (lambda () value))
          (getaverage (lambda () (/ value totalitems)))
          (getmax (lambda () maximum))
          (getmin (lambda () minimum))
          (reset-all (lambda ()
                       (set! value 0)
                       (set! maximum -inf.0)
                       (set! minimum +inf.0)
                       (set! totalitems 0))))
      (lambda (action value)
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
  (let ((value 0))
    (lambda (action amount)
      (case action
	((add) (if (number? amount)
                   (set! value (+ amount value))))
	((total) value)
	(else (gnc:warn "bad value-collector action: " action))))))

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
      (let ((pair (assoc commodity commoditylist))
            (rvalue (gnc-numeric-convert
                     value
                     (gnc-commodity-get-fraction commodity) GNC-RND-ROUND)))
	(if (not pair)
	    (begin
	      ;; create a new pair, using the gnc:value-collector
	      (set! pair (list commodity (gnc:make-value-collector)))
	      ;; and add it to the alist
	      (set! commoditylist (cons pair commoditylist))))
	;; add the value
	((cadr pair) 'add rvalue)))
    
    ;; helper function to walk an association list, adding each
    ;; (commodity -> collector) pair to our list at the appropriate 
    ;; place
    (define (add-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value 
		   (caar clist) 
		   ((cadar clist) 'total #f))
		  (add-commodity-clist (cdr clist)))))

    (define (minus-commodity-clist clist)
      (cond ((null? clist) '())
	    (else (add-commodity-value 
		   (caar clist) 
		   (- ((cadar clist) 'total #f)))
		  (minus-commodity-clist (cdr clist)))))

    ;; helper function walk the association list doing a callback on
    ;; each key-value pair.
    (define (process-commodity-list fn clist)
      (map 
       (lambda (pair) (fn (car pair) 
			  ((cadr pair) 'total #f)))
       clist))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a list (gnc:commodity gnc:numeric).
    (define (getpair c sign?)
      (let* ((pair (assoc c commoditylist))
             (total (and pair ((cadr pair) 'total #f))))
	(list c (if pair (if sign? (- total) total) 0))))

    ;; helper function which is given a commodity and returns, if
    ;; existing, a <gnc:monetary> value.
    (define (getmonetary c sign?)
      (let* ((pair (assoc c commoditylist))
             (total (and pair ((cadr pair) 'total #f))))
	(gnc:make-gnc-monetary
         c (if pair (if sign? (- total) total) 0))))
    
    ;; Dispatch function
    (lambda (action commodity amount)
      (case action
	((add) (add-commodity-value commodity amount))
	((merge) (add-commodity-clist
                  (commodity 'list #f #f)))
	((minusmerge) (minus-commodity-clist
                       (commodity 'list #f #f)))
	((format) (process-commodity-list commodity commoditylist))
	((reset) (set! commoditylist '()))
	((getpair) (getpair commodity amount))
	((getmonetary) (getmonetary commodity amount))
	((list) commoditylist) ; this one is only for internal use
	(else (gnc:warn "bad commodity-collector action: " action))))))

(define (gnc:commodity-collector-get-negated collector)
  (let ((negated (gnc:make-commodity-collector)))
    (negated 'minusmerge collector #f)
    negated))

(define (gnc:commodity-collectorlist-get-merged collectorlist)
  (issue-deprecation-warning
   "gnc:commodity-collectorlist-get-merged is now deprecated.")
  (let ((merged (gnc:make-commodity-collector)))
    (for-each (lambda (collector) (merged 'merge collector #f)) collectorlist)
    merged))

;; Returns zero if all entries in this collector are zero.
(define (gnc-commodity-collector-allzero? collector)
  (every zero?
         (map gnc:gnc-monetary-amount
              (collector 'format gnc:make-gnc-monetary #f))))

;; add any number of gnc-monetary objects into a commodity-collector
;; usage: (gnc:monetaries-add monetary1 monetary2 ...)
;; output: a commodity-collector object
(define (gnc:monetaries-add . monetaries)
  (let ((coll (gnc:make-commodity-collector)))
    (for-each
     (lambda (mon)
       (coll 'add (gnc:gnc-monetary-commodity mon) (gnc:gnc-monetary-amount mon)))
     monetaries)
    coll))

;; special case for gnc:monetaries-add whereby only 1 currency is expected
;; usage: (gnc:monetaries-add monetary1 monetary2 ...)
;; output: a monetary object
(define (gnc:monetary+ . monetaries)
  (let* ((coll (apply gnc:monetaries-add monetaries))
         (list-of-monetaries (coll 'format gnc:make-gnc-monetary #f)))
    (if (null? (cdr list-of-monetaries))
        (car list-of-monetaries)
        (throw "gnc:monetary+ expects 1 currency " (gnc:strify monetaries)))))

;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
;; I think this (gnc:account-get-balance-at-date) is flawed in sub-acct handling.
;; Consider account structure:
;; Assets [USD] - bal=$0
;;    Bank [USD] - bal=$100
;;    Broker [USD] - bal=$200
;;       Cash [USD] - bal=$800
;;       Funds [FUND] - bal=3 FUND @ $1000 each = $3000
;; - Calling (gnc:account-get-balance-at-date BANK TODAY #f) returns 100
;; - Calling (gnc:account-get-balance-at-date BROKER TODAY #f) returns 200
;; - Calling (gnc:account-get-balance-at-date BROKER TODAY #t) returns 1000
;;   this is because although it counts all subaccounts bal $200 + $800 + 3FUND,
;;   it retrieves the parent account commodity USD $1000 only.
;; It needs to be deprecated.
(define (gnc:account-get-balance-at-date account date include-children?)
  (issue-deprecation-warning "this gnc:account-get-balance-at-date function is \
flawed. see report-utilities.scm. please update reports.")
  (let ((collector (gnc:account-get-comm-balance-at-date
                    account date include-children?)))
    (cadr (collector 'getpair (xaccAccountGetCommodity account) #f))))

;; this function will scan through the account splitlist, building
;; a list of balances along the way at dates specified in dates-list.
;; in:  account
;;      dates-list (list of time64) - NOTE: IT WILL BE SORTED
;;      split->amount - an unary lambda. calling (split->amount split)
;;      returns a number, or #f which effectively skips the split.
;; out: (list bal0 bal1 ...), each entry is a gnc-monetary object
;;
;; NOTE a prior incarnation accepted a #:ignore-closing? boolean
;; keyword which can be reproduced via #:split->amount (lambda (s)
;; (and (not (xaccTransGetIsClosingTxn (xaccSplitGetParent s)))
;; (xaccSplitGetAmount s)))
(define* (gnc:account-get-balances-at-dates
          account dates-list #:key (split->amount xaccSplitGetAmount))
  (define (amount->monetary bal)
    (gnc:make-gnc-monetary (xaccAccountGetCommodity account) bal))
  (let loop ((splits (xaccAccountGetSplitList account))
             (dates-list (stable-sort! dates-list <))
             (currentbal 0)
             (lastbal 0)
             (balancelist '()))
    (cond

     ;; end of dates. job done!
     ((null? dates-list)
      (map amount->monetary (reverse balancelist)))

     ;; end of splits, but still has dates. pad with last-bal
     ;; until end of dates.
     ((null? splits)
      (loop '()
            (cdr dates-list)
            currentbal
            lastbal
            (cons lastbal balancelist)))

     (else
      (let* ((this (car splits))
             (rest (cdr splits))
             (currentbal (+ (or (split->amount this) 0) currentbal))
             (next (and (pair? rest) (car rest))))

        (cond
         ;; the next split is still before date
         ((and next (< (xaccTransGetDate (xaccSplitGetParent next)) (car dates-list)))
          (loop rest dates-list currentbal lastbal balancelist))

         ;; this split after date, add previous bal to balancelist
         ((< (car dates-list) (xaccTransGetDate (xaccSplitGetParent this)))
          (loop splits
                (cdr dates-list)
                lastbal
                lastbal
                (cons lastbal balancelist)))

         ;; this split before date, next split after date, or end.
         (else
          (loop rest
                (cdr dates-list)
                currentbal
                currentbal
                (cons currentbal balancelist)))))))))

;; This works similar as above but returns a commodity-collector, 
;; thus takes care of children accounts with different currencies.
(define (gnc:account-get-comm-balance-at-date
         account date include-children?)
  (let ((balance-collector (gnc:make-commodity-collector))
        (accounts (cons account
                        (if include-children?
                            (gnc-account-get-descendants account)
                            '()))))
    (for-each
     (lambda (acct)
       (balance-collector 'add
                          (xaccAccountGetCommodity acct)
                          (xaccAccountGetBalanceAsOfDate acct date)))
     accounts)
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
        (accounts (cons account
                        (if include-children?
                            (gnc-account-get-descendants account)
                            '()))))

    ;; Build a query to find all splits between the indicated dates.
    (qof-query-set-book query (gnc-get-current-book))
    (xaccQueryAddAccountMatch query accounts
                              QOF-GUID-MATCH-ANY
                              QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT query
                             (and start-date #t) (or start-date 0)
                             (and end-date #t) (or end-date 0)
                             QOF-QUERY-AND)

    ;; Get the query results.
    (let ((splits (qof-query-run query)))
      (qof-query-destroy query)
      ;; Add the "value" of each split returned (which is measured
      ;; in the transaction currency).
      (for-each
       (lambda (split)
         (value-collector 'add
                          (xaccTransGetCurrency (xaccSplitGetParent split))
                          (xaccSplitGetValue split)))
       splits))
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
       (collector
        (if (reverse-balance-fn acct) 'minusmerge 'merge)
        (get-balance-fn acct)
        #f))
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
    (cadr (collector 'getpair (xaccAccountGetCommodity account) #f))))

;; the version which returns a commodity-collector
(define (gnc:account-get-comm-balance-interval account from to include-children?)
  (let ((sub-accts (gnc-account-get-descendants-sorted account)))
    (gnc:account-get-trans-type-balance-interval
     (cons account (or (and include-children? sub-accts) '()))
     #f from to)))

;; This calculates the increase in the balance(s) of all accounts in
;; <accountlist> over the period from <from-date> to <to-date>.
;; Returns a commodity collector.
(define (gnc:accountlist-get-comm-balance-interval accountlist from to)
  (gnc:account-get-trans-type-balance-interval accountlist #f from to))

(define (gnc:accountlist-get-comm-balance-interval-with-closing accountlist from to)
  (gnc:account-get-trans-type-balance-interval-with-closing accountlist #f from to))

(define (gnc:accountlist-get-comm-balance-at-date accountlist date)
  (gnc:account-get-trans-type-balance-interval accountlist #f #f date))

(define (gnc:accountlist-get-comm-balance-at-date-with-closing accountlist date)
  (gnc:account-get-trans-type-balance-interval-with-closing accountlist #f #f date))

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
  (gnc-window-show-progress (format #f
				     (_ "Building '~a' report ...")
				     (gnc:gettext report-name))
			    0))

(define (gnc:report-render-starting report-name)
  (gnc-window-show-progress (format #f
				     (_ "Rendering '~a' report ...")
				     (if (string-null? report-name)
					 (gnc:gettext "Untitled")
					 (gnc:gettext report-name)))
			    0))

(define (gnc:report-percent-done percent)
  (if (> percent 100)
      (gnc:warn "report more than 100% finished. " percent))
  (gnc-window-show-progress "" percent))

(define (gnc:report-finished)
  (gnc-window-show-progress "" -1))

;; function to count the total number of splits to be iterated
(define (gnc:accounts-count-splits accounts)
  (apply + (map length (map xaccAccountGetSplitList accounts))))

;; Sums up any splits of a certain type affecting a set of accounts.
;; the type is an alist '((str "match me") (cased #f) (regexp #f))
;; If type is #f, sums all non-closing splits in the interval
(define (gnc:account-get-trans-type-balance-interval
         account-list type start-date end-date)
  (let* ((total (gnc:make-commodity-collector)))
    (for-each
     (lambda (split)
       (if (or type (not (xaccTransGetIsClosingTxn (xaccSplitGetParent split))))
           (total 'add
                  (xaccAccountGetCommodity (xaccSplitGetAccount split))
                  (xaccSplitGetAmount split))))
     (gnc:account-get-trans-type-splits-interval
      account-list type start-date end-date))
    total))

;; Sums up any splits of a certain type affecting a set of accounts.
;; the type is an alist '((str "match me") (cased #f) (regexp #f))
;; If type is #f, sums all splits in the interval (even closing splits)
(define (gnc:account-get-trans-type-balance-interval-with-closing
         account-list type start-date end-date)
  (let ((total (gnc:make-commodity-collector)))
    (for-each
     (lambda (split)
       (total 'add
              (xaccAccountGetCommodity (xaccSplitGetAccount split))
              (xaccSplitGetAmount split)))
     (gnc:account-get-trans-type-splits-interval
      account-list type start-date end-date))
    total))

;; Filters the splits from the source to the target accounts
;; returns a commodity collector
;; does NOT do currency exchanges
(define (gnc:account-get-total-flow direction target-account-list from-date to-date)
  (issue-deprecation-warning
   "(gnc:account-get-total-flow) is deprecated.")
  (let ((total-flow (gnc:make-commodity-collector)))
    (for-each
     (lambda (target-account)
       (for-each
        (lambda (target-account-split)
          (let* ((transaction (xaccSplitGetParent target-account-split))
                 (split-value (xaccSplitGetAmount target-account-split)))
            (if (and (<= from-date (xaccTransGetDate transaction) to-date)
                     (or (and (eq? direction 'in)
                              (positive? split-value))
                         (and (eq? direction 'out)
                              (negative? split-value))))
                (total-flow 'add (xaccTransGetCurrency transaction) split-value))))
        (xaccAccountGetSplitList target-account)))
     target-account-list)
    total-flow))

;; similar, but only counts transactions with non-negative shares and
;; *ignores* any closing entries
(define (gnc:account-get-pos-trans-total-interval
	 account-list type start-date end-date)
  (issue-deprecation-warning
   "(gnc:account-get-pos-trans-total-interval) is deprecated.")
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
    (xaccQueryAddDateMatchTT
     str-query
     (and start-date #t) (if start-date start-date 0)
     (and end-date #t) (if end-date end-date 0)
     QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT
     sign-query
     (and start-date #t) (if start-date start-date 0)
     (and end-date #t) (if end-date end-date 0)
     QOF-QUERY-AND)
    (xaccQueryAddDescriptionMatch
     str-query matchstr case-sens regexp QOF-COMPARE-CONTAINS QOF-QUERY-AND)
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
		 (total 'add acct-comm shares)
		 )
	     )
	   )
         splits
         )
    (qof-query-destroy total-query)
    total))

;; Return the splits that match an account list, date range, and (optionally) type
;; where type is defined as an alist like:
;; '((str "match me") (cased #f) (regexp #f) (closing #f))
;; where str, cased, and regexp define a pattern match on transaction deseriptions 
;; and "closing" matches transactions created by the book close command.  If "closing"
;; is given as #t then only closing transactions will be returned, if it is #f then
;; only non-closing transactions will be returned, and if it is omitted then both
;; kinds of transactions will be returned.
(define (gnc:account-get-trans-type-splits-interval
         account-list type start-date end-date)
  (if (null? account-list)
      '()
      (let* ((query (qof-query-create-for-splits))
             (get-val (lambda (key)
                        (let ((lst (assq-ref type key)))
                          (and lst (car lst)))))
             (matchstr (get-val 'str))
             (case-sens (get-val 'cased))
             (regexp (get-val 'regexp))
             (closing (get-val 'closing)))
        (qof-query-set-book query (gnc-get-current-book))
        (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
        (xaccQueryAddAccountMatch query account-list QOF-GUID-MATCH-ANY QOF-QUERY-AND)
        (xaccQueryAddDateMatchTT
         query
         (and start-date #t) (or start-date 0)
         (and end-date #t) (or end-date 0)
         QOF-QUERY-AND)
        (when (or matchstr closing)
          (let ((query2 (qof-query-create-for-splits)))
            (if matchstr
                (xaccQueryAddDescriptionMatch
                 query2 matchstr case-sens regexp
                 QOF-COMPARE-CONTAINS QOF-QUERY-OR))
            (if closing
                (xaccQueryAddClosingTransMatch query2 1 QOF-QUERY-OR))
            (qof-query-merge-in-place query query2 QOF-QUERY-AND)
            (qof-query-destroy query2)))
        (let ((splits (qof-query-run query)))
          (qof-query-destroy query)
          splits))))

;; the following function is only used in trial-balance. best move it
;; back there, and deprecate this exported function.
(define (gnc:double-col
	 req signed-balance report-commodity exchange-fn show-comm?)
  (issue-deprecation-warning
   "(gnc:double-col) is deprecated.")
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
  (let ((net (gnc:make-commodity-collector)))
    (for-each
     (lambda (account)
       (net 'merge (get-fn account) #f))
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
  (let* ((period (or start-period 0))
         (maxperiod (or end-period (gnc-budget-get-num-periods budget)))
         (net (gnc:make-commodity-collector)))
    (while (< period maxperiod)
      (net 'add
           (xaccAccountGetCommodity account)
           (gnc-budget-get-account-period-value budget account period))
      (set! period (1+ period)))
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
  (apply + (map
            (lambda (child)
              (gnc:get-account-period-rolledup-budget-value budget child period))
            children)))

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
         (children (gnc-account-get-children acct)))
    (cond
     (bgt-set? (gnc-budget-get-account-period-value budget acct period))
     ((not (null? children)) (budget-account-sum budget children period))
     (else 0))))

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
  (let* ((start (or start-period 0))
         (end (or end-period (gnc-budget-get-num-periods budget)))
         (numperiods (- end start -1)))
  (apply +
         (map
          (lambda (period)
            (gnc:get-account-period-rolledup-budget-value budget account period))
          (iota numperiods start 1)))))

;; ***************************************************************************
;; The following 3 functions belong together
;; Input: accounts, get-balance-fn
;; Output: account-balances, a list of 2-element lists

(define (gnc:get-assoc-account-balances accounts get-balance-fn)
  (map
   (lambda (acct)
     (list acct (get-balance-fn acct)))
   accounts))

;; Input: account-balances, account
;; Output: commodity-collector
(define (gnc:select-assoc-account-balance account-balances account)
  (let ((found (find
                (lambda (acct-bal)
                  (equal? (car acct-bal) account))
                account-balances)))
    (and found (cadr found))))

;; Input: account-balances
;; Output: commodity-collector
(define (gnc:get-assoc-account-balances-total account-balances)
  (let ((total (gnc:make-commodity-collector)))
    (for-each
     (lambda (account-balance)
       (total 'merge (cadr account-balance) #f))
     account-balances)
    total))
;; ***************************************************************************

;; Adds "file:///" to the beginning of a URL if it doesn't already exist
;;
;; @param url URL
;; @return URL with "file:///" as the URL type if it isn't already there
(define (make-file-url url)
  (if (string-prefix? "file:///" url)
     url
     (string-append "file:///" url)))

(define-public (gnc:strify d)
  ;; any object -> string.  The option is passed to various
  ;; scm->string converters; ultimately a generic stringify
  ;; function handles symbol/string/other types.
  (define (split->str spl)
    (let ((txn (xaccSplitGetParent spl)))
      (format #f "Split<d:~a,acc:~a,amt:~a,val:~a>"
              (qof-print-date (xaccTransGetDate txn))
              (xaccAccountGetName (xaccSplitGetAccount spl))
              (gnc:monetary->string
               (gnc:make-gnc-monetary
                (xaccTransGetCurrency txn)
                (xaccSplitGetValue spl)))
              (gnc:monetary->string
               (gnc:make-gnc-monetary
                (xaccAccountGetCommodity
                 (xaccSplitGetAccount spl))
                (xaccSplitGetAmount spl))))))
  (define (trans->str txn)
    (format #f "Txn<d:~a>" (qof-print-date (xaccTransGetDate txn))))
  (define (account->str acc)
    (format #f "Acc<~a>" (xaccAccountGetName acc)))
  (define (monetary-collector->str coll)
    (format #f "Mon-coll<~a>"
            (map gnc:strify (coll 'format gnc:make-gnc-monetary #f))))
  (define (value-collector->str coll)
    (format #f "Val-coll<~a>"
            (map gnc:strify (coll 'total gnc:make-gnc-monetary))))
  (define (procedure->str proc)
    (format #f "Proc<~a>"
            (or (procedure-name proc) "unk")))
  (define (monetary->string mon)
    (format #f "Mon<~a>"
            (gnc:monetary->string mon)))
  (define (try proc)
    ;; Try proc with d as a parameter, catching 'wrong-type-arg
    ;; exceptions to return #f to the (or) evaluator below.
    (catch 'wrong-type-arg
      (lambda () (proc d))
      (const #f)))
  (or (and (boolean? d) (if d "#t" "#f"))
      (and (null? d) "#null")
      (and (symbol? d) (format #f "'~a" d))
      (and (string? d) d)
      (and (list? d) (string-append
                      "(list "
                      (string-join (map gnc:strify d) " ")
                      ")"))
      (and (pair? d) (format #f "(~a . ~a)"
                             (gnc:strify (car d))
                             (if (eq? (car d) 'absolute)
                                 (qof-print-date (cdr d))
                                 (gnc:strify (cdr d)))))
      (try procedure->str)
      (try gnc-commodity-get-mnemonic)
      (try account->str)
      (try split->str)
      (try trans->str)
      (try monetary-collector->str)
      (try value-collector->str)
      (try monetary->string)
      (try gnc-budget-get-name)
      (object->string d)))

(define (pair->num pair)
  (+ (car pair)
     (/ (cdr pair) 1000000)))

(define (delta t1 t2)
  (exact->inexact
   (- (pair->num t2)
      (pair->num t1))))

(define-public gnc:pk
  (let* ((start-time (gettimeofday))
         (last-time start-time))
    (lambda args
      (let ((now (gettimeofday)))
        (format #t "d~,4f t~,3f: "
                (delta last-time now)
                (delta start-time now))
        (set! last-time now))
      (display (map gnc:strify args))
      (newline)
      (last args))))
