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

(define-module (gnucash report report-utilities))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-26))
(use-modules (ice-9 format))
(use-modules (ice-9 match))

(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report html-text))
(use-modules (gnucash report html-acct-table))
(use-modules (gnucash gnome-utils))

(export list-ref-safe)
(export list-set-safe!)
(export gnc:monetary->string)
(export gnc:not-all-zeros)
(export gnc:account-has-shares?)
(export gnc:account-is-stock?)
(export gnc:account-is-inc-exp?)
(export gnc:account-full-name<?)
(export gnc:accounts-get-children-depth)
(export gnc:filter-accountlist-type)
(export gnc:decompose-accountlist)
(export gnc:account-get-type-string-plural)
(export gnc:accounts-get-commodities)
(export gnc:get-current-account-tree-depth)
(export gnc:accounts-and-all-descendants)
(export gnc:make-value-collector)
(export gnc:make-commodity-collector)
(export gnc:collector+)
(export gnc:collector-)
(export gnc:commodity-collector-get-negated)
(export gnc:account-accumulate-at-dates)
(export gnc:account-get-balance-at-date)
(export gnc:account-get-balances-at-dates)
(export gnc:account-get-comm-balance-at-date)
(export gnc:account-get-comm-value-interval)
(export gnc:account-get-comm-value-at-date)
(export gnc:accounts-get-balance-helper)
(export gnc:accounts-get-comm-total-profit)
(export gnc:accounts-get-comm-total-income)
(export gnc:accounts-get-comm-total-expense)
(export gnc:accounts-get-comm-total-assets)
(export gnc:account-get-balance-interval)
(export gnc:account-get-comm-balance-interval)
(export gnc:accountlist-get-comm-balance-interval)
(export gnc:accountlist-get-comm-balance-interval-with-closing)
(export gnc:accountlist-get-comm-balance-at-date)
(export gnc:accountlist-get-comm-balance-at-date-with-closing)
(export gnc:query-set-match-non-voids-only!)
(export gnc:query-set-match-voids-only!)
(export gnc:split-voided?)
(export gnc:report-starting)
(export gnc:report-render-starting)
(export gnc:report-percent-done)
(export gnc:report-finished)
(export gnc:accounts-count-splits)
(export gnc-commodity-collector-allzero?)
(export gnc:monetary+)
(export gnc:monetaries-add)
(export gnc:account-get-trans-type-balance-interval)
(export gnc:account-get-trans-type-balance-interval-with-closing)
(export gnc:account-get-trans-type-splits-interval)
(export gnc:budget-get-start-date)
(export gnc:budget-get-end-date)
(export gnc:budget-account-get-net)
(export gnc:budget-accountlist-get-net)
(export gnc:budget-account-get-initial-balance)
(export gnc:budget-accountlist-get-initial-balance)
(export budget-account-sum budget)
(export gnc:get-account-period-rolledup-budget-value)
(export gnc:budget-account-get-rolledup-net)
(export gnc:get-assoc-account-balances)
(export gnc:select-assoc-account-balance)
(export gnc:get-assoc-account-balances-total)
(export gnc:multiline-to-html-text)
(export gnc:payment-txn->payment-info)
(export make-file-url)
(export gnc:strify)
(export gnc:pk)
(export gnc:dump-book)
(export gnc:dump-invoices)
(export gnc:dump-lot)

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

;; account related functions
;; helper for sorting of account list
(define (gnc:account-full-name<? a b)
  (gnc:string-locale<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

(define (gnc:accounts-get-children-depth accounts)
  (1- (apply max
             (map (lambda (acct)
                    (+ (gnc-account-get-current-depth acct)
                       (gnc-account-get-tree-depth acct)))
                  accounts))))

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
    (cons ACCT-TYPE-BANK (G_ "Bank"))
    (cons ACCT-TYPE-CASH (G_ "Cash"))
    (cons ACCT-TYPE-CREDIT (G_ "Credits"))
    (cons ACCT-TYPE-ASSET (G_ "Assets"))
    (cons ACCT-TYPE-LIABILITY (G_ "Liabilities"))
    (cons ACCT-TYPE-STOCK (G_ "Stocks"))
    (cons ACCT-TYPE-MUTUAL (G_ "Mutual Funds"))
    (cons ACCT-TYPE-CURRENCY (G_ "Currencies"))
    (cons ACCT-TYPE-INCOME (G_ "Income"))
    (cons ACCT-TYPE-EXPENSE (G_ "Expenses"))
    (cons ACCT-TYPE-EQUITY (G_ "Equities"))
    (cons ACCT-TYPE-CHECKING (G_ "Checking"))
    (cons ACCT-TYPE-SAVINGS (G_ "Savings"))
    (cons ACCT-TYPE-MONEYMRKT (G_ "Money Market"))
    (cons ACCT-TYPE-RECEIVABLE (G_ "Accounts Receivable"))
    (cons ACCT-TYPE-PAYABLE (G_ "Accounts Payable"))
    (cons ACCT-TYPE-CREDITLINE (G_ "Credit Lines"))
    (cons ACCT-TYPE-TRADING (G_ "Trading Accounts")))
   type))

;; Get the list of all different commodities that are used within the
;; 'accounts', excluding the 'exclude-commodity'.
(define (gnc:accounts-get-commodities accounts exclude-commodity)
  (delete exclude-commodity
	  (sort-and-delete-duplicates
           (map xaccAccountGetCommodity accounts)
           (lambda (a b)
             (gnc:string-locale<? (gnc-commodity-get-unique-name a)
                                  (gnc-commodity-get-unique-name b)))
           gnc-commodity-equiv)))


;; Returns the depth of the current account hierarchy, that is, the
;; maximum level of subaccounts in the tree
(define (gnc:get-current-account-tree-depth)
  (let ((root (gnc-get-current-root-account)))
    (gnc-account-get-tree-depth root)))

;; Return accountslist *and* their descendant accounts
(define (gnc:accounts-and-all-descendants accountslist)
  (issue-deprecation-warning "gnc:accounts-and-all-descendants is \
now deprecated, use gnc-accounts-and-all-descendants instead. sort \
with gnc:account-full-name<? if necessary.")
  (sort (gnc-accounts-and-all-descendants accountslist) gnc:account-full-name<?))

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
;
;; Note amounts are rounded to the commodity's SCU.
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
;;       <commodity> doesn't exist, the balance will be 0. If
;;       signreverse? is true, the result's sign will be reversed.
;;   'getmonetary <commodity> signreverse?: Returns a gnc-monetary
;;       of the <commodity> and its corresponding balance. If
;;       <commodity> doesn't exist, the balance will be 0. If
;;       signreverse? is true, the result's sign will be reversed.
;;   (internal) 'list #f #f: get the list of
;;       (cons commodity numeric-collector)

(define (gnc:make-commodity-collector)
  ;; the association list of (commodity . value-collector) pairs.
  (let ((commoditylist '()))
    
    ;; helper function to add a (commodity . value) pair to our list.
    ;; If no pair with this commodity exists, we will create one.
    (define (add-commodity-value commodity value)
      (let ((pair (assoc commodity commoditylist)))
	(unless pair
	  (set! pair (list commodity (gnc:make-value-collector)))
	  (set! commoditylist (cons pair commoditylist)))
	((cadr pair) 'add value)))
    
    ;; helper function to walk an association list, adding each
    ;; (commodity . collector) pair to our list at the appropriate
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
       (lambda (pair)
         (fn (car pair) ((cadr pair) 'total #f)))
       clist))

    ;; helper function which is given a commodity and returns a list
    ;; (list gnc:commodity number).
    (define (getpair c sign?)
      (let* ((pair (assoc c commoditylist))
             (total (if pair ((cadr pair) 'total #f) 0)))
	(list c (if sign? (- total) total))))

    ;; helper function which is given a commodity and returns a
    ;; <gnc:monetary> value, whose amount may be 0.
    (define (getmonetary c sign?)
      (let* ((pair (assoc c commoditylist))
             (total (if pair ((cadr pair) 'total #f) 0)))
	(gnc:make-gnc-monetary c (if sign? (- total) total))))
    
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

;; Returns zero if all entries in this collector are zero.
(define (gnc-commodity-collector-allzero? collector)
  (every zero? (map cdr (collector 'format cons #f))))

;; (gnc:collector+ collectors ...) equiv to (+ collectors ...) and
;; outputs: a collector
(define (gnc:collector+ . collectors)
  (let ((res (gnc:make-commodity-collector)))
    (for-each (lambda (coll) (res 'merge coll #f)) collectors)
    res))

;; (gnc:collectors- collectors ...) equiv to (- collectors ...), can
;; also negate single-argument collector. outputs collector
(define gnc:collector-
  (case-lambda
    (() (error "gnc:collector- needs at least 1 collector argument"))
    ((coll) (gnc:collector- (gnc:make-commodity-collector) coll))
    ((coll . rest)
     (let ((res (gnc:make-commodity-collector)))
       (res 'merge coll #f)
       (res 'minusmerge (apply gnc:collector+ rest) #f)
       res))))

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
  (let ((coll (apply gnc:monetaries-add monetaries)))
    (match (coll 'format gnc:make-gnc-monetary #f)
      ((mon) mon)
      (_ (throw "gnc:monetary+ expects 1 currency " (gnc:strify monetaries))))))

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
    (gnc:make-gnc-monetary (xaccAccountGetCommodity account) (or bal 0)))
  (define balance 0)
  (map amount->monetary
       (gnc:account-accumulate-at-dates
        account dates-list #:split->elt
        (lambda (s)
          (if s (set! balance (+ balance (or (split->amount s) 0))))
          balance))))


;; this function will scan through account splitlist, building a list
;; of split->elt results along the way at dates specified in dates.
;; in: acc   - account
;;     dates - a list of time64 -- it will be sorted
;;     nosplit->elt - if report-dates occur *before* earliest split, the
;;                    result list will be padded with this value
;;     split->date - an unary lambda. result to compare with dates list.
;;     split->elt - an unary lambda. it will be called successfully for each
;;                  split in the account until the last date. the result
;;                  will be accumulated onto the resulting list. the default
;;                  xaccSplitGetBalance makes it similar to
;;                  gnc:account-get-balances-at-dates.
;; out: (list elt0 elt1 ...), each entry is the result of split->elt
;;      or nosplit->elt
(define* (gnc:account-accumulate-at-dates
          acc dates #:key
          (nosplit->elt #f)
          (split->date #f)
          (split->elt xaccSplitGetBalance))
  (define to-date (or split->date (compose xaccTransGetDate xaccSplitGetParent)))
  (define (less? a b) (< (to-date a) (to-date b)))

  (let lp ((splits (if split->date
                       (stable-sort! (xaccAccountGetSplitList acc) less?)
                       (xaccAccountGetSplitList acc)))
           (dates (sort dates <))
           (result '())
           (last-result nosplit->elt))
    (match dates

      ;; end of dates. job done!
      (() (reverse result))

      ((date . rest)
       (define (before-date? s) (<= (to-date s) date))
       (define (after-date? s) (< date (to-date s)))
       (cond

        ;; end of splits, but still has dates. pad with last-result
        ;; until end of dates.
        ((null? splits) (lp '() rest (cons last-result result) last-result))

        ;; the next split is still before date.
        ((and (pair? (cdr splits)) (before-date? (cadr splits)))
         (lp (cdr splits) dates result (split->elt (car splits))))

        ;; head split after date, accumulate previous result
        ((after-date? (car splits))
         (lp splits rest (cons last-result result) last-result))

        ;; head split before date, next split after date, or end.
        (else
         (let ((head-result (split->elt (car splits))))
           (lp (cdr splits) rest (cons head-result result) head-result))))))))

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
  (issue-deprecation-warning "gnc:accounts-get-comm-total-profit deprecated.")
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the income accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-income accounts 
					    get-balance-fn)
  (issue-deprecation-warning "gnc:accounts-get-comm-total-income deprecated.")
  (gnc:accounts-get-balance-helper
   (gnc:filter-accountlist-type (list ACCT-TYPE-INCOME) accounts)
   get-balance-fn
   (lambda(x) #t)))

;; Adds all accounts' balances, where the balances are determined with
;; the get-balance-fn. Only the expense accounts are regarded, and
;; the result is sign reversed. Returns a commodity-collector.
(define (gnc:accounts-get-comm-total-expense accounts 
                                             get-balance-fn)
  (issue-deprecation-warning "gnc:accounts-get-comm-total-expense deprecated.")
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
  (issue-deprecation-warning
   "gnc:query-set-match-non-voids-only! is deprecated. add query for\
(logand CLEARED-ALL (lognot CLEARED-VOIDED)) instead.")
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
  (issue-deprecation-warning
   "gnc:query-set-match-non-voids-only! is deprecated. add CLEARED-VOIDED \
query instead.")
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
				     (G_ "Building '~a' report ...")
				     (G_ report-name))
			    0))

(define (gnc:report-render-starting report-name)
  (gnc-window-show-progress (format #f
				     (G_ "Rendering '~a' report ...")
				     (if (string-null? report-name)
					 (G_ "Untitled")
					 (G_ report-name)))
			    0))

(define (gnc:report-percent-done percent)
  (if (> percent 100)
      (gnc:warn "report more than 100% finished. " percent))
  (gnc-window-show-progress "" percent))

(define-public gnc:pulse-progress-bar
  (let ((pulse-idx 0))
    (lambda ()
      (set! pulse-idx (1+ pulse-idx))
      (when (= pulse-idx 1000)
        (set! pulse-idx 0)
        (gnc-window-show-progress "" 105)))))

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
        (xaccQueryAddClearedMatch
         query (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)
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
;;   currency - currency to use to accumulate the balances
;;
;; Return value:
;;   budget value to use for account for specified period.
(define (budget-account-sum budget children period currency)
  (let ((pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
        (start (gnc-budget-get-period-start-date budget period)))
    (apply + (map
              (lambda (child)
                (gnc-pricedb-convert-balance-nearest-price-t64
                  pricedb
                  (gnc:get-account-period-rolledup-budget-value budget child period)
                  (xaccAccountGetCommodity child)
                  currency
                  start))
              children))))

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
         (currency (xaccAccountGetCommodity acct)))
    (cond
     (bgt-set? (gnc-budget-get-account-period-value budget acct period))
     ((not (null? children)) (budget-account-sum budget children period currency))
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
  (define end (or end-period (1- (gnc-budget-get-num-periods budget))))
  (let lp ((period (or start-period 0)) (sum 0))
    (cond
     ((> period end) sum)
     (else (lp (1+ period) (+ sum (gnc:get-account-period-rolledup-budget-value
                                   budget account period)))))))

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
  (apply gnc:collector+ (map cadr account-balances)))

(define (gnc:multiline-to-html-text str)
  ;; simple function - splits string containing #\newline into
  ;; substrings, and convert to a gnc:make-html-text construct which
  ;; adds gnc:html-markup-br after each substring.
  (define (interleave a b) (cons* a (gnc:html-markup-br) b))
  (apply gnc:make-html-text (fold-right interleave '() (string-split str #\nl))))

;; ***************************************************************************
;; Business Functions

(define (sign-equal? a b) (or (= 0 a b) (< 0 (* a b))))
(define (not-APAR? s)
  (not (xaccAccountIsAPARType (xaccAccountGetType (xaccSplitGetAccount s)))))
;; analyse a payment transaction and return a 3-element vector:
;; (vector invoices opposing-splits overpayment)
;;
;; invoices: a list of (cons invoice inv-APAR-split)
;; opposing-splits: a list of (list pmt-APAR-split partial-amount derived?)
;;                 partial-amount is a number, derived? is #true if the partial
;;                 amount does not match the transaction amount
;; overpayment: a number indicating overpayment amount
(define (gnc:payment-txn->payment-info txn)
  (let lp ((splits (xaccTransGetSplitList txn))
           (invoices '())
           (overpayment 0)
           (opposing-splits '()))
    (match splits
      (() (vector invoices opposing-splits overpayment))
      (((? not-APAR? split) . rest)
       (lp rest invoices (+ overpayment (xaccSplitGetAmount split))
           opposing-splits))
      ((split . rest)
       (let* ((lot (xaccSplitGetLot split))
              (lot-all-splits (gnc-lot-get-split-list lot)))
         (define split=? (cut equal? <> split))
         (match (gncInvoiceGetInvoiceFromLot lot)
           (() (let lp1 ((lot-splits lot-all-splits)
                         (overpayment overpayment)
                         (opposing-splits opposing-splits))
                 (match lot-splits
                   (() (lp rest invoices overpayment opposing-splits))
                   (((? split=?) . tail) (lp1 tail overpayment opposing-splits))
                   ((s . tail)
                    (let* ((lot-bal (gnc-lot-get-balance lot))
                           (lot-bal (if (sign-equal? lot-bal (xaccSplitGetAmount s))
                                        0 lot-bal))
                           (derived? (not (zero? lot-bal)))
                           (partial-amount
                            (fold
                             (lambda (a b)
                               (if (equal? s a) b (+ b (xaccSplitGetAmount a))))
                             (- lot-bal) lot-all-splits)))
                      (lp1 tail (+ overpayment partial-amount)
                           (cons (list s partial-amount derived?)
                                 opposing-splits)))))))
           (inv
            (lp rest
                (cons (cons inv split) invoices)
                (+ overpayment (xaccSplitGetAmount split))
                opposing-splits))))))))

;; create a stepped list, then add a date in the infinite future for
;; the "current" bucket
(define (make-extended-interval-list to-date num-buckets)
  (let lp ((begindate to-date) (num-buckets num-buckets))
    (if (zero? num-buckets)
        (append (gnc:make-date-list begindate to-date ThirtyDayDelta) (list +inf.0))
        (lp (decdate begindate ThirtyDayDelta) (1- num-buckets)))))

;; Outputs: aging list of numbers
(define-public (gnc:owner-splits->aging-list splits num-buckets
                                             to-date date-type receivable?)
  (gnc:msg "processing " (qof-print-date to-date) " date-type " date-type
           "receivable? " receivable?)
  (let ((bucket-dates (make-extended-interval-list to-date (- num-buckets 3)))
        (buckets (make-vector num-buckets 0)))
    (define (addbucket! idx amt)
      (vector-set! buckets idx (+ amt (vector-ref buckets idx))))
    (let lp ((splits splits))
      (cond
       ((null? splits)
        (vector->list buckets))

       ;; next split is an invoice posting split. add its balance to
       ;; bucket
       ((eqv? (xaccTransGetTxnType (xaccSplitGetParent (car splits)))
              TXN-TYPE-INVOICE)
        (let* ((invoice (gncInvoiceGetInvoiceFromTxn
                         (xaccSplitGetParent (car splits))))
               (lot (gncInvoiceGetPostedLot invoice))
               (lot-splits (gnc-lot-get-split-list lot))
               (bal (fold
                     (lambda (a b)
                       (if (<= (xaccTransGetDate (xaccSplitGetParent a)) to-date)
                           (+ (xaccSplitGetAmount a) b)
                           b))
                     0 lot-splits))
               (bal (if receivable? bal (- bal)))
               (date (if (eq? date-type 'postdate)
                         (gncInvoiceGetDatePosted invoice)
                         (gncInvoiceGetDateDue invoice))))
          (gnc:msg "next " (gnc:strify (car splits))
                   " invoice " (gnc:strify invoice) " bal " bal)
          (let loop ((idx 0) (bucket-dates bucket-dates))
            (if (< date (car bucket-dates))
                (addbucket! idx bal)
                (loop (1+ idx) (cdr bucket-dates))))
          (lp (cdr splits))))

       ;; next split is a payment. analyze using
       ;; gnc:payment-txn->payment-info and use its overpayment
       ((eqv? (xaccTransGetTxnType (xaccSplitGetParent (car splits)))
              TXN-TYPE-PAYMENT)
        (let* ((txn (xaccSplitGetParent (car splits)))
               (payment-info (gnc:payment-txn->payment-info txn))
               (overpayment (vector-ref payment-info 2)))
          (define (not-txn? s) (not (equal? txn (xaccSplitGetParent s))))
          (gnc:msg "next " (gnc:strify (car splits))
                   " overpayment " overpayment)
          (addbucket! (1- num-buckets) (if receivable? (- overpayment) overpayment))
          (lp (filter not-txn? splits))))

       ;; not invoice/prepayment. regular or payment split.
       (else
        (gnc:msg "next " (gnc:strify (car splits)) " skipped")
        (lp (cdr splits)))))))

;; ***************************************************************************

(define (gnc:not-all-zeros data)
  (cond
   ((number? data) (not (= 0 data)))
   ((pair? data) (any gnc:not-all-zeros data))
   (else #f)))

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
                (xaccAccountGetCommodity
                 (xaccSplitGetAccount spl))
                (xaccSplitGetAmount spl)))
              (gnc:monetary->string
               (gnc:make-gnc-monetary
                (xaccTransGetCurrency txn)
                (xaccSplitGetValue spl))))))
  (define (trans->str txn)
    (format #f "Txn<d:~a,desc:~a>"
            (qof-print-date (xaccTransGetDate txn))
            (xaccTransGetDescription txn)))
  (define (account->str acc)
    (format #f "Acc<~a>" (xaccAccountGetName acc)))
  (define (monetary-collector->str coll)
    (format #f "coll<~a>"
            (map gnc:strify (coll 'format gnc:make-gnc-monetary #f))))
  (define (value-collector->str coll)
    (format #f "coll<~a>" (coll 'total #f)))
  (define (procedure->str proc)
    (format #f "Proc<~a>"
            (or (procedure-name proc) "unk")))
  (define (monetary->string mon)
    (format #f "[~a]"
            (gnc:monetary->string mon)))
  (define (owner->str owner)
    (format #f "[~a:~a]"
            (gncOwnerGetTypeString owner)
            (gncOwnerGetName owner)))
  (define (invoice->str inv)
    (format #f "~a<Post:~a,Owner:~a,Notes:~a,Total:~a>"
            (gncInvoiceGetTypeString inv)
            (qof-print-date (gncInvoiceGetDatePosted inv))
            (gncOwnerGetName (gncInvoiceGetOwner inv))
            (gncInvoiceGetNotes inv)
            (monetary->string (gnc:make-gnc-monetary
                               (gncInvoiceGetCurrency inv)
                               (gncInvoiceGetTotal inv)))))
  (define (lot->str lot)
    (format #f "Lot<Acc:~a,Title:~a,Notes:~a,Balance:~a,NSplits:~a>"
            (xaccAccountGetName (gnc-lot-get-account lot))
            (gnc-lot-get-title lot)
            (gnc-lot-get-notes lot)
            (gnc-lot-get-balance lot)
            (gnc-lot-count-splits lot)))
  (define (record->str rec)
    (let ((rtd (record-type-descriptor rec)))
      (define (fld->str fld)
        (format #f "~a=~a" fld (gnc:strify ((record-accessor rtd fld) rec))))
      (format #f "Rec:~a{~a}"
              (record-type-name rtd)
              (string-join (map fld->str (record-type-fields rtd)) ", "))))
  (define (hash-table->str hash)
    (string-append
     "Hash(" (string-join
              (hash-map->list (lambda (k v) (format #f "~a=~a" k v)) hash) ",")
     ")"))
  (define (try proc)
    ;; Try proc with d as a parameter, catching exceptions to return
    ;; #f to the (or) evaluator below.
    (catch #t
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
      (and (vector? d) (string-append
                        "(vector "
                        (string-join (map gnc:strify (vector->list d)) " ")
                        ")"))
      (and (pair? d) (format #f "(~a . ~a)"
                             (gnc:strify (car d))
                             (if (eq? (car d) 'absolute)
                                 (qof-print-date (cdr d))
                                 (gnc:strify (cdr d)))))
      (try monetary-collector->str)
      (try value-collector->str)
      (try procedure->str)
      (try gnc-commodity-get-mnemonic)
      (try account->str)
      (try split->str)
      (try trans->str)
      (try monetary->string)
      (try gnc-budget-get-name)
      (try owner->str)
      (try invoice->str)
      (try lot->str)
      (try hash-table->str)
      (try record->str)
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

(define (maybe-str name str)
  (match str
    ((or "" #f) "")
    (_ (format #f " ~a<~a>" name str))))

(define-public (gnc:dump-split s show-acc?)
  (define txn (xaccSplitGetParent s))
  (format #t "~a Split ~a: ~a~a Amt<~a> Val<~a> ~a~a~a\n"
          (xaccSplitGetReconcile s)
          (string-take (gncSplitGetGUID s) 8)
          (qof-print-date (xaccTransGetDate txn))
          (maybe-str 'Acc (and show-acc? (xaccAccountGetName (xaccSplitGetAccount s))))
          (gnc:monetary->string
           (gnc:make-gnc-monetary
            (xaccAccountGetCommodity (xaccSplitGetAccount s))
            (xaccSplitGetAmount s)))
          (gnc:monetary->string
           (gnc:make-gnc-monetary
            (xaccTransGetCurrency txn)
            (xaccSplitGetValue s)))
          (maybe-str 'Desc (xaccTransGetDescription txn))
          (maybe-str 'Action (xaccSplitGetAction s))
          (maybe-str 'Memo (xaccSplitGetMemo s))))

(define-public (gnc:dump-all-transactions)
  (define query (qof-query-create-for-splits))
  (define (split-has-no-account? split) (null? (xaccSplitGetAccount split)))
  (display "\n(gnc:dump-all-transactions)\n")
  (qof-query-set-book query (gnc-get-current-book))
  (qof-query-set-sort-order query (list SPLIT-TRANS TRANS-DATE-POSTED) '() '())
  (qof-query-set-sort-increasing query #t #t #t)
  (let lp ((splits (xaccQueryGetSplitsUniqueTrans query)))
    (newline)
    (match splits
      (() (qof-query-destroy query))
      (((? split-has-no-account?) . rest) (lp rest))
      ((split . rest)
       (let ((trans (xaccSplitGetParent split)))
         (format #t "  Trans ~a: ~a Curr ~a ~a~a\n"
                 (string-take (gncTransGetGUID trans) 8)
                 (qof-print-date (xaccTransGetDate trans))
                 (gnc-commodity-get-mnemonic (xaccTransGetCurrency trans))
                 (maybe-str 'Desc (xaccTransGetDescription trans))
                 (maybe-str 'Notes (xaccTransGetNotes trans)))
         (for-each (cut gnc:dump-split <> #t) (xaccTransGetSplitList trans))
         (lp rest))))))

;; utility function for testing. dumps the whole book contents to
;; console.
(define (gnc:dump-book)
  (display "\n(gnc:dump-book)\n")
  (for-each
   (lambda (acc)
     (format #t "\nAccount ~a: ~a Comm<~a> Type<~a>\n"
             (string-take (gncAccountGetGUID acc) 8)
             (gnc-account-get-full-name acc)
             (gnc-commodity-get-mnemonic (xaccAccountGetCommodity acc))
             (xaccAccountGetTypeStr (xaccAccountGetType acc)))
     (for-each (cut gnc:dump-split <> #f) (xaccAccountGetSplitList acc))
     (format #t "         Balance: ~a Cleared: ~a Reconciled: ~a\n"
             (gnc:monetary->string
              (gnc:make-gnc-monetary
               (xaccAccountGetCommodity acc)
               (xaccAccountGetBalance acc)))
             (gnc:monetary->string
              (gnc:make-gnc-monetary
               (xaccAccountGetCommodity acc)
               (xaccAccountGetClearedBalance acc)))
             (gnc:monetary->string
              (gnc:make-gnc-monetary
               (xaccAccountGetCommodity acc)
               (xaccAccountGetReconciledBalance acc)))))
   (gnc-account-get-descendants-sorted
    (gnc-get-current-root-account))))

;; dump all invoices posted into an AP/AR account
(define (gnc:dump-invoices)
  (display "\n(gnc:dump-invoices)\n")
  (let* ((acc-APAR (filter (compose xaccAccountIsAPARType xaccAccountGetType)
                           (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account))))
         (inv-txns (filter (lambda (t) (eqv? (xaccTransGetTxnType t) TXN-TYPE-INVOICE))
                           (map xaccSplitGetParent
                                (append-map xaccAccountGetSplitList acc-APAR))))
         (invoices (map gncInvoiceGetInvoiceFromTxn inv-txns)))
    (define (maybe-date time64)         ;handle INT-MAX differently
      (if (= 9223372036854775807 time64) "?" (qof-print-date time64)))
    (define (maybe-trunc str)
      (if (> (string-length str) 20) (string-append (substring str 0 17) "...") str))
    (define (inv-amt->string inv amt)
      (gnc:monetary->string
       (gnc:make-gnc-monetary
        (gncInvoiceGetCurrency inv) amt)))
    (for-each
     (lambda (inv)
       (format #t "Invoice: ID<~a> Owner<~a> Account<~a>\n"
               (gncInvoiceGetID inv)
               (gncOwnerGetName (gncInvoiceGetOwner inv))
               (xaccAccountGetName (gncInvoiceGetPostedAcc inv)))
       (format #t "   Date: Open<~a> Post<~a> Due<~a>\n"
               (maybe-date (gncInvoiceGetDateOpened inv))
               (maybe-date (gncInvoiceGetDatePosted inv))
               (maybe-date (gncInvoiceGetDateDue inv)))
       (for-each
        (lambda (entry)
          (format #t "  Entry: Date<~a> Desc<~a> Action<~a> Notes<~a> Qty<~a>\n"
                  (maybe-date (gncEntryGetDate entry))
                  (maybe-trunc (gncEntryGetDescription entry))
                  (maybe-trunc (gncEntryGetAction entry))
                  (maybe-trunc (gncEntryGetNotes entry))
                  (gncEntryGetQuantity entry)))
        (gncInvoiceGetEntries inv))
       (format #t " Totals: Total<~a> TotalSubtotal<~a> TotalTax<~a>\n"
               (inv-amt->string inv (gncInvoiceGetTotal inv))
               (inv-amt->string inv (gncInvoiceGetTotalSubtotal inv))
               (inv-amt->string inv (gncInvoiceGetTotalTax inv)))
       (newline))
     invoices)))

(define (gnc:dump-lot lot)
  (display "gnc:dump-lot: ")
  (display (gnc:strify lot))
  (newline)
  (for-each
   (lambda (s)
     (display "Lot-split: ")
     (display (gnc:strify s))
     (newline))
   (gnc-lot-get-split-list lot)))
