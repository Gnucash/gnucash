;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commodity-utilities.scm: Functions for handling different commodities.
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "commodity-utilities.scm")
(gnc:depend "report-utilities.scm")

;; Returns true if the commodity comm represents a currency, false if
;; it represents a stock or mutual-fund.
(define (gnc:commodity-is-currency? comm)
  (equal? GNC_COMMODITY_NS_ISO
	  (gnc:commodity-get-namespace comm)))

;; All the functions below up to gnc:make-exchange-fn are calculating
;; the exchange rate for different commodities by determining the
;; weighted average of all currency transactions.

;; Returns a list of all splits in the currency-accounts up to
;; end-date which have two *different* commodities involved.
(define (gnc:get-all-commodity-splits 
	 currency-accounts end-date-tp)
  (let ((query (gnc:malloc-query))
	(splits #f))
    
    (gnc:query-set-group query (gnc:get-current-group))
    (gnc:query-add-account-match 
     query (gnc:list->glist currency-accounts)
     'acct-match-any 'query-and)
    (gnc:query-add-date-match-timepair 
     query #f end-date-tp #t end-date-tp 'query-and) 
    
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
    splits))


;; Go through all toplevel non-report-commodity balances in sumlist
;; and add them to report-commodity, if possible. This function takes
;; a sumlist (described below) and returns an alist similar to one
;; value of the sumlist's alist, e.g. (cadr (assoc report-commodity
;; sumlist))). This resulting alist can immediately be plugged into
;; gnc:make-exchange-alist.
(define (gnc:resolve-unknown-comm sumlist report-commodity)
  ;; reportlist contains all known transactions with the
  ;; report-commodity, and now the transactions with unknown
  ;; currencies should be added to that list (with an appropriate
  ;; exchange rate).
  (let ((reportlist (cadr (assoc report-commodity sumlist))))

    ;; Helper function to calculate (a*b)/c and create the new pair of
    ;; numeric-collectors, where [abc] are numeric-collectors. See the
    ;; real variable names below.
    (define (make-newrate unknown-coll un->known-coll known-pair)
      (let ((a (gnc:make-numeric-collector))
	    (b (gnc:make-numeric-collector)))
	(a 'add (unknown-coll 'total #f))
	(b 'add 
	   ;; round to (at least) 8 significant digits
	   (gnc:numeric-div
	    (gnc:numeric-mul 
	     (un->known-coll 'total #f) 
	     ((cdadr known-pair) 'total #f)
	     GNC-DENOM-AUTO 
	     (logior (GNC-DENOM-SIGFIGS 9) GNC-RND-ROUND))
	    ((caadr known-pair) 'total #f)
	    GNC-DENOM-AUTO 
	    (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND)))
	;; in other words: (/ (* (caadr un->known-coll) (cdadr
	;; known-pair)) (caadr known-pair) ))
	(cons a b)))

    ;; Go through sumlist.
    (for-each
     (lambda (otherlist)
       (if (not (gnc:commodity-equiv? (car otherlist) report-commodity))
	   (for-each
	    (lambda (pair)
	      ;; pair-{a,b}: Try to find either the currency of
	      ;; otherlist or of pair in reportlist.
	      (let ((pair-a (assoc (car otherlist) reportlist))
		    (pair-b (assoc (car pair) reportlist))
		    (rate (gnc:numeric-zero)))
		(if (and (not pair-a) (not pair-b))
		    ;; If neither the currency of otherlist nor of
		    ;; pair was found in reportlist then we can't
		    ;; resolve the exchange rate to this currency.
		    (warn "gnc:resolve-unknown-comm:" 
			  "can't calculate rate for "
			  (gnc:commodity-value->string 
			   (list (car pair) ((caadr pair) 'total #f)))
			  " = "
			  (gnc:commodity-value->string 
			   (list (car otherlist) ((cdadr pair) 'total #f)))
			  " to "
			  (gnc:commodity-value->string 
			   (list report-commodity (gnc:numeric-zero))))
		    (if (and pair-a pair-b)
			;; If both currencies are found then something
			;; went wrong inside
			;; gnc:get-exchange-totals. FIXME: Find a
			;; better thing to do in this case.
			(warn "gnc:resolve-unknown-comm:" 
			      "Oops - exchange rate ambiguity error: "
			      (gnc:commodity-value->string 
			       (list (car pair) ((caadr pair) 'total #f)))
			      " = "
				(gnc:commodity-value->string 
				 (list (car otherlist) 
				       ((cdadr pair) 'total #f))))
			(let 
			    ;; Usual case: one of pair-{a,b} was found
			    ;; in reportlist, i.e. this transaction
			    ;; can be resolved to report-commodity.
			    ((newrate
			      (if (not pair-a)
				  (list (car otherlist)
					(make-newrate (cdadr pair) 
						      (caadr pair) pair-b))
				  (list (car pair)
					(make-newrate (caadr pair) 
						      (cdadr pair) pair-a)))))
			  ;; (warn "created new rate: "
			  ;; (gnc:commodity-value->string (list (car
			  ;; newrate) ((caadr newrate) 'total #f))) "
			  ;; = " (gnc:commodity-value->string (list
			  ;; report-commodity ((cdadr newrate) 'total
			  ;; #f))))
			  (set! reportlist (cons newrate reportlist)))))))
	    (cadr otherlist))))
     sumlist)

    ;; Return the reportlist.
    reportlist))
;; Some thoughts: In the (and (not pair-a) (not pair-b)) case above we
;; will have unresolvable transaction exchange rates. But there might
;; be cases where we will be able to resolve this, but only after one
;; or more runs of gnc:resolve-unknown-comm. Maybe we could transform
;; this functions to use some kind of recursiveness.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In progress: A suggested function to calculate the weighted average
;; exchange rate between all commodities and the
;; report-commodity. Uses all currency transactions up until the
;; end-date. Returns an alist, see sumlist.
(define (gnc:get-exchange-totals report-commodity end-date)
  (let ((curr-accounts 
	 (filter gnc:account-has-shares? (gnc:group-get-subaccounts
					  (gnc:get-current-group))))
	;; sumlist: a multilevel alist. Each element has a commodity
	;; as key, and another alist as a value. The value-alist's
	;; elements consist of a commodity as a key, and a pair of two
	;; value-collectors as value, e.g. with only one (the report-)
	;; commodity DEM in the outer alist: ( {DEM ( [USD (400 .
	;; 1000)] [FRF (300 . 100)] ) } ) where DEM,USD,FRF are
	;; <gnc:commodity> and the numbers are a numeric-collector
	;; which in turn store a <gnc:numeric>. In the example, USD
	;; 400 were bought for an amount of DEM 1000, FRF 300 were
	;; bought for DEM 100. The reason for the outer alist is that
	;; there might be commodity transactions which do not involve
	;; the report-commodity, but which can still be calculated
	;; after *all* transactions are processed.
	(sumlist (list (list report-commodity '()))))

    (if (not (null? curr-accounts))
	;; Go through all splits and add up all value-amounts
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
		  (comm-list (if (not tmp) 
				 (assoc account-comm sumlist)
				 tmp)))
	       
	     ;; entry exists already in comm-list?
	     (if (not comm-list)
		 ;; no, create sub-alist from scratch
		 (let ((pair (list transaction-comm
				   (cons (gnc:make-numeric-collector)
					 (gnc:make-numeric-collector)))))
		   ((caadr pair) 'add value-amount)
		   ((cdadr pair) 'add share-amount)
		   (set! comm-list (list account-comm (list pair)))
		   ;; and add the new sub-alist to sumlist.
		   (set! sumlist (cons comm-list sumlist)))
		 ;; yes, check for second commodity.
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
		   ;; if not, create a new entry in comm-list.
		   (if (not pair)
		       (begin
			 (set! 
			  pair (list (car foreignlist)
				     (cons (gnc:make-numeric-collector) 
					   (gnc:make-numeric-collector))))
			 (set! 
			  comm-list (list (car comm-list) 
					  (cons pair (cadr comm-list))))
			 (set! 
			  sumlist (cons comm-list 
					(alist-delete 
					 (car comm-list) sumlist)))))
		   ;; And add the balances to the comm-list entry.
		   ((caadr pair) 'add (cadr foreignlist))
		   ((cdadr pair) 'add (caddr foreignlist))))))
	 (gnc:get-all-commodity-splits curr-accounts end-date)))
  
    (gnc:resolve-unknown-comm sumlist report-commodity)))

;; Anybody feel free to reimplement any of these functions, either in
;; scheme or in C. -- cstim

(define (gnc:make-exchange-alist report-commodity end-date)
  ;; This returns the alist with the actual exchange rates, i.e. the
  ;; total balances from get-exchange-totals are divided by each
  ;; other.
  (map 
   (lambda (e)
     (list (car e) 
	   (gnc:numeric-abs
	    (gnc:numeric-div ((cdadr e) 'total #f) 
			     ((caadr e) 'total #f)
			     GNC-DENOM-AUTO 
			     (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND)))))
   (gnc:get-exchange-totals report-commodity end-date)))

;; This one returns the ready-to-use function for calculation of the
;; exchange rates. The returned function takes a <gnc-monetary> and
;; the domestic-commodity, exchanges the amount into the domestic
;; currency and returns a <gnc-monetary>.
(define (gnc:make-exchange-function exchange-alist)
  (let ((exchangelist exchange-alist))
    (lambda (foreign domestic)
      (if foreign
	  (gnc:make-gnc-monetary 
	   domestic
	   (let ((pair (assoc (gnc:gnc-monetary-commodity foreign) 
			      exchangelist)))
	     (if (not pair)
		 (gnc:numeric-zero)
		 (gnc:numeric-mul (gnc:gnc-monetary-amount foreign) 
				  (cadr pair)
                                  (gnc:commodity-get-fraction domestic)
				  GNC-RND-ROUND))))
	  #f))))

;; Helper for gnc:exchange-by-pricedb-* below. 'price' gets tested for
;; #f here, and gets unref'd here too. Returns a <gnc:monetary>.
(define (gnc:exchange-by-price-helper
	 foreign domestic price)
  (if (gnc:gnc-monetary? foreign)
      (gnc:make-gnc-monetary 
       domestic
       (if price
	   (let ((result
		  (gnc:numeric-mul (gnc:gnc-monetary-amount foreign) 
				   (gnc:price-get-value price)
				   (gnc:commodity-get-fraction domestic)
				   GNC-RND-ROUND)))
	     (gnc:price-unref price)
	     result)
	   (begin
	     (warn "gnc:exchange-by-price-helper: No price found for "
		   (gnc:commodity-value->string foreign) " into "
		   (gnc:commodity-value->string 
		    (list domestic (gnc:numeric-zero))))
	     (gnc:numeric-zero))))
      #f))

;; This is another ready-to-use function for calculation of exchange
;; rates. (Note that this is already the function itself. It doesn't
;; return a function as opposed to make-exchange-function.) It takes
;; the <gnc-monetary> 'foreign' amount and the <gnc:commodity*>
;; 'domestic' commodity. It exchanges the amount into the domestic
;; currency, using the latest price from the pricedb. The function
;; returns a <gnc-monetary>.
(define (gnc:exchange-by-pricedb-latest 
	 foreign domestic)
  (if (and (record? foreign) (gnc:gnc-monetary? foreign))
      (gnc:exchange-by-price-helper
       foreign domestic
       (gnc:pricedb-lookup-latest 
	(gnc:gnc-monetary-commodity foreign)
	domestic))
      #f))

;; Yet another ready-to-use function for calculation of exchange
;; rates. (Note that this is already the function itself. It doesn't
;; return a function as opposed to make-exchange-function.) It takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity *and* a <gnc:time-pair> 'date'. It exchanges
;; the amount into the domestic currency, using a price from the
;; pricedb according to the given date. The function returns a
;; <gnc-monetary>.
(define (gnc:exchange-by-pricedb-nearest
	 foreign domestic date)
  (if (and (record? foreign) (gnc:gnc-monetary? foreign)
	   date)
      (gnc:exchange-by-price-helper
       foreign domestic
       (gnc:pricedb-lookup-nearest-in-time
	(gnc:gnc-monetary-commodity foreign)
	domestic date))
      #f))


;; Adds all different commodities in the commodity-collector <foreign>
;; by using the exchange rates of <exchange-fn> to calculate the
;; exchange rates to the commodity <domestic>. Returns a
;; <gnc-monetary> with the domestic commodity and its corresponding
;; balance. If the foreign balance is #f, it returns #f.
(define (gnc:sum-collector-commodity foreign domestic exchange-fn)
  (if foreign
      (let ((balance (gnc:make-commodity-collector)))
	(foreign
	 'format 
	 (lambda (curr val) 
	   (if (gnc:commodity-equiv? domestic curr)
	       (balance 'add domestic val)
	       (balance 'add domestic 
			(gnc:gnc-monetary-amount 
			 (exchange-fn (gnc:make-gnc-monetary curr val) 
				      domestic)))))
	 #f)
	(balance 'getmonetary domestic #f))
      #f))

;; As above, but adds only the commodities of other stocks and
;; mutual-funds. Returns a commodity-collector which (still) may have
;; several different commodities in it -- if there have been different
;; *currencies*, not only stocks.
(define (gnc:sum-collector-stocks foreign domestic exchange-fn)
  (if foreign
      (let ((balance (gnc:make-commodity-collector)))
	(foreign
	 'format 
	 (lambda (curr val) 
	   (if (gnc:commodity-equiv? domestic curr)
	       (balance 'add domestic val)
	       (if (gnc:commodity-is-currency? curr)
		   (balance 'add curr val)
		   (balance 'add domestic 
			    (gnc:gnc-monetary-amount 
			     (exchange-fn (gnc:make-gnc-monetary curr val) 
					  domestic))))))
	 #f)
	balance)
      #f))
