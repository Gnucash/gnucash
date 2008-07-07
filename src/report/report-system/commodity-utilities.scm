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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (gnc-commodity-collector-contains-commodity? collector commodity)
    (let ((ret #f))
        (gnc-commodity-collector-map
	    collector
	    (lambda (comm amt)
		(set! ret (or ret (gnc-commodity-equiv comm commodity)))))
	ret
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to get splits with interesting data from accounts.


;; Returns a list of all splits in the 'currency-accounts' up to
;; 'end-date-tp' which have two different commodities involved, one of
;; which is equivalent to 'commodity' (the latter constraint only if
;; 'commodity' != #f ).
(define (gnc:get-match-commodity-splits 
	 currency-accounts end-date-tp commodity)
  (let ((query (qof-query-create-for-splits))
	(splits #f))
    
    (qof-query-set-book query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
    (xaccQueryAddAccountMatch query
                                 currency-accounts
                                 QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTS
     query #f end-date-tp #t end-date-tp QOF-QUERY-AND)
    
    ;; Get the query result, i.e. all splits in currency
    ;; accounts.
    (set! splits (filter 
		  ;; Filter such that we get only those splits
		  ;; which have two *different* commodities
		  ;; involved.
		  (lambda (s) (let ((trans-comm
				     (xaccTransGetCurrency
				      (xaccSplitGetParent s)))
				    (acc-comm
				     (xaccAccountGetCommodity
				      (xaccSplitGetAccount s))))
				(and
				 (not (gnc-commodity-equiv
				       trans-comm acc-comm))
				 (or
				  (not commodity)
				  (gnc-commodity-equiv
				   commodity trans-comm)
				  (gnc-commodity-equiv
				   commodity acc-comm)))))
		  (qof-query-run query)))
    (qof-query-destroy query)
    splits))

;; Returns a sorted list of all splits in the 'currency-accounts' up
;; to 'end-date-tp' which have the 'commodity' and one other commodity
;; involved. The splits are sorted by date.
(define (gnc:get-match-commodity-splits-sorted currency-accounts
                                               end-date-tp
                                               commodity)
  (sort (gnc:get-match-commodity-splits currency-accounts 
					end-date-tp commodity)
	(lambda (a b)
	  (gnc:timepair-lt 
	   (gnc-transaction-get-date-posted (xaccSplitGetParent a))
	   (gnc-transaction-get-date-posted (xaccSplitGetParent b))))))


;; Returns a list of all splits in the currency-accounts up to
;; end-date which have two *different* commodities involved.
(define (gnc:get-all-commodity-splits currency-accounts end-date-tp)
  (gnc:get-match-commodity-splits currency-accounts end-date-tp #f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to create some list of prices from data in transactions.


;; Helper for warnings below.
(define (gnc-commodity-numeric->string commodity numeric)
  (gnc:monetary->string
   (gnc:make-gnc-monetary commodity numeric)))

;; Helper for exchange below
(define (gnc:exchange-by-euro-numeric
	 foreign-commodity foreign-numeric domestic date)
  (gnc:exchange-by-euro
   (gnc:make-gnc-monetary foreign-commodity foreign-numeric)
   domestic date))

;; Returns true if the given pricealist element is a non-zero price.
(define (gnc:price-is-not-zero? elem)
  (not (gnc-numeric-zero-p (second elem))))

;; Create a list of all prices of 'price-commodity' measured in the
;; currency 'report-currency'. The prices are taken from all splits in
;; 'currency-accounts' up until the date 'end-date-tp'. Returns a list
;; of lists. Each listelement looks like the list (time price), where
;; 'time' is the timepair when the <gnc:numeric*> 'price' was valid.
(define (gnc:get-commodity-totalavg-prices
	 currency-accounts end-date-tp price-commodity report-currency)
  (let ((total-foreign (gnc-numeric-zero))
	(total-domestic (gnc-numeric-zero)))
    (filter 
     gnc:price-is-not-zero?
     (map-in-order
      (lambda (a)
	(let* ((transaction-comm (xaccTransGetCurrency
				  (xaccSplitGetParent a)))
	       (account-comm (xaccAccountGetCommodity
			      (xaccSplitGetAccount a)))
	       (share-amount (gnc-numeric-abs
			      (xaccSplitGetAmount a)))
	       (value-amount (gnc-numeric-abs
			      (xaccSplitGetValue a)))
	       (transaction-date (gnc-transaction-get-date-posted
				  (xaccSplitGetParent a)))
	       (foreignlist
		(if (gnc-commodity-equiv transaction-comm
					  price-commodity)
		    (list account-comm
			  share-amount value-amount)
		    (list transaction-comm
			  value-amount share-amount))))
	  
	  ;;(warn "gnc:get-commodity-totalavg-prices: value " 
	  ;;    (gnc-commodity-numeric->string
	  ;;(first foreignlist) (second foreignlist))
	  ;;      " bought shares "
	  ;;    (gnc-commodity-numeric->string
	  ;;price-commodity (third foreignlist)))

	  ;; Try EURO exchange if necessary
	  (if (not (gnc-commodity-equiv (first foreignlist)
					 report-currency))
	      (let ((exchanged (gnc:exchange-by-euro-numeric
				(first foreignlist) (second foreignlist)
				report-currency transaction-date)))
		(if exchanged
		    (set! foreignlist
			  (list report-currency
				(gnc:gnc-monetary-amount exchanged)
				(third foreignlist))))))
	  
	  (list
	   transaction-date
	   (if (not (gnc-commodity-equiv (first foreignlist)
					  report-currency))
	       (begin
		 (warn "gnc:get-commodity-totalavg-prices: " 
		       "Sorry, currency exchange not yet implemented:"
		       (gnc-commodity-numeric->string
			(first foreignlist) (second foreignlist))
		       " (buying "
		       (gnc-commodity-numeric->string
			price-commodity (third foreignlist))
		       ") =? "
		       (gnc-commodity-numeric->string
			report-currency (gnc-numeric-zero)))
		 (gnc-numeric-zero))
	       (begin
		 (set! total-foreign (gnc-numeric-add-fixed
				      total-foreign (third foreignlist)))
		 (set! total-domestic (gnc-numeric-add-fixed
				       total-domestic (second foreignlist)))
		 (gnc-numeric-div
		  total-domestic
		  total-foreign
		  GNC-DENOM-AUTO 
		  (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND)))))))
      ;; Get all the interesting splits, and sort them according to the
      ;; date.
      (gnc:get-match-commodity-splits-sorted
       currency-accounts 
       end-date-tp price-commodity)))))

;; Create a list of prices for all commodities in 'commodity-list',
;; i.e. the same thing as in get-commodity-totalavg-prices but
;; extended to a commodity-list. Returns an alist. Each pair consists
;; of the foreign-currency and the appropriate list from
;; gnc:get-commodity-totalavg-prices, see there.
(define (gnc:get-commoditylist-totalavg-prices
	 commodity-list report-currency end-date-tp
	 start-percent delta-percent)
  (let ((currency-accounts 
	 ;;(filter gnc:account-has-shares?  
	 ;; -- use all accounts, not only share accounts, since gnucash-1.7
	 (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
	(work-to-do (length commodity-list))
	(work-done 0))
    (map
     (lambda (c)
       (begin
	 (set! work-done (+ 1 work-done))
	 (if start-percent
	     (gnc:report-percent-done
	      (+ start-percent (* delta-percent (/ work-done work-to-do)))))
	 (cons c
	       (gnc:get-commodity-totalavg-prices
		currency-accounts end-date-tp c report-currency))))
     commodity-list)))

;; Get the instantaneous prices for the 'price-commodity', measured in
;; amounts of the 'report-currency'. The prices are taken from all
;; splits in 'currency-accounts' up until the date
;; 'end-date-tp'. Returns a list of lists. Each listelement looks like
;; the list (time price), where 'time' is the timepair when the
;; <gnc:numeric*> 'price' was valid.
(define (gnc:get-commodity-inst-prices
	 currency-accounts end-date-tp price-commodity report-currency)
  ;; go through all splits; convert all splits into a price.
  (filter 
   gnc:price-is-not-zero?
   (map-in-order
    (lambda (a)
      (let* ((transaction-comm (xaccTransGetCurrency
				(xaccSplitGetParent a)))
	     (account-comm (xaccAccountGetCommodity
			    (xaccSplitGetAccount a)))
	     (share-amount (gnc-numeric-abs
			    (xaccSplitGetAmount a)))
	     (value-amount (gnc-numeric-abs
			    (xaccSplitGetValue a)))
	     (transaction-date (gnc-transaction-get-date-posted
				(xaccSplitGetParent a)))
	     (foreignlist 
	      (if (gnc-commodity-equiv transaction-comm price-commodity)
		  (list account-comm
			share-amount value-amount)
		  (list transaction-comm
			value-amount share-amount))))
	
	;;(warn "get-commodity-inst-prices: value " 
	;;    (gnc-commodity-numeric->string
	;;   (first foreignlist) (second foreignlist))
	;; " bought shares "
	;;(gnc-commodity-numeric->string
	;; price-commodity (third foreignlist)))
	
	;; Try EURO exchange if necessary
	(if (not (gnc-commodity-equiv (first foreignlist)
				       report-currency))
	    (let ((exchanged (gnc:exchange-by-euro-numeric
			      (first foreignlist) (second foreignlist)
			      report-currency transaction-date)))
	      (if exchanged
		  (set! foreignlist
			(list report-currency
			      (gnc:gnc-monetary-amount exchanged)
			      (third foreignlist))))))
	
	(list
	 transaction-date
	 (if (not (gnc-commodity-equiv (first foreignlist)
					report-currency))
	     (begin
	       (warn "get-commodity-inst-prices: " 
		     "Sorry, currency exchange not yet implemented:"
		     (gnc-commodity-numeric->string
		      (first foreignlist) (second foreignlist))
		     " (buying "
		     (gnc-commodity-numeric->string
		      price-commodity (third foreignlist))
		     ") =? "
		     (gnc-commodity-numeric->string
		      report-currency (gnc-numeric-zero)))
	       (gnc-numeric-zero))
	     (gnc-numeric-div
	      (second foreignlist)
	      (third foreignlist)
	      GNC-DENOM-AUTO 
	      (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND))))))
    ;; Get all the interesting splits, sorted by date.
    (gnc:get-match-commodity-splits-sorted
     currency-accounts 
     end-date-tp price-commodity))))

;; Get the instantaneous prices for all commodities in
;; 'commodity-list', i.e. the same thing as get-commodity-inst-prices
;; but extended to a commodity-list. Returns an alist. Each pair
;; consists of the foreign-currency and the appropriate list from
;; gnc:get-commodity-inst-prices, see there.
(define (gnc:get-commoditylist-inst-prices
	 commodity-list report-currency end-date-tp
	 start-percent delta-percent)
  (let ((currency-accounts 
	 ;;(filter gnc:account-has-shares? 
	 ;; -- use all accounts, not only share accounts, since gnucash-1.7
	 (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
	(work-to-do (length commodity-list))
	(work-done 0))
    (map
     (lambda (c)
       (begin
	 (set! work-done (+ 1 work-done))
	 (if start-percent
	     (gnc:report-percent-done
	      (+ start-percent (* delta-percent (/ work-done work-to-do)))))
	 (cons c
	       (gnc:get-commodity-inst-prices
		currency-accounts end-date-tp c report-currency))))
     commodity-list)))


;; Find the price in 'pricelist' that's nearest to 'date'. The
;; pricelist comes from
;; e.g. gnc:get-commodity-totalavg-prices. Returns a <gnc-numeric> or,
;; if pricelist was empty, #f.
(define (gnc:pricelist-price-find-nearest
	 pricelist date)
  (let* ((later (find (lambda (p) 
			(gnc:timepair-lt date (first p)))
		      pricelist))
	 (earlierlist (take-while 
		       (lambda (p) 
			 (gnc:timepair-ge date (first p)))
		       pricelist))
	 (earlier (and (not (null? earlierlist))
		       (last earlierlist))))
    ;;		(if earlier
    ;;		    (warn "earlier" 
    ;;			  (gnc-print-date (first earlier))
    ;;			  (gnc-numeric-to-double (second earlier))))
    ;;		(if later
    ;;		    (warn "later" 
    ;;			  (gnc-print-date (first later))
    ;;			  (gnc-numeric-to-double (second later))))
    
    (if (and earlier later)
	(if (< (abs (gnc:timepair-delta date (first earlier)))
	       (abs (gnc:timepair-delta date (first later))))
	    (second earlier)
	    (second later))
	(or
	 (and earlier (second earlier))
	 (and later (second later))))))


;; Find the price of the 'commodity' in the 'pricealist' that is
;; nearest to the 'date'.
(define (gnc:pricealist-lookup-nearest-in-time
	 pricealist commodity date)
  (let ((plist (assoc-ref pricealist commodity)))
    (if (and plist (not (null? plist)))
	(let ((price
	       (gnc:pricelist-price-find-nearest
		plist date)))
	  (if price
	      price
	      (gnc-numeric-zero)))
	(gnc-numeric-zero))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to get one price at a given time (i.e. not time-variant).


;; Go through all toplevel non-'report-commodity' balances in
;; 'sumlist' and add them to 'report-commodity', if possible. This
;; function takes a sumlist (described in gnc:get-exchange-totals) and
;; returns an alist similar to one value of the sumlist's alist,
;; e.g. (cadr (assoc report-commodity sumlist))). This resulting alist
;; can immediately be plugged into gnc:make-exchange-alist.
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
	   (gnc-numeric-div
	    (gnc-numeric-mul
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
       (if (not (gnc-commodity-equiv (car otherlist) report-commodity))
	   (for-each
	    (lambda (pair)
	      ;; Check whether by any accident the report-commodity
	      ;; appears here.
	      (if 
	       (not (gnc-commodity-equiv (car pair) report-commodity))
	       ;; pair-{a,b}: Try to find either the currency of
	       ;; otherlist or of pair in reportlist.
	       (let ((pair-a 
		      (or
		       ;; Find the otherlist's currency in reportlist
		       (assoc (car otherlist) reportlist)
		       ;; Or try whether that's an Euro currency.
		       (let 
			   ((euro-monetary
			     (gnc:exchange-by-euro (gnc:make-gnc-monetary
						    (car otherlist)
						    ((cdadr pair) 'total #f))
						   report-commodity #f)))
			 ;; If this is an Euro currency, create the
			 ;; pair of appropriately exchanged amounts.
			 (if euro-monetary
			     (let ((a (gnc:make-numeric-collector)))
			       (a 'add 
				  (gnc:gnc-monetary-amount euro-monetary))
			       (list report-commodity
				     (cons (cdadr pair) a)))
			     #f))))
		     ;; Find the pair's currency in reportlist. FIXME:
		     ;; Also try the Euro here.
		     (pair-b (assoc (car pair) reportlist))
		     (rate (gnc-numeric-zero)))
		 (if (and (not pair-a) (not pair-b))
		     ;; If neither the currency of otherlist nor of
		     ;; pair was found in reportlist then we can't
		     ;; resolve the exchange rate to this currency.
		     (warn "gnc:resolve-unknown-comm:" 
			   "can't calculate rate for "
			   (gnc-commodity-value->string
			    (list (car pair) ((caadr pair) 'total #f)))
			   " = "
			   (gnc-commodity-value->string
			    (list (car otherlist) ((cdadr pair) 'total #f)))
			   " to "
			   (gnc-commodity-value->string
			    (list report-commodity (gnc-numeric-zero))))
		     (if (and pair-a pair-b)
			 ;; If both currencies are found then something
			 ;; went wrong inside
			 ;; gnc:get-exchange-totals. FIXME: Find a
			 ;; better thing to do in this case.
			 (warn "gnc:resolve-unknown-comm:" 
			       "Oops - exchange rate ambiguity error: "
			       (gnc-commodity-value->string
				(list (car pair) ((caadr pair) 'total #f)))
			       " = "
			       (gnc-commodity-value->string
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
			   ;; (gnc-commodity-value->string (list (car
			   ;; newrate) ((caadr newrate) 'total #f))) "
			   ;; = " (gnc-commodity-value->string (list
			   ;; report-commodity ((cdadr newrate) 'total
			   ;; #f))))
			   (set! reportlist (cons newrate reportlist))))))
	       ;; Huh, the report-currency showed up on the wrong side
	       ;; -- we will just add it to the reportlist on the
	       ;; right side.
	       (let ((newrate (list (car otherlist) 
				    (cons (cdadr pair) (caadr pair)))))
		 ;; (warn "created new rate: "
		 ;; (gnc-commodity-value->string (list (car newrate)
		 ;; ((caadr newrate) 'total #f))) " = "
		 ;; (gnc-commodity-value->string (list
		 ;; report-commodity ((cdadr newrate) 'total #f))))
		 (set! reportlist (cons newrate reportlist)))))
	    (cadr otherlist))))
     sumlist)

    ;; Return the reportlist.
    reportlist))
;; Some thoughts: In the (and (not pair-a) (not pair-b)) case above we
;; will have unresolvable transaction exchange rates. But there might
;; be cases where we will be able to resolve this, but only after one
;; or more runs of gnc:resolve-unknown-comm. Maybe we could transform
;; this functions to use some kind of recursiveness.


;; Calculate the weighted average exchange rate between all
;; commodities and the 'report-commodity'. Uses all currency
;; transactions up until the 'end-date'. Returns an alist, see
;; sumlist.
(define (gnc:get-exchange-totals report-commodity end-date)
  (let ((curr-accounts 
	 ;;(filter gnc:account-has-shares? ))
	 ;; -- use all accounts, not only share accounts, since gnucash-1.7
	 (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
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
	   (let* ((transaction-comm (xaccTransGetCurrency
				     (xaccSplitGetParent a)))
		  (account-comm (xaccAccountGetCommodity
				 (xaccSplitGetAccount a)))
		  ;; Always use the absolute value here.
		  (share-amount (gnc-numeric-abs
				 (xaccSplitGetAmount a)))
		  (value-amount (gnc-numeric-abs
				 (xaccSplitGetValue a)))
		  (tmp (assoc transaction-comm sumlist))
		  (comm-list (if (not tmp) 
				 (assoc account-comm sumlist)
				 tmp)))
	       
             (cond ((gnc-numeric-zero-p share-amount)
                    ;; Without shares this is not a buy or sell; ignore it.
                    #f)

                   ((not comm-list)
	            ;; entry doesn't exist in comm-list
		    ;; create sub-alist from scratch
		    (let ((pair (list transaction-comm
				      (cons (gnc:make-numeric-collector)
					    (gnc:make-numeric-collector)))))
		      ((caadr pair) 'add value-amount)
		      ((cdadr pair) 'add share-amount)
		      (set! comm-list (list account-comm (list pair)))
		      ;; and add the new sub-alist to sumlist.
		      (set! sumlist (cons comm-list sumlist))))

                   (else
		    (let* 
		          ;; Put the amounts in the right place.
		          ((foreignlist 
		            (if (gnc-commodity-equiv transaction-comm
						      (car comm-list))
			        (list account-comm 
				      share-amount value-amount)
			        (list transaction-comm 
				      value-amount share-amount)))
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
		      ((cdadr pair) 'add (caddr foreignlist)))))))
	 (gnc:get-all-commodity-splits curr-accounts end-date)))
  
    (gnc:resolve-unknown-comm sumlist report-commodity)))

;; Calculate the volume-weighted average cost of all commodities,
;; priced in the 'report-commodity'. Uses all transactions up until
;; the 'end-date'. Returns an alist, see sumlist.
(define (gnc:get-exchange-cost-totals report-commodity end-date)
  (let ((curr-accounts 
	 ;;(filter gnc:account-has-shares? ))
	 ;; -- use all accounts, not only share accounts, since gnucash-1.7
	 (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
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
	   (let* ((transaction-comm (xaccTransGetCurrency
				     (xaccSplitGetParent a)))
		  (account-comm (xaccAccountGetCommodity
				 (xaccSplitGetAccount a)))
		  (share-amount (xaccSplitGetAmount a))
		  (value-amount (xaccSplitGetValue a))
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
		     ;; Put the amounts in the right place.
		     ((foreignlist 
		       (if (gnc-commodity-equiv transaction-comm
						 (car comm-list))
			   (list account-comm 
				 share-amount value-amount)
			   (list transaction-comm 
				 value-amount share-amount)))
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
	   (gnc-numeric-abs
	    (gnc-numeric-div ((cdadr e) 'total #f)
			     ((caadr e) 'total #f)
			     GNC-DENOM-AUTO 
			     (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND)))))
   (gnc:get-exchange-totals report-commodity end-date)))

(define (gnc:make-exchange-cost-alist report-commodity end-date)
  ;; This returns the alist with the actual exchange rates, i.e. the
  ;; total balances from get-exchange-totals are divided by each
  ;; other.
  (map 
   (lambda (e)
     (list (car e) 
	   (gnc-numeric-abs
	    (gnc-numeric-div ((cdadr e) 'total #f)
			     ((caadr e) 'total #f)
			     GNC-DENOM-AUTO 
			     (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND)))))
   (gnc:get-exchange-cost-totals report-commodity end-date)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual functions for exchanging amounts.


;; Exchange EURO currencies to each other, or returns #f if one of
;; them is not an EURO currency at the given time. The function takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity, and a <gnc:time-pair> 'date'. It exchanges
;; the amount into the domestic currency. If the 'date' is #f, it
;; doesn't check for it. Returns a <gnc-monetary>, or #f if at least
;; one of the currencies is not in the EURO.
(define (gnc:exchange-by-euro foreign domestic date)
  (and (gnc-is-euro-currency domestic)
       (gnc-is-euro-currency (gnc:gnc-monetary-commodity foreign))
       ;; FIXME: implement the date check.
       (gnc:make-gnc-monetary
	domestic
	(gnc-convert-from-euro
	 domestic
	 (gnc-convert-to-euro (gnc:gnc-monetary-commodity foreign)
			      (gnc:gnc-monetary-amount foreign))))))


;; A trivial exchange function - if the "foreign" monetary amount
;; and the domestic currency are the same, return the foreign 
;; amount unchanged, otherwise return 0

;; WARNING: many uses of exchange functions assume that the function
;; will return a valid <gnc:monetary>.  However, this function returns
;; #f if the commodities don't match.  Therefore, if you use this
;; function in a mixed commodity context, stuff will probably crash.
(define (gnc:exchange-if-same foreign domestic)
  (if (gnc-commodity-equiv (gnc:gnc-monetary-commodity foreign) domestic)
      foreign
      #f))

;; This one returns the ready-to-use function for calculation of the
;; exchange rates.  The returned function takes a <gnc-monetary> and
;; the <gnc:commodity*> domestic-commodity, exchanges the amount into
;; the domestic currency and returns a <gnc-monetary>.
(define (gnc:make-exchange-function exchange-alist)
  (let ((exchangelist exchange-alist))
    (lambda (foreign domestic)
      (begin
	(gnc:debug "foreign: " foreign)
	(gnc:debug "domestic: " domestic)
	(if foreign
	    (or (gnc:exchange-by-euro foreign domestic #f)
		(gnc:exchange-if-same foreign domestic)
		(gnc:make-gnc-monetary 
		 domestic
		 (let ((pair (assoc (gnc:gnc-monetary-commodity foreign) 
				    exchangelist)))
		   (if (not pair)
		       (gnc-numeric-zero)
		       (gnc-numeric-mul (gnc:gnc-monetary-amount foreign)
					(cadr pair)
					(gnc-commodity-get-fraction domestic)
					GNC-RND-ROUND)))))
	  #f)))))

;; Helper for the gnc:exchange-by-pricalist* below. Exchange the
;; <gnc:monetary> 'foreign' into the <gnc:commodity*> 'domestic' by
;; the <gnc:numeric> 'price-value'. Returns a <gnc:monetary>.
(define (gnc:exchange-by-pricevalue-helper
	 foreign domestic price-value)
  (if (gnc:gnc-monetary? foreign)
      (gnc:make-gnc-monetary 
       domestic
       (if price-value
	   (gnc-numeric-mul (gnc:gnc-monetary-amount foreign)
			    price-value
			    (gnc-commodity-get-fraction domestic)
			    GNC-RND-ROUND)
	   (begin
	     (warn "gnc:exchange-by-pricevalue-helper: No price found for "
		   (gnc:monetary->string foreign) " into "
		   (gnc:monetary->string
		    (gnc:make-gnc-monetary domestic (gnc-numeric-zero))))
	     (gnc-numeric-zero))))
      #f))

;; Helper for gnc:exchange-by-pricedb-* below. 'price' gets tested for
;; #f here, and gets unref'd here too. Exchange the <gnc:monetary>
;; 'foreign' into the <gnc:commodity*> 'domestic' by the <gnc:Price>
;; 'price'. Returns a <gnc:monetary>.
(define (gnc:exchange-by-pricedb-helper
	 foreign domestic price)
  (if (gnc:gnc-monetary? foreign)
      (gnc:make-gnc-monetary 
       domestic
       (if price
	   (let ((result
		  (gnc-numeric-mul (gnc:gnc-monetary-amount foreign)
				   (gnc-price-get-value price)
				   (gnc-commodity-get-fraction domestic)
				   GNC-RND-ROUND)))
	     (gnc-price-unref price)
	     result)
	   (begin
	     (warn "gnc:exchange-by-pricedb-helper: No price found for "
		   (gnc:monetary->string foreign) " into "
		   (gnc:monetary->string
		    (gnc:make-gnc-monetary domestic (gnc-numeric-zero))))
	     (gnc-numeric-zero))))
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
      (or (gnc:exchange-by-euro foreign domestic #f)
	  (gnc:exchange-if-same foreign domestic)
	   (gnc:make-gnc-monetary
	    domestic
	    (gnc-pricedb-convert-balance-latest-price
             (gnc-pricedb-get-db (gnc-get-current-book))
	     (gnc:gnc-monetary-amount foreign)
	     (gnc:gnc-monetary-commodity foreign)
	     domestic)))
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
      (or (gnc:exchange-by-euro foreign domestic date)
	  (gnc:exchange-if-same foreign domestic)
	   (gnc:make-gnc-monetary
	    domestic
	    (gnc-pricedb-convert-balance-nearest-price
             (gnc-pricedb-get-db (gnc-get-current-book))
	     (gnc:gnc-monetary-amount foreign)
	     (gnc:gnc-monetary-commodity foreign)
	     domestic (timespecCanonicalDayTime date))))
      #f))

;; Exchange by the nearest price from pricelist. This function takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity, a <gnc:time-pair> 'date' and the
;; 'pricelist'. It exchanges the amount into the domestic currency,
;; using the price nearest to 'data' found in the pricelist. The
;; function returns a <gnc-monetary>.
(define (gnc:exchange-by-pricealist-nearest
	 pricealist foreign domestic date)
  (begin 
    (gnc:debug "foreign " foreign)
    (gnc:debug "domestic " domestic)
    (gnc:debug "pricealist " pricealist)
    
    (if (and (record? foreign) (gnc:gnc-monetary? foreign)
	   date)
      (or (gnc:exchange-by-euro foreign domestic date)
	  (gnc:exchange-if-same foreign domestic)
	  (if (not (null? pricealist))
	      (gnc:exchange-by-pricevalue-helper
	       foreign domestic
	       (gnc:pricealist-lookup-nearest-in-time
		pricealist (gnc:gnc-monetary-commodity foreign) date))
	      #f))
      #f)))

       



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choosing exchange functions made easy -- get the right function by
;; the value of a multichoice option.


;; Return a ready-to-use function. Which one to use is determined by
;; the value of 'source-option', whose possible values are set in
;; gnc:options-add-price-source!.
(define (gnc:case-exchange-fn 
	 source-option report-currency to-date-tp)
  (case source-option
    ((average-cost) (gnc:make-exchange-function 
                     (gnc:make-exchange-cost-alist
                      report-currency to-date-tp)))
    ((weighted-average) (gnc:make-exchange-function 
			(gnc:make-exchange-alist 
			 report-currency to-date-tp)))
    ((pricedb-latest) gnc:exchange-by-pricedb-latest)
    ((pricedb-nearest) (lambda (foreign domestic)
			(gnc:exchange-by-pricedb-nearest
			 foreign domestic to-date-tp)))
    (else 
     (begin
       ;; FIX-ME 
       ;; this is a hack to prevent report crashing if a report
       ;; implements source-options that aren't fully implemented. We
       ;; return a reasonably sane fallback function: nearest.
       ;;
       ;; known to be missing: pricedb-latest-before
       (gnc:warn "gnc:case-exchange-fn: bad price-source value: " 
                    source-option " using pricedb-nearest.")
       (lambda (foreign domestic)
	 (gnc:exchange-by-pricedb-nearest
	  foreign domestic to-date-tp))))))

;; Return a ready-to-use function. Which one to use is determined by
;; the value of 'source-option', whose possible values are set in
;; gnc:options-add-price-source!.
;;
;; <int> start-percent, delta-percent: Fill in the [start:start+delta]
;; section of the progress bar while running this function.
;;
(define (gnc:case-exchange-time-fn 
	 source-option report-currency commodity-list to-date-tp
	 start-percent delta-percent)
  (case source-option
    ((weighted-average) (let ((pricealist
			      (gnc:get-commoditylist-totalavg-prices
			       commodity-list report-currency to-date-tp
			       start-percent delta-percent)))
			 (lambda (foreign domestic date)
			   (gnc:exchange-by-pricealist-nearest
			    pricealist foreign domestic date))))
    ((actual-transactions) (let ((pricealist
				 (gnc:get-commoditylist-inst-prices
				  commodity-list report-currency to-date-tp)))
			    (lambda (foreign domestic date)
			      (gnc:exchange-by-pricealist-nearest
			       pricealist foreign domestic date))))
    ((pricedb-latest) (lambda (foreign domestic date)
		       (gnc:exchange-by-pricedb-latest foreign domestic)))
    ((pricedb-nearest) gnc:exchange-by-pricedb-nearest)
    (else 
     (begin
       (gnc:warn "gnc:case-exchange-time-fn: bad price-source value: " 
                    source-option " using pricedb-nearest.")
       ;; FIX-ME another hack to prevent report crashing when an
       ;; unimplemented source-option comes through
       gnc:exchange-by-pricedb-nearest
       ))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions using the exchange-fn's above to get from a
;; commodity-collector to one value.


;; Adds all different commodities in the commodity-collector <foreign>
;; by using the exchange rates of <exchange-fn> to calculate the
;; exchange rates to the commodity <domestic>.
;;
;; CAS: Previously, the exchange-fn was not optional -- we would crash
;; if it was invalid.  I've changed this so that when exchange-fn is
;; #f, #f is returned.  Since #f is already returned when foreign is
;; #f, and since any previous dependance on some behavior for the case
;; where exchange-fn was #f would've crashed, I think this change is
;; safe.
;;
;; Returns a <gnc-monetary> with the domestic commodity and its
;; corresponding balance. If the foreign balance is #f, it returns #f.
(define (gnc:sum-collector-commodity foreign domestic exchange-fn)
  (cond ((and foreign exchange-fn)
         (let ((balance (gnc:make-commodity-collector)))
           (foreign
            'format
            (lambda (curr val)
              (if (gnc-commodity-equiv domestic curr)
                  (balance 'add domestic val)
                  (balance 'add domestic
                           (gnc:gnc-monetary-amount
                            ;; BUG?: this bombs if the exchange-fn
                            ;; returns #f instead of an actual
                            ;; <gnc:monetary>.  Better to just return #f.
                            (exchange-fn (gnc:make-gnc-monetary curr val)
                                         domestic))
                           )
                  )
              )
            #f)
           (balance 'getmonetary domestic #f)))
        (else #f)
        )
  )

;; As above, but adds only the commodities of other stocks and
;; mutual-funds. Returns a commodity-collector, (not a <gnc:monetary>)
;; which (still) may have several different commodities in it -- if
;; there have been different *currencies*, not only stocks.
(define (gnc:sum-collector-stocks foreign domestic exchange-fn)
  (if foreign
      (let ((balance (gnc:make-commodity-collector)))
	(foreign
	 'format 
	 (lambda (curr val) 
	   (if (gnc-commodity-equiv domestic curr)
	       (balance 'add domestic val)
	       (if (gnc-commodity-is-currency curr)
		   (balance 'add curr val)
		   (balance 'add domestic 
			    (gnc:gnc-monetary-amount 
			     (exchange-fn (gnc:make-gnc-monetary curr val) 
					  domestic))))))
	 #f)
	balance)
      #f))

;; Returns the number of commodities in a commodity-collector.
;; (If this were implemented as a record, I would be able to
;; just (length ...) the alist, but....)
(define (gnc-commodity-collector-commodity-count collector)
    (let ((commodities 0))
	(gnc-commodity-collector-map
	    collector
		(lambda (comm amt)
		  (set! commodities (+ commodities 1))))
	commodities
    ))

(define (gnc:uniform-commodity? amt report-commodity)
  ;; function to see if the commodity-collector amt
  ;; contains any foreign commodities
  (let ((elts (gnc-commodity-collector-commodity-count amt))
	)
    (or (equal? elts 0)
	(and (equal? elts 1)
	     (gnc-commodity-collector-contains-commodity?
	      amt report-commodity)
	     )
	)
    )
  )

