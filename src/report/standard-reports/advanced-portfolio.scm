;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advanced-portfolio.scm
;; by Martijn van Oosterhout (kleptog@svana.org) Feb 2002
;; modified for GnuCash 1.8 by Herbert Thoma (herbie@hthoma.de) Oct 2002
;;
;; Heavily based on portfolio.scm
;; by Robert Merkel (rgmerk@mira.net)
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report advanced-portfolio))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Advanced Portfolio"))

(define optname-price-source (N_ "Price Source"))
(define optname-shares-digits (N_ "Share decimal places"))
(define optname-zero-shares (N_ "Include accounts with no shares"))
(define optname-show-symbol (N_ "Show ticker symbols"))
(define optname-show-listing (N_ "Show listings"))
(define optname-show-price (N_ "Show prices"))
(define optname-show-shares (N_ "Show number of shares"))
(define optname-basis-method (N_ "Basis calculation method"))
(define optname-prefer-pricelist (N_ "Set preference for price list data"))
(define optname-ignore-brokerage-fees (N_ "Ignore brokerage fees when calculating returns"))

(define (options-generator)
  (let* ((options (gnc:new-options)) 
         ;; This is just a helper function for making options.
         ;; See gnucash/src/scm/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; General Tab
    ;; date at which to report balance
    (gnc:options-add-report-date!
     options gnc:pagename-general 
     (N_ "Date") "a")

    (gnc:options-add-currency! 
     options gnc:pagename-general (N_ "Report Currency") "c")

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-price-source
      "d" (N_ "The source of price information") 'pricedb-nearest
      (list (vector 'pricedb-latest 
		    (N_ "Most recent")
		    (N_ "The most recent recorded price"))
	    (vector 'pricedb-nearest
		    (N_ "Nearest in time")
		    (N_ "The price recorded nearest in time to the report date"))
	    (vector 'pricedb-latest-before
		    (N_ "Most recent to report")
		    (N_ "The most recent recorded price before report date"))
	    )))
    
    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-basis-method
      "e" (N_ "Basis calculation method") 'average-basis
      (list (vector 'average-basis
		    (N_ "Average")
		    (N_ "Use average cost of all shares for basis"))
	    (vector 'fifo-basis
		    (N_ "FIFO")
		    (N_ "Use first-in first-out method for basis"))
	    (vector 'filo-basis
		    (N_ "FILO")
		    (N_ "Use first-in last-out method for basis"))
	    )))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-prefer-pricelist "f" 
      (N_ "Prefer use of price editor pricing over transactions, where applicable.")
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-ignore-brokerage-fees "g"
      (N_ "Ignore brokerage fees when calculating returns")
      #t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-symbol "a"
	(N_ "Display the ticker symbols")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-listing "b"
	(N_ "Display exchange listings")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-shares "c"
	(N_ "Display numbers of shares in accounts")
	#t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-shares-digits
      "d" (N_ "The number of decimal places to use for share numbers") 2
      0 6 0 1))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-price "e"
	(N_ "Display share prices")
	#t))

    ;; Account tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "b"
      (N_ "Stock Accounts to report on")
      (lambda () (filter gnc:account-is-stock?
                         (gnc-account-get-descendants-sorted
                          (gnc-get-current-root-account))))
      (lambda (accounts) (list  #t 
                                (filter gnc:account-is-stock? accounts)))
      #t))

    (gnc:register-option 
     options 
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-zero-shares "e" 
      (N_ "Include accounts that have a zero share balances.")
      #f))
    
    (gnc:options-set-default-section options gnc:pagename-general)      
    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.

(define (advanced-portfolio-renderer report-obj)
  
 (let ((work-done 0)
       (work-to-do 0)
       (warn-price-dirty #f))

  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (get-option section name)
    (gnc:option-value (get-op section name)))
  
  (define (split-account-type? split type)
    (eq? type (xaccAccountGetType (xaccSplitGetAccount split))))

  (define (same-split? s1 s2)
    (equal? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

  (define (same-account? a1 a2)
    (equal? (gncAccountGetGUID a1) (gncAccountGetGUID a2)))

  ;; sum up the contents of the b-list built by basis-builder below
  (define (sum-basis b-list)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (gnc-numeric-mul (caar b-list) (cdar b-list) GNC-DENOM-AUTO GNC-RND-ROUND)
			 (sum-basis (cdr b-list)) 100 GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )
  
  ;; sum up the total number of units in the b-list built by basis-builder below
  (define (units-basis b-list)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (caar b-list) (units-basis (cdr b-list)) 
			 100 GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )

  ;; apply a ratio to an existing basis-list, useful for splits/mergers and spinoffs
  ;; I need to get a brain and use (map) for this.
  (define (apply-basis-ratio b-list units-ratio value-ratio)
    (if (not (eqv? b-list '()))
	(cons (cons (gnc-numeric-mul units-ratio (caar b-list) GNC-DENOM-AUTO GNC-RND-ROUND) 
		    (gnc-numeric-mul value-ratio (cdar b-list) GNC-DENOM-AUTO GNC-RND-ROUND)) 
	      (apply-basis-ratio (cdr b-list) units-ratio value-ratio))
	'()
	)    
    )
  
  ;; this builds a list for basis calculation and handles average, fifo and filo methods
  ;; the list is cons cells of (units-of-stock . price-per-unit)... average method produces only one
  ;; cell that mutates to the new average. Need to add a date checker so that we allow for prices
  ;; coming in out of order, such as a transfer with a price adjusted to carryover the basis.
  (define (basis-builder b-list b-units b-value b-method)
    (gnc:debug "actually in basis-builder")
    (gnc:debug "b-list is " b-list " b-units is " b-units " b-value is " b-value " b-method is " b-method)

    ;; if there is no b-value, then this is a split/merger and needs special handling
    (cond 

     ;; we have value and positive units, add units to basis
     ((and (not (gnc-numeric-zero-p b-value))
	   (gnc-numeric-positive-p b-units))
      (case b-method
	((average-basis) 
	 (if (not (eqv? b-list '()))
	     (list (cons (gnc-numeric-add b-units
					  (caar b-list) GNC-DENOM-AUTO GNC-RND-ROUND) 
			 (gnc-numeric-div
			  (gnc-numeric-add b-value
					   (gnc-numeric-mul (caar b-list)
							    (cdar b-list) 
							    GNC-DENOM-AUTO GNC-RND-ROUND)
					   GNC-DENOM-AUTO GNC-RND-ROUND)
			  (gnc-numeric-add b-units
					   (caar b-list) GNC-DENOM-AUTO GNC-RND-ROUND)
			  GNC-DENOM-AUTO GNC-RND-ROUND)))
	     (append b-list 
		     (list (cons b-units (gnc-numeric-div
					  b-value b-units GNC-DENOM-AUTO GNC-RND-ROUND))))))
	(else (append b-list 
		      (list (cons b-units (gnc-numeric-div
					   b-value b-units GNC-DENOM-AUTO GNC-RND-ROUND)))))))

     ;; we have value and negative units, remove units from basis
     ((and (not (gnc-numeric-zero-p b-value))
	   (gnc-numeric-negative-p b-units))
      (if (not (eqv? b-list '()))
	  (case b-method
	    ((fifo-basis) 
	     (if (not (= -1 (gnc-numeric-compare
			     (gnc-numeric-abs b-units) (caar b-list))))
		 (basis-builder (cdr b-list) (gnc-numeric-add
					      b-units 
					      (caar b-list) GNC-DENOM-AUTO GNC-RND-ROUND) 
				b-value b-method)
		 (append (list (cons (gnc-numeric-add
				      b-units 
				      (caar b-list) GNC-DENOM-AUTO GNC-RND-ROUND) 
				     (cdar b-list))) (cdr b-list))))
	    ((filo-basis) 
	     (if (not (= -1 (gnc-numeric-compare
			     (gnc-numeric-abs b-units) (caar (reverse b-list)))))
		 (basis-builder (reverse (cdr (reverse b-list))) 
				(gnc-numeric-add
				 b-units 
				 (caar (reverse b-list)) 
				 GNC-DENOM-AUTO GNC-RND-ROUND) 
				b-value b-method)
		 (append (cdr (reverse b-list)) 
			 (list (cons (gnc-numeric-add
				      b-units 
				      (caar (reverse b-list)) GNC-DENOM-AUTO GNC-RND-ROUND) 
				     (cdar (reverse b-list)))))))
	    ((average-basis) 
	     (list (cons (gnc-numeric-add
			  (caar b-list) b-units GNC-DENOM-AUTO GNC-RND-ROUND) 
			 (cdar b-list)))))
	  '()
	  ))
	
     ;; no value, just units, this is a split/merge...
     ((and (gnc-numeric-zero-p b-value)
	   (not (gnc-numeric-zero-p b-units)))
	(let* ((current-units (units-basis b-list))
	       (units-ratio (gnc-numeric-div (gnc-numeric-add b-units current-units GNC-DENOM-AUTO GNC-RND-ROUND) 
					     current-units GNC-DENOM-AUTO GNC-RND-ROUND))
	       (value-ratio (gnc-numeric-div (gnc:make-gnc-numeric 1 1) units-ratio GNC-DENOM-AUTO GNC-RND-ROUND)))
	  
	  (gnc:debug "blist is " b-list " current units is " current-units " units ratio is " units-ratio)
	  (apply-basis-ratio b-list units-ratio value-ratio) 
	  ))

	;; If there are no units, just a value, then its a spin-off,
	;; calculate a ratio for the values, but leave the units alone
	;; with a ratio of 1
     ((and (gnc-numeric-zero-p b-units)
	   (not (gnc-numeric-zero-p b-value)))
      (let* ((current-value (sum-basis b-list))
	     (value-ratio (gnc-numeric-div (gnc-numeric-add b-value current-value GNC-DENOM-AUTO GNC-RND-ROUND) 
					   current-value GNC-DENOM-AUTO GNC-RND-ROUND)))
	  
	(gnc:debug "this is a spinoff")
	(gnc:debug "blist is " b-list " value ratio is " value-ratio)
	(apply-basis-ratio b-list (gnc:make-gnc-numeric 1 1) value-ratio))
      )

     ;; when all else fails, just send the b-list back
     (else
      b-list)
     )
    )

  
(define (table-add-stock-rows table accounts to-date
                                currency price-fn exchange-fn 
				include-empty show-symbol show-listing show-shares show-price
                                basis-method prefer-pricelist ignore-brokerage-fees
                                total-basis total-value total-moneyin total-moneyout
                                total-income total-gain total-ugain total-brokerage)

   (let ((share-print-info
	  (gnc-share-print-info-places
	   (inexact->exact (get-option gnc:pagename-display
      			       optname-shares-digits)))))
    
    (define (table-add-stock-rows-internal accounts odd-row?)
      (if (null? accounts) total-value
          (let* ((row-style (if odd-row? "normal-row" "alternate-row"))
                 (current (car accounts))
                 (rest (cdr accounts))
		 ;; commodity is the actual stock/thing we are looking at
                 (commodity (xaccAccountGetCommodity current))
                 (ticker-symbol (gnc-commodity-get-mnemonic commodity))
                 (listing (gnc-commodity-get-namespace commodity))
                 (unit-collector (gnc:account-get-comm-balance-at-date
                                  current to-date #f))
                 (units (cadr (unit-collector 'getpair commodity #f)))

                 ;; Counter to keep track of stuff
                 (unitscoll     (gnc:make-commodity-collector))
                 (brokeragecoll (gnc:make-commodity-collector))
                 (dividendcoll  (gnc:make-commodity-collector))
		 (dividend-reincoll (gnc:make-commodity-collector))
                 (moneyincoll   (gnc:make-commodity-collector))
                 (moneyoutcoll  (gnc:make-commodity-collector))
                 (gaincoll      (gnc:make-commodity-collector))


                 (price-list (price-fn commodity to-date))
		 ;; the price of the commodity at the time of the report
                 (price      (if (> (length price-list) 0)
				 (car price-list) #f))
		 ;; if there is no price, set a sane commod-currency
		 ;; for those zero-share accounts. if its a no price
		 ;; account with shares, we'll get a currency later.
		 ;; the currency in which the transaction takes place,
		 ;; for example IBM shares are the commodity, purchsed
		 ;; with US dollars. In this case, commod-currency
		 ;; would be US dollars. If there is no price, we
		 ;; arbitrarily set the commod-currency to the same as
		 ;; that of the report, currency
		 (commod-currency (if price (gnc-price-get-currency price) currency))
		 ;; the value of the commodity, expressed in terms of
		 ;; the report's currency.
                 (value (exchange-fn (gnc:make-gnc-monetary commodity units) currency))

		 (txn-value (gnc-numeric-zero))
		 (txn-date to-date)
		 (pricing-txn #f)
		 (use-txn #f)
		 (basis-list '())
		 (txn-units (gnc-numeric-zero))
		 ;; setup an alist for the splits we've already seen.
		 (seen_split '())
		 )

	    (for-each
	     ;; we're looking at each split we find in the account. these splits
	     ;; could refer to the same transaction, so we have to examine each
	     ;; split, determine what kind of split it is and then act accordingly.
	     (lambda (split)
	       (set! work-done (+ 1 work-done))
	       (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
	       
	       (let* ((parent (xaccSplitGetParent split))
		      (txn-date (gnc-transaction-get-date-posted parent)))
		 
		 ;; we must have a good commod-currency before we go any
		 ;; farther as the rest relies on it. If we don't have a
		 ;; price, then we need to make one from somewhere and
		 ;; grab its commod-currency as well.
		 (if (not price)
		       (for-each
			(lambda (s)
			(if (and (not (or (split-account-type? s ACCT-TYPE-EXPENSE)
					  (split-account-type? s ACCT-TYPE-INCOME)
					  (split-account-type? s ACCT-TYPE-ROOT)))
				 (not (same-account? current (xaccSplitGetAccount s))))
			    (begin
			      ;; we're using a transaction to get the price, so we have to set some stuff
			      (set! commod-currency (xaccAccountGetCommodity (xaccSplitGetAccount s)))
			      ;; FIX-ME this doesn't set a pricing-txn
			      ;; if there is a price list which leads
			      ;; to a swigification crash if the user
			      ;; unchecks "prefer price list" option.
			      (set! pricing-txn (xaccSplitGetParent s))
			      (gnc:debug "pricing txn is " pricing-txn)
			      )
			    )
			) 
		      (xaccTransGetSplitList parent)) 
			  )

		 (if (gnc:timepair-le txn-date to-date)
		     (begin
		       ;; here's where we have problems. we are now going to look at each
		       ;; split of the the parent txn of the current split (above) that we
		       ;; are on. This means we might hit each split more than once as the
		       ;; parent transaction might touch the current account more than once.
		       (for-each
			(lambda (s)

			  ;; have we seen this split?
			  (if (not (assoc-ref seen_split (gncSplitGetGUID s)))

			      (let
				  ;; get the split's units and value
				  ((split-units (xaccSplitGetAmount s))
				   (split-value (xaccSplitGetValue s)))

				;; first add this split to the seen_split list so we only look at it once.
				(set! seen_split (acons (gncSplitGetGUID s) #t seen_split))

				(gnc:debug "split units " split-units " split-value " split-value " commod-currency " commod-currency)
				
				;; now we look at what type of split this is and process accordingly
			  (cond

				 ;; in theory, the only expenses are
				 ;; brokerage fees. Not true, you can
				 ;; have expenses for "donating"
				 ;; shares to a charity, for
				 ;; example. In this case, there will
				 ;; be *only* two
				 ;; splits. xaccSplitGetOtherSplit
				 ;; returns null for a
				 ;; more-than-two-splits txn
				 ((split-account-type? s ACCT-TYPE-EXPENSE)
				  (if (equal? current (xaccSplitGetAccount (xaccSplitGetOtherSplit s)))
				      ;; "donated shares"
				      (moneyoutcoll 'add commod-currency split-value)
				      ;; brokerage fees
				      (brokeragecoll 'add commod-currency split-value)))

				 ;; in theory, income is a dividend of
				 ;; some kind. it could also be
				 ;; gains. that gets handled later. it
				 ;; could also be direct income into
				 ;; shares, say from an employer into
				 ;; a retirement account. basically,
				 ;; there is nothing that can be done
				 ;; with these to differentiate them
				 ;; :(
				 ((split-account-type? s ACCT-TYPE-INCOME)
				  (dividendcoll 
				   'add commod-currency 
				   ;; dig through the txn looking for
				   ;; the stock itself and base the
				   ;; dividend on that. This allows
				   ;; dividends to be split between
				   ;; multiple stocks based on the
				   ;; value of each stock purchased
				   (let* ((txn (xaccSplitGetParent s))
					 (dividend-rein (gnc-numeric-zero))
					 (dividend-income (gnc-numeric-neg (xaccSplitGetValue s)))
					 (adjusted-dividend dividend-income)
					 (split-brokerage (gnc-numeric-zero))
					 (split-ratio (gnc-numeric-zero)))
				     (for-each
				      (lambda (x) 
					(cond 
					 ((and (same-account? current (xaccSplitGetAccount x))
					      (gnc-numeric-positive-p (xaccSplitGetAmount x)))
					  (begin
					    (set! dividend-rein (xaccSplitGetValue x))
					    (dividend-reincoll 'add commod-currency dividend-rein)
					    (gnc:debug "setting the dividend-rein to" (xaccSplitGetValue x))))
					 ;; very special case: we have
					 ;; a split that points to the
					 ;; current account with no
					 ;; shares (amount) but a
					 ;; value == gains/loss split,
					 ;; adjust this back out of
					 ;; dividends because we'll
					 ;; erroneously pick it up
					 ;; later.
					 ((and (same-account? current (xaccSplitGetAccount x))
					       (gnc-numeric-zero-p (xaccSplitGetAmount x))
					       (not (gnc-numeric-zero-p (xaccSplitGetValue x))))
					  (dividendcoll 'add commod-currency (xaccSplitGetValue x)))

					 ((split-account-type? x ACCT-TYPE-EXPENSE)
					  (begin
					    (set! adjusted-dividend (gnc-numeric-sub dividend-income (xaccSplitGetValue x) 
										     GNC-DENOM-AUTO GNC-RND-ROUND))
					    (gnc:debug "setting adjusted-dividend to" dividend-income)
					    ;; grab the brokerage that
					    ;; may be associated so we
					    ;; can split it too
					    (set! split-brokerage (xaccSplitGetValue x))
					    )
					  )
					 )
					)
				      (xaccTransGetSplitList txn))
				     
				     ;; make a ratio out of the reinvest and adjusted dividends
				     (set! split-ratio (gnc-numeric-div dividend-rein 
									adjusted-dividend 
									GNC-DENOM-AUTO GNC-RND-ROUND))

				     ;; take the brokerage back out and apply the ratio
				     (brokeragecoll 'add commod-currency (gnc-numeric-neg split-brokerage))
				     (brokeragecoll 'add commod-currency 
						    (gnc-numeric-mul split-brokerage 
								     split-ratio
								     100 GNC-RND-ROUND))

				     (if (gnc-numeric-zero-p dividend-rein)
					 ;; no reinvested dividend, return just the income split
					 (xaccSplitGetValue s)
					 ;; dividend reinvested so
					 ;; apply the ratio to the
					 ;; dividend and return it for
					 ;; use in the dividend
					 ;; collector
					 (gnc-numeric-mul dividend-income 
							  split-ratio
							  100 GNC-RND-ROUND)
					 )
				     )
				   ))

				 ;; we have units, handle all cases of that
				 ((not (gnc-numeric-zero-p split-units))
				  (begin
				    
				    (gnc:debug "going in to basis list " basis-list split-units split-value)

				    ;; are we dealing with the actual stock/fund?
				    (if (same-account? current (xaccSplitGetAccount s))
					(begin

					  ;; adjust the basis
					  (set! basis-list (basis-builder basis-list split-units (gnc:gnc-monetary-amount 
												  (exchange-fn (gnc:make-gnc-monetary 
														commod-currency split-value) 
													       currency)) basis-method))
					  ;; adjust moneyin/out
					  (if (gnc-numeric-positive-p split-value)
					      ;; but only adjust moneyin if it's not a spinoff
					      (if (or (null? (xaccSplitGetOtherSplit s))
						      (not (gnc-numeric-zero-p (xaccSplitGetAmount (xaccSplitGetOtherSplit s)))))
					       (moneyincoll 'add commod-currency split-value))
					      (moneyoutcoll 'add commod-currency (gnc-numeric-neg split-value)))
					  )
					)
				    (gnc:debug  "coming out of basis list " basis-list)
				    )
				  )

				 ;; here is where we handle a spin-off txn. This will be a no-units
				 ;; transaction with only one other split. xaccSplitGetOtherSplit only
				 ;; returns on a two-split txn :) 
				 ((and (gnc-numeric-zero-p txn-units) (not (null? (xaccSplitGetOtherSplit s))))
				  (if (same-account? current (xaccSplitGetAccount s))
				      (set! basis-list (basis-builder basis-list split-units (gnc:gnc-monetary-amount 
											      (exchange-fn (gnc:make-gnc-monetary 
													    commod-currency split-value) 
													   currency)) basis-method))
				      )
				  )
				 )
				)
			      )
			  )
			(xaccTransGetSplitList parent)
			)
		       )
		     )
		 )
	       )
	     (xaccAccountGetSplitList current)
	     )

	    ;; now we determine which price data to use, the pricelist or the txn
	    ;; and if we have a choice, use whichever is newest.
	    (set! use-txn (if (not price) #t 
			      (if prefer-pricelist #f
				  (if (not (gnc:timepair-le txn-date (gnc-price-get-time price)))
				      #t #f))))
	    (gnc:debug "use txn is " use-txn)
	    (gnc:debug "prefer-pricelist is " prefer-pricelist)
	    (gnc:debug "price is " price)

	    ;; okay we're using the txn, so make a new price, value etc. and warn the user
	    (if use-txn
		(begin
		  (set! price (if (not (gnc-numeric-zero-p txn-units))
				  (gnc:make-gnc-monetary commod-currency
							 (gnc-numeric-div txn-value
									  (gnc-numeric-abs txn-units)
									  100 GNC-RND-ROUND))
				  (gnc:make-gnc-monetary commod-currency (gnc-numeric-zero))))
		  
		  (set! value (if price (gnc:make-gnc-monetary commod-currency 
						     (gnc-numeric-mul units
								      (gnc:gnc-monetary-amount price)
										100 GNC-RND-ROUND))
				  (gnc:make-gnc-monetary commod-currency (gnc-numeric-zero))))
		  (set! warn-price-dirty #t)
		  )  
		)

	    ;; what this means is gain = moneyout - moneyin + basis-of-current-shares, and
	    ;; adjust for brokers and dividends.
	    (gaincoll 'add currency (sum-basis basis-list))
	    (gnc:debug (list "basis we're using to build rows is " (sum-basis basis-list)))
	    (gnc:debug (list "but the actual basis list is " basis-list))

	    (gaincoll 'merge moneyoutcoll #f)
	    (gaincoll 'minusmerge moneyincoll #f)

            ;; This removes the already-counted reinvested dividends from moneyin.
	    (moneyincoll 'minusmerge dividend-reincoll #f)

            (if (not ignore-brokerage-fees)
	      (moneyincoll 'merge brokeragecoll #f))

	  (if (or include-empty (not (gnc-numeric-zero-p units)))
	    (let* ((moneyin (gnc:sum-collector-commodity moneyincoll currency exchange-fn))
		  (moneyout (gnc:sum-collector-commodity moneyoutcoll currency exchange-fn))
                  (brokerage (gnc:sum-collector-commodity brokeragecoll currency exchange-fn))
		  (income (gnc:sum-collector-commodity dividendcoll currency exchange-fn))
		  ;; just so you know, gain == realized gain, ugain == un-realized gain, bothgain, well..
		  (gain (gnc:sum-collector-commodity gaincoll currency exchange-fn))
		  (ugain (gnc:make-gnc-monetary currency 
						(gnc-numeric-sub (gnc:gnc-monetary-amount (exchange-fn value currency))
								 (sum-basis basis-list) 
								 100 GNC-RND-ROUND)))
		  (bothgain (gnc:make-gnc-monetary currency  (gnc-numeric-add (gnc:gnc-monetary-amount gain)
									      (gnc:gnc-monetary-amount ugain)
									      100 GNC-RND-ROUND)))

		  (activecols (list (gnc:html-account-anchor current)))
		  )

	      (total-value 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))
	      (total-moneyin 'merge moneyincoll #f)
	      (total-moneyout 'merge moneyoutcoll #f)
              (total-brokerage 'merge brokeragecoll #f)
	      (total-income 'merge dividendcoll #f)
	      (total-gain 'merge gaincoll #f)
	      (total-ugain 'add (gnc:gnc-monetary-commodity ugain) (gnc:gnc-monetary-amount ugain))
	      (total-basis 'add currency (sum-basis basis-list))

	      ;; build a list for the row  based on user selections
	      (if show-symbol (append! activecols (list (gnc:make-html-table-header-cell/markup "text-cell" ticker-symbol))))
	      (if show-listing (append! activecols (list (gnc:make-html-table-header-cell/markup "text-cell" listing))))
	      (if show-shares (append! activecols (list (gnc:make-html-table-header-cell/markup
 	        "number-cell" (xaccPrintAmount units share-print-info)))))
	      (if show-price (append! activecols (list (gnc:make-html-table-header-cell/markup
	        "number-cell"
	        (if use-txn
		    (gnc:html-transaction-anchor
		     pricing-txn
		     price
		     )
	 	    (gnc:html-price-anchor
	 	     price
	 	     (gnc:make-gnc-monetary
	  	     (gnc-price-get-currency price)
		     (gnc-price-get-value price)))
		    )))))
 	      (append! activecols (list (if use-txn "*" " ")
					(gnc:make-html-table-header-cell/markup 
					 "number-cell" (gnc:make-gnc-monetary currency (sum-basis basis-list)))
					(gnc:make-html-table-header-cell/markup "number-cell" value)
					(gnc:make-html-table-header-cell/markup "number-cell" moneyin)
					(gnc:make-html-table-header-cell/markup "number-cell" moneyout)
					(gnc:make-html-table-header-cell/markup "number-cell" income)
					(gnc:make-html-table-header-cell/markup "number-cell" gain)
					(gnc:make-html-table-header-cell/markup "number-cell" ugain)
					(gnc:make-html-table-header-cell/markup "number-cell" bothgain)
					(gnc:make-html-table-header-cell/markup "number-cell" 
					    (let* ((moneyinvalue (gnc-numeric-to-double
								  (gnc:gnc-monetary-amount moneyin)))
					           (bothgainvalue (+ (gnc-numeric-to-double
								      (gnc:gnc-monetary-amount income))
								   (- (gnc-numeric-to-double
								       (gnc:gnc-monetary-amount bothgain))
								      (if ignore-brokerage-fees
								       0
								       (gnc-numeric-to-double
								        (gnc:gnc-monetary-amount brokerage))))))
                                             )
					      (if (= 0.0 moneyinvalue)
						  (sprintf #f "%.2f%%" moneyinvalue)
						  (sprintf #f "%.2f%%" (* 100 (/ bothgainvalue moneyinvalue))))))
                                        (gnc:make-html-table-header-cell/markup "number-cell" brokerage)
					 )
			)
                       
	      (gnc:html-table-append-row/markup!
	       table
	       row-style
	       activecols)
	        
	      (table-add-stock-rows-internal rest (not odd-row?))
	      )
	    (table-add-stock-rows-internal rest odd-row?)
            )
            (gnc-price-list-destroy price-list)
	    )))

    (set! work-to-do (gnc:accounts-count-splits accounts))
    (table-add-stock-rows-internal accounts #t)))
  
  ;; Tell the user that we're starting.
  (gnc:report-starting reportname)

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((to-date     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general "Date")))
        (accounts    (get-option gnc:pagename-accounts "Accounts"))
        (currency    (get-option gnc:pagename-general "Report Currency"))
        (price-source (get-option gnc:pagename-general
                                  optname-price-source))
        (report-title (get-option gnc:pagename-general 
                                  gnc:optname-reportname))
        (include-empty (get-option gnc:pagename-accounts
                                  optname-zero-shares))
	(show-symbol (get-option gnc:pagename-display
				  optname-show-symbol))
	(show-listing (get-option gnc:pagename-display
				  optname-show-listing))
	(show-shares (get-option gnc:pagename-display
				  optname-show-shares))
	(show-price (get-option gnc:pagename-display
				  optname-show-price))
	(basis-method (get-option gnc:pagename-general
				  optname-basis-method))
	(prefer-pricelist (get-option gnc:pagename-general
				      optname-prefer-pricelist))
	(ignore-brokerage-fees (get-option gnc:pagename-general
				  optname-ignore-brokerage-fees))

	(total-basis (gnc:make-commodity-collector))
        (total-value    (gnc:make-commodity-collector))
        (total-moneyin  (gnc:make-commodity-collector))
        (total-moneyout (gnc:make-commodity-collector))
        (total-income   (gnc:make-commodity-collector))
        (total-gain     (gnc:make-commodity-collector)) ;; realized gain
	(total-ugain (gnc:make-commodity-collector))    ;; unrealized gain
        (total-brokerage (gnc:make-commodity-collector))
	;;document will be the HTML document that we return.
        (table (gnc:make-html-table))
        (document (gnc:make-html-document)))

    (gnc:html-document-set-title!
     document (string-append 
               report-title
               (sprintf #f " %s" (gnc-print-date to-date))))

    (if (not (null? accounts))
        ; at least 1 account selected
        (let* ((exchange-fn (gnc:case-exchange-fn price-source currency to-date))
               (pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
               (price-fn
                (case price-source
                  ((pricedb-latest) 
                   (lambda (foreign date) 
                    (gnc-pricedb-lookup-latest-any-currency pricedb foreign)))
                  ((pricedb-nearest) 
                   (lambda (foreign date) 
                    (gnc-pricedb-lookup-nearest-in-time-any-currency
		     pricedb foreign (timespecCanonicalDayTime date))))
		  ((pricedb-latest-before)
		   (lambda (foreign date)
		     (gnc-pricedb-lookup-latest-before-any-currency
		      pricedb foreign (timespecCanonicalDayTime date))))))
	       (headercols (list (_ "Account")))
	       (totalscols (list (gnc:make-html-table-cell/markup "total-label-cell" (_ "Total"))))
	       (sum-total-moneyin (gnc-numeric-zero))
	       (sum-total-income (gnc-numeric-zero))
	       (sum-total-both-gains (gnc-numeric-zero))
	       (sum-total-gain (gnc-numeric-zero))
	       (sum-total-ugain (gnc-numeric-zero))
	       (sum-total-brokerage (gnc-numeric-zero)))

	  ;;begin building lists for which columns to display
          (if show-symbol 
	      (begin (append! headercols (list (_ "Symbol")))
		     (append! totalscols (list " "))))

	  (if show-listing 
	      (begin (append! headercols (list (_ "Listing")))
		     (append! totalscols (list " "))))

	  (if show-shares 
	      (begin (append! headercols (list (_ "Shares")))
		     (append! totalscols (list " "))))

	  (if show-price 
	      (begin (append! headercols (list (_ "Price")))
		     (append! totalscols (list " "))))

	  (append! headercols (list " "
				    (_ "Basis")
				    (_ "Value")
				    (_ "Money In")
				    (_ "Money Out")
				    (_ "Income")
				    (_ "Realized Gain")
				    (_ "Unrealized Gain")
				    (_ "Total Gain")
				    (_ "Total Return")
                                    (_ "Brokerage Fees")))

          (append! totalscols (list " "))

          (gnc:html-table-set-col-headers!
           table
	   headercols)
          
          (table-add-stock-rows
           table accounts to-date currency price-fn exchange-fn
           include-empty show-symbol show-listing show-shares show-price
	   basis-method prefer-pricelist ignore-brokerage-fees
           total-basis total-value total-moneyin total-moneyout
           total-income total-gain total-ugain total-brokerage)
	  

	  (set! sum-total-moneyin (gnc:sum-collector-commodity total-moneyin currency exchange-fn))
	  (set! sum-total-income (gnc:sum-collector-commodity total-income currency exchange-fn))
	  (set! sum-total-gain (gnc:sum-collector-commodity total-gain currency exchange-fn))
	  (set! sum-total-ugain (gnc:sum-collector-commodity total-ugain currency exchange-fn))
	  (set! sum-total-both-gains (gnc:make-gnc-monetary currency (gnc-numeric-add (gnc:gnc-monetary-amount sum-total-gain)
										      (gnc:gnc-monetary-amount sum-total-ugain)
										      100 GNC-RND-ROUND)))
	  (set! sum-total-brokerage (gnc:sum-collector-commodity total-brokerage currency exchange-fn))

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
            (gnc:make-html-table-cell/size
             1 16 (gnc:make-html-text (gnc:html-markup-hr)))))

	  ;; finish building the totals columns, now that totals are complete
	  (append! totalscols (list
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:sum-collector-commodity total-basis currency exchange-fn))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:sum-collector-commodity total-value currency exchange-fn))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-moneyin)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:sum-collector-commodity total-moneyout currency exchange-fn))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-income)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-gain)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-ugain)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-both-gains)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" 
				(let* ((totalinvalue (gnc-numeric-to-double
						      (gnc:gnc-monetary-amount sum-total-moneyin)))
				       (totalgainvalue (+ (gnc-numeric-to-double
							   (gnc:gnc-monetary-amount sum-total-income))
							(- (gnc-numeric-to-double
						            (gnc:gnc-monetary-amount sum-total-both-gains))
							   (if ignore-brokerage-fees
							    0
							    (gnc-numeric-to-double
							     (gnc:gnc-monetary-amount sum-total-brokerage))))))
				 )

				  (if (= 0.0 totalinvalue) 
				      (sprintf #f "%.2f%%" totalinvalue) 
				      (sprintf #f "%.2f%%" (* 100 (/ totalgainvalue totalinvalue))))))
                             (gnc:make-html-table-cell/markup
                              "total-number-cell" sum-total-brokerage)
			       ))
	  

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           totalscols
            )

          (gnc:html-document-add-object! document table)
          (if warn-price-dirty 
              (gnc:html-document-append-objects! document 
                                                 (list (gnc:make-html-text (_ "* this commodity data was built using transaction pricing instead of the price list."))
						       (gnc:make-html-text (gnc:html-markup-br))
						       (gnc:make-html-text (_ "If you are in a multi-currency situation, the exchanges may not be correct.")))))
)

					;if no accounts selected.
        (gnc:html-document-add-object!
         document
	 (gnc:html-make-no-account-warning 
	  report-title (gnc:report-id report-obj))))
    
    (gnc:report-finished)
    document)))

(gnc:define-report
 'version 1
 'report-guid "21d7cfc59fc74f22887596ebde7e462d"
 'name reportname
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer advanced-portfolio-renderer)
