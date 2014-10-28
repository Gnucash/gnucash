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

(define-module (gnucash report standard-reports advanced-portfolio))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))

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
(define optname-brokerage-fees (N_ "How to report brokerage fees"))

;; To avoid overflows in our calculations, define a denominator for prices and unit values
(define price-denom 100000000)
(define units-denom 100000000)

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
     options gnc:pagename-general (N_ "Report's currency") "c")

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-price-source
      "d" (N_ "The source of price information.") 'pricedb-nearest
      (list (vector 'pricedb-latest 
		    (N_ "Most recent")
		    (N_ "The most recent recorded price."))
	    (vector 'pricedb-nearest
		    (N_ "Nearest in time")
		    (N_ "The price recorded nearest in time to the report date."))
	    )))
    
    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-basis-method
      "e" (N_ "Basis calculation method.") 'average-basis
      (list (vector 'average-basis
		    (N_ "Average")
		    (N_ "Use average cost of all shares for basis."))
	    (vector 'fifo-basis
		    (N_ "FIFO")
		    (N_ "Use first-in first-out method for basis."))
	    (vector 'filo-basis
		    (N_ "LIFO")
		    (N_ "Use last-in first-out method for basis."))
	    )))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-prefer-pricelist "f" 
      (N_ "Prefer use of price editor pricing over transactions, where applicable.")
      #t))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-brokerage-fees
      "g" (N_ "How to report commissions and other brokerage fees.") 'include-in-basis
      (list (vector 'include-in-basis
                    (N_ "Include in basis")
                    (N_ "Include brokerage fees in the basis for the asset."))
            (vector 'include-in-gain
                    (N_ "Include in gain")
                    (N_  "Include brokerage fees in the gain and loss but not in the basis."))
            (vector 'ignore-brokerage
                    (N_ "Ignore")
                    (N_ "Ignore brokerage fees entirely."))
            )))
      
    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-symbol "a"
	(N_ "Display the ticker symbols.")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-listing "b"
	(N_ "Display exchange listings.")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-shares "c"
	(N_ "Display numbers of shares in accounts.")
	#t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-shares-digits
      "d" (N_ "The number of decimal places to use for share numbers.") 2
      0 6 0 1))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-price "e"
	(N_ "Display share prices.")
	#t))

    ;; Account tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "b"
      (N_ "Stock Accounts to report on.")
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
       (warn-no-price #f)
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
  (define (sum-basis b-list currency-frac)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (gnc-numeric-mul (caar b-list) (cdar b-list) currency-frac GNC-RND-ROUND)
			 (sum-basis (cdr b-list) currency-frac) currency-frac GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )
  
  ;; sum up the total number of units in the b-list built by basis-builder below
  (define (units-basis b-list)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (caar b-list) (units-basis (cdr b-list)) 
			 units-denom GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )

  ;; apply a ratio to an existing basis-list, useful for splits/mergers and spinoffs
  ;; I need to get a brain and use (map) for this.
  (define (apply-basis-ratio b-list units-ratio value-ratio)
    (if (not (eqv? b-list '()))
	(cons (cons (gnc-numeric-mul units-ratio (caar b-list) units-denom GNC-RND-ROUND)
		    (gnc-numeric-mul value-ratio (cdar b-list) price-denom GNC-RND-ROUND))
	      (apply-basis-ratio (cdr b-list) units-ratio value-ratio))
	'()
	)    
    )
  
  ;; this builds a list for basis calculation and handles average, fifo and lifo methods
  ;; the list is cons cells of (units-of-stock . price-per-unit)... average method produces only one
  ;; cell that mutates to the new average. Need to add a date checker so that we allow for prices
  ;; coming in out of order, such as a transfer with a price adjusted to carryover the basis.
  (define (basis-builder b-list b-units b-value b-method currency-frac)
    (gnc:debug "actually in basis-builder")
    (gnc:debug "b-list is " b-list " b-units is " (gnc-numeric-to-string b-units) 
               " b-value is " (gnc-numeric-to-string b-value) " b-method is " b-method)

    ;; if there is no b-value, then this is a split/merger and needs special handling
    (cond 

     ;; we have value and positive units, add units to basis
     ((and (not (gnc-numeric-zero-p b-value))
	   (gnc-numeric-positive-p b-units))
      (case b-method
	((average-basis) 
	 (if (not (eqv? b-list '()))
	     (list (cons (gnc-numeric-add b-units
					  (caar b-list) units-denom GNC-RND-ROUND) 
			 (gnc-numeric-div
			  (gnc-numeric-add b-value
					   (gnc-numeric-mul (caar b-list)
							    (cdar b-list) 
							    GNC-DENOM-AUTO GNC-DENOM-REDUCE)
					   GNC-DENOM-AUTO GNC-DENOM-REDUCE)
			  (gnc-numeric-add b-units
					   (caar b-list) GNC-DENOM-AUTO GNC-DENOM-REDUCE)
			  price-denom GNC-RND-ROUND)))
	     (append b-list 
		     (list (cons b-units (gnc-numeric-div
					  b-value b-units price-denom GNC-RND-ROUND))))))
	(else (append b-list 
		      (list (cons b-units (gnc-numeric-div
					   b-value b-units price-denom GNC-RND-ROUND)))))))

     ;; we have value and negative units, remove units from basis
     ((and (not (gnc-numeric-zero-p b-value))
	   (gnc-numeric-negative-p b-units))
      (if (not (eqv? b-list '()))
          (case b-method
            ((fifo-basis) 
             (case (gnc-numeric-compare (gnc-numeric-abs b-units) (caar b-list))
               ((-1)
                 ;; Sold less than the first lot, create a new first lot from the remainder
                 (let ((new-units (gnc-numeric-add b-units (caar b-list) units-denom GNC-RND-ROUND)))
                        (cons (cons new-units (cdar b-list)) (cdr b-list))))
               ((0)
                 ;; Sold all of the first lot
                 (cdr b-list))
               ((1)  
                 ;; Sold more than the first lot, delete it and recurse
                 (basis-builder (cdr b-list) (gnc-numeric-add b-units (caar b-list) units-denom GNC-RND-ROUND)
                                b-value  ;; Only the sign of b-value matters since the new b-units is negative
                                b-method currency-frac))))
            ((filo-basis) 
             (let ((rev-b-list (reverse b-list)))
               (case (gnc-numeric-compare (gnc-numeric-abs b-units) (caar rev-b-list))
                 ((-1)
                   ;; Sold less than the last lot
                 (let ((new-units (gnc-numeric-add b-units (caar rev-b-list) units-denom GNC-RND-ROUND)))
                        (reverse (cons (cons new-units (cdar rev-b-list)) (cdr rev-b-list)))))
                 ((0)
                   ;; Sold all of the last lot
                   (reverse (cdr rev-b-list))
                 )
                 ((1)
                   ;; Sold more than the last lot
                   (basis-builder (reverse (cdr rev-b-list)) (gnc-numeric-add b-units (caar rev-b-list) units-denom GNC-RND-ROUND)
                                           b-value b-method currency-frac)
                 ))))
            ((average-basis) 
             (list (cons (gnc-numeric-add
                          (caar b-list) b-units units-denom GNC-RND-ROUND) 
                         (cdar b-list)))))
          '()
          ))
	
     ;; no value, just units, this is a split/merge...
     ((and (gnc-numeric-zero-p b-value)
	   (not (gnc-numeric-zero-p b-units)))
	(let* ((current-units (units-basis b-list))
	       (units-ratio (gnc-numeric-div (gnc-numeric-add b-units current-units GNC-DENOM-AUTO GNC-DENOM-REDUCE) 
					     current-units GNC-DENOM-AUTO GNC-DENOM-REDUCE))
               ;; If the units ratio is zero the stock is worthless and the value should be zero too 
	       (value-ratio (if (gnc-numeric-zero-p units-ratio)
	                        (gnc-numeric-zero)
                                (gnc-numeric-div (gnc:make-gnc-numeric 1 1) units-ratio GNC-DENOM-AUTO GNC-DENOM-REDUCE))))
	  
	  (gnc:debug "blist is " b-list " current units is " 
	             (gnc-numeric-to-string current-units) 
	             " value ratio is " (gnc-numeric-to-string value-ratio)
	             " units ratio is " (gnc-numeric-to-string units-ratio))
	  (apply-basis-ratio b-list units-ratio value-ratio) 
	  ))

	;; If there are no units, just a value, then its a spin-off,
	;; calculate a ratio for the values, but leave the units alone
	;; with a ratio of 1
     ((and (gnc-numeric-zero-p b-units)
	   (not (gnc-numeric-zero-p b-value)))
      (let* ((current-value (sum-basis b-list GNC-DENOM-AUTO))
	     (value-ratio (gnc-numeric-div (gnc-numeric-add b-value current-value GNC-DENOM-AUTO GNC-DENOM-REDUCE) 
					   current-value GNC-DENOM-AUTO GNC-DENOM-REDUCE)))
	  
	(gnc:debug "this is a spinoff")
	(gnc:debug "blist is " b-list " value ratio is " (gnc-numeric-to-string value-ratio))
	(apply-basis-ratio b-list (gnc:make-gnc-numeric 1 1) value-ratio))
      )

     ;; when all else fails, just send the b-list back
     (else
      b-list)
     )
    )

  ;; Given a price list and a currency find the price for that currency on the list.
  ;; If there is none for the requested currency, return the first one.
  ;; The price list is released but the price returned is ref counted.
  (define (find-price price-list currency)
    (if (eqv? price-list '()) #f
      (let ((price (car price-list)))
        (for-each
          (lambda (p)
            (if (gnc-commodity-equiv currency (gnc-price-get-currency p))
                  (set! price p)))
          price-list)
        (gnc-price-ref price)
        (gnc-price-list-destroy price-list)
        price)))
        
  ;; Return true if either account is the parent of the other or they are siblings
  (define (parent-or-sibling? a1 a2)
    (let ((a2parent (gnc-account-get-parent a2))
          (a1parent (gnc-account-get-parent a1)))
          (or (same-account? a2parent a1)
              (same-account? a1parent a2) 
              (same-account? a1parent a2parent))))
              
  ;; Test whether the given split is the source of a spin off transaction
  ;; This will be a no-units split with only one other split.
  ;; xaccSplitGetOtherSplit only returns on a two-split txn.  It's not a spinoff
  ;; is the other split is in an income or expense account.
  (define (spin-off? split current)
     (let ((other-split (xaccSplitGetOtherSplit split)))
          (and (gnc-numeric-zero-p (xaccSplitGetAmount split))
               (same-account? current (xaccSplitGetAccount split))
               (not (null? other-split))
               (not (split-account-type? other-split ACCT-TYPE-EXPENSE))
               (not (split-account-type? other-split ACCT-TYPE-INCOME)))))
  
  
(define (table-add-stock-rows table accounts to-date
                                currency price-fn exchange-fn price-source
				include-empty show-symbol show-listing show-shares show-price
                                basis-method prefer-pricelist handle-brokerage-fees 
                                total-basis total-value
                                total-moneyin total-moneyout total-income total-gain 
                                total-ugain total-brokerage)

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
                 (brokeragecoll (gnc:make-commodity-collector))
                 (dividendcoll  (gnc:make-commodity-collector))
                 (moneyincoll   (gnc:make-commodity-collector))
                 (moneyoutcoll  (gnc:make-commodity-collector))
                 (gaincoll      (gnc:make-commodity-collector))


		 ;; the price of the commodity at the time of the report
                 (price (price-fn commodity currency to-date))
		 ;; the value of the commodity, expressed in terms of
		 ;; the report's currency.
                 (value (gnc:make-gnc-monetary currency (gnc-numeric-zero)))  ;; Set later
                 (currency-frac (gnc-commodity-get-fraction currency))

		 (pricing-txn #f)
		 (use-txn #f)
		 (basis-list '())
		 ;; setup an alist for the splits we've already seen.
		 (seen_trans '())
		 ;; Account used to hold remainders from income reinvestments and
		 ;; running total of amount moved there
		 (drp-holding-account #f)
		 (drp-holding-amount (gnc-numeric-zero))
		 )

            (define (my-exchange-fn fromunits tocurrency)
              (if (and (gnc-commodity-equiv currency tocurrency)
                       (gnc-commodity-equiv (gnc:gnc-monetary-commodity fromunits) commodity))
                    (gnc:make-gnc-monetary tocurrency
                      (gnc-numeric-mul (gnc:gnc-monetary-amount fromunits)
                                       (if use-txn
                                           (gnc:gnc-monetary-amount price)
                                           (gnc-price-get-value price))
                                       currency-frac GNC-RND-ROUND))
                    (exchange-fn fromunits tocurrency)))
            
            (gnc:debug "Starting account " (xaccAccountGetName current) ", initial price: " 
                   (if price
                     (gnc-commodity-value->string
	 	         (list (gnc-price-get-currency price) (gnc-price-get-value price))) 
	 	     #f))
            
            ;; If we have a price that can't be converted to the report currency
            ;; don't use it
            (if (and price (gnc-numeric-zero-p (gnc:gnc-monetary-amount 
                                       (exchange-fn 
                                          (gnc:make-gnc-monetary 
                                            (gnc-price-get-currency price)
                                            (gnc:make-gnc-numeric 100 1))
                                          currency))))
                (set! price #f))
                  
            ;; If we are told to use a pricing transaction, or if we don't have a price
            ;; from the price DB, find a good transaction to use.
            (if (and (not use-txn)
                     (or (not price) (not prefer-pricelist)))
                  (let ((split-list (reverse (gnc:get-match-commodity-splits-sorted 
                                                 (list current) 
                                                 (case price-source 
                                                   ((pricedb-latest) (gnc:get-today))
                                                   ((pricedb-nearest) to-date)
                                                   (else (gnc:get-today)))  ;; error, but don't crash
                                                 #f))))  ;; Any currency
                        ;; Find the first (most recent) one that can be converted to report currency
                        (while (and (not use-txn) (not (eqv? split-list '())))
                          (let ((split (car split-list)))
                            (if (and (not (gnc-numeric-zero-p (xaccSplitGetAmount split)))
                                     (not (gnc-numeric-zero-p (xaccSplitGetValue split))))
                              (let* ((trans (xaccSplitGetParent split))
                                     (trans-currency (xaccTransGetCurrency trans))
                                     (trans-price (exchange-fn (gnc:make-gnc-monetary
                                                                   trans-currency 
                                                                   (xaccSplitGetSharePrice split))
                                                               currency)))
                                (if (not (gnc-numeric-zero-p (gnc:gnc-monetary-amount trans-price)))
                                  ;; We can exchange the price from this transaction into the report currency
                                  (begin
                                    (if price (gnc-price-unref price))
                                    (set! pricing-txn trans)
                                    (set! price trans-price)
                                    (gnc:debug "Transaction price is " (gnc:monetary->string price))
                                    (set! use-txn #t))
                                  (set! split-list (cdr split-list))))
                              (set! split-list (cdr split-list)))
                            ))))

            ;; If we still don't have a price, use a price of 1 and complain later
            (if (not price)
              (begin
                (set! price (gnc:make-gnc-monetary currency (gnc:make-gnc-numeric 1 1)))
                ;; If use-txn is set, but pricing-txn isn't set, it's a bogus price
                (set! use-txn #t)
                (set! pricing-txn #f)
              )
            )  

            ;; Now that we have a pricing transaction if needed, set the value of the asset
            (set! value (my-exchange-fn (gnc:make-gnc-monetary commodity units) currency))
            (gnc:debug "Value " (gnc:monetary->string value) 
                       " from " (gnc-commodity-numeric->string commodity units))
                      
	    (for-each
	     ;; we're looking at each split we find in the account. these splits
	     ;; could refer to the same transaction, so we have to examine each
	     ;; split, determine what kind of split it is and then act accordingly.
	     (lambda (split)
	       (set! work-done (+ 1 work-done))
	       (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
	       
	       (let* ((parent (xaccSplitGetParent split))
		      (txn-date (gnc-transaction-get-date-posted parent))
		      (commod-currency (xaccTransGetCurrency parent))
		      (commod-currency-frac (gnc-commodity-get-fraction commod-currency)))
		 
		 (if (and (gnc:timepair-le txn-date to-date)
		          (not (assoc-ref seen_trans (gncTransGetGUID parent))))
		     (let ((trans-income (gnc-numeric-zero))
		           (trans-brokerage (gnc-numeric-zero))
		           (trans-shares (gnc-numeric-zero))
		           (shares-bought (gnc-numeric-zero))
		           (trans-sold (gnc-numeric-zero))
		           (trans-bought (gnc-numeric-zero))
		           (trans-spinoff (gnc-numeric-zero))
		           (trans-drp-residual (gnc-numeric-zero))
		           (trans-drp-account #f))

		       (gnc:debug "Transaction " (xaccTransGetDescription parent))
		       ;; Add this transaction to the list of processed transactions so we don't
		       ;; do it again if there is another split in it for this account
		       (set! seen_trans (acons (gncTransGetGUID parent) #t seen_trans))
		       
		       ;; Go through all the splits in the transaction to get an overall idea of 
		       ;; what it does in terms of income, money in or out, shares bought or sold, etc.
		       (for-each
		         (lambda (s)
                           (let ((split-units (xaccSplitGetAmount s))
                                 (split-value (xaccSplitGetValue s)))

                             (gnc:debug "Pass 1: split units " (gnc-numeric-to-string split-units) " split-value " 
                                        (gnc-numeric-to-string split-value) " commod-currency " 
                                        (gnc-commodity-get-printname commod-currency))
                             
                             (cond 
                                ((split-account-type? s ACCT-TYPE-EXPENSE)
                                 ;; Brokerage expense unless a two split transaction with other split
                                 ;; in the stock account in which case it's a stock donation to charity.
                                 (if (not (same-account? current (xaccSplitGetAccount (xaccSplitGetOtherSplit s)))) 
                                   (set! trans-brokerage 
                                         (gnc-numeric-add trans-brokerage split-value commod-currency-frac GNC-RND-ROUND))))
                                   
                                ((split-account-type? s ACCT-TYPE-INCOME)
                                 (set! trans-income (gnc-numeric-sub trans-income split-value
                                                             commod-currency-frac GNC-RND-ROUND)))
                                                  
                                ((same-account? current (xaccSplitGetAccount s))
                                 (set! trans-shares (gnc-numeric-add trans-shares (gnc-numeric-abs split-units)
                                                  units-denom GNC-RND-ROUND))
                                 (if (gnc-numeric-zero-p split-units)
                                     (if (spin-off? s current)
                                         ;; Count money used in a spin off as money out
                                         (if (gnc-numeric-negative-p split-value)
                                             (set! trans-spinoff (gnc-numeric-sub trans-spinoff split-value
                                                                                  commod-currency-frac GNC-RND-ROUND)))
                                         (if (not (gnc-numeric-zero-p split-value))
                                              ;; Gain/loss split (amount zero, value non-zero, and not spinoff).  There will be
                                              ;; a corresponding income split that will incorrectly be added to trans-income
                                              ;; Fix that by subtracting it here
                                              (set! trans-income (gnc-numeric-sub trans-income split-value 
                                                                                  commod-currency-frac GNC-RND-ROUND))))
                                     ;; Non-zero amount, add the value to the sale or purchase total.
                                     (if (gnc-numeric-positive-p split-value)
                                          (begin
                                             (set! trans-bought
                                                  (gnc-numeric-add trans-bought split-value commod-currency-frac GNC-RND-ROUND))
                                             (set! shares-bought
                                                  (gnc-numeric-add shares-bought split-units units-denom GNC-RND-ROUND)))
                                          (set! trans-sold
                                               (gnc-numeric-sub trans-sold split-value commod-currency-frac GNC-RND-ROUND)))))
                                                  
                                ((split-account-type? s ACCT-TYPE-ASSET)
                                 ;; If all the asset accounts mentioned in the transaction are siblings of each other 
                                 ;; keep track of the money transfered to them if it is in the correct currency
                                 (if (not trans-drp-account)
                                     (begin
                                       (set! trans-drp-account (xaccSplitGetAccount s))
                                         (if (gnc-commodity-equiv commod-currency (xaccAccountGetCommodity trans-drp-account))
                                             (set! trans-drp-residual split-value)
                                             (set! trans-drp-account 'none)))
                                     (if (not (eq? trans-drp-account 'none))
                                       (if (parent-or-sibling? trans-drp-account (xaccSplitGetAccount s))
                                           (set! trans-drp-residual (gnc-numeric-add trans-drp-residual split-value
                                                                                     commod-currency-frac GNC-RND-ROUND))
                                           (set! trans-drp-account 'none))))))
		         ))
		         (xaccTransGetSplitList parent)
		       )
		       
		       (gnc:debug "Income: " (gnc-numeric-to-string trans-income)
		                  " Brokerage: " (gnc-numeric-to-string trans-brokerage)
		                  " Shares traded: " (gnc-numeric-to-string trans-shares)
		                  " Shares bought: " (gnc-numeric-to-string shares-bought))
		       (gnc:debug " Value sold: " (gnc-numeric-to-string trans-sold)
		                  " Value purchased: " (gnc-numeric-to-string trans-bought)
		                  " Spinoff value " (gnc-numeric-to-string trans-spinoff)
		                  " Trans DRP residual: " (gnc-numeric-to-string trans-drp-residual))
		                  
		       ;; We need to calculate several things for this transaction:
		       ;; 1. Total income: this is already in trans-income
		       ;; 2. Change in basis: calculated by loop below that looks at every 
		       ;;    that acquires or disposes of shares
		       ;; 3. Realized gain: also calculated below while calculating basis
		       ;; 4. Money in to the account: this is the value of shares bought
		       ;;    except those purchased with reinvested income
		       ;; 5. Money out: the money received by disposing of shares.   This
		       ;;    is in trans-sold plus trans-spinoff
		       ;; 6. Brokerage fees: this is in trans-brokerage
		       
		       ;; Income
		       (dividendcoll 'add commod-currency trans-income)
		       
                       ;; Brokerage fees.  May be either ignored or part of basis, but that
                       ;; will be dealt with elsewhere.
                       (brokeragecoll 'add commod-currency trans-brokerage)
                           
                       ;; Add brokerage fees to trans-bought if not ignoring them and there are any
                       (if (and (not (eq? handle-brokerage-fees 'ignore-brokerage))
                                (gnc-numeric-positive-p trans-brokerage)
                                (gnc-numeric-positive-p trans-shares))
                           (let* ((fee-frac (gnc-numeric-div shares-bought trans-shares GNC-DENOM-AUTO GNC-DENOM-REDUCE))
                                  (fees (gnc-numeric-mul trans-brokerage fee-frac commod-currency-frac GNC-RND-ROUND)))
                                 (set! trans-bought (gnc-numeric-add trans-bought fees commod-currency-frac GNC-RND-ROUND)))) 
                       
                       ;; Update the running total of the money in the DRP residual account.  This is relevant
                       ;; if this is a reinvestment transaction (both income and purchase) and there seems to
                       ;; asset accounts used to hold excess income.
                       (if (and trans-drp-account
                                (not (eq? trans-drp-account 'none))
                                (gnc-numeric-positive-p trans-income)
                                (gnc-numeric-positive-p trans-bought))
                           (if (not drp-holding-account)
                               (begin
                                 (set! drp-holding-account trans-drp-account)
                                 (set! drp-holding-amount trans-drp-residual))
                               (if (and (not (eq? drp-holding-account 'none))
                                        (parent-or-sibling? trans-drp-account drp-holding-account))
                                   (set! drp-holding-amount (gnc-numeric-add drp-holding-amount trans-drp-residual
                                                                              commod-currency-frac GNC-RND-ROUND))
                                   (begin 
                                     ;; Wrong account (or no account), assume there isn't a DRP holding account 
                                     (set! drp-holding-account 'none)
                                     (set trans-drp-residual (gnc-numeric-zero))
                                     (set! drp-holding-amount (gnc-numeric-zero))))))
                                   
                       ;; Set trans-bought to the amount of money moved in to the account which was used to
                       ;; purchase more shares.  If this is not a DRP transaction then all money used to purchase
                       ;; shares is money in.
                       (if (and (gnc-numeric-positive-p trans-income)
                                (gnc-numeric-positive-p trans-bought))
                           (begin
                             (set! trans-bought (gnc-numeric-sub trans-bought trans-income
                                                                 commod-currency-frac GNC-RND-ROUND))
                             (set! trans-bought (gnc-numeric-add trans-bought trans-drp-residual
                                                                 commod-currency-frac GNC-RND-ROUND))
                             (set! trans-bought (gnc-numeric-sub trans-bought drp-holding-amount
                                                                 commod-currency-frac GNC-RND-ROUND))
                             ;; If the DRP holding account balance is negative, adjust it by the amount
                             ;; used in this transaction
                             (if (and (gnc-numeric-negative-p drp-holding-amount)
                                      (gnc-numeric-positive-p trans-bought)) 
                                 (set! drp-holding-amount (gnc-numeric-add drp-holding-amount trans-bought
                                                                           commod-currency-frac GNC-RND-ROUND)))
                             ;; Money in is never more than amount spent to purchase shares
                             (if (gnc-numeric-negative-p trans-bought)
                                 (set! trans-bought (gnc-numeric-zero)))))
                                 
                       (gnc:debug "Adjusted trans-bought " (gnc-numeric-to-string trans-bought)
                                  " DRP holding account " (gnc-numeric-to-string drp-holding-amount))

                       (moneyincoll 'add commod-currency trans-bought)
                       (moneyoutcoll 'add commod-currency trans-sold)
                       (moneyoutcoll 'add commod-currency trans-spinoff)
                           
                       ;; Look at splits again to handle changes in basis and realized gains 
		       (for-each
		         (lambda (s)
                           (let
                              ;; get the split's units and value
                              ((split-units (xaccSplitGetAmount s))
                               (split-value (xaccSplitGetValue s)))

                             (gnc:debug "Pass 2: split units " (gnc-numeric-to-string split-units) " split-value " 
                                        (gnc-numeric-to-string split-value) " commod-currency " 
                                        (gnc-commodity-get-printname commod-currency))
                             
                             (cond 
                               ((and (not (gnc-numeric-zero-p split-units))
                                     (same-account? current (xaccSplitGetAccount s)))
                                ;; Split into subject account with non-zero amount.  This is a purchase
                                ;; or a sale, adjust the basis
				(let* ((split-value-currency (gnc:gnc-monetary-amount 
								(my-exchange-fn (gnc:make-gnc-monetary 
								   commod-currency split-value) currency)))
			               (orig-basis (sum-basis basis-list currency-frac))
			               ;; proportion of the fees attributable to this split
			               (fee-ratio (gnc-numeric-div (gnc-numeric-abs split-units) trans-shares
			                                           GNC-DENOM-AUTO GNC-DENOM-REDUCE))
			               ;; Fees for this split in report currency 
			               (fees-currency (gnc:gnc-monetary-amount (my-exchange-fn 
			                               (gnc:make-gnc-monetary commod-currency
			                                 (gnc-numeric-mul fee-ratio trans-brokerage
			                                                commod-currency-frac GNC-RND-ROUND))
			                                currency)))
			               (split-value-with-fees (if (eq? handle-brokerage-fees 'include-in-basis) 
			                                          ;; Include brokerage fees in basis 
			                                          (gnc-numeric-add split-value-currency fees-currency
			                                                        currency-frac GNC-RND-ROUND)
			                                          split-value-currency)))
                                  (gnc:debug "going in to basis list " basis-list " " (gnc-numeric-to-string split-units) " "
                                             (gnc-numeric-to-string split-value-with-fees))

				  ;; adjust the basis
				  (set! basis-list (basis-builder basis-list split-units split-value-with-fees 
								  basis-method currency-frac))
                                  (gnc:debug  "coming out of basis list " basis-list)
                                  
                                  ;; If it's a sale or the stock is worthless, calculate the gain
                                  (if (not (gnc-numeric-positive-p split-value))
                                       ;; Split value is zero or negative.  If it's zero it's either a stock split/merge
                                       ;; or the stock has become worthless (which looks like a merge where the number
                                       ;; of shares goes to zero).  If the value is negative then it's a disposal of some sort.
                                       (let ((new-basis (sum-basis basis-list currency-frac)))
                                              (if (or (gnc-numeric-zero-p new-basis)
                                                      (gnc-numeric-negative-p split-value))
                                                ;; Split value is negative or new basis is zero (stock is worthless), 
                                                ;; Capital gain is money out minus change in basis
                                                (let ((gain (gnc-numeric-sub (gnc-numeric-abs split-value-with-fees)
                                                                          (gnc-numeric-sub orig-basis new-basis
                                                                                           currency-frac GNC-RND-ROUND)
                                                                          currency-frac GNC-RND-ROUND)))
                                                       (gnc:debug "Old basis=" (gnc-numeric-to-string orig-basis)
                                                                  " New basis=" (gnc-numeric-to-string new-basis)
                                                                  " Gain=" (gnc-numeric-to-string gain))
                                                       (gaincoll 'add currency gain)))))))

                               ;; here is where we handle a spin-off txn. This will be a no-units
                               ;; split with only one other split. xaccSplitGetOtherSplit only
                               ;; returns on a two-split txn.  It's not a spinoff is the other split is
                               ;; in an income or expense account.
                               ((spin-off? s current)
                                  (gnc:debug "before spin-off basis list " basis-list)
                                  (set! basis-list (basis-builder basis-list split-units (gnc:gnc-monetary-amount 
                                                                                          (my-exchange-fn (gnc:make-gnc-monetary 
                                                                                                        commod-currency split-value) 
                                                                                                       currency)) 
                                                                                                       basis-method
                                                                                                       currency-frac))
                                  (gnc:debug "after spin-off basis list "  basis-list))
                             )
		         ))
		         (xaccTransGetSplitList parent)
		       )  
		      )
		   )
		 )
	       )
	     (xaccAccountGetSplitList current)
	     )

	    (gnc:debug "pricing txn is " pricing-txn)
	    (gnc:debug "use txn is " use-txn)
	    (gnc:debug "prefer-pricelist is " prefer-pricelist)
	    (gnc:debug "price is " price)

	    (gnc:debug "basis we're using to build rows is " (gnc-numeric-to-string (sum-basis basis-list 
	                                                            currency-frac)))
	    (gnc:debug "but the actual basis list is " basis-list)

            (if (eq? handle-brokerage-fees 'include-in-gain)
	      (gaincoll 'minusmerge brokeragecoll #f))

	  (if (or include-empty (not (gnc-numeric-zero-p units)))
	    (let* ((moneyin (gnc:sum-collector-commodity moneyincoll currency my-exchange-fn))
		  (moneyout (gnc:sum-collector-commodity moneyoutcoll currency my-exchange-fn))
                  (brokerage (gnc:sum-collector-commodity brokeragecoll currency my-exchange-fn))
		  (income (gnc:sum-collector-commodity dividendcoll currency my-exchange-fn))
		  ;; just so you know, gain == realized gain, ugain == un-realized gain, bothgain, well..
		  (gain (gnc:sum-collector-commodity gaincoll currency my-exchange-fn))
		  (ugain (gnc:make-gnc-monetary currency 
						(gnc-numeric-sub (gnc:gnc-monetary-amount (my-exchange-fn value currency))
								 (sum-basis basis-list (gnc-commodity-get-fraction currency)) 
								 currency-frac GNC-RND-ROUND)))
		  (bothgain (gnc:make-gnc-monetary currency  (gnc-numeric-add (gnc:gnc-monetary-amount gain)
									      (gnc:gnc-monetary-amount ugain)
									      currency-frac GNC-RND-ROUND)))
		  (totalreturn (gnc:make-gnc-monetary currency (gnc-numeric-add (gnc:gnc-monetary-amount bothgain)
										    (gnc:gnc-monetary-amount income)
										currency-frac GNC-RND-ROUND)))

		  (activecols (list (gnc:html-account-anchor current)))
		  )

              ;; If we're using the txn, warn the user
              (if use-txn
                  (if pricing-txn
                      (set! warn-price-dirty #t)
                      (set! warn-no-price #t)
                  ))

	      (total-value 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))
	      (total-moneyin 'merge moneyincoll #f)
	      (total-moneyout 'merge moneyoutcoll #f)
              (total-brokerage 'merge brokeragecoll #f)
	      (total-income 'merge dividendcoll #f)
	      (total-gain 'merge gaincoll #f)
	      (total-ugain 'add (gnc:gnc-monetary-commodity ugain) (gnc:gnc-monetary-amount ugain))
	      (total-basis 'add currency (sum-basis basis-list currency-frac))

	      ;; build a list for the row  based on user selections
	      (if show-symbol (append! activecols (list (gnc:make-html-table-header-cell/markup "text-cell" ticker-symbol))))
	      (if show-listing (append! activecols (list (gnc:make-html-table-header-cell/markup "text-cell" listing))))
	      (if show-shares (append! activecols (list (gnc:make-html-table-header-cell/markup
 	        "number-cell" (xaccPrintAmount units share-print-info)))))
	      (if show-price (append! activecols (list (gnc:make-html-table-header-cell/markup
	        "number-cell"
	        (if use-txn
	            (if pricing-txn
                        (gnc:html-transaction-anchor
                         pricing-txn
                         price
                         )
                         price
                     )    
	 	    (gnc:html-price-anchor
	 	     price
	 	     (gnc:make-gnc-monetary
	  	     (gnc-price-get-currency price)
		     (gnc-price-get-value price)))
		    )))))
 	      (append! activecols (list (if use-txn (if pricing-txn "*" "**") " ")
					(gnc:make-html-table-header-cell/markup 
					 "number-cell" (gnc:make-gnc-monetary currency (sum-basis basis-list
					                         currency-frac)))
					(gnc:make-html-table-header-cell/markup "number-cell" value)
					(gnc:make-html-table-header-cell/markup "number-cell" moneyin)
					(gnc:make-html-table-header-cell/markup "number-cell" moneyout)
					(gnc:make-html-table-header-cell/markup "number-cell" gain)
					(gnc:make-html-table-header-cell/markup "number-cell" ugain)
					(gnc:make-html-table-header-cell/markup "number-cell" bothgain)
					(gnc:make-html-table-header-cell/markup "number-cell"
					    (let* ((moneyinvalue (gnc-numeric-to-double
								  (gnc:gnc-monetary-amount moneyin)))
					           (bothgainvalue (gnc-numeric-to-double
								   (gnc:gnc-monetary-amount bothgain)))
                                             )
					      (if (= 0.0 moneyinvalue)
						  ""
						  (sprintf #f "%.2f%%" (* 100 (/ bothgainvalue moneyinvalue)))))
					)
					(gnc:make-html-table-header-cell/markup "number-cell" income)))
	      (if (not (eq? handle-brokerage-fees 'ignore-brokerage))
		  (append! activecols (list (gnc:make-html-table-header-cell/markup "number-cell" brokerage))))
	      (append! activecols (list (gnc:make-html-table-header-cell/markup "number-cell" totalreturn)
					(gnc:make-html-table-header-cell/markup "number-cell" 
					    (let* ((moneyinvalue (gnc-numeric-to-double
								  (gnc:gnc-monetary-amount moneyin)))
					           (totalreturnvalue (gnc-numeric-to-double
								      (gnc:gnc-monetary-amount totalreturn)))
                                             )
					      (if (= 0.0 moneyinvalue)
						  ""
						  (sprintf #f "%.2f%%" (* 100 (/ totalreturnvalue moneyinvalue))))))
					 )
			)
                       
	      (gnc:html-table-append-row/markup!
	       table
	       row-style
	       activecols)
	        
              (if (and (not use-txn) price) (gnc-price-unref price))
	      (table-add-stock-rows-internal rest (not odd-row?))
	      )
	    (begin
	      (if (and (not use-txn) price) (gnc-price-unref price))
	      (table-add-stock-rows-internal rest odd-row?)
	      )
            )
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
        (currency    (get-option gnc:pagename-general "Report's currency"))
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
	(handle-brokerage-fees (get-option gnc:pagename-general
				  optname-brokerage-fees))

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
                   (lambda (foreign domestic date) 
                    (find-price (gnc-pricedb-lookup-latest-any-currency pricedb foreign)
                                domestic)))
                  ((pricedb-nearest) 
                   (lambda (foreign domestic date) 
                    (find-price (gnc-pricedb-lookup-nearest-in-time-any-currency
		     pricedb foreign (timespecCanonicalDayTime date)) domestic)))))
	       (headercols (list (_ "Account")))
	       (totalscols (list (gnc:make-html-table-cell/markup "total-label-cell" (_ "Total"))))
	       (sum-total-moneyin (gnc-numeric-zero))
	       (sum-total-income (gnc-numeric-zero))
	       (sum-total-both-gains (gnc-numeric-zero))
	       (sum-total-gain (gnc-numeric-zero))
	       (sum-total-ugain (gnc-numeric-zero))
	       (sum-total-brokerage (gnc-numeric-zero))
	       (sum-total-totalreturn (gnc-numeric-zero)))

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
				    (_ "Realized Gain")
				    (_ "Unrealized Gain")
				    (_ "Total Gain")
				    (_ "Rate of Gain")
				    (_ "Income")))

	  (if (not (eq? handle-brokerage-fees 'ignore-brokerage))
	      (append! headercols (list (_ "Brokerage Fees"))))

	  (append! headercols (list (_ "Total Return")
				    (_ "Rate of Return")))

          (append! totalscols (list " "))

          (gnc:html-table-set-col-headers!
           table
	   headercols)
          
          (table-add-stock-rows
           table accounts to-date currency price-fn exchange-fn price-source
           include-empty show-symbol show-listing show-shares show-price basis-method
	   prefer-pricelist handle-brokerage-fees
           total-basis total-value total-moneyin total-moneyout
           total-income total-gain total-ugain total-brokerage)
	  

	  (set! sum-total-moneyin (gnc:sum-collector-commodity total-moneyin currency exchange-fn))
	  (set! sum-total-income (gnc:sum-collector-commodity total-income currency exchange-fn))
	  (set! sum-total-gain (gnc:sum-collector-commodity total-gain currency exchange-fn))
	  (set! sum-total-ugain (gnc:sum-collector-commodity total-ugain currency exchange-fn))
	  (set! sum-total-both-gains (gnc:make-gnc-monetary currency (gnc-numeric-add (gnc:gnc-monetary-amount sum-total-gain)
										      (gnc:gnc-monetary-amount sum-total-ugain)
										      (gnc-commodity-get-fraction currency) GNC-RND-ROUND)))
	  (set! sum-total-brokerage (gnc:sum-collector-commodity total-brokerage currency exchange-fn))
	  (set! sum-total-totalreturn (gnc:make-gnc-monetary currency (gnc-numeric-add (gnc:gnc-monetary-amount sum-total-both-gains)
										           (gnc:gnc-monetary-amount sum-total-income)
										       (gnc-commodity-get-fraction currency) GNC-RND-ROUND)))

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
            (gnc:make-html-table-cell/size
             1 17 (gnc:make-html-text (gnc:html-markup-hr)))))

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
				"total-number-cell" sum-total-gain)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-ugain)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-both-gains)
			       (gnc:make-html-table-cell/markup
				"total-number-cell"
				(let* ((totalinvalue (gnc-numeric-to-double
						      (gnc:gnc-monetary-amount sum-total-moneyin)))
				       (totalgainvalue (gnc-numeric-to-double
							(gnc:gnc-monetary-amount sum-total-both-gains)))
				       )
				  (if (= 0.0 totalinvalue)
				      ""
				      (sprintf #f "%.2f%%" (* 100 (/ totalgainvalue totalinvalue))))))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" sum-total-income)))
	  (if (not (eq? handle-brokerage-fees 'ignore-brokerage))
	      (append! totalscols (list
			       (gnc:make-html-table-cell/markup
                                "total-number-cell" sum-total-brokerage))))
	  (append! totalscols (list
			       (gnc:make-html-table-cell/markup
                                "total-number-cell" sum-total-totalreturn)
			       (gnc:make-html-table-cell/markup
				"total-number-cell" 
				(let* ((totalinvalue (gnc-numeric-to-double
						      (gnc:gnc-monetary-amount sum-total-moneyin)))
				       (totalreturnvalue (gnc-numeric-to-double
						          (gnc:gnc-monetary-amount sum-total-totalreturn)))
				 )
				  (if (= 0.0 totalinvalue) 
				      ""
				      (sprintf #f "%.2f%%" (* 100 (/ totalreturnvalue totalinvalue))))))
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

          (if warn-no-price 
              (gnc:html-document-append-objects! document 
                                                 (list (gnc:make-html-text (if warn-price-dirty (gnc:html-markup-br) "")) 
                                                       (gnc:make-html-text (_ "** this commodity has no price and a price of 1 has been used.")))))
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
