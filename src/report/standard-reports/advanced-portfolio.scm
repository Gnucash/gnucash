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
(define optname-include-gains (N_ "Include gains and losses"))
(define optname-show-symbol (N_ "Show ticker symbols"))
(define optname-show-listing (N_ "Show listings"))
(define optname-show-price (N_ "Show prices"))
(define optname-show-shares (N_ "Show number of shares"))
(define optname-basis-method (N_ "Basis calculation method"))
(define optname-prefer-pricelist (N_ "Set preference for price list data"))

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


    (gnc:register-option 
     options 
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-include-gains "g" 
      (N_ "Include splits with no shares for calculating money-in and money-out")
      #f))

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
    (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

  (define (same-account? a1 a2)
    (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2)))
  
  ;; this builds a list for basis calculation and handles average, fifo and filo methods
  ;; the list is cons cells of (units-of-stock . price-per-unit)... average method produces only one
  ;; cell that mutates to the new average. Need to add a date checker so that we allow for prices
  ;; coming in out of order, such as a transfer with a price adjusted to carryover the basis.
  (define (basis-builder b-list b-units b-value b-method)
    (if (gnc-numeric-positive-p b-units)
	(case b-method
	  ((average-basis) 
           (if (not (eqv? b-list '()))
               (list (cons (gnc-numeric-add b-units
                                            (caar b-list) 10000 GNC-RND-ROUND) 
                           (gnc-numeric-div
                            (gnc-numeric-add b-value
                                             (gnc-numeric-mul (caar b-list)
                                                              (cdar b-list) 
                                                              10000 GNC-RND-ROUND)
                                             10000 GNC-RND-ROUND)
                            (gnc-numeric-add b-units
                                             (caar b-list) 10000 GNC-RND-ROUND)
                            10000 GNC-RND-ROUND)))
               (append b-list 
                       (list (cons b-units (gnc-numeric-div
                                            b-value b-units 10000 
                                            GNC-RND-ROUND))))))
	  (else (append b-list 
                        (list (cons b-units (gnc-numeric-div
                                             b-value b-units 10000 
                                             GNC-RND-ROUND))))))
	(if (not (eqv? b-list '()))
	    (case b-method
	      ((fifo-basis) 
               (if (not (= -1 (gnc-numeric-compare
                               (gnc-numeric-abs b-units) (caar b-list))))
                   (basis-builder (cdr b-list) (gnc-numeric-add
                                                b-units 
                                                (caar b-list) 10000 GNC-RND-ROUND) 
                                  b-value b-method)
                   (append (list (cons (gnc-numeric-add
                                        b-units 
                                        (caar b-list) 10000 GNC-RND-ROUND) 
                                       (cdar b-list))) (cdr b-list))))
	      ((filo-basis) 
               (if (not (= -1 (gnc-numeric-compare
                               (gnc-numeric-abs b-units) (caar (reverse b-list)))))
                   (basis-builder (reverse (cdr (reverse b-list))) 
                                  (gnc-numeric-add
                                   b-units 
                                   (caar (reverse b-list)) 
                                   10000 GNC-RND-ROUND) 
                                  b-value b-method)
                   (append (cdr (reverse b-list)) 
                           (list (cons (gnc-numeric-add
                                        b-units 
                                        (caar (reverse b-list)) 10000 GNC-RND-ROUND) 
                                       (cdar (reverse b-list)))))))
	      ((average-basis) 
               (list (cons (gnc-numeric-add
                            (caar b-list) b-units 10000 GNC-RND-ROUND) 
                           (cdar b-list)))))
	    '()
	    )
	)
    )

  ;; sum up the contents of the b-list built by basis-builder above
  (define (sum-basis b-list)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (gnc-numeric-mul (caar b-list) (cdar b-list) 100 GNC-RND-ROUND)
			 (sum-basis (cdr b-list)) 100 GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )
  
  ;; sum up the total number of units in the b-list built by basis-builder above
  (define (units-basis b-list)
    (if (not (eqv? b-list '()))
	(gnc-numeric-add (caar b-list) (units-basis (cdr b-list)) 100 GNC-RND-ROUND)
	(gnc-numeric-zero)
	)
    )

  
(define (table-add-stock-rows table accounts to-date
                                currency price-fn exchange-fn 
				include-empty include-gains show-symbol show-listing show-shares show-price
                                basis-method prefer-pricelist total-basis total-value total-moneyin total-moneyout
                                total-gain total-ugain)

   (let ((share-print-info
	  (gnc-share-print-info-places
	   (inexact->exact (get-option gnc:pagename-display
      			       optname-shares-digits)))))
    
    (define (table-add-stock-rows-internal accounts odd-row?)
      (if (null? accounts) total-value
          (let* ((row-style (if odd-row? "normal-row" "alternate-row"))
                 (current (car accounts))
                 (rest (cdr accounts))
                 (name (xaccAccountGetName current))
                 (commodity (xaccAccountGetCommodity current))
                 (ticker-symbol (gnc-commodity-get-mnemonic commodity))
                 (listing (gnc-commodity-get-namespace commodity))
                 (unit-collector (gnc:account-get-comm-balance-at-date
                                  current to-date #f))
                 (units (cadr (unit-collector 'getpair commodity #f)))
;;                 (totalunits 0.0) ;;      these two items do nothing, but are in a debug below, 
 ;;                (totalunityears 0.0);;   so I'm leaving it. asw

                 ;; Counter to keep track of stuff
                 (unitscoll     (gnc:make-commodity-collector))
                 (brokeragecoll (gnc:make-commodity-collector))
                 (dividendcoll  (gnc:make-commodity-collector))
                 (moneyincoll   (gnc:make-commodity-collector))
                 (moneyoutcoll  (gnc:make-commodity-collector))
                 (gaincoll      (gnc:make-commodity-collector))


                 (price-list (price-fn commodity to-date))
                 (price      (if (> (length price-list) 0)
				 (car price-list) #f))
		 ;; if there is no price, set a sane commod-currency for those zero-share 
		 ;; accounts. if its a no price account with shares, we'll get a currency later.
		 (commod-currency (if price (gnc-price-get-currency price) currency))
                 (value (exchange-fn (gnc:make-gnc-monetary commodity units) currency))

		 (txn-value (gnc-numeric-zero))
		 (txn-date to-date)
		 (pricing-txn #f)
		 (use-txn #f)
		 (basis-list '())
		 (txn-units (gnc-numeric-zero))
		 )


;;          (gnc:debug "---" name "---")
	    (for-each
	     (lambda (split)
	       (set! work-done (+ 1 work-done))
	       (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
	       (let ((parent (xaccSplitGetParent split)))
		 (if (gnc:timepair-le (gnc-transaction-get-date-posted parent) to-date)
		     (begin
		       (for-each
			(lambda (s)
			  ;; If this is an asset type account for buy or sell, then grab a 
			  ;; currency and a txn-value for later computation
			  (cond
			   ((and (not (same-account? current (xaccSplitGetAccount s)))
				 (not (or (split-account-type?
                                           s ACCT-TYPE-EXPENSE)
					  (split-account-type?
                                           s ACCT-TYPE-INCOME))))

			    ;;only change the commod-currency if price failed
			    (if (not price) (set! commod-currency (xaccAccountGetCommodity (xaccSplitGetAccount s))))
			    (set! txn-value (gnc-numeric-abs (xaccSplitGetValue s)));;FIXME use xaccSplitGetSharePrice
			    (set! txn-date (gnc-transaction-get-date-posted parent))
			    (set! pricing-txn parent)
			    )
			   ((same-account? current (xaccSplitGetAccount s))
			    (set! txn-units (xaccSplitGetAmount s)))
			    
			      )
			  )

			(xaccTransGetSplitList parent))


		       ;; go build the basis-list
		       ;; the use of exchange-fn here is an attempt to get the basis list into one
		       ;; currency to help accomodate stock transfers and other things. might not work.
		       (set! basis-list (basis-builder basis-list txn-units (gnc:gnc-monetary-amount
									     (exchange-fn (gnc:make-gnc-monetary 
											   commod-currency txn-value) 
											  currency)) basis-method))

		       (for-each
			(lambda (s)
			  (cond
			   ((same-split? s split) 
;;                       (gnc:debug "amount " (gnc-numeric-to-double (xaccSplitGetAmount s))
;;                                  " acct " (xaccAccountGetName (xaccSplitGetAccount s)) )
;;                       (gnc:debug "value " (gnc-numeric-to-double (xaccSplitGetValue s))
;;                                  " in " (gnc-commodity-get-printname commod-currency)
;;                                  " from " (xaccTransGetDescription (xaccSplitGetParent s)))
			    (cond
			     ((or include-gains (not (gnc-numeric-zero-p (xaccSplitGetAmount s))))
			      (unitscoll 'add commodity (xaccSplitGetAmount s)) ;; Is the stock transaction?
;; these lines do nothing, but are in a debug so I'm leaving it, just in case. asw.			     
;;			      (if (< 0 (gnc-numeric-to-double
;;					(xaccSplitGetAmount s)))


;;				  (set! totalunits
;;					(+ totalunits
;;					   (gnc-numeric-to-double (xaccSplitGetAmount s))))
;;				  )


;;			      (set! totalunityears
;;				    (+ totalunityears 
;;				       (* (gnc-numeric-to-double (xaccSplitGetAmount s))
;;					  (gnc:date-year-delta 
;;					   (car (gnc-transaction-get-date-posted parent))
;;					   (current-time))))) 
			      (cond 
			       ((gnc-numeric-negative-p (xaccSplitGetValue s))
				(moneyoutcoll
				 'add commod-currency
				 (gnc-numeric-neg (xaccSplitGetValue s))))
			       (else (moneyincoll 
				      'add commod-currency
				      (gnc-numeric-neg (xaccSplitGetValue s))))))))
			 
			   ((split-account-type? s ACCT-TYPE-EXPENSE)
			     (brokeragecoll 'add commod-currency (xaccSplitGetValue s)))
			   
			   ((split-account-type? s ACCT-TYPE-INCOME)
			     (dividendcoll 'add commod-currency (xaccSplitGetValue s)))
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
;;          (gnc:debug "totalunits" totalunits)
;;          (gnc:debug "totalunityears" totalunityears)

	    ;; now we determine which price data to use, the pricelist or the txn
	    ;; and if we have a choice, use whichever is newest.
	    (set! use-txn (if (not price) #t 
			      (if prefer-pricelist #f
				  (if (not (gnc:timepair-le txn-date (gnc-price-get-time price)))
				      #t #f))))

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
            (moneyincoll 'minusmerge dividendcoll #f)
	    (moneyoutcoll 'minusmerge brokeragecoll #f)
	    (gaincoll 'merge moneyoutcoll #f)
	    (gaincoll 'merge moneyincoll #f)



	    
	  (if (or include-empty (not (gnc-numeric-zero-p units)))
	    (let* ((moneyin (gnc:monetary-neg
			    (gnc:sum-collector-commodity moneyincoll currency exchange-fn)))
		  (moneyout (gnc:sum-collector-commodity moneyoutcoll currency exchange-fn))
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
	      (total-gain 'merge gaincoll #f)
	      (total-ugain 'add (gnc:gnc-monetary-commodity ugain) (gnc:gnc-monetary-amount ugain))
	      (total-basis 'add currency (sum-basis basis-list))

	      ;; build a list for the row  based on user selections
	      (if show-symbol (append! activecols (list ticker-symbol)))
	      (if show-listing (append! activecols (list listing)))
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
					(gnc:make-html-table-header-cell/markup "number-cell" gain)
					(gnc:make-html-table-header-cell/markup "number-cell" ugain)
					(gnc:make-html-table-header-cell/markup "number-cell" bothgain)
										
										
					(gnc:make-html-table-header-cell/markup "number-cell" 
					    (let ((moneyinvalue (gnc-numeric-to-double
								 (gnc:gnc-monetary-amount moneyin))))
					      (if (= 0.0 moneyinvalue)
						  (sprintf #f "%.2f%%" moneyinvalue)
						  (sprintf #f "%.2f%%" (* 100 (/ (gnc-numeric-to-double
									     (gnc:gnc-monetary-amount bothgain))
									    moneyinvalue))))))
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
        (include-gains (get-option gnc:pagename-general
                                  optname-include-gains))
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

	(total-basis (gnc:make-commodity-collector))
        (total-value    (gnc:make-commodity-collector))
        (total-moneyin  (gnc:make-commodity-collector))
        (total-moneyout (gnc:make-commodity-collector))
        (total-gain     (gnc:make-commodity-collector)) ;; realized gain
	(total-ugain (gnc:make-commodity-collector))    ;; unrealized gain
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
	       (sum-total-both-gains (gnc-numeric-zero))
	       (sum-total-gain (gnc-numeric-zero))
	       (sum-total-ugain (gnc-numeric-zero)))

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
				    (_ "Total Return")))

          (append! totalscols (list " "))

          (gnc:html-table-set-col-headers!
           table
	   headercols)
          
          (table-add-stock-rows
           table accounts to-date currency price-fn exchange-fn
           include-empty include-gains show-symbol show-listing show-shares show-price 
	   basis-method prefer-pricelist total-basis total-value total-moneyin total-moneyout total-gain total-ugain)
	  

	  (set! sum-total-gain (gnc:sum-collector-commodity total-gain currency exchange-fn))
	  (set! sum-total-ugain (gnc:sum-collector-commodity total-ugain currency exchange-fn))
	  (set! sum-total-both-gains (gnc:make-gnc-monetary currency (gnc-numeric-add (gnc:gnc-monetary-amount sum-total-gain)
										      (gnc:gnc-monetary-amount sum-total-ugain)
										      100 GNC-RND-ROUND)))

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
            (gnc:make-html-table-cell/size
             1 14 (gnc:make-html-text (gnc:html-markup-hr)))))

	  ;; finish building the totals columns, now that totals are complete
	  (append! totalscols (list
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:sum-collector-commodity total-basis currency exchange-fn))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:sum-collector-commodity total-value currency exchange-fn))
			       (gnc:make-html-table-cell/markup
				"total-number-cell" (gnc:monetary-neg (gnc:sum-collector-commodity total-moneyin currency exchange-fn)))
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
				(let ((totalinvalue (gnc-numeric-to-double
						     (gnc:gnc-monetary-amount (gnc:monetary-neg (gnc:sum-collector-commodity 
									       total-moneyin currency exchange-fn))))))
				  (if (= 0.0 totalinvalue) 
				      (sprintf #f "%.2f%%" totalinvalue) 
				      (sprintf #f "%.2f%%" (* 100 (/ (gnc-numeric-to-double
								      (gnc:gnc-monetary-amount sum-total-both-gains))
										   totalinvalue))))))
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
 'name reportname
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer advanced-portfolio-renderer)
