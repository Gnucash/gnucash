;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; price-scatter.scm: A scatter plot report about some price.
;;
;; By Christian Stimming <stimming@tuhh.de>
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/price-scatter.scm")
(gnc:depend  "report-html.scm")

(let ((optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-stepsize (N_ "Step Size"))
      (optname-report-currency (N_ "Report's currency"))

      (optname-price-commodity (N_ "Price of Commodity"))
      (optname-price-source (N_ "Price Source"))

      ;;      (optname-accounts (N_ "Accounts"))

      (optname-inc-exp (N_ "Show Income/Expense"))
      (optname-show-profit (N_ "Show Net Profit"))

      (optname-sep-bars (N_ "Show Asset & Liability bars"))
      (optname-net-bars (N_ "Show Net Worth bars"))

      (optname-marker (N_ "Marker"))
      (optname-markercolor (N_ "Marker Color"))
      (optname-plot-width (N_ "Plot Width"))
      (optname-plot-height (N_ "Plot Height")))

  (define (options-generator)
    (let* ((options (gnc:new-options)) 
           ;; This is just a helper function for making options.
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      (gnc:options-add-date-interval!
       options gnc:pagename-general
       optname-from-date optname-to-date "a")

      (gnc:options-add-interval-choice! 
       options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

					;      (add-option
					;       (gnc:make-account-list-option
					;	gnc:pagename-accounts optname-accounts
					;	"c"
					;	(N_ "Report on these accounts, if chosen account level allows.")
					;	(lambda ()
					;	  (gnc:group-get-subaccounts (gnc:get-current-group)))
					;	(lambda (accounts)
					;	  (list #t
					;		accounts))
					;	#t))

      (gnc:options-add-currency! 
       options gnc:pagename-general optname-report-currency "d")
      
      (add-option
       (gnc:make-currency-option 
	gnc:pagename-general optname-price-commodity
	"e"
	(N_ "Calculate the price of this commodity.")
	(gnc:locale-default-currency)))
      
      (add-option
       (gnc:make-multichoice-option
	gnc:pagename-general optname-price-source
	"f" (N_ "The source of price information") 
	'actual-transactions
	(list (vector 'weighted-average 
		      (N_ "Weighted Average")
		      (N_ "The weighted average all currency transactions of the past"))
	      (vector 'actual-transactions
		      (N_ "Actual Transactions")
		      (N_ "The actual price of currency transactions in the past"))
	      ;;(vector 'pricedb-nearest
	      ;;      (N_ "Pricedb: Nearest in time")
	      ;;    (N_ "The price recorded nearest in time to the report date"))
	      )))

      
      (gnc:options-add-plot-size! 
       options gnc:pagename-display 
       optname-plot-width optname-plot-height "c" 500 400)
      
      (gnc:options-add-marker-choice!
       options gnc:pagename-display 
       optname-marker "a" 'filledsquare)

      (add-option
       (gnc:make-color-option
	gnc:pagename-display optname-markercolor
	"b"
	(N_ "Color of the marker")
	(list #xb2 #x22 #x22 0)
	255 #f))

      (gnc:options-set-default-section options gnc:pagename-general)

      options))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The renderer function
  (define (renderer report-obj)

    ;; This is a helper function for looking up option values.
    (define (op-value section name)
      (gnc:option-value 
       (gnc:lookup-option (gnc:report-options report-obj) section name)))

    ;; small helper for the warnings below
    (define (commodity-numeric->string c n)
      (gnc:monetary->string
       (gnc:make-gnc-monetary c n)))


    (let* ((to-date-tp (gnc:timepair-end-day-time 
			(gnc:date-option-absolute-time
                         (op-value gnc:pagename-general 
				   optname-to-date))))
	   (from-date-tp (gnc:timepair-start-day-time 
			  (gnc:date-option-absolute-time
                           (op-value gnc:pagename-general 
				     optname-from-date))))
	   (interval (op-value gnc:pagename-general optname-stepsize))
	   ;; (accounts (op-value gnc:pagename-accounts optname-accounts))

	   (height (op-value gnc:pagename-display optname-plot-height))
	   (width (op-value gnc:pagename-display optname-plot-width))
	   (marker (op-value gnc:pagename-display optname-marker))
	   (mcolor 
	    (gnc:color-option->hex-string
	     (gnc:lookup-option (gnc:report-options report-obj)
				gnc:pagename-display optname-markercolor)))
	   
           (report-currency (op-value gnc:pagename-general
                                      optname-report-currency))
	   (price-commodity (op-value gnc:pagename-general 
				      optname-price-commodity))
	   (price-source (op-value gnc:pagename-general
				   optname-price-source))

	   (dates-list (gnc:make-date-list
			(gnc:timepair-end-day-time from-date-tp) 
                        (gnc:timepair-end-day-time to-date-tp)
                        (eval interval)))
	   
	   (document (gnc:make-html-document))
	   (chart (gnc:make-html-scatter))
	   (currency-accounts 
	    (filter gnc:account-has-shares? (gnc:group-get-subaccounts
					     (gnc:get-current-group))))
	   ;; some bogus data
	   (data '((1.0 1.0) (1.1 1.2) (1.2 1.4) (1.3 1.6) 
		   (2.0 1.0) (2.1 1.2) (2.2 1.4) (2.3 1.6))))
      
      (gnc:html-scatter-set-title! 
       chart (_ "Price Plot (Test)"))
      (gnc:html-scatter-set-subtitle!
       chart (sprintf #f
                      (_ "%s to %s")
                      (gnc:timepair-to-datestring from-date-tp) 
                      (gnc:timepair-to-datestring to-date-tp)))
      (gnc:html-scatter-set-width! chart width)
      (gnc:html-scatter-set-height! chart height)
      (gnc:html-scatter-set-marker! chart 
				    (case marker
				      ('circle "circle")
				      ('cross "cross")
				      ('square "square")
				      ('asterisk "asterisk")
				      ('filledcircle "filled circle")
				      ('filledsquare "filled square")))
      ;;(warn marker mcolor)
      ;; FIXME: workaround to set the alpha channel
      (set! mcolor (string-append mcolor "ff"))
      (gnc:html-scatter-set-markercolor! chart mcolor)
      (gnc:html-scatter-set-y-axis-label!
       chart (gnc:commodity-get-mnemonic report-currency))
      (gnc:html-scatter-set-x-axis-label!
       chart (case interval
	       ('DayDelta (N_ "Days"))
	       ('WeekDelta (N_ "Weeks"))
	       ('TwoWeekDelta (N_ "Double-Weeks"))
	       ('MonthDelta (N_ "Months"))
	       ('YearDelta (N_ "Years"))))

      (if 
       (not (gnc:commodity-equiv? report-currency price-commodity))
       (begin
	 (if (not (null? currency-accounts))
	     ;; This is an experiment, and if the code is good, it could
	     ;; go into commodity-utilities.scm or even start a new file.
	     (set!
	      data
	      (case price-source
		('actual-transactions
		 ;; go through all splits; convert all splits into a
		 ;; price. 
		 (map
		  (lambda (a)
		    (let* ((transaction-comm (gnc:transaction-get-commodity 
					      (gnc:split-get-parent a)))
			   (account-comm (gnc:account-get-commodity 
					  (gnc:split-get-account a)))
			   (share-amount (gnc:split-get-share-amount a))
			   (value-amount (gnc:split-get-value a))
			   (transaction-date (gnc:transaction-get-date-posted
					      (gnc:split-get-parent a)))
			   (foreignlist
			    (if (gnc:commodity-equiv? transaction-comm 
						      price-commodity)
				(list account-comm
				      (gnc:numeric-neg share-amount)
				      (gnc:numeric-neg value-amount))
				(list transaction-comm
				      value-amount
				      share-amount))))
		      
		      ;;(warn "render-scatterplot: value " 
		      ;;    (commodity-numeric->string
		      ;;   (first foreignlist) (second foreignlist))
		      ;; " bought shares "
		      ;;(commodity-numeric->string
		      ;; price-commodity (third foreignlist)))
		      
		      (list
		       transaction-date
		       (if (not (gnc:commodity-equiv? (first foreignlist) 
						      report-currency))
			   (begin
			     (warn "render-scatterplot: " 
				   "Sorry, currency exchange not yet implemented:"
				   (commodity-numeric->string
				    (first foreignlist) (second foreignlist))
				   " (buying "
				   (commodity-numeric->string
				    price-commodity (third foreignlist))
				   ") =? "
				   (commodity-numeric->string
				    report-currency (gnc:numeric-zero)))
			     (gnc:numeric-zero))
			   (gnc:numeric-div 
			    (second foreignlist)
			    (third foreignlist)
			    GNC-DENOM-AUTO 
			    (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND))))))
		  ;; Get all the interesting splits
		  (gnc:get-match-commodity-splits 
		   currency-accounts 
		   to-date-tp price-commodity)))
		('weighted-average
		 (gnc:get-commodity-totalaverage-prices
		  currency-accounts to-date-tp 
		  price-commodity report-currency))
		)))

	 (set! data (filter
		     (lambda (x) 
		       (gnc:timepair-lt from-date-tp (first x)))
		     data))
	 
	 ;; some output
	 ;;(warn (map (lambda (x) (list
	 ;;		     (gnc:timepair-to-datestring (car x))
	 ;;	     (gnc:numeric-to-double (second x))))
	 ;;data))

	 ;; convert the gnc:numeric's to doubles
	 (set! data (map (lambda (x) 
			   (list (first x) 
				 (gnc:numeric-to-double (second x))))
			 data))

	 ;; convert the dates to the weird x-axis scaling of the
	 ;; scatterplot
	 (set! data
	       (map (lambda (x)
		      (list
		       (/ (- (car (first x))
			     (car from-date-tp))
			  ;; FIXME: These hard-coded values are more
			  ;; or less totally bogus. OTOH this whole
			  ;; scaling thing is totally bogus as well,
			  ;; so this doesn't matter too much.
			  (case interval
			    ('DayDelta 86400)
			    ('WeekDelta 604800)
			    ('TwoWeekDelta 1209600)
			    ('MonthDelta 2628000)
			    ('YearDelta 31536000)))
		       (second x)))
		    data))
	 ))
      
      (gnc:html-scatter-set-data! 
       chart data)
      
      (gnc:html-document-add-object! document chart) 
      
      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
	(gnc:html-markup-p 
	 "This report calculates the 'prices of commodity' transactions \
versus the 'report commodity'. (I.e. it won't work if there's another \
commodity involved in between.) cstim.")))
      
      document))

  ;; Here we define the actual report
  (gnc:define-report
   'version 1
   'name (N_ "Price Scatter Plot (Test)")
   ;;'menu-path (list gnc:menuname-asset-liability)
   'options-generator options-generator
   'renderer renderer))
