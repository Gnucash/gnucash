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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report price-scatter))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-stepsize (N_ "Step Size"))

(define pagename-price (N_ "Price"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-commodity (N_ "Price of Commodity"))
(define optname-price-source (N_ "Price Source"))
(define optname-invert (N_ "Invert prices"))

;;      (optname-accounts (N_ "Accounts"))

(define optname-inc-exp (N_ "Show Income/Expense"))
(define optname-show-profit (N_ "Show Net Profit"))

(define optname-sep-bars (N_ "Show Asset & Liability bars"))
(define optname-net-bars (N_ "Show Net Worth bars"))

(define optname-marker (N_ "Marker"))
(define optname-markercolor (N_ "Marker Color"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

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

    (gnc:options-add-currency! 
     options pagename-price optname-report-currency "d")

    (add-option
     (gnc:make-commodity-option 
      pagename-price optname-price-commodity
      "e"
      (N_ "Calculate the price of this commodity.")
      (gnc-locale-default-iso-currency-code)))

    (add-option
     (gnc:make-multichoice-option
      pagename-price optname-price-source
      "f" (N_ "The source of price information") 
      'actual-transactions
      (list (vector 'weighted-average 
                    (N_ "Weighted Average")
                    (N_ "The weighted average of all currency transactions of the past"))
            (vector 'actual-transactions
                    (N_ "Actual Transactions")
                    (N_ "The instantaneous price of actual currency transactions in the past"))
            (vector 'pricedb
                    (N_ "Price Database")
                    (N_ "The recorded prices"))
            )))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-price optname-invert
      "g"
      (N_ "Plot commodity per currency rather than currency per commodity.")
      #f))

    
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
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  ;; small helper for the warnings below
  (define (commodity-numeric->string c n)
    (gnc:monetary->string
     (gnc:make-gnc-monetary c n)))


  (let* ((to-date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general 
                                   optname-to-date))))
         (from-date-tp (gnc:timepair-start-day-time 
                        (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general 
                                     optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-title (get-option gnc:pagename-general 
                                   gnc:optname-reportname))

         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (marker (get-option gnc:pagename-display optname-marker))
         (mcolor 
          (gnc:color-option->hex-string
           (gnc:lookup-option (gnc:report-options report-obj)
                              gnc:pagename-display optname-markercolor)))
         
         (report-currency (get-option pagename-price
                                      optname-report-currency))
         (price-commodity (get-option pagename-price
                                      optname-price-commodity))
         (price-source (get-option pagename-price
                                   optname-price-source))

         (dates-list (gnc:make-date-list
                      (gnc:timepair-end-day-time from-date-tp) 
                      (gnc:timepair-end-day-time to-date-tp)
		      (gnc:deltasym-to-delta interval)))
         
         (document (gnc:make-html-document))
         (chart (gnc:make-html-scatter))
         (currency-accounts 
          (filter gnc:account-has-shares? (gnc-account-get-descendants-sorted
                                           (gnc-get-current-root-account))))
	 (invert (get-option pagename-price optname-invert))
         (data '()))

    ;; Short helper for all the warnings below
    (define (make-warning title text)
      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
        (gnc:html-markup-h2 title)
        (gnc:html-markup-p text))))

    (gnc:html-scatter-set-title! 
     chart report-title)
    (gnc:html-scatter-set-subtitle!
     chart (string-append
	    ;; Check for whether it is commodity against currency or
	    ;; the other way round. 
	    (if invert
		(gnc-commodity-get-mnemonic report-currency)
		(gnc-commodity-get-mnemonic price-commodity))
            " - "
            (sprintf #f
                     (_ "%s to %s")
                     (gnc-print-date from-date-tp)
                     (gnc-print-date to-date-tp))))
    (gnc:html-scatter-set-width! chart width)
    (gnc:html-scatter-set-height! chart height)
    (gnc:html-scatter-set-marker! chart 
                                  (case marker
                                    ((circle) "circle")
                                    ((cross) "cross")
                                    ((square) "square")
                                    ((asterisk) "asterisk")
                                    ((filledcircle) "filled circle")
                                    ((filledsquare) "filled square")))
    (gnc:html-scatter-set-markercolor! chart mcolor)
    (gnc:html-scatter-set-y-axis-label!
     chart 
     ;; Check for whether it is commodity against currency or
     ;; the other way round. 
     (if invert
	 (gnc-commodity-get-mnemonic price-commodity)
	 (gnc-commodity-get-mnemonic report-currency)))
    (gnc:html-scatter-set-x-axis-label!
     chart (case interval
             ((DayDelta) (N_ "Days"))
             ((WeekDelta) (N_ "Weeks"))
             ((TwoWeekDelta) (N_ "Double-Weeks"))
             ((MonthDelta) (N_ "Months"))
             ((YearDelta) (N_ "Years"))))

    (if 
     (not (gnc-commodity-equiv report-currency price-commodity))
     (begin
       (if (or (not (null? currency-accounts))
               (eq? price-source 'pricedb))
           (set!
            data
            (case price-source
              ((actual-transactions)
               (gnc:get-commodity-inst-prices
                currency-accounts to-date-tp 
                price-commodity report-currency))
              ((weighted-average)
               (gnc:get-commodity-totalavg-prices
                currency-accounts to-date-tp 
                price-commodity report-currency))
              ((pricedb)
               (map (lambda (p)
                      (list (gnc-price-get-time p)
                            (gnc-price-get-value p)))
                    (gnc-pricedb-get-prices
                     (gnc-pricedb-get-db (gnc-get-current-book))
                     price-commodity report-currency)))
              )))

       (set! data (filter
                   (lambda (x) 
                     (and 
                      (gnc:timepair-ge to-date-tp (first x))
                      (gnc:timepair-ge (first x) from-date-tp)))
                   data))

       ;; some output
       ;;(warn "data" (map (lambda (x) (list
       ;;			(gnc-print-date (car x))
       ;;		(gnc-numeric-to-double (second x))))
       ;; data))
       
       ;; convert the gnc:numeric's to doubles
       (if invert
	   (set! data (map (lambda (x) 
			     (list (first x) 
				   (/ 1 (gnc-numeric-to-double (second x)))))
			   data))
	   (set! data (map (lambda (x) 
			     (list (first x) 
				   (gnc-numeric-to-double (second x))))
			   data)))

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
                          ((DayDelta) 86400)
                          ((WeekDelta) 604800)
                          ((TwoWeekDelta) 1209600)
                          ((MonthDelta) 2628000)
                          ((YearDelta) 31536000)))
                     (second x)))
                  data))
       
       (gnc:html-scatter-set-data! 
        chart data)

       ;; Make tons of tests so that Guppi won't barf
       (if (not (null? data))
           (if (> (length data) 1)
               (if (apply equal? (map second data))
                   (make-warning 
                    (_ "All Prices equal")
                    (_ "All the prices found are equal. \
This would result in a plot with one straight line. \
Unfortunately, the plotting tool can't handle that."))
                   (if (apply equal? (map first data))
                       (make-warning
                        (_ "All Prices at the same date")
                        (_ "All the prices found are from the same date. \
This would result in a plot with one straight line. \
Unfortunately, the plotting tool can't handle that."))

                       (gnc:html-document-add-object! document chart)))

               (make-warning
                (_ "Only one price")
                (_ "There was only one single price found for the \
selected commodities in the selected time period. This doesn't give \
a useful plot.")))
           (make-warning
            (_ "No data")
            (_ "There is no price information available for the \
selected commodities in the selected time period."))))

     ;; warning if report-currency == price-commodity
     (make-warning 
      (_ "Identical commodities")
      (_ "Your selected commodity and the currency of the report \
are identical. It doesn't make sense to show prices for identical \
commodities.")))
    
    document))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name (N_ "Price")
 'report-guid "1d241609fd4644caad765c95be20ff4c"
 'menu-path (list gnc:menuname-asset-liability)
 'menu-name (N_ "Price Scatterplot")
 'options-generator options-generator
 'renderer renderer)
