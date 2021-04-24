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

(define-module (gnucash reports standard price-scatter))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 match))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))

(define pagename-price (N_ "Price"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-commodity (N_ "Price of Commodity"))
(define optname-price-source (N_ "Price Source"))
(define optname-invert (N_ "Invert prices"))

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
      "f" (N_ "The source of price information.") 
      'actual-transactions
      (list (vector 'weighted-average (N_ "Weighted Average"))
            (vector 'actual-transactions (N_ "Actual Transactions"))
            (vector 'pricedb (N_ "Price Database")))))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-price optname-invert
      "g"
      (N_ "Plot commodity per currency rather than currency per commodity.")
      #f))

    
    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "c" (cons 'percent 100.0) (cons 'percent 100.0))
    
    (gnc:options-add-marker-choice!
     options gnc:pagename-display 
     optname-marker "a" 'filledsquare)

    (add-option
     (gnc:make-color-option
      gnc:pagename-display optname-markercolor
      "b"
      (N_ "Color of the marker.")
      (list #xb2 #x22 #x22 #xff)
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

  (define intervals
    (list (list 'DayDelta (G_ "Days") 86400)
          (list 'WeekDelta (G_ "Weeks") 604800)
          (list 'TwoWeekDelta (G_ "Double-Weeks") 1209600)
          (list 'MonthDelta (G_ "Months") 2628000)
          (list 'QuarterDelta (G_ "Quarters") (/ 31536000 4))
          (list 'HalfYearDelta (G_ "Half Years") (/ 31536000 2))
          (list 'YearDelta (G_ "Years") 31536000)))

  (let* ((to-date (gnc:time64-end-day-time 
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general 
                                optname-to-date))))
         (from-date (gnc:time64-start-day-time 
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
                      (gnc:time64-end-day-time from-date) 
                      (gnc:time64-end-day-time to-date)
                      (gnc:deltasym-to-delta interval)))
         
         (document (gnc:make-html-document))
         (chart (gnc:make-html-chart))
         (currency-accounts
          (filter gnc:account-has-shares? (gnc-account-get-descendants-sorted
                                           (gnc-get-current-root-account))))
	 (invert (get-option pagename-price optname-invert))
         (amount-commodity (if invert price-commodity report-currency))
         (base-commodity (if invert report-currency price-commodity))
         (int-label (car (assq-ref intervals interval)))
         (int-secs (cadr (assq-ref intervals interval)))
         (data '()))

    ;; Short helper for all the warnings below
    (define (make-warning title text)
      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
        (gnc:html-markup-h3 title)
        (gnc:html-markup-p text))))

    (gnc:html-chart-set-type! chart 'line)

    (gnc:html-chart-set-currency-iso!
     chart (gnc-commodity-get-mnemonic amount-commodity))
    (gnc:html-chart-set-currency-symbol!
     chart (gnc-commodity-get-nice-symbol amount-commodity))

    (gnc:html-chart-set-title!
     chart
     (list report-title
           (string-append
            (gnc-commodity-get-mnemonic base-commodity)
            " - "
            (format #f
                    (G_ "~a to ~a")
                    (qof-print-date from-date)
                    (qof-print-date to-date)))))
    (gnc:html-chart-set-width! chart width)
    (gnc:html-chart-set-height! chart height)
    (gnc:html-chart-set! chart
                         '(options elements point pointStyle)
                         (case marker
                           ((filleddiamond diamond) "rectRot")
                           ((filledcircle circle) "circle")
                           ((filledsquare square) "rect")
                           ((cross) "crossRot")
                           ((plus) "cross")
                           ((dash) "line")))

    (gnc:html-chart-set-y-axis-label!
     chart (gnc-commodity-get-mnemonic amount-commodity))

    (gnc:html-chart-set-x-axis-label! chart int-label)

    (gnc:html-chart-set! chart '(options scales xAxes (0) type) 'linear)
    (gnc:html-chart-set-custom-x-axis-ticks?! chart #f)

    (cond
     ((gnc-commodity-equiv report-currency price-commodity)
      (make-warning
       (G_ "Identical commodities")
       (G_ "Your selected commodity and the currency of the report \
are identical. It doesn't make sense to show prices for identical \
commodities.")))

     (else
      (when (or (not (null? currency-accounts)) (eq? price-source 'pricedb))
        (set! data
          (case price-source
            ((actual-transactions)
             (gnc:get-commodity-inst-prices
              currency-accounts to-date price-commodity report-currency))
            ((weighted-average)
             (gnc:get-commodity-totalavg-prices
              currency-accounts to-date price-commodity report-currency))
            ((pricedb)
             (map (lambda (p)
                    (list (gnc-price-get-time64 p) (gnc-price-get-value p)))
                  (gnc-pricedb-get-prices
                   (gnc-pricedb-get-db (gnc-get-current-book))
                   price-commodity report-currency))))))

      ;; the following transforms data in 1 assignment operation
      ;; 1. filters prices within specified dates
      ;; 2. transforms the price-date to numperiod since report start-date
      ;; 3. inverts the price-ratio if required
      (set! data
        (filter-map
         (match-lambda
           ((date amt)
            (and (<= from-date date to-date)
                 (list (/ (- date from-date) int-secs)
                       (if invert (/ 1 amt) amt)))))
         data))

      (cond
       ((null? data)
        (make-warning
         (G_ "No data")
         (G_ "There is no price information available for the \
selected commodities in the selected time period.")))

       ((<= (length data) 1)
        (make-warning
         (G_ "Only one price")
         (G_ "There was only one single price found for the \
selected commodities in the selected time period. This doesn't give \
a useful plot.")))

       ((apply equal? (map cadr data))
        (make-warning
         (G_ "All Prices equal")
         (G_ "All the prices found are equal. \
This would result in a plot with one straight line. \
Unfortunately, the plotting tool can't handle that.")))

       ((apply equal? (map car data))
        (make-warning
         (G_ "All Prices at the same date")
         (G_ "All the prices found are from the same date. \
This would result in a plot with one straight line. \
Unfortunately, the plotting tool can't handle that.")))

       (else
        (gnc:html-chart-set-data-labels!
         chart (map
                (match-lambda
                  ((x y)
                   (format #f "~2,2f ~a = ~a"
                           x int-label (gnc:monetary->string
                                        (gnc:make-gnc-monetary amount-commodity y)))))
                data))

        (gnc:html-chart-add-data-series!
         chart (G_ "Price")
         (map (match-lambda ((x y) (list (cons 'x x) (cons 'y y)))) data)
         mcolor
         'pointBorderColor mcolor 'fill #f 'borderColor "#4bb2c5"
         'pointBackgroundColor
         (if (memq marker '(filledcircle filledsquare filleddiamond)) mcolor "white"))

        (gnc:html-document-add-object! document chart)))))

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
