;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; options-utilities.scm: Useful option helper functions.
;; 
;; By Christian Stimming <stimming@tu-harburg.de>
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


;; These are just a bunch of options which were useful in several
;; reports and hence they got defined in a seperate function.

;; This is one single end-date of a report.
(define (gnc:options-add-report-date!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-date-option
     pagename optname 
     sort-tag (N_ "Select a date to report on")
     (lambda () 
       (cons 'relative 'today))
     #f 'both 
     '(
       today 
       end-this-month
       end-prev-month 
       end-current-quarter 
       end-prev-quarter
       end-cal-year 
       end-prev-year 
       end-cur-fin-year
       end-prev-fin-year
       ))))


;; This is a date-interval for a report.
(define (gnc:options-add-date-interval!
	 options pagename name-from name-to sort-tag)
  (gnc:register-option 
   options  
   (gnc:make-date-option
    pagename name-from 
    (string-append sort-tag "a")
    (N_ "Start of reporting period")
    (lambda () (cons 'relative 'start-cal-year))
     #f 'both 
     '(
       start-this-month 
       start-prev-month 
       start-current-quarter
       start-prev-quarter
       start-cal-year 
       start-prev-year
       start-cur-fin-year 
       start-prev-fin-year
       )))
  (gnc:register-option 
   options  
   (gnc:make-date-option
    pagename name-to
    (string-append sort-tag "b")
    (N_ "End of reporting period")
    (lambda () (cons 'relative 'today))
    #f 'both 
    '(
      today
      end-this-month
      end-prev-month 
      end-current-quarter 
      end-prev-quarter
      end-cal-year 
      end-prev-year 
      end-cur-fin-year
      end-prev-fin-year
      ))))

;; A date interval multichoice option.
(define (gnc:options-add-interval-choice! 
	 options pagename optname sort-tag default)
  (gnc:register-option 
   options
   (gnc:make-multichoice-option
    pagename optname
    sort-tag (N_ "The amount of time between data points") default
    (list (vector 'DayDelta (N_ "Day") (N_ "Day"))
	  (vector 'WeekDelta (N_ "Week") (N_ "Week"))
	  (vector 'TwoWeekDelta (N_ "2Week") (N_ "Two Week"))
	  (vector 'MonthDelta (N_ "Month") (N_ "Month"))
	  (vector 'QuarterDelta (N_ "Quarter") (N_ "Quarter"))
	  (vector 'HalfYearDelta (N_ "Half Year") (N_ "Half Year"))
	  (vector 'YearDelta (N_ "Year") (N_ "Year"))
	  ))))

;; A multichoice option intended to chose the account level. Different
;; from the other functions the help string can still be given. Used
;; below.
(define (gnc:options-add-account-levels! 
	 options pagename name-display-depth 
	 sort-tag help-string default-depth)
  (gnc:register-option 
   options  
   (gnc:make-multichoice-option
    pagename name-display-depth
    sort-tag 
    help-string
    default-depth
    (list (list->vector
	   (list 'all (N_ "All") (N_ "All accounts")))
	  (list->vector
	   (list 1 "1" (N_ "Top-level")))
	  (list->vector
	   (list 2 "2" (N_ "Second-level")))
	  (list->vector
	   (list 3 "3" (N_ "Third-level")))
	  (list->vector
	   (list 4 "4" (N_ "Fourth-level")))
	  (list->vector
	   (list 5 "5" (N_ "Fourth-level")))
	  (list->vector
	   (list 6 "6" (N_ "Sixth-level")))))))

;; These help for selecting a bunch of accounts.
(define (gnc:options-add-account-selection! 
	 options pagename 
	 name-display-depth name-show-subaccounts name-accounts
	 sort-tag default-depth default-accounts)
  (gnc:options-add-account-levels!
   options pagename name-display-depth 
   (string-append sort-tag "a")
   (N_ "Show accounts to this depth, overriding any other option.") 
   default-depth)
    
  (gnc:register-option 
   options  
   (gnc:make-simple-boolean-option
    pagename name-show-subaccounts
    (string-append sort-tag "b")
    (N_ "Override account-selection and show sub-accounts of all selected accounts?") 
    #t))

  ;; Semantics of the account selection, as used in the
  ;; gnc:html-build-acct-table: An account shows up if ( the
  ;; tree-depth is large enough AND ( it is selected in the account
  ;; selector OR ( always show sub-accounts is selected AND one of
  ;; the parents is selected in the account selector. )))
  (gnc:register-option 
   options  
   (gnc:make-account-list-option
    pagename name-accounts
    (string-append sort-tag "c")
    (N_ "Report on these accounts, if display depth allows.")
    default-accounts
    #f #t)))

;; The single checkbox whether to include the sub-account balances
;; into the other balances.
(define (gnc:options-add-include-subaccounts!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-simple-boolean-option
     pagename optname
     sort-tag (N_ "Include sub-account balances in printed balance?") #t)))

;; The single checkbox whether to group the accounts into main
;; categories and ahow a subtotal for those.
(define (gnc:options-add-group-accounts!
	 options pagename optname sort-tag default?)
  (gnc:register-option 
    options  
    (gnc:make-simple-boolean-option
     pagename optname
     sort-tag (N_ "Group the accounts in main categories?") default?)))

;; To let the user select a currency for the report.
(define (gnc:options-add-currency!
	 options pagename name-report-currency sort-tag)
  (gnc:register-option 
   options 
   (gnc:make-currency-option 
    pagename name-report-currency
    sort-tag 
    (N_ "Select the currency to display the values of this report in.")
    (gnc:default-currency))))

;; These are common options for the selection of the report's
;; currency/commodity.
(define (gnc:options-add-currency-selection!
	 options pagename 
	 name-show-foreign name-report-currency sort-tag)
  (gnc:register-option 
   options 
   (gnc:make-simple-boolean-option
    pagename name-show-foreign
    (string-append sort-tag "a")
    (N_ "Display the account's foreign currency amount?") #f))

  (gnc:options-add-currency! options pagename name-report-currency 
			     (string-append sort-tag "b")))

;; A multichoice option for the source of prices
(define (gnc:options-add-price-source! 
	 options pagename optname sort-tag default)
  (gnc:register-option 
   options
   (gnc:make-multichoice-option
    pagename optname
    sort-tag (N_ "The source of price information") default
    (list (vector 'weighted-average 
		  (N_ "Weighted Average")
		  (N_ "The weighted average of all currency transactions of the past"))
	  (vector 'pricedb-latest 
		  (N_ "Most recent")
		  (N_ "The most recent recorded price"))
	  (vector 'pricedb-nearest
		  (N_ "Nearest in time")
		  (N_ "The price recorded nearest in time to the report date"))
	  ))))

;; The width- and height- options for charts
(define (gnc:options-add-plot-size!
	 options pagename 
	 name-width name-height sort-tag 
	 default-width default-height)
  (gnc:register-option
   options
   (gnc:make-number-range-option
    pagename name-width
    (string-append sort-tag "a")
    (N_ "Width of plot in pixels.") default-width
    100 20000 0 5))

  (gnc:register-option
   options
   (gnc:make-number-range-option
    pagename name-height
    (string-append sort-tag "b")
    (N_ "Height of plot in pixels.") default-height
    100 20000 0 5)))

;; A multicoice option for the marker of a scatter plot.
(define (gnc:options-add-marker-choice!
	 options pagename optname sort-tag default)
  (gnc:register-option
   options
   (gnc:make-multichoice-option
    pagename optname 
    sort-tag
    (N_ "Choose the marker for each data point.")
    default
    (list
     (vector 'circle "circle" "circle")
     (vector 'cross "cross" "cross")
     (vector 'square "square" "square")
     (vector 'asterisk "asterisk" "asterisk")
     (vector 'filledcircle "filled circle" "filled circle")
     (vector 'filledsquare "filled square" "filled square")))))

