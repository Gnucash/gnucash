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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report options-utilities))

(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))

(export gnc:options-add-report-date!)
(export gnc:options-add-date-interval!)
(export gnc:options-add-interval-choice!)
(export gnc:options-add-account-levels!)
(export gnc:options-add-account-selection!)
(export gnc:options-add-currency!)
(export gnc:options-add-price-source!)
(export gnc:options-add-plot-size!)
(export gnc:options-add-marker-choice!)
(export gnc:options-add-sort-method!)
(export gnc:options-add-subtotal-view!)

;; These are just a bunch of options which were useful in several
;; reports and hence they got defined in a separate function.

;; This is one single end-date of a report.
(define (gnc:options-add-report-date!
	 options pagename optname sort-tag)
  (gnc:options-make-end-date! options pagename optname sort-tag
			      (N_ "Select a date to report on.")))

;; This is a date-interval for a report.
(define (gnc:options-add-date-interval!
	 options pagename name-from name-to sort-tag)
  (gnc:options-make-date-interval! options pagename
				   name-from (N_ "Start of reporting period.")
				   name-to (N_ "End of reporting period.")
				   sort-tag))

;; A date interval multichoice option.
(define (gnc:options-add-interval-choice! 
	 options pagename optname sort-tag default)
  (gnc:register-option 
   options
   (gnc:make-multichoice-option
    pagename optname
    sort-tag (N_ "The amount of time between data points.") default
    (list (vector 'DayDelta (N_ "One Day"))
          (vector 'WeekDelta (N_ "One Week"))
          (vector 'TwoWeekDelta (N_ "Two Weeks"))
          (vector 'MonthDelta (N_ "One Month"))
          (vector 'QuarterDelta (N_ "Quarter Year"))
          (vector 'HalfYearDelta (N_ "Half Year"))
          (vector 'YearDelta (N_ "One Year"))))))

;; A multichoice option intended to chose the account level. Different
;; from the other functions the help string can still be given. Used
;; below.
(define (gnc:options-add-account-levels! 
	 options pagename name-display-depth 
	 sort-tag help-string default-depth)
  (gnc:register-option 
   options  
   (gnc:make-multichoice-option
    pagename name-display-depth sort-tag help-string default-depth
    (list (vector 'all (N_ "All"))
          (vector 1 "1")
          (vector 2 "2")
          (vector 3 "3")
          (vector 4 "4")
          (vector 5 "5")
          (vector 6 "6")))))

;; These help for selecting a bunch of accounts.
(define (gnc:options-add-account-selection! 
	 options pagename 
	 name-display-depth name-show-subaccounts name-accounts
	 sort-tag default-depth default-accounts default-show-subaccounts)
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
    default-show-subaccounts))

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

;; To let the user select a currency for the report.
(define (gnc:options-add-currency!
	 options pagename name-report-currency sort-tag)
  (gnc:register-option 
   options 
   (gnc:make-currency-option 
    pagename name-report-currency
    sort-tag 
    (N_ "Select the currency to display the values of this report in.")
    (gnc-default-report-currency))))

;; A multichoice option for the source of prices
(define (gnc:options-add-price-source! 
	 options pagename optname sort-tag default)
  (gnc:register-option 
   options
   (gnc:make-multichoice-option
    pagename optname
    sort-tag (N_ "The source of price information.") default
    (list (vector 'average-cost (N_ "Average cost of purchases weighted by volume"))
          (vector 'weighted-average (N_ "Weighted average of all transactions in the past"))
          (vector 'pricedb-before (N_ "Last up through report date"))
          (vector 'pricedb-nearest (N_ "Closest to report date"))
          (vector 'pricedb-latest (N_ "Most recent"))))))

;; The width- and height- options for charts
(define (gnc:options-add-plot-size!
	 options pagename 
	 name-width name-height sort-tag 
	 default-width default-height)
  (gnc:register-option
   options
   (gnc:make-number-plot-size-option
    pagename name-width
    (string-append sort-tag "a")
    (N_ "Width of plot in pixels.") default-width
    100 20000 0 5))

  (gnc:register-option
   options
   (gnc:make-number-plot-size-option
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
     (vector 'diamond (N_ "Diamond"))
     (vector 'circle (N_ "Circle"))
     (vector 'square (N_ "Square"))
     (vector 'cross (N_ "Cross"))
     (vector 'plus (N_ "Plus"))
     (vector 'dash (N_ "Dash"))
     (vector 'filleddiamond (N_ "Filled diamond"))
     (vector 'filledcircle (N_ "Filled circle"))
     (vector 'filledsquare (N_ "Filled square"))))))


(define (gnc:options-add-sort-method!
	 options pagename optname sort-tag default)
  (gnc:register-option
   options
   (gnc:make-multichoice-option
    pagename optname 
    sort-tag
    (N_ "Choose the method for sorting accounts.")
    default
    (list
     (vector 'acct-code (N_ "Alphabetical by account code"))
     (vector 'alphabetical (N_ "Alphabetical by account name"))
     (vector 'amount (N_ "Numerical by descending amount"))))))


;; These control the calculation and view mode of subtotal balances
(define (gnc:options-add-subtotal-view!
	 options pagename 
	 optname-parent-balance-mode optname-parent-total-mode
	 sort-tag)
  ;; what to show for non-leaf accounts
  (gnc:register-option
   options
   (gnc:make-multichoice-option
    pagename 
    ;; usually the option name is: (N_ "Parent account balances")
    optname-parent-balance-mode
    (string-append sort-tag "a")
    (string-join
     (list
      (G_ "How to show the balances of parent accounts.")
      (G_ "Account Balance in the parent account, excluding any subaccounts.")
      (G_ "Do not show any balances of parent accounts."))
      "\n* ")
    'immediate-bal
    (list (vector 'immediate-bal (N_ "Account Balance"))
          (vector 'recursive-bal (N_ "Calculate Subtotal"))
          (vector 'omit-bal (N_ "Do not show")))))
  (gnc:register-option
   options
   (gnc:make-multichoice-option
    pagename
    ;; usually the option name is: (N_ "Parent account subtotals")
    optname-parent-total-mode
    (string-append sort-tag "b")
    (string-join
     (list
      (G_ "How to show account subtotals for parent accounts.")
      (G_ "Show subtotals for selected parent accounts which have subaccounts.")
      (G_ "Do not show any subtotals for parent accounts."))
      "\n* ")
    'f
    (list (vector 't (N_ "Show subtotals"))
          (vector 'f (N_ "Do not show"))))))
