;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category-barchart.scm: shows barchart of income/expense categories
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/category-barchart.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

;; The option names are defined here to 1. save typing and 2. avoid
;; spelling errors. The *reportnames* are defined here (and not only
;; once at the very end) because I need them to define the "other"
;; report, thus needing them twice.
(let ((reportname-income (N_ "Income Barchart"))
      (reportname-expense (N_ "Expense Barchart"))
      (reportname-assets (N_ "Asset Barchart"))
      (reportname-liabilities (N_ "Liability Barchart"))
      ;; The names are used in the menu, as labels and as identifiers.

      ;; The titels here are only printed as titles of the report.
      (reporttitle-income (_ "Income Accounts"))
      (reporttitle-expense (_ "Expense Accounts"))
      (reporttitle-assets (_ "Asset Accounts"))
      (reporttitle-liabilities (_ "Liability/Equity Accounts"))

      ;; Option names
      (pagename-general (N_ "General"))
      (optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-stepsize (N_ "Step Size"))
      (optname-report-currency (N_ "Report's currency"))

      (pagename-accounts (N_ "Accounts"))
      (optname-accounts (N_ "Accounts"))
      (optname-levels (N_ "Show Accounts until level"))

      (pagename-display (N_ "Display"))
      (optname-fullname (N_ "Show long account names"))
      (optname-stacked (N_ "Use Stacked Bars"))
      (optname-slices (N_ "Maximum Bars"))
      (optname-plot-width (N_ "Plot Width"))
      (optname-plot-height (N_ "Plot Height")))

  (define (options-generator account-types)
    (let* ((options (gnc:new-options)) 
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      ;; General tab
      (gnc:options-add-date-interval!
       options pagename-general
       optname-from-date optname-to-date "a")

      (gnc:options-add-interval-choice! 
       options pagename-general optname-stepsize "b" 'MonthDelta)

      (gnc:options-add-currency! 
       options pagename-general optname-report-currency "c")

      ;; Accounts tab
      (add-option
       (gnc:make-account-list-option
	pagename-accounts optname-accounts
	"a"
	(N_ "Report on these accounts, if chosen account level allows.")
	(lambda ()
	  (gnc:filter-accountlist-type 
	   account-types
	   (gnc:group-get-subaccounts (gnc:get-current-group))))
	(lambda (accounts)
	  (list #t
		(gnc:filter-accountlist-type account-types accounts)))
	#t))
      
      (gnc:options-add-account-levels! 
       options pagename-accounts optname-levels "c" 
       (N_ "Show accounts to this depth and not further") 
       2)

      ;; Display tab
      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-fullname
        "a" (N_ "Show the full account name in legend?") #f))

      (add-option
       (gnc:make-simple-boolean-option
        pagename-display optname-stacked
        "b" 
	(N_ "Show barchart as stacked barchart? (Guppi>=0.35.4 required)") 
	#t))

      (add-option
       (gnc:make-number-range-option
        pagename-display optname-slices
        "c" (N_ "Maximum number of bars in the chart") 8
        2 24 0 1))

      (gnc:options-add-plot-size! 
       options pagename-display 
       optname-plot-width optname-plot-height "c" 400 400)

      (gnc:options-set-default-section options pagename-general)

      options))

  ;; This is the rendering function. It accepts a database of options
  ;; and generates an object of type <html-document>.  See the file
  ;; report-html.txt for documentation; the file report-html.scm
  ;; includes all the relevant Scheme code. The option database passed
  ;; to the function is one created by the options-generator function
  ;; defined above.

  ;; FIXME: the exchange rate should change every time interval, of
  ;; course, but right now we assume the very last exchange rate to be
  ;; constant over the whole report period. Note that this might get
  ;; *really* complicated.

  (define (category-barchart-renderer report-obj reportname 
				      account-types report-title)
    ;; A helper functions for looking up option values.
    (define (get-option section name)
      (gnc:option-value 
       (gnc:lookup-option 
	(gnc:report-options report-obj) section name)))
    
    (let ((to-date-tp (gnc:timepair-end-day-time 
		       (vector-ref (get-option pagename-general
					       optname-to-date) 1)))
	  (from-date-tp (gnc:timepair-start-day-time 
			 (vector-ref (get-option pagename-general
						 optname-from-date) 1)))
	  (interval (get-option pagename-general optname-stepsize))
	  (report-currency (get-option pagename-general
				       optname-report-currency))
	  
	  (accounts (get-option pagename-accounts optname-accounts))
	  (account-levels (get-option pagename-accounts optname-levels))
	  
	  (stacked? (get-option pagename-display optname-stacked))
	  (show-fullname? (get-option pagename-display optname-fullname))
	  (max-slices (get-option pagename-display optname-slices))
	  (height (get-option pagename-display optname-plot-height))
	  (width (get-option pagename-display optname-plot-width))
	  
	  (document (gnc:make-html-document))
	  (chart (gnc:make-html-barchart))
	  (topl-accounts (gnc:filter-accountlist-type 
			  account-types
			  (gnc:group-get-account-list 
			   (gnc:get-current-group)))))
      
      ;; Returns true if the account a was selected in the account
      ;; selection option.
      (define (show-acct? a)
	(member a accounts))

      ;; Define more helper variables.
      (let* ((exchange-alist (gnc:make-exchange-alist
			      report-currency to-date-tp))
	     (exchange-fn (gnc:make-exchange-function exchange-alist))
	     (tree-depth (if (equal? account-levels 'all)
			     (gnc:get-current-group-depth)
			     account-levels))
	     ;; This is the list of date intervals to calculate.
	     (dates-list (gnc:make-date-interval-list
			  (gnc:timepair-start-day-time from-date-tp) 
			  (gnc:timepair-end-day-time to-date-tp)
			  (eval interval)))
	     ;; Here the date strings for the x-axis labels are
	     ;; created.
	     (date-string-list
	      (map (lambda (date-list-item)
		     (gnc:timepair-to-datestring
		      (car date-list-item)))
		   dates-list))
	     (other-anchor "")
	     (all-data '()))
	
	;; Converts a commodity-collector into one single double
	;; number, depending on the report currency and the
	;; exchange-alist calculated above. Returns a double.
	(define (collector->double c)
	  ;; Future improvement: Let the user choose which kind of
	  ;; currency combining she want to be done. 
	  (gnc:numeric-to-double 
	   (gnc:gnc-monetary-amount
	    (gnc:sum-collector-commodity 
	     c report-currency 
	     exchange-fn))))
	
	;; Calculates the net balance (profit or loss) of an account in
	;; the given time interval. date-list-entry is a pair containing
	;; the start- and end-date of that interval. If subacct?==#t,
	;; the subaccount's balances are included as well. Returns a
	;; double, exchanged into the report-currency by the above
	;; conversion function, and possibly with reversed sign.
	(define (get-balance account date-list-entry subacct?)
	  ((if (gnc:account-reverse-balance? account)
	       - +)
	   (collector->double
	    (gnc:account-get-comm-balance-interval 
	     account 
	     (car date-list-entry) 
	     (cadr date-list-entry) subacct?))))

	;; Creates the <balance-list> to be used in the function
	;; below. 
	(define (account->balance-list account subacct?)
	  (map 
	   (lambda (d) (get-balance account d subacct?))
	   dates-list))
	
	;; Calculates all account's balances. Returns a list of pairs:
	;; (<account> <balance-list>), like '((Earnings (10.0 11.2))
	;; (Gifts (12.3 14.5))), where each element of <balance-list>
	;; is the balance corresponding to one element in
	;; <dates-list>.
	;;
	;; If current-depth >= tree-depth, then the balances are
	;; calculated *with* subaccount's balances. Else only the
	;; current account is regarded. Note: All accounts in accts
	;; and all their subaccounts are processed, but a balances is
	;; calculated and returned *only* for those accounts where
	;; show-acct? is true. This is necessary because otherwise we
	;; would forget an account that is selected but not its
	;; parent.
	(define (traverse-accounts current-depth accts)
	  (if (< current-depth tree-depth)
	      (let ((res '()))
		(for-each
		 (lambda (a)
		   (begin
		     (if (show-acct? a)
			 (set! res 
			       (cons (list a (account->balance-list a #f))
				     res)))
		     (set! res (append
				(traverse-accounts
				 (+ 1 current-depth)
				 (gnc:account-get-immediate-subaccounts a))
				res))))
		 accts)
		res)
	      ;; else (i.e. current-depth == tree-depth)
	      (map
	       (lambda (a)
		 (list a (account->balance-list a #t)))
	       (filter show-acct? accts))))
	
	;; Sort the account list according to the account code field.
	(set! all-data (sort 
			(filter (lambda (l) 
				  (not (= 0.0 (apply + (cadr l))))) 
				(traverse-accounts 1 topl-accounts))
			(lambda (a b) 
			  (string<? (gnc:account-get-code (car a))
				    (gnc:account-get-code (car b))))))
	;; Or rather sort by total amount?
	;;(< (apply + (cadr a)) 
	;;   (apply + (cadr b))))))
	;; Other sort criteria: max. amount, standard deviation of amount,
	;; min. amount; ascending, descending. FIXME: Add user options to
	;; choose sorting.


	;;(warn "all-data" all-data)

	;; Set chart title, subtitle etc.
	(gnc:html-barchart-set-title! chart report-title)
	(gnc:html-barchart-set-subtitle!
	 chart (sprintf #f
			(_ "%s to %s")
			(gnc:timepair-to-datestring from-date-tp) 
			(gnc:timepair-to-datestring to-date-tp)))
	(gnc:html-barchart-set-width! chart width)
	(gnc:html-barchart-set-height! chart height)

	;; row labels etc.
	(gnc:html-barchart-set-row-labels! chart date-string-list)
	;; FIXME: why doesn't the y-axis label get printed?!?
	(gnc:html-barchart-set-y-axis-label!
	 chart (gnc:commodity-get-mnemonic report-currency))
	(gnc:html-barchart-set-row-labels-rotated?! chart #t)
	(gnc:html-barchart-set-stacked?! chart stacked?)
	;; If this is a stacked barchart, then reverse the legend.
	(gnc:html-barchart-set-legend-reversed?! chart stacked?)

	;; If we have too many categories, we sum them into a new
	;; 'other' category and add a link to a new report with just
	;; those accounts.
	(if (> (length all-data) max-slices)
	    (let* ((start (take all-data (- max-slices 1)))
		   (finish (drop all-data (- max-slices 1)))
		   (other-sum (map 
			       (lambda (l) (apply + l))
			       (apply zip (map cadr finish)))))
	      (set! all-data
		    (append start
			    (list (list (_ "Other") other-sum))))
	      (let* ((name reportname)
		     (options (gnc:make-report-options name)))
		;; now copy all the options
		(define (set-option! pagename optname value)
		  (gnc:option-set-value
		   (gnc:lookup-option options pagename optname)
		   value))
		(for-each
		 (lambda (l) (set-option! (car l) (cadr l) (caddr l)))
		 (list
		  (list pagename-general optname-from-date
			(cons 'absolute from-date-tp))
		  (list pagename-general optname-to-date
			(cons 'absolute to-date-tp))
		  (list pagename-general optname-stepsize interval)
		  (list pagename-general optname-report-currency
			report-currency)
		  (list pagename-accounts optname-accounts
			(map car finish))
		  (list pagename-accounts optname-levels account-levels)
		  (list pagename-display optname-fullname show-fullname?)
		  (list pagename-display optname-stacked stacked?)
		  (list pagename-display optname-slices max-slices)
		  (list pagename-display optname-plot-height height) 
		  (list pagename-display optname-plot-width width)))
		(set! other-anchor
		      (gnc:report-anchor-text
		       (gnc:make-report name options))))))
	
	;; This adds the data. Note the apply-zip stuff: This
	;; transposes the data, i.e. swaps rows and columns. Pretty
	;; cool, eh? Courtesy of dave_p.
	(gnc:html-barchart-set-data! chart 
				     (apply zip (map cadr all-data)))

	;; Labels and colors
	(gnc:html-barchart-set-col-labels!
	 chart (map (lambda (pair)
		      (if (string? (car pair))
			  (car pair)
			  ((if show-fullname?
			       gnc:account-get-full-name
			       gnc:account-get-name) (car pair))))
		    all-data))
	(gnc:html-barchart-set-col-colors! 
	 chart
	 (gnc:assign-colors (length all-data)))

	(let ((urls (map (lambda (pair)
			   (if (string? (car pair))
			       other-anchor
			       (gnc:account-anchor-text (car pair))))
			 all-data)))
	  (gnc:html-barchart-set-button-1-bar-urls! chart urls)
	  (gnc:html-barchart-set-button-1-legend-urls! chart urls))

	(gnc:html-document-add-object! document chart) 
	document)))

  (for-each 
   (lambda (l)
     (gnc:define-report
      'version 1
      ;; why I use a variable here? See comment at the top.
      'name (car l)
      'options-generator (lambda () (options-generator (cadr l)))
      'renderer (lambda (report-obj)
		  (category-barchart-renderer report-obj 
					      (car l) 
					      (cadr l)
					      (caddr l)))))
   (list 
    (list reportname-income '(income) reporttitle-income)
    (list reportname-expense '(expense) reporttitle-expense)
    (list reportname-assets 
	  '(asset bank cash checking savings money-market 
		  stock mutual-fund currency)
	  reporttitle-assets)
    (list reportname-liabilities 
	  '(liability credit credit-line equity)
	  reporttitle-liabilities))))
