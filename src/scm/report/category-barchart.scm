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
(let ((menuname-income (N_ "Income Barchart"))
      (menuname-expense (N_ "Expense Barchart"))
      (menuname-assets (N_ "Asset Barchart"))
      (menuname-liabilities (N_ "Liability Barchart"))
      ;; The names are used in the menu

      ;; The menu statusbar tips.
      (menutip-income 
       (N_ "Shows a barchart with the Income per interval \
developing over time"))
      (menutip-expense 
       (N_ "Shows a barchart with the Expenses per interval \
developing over time"))
      (menutip-assets 
       (N_ "Shows a barchart with the Assets developing over time"))
      (menutip-liabilities 
       (N_ "Shows a barchart with the Liabilities \
developing over time"))

      ;; The names here are used 1. for internal identification, 2. as
      ;; tab labels, 3. as default for the 'Report name' option which
      ;; in turn is used for the printed report title.
      (reportname-income (N_ "Income Over Time"))
      (reportname-expense (N_ "Expense Over Time"))
      (reportname-assets (N_ "Assets Over Time"))
      (reportname-liabilities (N_ "Liabilities Over Time"))

      ;; Option names
      (optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-stepsize (N_ "Step Size"))
      (optname-report-currency (N_ "Report's currency"))
      (optname-price-source (N_ "Price Source"))

      (optname-accounts (N_ "Accounts"))
      (optname-levels (N_ "Show Accounts until level"))

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
       options gnc:pagename-general
       optname-from-date optname-to-date "a")

      (gnc:options-add-interval-choice! 
       options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

      (gnc:options-add-currency! 
       options gnc:pagename-general optname-report-currency "c")

      (gnc:options-add-price-source! 
       options gnc:pagename-general
       optname-price-source "d" 'pricedb-latest)

      ;; Accounts tab
      (add-option
       (gnc:make-account-list-option
	gnc:pagename-accounts optname-accounts
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
       options gnc:pagename-accounts optname-levels "c" 
       (N_ "Show accounts to this depth and not further") 
       2)

      ;; Display tab
      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display optname-fullname
        "a" (N_ "Show the full account name in legend?") #f))

      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display optname-stacked
        "b" 
	(N_ "Show barchart as stacked barchart? (Guppi>=0.35.4 required)") 
	#t))

      (add-option
       (gnc:make-number-range-option
        gnc:pagename-display optname-slices
        "c" (N_ "Maximum number of bars in the chart") 8
        2 24 0 1))

      (gnc:options-add-plot-size! 
       options gnc:pagename-display 
       optname-plot-width optname-plot-height "c" 400 400)

      (gnc:options-set-default-section options gnc:pagename-general)

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
				      account-types do-intervals?)
    ;; A helper functions for looking up option values.
    (define (get-option section name)
      (gnc:option-value 
       (gnc:lookup-option 
	(gnc:report-options report-obj) section name)))
    
    (let ((to-date-tp (gnc:timepair-end-day-time 
		       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general 
				    optname-to-date))))
	  (from-date-tp (gnc:timepair-start-day-time 
			 (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general 
				      optname-from-date))))
	  (interval (get-option gnc:pagename-general optname-stepsize))
	  (report-currency (get-option gnc:pagename-general
				       optname-report-currency))
	  (price-source (get-option gnc:pagename-general
				    optname-price-source))
	  (report-title (get-option gnc:pagename-general 
				    gnc:optname-reportname))

	  (accounts (get-option gnc:pagename-accounts optname-accounts))
	  (account-levels (get-option gnc:pagename-accounts optname-levels))
	  
	  (stacked? (get-option gnc:pagename-display optname-stacked))
	  (show-fullname? (get-option gnc:pagename-display optname-fullname))
	  (max-slices (get-option gnc:pagename-display optname-slices))
	  (height (get-option gnc:pagename-display optname-plot-height))
	  (width (get-option gnc:pagename-display optname-plot-width))
	  
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

      (gnc:debug accounts)
      (if (not (null? accounts))
	  
	  ;; Define more helper variables.
	  
	  (let* ((commodity-list (gnc:accounts-get-commodities 
				  (append 
				   (gnc:acccounts-get-all-subaccounts accounts)
				   accounts)
				  report-currency))
		 (exchange-fn (gnc:case-exchange-time-fn 
			       price-source report-currency 
			       commodity-list to-date-tp))
		 (tree-depth (if (equal? account-levels 'all)
				 (gnc:get-current-group-depth)
				 account-levels))
		 ;; This is the list of date intervals to calculate.
		 (dates-list (if do-intervals?
				 (gnc:make-date-interval-list
				  (gnc:timepair-start-day-time from-date-tp) 
				  (gnc:timepair-end-day-time to-date-tp)
				  (eval interval))
				 (gnc:make-date-list
				  (gnc:timepair-end-day-time from-date-tp) 
				  (gnc:timepair-end-day-time to-date-tp)
				  (eval interval))))
		 ;; Here the date strings for the x-axis labels are
		 ;; created.
		 (date-string-list
		  (map (lambda (date-list-item)
			 (gnc:timepair-to-datestring
			  (if do-intervals?
			      (car date-list-item)
			      date-list-item)))
		       dates-list))
		 (other-anchor "")
		 (all-data '()))
	    
	    ;; Converts a commodity-collector into one single double
	    ;; number, depending on the report currency and the
	    ;; exchange-fn calculated above. Returns a double.
	    (define (collector->double c date)
	      ;; Future improvement: Let the user choose which kind of
	      ;; currency combining she want to be done. 
	      (gnc:numeric-to-double 
	       (gnc:gnc-monetary-amount
		(gnc:sum-collector-commodity 
		 c report-currency 
		 (lambda (a b) (exchange-fn a b date))))))
	    
	    ;; Calculates the net balance (profit or loss) of an account in
	    ;; the given time interval. date-list-entry is a pair containing
	    ;; the start- and end-date of that interval. If subacct?==#t,
	    ;; the subaccount's balances are included as well. Returns a
	    ;; double, exchanged into the report-currency by the above
	    ;; conversion function, and possibly with reversed sign.
	    (define (get-balance account date-list-entry subacct?)
	      ((if (gnc:account-reverse-balance? account)
		   - +)
		(if do-intervals?
		    (collector->double
		     (gnc:account-get-comm-balance-interval 
		      account 
		      (first date-list-entry) 
		      (second date-list-entry) subacct?)
		     (second date-list-entry))
		    (collector->double
		     (gnc:account-get-comm-balance-at-date
		      account date-list-entry subacct?)
		     date-list-entry))))
	    
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
	    
	    
	    ;;(gnc:warn "all-data" all-data)

	    ;; Proceed if the data is non-zeros
	    (if 
	     (and (not (null? all-data))
		  (gnc:not-all-zeros (map cadr all-data)))
	     (begin 
	       ;; Set chart title, subtitle etc.
	       (gnc:html-barchart-set-title! chart report-title)
	       (gnc:html-barchart-set-subtitle!
		chart (sprintf #f
			       (if do-intervals?
				   (_ "%s to %s")
				   (_ "Balances %s to %s"))
			       (gnc:timepair-to-datestring from-date-tp) 
			       (gnc:timepair-to-datestring to-date-tp)))
	       (gnc:html-barchart-set-width! chart width)
	       (gnc:html-barchart-set-height! chart height)
	       
	       ;; row labels etc.
	       (gnc:html-barchart-set-row-labels! chart date-string-list)
	       ;; FIXME: axis labels are not yet supported by
	       ;; libguppitank.
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
		     (let* ((options (gnc:make-report-options reportname))
			    (id #f))
		       ;; now copy all the options
		       (gnc:options-copy-values 
			(gnc:report-options report-obj) options)
		       ;; and set the destination accounts
		       (gnc:option-set-value
			(gnc:lookup-option options gnc:pagename-accounts 
					   optname-accounts)
			(map car finish))
		       ;; Set the URL to point to this report.
		       (set! id (gnc:make-report reportname options))
		       (set! other-anchor (gnc:report-anchor-text id)))))
	       
	       
	       ;; This adds the data. Note the apply-zip stuff: This
	       ;; transposes the data, i.e. swaps rows and columns. Pretty
	       ;; cool, eh? Courtesy of dave_p.
	       (if (not (null? all-data))
		   (gnc:html-barchart-set-data! 
		    chart 
		    (apply zip (map cadr all-data))))
	       
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
	       
	       ;; set the URLs; the slices are links to other reports
	       (let 
		   ((urls
		     (map 
		      (lambda (pair)
			(if 
			 (string? (car pair))
			 other-anchor
			 (let* ((acct (car pair))
				(subaccts 
				 (gnc:account-get-immediate-subaccounts acct)))
			   (if (null? subaccts)
			       ;; if leaf-account, make this an anchor
			       ;; to the register.
			       (gnc:account-anchor-text acct)
			       ;; if non-leaf account, make this a link
			       ;; to another report which is run on the
			       ;; immediate subaccounts of this account
			       ;; (and including this account).
			       (gnc:make-report-anchor
				reportname
				report-obj
				(list
				 (list gnc:pagename-accounts optname-accounts
				       (cons acct subaccts))
				 (list gnc:pagename-accounts optname-levels
				       (+ 1 tree-depth))
				 (list gnc:pagename-general 
				       gnc:optname-reportname
				       ((if show-fullname?
					    gnc:account-get-full-name
					    gnc:account-get-name) acct))))))))
		      all-data)))
		 (gnc:html-barchart-set-button-1-bar-urls! 
		  chart (append urls urls))
		 ;; The legend urls do the same thing.
		 (gnc:html-barchart-set-button-1-legend-urls! 
		  chart (append urls urls)))
	       
	       (gnc:html-document-add-object! document chart) 
	       
	       (if (gnc:option-value 
		    (gnc:lookup-global-option "General" 
					      "Display \"Tip of the Day\""))
		   (gnc:html-document-add-object! 
		    document 
		    (gnc:make-html-text 
		     (gnc:html-markup-p 
		      "If you don't see a stacked barchart i.e. you only see \
lots of thin bars next to each other for each date, then you \
should upgrade Guppi to version 0.35.5.")
		     (gnc:html-markup-p
		      "Double-click on any legend box or any bar opens \
another barchart report with the subaccounts of that account or, \
if that account doesn't have subaccounts, the register for the account.")
		     (gnc:html-markup-p "Remove this text by disabling \
the global Preference \"Display Tip of the Day\".")))))

	     ;; else if empty data
	     (gnc:html-document-add-object!
	      document
	      (gnc:html-make-empty-data-warning))))
	  
	  ;; else if no accounts selected
	  (gnc:html-document-add-object! 
	   document 
	   (gnc:html-make-no-account-warning)))
      
      document))
	  
  (for-each 
   (lambda (l)
     (gnc:define-report
      'version 1
      'name (car l)
      'menu-path (if (caddr l)
                     (list gnc:menuname-income-expense)
                     (list gnc:menuname-asset-liability))
      'menu-name (cadddr l)
      'menu-tip (car (cddddr l))
      'options-generator (lambda () (options-generator (cadr l)))
      'renderer (lambda (report-obj)
		  (category-barchart-renderer report-obj 
				     (car l) 
				     (cadr l)
				     (caddr l)))))
   (list 
    ;; reportname, account-types, do-intervals?, 
    ;; menu-reportname, menu-tip
    (list reportname-income '(income) #t menuname-income menutip-income)
    (list reportname-expense '(expense) #t menuname-expense menutip-expense)
    (list reportname-assets 
	  '(asset bank cash checking savings money-market 
		  stock mutual-fund currency)
	  #f menuname-assets menutip-assets)
    (list reportname-liabilities 
	  '(liability credit credit-line)
	  #f menuname-liabilities menutip-liabilities))))
