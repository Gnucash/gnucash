;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-piecharts.scm: shows piechart of accounts
;;  
;; By Robert Merkel (rgmerk@mira.net) 
;; and Christian Stimming <stimming@tu-harburg.de>
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

(gnc:support "report/account-piecharts.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ((menuname-income (N_ "Income Piechart"))
      (menuname-expense (N_ "Expense Piechart"))
      (menuname-assets (N_ "Asset Piechart"))
      (menuname-liabilities (N_ "Liability Piechart"))
      ;; The names are used in the menu

      ;; The menu statusbar tips.
      (menutip-income 
       (N_ "Shows a piechart with the Income per given time interval"))
      (menutip-expense 
       (N_ "Shows a piechart with the Expenses per given time interval"))
      (menutip-assets 
       (N_ "Shows a piechart with the Assets balance at a given time"))
      (menutip-liabilities 
       (N_ "Shows a piechart with the Liabilities and Equity \
balance at a given time"))

      ;; The names here are used 1. for internal identification, 2. as
      ;; tab labels, 3. as default for the 'Report name' option which
      ;; in turn is used for the printed report title.
      (reportname-income (N_ "Income Accounts"))
      (reportname-expense (N_ "Expense Accounts"))
      (reportname-assets (N_ "Assets"))
      (reportname-liabilities (N_ "Liabilities/Equity"))

      (optname-from-date (N_ "From"))
      (optname-to-date (N_ "To"))
      (optname-report-currency (N_ "Report's currency"))

      (optname-accounts (N_ "Accounts"))
      (optname-levels (N_ "Show Accounts until level"))

      (optname-fullname (N_ "Show long account names"))
      (optname-show-total (N_ "Show Totals"))
      (optname-slices (N_ "Maximum Slices"))
      (optname-plot-width (N_ "Plot Width"))
      (optname-plot-height (N_ "Plot Height")))

  ;; The option-generator. The only dependance on the type of piechart
  ;; is the list of account types that the account selection option
  ;; accepts.
  (define (options-generator account-types do-intervals?)
    (let* ((options (gnc:new-options)) 
           (add-option 
            (lambda (new-option)
              (gnc:register-option options new-option))))

      (if do-intervals?
	  (gnc:options-add-date-interval!
	   options gnc:pagename-general
	   optname-from-date optname-to-date "a")
	  (gnc:options-add-report-date!
	   options gnc:pagename-general
	   optname-to-date "a"))

      (gnc:options-add-currency! 
       options gnc:pagename-general optname-report-currency "b")
      
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
		(gnc:filter-accountlist-type
		 account-types
		 accounts)))
	#t))

      (gnc:options-add-account-levels! 
       options gnc:pagename-accounts optname-levels "b" 
       (N_ "Show accounts to this depth and not further") 
       2)

      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display optname-fullname
        "a" (N_ "Show the full account name in legend?") #f))

      (add-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display optname-show-total
        "b" (N_ "Show the total balance in legend?") #t))

      (add-option
       (gnc:make-number-range-option
        gnc:pagename-display optname-slices
        "c" (N_ "Maximum number of slices in pie") 7
        2 24 0 1))

      (gnc:options-add-plot-size!
       options gnc:pagename-display 
       optname-plot-width optname-plot-height "d" 500 250)

      (gnc:options-set-default-section options gnc:pagename-general)      

      options))


  ;; The rendering function. Since it works for a bunch of different
  ;; account settings, you have to give the reportname, the
  ;; account-types to work on and whether this report works on
  ;; intervals as arguments.
  (define (piechart-renderer report-obj reportname
			     account-types do-intervals?)
    
    ;; This is a helper function for looking up option values.
    (define (op-value section name)
      (gnc:option-value 
       (gnc:lookup-option 
	(gnc:report-options report-obj) section name)))
    
    ;; Get all options
    (let ((to-date-tp (gnc:timepair-end-day-time 
		       (gnc:date-option-absolute-time
                        (op-value gnc:pagename-general optname-to-date))))
	  (from-date-tp (if do-intervals?
			    (gnc:timepair-start-day-time 
			     (gnc:date-option-absolute-time 
			      (op-value gnc:pagename-general 
					optname-from-date)))
			    '()))
	  (accounts (op-value gnc:pagename-accounts optname-accounts))
	  (account-levels (op-value gnc:pagename-accounts optname-levels))
	  (report-currency (op-value gnc:pagename-general
				     optname-report-currency))
	  (report-title (op-value gnc:pagename-general 
				  gnc:optname-reportname))

	  (show-fullname? (op-value gnc:pagename-display optname-fullname))
	  (show-total? (op-value gnc:pagename-display optname-show-total))
	  (max-slices (op-value gnc:pagename-display optname-slices))
	  (height (op-value gnc:pagename-display optname-plot-height))
	  (width (op-value gnc:pagename-display optname-plot-width))

	  (document (gnc:make-html-document))
	  (chart (gnc:make-html-piechart))
	  (topl-accounts (gnc:filter-accountlist-type 
			  account-types
			  (gnc:group-get-account-list 
			   (gnc:get-current-group)))))

      ;; Returns true if the account a was selected in the account
      ;; selection option.
      (define (show-acct? a)
	(member a accounts))
      
      ;; Calculates the net balance (profit or loss) of an account
      ;; over the selected reporting period. If subaccts? == #t, all
      ;; subaccount's balances are included as well. Returns a
      ;; commodity-collector.
      (define (profit-fn account subaccts?)
	(if do-intervals?
	    (gnc:account-get-comm-balance-interval
	     account from-date-tp to-date-tp subaccts?)
	    (gnc:account-get-comm-balance-at-date
	     account to-date-tp subaccts?)))

      ;; Define more helper variables.
      (let* ((exchange-alist (gnc:make-exchange-alist
			      report-currency to-date-tp))
	     (exchange-fn-internal 
	      (gnc:make-exchange-function exchange-alist))
	     (tree-depth (if (equal? account-levels 'all)
			     (gnc:get-current-group-depth)
			     account-levels))
	     (combined '())
	     (other-anchor "")
	     (print-info (gnc:commodity-print-info report-currency #t)))

	;; Converts a commodity-collector into one single double
	;; number, depending on the report currency and the
	;; exchange-alist calculated above. Returns the absolute value
	;; as double.
	(define (collector->double c)
	  ;; Future improvement: Let the user choose which kind of
	  ;; currency combining she want to be done. Right now
	  ;; everything foreign gets converted
	  ;; (gnc:sum-collector-commodity) based on the weighted
	  ;; average of all past transactions.
	  (abs (gnc:numeric-to-double 
		(gnc:gnc-monetary-amount
		 (gnc:sum-collector-commodity 
		  c report-currency 
		  exchange-fn-internal)))))

	;; Calculates all account's balances. Returns a list of
	;; balance <=> account pairs, like '((10.0 Earnings) (142.5
	;; Gifts)). If current-depth >= tree-depth, then the balances
	;; are calculated *with* subaccount's balances. Else only the
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
			 (set! res (cons (list (collector->double 
						(profit-fn a #f)) a)
					 res)))
		     (set! res (append
				(traverse-accounts
				 (+ 1 current-depth)
				 (gnc:account-get-immediate-subaccounts a))
				res))))
		 accts)
		res)
	      (map
	       (lambda (a)
		 (list (collector->double (profit-fn a #t)) a))
	       (filter show-acct? accts))))
	
	;; Now do the work here.
	(set! combined
	      (sort (filter (lambda (pair) (not (= 0.0 (car pair))))
			    (traverse-accounts 
			     1 topl-accounts))
		    (lambda (a b) (> (car a) (car b)))))

	;; if too many slices, condense them to an 'other' slice
	;; and add a link to a new pie report with just those
	;; accounts
	(if (> (length combined) max-slices)
	    (let* ((start (take combined (- max-slices 1)))
		   (finish (drop combined (- max-slices 1)))
		   (sum (apply + (unzip1 finish))))
	      (set! combined
		    (append start
			    (list (list sum (_ "Other")))))
	      (let ((options (gnc:make-report-options reportname))
                    (id #f))
		;; now copy all the options
		(gnc:options-copy-values (gnc:report-options report-obj)
					 options)
		;; and set the destination accounts
		(gnc:option-set-value
		 (gnc:lookup-option options gnc:pagename-accounts 
				    optname-accounts)
		 (map cadr finish))
                (set! id (gnc:make-report reportname options))
                (gnc:report-add-child-by-id! report-obj id)
                (gnc:report-set-parent! (gnc:find-report id) report-obj)
                
		;; set the URL.
		(set! other-anchor (gnc:report-anchor-text id)))))

	;; set the URLs; the slices are links to other reports
	(let 
	    ((urls
	      (map 
	       (lambda (pair)
		 (if (string? (cadr pair))
		     other-anchor
		     (let* ((acct (cadr pair))
			    (subaccts 
			     (gnc:account-get-immediate-subaccounts acct)))
		       (if (null? subaccts)
			   ;; if leaf-account, make this an anchor
			   ;; to the register.
			   (gnc:account-anchor-text (cadr pair))
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
	       combined)))
	  (gnc:html-piechart-set-button-1-slice-urls! 
	   chart urls)
	  (gnc:html-piechart-set-button-1-legend-urls! 
	   chart urls))
	
	(gnc:html-piechart-set-title!
	 chart report-title)
	(gnc:html-piechart-set-width! chart width)
	(gnc:html-piechart-set-height! chart height)
	(gnc:html-piechart-set-data! chart (unzip1 combined))
	(gnc:html-piechart-set-colors! chart
				       (gnc:assign-colors (length combined)))

	(gnc:html-piechart-set-subtitle!
	 chart (string-append
		(if do-intervals?
		    (sprintf #f
			     (_ "%s to %s")
			     (gnc:timepair-to-datestring from-date-tp) 
			     (gnc:timepair-to-datestring to-date-tp))
		    (sprintf #f
			     (_ "Balance at %s")
			     (gnc:timepair-to-datestring to-date-tp)))
		(if show-total?
		    (let ((total (apply + (unzip1 combined))))
		      (sprintf #f ": %s"
			       (gnc:amount->string total print-info)))
		    
		    "")))

	(let ((legend-labels
	       (map 
		(lambda (pair)
		  (string-append
		   (if (string? (cadr pair))
		       (cadr pair)
		       ((if show-fullname?
			    gnc:account-get-full-name
			    gnc:account-get-name) (cadr pair)))
		   (if show-total?
		       (string-append 
			" - "
			(gnc:amount->string (car pair) print-info))
		       "")))
		combined)))
	  (gnc:html-piechart-set-labels! chart legend-labels))
	
	(gnc:html-document-add-object! document chart) 

	(if (gnc:option-value 
	     (gnc:lookup-global-option "General" 
				       "Display \"Tip of the Day\""))
	    (gnc:html-document-add-object! 
	     document 
	     (gnc:make-html-text 
	      (gnc:html-markup-p 
	       "Double-click on any legend box or pie slice opens either the \
register or, if the account has subaccounts, opens \
another piechart report with precisely those subaccounts.")
	      (gnc:html-markup-p "Dragging with left button \
lets you drag single slices out of the pie. \
Dragging with right button lets you rotate the pie. ")
	      (gnc:html-markup-p "Remove this text by disabling \
the global Preference \"Display Tip of the Day\"."))))
	
	document)))
  
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
      'options-generator (lambda () (options-generator (cadr l) 
						       (caddr l)))
      'renderer (lambda (report-obj)
		  (piechart-renderer report-obj 
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
	  '(liability credit credit-line equity)
	  #f menuname-liabilities menutip-liabilities))))
