;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-income-statement.scm: income statement (a.k.a. Profit & Loss)
;; 
;; Copyright (c) the following:
;;
;;  Forest Bond <forest@alittletooquiet.net>
;;  David Montenegro <sunrise2000@comcast.net>
;;
;;  * BUGS:
;;    
;;    The Company Name field does not currently default to the name
;;    in (gnc-get-current-book).
;;    
;;    Line & column alignments may still not conform with
;;    textbook accounting practice (they're close though!).
;;    The 'canonically-tabbed option is currently broken.
;;    
;;    Progress bar functionality is currently mostly broken.
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    See also all the "FIXME"s in the code.
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

(define-module (gnucash report budget-income-statement))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report"))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual"))

(define optname-budget (N_ "Budget"))
(define opthelp-budget (N_ "Budget to use."))

;; FIXME this could use an indent option

(define optname-accounts (N_ "Accounts to include"))
(define opthelp-accounts
  (N_ "Report on these accounts, if display depth allows."))
(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed"))
(define optname-bottom-behavior (N_ "Flatten list to depth limit"))
(define opthelp-bottom-behavior
  (N_ "Displays accounts which exceed the depth limit at the depth limit"))

(define optname-parent-balance-mode (N_ "Parent account balances"))
(define optname-parent-total-mode (N_ "Parent account subtotals"))

(define optname-show-zb-accts (N_ "Include accounts with zero total balances"))
(define opthelp-show-zb-accts
  (N_ "Include accounts with zero total (recursive) balances in this report"))
(define optname-omit-zb-bals (N_ "Omit zero balance figures"))
(define opthelp-omit-zb-bals
  (N_ "Show blank space in place of any zero balances which would be shown"))

(define optname-use-rules (N_ "Show accounting-style rules"))
(define opthelp-use-rules
  (N_ "Use rules beneath columns of added numbers like accountants do"))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links (N_ "Shows each account in the table as a hyperlink to its register window"))

(define optname-label-revenue (N_ "Label the revenue section"))
(define opthelp-label-revenue
  (N_ "Whether or not to include a label for the revenue section"))
(define optname-total-revenue (N_ "Include revenue total"))
(define opthelp-total-revenue
  (N_ "Whether or not to include a line indicating total revenue"))
(define optname-label-expense (N_ "Label the expense section"))
(define opthelp-label-expense
  (N_ "Whether or not to include a label for the expense section"))
(define optname-total-expense (N_ "Include expense total"))
(define opthelp-total-expense
  (N_ "Whether or not to include a line indicating total expense"))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used"))

(define pagename-entries (N_ "Entries"))
(define optname-two-column
  (N_ "Display as a two column report"))
(define opthelp-two-column
  (N_ "Divides the report into an income column and an expense column"))
(define optname-standard-order
  (N_ "Display in standard, income first, order"))
(define opthelp-standard-order
  (N_ "Causes the report to display in the standard order, placing income before expenses"))

;; options generator
(define (budget-income-statement-options-generator-internal reportname)
  (let* ((options (gnc:new-options))
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    (add-option
      (gnc:make-string-option
      gnc:pagename-general optname-report-title
      "a" opthelp-report-title (_ reportname)))
    (add-option
      (gnc:make-string-option
      gnc:pagename-general optname-party-name
      "b" opthelp-party-name ""))
    ;; this should default to company name in (gnc-get-current-book)
    ;; does anyone know the function to get the company name??
    ;; (GnuCash is *so* well documented... sigh)

    (add-option
     (gnc:make-budget-option
      gnc:pagename-general optname-budget
      "c" opthelp-budget))
    
    ;; accounts to work on
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      opthelp-accounts
      (lambda ()
	(gnc:filter-accountlist-type
	 ;; select, by default, only income and expense accounts
	 (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
	 (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))
    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-depth-limit
     "b" opthelp-depth-limit 3)
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior #f))
    
    ;; all about currencies
    (gnc:options-add-currency!
     options pagename-commodities
     optname-report-commodity "a")
    
    (gnc:options-add-price-source! 
     options pagename-commodities
     optname-price-source "b" 'average-cost)
    
    (add-option 
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-foreign 
      "c" opthelp-show-foreign #t))
    
    (add-option 
     (gnc:make-simple-boolean-option
      pagename-commodities optname-show-rates
      "d" opthelp-show-rates #f))
    
    ;; what to show for zero-balance accounts
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-zb-accts
      "a" opthelp-show-zb-accts #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-omit-zb-bals
      "b" opthelp-omit-zb-bals #f))
    ;; what to show for non-leaf accounts
    (gnc:options-add-subtotal-view!
     options gnc:pagename-display
     optname-parent-balance-mode optname-parent-total-mode
     "c")

    ;; some detailed formatting options
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-account-links
      "d" opthelp-account-links #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-use-rules
      "e" opthelp-use-rules #f))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-revenue
      "f" opthelp-label-revenue #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-revenue
      "g" opthelp-total-revenue #t))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-expense
      "h" opthelp-label-expense #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-expense
      "i" opthelp-total-expense #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-two-column
      "j" opthelp-two-column #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-standard-order
      "k" opthelp-standard-order #t))
    
    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)
    
    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-income-statement-renderer
;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-income-statement-renderer-internal report-obj reportname)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))
  
  (define (get-assoc-account-balances-budget budget accountlist get-balance-fn)
    (gnc:get-assoc-account-balances
      accountlist
      (lambda (account) (get-balance-fn budget account))))

  (define (get-budget-account-budget-balance budget account)
    (gnc:budget-account-get-net budget account #f #f))

  (gnc:report-starting reportname)
  
  ;; get all option's values
  (let* (
	 (report-title (get-option gnc:pagename-general optname-report-title))
	 (company-name (get-option gnc:pagename-general optname-party-name))
         (budget (get-option gnc:pagename-general optname-budget))
         (budget-valid? (and budget (not (null? budget))))
         (date-tp (if budget-valid? (gnc:budget-get-start-date budget) #f))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))	 
	 (depth-limit (get-option gnc:pagename-accounts 
				  optname-depth-limit))
	 (bottom-behavior (get-option gnc:pagename-accounts 
				  optname-bottom-behavior))
         (report-commodity (get-option pagename-commodities
                                      optname-report-commodity))
         (price-source (get-option pagename-commodities
                                   optname-price-source))
         (show-fcur? (get-option pagename-commodities
                                 optname-show-foreign))
         (show-rates? (get-option pagename-commodities
                                  optname-show-rates))
         (parent-balance-mode (get-option gnc:pagename-display
                                           optname-parent-balance-mode))
         (parent-total-mode
	  (car
	   (assoc-ref '((t #t) (f #f) (canonically-tabbed canonically-tabbed))
		      (get-option gnc:pagename-display
				  optname-parent-total-mode))))
         (show-zb-accts? (get-option gnc:pagename-display
				     optname-show-zb-accts))
         (omit-zb-bals? (get-option gnc:pagename-display
				    optname-omit-zb-bals))
         (label-revenue? (get-option gnc:pagename-display
				    optname-label-revenue))
         (total-revenue? (get-option gnc:pagename-display
				    optname-total-revenue))
         (label-expense? (get-option gnc:pagename-display
				    optname-label-expense))
         (total-expense? (get-option gnc:pagename-display
				    optname-total-expense))
         (use-links? (get-option gnc:pagename-display
				     optname-account-links))
         (use-rules? (get-option gnc:pagename-display
				    optname-use-rules))
	 (two-column? (get-option gnc:pagename-display
				  optname-two-column))
	 (standard-order? (get-option gnc:pagename-display
				      optname-standard-order))
	 (indent 0)
	 (tabbing #f)
	 
         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
	 (revenue-accounts (assoc-ref split-up-accounts ACCT-TYPE-INCOME))
	 (expense-accounts (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE))
	 
         (doc (gnc:make-html-document))
	 ;; this can occasionally put extra (blank) columns in our
	 ;; table (when there is one account at the maximum depth and
	 ;; it has at least one of its ancestors deselected), but this
	 ;; is the only simple way to ensure that both tables
	 ;; (revenue, expense) have the same width.
         (tree-depth (if (equal? depth-limit 'all)
                         (gnc:get-current-account-tree-depth) 
			 depth-limit))
         ;; exchange rates calculation parameters
	 (exchange-fn
	  (gnc:case-exchange-fn price-source report-commodity date-tp))
	 )
    
    ;; Wrapper to call gnc:html-table-add-labeled-amount-line!
    ;; with the proper arguments.
    (define (add-subtotal-line table pos-label neg-label signed-balance)
      (define allow-same-column-totals #t)
      (let* ((neg? (and signed-balance
			neg-label
			(gnc-numeric-negative-p
			 (gnc:gnc-monetary-amount
			  (gnc:sum-collector-commodity
			   signed-balance report-commodity exchange-fn)))))
	     (label (if neg? (or neg-label pos-label) pos-label))
	     (balance (if neg?
			  (let ((bal (gnc:make-commodity-collector)))
			    (bal 'minusmerge signed-balance #f)
			    bal)
			  signed-balance))
	     )
	(gnc:html-table-add-labeled-amount-line!
	 table
	 (+ indent (* tree-depth 2)
	    (if (equal? tabbing 'canonically-tabbed) 1 0))
	 "primary-subheading"
	 (and (not allow-same-column-totals) balance use-rules?)
	 label indent 1 "total-label-cell"
	 (gnc:sum-collector-commodity balance report-commodity exchange-fn)
	 (+ indent (* tree-depth 2) (- 0 1)
	    (if (equal? tabbing 'canonically-tabbed) 1 0))
	 1 "total-number-cell")
	)
      )
    
    ;; wrapper around gnc:html-table-append-ruler!
    (define (add-rule table)
      (gnc:html-table-append-ruler!
       table
       (+ (* 2 tree-depth)
	  (if (equal? tabbing 'canonically-tabbed) 1 0))))
    
    (cond
      ((null? accounts)
        ;; No accounts selected.
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj))))
      ((not budget-valid?)
        ;; No budget selected.
        (gnc:html-document-add-object!
          doc (gnc:html-make-generic-budget-warning report-title)))
      (else (begin
        ;; Get all the balances for each of the account types.
        (let* (
               (revenue-account-balances #f)
               (expense-account-balances #f)

	       (revenue-total #f)
               (revenue-get-balance-fn #f)

	       (expense-total #f)
               (expense-get-balance-fn #f)

	       (net-income #f)
	       
               ;; Create the account tables below where their
               ;; percentage time can be tracked.
	       (inc-table (gnc:make-html-table)) ;; gnc:html-table
	       (exp-table (gnc:make-html-table))

	       (table-env #f)                      ;; parameters for :make-
	       (params #f)                         ;; and -add-account-
               (revenue-table #f)                  ;; gnc:html-acct-table
               (expense-table #f)                  ;; gnc:html-acct-table
               (budget-name (gnc-budget-get-name budget))
	       (period-for (string-append " " (_ "for Budget ") budget-name))
	       )

	  ;; a helper to add a line to our report
	  (define (report-line
		   table pos-label neg-label amount col
		   exchange-fn rule? row-style)
	    (let* ((neg? (and amount
			      neg-label
			      (gnc-numeric-negative-p
			       (gnc:gnc-monetary-amount
				(gnc:sum-collector-commodity
				 amount report-commodity exchange-fn)))))
		   (label (if neg? (or neg-label pos-label) pos-label))
		   (pos-bal (if neg?
				(let ((bal (gnc:make-commodity-collector)))
				  (bal 'minusmerge amount #f)
				  bal)
				amount))
		   (bal (gnc:sum-collector-commodity
			 pos-bal report-commodity exchange-fn))
		   (balance
		    (or (and (gnc:uniform-commodity? pos-bal report-commodity)
			     bal)
			(and show-fcur?
			     (gnc-commodity-table
			      pos-bal report-commodity exchange-fn))
			bal
			))
		   (column (or col 0))
		   )
	      (gnc:html-table-add-labeled-amount-line!
	       table (* 2 tree-depth)  row-style rule?
	       label                0  1 "text-cell"
	       bal          (+ col 1)  1 "number-cell")
	      )
	    )


	  (gnc:report-percent-done 5)


          ;; Pre-fetch expense account balances.
          (set! expense-account-balances
            (get-assoc-account-balances-budget
              budget
              expense-accounts
              get-budget-account-budget-balance))

          ;; Total expenses.
          (set! expense-total
            (gnc:get-assoc-account-balances-total expense-account-balances))

          ;; Function to get individual expense account total.
          (set! expense-get-balance-fn
            (lambda (account start-date end-date)
              (gnc:select-assoc-account-balance expense-account-balances account)))


	  (gnc:report-percent-done 10)


          ;; Pre-fetch revenue account balances.
          (set! revenue-account-balances
            (get-assoc-account-balances-budget
              budget
              revenue-accounts
              get-budget-account-budget-balance))

          ;; Total revenue.
          (set! revenue-total
            (gnc:get-assoc-account-balances-total revenue-account-balances))

          ;; Function to get individual revenue account total.
          ;; Budget revenue is always positive, so this must be negated.
          (set! revenue-get-balance-fn
            (lambda (account start-date end-date)
              (gnc:commodity-collector-get-negated
                (gnc:select-assoc-account-balance revenue-account-balances account))))


	  (gnc:report-percent-done 20)


	  ;; calculate net income
	  (set! net-income (gnc:make-commodity-collector))
	  (net-income 'merge revenue-total #f)
	  (net-income 'minusmerge expense-total #f)
	  

	  (gnc:report-percent-done 30)

          (gnc:html-document-set-title! 
            doc (sprintf #f "%s %s %s" company-name report-title budget-name))

	  (set! table-env
		(list
		 (list 'display-tree-depth tree-depth)
		 (list 'depth-limit-behavior (if bottom-behavior
						 'flatten
						 'summarize))
		 (list 'report-commodity report-commodity)
		 (list 'exchange-fn exchange-fn)
		 (list 'parent-account-subtotal-mode parent-total-mode)
		 (list 'zero-balance-mode (if show-zb-accts?
					      'show-leaf-acct
					      'omit-leaf-acct))
		 (list 'account-label-mode (if use-links?
					       'anchor
					       'name))
		 )
		)
	  (set! params
		(list
		 (list 'parent-account-balance-mode parent-balance-mode)
		 (list 'zero-balance-display-mode (if omit-zb-bals?
						      'omit-balance
						      'show-balance))
		 (list 'multicommodity-mode (if show-fcur? 'table #f))
		 (list 'rule-mode use-rules?)
		  )
		)
	  
	  ;; Workaround to force gtkhtml into displaying wide
	  ;; enough columns.
	  (let ((space
		 (make-list tree-depth "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
		 ))
	    (gnc:html-table-append-row! inc-table space)
	    (gnc:html-table-append-row! exp-table space))

	       
	  (gnc:report-percent-done 80)
	  (if label-revenue?
	      (add-subtotal-line inc-table (_ "Revenues") #f #f))
	  (set! revenue-table
		(gnc:make-html-acct-table/env/accts
                 (append table-env (list (list 'get-balance-fn revenue-get-balance-fn)))
                 revenue-accounts))
	  (gnc:html-table-add-account-balances
	   inc-table revenue-table params)
          (if total-revenue?
	      (add-subtotal-line 
	       inc-table (_ "Total Revenue") #f revenue-total))
	  
	  (gnc:report-percent-done 85)
	  (if label-expense?
	      (add-subtotal-line 
	       exp-table (_ "Expenses") #f #f))
	  (set! expense-table
		(gnc:make-html-acct-table/env/accts
                 (append table-env (list (list 'get-balance-fn expense-get-balance-fn)))
                 expense-accounts))
	  (gnc:html-table-add-account-balances
	   exp-table expense-table params)
	  (if total-expense?
	      (add-subtotal-line
	       exp-table (_ "Total Expenses") #f expense-total))
	  
	  (report-line
	   (if standard-order? 
	       exp-table 
	       inc-table)
	   (string-append (_ "Net income") period-for)
	   (string-append (_ "Net loss") period-for)
	   net-income
	   (* 2 (- tree-depth 1)) exchange-fn #f #f
	   )
	  
	  (gnc:html-document-add-object! 
	   doc 
	   (let* ((build-table (gnc:make-html-table)))
	     (if two-column?     
		 (gnc:html-table-append-row!
		  build-table
		  (if standard-order?
		      (list
		       (gnc:make-html-table-cell inc-table)
		       (gnc:make-html-table-cell exp-table)
		       )
		      (list
		       (gnc:make-html-table-cell exp-table)
		       (gnc:make-html-table-cell inc-table)
		       )
		      )
		  )
		 (if standard-order?
		     (begin
		       (gnc:html-table-append-row!
			build-table
			(list (gnc:make-html-table-cell inc-table)))
		       (gnc:html-table-append-row!
			build-table
			(list (gnc:make-html-table-cell exp-table)))
		       )
		     (begin
		       (gnc:html-table-append-row!
			build-table
			(list (gnc:make-html-table-cell exp-table)))
		       (gnc:html-table-append-row!
			build-table
			(list (gnc:make-html-table-cell inc-table)))
		       )
		     )
		 )
	     
	     (gnc:html-table-set-style!
	      build-table "td"
	      'attribute '("align" "left")
	      'attribute '("valign" "top"))
	     build-table
	     )
	   )
  
	  
	  
          ;; add currency information if requested
	  (gnc:report-percent-done 90)
          (if show-rates?
              (gnc:html-document-add-object! 
               doc ;;(gnc:html-markup-p)
               (gnc:html-make-exchangerates 
                report-commodity exchange-fn accounts)))
	  (gnc:report-percent-done 100)
	  
	  )
	))) ;; end cond
    
    (gnc:report-finished)
    
    doc))

(define is-reportname (N_ "Budget Income Statement"))
(define pnl-reportname (N_ "Budget Profit & Loss"))

(define (budget-income-statement-options-generator)
  (budget-income-statement-options-generator-internal is-reportname))
(define (budget-income-statement-renderer report-obj)
  (budget-income-statement-renderer-internal report-obj is-reportname))

(define (budget-profit-and-loss-options-generator)
  (budget-income-statement-options-generator-internal pnl-reportname))
(define (budget-profit-and-loss-renderer report-obj)
  (budget-income-statement-renderer-internal report-obj is-reportname))


(gnc:define-report 
 'version 1
 'name is-reportname
 'report-guid "583c313fcc484efc974c4c844404f454"
 'menu-path (list gnc:menuname-budget)
 'options-generator budget-income-statement-options-generator
 'renderer budget-income-statement-renderer
 )

;; Also make a "Profit & Loss" report, even if it's the exact same one,
;; just relabeled.
(gnc:define-report 
 'version 1
 'name pnl-reportname
 'report-guid "e5fa5ce805e840ecbeca4dba3fa4ead9"
 'menu-path (list gnc:menuname-budget)
 'options-generator budget-profit-and-loss-options-generator
 'renderer budget-profit-and-loss-renderer
 )

;; END
