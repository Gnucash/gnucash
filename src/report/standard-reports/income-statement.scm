;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; income-statement.scm: income statement (a.k.a. Profit & Loss)
;; 
;; By David Montenegro <sunrise2000@comcast.net>
;;  2004.07.13 - 2004.07.14
;;
;;  * BUGS:
;;    
;;    This code makes the assumption that you want your income
;;    statement to no more than daily resolution.
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

(define-module (gnucash report income-statement))
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

(define optname-start-date (N_ "Start Date"))
(define optname-end-date (N_ "End Date"))
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
(define optname-closing-pattern (N_ "Closing Entries pattern"))
(define opthelp-closing-pattern
  (N_ "Any text in the Description column which identifies closing entries"))
(define optname-closing-casing
  (N_ "Closing Entries pattern is case-sensitive"))
(define opthelp-closing-casing
  (N_ "Causes the Closing Entries Pattern match to be case-sensitive"))
(define optname-closing-regexp
  (N_ "Closing Entries Pattern is regular expression"))
(define opthelp-closing-regexp
  (N_ "Causes the Closing Entries Pattern to be treated as a regular expression"))
(define optname-two-column
  (N_ "Display as a two column report"))
(define opthelp-two-column
  (N_ "Divides the report into an income column and an expense column"))
(define optname-standard-order
  (N_ "Display in standard, income first, order"))
(define opthelp-standard-order
  (N_ "Causes the report to display in the standard order, placing income before expenses"))

;; options generator
(define (income-statement-options-generator-internal reportname)
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
    
    ;; period over which to report income
    (gnc:options-add-date-interval!
     options gnc:pagename-general 
     optname-start-date optname-end-date "c")
    
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
      "e" opthelp-account-links #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-use-rules
      "f" opthelp-use-rules #f))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-revenue
      "g" opthelp-label-revenue #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-revenue
      "h" opthelp-total-revenue #t))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-expense
      "i" opthelp-label-expense #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-expense
      "j" opthelp-total-expense #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-two-column
      "k" opthelp-two-column #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-standard-order
      "l" opthelp-standard-order #t))
    
    ;; closing entry match criteria
    ;; 
    ;; N.B.: transactions really should have a field where we can put
    ;; transaction types like "Adjusting/Closing/Correcting Entries"
    (add-option
      (gnc:make-string-option
      pagename-entries optname-closing-pattern
      "a" opthelp-closing-pattern (N_ "Closing Entries")))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-closing-casing
      "b" opthelp-closing-casing #f))
    (add-option
     (gnc:make-simple-boolean-option
      pagename-entries optname-closing-regexp
      "c" opthelp-closing-regexp #f))
    
    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)
    
    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; income-statement-renderer
;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (income-statement-renderer-internal report-obj reportname)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))
  
  (gnc:report-starting reportname)
  
  ;; get all option's values
  (let* (
	 (report-title (get-option gnc:pagename-general optname-report-title))
	 (company-name (get-option gnc:pagename-general optname-party-name))
         (start-date-printable (gnc:date-option-absolute-time
				(get-option gnc:pagename-general
					    optname-start-date)))
         (start-date-tp (gnc:timepair-start-day-time
			 (gnc:date-option-absolute-time
			  (get-option gnc:pagename-general
				      optname-start-date))))
         (end-date-tp (gnc:timepair-end-day-time
		       (gnc:date-option-absolute-time
			(get-option gnc:pagename-general
				    optname-end-date))))
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
	 (closing-str (get-option pagename-entries
				  optname-closing-pattern))
	 (closing-cased (get-option pagename-entries
				    optname-closing-casing))
	 (closing-regexp (get-option pagename-entries
				     optname-closing-regexp))
	 (two-column? (get-option gnc:pagename-display
				  optname-two-column))
	 (standard-order? (get-option gnc:pagename-display
				      optname-standard-order))
	 (closing-pattern
	  (list (list 'str closing-str)
		(list 'cased closing-cased)
		(list 'regexp closing-regexp)
		)
	  )
	 (indent 0)
	 (tabbing #f)
	 
         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
	 (revenue-accounts (assoc-ref split-up-accounts ACCT-TYPE-INCOME))
	 (expense-accounts (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE))
         (income-expense-accounts
          (append (assoc-ref split-up-accounts ACCT-TYPE-INCOME)
                  (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)))
	 
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
	  (gnc:case-exchange-fn price-source report-commodity end-date-tp))
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
    
    (gnc:html-document-set-title! 
     doc (sprintf #f
		  (string-append "%s %s "
				 (_ "For Period Covering %s to %s"))
		  company-name report-title
                  (gnc-print-date start-date-printable)
                  (gnc-print-date end-date-tp)))
    
    (if (null? accounts)
	
        ;; error condition: no accounts specified
	;; is this *really* necessary??
	;; i'd be fine with an all-zero P&L
	;; that would, technically, be correct....
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj)))
	
        ;; Get all the balances for each of the account types.
        (let* ((revenue-closing #f)
	       (expense-closing #f)
	       (neg-revenue-total #f)
	       (revenue-total #f)
	       (expense-total #f)
	       (net-income #f)
	       
               ;; Create the account tables below where their
               ;; percentage time can be tracked.
	       (inc-table (gnc:make-html-table)) ;; gnc:html-table
	       (exp-table (gnc:make-html-table))

	       (table-env #f)                      ;; parameters for :make-
	       (params #f)                         ;; and -add-account-
               (revenue-table #f)                  ;; gnc:html-acct-table
               (expense-table #f)                  ;; gnc:html-acct-table
	       
	       (terse-period? #t)
	       (period-for (if terse-period?
			       (string-append " " (_ "for Period"))
			       (sprintf #f (string-append ", " (_ "%s to %s"))
					(gnc-print-date start-date-printable)
					(gnc-print-date end-date-tp))
			       )
			   )
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
	  
	  ;; sum revenues and expenses
	  (set! revenue-closing
		(gnc:account-get-trans-type-balance-interval
		 revenue-accounts closing-pattern
		 start-date-tp end-date-tp)
		) ;; this is norm positive (debit)
	  (set! expense-closing
		(gnc:account-get-trans-type-balance-interval
		 expense-accounts closing-pattern
		 start-date-tp end-date-tp)
		) ;; this is norm negative (credit)
	  (set! expense-total
		(gnc:accountlist-get-comm-balance-interval
		 expense-accounts
		 start-date-tp end-date-tp))
	  (expense-total 'minusmerge expense-closing #f)
	  (set! neg-revenue-total
		(gnc:accountlist-get-comm-balance-interval
		 revenue-accounts
		 start-date-tp end-date-tp))
	  (neg-revenue-total 'minusmerge revenue-closing #f)
	  (set! revenue-total (gnc:make-commodity-collector))
	  (revenue-total 'minusmerge neg-revenue-total #f)
	  ;; calculate net income
	  (set! net-income (gnc:make-commodity-collector))
	  (net-income 'merge revenue-total #f)
	  (net-income 'minusmerge expense-total #f)
	  
	  (set! table-env
		(list
		 (list 'start-date start-date-tp)
		 (list 'end-date end-date-tp)
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
		 ;; we may, at some point, want to add an option to
		 ;; generate a pre-adjustment income statement...
		 (list 'balance-mode 'pre-closing)
		 (list 'closing-pattern closing-pattern)
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
		 table-env revenue-accounts))
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
		 table-env expense-accounts))
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
	)
    
    (gnc:report-finished)
    
    doc
    )
  )

(define is-reportname (N_ "Income Statement"))
(define pnl-reportname (N_ "Profit & Loss"))

(define (income-statement-options-generator)
  (income-statement-options-generator-internal is-reportname))
(define (income-statement-renderer report-obj)
  (income-statement-renderer-internal report-obj is-reportname))

(define (profit-and-loss-options-generator)
  (income-statement-options-generator-internal pnl-reportname))
(define (profit-and-loss-renderer report-obj)
  (income-statement-renderer-internal report-obj is-reportname))


(gnc:define-report 
 'version 1
 'name is-reportname
 'report-guid "0b81a3bdfd504aff849ec2e8630524bc"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator income-statement-options-generator
 'renderer income-statement-renderer
 )

;; Also make a "Profit & Loss" report, even if it's the exact same one,
;; just relabeled.
(gnc:define-report 
 'version 1
 'name pnl-reportname
 'report-guid "8758ba23984c40dea5527f5f0ca2779e"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator profit-and-loss-options-generator
 'renderer profit-and-loss-renderer
 )

;; END
