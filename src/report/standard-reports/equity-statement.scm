;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equity-statement.scm: statement of owner's equity (net worth)
;; 
;; By David Montenegro 2004.06.23 <sunrise2000@comcast.net>
;;  
;;  * Based on balance-sheet.scm by Robert Merkel <rgmerk@mira.net>
;;  
;;  * BUGS:
;;    
;;    The multicurrency support has NOT been tested and IS ALPHA.  I
;;    really don't if I used the correct exchange functions.  Search
;;    code for regexp "*exchange-fn".
;;    
;;    I have also made the educated assumption <grin> that a decrease
;;    in the value of a liability or equity also represents an
;;    unrealized loss.  I *think* that is right, but am not sure.
;;    
;;    This code makes the assumption that you want your equity
;;    statement to no more than daily resolution.
;;    
;;    The Accounts option panel needs a way to select (and select by
;;    default) capital and draw accounts. There really should be a
;;    contra account type or attribute....
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    See also any "FIXME"s in the code.
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

(define-module (gnucash report standard-reports equity-statement))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Equity Statement"))

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report"))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual"))

(define optname-start-date (N_ "Start Date"))
(define optname-end-date (N_ "End Date"))

(define optname-accounts (N_ "Accounts to include"))
(define opthelp-accounts
  (N_ "Report only on these accounts"))

(define optname-use-rules (N_ "Show accounting-style rules"))
(define opthelp-use-rules
  (N_ "Use rules beneath columns of added numbers like accountants do"))

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

;; options generator
(define (equity-statement-options-generator)
  (let* ((options (gnc:new-options))
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    (add-option
      (gnc:make-string-option
      (N_ "General") optname-report-title
      "a" opthelp-report-title (_ reportname)))
    (add-option
      (gnc:make-string-option
      (N_ "General") optname-party-name
      "b" opthelp-party-name ""))
    ;; this should default to company name in (gnc-get-current-book)
    ;; does anyone know the function to get the company name??
    ;; (GnuCash is *so* well documented... sigh)
    
    ;; date at which to report balance
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
         (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CREDIT
               ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY
               ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL ACCT-TYPE-CURRENCY
               ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE
               ACCT-TYPE-EQUITY ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))
    
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
    
    ;; some detailed formatting options
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-use-rules
      "f" opthelp-use-rules #f))
    
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
;; equity-statement-renderer
;; set up the document and add the table
;; then then return the document or, if
;; requested, export it to a file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (equity-statement-renderer report-obj choice filename)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))
  
  (gnc:report-starting reportname)
  
  ;; get all option's values
  (let* (
	 (report-title (get-option gnc:pagename-general optname-report-title))
	 (company-name (get-option gnc:pagename-general optname-party-name))
	 ;; this code makes the assumption that you want your equity
	 ;; statement to no more than daily resolution
         (start-date-printable (gnc:date-option-absolute-time
				(get-option gnc:pagename-general
					    optname-start-date)))
         (start-date-tp (gnc:timepair-end-day-time
			 (gnc:timepair-previous-day start-date-printable)))
         (end-date-tp (gnc:timepair-end-day-time 
		       (gnc:date-option-absolute-time
			(get-option gnc:pagename-general
				    optname-end-date))))
         ;;(end-date-printable (gnc:date-option-absolute-time
         ;;                      (get-option gnc:pagename-general
         ;;                                  optname-end-date)))
	 ;; why dont we use this?  why use any -printable at all?
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))	 
         (report-commodity (get-option pagename-commodities
                                      optname-report-commodity))
         (price-source (get-option pagename-commodities
                                   optname-price-source))
         (show-fcur? (get-option pagename-commodities
                                 optname-show-foreign))
         (show-rates? (get-option pagename-commodities
                                  optname-show-rates))
         (use-rules? (get-option gnc:pagename-display
				    optname-use-rules))
	 (closing-str (get-option pagename-entries
				  optname-closing-pattern))
	 (closing-cased (get-option pagename-entries
				    optname-closing-casing))
	 (closing-regexp (get-option pagename-entries
				     optname-closing-regexp))
	 
         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (asset-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-ASSET))
         (liability-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY))
         (income-expense-accounts
          (append (assoc-ref split-up-accounts ACCT-TYPE-INCOME)
                  (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)))
         (equity-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))

	 ;; N.B.: equity-accounts will also contain drawing accounts
	 ;; these must still be split-out and itemized separately
	 (capital-accounts #f)
	 (drawing-accounts #f)
	 (investments #f)
	 (withdrawals #f)
	 (net-investment #f)
	 (income-expense-closing #f)
	 (closing-pattern
	  (list (list 'str closing-str)
		(list 'cased closing-cased)
		(list 'regexp closing-regexp)
		(list 'positive #f)
		)
	  )
	 
         (doc (gnc:make-html-document))
         ;; exchange rates calculation parameters
	 (start-exchange-fn
	  (gnc:case-exchange-fn
	   price-source report-commodity start-date-tp))
	 (end-exchange-fn
	  (gnc:case-exchange-fn
	   price-source report-commodity end-date-tp))
	 )
    
    (gnc:html-document-set-title! 
     doc (sprintf #f
		  (string-append "%s %s "
				 (_ "For Period Covering %s to %s"))
		  company-name report-title
                  (gnc-print-date start-date-printable)
                  (gnc-print-date end-date-tp)))
    
    (if (null? accounts)
	
        ;; error condition: no accounts specified is this *really*
	;; necessary??  i'd be fine with an all-zero income statement
	;; that would, technically, be correct....
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj)))
	
        ;; Get all the balances for each account group.
        (let* ((book-balance #f) ;; assets - liabilities - equity, norm 0
	       (start-asset-balance #f)
	       (end-asset-balance #f)
	       (neg-start-liability-balance #f) ;; credit balances are < 0
	       (neg-end-liability-balance #f)
	       (neg-pre-start-retained-earnings #f)
	       (neg-pre-end-retained-earnings #f)
	       (neg-net-income #f)
	       (net-income #f)
	       
               (neg-start-equity-balance #f)
               (neg-end-equity-balance #f)
	       
	       ;; these variables wont be used until gnucash gets
	       ;; conta account types
               (start-capital-balance #f)
               (end-capital-balance #f)
               (start-drawing-balance #f)
               (end-drawing-balance #f)
	       
	       (start-book-balance #f)
	       (end-book-balance #f)
	       
	       (start-unrealized-gains #f)
	       (end-unrealized-gains #f)
	       (net-unrealized-gains #f)
	       
	       (equity-closing #f)
	       (neg-pre-closing-equity #f)
	       
	       (capital-increase #f)
	       
	       (start-total-equity #f)
	       (end-total-equity #f)
	       
	       ;; Create the account table below where its
	       ;; percentage time can be tracked.
	       (build-table (gnc:make-html-table)) ;; gnc:html-table
	       (get-start-balance-fn
		(lambda (account)
		  (gnc:account-get-comm-balance-at-date 
		   account start-date-tp #f)))
	       (get-end-balance-fn
		(lambda (account)
		  (gnc:account-get-comm-balance-at-date 
		   account end-date-tp #f)))
	       (terse-period? #t)
	       (period-for (if terse-period?
			       (string-append " " (_ "for Period"))
			       (sprintf #f (string-append ", " (_ "%s to %s"))
					(gnc-print-date start-date-printable)
					(gnc-print-date end-date-tp))
			       ))
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
	       table         3 row-style rule?
	       label         0         1 "text-cell"
	       bal   (+ col 1)         1 "number-cell")
	      )
	    )
	  
	  ;; sum any unrealized gains
	  ;; 
	  ;; Hm... unrealized gains....  This is when you purchase
	  ;; something and its value increases/decreases (prior to
	  ;; your selling it) and you have to reflect that on your
	  ;; balance sheet.
	  ;; 
	  ;; I *think* a decrease in the value of a liability or
	  ;; equity constitutes an unrealized loss.  I'm unsure about
	  ;; that though....
	  ;; 
	  (define (unrealized-gains-at-date book-balance exchange-fn date-tp)
	    (let* ((unrealized-gain-collector (gnc:make-commodity-collector))
		   (weighted-fn
		    (gnc:case-exchange-fn 'weighted-average
					  report-commodity date-tp))
		   
		   (value
		    (gnc:gnc-monetary-amount
		     (gnc:sum-collector-commodity book-balance
						  report-commodity
						  exchange-fn)))
		   
		   (cost
		    (gnc:gnc-monetary-amount
		     (gnc:sum-collector-commodity book-balance
						  report-commodity
						  weighted-fn)))
		   
		   (unrealized-gain (gnc-numeric-sub-fixed value cost)))
	      
	      (unrealized-gain-collector 'add report-commodity unrealized-gain)
	      unrealized-gain-collector
	      )
	    )
	  
	  ;; If you ask me, any outstanding(TM) retained earnings and
	  ;; unrealized gains should be added directly into equity,
	  ;; both at the start and end dates of the reporting period.
	  (gnc:report-percent-done 4)
	  
	  ;; start and end asset balances
	  (set! start-asset-balance 
                (gnc:accounts-get-comm-total-assets 
                 asset-accounts get-start-balance-fn)) ; OK
	  (set! end-asset-balance 
                (gnc:accounts-get-comm-total-assets 
                 asset-accounts get-end-balance-fn)) ; OK
	  
	  ;; start and end liability balances
	  (set! neg-start-liability-balance
                (gnc:accounts-get-comm-total-assets 
                 liability-accounts get-start-balance-fn)) ; OK
	  (set! neg-end-liability-balance
                (gnc:accounts-get-comm-total-assets 
                 liability-accounts get-end-balance-fn)) ; OK
	  
	  ;; start and end retained earnings (income - expenses)
	  (set! neg-pre-start-retained-earnings
		(gnc:accountlist-get-comm-balance-at-date
		 income-expense-accounts start-date-tp)) ; OK
	  (set! neg-pre-end-retained-earnings
		(gnc:accountlist-get-comm-balance-at-date
		 income-expense-accounts end-date-tp)) ; OK
	  ;; neg-pre-end-retained-earnings is not used to calculate
	  ;; profit but is used to calculate unrealized gains
	  
	  ;; calculate net income
	  ;; first, ask out how much profit/loss was closed
	  (set! income-expense-closing
		(gnc:account-get-trans-type-balance-interval
		 income-expense-accounts closing-pattern
		 start-date-tp end-date-tp)
		)
	  ;; find retained earnings for the period
	  (set! neg-net-income
		(gnc:accountlist-get-comm-balance-interval
		 income-expense-accounts
		 start-date-tp end-date-tp)) ; OK
	  ;; revert the income/expense to its pre-closing balance
	  (neg-net-income 'minusmerge income-expense-closing #f)
	  (set! net-income (gnc:make-commodity-collector))
	  (net-income 'minusmerge neg-net-income #f)
	  ;; now we know the net income for the period
	  
	  ;; start and end (unadjusted) equity balances
	  (set! neg-start-equity-balance
                (gnc:accounts-get-comm-total-assets 
                 equity-accounts get-start-balance-fn)) ; OK
	  (set! neg-end-equity-balance
                (gnc:accounts-get-comm-total-assets 
                 equity-accounts get-end-balance-fn)) ; OK
	  ;; neg-end-equity-balance is used to calculate unrealized
	  ;; gains and investments/withdrawals
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; 
	  ;; beleive it or not, i think this part is right...
	  ;; 
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
	  ;; start and end unrealized gains
	  (set! start-book-balance (gnc:make-commodity-collector))
	  (start-book-balance 'merge start-asset-balance #f)
	  (start-book-balance 'merge neg-start-liability-balance #f)
	  (start-book-balance 'merge neg-start-equity-balance #f)
	  (start-book-balance 'merge neg-pre-start-retained-earnings #f) ; OK
	  
	  (set! end-book-balance (gnc:make-commodity-collector))
	  (end-book-balance 'merge end-asset-balance #f)
	  (end-book-balance 'merge neg-end-liability-balance #f)
	  (end-book-balance 'merge neg-end-equity-balance #f)
	  (end-book-balance 'merge neg-pre-end-retained-earnings #f) ; OK
	  
	  (set! start-unrealized-gains
		(unrealized-gains-at-date start-book-balance
					  start-exchange-fn
					  start-date-tp)) ; OK
	  ;; I suspect that unrealized gains (since never realized)
	  ;; must be counted from forever-ago....
	  ;; ...yep, this appears to be correct.
	  (set! start-unrealized-gains (gnc:make-commodity-collector))
	  (set! end-unrealized-gains
		(unrealized-gains-at-date end-book-balance
					  end-exchange-fn
					  end-date-tp)) ; OK
	  
	  ;; unrealized gains accrued during the reporting period...
	  (set! net-unrealized-gains (gnc:make-commodity-collector))
	  (net-unrealized-gains 'merge end-unrealized-gains #f)
	  (net-unrealized-gains 'minusmerge start-unrealized-gains #f) ; OK
	  
	  ;; 
	  ;; calculate investments & draws...
	  ;; 
	  ;; since, as this time, GnuCash does not have any
	  ;; contra-account types, i'm gonna have to fudge this a
	  ;; bit...  i'll do a transaction query and classify the
	  ;; splits by debit/credit.
	  ;; 
	  ;;   withdrawals = investments - (investments - withdrawals)
	  ;;   investments = withdrawals + (investments - withdrawals)
	  ;; 
	  ;; assume that positive shares on an equity account are debits...
	  ;; 
	  
	  (set! equity-closing 
		(gnc:account-get-trans-type-balance-interval
		 equity-accounts closing-pattern
		 start-date-tp end-date-tp)
		)
	  (set! neg-pre-closing-equity (gnc:make-commodity-collector))
	  (neg-pre-closing-equity 'merge neg-end-equity-balance #f)
	  (neg-pre-closing-equity 'minusmerge equity-closing #f)
	  
	  (set! net-investment (gnc:make-commodity-collector))  ;; 0
	  (net-investment 'minusmerge neg-pre-closing-equity #f);; > 0
	  (net-investment 'merge neg-start-equity-balance #f)   ;; net increase
	  
	  (set! withdrawals (gnc:make-commodity-collector))
	  (withdrawals 'merge (gnc:account-get-pos-trans-total-interval
				    equity-accounts closing-pattern
				    start-date-tp end-date-tp)
		       #f)
	  (set! investments (gnc:make-commodity-collector))
	  (investments 'merge net-investment #f)
	  (investments 'merge withdrawals #f)
	  
	  ;; increase in equity
	  (set! capital-increase (gnc:make-commodity-collector))
	  (capital-increase 'merge net-income #f)
	  (capital-increase 'merge investments #f)
	  (capital-increase 'minusmerge withdrawals #f)
	  (capital-increase 'merge net-unrealized-gains #f)
	  
	  ;; starting total equity
	  (set! start-total-equity (gnc:make-commodity-collector))
	  (start-total-equity 'minusmerge neg-start-equity-balance #f)
	  (start-total-equity 'minusmerge neg-pre-start-retained-earnings #f)
	  (start-total-equity 'merge start-unrealized-gains #f) ; OK
	  
	  ;; ending total equity
	  (set! end-total-equity (gnc:make-commodity-collector))
	  (end-total-equity 'merge start-total-equity #f)
	  (end-total-equity 'merge capital-increase #f) ; OK
	  
	  (gnc:report-percent-done 30)
	  
	  ;; Workaround to force gtkhtml into displaying wide
	  ;; enough columns.
	  (gnc:html-table-append-row!
	   build-table
	   (make-list 2 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
	   )
	  
	  (gnc:report-percent-done 80)
	  
	  (report-line
	   build-table
	   (string-append (_ "Capital") ", "
			  (gnc-print-date start-date-printable))
	   #f start-total-equity
	   1 start-exchange-fn #f "primary-subheading"
	   )
	  (report-line
	   build-table 
	   (string-append (_ "Net income") period-for)
	   (string-append (_ "Net loss") period-for)
	   net-income
	   0 end-exchange-fn #f #f
	   )
	  (report-line
	   build-table 
	   (string-append (_ "Investments") period-for)
	   #f
	   investments
	   0 end-exchange-fn #f #f
	   )
	  (report-line
	   build-table 
	   (string-append (_ "Withdrawals") period-for)
	   #f
	   withdrawals
	   0 end-exchange-fn #f #f
	   )
	  (or (gnc-commodity-collector-allzero? net-unrealized-gains)
	      (report-line
	       build-table 
	       (_ "Unrealized Gains")
	       (_ "Unrealized Losses")
	       net-unrealized-gains
	       0 end-exchange-fn #f #f
	       )
	   )
	  (report-line
	   build-table 
	   (_ "Increase in capital")
	   (_ "Decrease in capital")
	   capital-increase
	   1 end-exchange-fn use-rules? #f
	   )
	  (report-line
	   build-table 
	   (string-append (_ "Capital") ", "
			  (gnc-print-date end-date-tp))
	   #f
	   end-total-equity
	   1 end-exchange-fn #f "primary-subheading"
	   )
	  
	  (gnc:html-document-add-object! doc build-table)
	  
          ;; add currency information if requested
	  (gnc:report-percent-done 90)
          (and show-rates?
	       (let* ((curr-tbl (gnc:make-html-table))
		      (headers (list
				(gnc-print-date start-date-printable)
				(gnc-print-date end-date-tp)
				)
			       )
		      (then (gnc:html-make-exchangerates
			     report-commodity start-exchange-fn accounts))
		      (now (gnc:html-make-exchangerates 
			    report-commodity end-exchange-fn accounts))
		      )
		 
		 (gnc:html-table-set-col-headers! curr-tbl headers)
		 (gnc:html-table-set-style!
		  curr-tbl "table" 'attribute '("border" "1"))
		 (gnc:html-table-set-style!
		  then "table" 'attribute '("border" "0"))
		 (gnc:html-table-set-style!
		  now "table" 'attribute '("border" "0"))
		 (gnc:html-table-append-ruler! build-table 3)
		 (gnc:html-table-append-row! curr-tbl (list then now))
		 (gnc:html-document-add-object! doc curr-tbl)
		 )
	       )
	  
	  (gnc:report-percent-done 100)
	  
	  ;; if sending the report to a file, do so now
	  ;; however, this still doesn't seem to get around the
	  ;; colspan bug... cf. gnc:colspans-are-working-right
	  (if filename
	      (let* ((port (open-output-file filename))
		     (gnc:display-report-list-item
		      (list doc) port " equity-statement.scm ")
		     (close-output-port port)
		     )
		)
	      )
	  )
	)
    
    (gnc:report-finished)
    
    doc
    )
  )

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "c2a996c8970f43448654ca84f17dda24"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator equity-statement-options-generator
 'renderer (lambda (report-obj)
	     (equity-statement-renderer report-obj #f #f))
 'export-types #f
 'export-thunk (lambda (report-obj choice filename)
		 (equity-statement-renderer report-obj #f filename)))

;; END

