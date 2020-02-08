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
(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Equity Statement"))

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual."))

(define optname-start-date (N_ "Start Date"))
(define optname-end-date (N_ "End Date"))

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts
  (N_ "Report only on these accounts."))

(define optname-use-rules (N_ "Show accounting-style rules"))
(define opthelp-use-rules
  (N_ "Use rules beneath columns of added numbers like accountants do."))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account."))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))

(define pagename-entries (N_ "Entries"))
(define optname-closing-pattern (N_ "Closing Entries pattern"))
(define opthelp-closing-pattern
  (N_ "Any text in the Description column which identifies closing entries."))
(define optname-closing-casing
  (N_ "Closing Entries pattern is case-sensitive"))
(define opthelp-closing-casing
  (N_ "Causes the Closing Entries Pattern match to be case-sensitive."))
(define optname-closing-regexp
  (N_ "Closing Entries Pattern is regular expression"))
(define opthelp-closing-regexp
  (N_ "Causes the Closing Entries Pattern to be treated as a regular expression."))

;; options generator
(define (equity-statement-options-generator)
  (let* ((options (gnc:new-options))
         (book (gnc-get-current-book)) ; XXX Find a way to get the book that opened the report
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
      "b" opthelp-party-name (or (gnc:company-info book gnc:*company-name*) "")))
    
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
               ACCT-TYPE-EQUITY ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE
               ACCT-TYPE-TRADING)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))
    
    ;; all about currencies
    (gnc:options-add-currency!
     options pagename-commodities
     optname-report-commodity "a")
    
    (gnc:options-add-price-source! 
     options pagename-commodities
     optname-price-source "b" 'pricedb-nearest)
    
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
      "a" opthelp-closing-pattern (_ "Closing Entries")))
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

(define (account-get-total-flow direction target-account-list from-date to-date)
  (let ((total-flow (gnc:make-commodity-collector)))
    (for-each
     (lambda (target-account)
       (for-each
        (lambda (target-account-split)
          (let* ((transaction (xaccSplitGetParent target-account-split))
                 (split-value (xaccSplitGetAmount target-account-split)))
            (if (and (<= from-date (xaccTransGetDate transaction) to-date)
                     (or (and (eq? direction 'in)
                              (positive? split-value))
                         (and (eq? direction 'out)
                              (negative? split-value))))
                (total-flow 'add (xaccTransGetCurrency transaction) split-value))))
        (xaccAccountGetSplitList target-account)))
     target-account-list)
    total-flow))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equity-statement-renderer
;; set up the document and add the table
;; then then return the document or, if
;; requested, export it to a file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (equity-statement-renderer report-obj)
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
         (start-date (gnc:time64-end-day-time
			 (gnc:time64-previous-day start-date-printable)))
         (end-date (gnc:time64-end-day-time 
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
                  (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)
                  (assoc-ref split-up-accounts ACCT-TYPE-TRADING)))
         (equity-accounts
          (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))

	 (closing-pattern
	  (list (list 'str closing-str)
		(list 'cased closing-cased)
		(list 'regexp closing-regexp)
		(list 'positive #f)
		(list 'closing #t)))

         (doc (gnc:make-html-document))
         ;; exchange rates calculation parameters
	 (start-exchange-fn
	  (gnc:case-exchange-fn
	   price-source report-commodity start-date))
	 (end-exchange-fn
	  (gnc:case-exchange-fn
	   price-source report-commodity end-date))
	 )

    (define (unrealized-gains-at-date book-balance exchange-fn date)
      (define cost-fn
	(gnc:case-exchange-fn 'average-cost report-commodity date))
      (gnc:monetaries-add
       (gnc:sum-collector-commodity book-balance report-commodity exchange-fn)
       (gnc:monetary-neg
        (gnc:sum-collector-commodity book-balance report-commodity cost-fn))))

    (define (get-start-balance-fn account)
      (gnc:account-get-comm-balance-at-date account start-date #f))

    (define (get-end-balance-fn account)
      (gnc:account-get-comm-balance-at-date account end-date #f))

    (gnc:html-document-set-title! 
     doc (format #f
		  (string-append "~a ~a "
				 (_ "For Period Covering ~a to ~a"))
		  company-name report-title
                  (qof-print-date start-date-printable)
                  (qof-print-date end-date)))
    
    (if (null? accounts)
	
        ;; error condition: no accounts specified is this *really*
	;; necessary??  i'd be fine with an all-zero income statement
	;; that would, technically, be correct....
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj)))
	
        ;; Get all the balances for each account group.
        (let* ((start-asset-balance
                (gnc:accounts-get-comm-total-assets
                 asset-accounts get-start-balance-fn))

               (end-asset-balance
                (gnc:accounts-get-comm-total-assets
                 asset-accounts get-end-balance-fn))

               (neg-start-liability-balance
                (gnc:accounts-get-comm-total-assets
                 liability-accounts get-start-balance-fn))

               (neg-end-liability-balance
                (gnc:accounts-get-comm-total-assets
                 liability-accounts get-end-balance-fn))

               (neg-pre-start-retained-earnings
                (gnc:accountlist-get-comm-balance-at-date-with-closing
		 income-expense-accounts start-date))

               (neg-pre-end-retained-earnings
                (gnc:accountlist-get-comm-balance-at-date-with-closing
		 income-expense-accounts end-date))

               (income-expense-closing
                (gnc:account-get-trans-type-balance-interval-with-closing
                 income-expense-accounts closing-pattern start-date end-date))

               (net-income
                (gnc:collector-
                 income-expense-closing
	         (gnc:accountlist-get-comm-balance-interval-with-closing
                  income-expense-accounts start-date end-date)))

               (neg-start-equity-balance
                (gnc:accounts-get-comm-total-assets
                 equity-accounts get-start-balance-fn))

               (neg-end-equity-balance
                (gnc:accounts-get-comm-total-assets
                 equity-accounts get-end-balance-fn))

               (start-book-balance
                (gnc:collector+ start-asset-balance
	                        neg-start-liability-balance
	                        neg-start-equity-balance
	                        neg-pre-start-retained-earnings))

               (end-book-balance
                (gnc:collector+ end-asset-balance
                                neg-end-liability-balance
                                neg-end-equity-balance
                                neg-pre-end-retained-earnings))

	       (start-unrealized-gains
                (unrealized-gains-at-date start-book-balance
				          start-exchange-fn
				          start-date))

	       (net-unrealized-gains
                (unrealized-gains-at-date end-book-balance
				          end-exchange-fn
				          end-date))

	       (equity-closing
                (gnc:account-get-trans-type-balance-interval-with-closing
                 equity-accounts closing-pattern start-date end-date))

               (neg-pre-closing-equity
                (gnc:collector- neg-end-equity-balance
                                equity-closing))

	       (net-investment
                (gnc:collector- neg-start-equity-balance
                                neg-pre-closing-equity))

               ;; calculate investments & draws...
	       ;; do a transaction query and classify the splits by dr/cr.
	       ;; assume that positive shares on an equity account are debits
	       ;;   withdrawals = investments - (investments - withdrawals)
	       ;;   investments = withdrawals + (investments - withdrawals)
	       (withdrawals
                (account-get-total-flow 'in equity-accounts start-date end-date))

               (investments
                (gnc:collector+ net-investment withdrawals))

               (capital-increase
                (gnc:collector+ net-income
                                investments
                                net-unrealized-gains
                                (gnc:collector- withdrawals)))

	       (start-total-equity
                (gnc:collector- start-unrealized-gains
                                neg-start-equity-balance
                                neg-pre-start-retained-earnings))

	       (end-total-equity
                (gnc:collector+ start-total-equity
                                capital-increase))

	       ;; Create the account table below where its
	       ;; percentage time can be tracked.
	       (build-table (gnc:make-html-table)) ;; gnc:html-table
	       (period-for (string-append " " (_ "for Period"))))

	  ;; a helper to add a line to our report
	  (define (add-report-line
                   table pos-label neg-label amount col
		   exchange-fn rule? row-style)
	    (let* ((neg? (and amount neg-label
			      (negative?
			       (gnc:gnc-monetary-amount
				(gnc:sum-collector-commodity
				 amount report-commodity exchange-fn)))))
		   (label (if neg? (or neg-label pos-label) pos-label))
		   (pos-bal (if neg? (gnc:collector- amount) amount)))
	      (gnc:html-table-add-labeled-amount-line!
               table 3 row-style rule? label 0 1 "text-cell"
	       (gnc:sum-collector-commodity pos-bal report-commodity exchange-fn)
               (1+ col) 1 "number-cell")))

	  (gnc:report-percent-done 30)

          (gnc:html-table-append-row!
           build-table (make-list 2 (gnc:make-html-table-cell/min-width 60)))

          (gnc:report-percent-done 80)

          (add-report-line
           build-table
           (string-append (_ "Capital") ", " (qof-print-date start-date-printable))
           #f start-total-equity 1 start-exchange-fn #f "primary-subheading")

          (add-report-line
           build-table
           (string-append (_ "Net income") period-for)
           (string-append (_ "Net loss") period-for)
           net-income 0 end-exchange-fn #f #f)

          (add-report-line
           build-table
           (string-append (_ "Investments") period-for) #f
           investments 0 end-exchange-fn #f #f)

          (add-report-line
           build-table
           (string-append (_ "Withdrawals") period-for)
           #f withdrawals 0 end-exchange-fn #f #f)

          (unless (gnc-commodity-collector-allzero? net-unrealized-gains)
            (add-report-line
             build-table
             (_ "Unrealized Gains")
             (_ "Unrealized Losses")
             net-unrealized-gains
             0 end-exchange-fn #f #f))

          (add-report-line
           build-table
           (_ "Increase in capital")
           (_ "Decrease in capital")
           capital-increase
           1 end-exchange-fn use-rules? #f)

          (add-report-line
           build-table
           (string-append (_ "Capital") ", " (qof-print-date end-date)) #f
           end-total-equity
           1 end-exchange-fn #f "primary-subheading")

          (gnc:html-document-add-object! doc build-table)
	  
          ;; add currency information if requested
	  (gnc:report-percent-done 90)
          (when show-rates?
	    (let* ((curr-tbl (gnc:make-html-table))
		   (headers (list
			     (qof-print-date start-date-printable)
			     (qof-print-date end-date)))
		   (then (gnc:html-make-exchangerates
			  report-commodity start-exchange-fn accounts))
		   (now (gnc:html-make-exchangerates
                         report-commodity end-exchange-fn accounts)))
	      (gnc:html-table-set-col-headers! curr-tbl headers)
	      (gnc:html-table-set-style!
	       curr-tbl "table" 'attribute '("border" "1"))
	      (gnc:html-table-set-style!
	       then "table" 'attribute '("border" "0"))
	      (gnc:html-table-set-style!
	       now "table" 'attribute '("border" "0"))
	      (gnc:html-table-append-ruler! build-table 3)
	      (gnc:html-table-append-row! curr-tbl (list then now))
	      (gnc:html-document-add-object! doc curr-tbl)))
	  
	  (gnc:report-percent-done 100)))
    
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
 'renderer equity-statement-renderer)

;; END

