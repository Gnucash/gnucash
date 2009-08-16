;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balance-sheet.scm: balance sheet
;; 
;; By Robert Merkel <rgmerk@mira.net>
;;
;; Heavily modified and Frankensteined by David Montenegro 
;;   2004.06.12-2004.06.23 <sunrise2000@comcast.net>
;;  
;;  * Removed from-date & Net Profit from the report.
;;  
;;  * Updated to use the new gnc:html-acct-table utility object.
;;    Added *lots* of new options.  The report can now probably
;;    be coerced into the form that *you* want. <grin>
;;  
;;  * BUGS:
;;    
;;    The Accounts option panel needs a way to select (and select by
;;    default) accounts representative of current & fixed assets &
;;    liabilities.
;;    
;;    This code makes the assumption that you want your balance
;;    sheet to no more than daily resolution.
;;    
;;    The Company Name field does not currently default to the name
;;    in (gnc-get-current-book).
;;    
;;    Line & column alignments still do not conform with
;;    textbook accounting practice (they're close though!).
;;    The 'canonically-tabbed option is currently broken.
;;    
;;    Progress bar functionality is currently mostly broken.
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    The multicurrency support has been tested, BUT IS ALPHA.  I
;;    *think* it works right, but can make no guarantees....  In
;;    particular, I have made the educated assumption <grin> that a
;;    decrease in the value of a liability or equity also represents
;;    an unrealized loss.  I *think* that is right, but am not sure.
;;    
;;    See also all the "FIXME"s in the code.
;;    
;; Largely borrowed from pnl.scm by:
;; Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report standard-reports balance-sheet))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Balance Sheet"))

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report"))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual"))

(define optname-date (N_ "Balance Sheet Date"))
(define optname-report-form (N_ "Single column Balance Sheet"))
(define opthelp-report-form
  (N_ "Print liability/equity section in the same column under the assets section as opposed to a second column right of the assets section"))
;; FIXME this needs an indent option

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

(define optname-label-assets (N_ "Label the assets section"))
(define opthelp-label-assets
  (N_ "Whether or not to include a label for the assets section"))
(define optname-total-assets (N_ "Include assets total"))
(define opthelp-total-assets
  (N_ "Whether or not to include a line indicating total assets"))
(define optname-label-liabilities (N_ "Label the liabilities section"))
(define opthelp-label-liabilities
  (N_ "Whether or not to include a label for the liabilities section"))
(define optname-total-liabilities (N_ "Include liabilities total"))
(define opthelp-total-liabilities
  (N_ "Whether or not to include a line indicating total liabilities"))
(define optname-label-equity (N_ "Label the equity section"))
(define opthelp-label-equity
  (N_ "Whether or not to include a label for the equity section"))
(define optname-total-equity (N_ "Include equity total"))
(define opthelp-total-equity
  (N_ "Whether or not to include a line indicating total equity"))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used"))
(define optname-unrealized-gains (N_ "Compute unrealized gains and losses"))
(define opthelp-unrealized-gains
  (N_ "Include unrealized gains and losses in the computation.  Will produce incorrect results if the current file uses commodity trading accounts"))


;; options generator
(define (balance-sheet-options-generator)
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
    
    ;; date at which to report balance
    (gnc:options-add-report-date!
     options gnc:pagename-general optname-date "c")
    
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-report-form
      "d" opthelp-report-form #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-unrealized-gains
      "e" opthelp-unrealized-gains #t))
    
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
      gnc:pagename-display optname-label-assets
      "g" opthelp-label-assets #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-assets
      "h" opthelp-total-assets #t))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-liabilities
      "i" opthelp-label-liabilities #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-liabilities
      "j" opthelp-total-liabilities #t))
    
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-label-equity
      "k" opthelp-label-equity #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-total-equity
      "l" opthelp-total-equity #t))
    
    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)
    
    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balance-sheet-renderer
;; set up the document and add the table
;; then return the document or, if
;; requested, export it to a file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (balance-sheet-renderer report-obj choice filename)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))
  
  (gnc:report-starting reportname)
  
  ;; get all option's values
  (let* (
	 (report-title (get-option gnc:pagename-general optname-report-title))
	 (company-name (get-option gnc:pagename-general optname-party-name))
         (date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-date))))
         (report-form? (get-option gnc:pagename-general
                               optname-report-form))
         (compute-unrealized-gains? (get-option gnc:pagename-general
                                                optname-unrealized-gains))
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
         (label-assets? (get-option gnc:pagename-display
				    optname-label-assets))
         (total-assets? (get-option gnc:pagename-display
				    optname-total-assets))
         (label-liabilities? (get-option gnc:pagename-display
				    optname-label-liabilities))
         (total-liabilities? (get-option gnc:pagename-display
				    optname-total-liabilities))
         (label-equity? (get-option gnc:pagename-display
				    optname-label-equity))
         (total-equity? (get-option gnc:pagename-display
				    optname-total-equity))
         (use-links? (get-option gnc:pagename-display
				     optname-account-links))
         (use-rules? (get-option gnc:pagename-display
				    optname-use-rules))
	 (indent 0)
	 (tabbing #f)
	 
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
	 
         (doc (gnc:make-html-document))
	 ;; this can occasionally put extra (blank) columns in our
	 ;; table (when there is one account at the maximum depth and
	 ;; it has at least one of its ancestors deselected), but this
	 ;; is the only simple way to ensure that all three tables
	 ;; (asset, liability, equity) have the same width.
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
    ;; (gnc:sum-collector-stocks balance report-commodity exchange-fn)
    ;; Hey! Look at that! This rolls the stocks into the balance!
    ;; Can anyone think of a reason why this would be desireable?
    ;; None come to (my) mind.  Perhaps this should be a report option?
    
    ;; Wrapper around gnc:html-table-append-ruler! since we call it so
    ;; often.
    (define (add-rule table)
      (gnc:html-table-append-ruler!
       table
       (+ (* 2 tree-depth)
	  (if (equal? tabbing 'canonically-tabbed) 1 0))))

    ;;(gnc:warn "account names" liability-account-names)
    (gnc:html-document-set-title! 
     doc (string-append company-name " " report-title " "
			(gnc-print-date date-tp))
     )
    
    (if (null? accounts)
	
        ;; error condition: no accounts specified
	;; is this *really* necessary??
	;; i'd be fine with an all-zero balance sheet
	;; that would, technically, be correct....
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj)))
	
        ;; Get all the balances for each of the account types.
        (let* ((asset-balance #f)
               (neg-liability-balance #f) ;; credit balances are < 0
               (liability-balance #f)
               (neg-equity-balance #f)
               (equity-balance #f)
	       (neg-retained-earnings #f) ;; credit, income - expenses, < 0
	       (retained-earnings #f)
               (unrealized-gain-collector #f)
               (total-equity-balance #f)
               (liability-plus-equity #f)
	       
               ;; Create the account tables below where their
               ;; percentage time can be tracked.
	       (left-table (gnc:make-html-table)) ;; gnc:html-table
	       (right-table (if report-form? left-table
				(gnc:make-html-table)))
	       (table-env #f)                      ;; parameters for :make-
	       (params #f)                         ;; and -add-account-
               (asset-table #f)                    ;; gnc:html-acct-table
               (liability-table #f)                ;; gnc:html-acct-table
               (equity-table #f)                   ;; gnc:html-acct-table
	       (get-total-balance-fn
		(lambda (account)
		  (gnc:account-get-comm-balance-at-date 
		   account date-tp #f)))
               (get-total-value-fn
                (lambda (account)
                  (gnc:account-get-comm-value-at-date account date-tp #f)))
	       )
	  
	  ;; If you ask me, any outstanding(TM) retained earnings and
	  ;; unrealized gains should be added directly into equity,
	  ;; since the balance sheet does not have a period over which
	  ;; to report earnings....  See discussion on bugzilla.
	  (gnc:report-percent-done 4)
	  ;; sum assets
	  (set! asset-balance 
                (gnc:accounts-get-comm-total-assets 
                 asset-accounts get-total-balance-fn))
	  (gnc:report-percent-done 6)
	  ;; sum liabilities
	  (set! neg-liability-balance
                (gnc:accounts-get-comm-total-assets 
                 liability-accounts get-total-balance-fn))
	  (set! liability-balance
                (gnc:make-commodity-collector))
          (liability-balance 'minusmerge
			     neg-liability-balance
			     #f)
	  (gnc:report-percent-done 8)
	  ;; sum equities
	  (set! neg-equity-balance
                (gnc:accounts-get-comm-total-assets 
                 equity-accounts get-total-balance-fn))
	  (set! equity-balance (gnc:make-commodity-collector))
	  (equity-balance 'minusmerge
			  neg-equity-balance
			  #f)
	  (gnc:report-percent-done 12)
	  ;; sum any retained earnings
	  (set! neg-retained-earnings
		(gnc:accountlist-get-comm-balance-at-date
		 income-expense-accounts date-tp))
	  (set! retained-earnings (gnc:make-commodity-collector))
	  (retained-earnings 'minusmerge
			  neg-retained-earnings
			  #f)
	  (gnc:report-percent-done 14)
	  ;; sum any unrealized gains
	  ;; 
	  ;; Hm... unrealized gains....  This is when you purchase
	  ;; something and its value increases/decreases (prior to
	  ;; your selling it) and you have to reflect that on your
	  ;; balance sheet.
	  ;;
          ;; Don't calculate unrealized gains if we were asked not to.  If we are using
          ;; commodity trading accounts they will automatically accumulate the gains.
          (set! unrealized-gain-collector (gnc:make-commodity-collector))
          (if compute-unrealized-gains?
              (let ((asset-basis 
                     (gnc:accounts-get-comm-total-assets asset-accounts
                                                         get-total-value-fn))
                    (neg-liability-basis 
                     (gnc:accounts-get-comm-total-assets liability-accounts
                                                         get-total-value-fn)))
                ;; Calculate unrealized gains from assets.
                (unrealized-gain-collector 'merge asset-balance #f)
                (unrealized-gain-collector 'minusmerge asset-basis #f)
                ;; Combine with unrealized gains from liabilities
                (unrealized-gain-collector 'merge neg-liability-balance #f)
                (unrealized-gain-collector 'minusmerge neg-liability-basis #f)))

          ;; calculate equity and liability+equity totals
	  (set! total-equity-balance (gnc:make-commodity-collector))
	  (total-equity-balance 'merge
				equity-balance
				#f)
	  (total-equity-balance 'merge
				retained-earnings
				#f)
	  (total-equity-balance 'merge
				unrealized-gain-collector
				#f)
	  (gnc:report-percent-done 18)
	  (set! liability-plus-equity (gnc:make-commodity-collector))
	  (liability-plus-equity 'merge
				 liability-balance
				 #f)
	  (liability-plus-equity 'merge
				 total-equity-balance
				 #f)
	  
	  (gnc:report-percent-done 20)
	  (gnc:report-percent-done 30)
	  
	  ;;; Arbitrarily declare that the building of these tables
	  ;;; takes 50% of the total amount of time spent building
	  ;;; this report. (from 30%-80%)
	  
	  (set! table-env
		(list
		 (list 'start-date #f)
		 (list 'end-date date-tp)
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
	  
	  ;(gnc:html-table-set-style!
	  ; left-table "table" 'attribute '("rules" "rows"))
	  ;(gnc:html-table-set-style!
	  ; right-table "table" 'attribute '("rules" "rows"))
	  ;; could also '("border" "1") or '("rules" "all")
	  
	  ;; Workaround to force gtkhtml into displaying wide
	  ;; enough columns.
	  (let ((space
		 (make-list tree-depth "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
		 ))
	    (gnc:html-table-append-row! left-table space)
	    (if (not report-form?)
		(gnc:html-table-append-row! right-table space))
	    )
	  
	  (gnc:report-percent-done 80)
	  (if label-assets? (add-subtotal-line left-table (_ "Assets") #f #f))
	  (set! asset-table
		(gnc:make-html-acct-table/env/accts
		 table-env asset-accounts))
	  (gnc:html-table-add-account-balances
	   left-table asset-table params)
          (if total-assets? (add-subtotal-line 
			     left-table (_ "Total Assets") #f asset-balance))
	  
	  (if report-form?
	      (add-rule left-table))
	  (if report-form?
	      (add-rule left-table))
	  
	  (gnc:report-percent-done 85)
	  (if label-liabilities?
	      (add-subtotal-line 
	       right-table (_ "Liabilities") #f #f))
	  (set! liability-table
		(gnc:make-html-acct-table/env/accts
		 table-env liability-accounts))
	  (gnc:html-table-add-account-balances
	   right-table liability-table params)
	  (if total-liabilities?
	      (add-subtotal-line
	       right-table (_ "Total Liabilities") #f liability-balance))
	  
	  (add-rule right-table)
	  
	  (gnc:report-percent-done 88)
	  (if label-equity?
	      (add-subtotal-line
	       right-table (_ "Equity") #f #f))
	  (set! equity-table
		(gnc:make-html-acct-table/env/accts
		 table-env equity-accounts))
	  (gnc:html-table-add-account-balances
	   right-table equity-table params)
	  ;; we omit retianed earnings & unrealized gains
	  ;; from the balance report, if zero, since they
	  ;; are not present on normal balance sheets
	  (and (not (gnc-commodity-collector-allzero?
		     retained-earnings))
	       (add-subtotal-line right-table
				  (_ "Retained Earnings")
				  (_ "Retained Losses")
				  retained-earnings))
	  (and (not (gnc-commodity-collector-allzero?
		     unrealized-gain-collector))
	       (add-subtotal-line right-table
				  (_ "Unrealized Gains")
				  (_ "Unrealized Losses")
				  unrealized-gain-collector))
	  (if total-equity?
	      (add-subtotal-line
	       right-table (_ "Total Equity") #f total-equity-balance))
	  
	  (add-rule right-table)
	  
          (add-subtotal-line
           right-table (_ "Total Liabilities & Equity")
	   #f liability-plus-equity)
	  
	  (gnc:html-document-add-object!
	   doc
	   (if report-form?
	       left-table
	       (let* ((build-table (gnc:make-html-table))
		      )
		 (gnc:html-table-append-row!
		  build-table
		  (list
		   (gnc:make-html-table-cell left-table)
		   (gnc:make-html-table-cell right-table)
		   )
		  )
		 (gnc:html-table-set-style!
		  build-table "td"
		  'attribute '("align" "left")
		  'attribute '("valign" "top"))
		 build-table
		 )
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
	  
	  ;; if sending the report to a file, do so now
	  ;; however, this still doesn't seem to get around the
	  ;; colspan bug... cf. gnc:colspans-are-working-right
	  (if filename
	      (let* ((port (open-output-file filename))
		     (gnc:display-report-list-item
		      (list doc) port " balance-sheet.scm ")
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
 'report-guid "c4173ac99b2b448289bf4d11c731af13"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator balance-sheet-options-generator
 'renderer (lambda (report-obj)
	     (balance-sheet-renderer report-obj #f #f))
 'export-types #f
 'export-thunk (lambda (report-obj choice filename)
		 (balance-sheet-renderer report-obj #f filename)))

;; END

