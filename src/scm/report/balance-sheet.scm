;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balance-sheet.scm: balance sheet 
;; 
;; By Robert Merkel <rgmerk@mira.net>
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/balance-sheet.scm")
(gnc:depend  "report-html.scm")

;; first define all option's names so that they are properly defined
;; in *one* place.

(let* ((pagename-general (N_ "General"))
       (optname-from-date (N_ "From"))
       (optname-to-date (N_ "To"))
       
       (pagename-accounts (N_ "Accounts"))
       (optname-display-depth (N_ "Account Display Depth"))
       (optname-show-subaccounts (N_ "Always show sub-accounts"))
       (optname-accounts (N_ "Account"))
;      (optname-group-accounts (N_ "Group the accounts"))
       (optname-include-subbalances (N_ "Include Sub-Account balances"))
       
;;      (pagename-currencies (N_ "Currencies")) too little options :)
       (pagename-currencies pagename-general)
       (optname-show-foreign (N_ "Show Foreign Currencies"))
       (optname-report-currency (N_ "Report's currency")))

;;; FIXME: UGLY HACK OCCURRING HERE!!!!!!!
  (define (html-table-merge t1 t2)
    (begin 
      (gnc:html-table-set-data! t1
			      (append
			       (gnc:html-table-data t2)
			       (gnc:html-table-data t1)))
      (gnc:html-table-set-num-rows-internal!
       t1 (length (gnc:html-table-data t1)))))
      
  (define (accountlist-get-comm-balance-at-date accountlist date)
    (let ((collector (gnc:make-commodity-collector)))
      (for-each (lambda (account)
		  (let ((balance 
			(gnc:account-get-comm-balance-at-date 
			 account date #f)))
		    (collector 'merge balance #f)))
		accountlist)
      collector))
		  
  ;; options generator
  (define (balance-sheet-options-generator)
    (let ((options (gnc:new-options)))
      
      ;; date at which to report balance
      (gnc:options-add-report-date!
       options pagename-general 
       optname-to-date "a")

      ;; all about currencies
      (gnc:options-add-currency-selection!
       options pagename-currencies
       optname-show-foreign optname-report-currency
       "b")

      ;; accounts to work on
      (gnc:options-add-account-selection! 
       options pagename-accounts
       optname-display-depth optname-show-subaccounts
       optname-accounts "a" 2
       (lambda ()
	 (gnc:filter-accountlist-type 
	  '(bank cash credit asset liability stock mutual-fund currency
            equity income expense)
	  (gnc:group-get-subaccounts (gnc:get-current-group)))))

      ;; with or without grouping
;      (gnc:options-add-group-accounts!      
;       options pagename-accounts optname-group-accounts "b" #t)

      ;; with or without subaccounts
      (gnc:options-add-include-subaccounts!
       options pagename-accounts optname-include-subbalances "c")
      
      ;; Set the general page as default option tab
      (gnc:options-set-default-section options pagename-general)      

      options))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; balance-sheet-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (balance-sheet-renderer report-obj)
    (define (get-option pagename optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) pagename optname)))

    (define (add-retained-profits-line
             table balance show-fcur? exchange-fn report-commodity)
      (if show-fcur?
	  (let ((first-row #t))
	    (balance 'format
                     (lambda (commodity amount)
                       (html-table-append-row!
                        (list (if first-row
                                  (begin 
                                    (set! first-row #f)
                                    (_ "Net Profit"))
                                  " ")
                              (gnc:make-gnc-monetary
                               commodity amount))))
		     #f))
	  (gnc:html-table-append-row!
           table (list (_ "Net Profit") 
                       (gnc:sum-collector-commodity 
                        balance report-commodity exchange-fn)))))
				   

    (define (add-subtotal-line
             table label total show-fcur? exchange-fn report-commodity)
      (if show-fcur?
	  (let ((first-row #t))
	    (total 'format
                   (lambda (commodity amount)
                     (html-table-append-row!
                      (list (if first-row
                                (begin 
                                  (set! first-row #f)
                                  label)
                                " ")
                            (gnc:make-gnc-monetary
                             commodity amount))))
		   #f))
	  (gnc:html-table-append-row!
           table (list label
                       (gnc:sum-collector-commodity 
                        total report-commodity exchange-fn)))))

    ;; get all option's values
    (let* ((display-depth (get-option pagename-accounts 
				      optname-display-depth))
	   (show-subaccts? (get-option pagename-accounts
				      optname-show-subaccounts))
	   (accounts (get-option pagename-accounts
				 optname-accounts))
	   (asset-accounts
	    (gnc:filter-accountlist-type
	     '(bank cash asset stock mutual-fund)
	     accounts))
	   (liability-accounts
	    (gnc:filter-accountlist-type
	     '(credit liability)
	     accounts))
	   (liability-account-names
	    (map gnc:account-get-name liability-accounts))
	   (equity-accounts
	    (gnc:filter-accountlist-type
	     '(equity)
	     accounts))
	   (income-expense-accounts
	    (gnc:filter-accountlist-type
	     '(income expense)
	     accounts))
	 
;	   (do-grouping? (get-option pagename-accounts
;				     optname-group-accounts))
	   (do-subtotals? (get-option pagename-accounts
				      optname-include-subbalances))
	   (show-fcur? (get-option pagename-currencies
				   optname-show-foreign))
	   (report-currency (get-option pagename-currencies
					optname-report-currency))
	   (to-date-tp (gnc:timepair-end-day-time 
		       (vector-ref (get-option pagename-general
					       optname-to-date) 1)))
	 
	   (doc (gnc:make-html-document))
	   (txt (gnc:make-html-text)))
      (gnc:warn "account names" liability-account-names)
      (gnc:html-document-set-title! 
       ;; FIXME: Use magic sprintf code.
       doc (sprintf #f (N_ "Balance sheet at %s")
			  (gnc:timepair-to-datestring to-date-tp)))
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let* ((tree-depth (if (equal? display-depth 'all)
				 (+ (gnc:get-current-group-depth) 
				    (if do-grouping? 1 0))
				 display-depth))
		 ;; calculate the exchange rates  
		 
		 (exchange-alist (gnc:make-exchange-alist 
				  report-currency to-date-tp))
		 (exchange-fn (gnc:make-exchange-function exchange-alist))
		 (totals-get-balance (lambda (account)
				    (gnc:account-get-comm-balance-at-date 
				     account to-date-tp #f)))

		 (asset-balance 
		  (gnc:accounts-get-comm-total-assets 
		   asset-accounts totals-get-balance))
		 (liability-balance
		  (gnc:accounts-get-comm-total-assets 
		   liability-accounts totals-get-balance))
		 (equity-balance
		  (gnc:accounts-get-comm-total-assets 
		   equity-accounts totals-get-balance))
		 (sign-reversed-liability-balance
		  (gnc:make-commodity-collector))
		 (neg-retained-profit-balance 
		  (accountlist-get-comm-balance-at-date
		   income-expense-accounts
		   to-date-tp))
		 (retained-profit-balance (gnc:make-commodity-collector))
		 (total-equity-balance (gnc:make-commodity-collector))
		 (equity-plus-liability (gnc:make-commodity-collector))

		 ;; do the processing here
		 (asset-table (gnc:html-build-acct-table 
			 #f  to-date-tp 
			 tree-depth show-subaccts? asset-accounts
			 #f #f
			 gnc:accounts-get-comm-total-assets
			 (_ "Assets") #f do-subtotals?
			 show-fcur? report-currency exchange-fn))
		 (liability-table 
		  (gnc:html-build-acct-table
			#f
			to-date-tp
			tree-depth
			show-subaccts?
			liability-accounts
			#f #f
			gnc:accounts-get-comm-total-assets
			(_ "Liabilities") #f do-subtotals?
			show-fcur? report-currency exchange-fn))
		 (equity-table
		  (gnc:html-build-acct-table
		   #f
		   to-date-tp
		   tree-depth
		   show-subaccts?
		   equity-accounts
		   #f #f
		   gnc:accounts-get-comm-total-assets
		   (_ "Equity") #f do-subtotals?
		   show-fcur? report-currency exchange-fn)))
	    (retained-profit-balance 'minusmerge
				     neg-retained-profit-balance
				     #f)
	    (total-equity-balance 'minusmerge equity-balance #f)
	    (total-equity-balance 'merge
				  retained-profit-balance
				  #f)	    
	    (sign-reversed-liability-balance 'minusmerge
					     liability-balance
					     #f)
	    (equity-plus-liability 'merge
				   sign-reversed-liability-balance
				   #f)
	    (equity-plus-liability 'merge
				   total-equity-balance
				   #f)
				  
			       

	    
	    ;; add the tables
	    (gnc:html-table-prepend-row! asset-table (list "Assets"))	    
	    (add-subtotal-line
	     asset-table "Assets" 
	     asset-balance show-fcur? exchange-fn
	     report-currency)	    
	    (gnc:html-table-append-row! asset-table (list "Liabilities"))
	    (html-table-merge asset-table liability-table)
	    (add-subtotal-line
	     asset-table "Liabilities" 
	     sign-reversed-liability-balance show-fcur? exchange-fn
	     report-currency)
	    (gnc:html-table-append-row! asset-table (list "Equity"))
	    (html-table-merge asset-table equity-table)
	    (add-retained-profits-line
             asset-table retained-profit-balance
             show-fcur? exchange-fn report-currency)
	    (add-subtotal-line
             asset-table "Total Equity" total-equity-balance show-fcur?
             exchange-fn report-currency)
	    (add-subtotal-line
	     asset-table "Liabilities & Equity" equity-plus-liability
	     show-fcur? exchange-fn report-currency)
	    (gnc:html-document-add-object! doc asset-table)

	    ;; add currency information
;	    (gnc:html-document-add-object! 
;	     doc ;;(gnc:html-markup-p
;	     (gnc:html-make-exchangerates 
;	      report-currency exchange-alist accounts #f)))
)
	  
	  ;; error condition: no accounts specified
          (let ((p (gnc:make-html-text)))
            (gnc:html-text-append! 
             p 
             (gnc:html-markup-h2 (_ "No accounts selected"))
             (gnc:html-markup-p
              (_ "This report requires accounts to be selected.")))
            (gnc:html-document-add-object! doc p)))      
      doc))

  (gnc:define-report 
   'version 1
   'name (N_ "Balance Sheet")
   'options-generator balance-sheet-options-generator
   'renderer balance-sheet-renderer))
