;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pnl.scm : profit-and-loss report 
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

(gnc:support "report/pnl.scm")
(gnc:depend  "report-html.scm")

;; Profit and loss report. Actually, people in finances might want
;; something different under this name, but they are welcomed to
;; contribute their changes :-)

;; first define all option's names so that they are properly defined
;; in *one* place.
(let* ((pagename-general (N_ "General"))
       (optname-from-date (N_ "From"))
       (optname-to-date (N_ "To"))
       
       (pagename-accounts (N_ "Accounts"))
       (optname-display-depth (N_ "Account Display Depth"))
       (optname-show-subaccounts (N_ "Always show sub-accounts"))
       (optname-accounts (N_ "Account"))
       (optname-group-accounts (N_ "Group the accounts"))
       (optname-include-subbalances (N_ "Include Sub-Account balances"))
       
;;      (pagename-currencies (N_ "Currencies")) too little options :)
       (pagename-currencies pagename-general)
       (optname-show-foreign (N_ "Show Foreign Currencies"))
       (optname-report-currency (N_ "Report's currency")))
  
  ;; options generator
  (define (pnl-options-generator)
    (let ((options (gnc:new-options)))
      
      ;; date at which to report balance
      (gnc:options-add-date-interval!
       options pagename-general 
       optname-from-date optname-to-date "a")

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
       ;; FIXME: get income/expense accounts
       (lambda ()
	 (filter 
	  gnc:account-is-inc-exp?
	  (gnc:group-get-account-list (gnc:get-current-group)))))

      ;; with or without grouping
      (gnc:options-add-group-accounts!      
       options pagename-accounts optname-group-accounts "b" #t)

      ;; with or without subaccounts
      (gnc:options-add-include-subaccounts!
       options pagename-accounts optname-include-subbalances "c")
      
      ;; Set the general page as default option tab
      (gnc:options-set-default-section options pagename-general)      

      options))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pnl-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (pnl-renderer report-obj)
    (define (get-option pagename optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) pagename optname)))

    ;; get all option's values
    (let ((display-depth (get-option pagename-accounts 
				     optname-display-depth))
	  (show-subaccts? (get-option pagename-accounts
				      optname-show-subaccounts))
	  (accounts (get-option pagename-accounts
				optname-accounts))
          (do-grouping? (get-option pagename-accounts
				    optname-group-accounts))
          (do-subtotals? (get-option pagename-accounts
				     optname-include-subbalances))
	  (show-fcur? (get-option pagename-currencies
				  optname-show-foreign))
	  (report-currency (get-option pagename-currencies
				       optname-report-currency))
          (to-date-tp (gnc:timepair-end-day-time 
		       (vector-ref (get-option pagename-general
					       optname-to-date) 1)))
          (from-date-tp (gnc:timepair-start-day-time 
			 (vector-ref (get-option pagename-general
						 optname-from-date) 1)))
          (doc (gnc:make-html-document)))
      
      (gnc:html-document-set-title! 
       doc (sprintf #f
                    (_ "Profit and Loss - %s to %s")
                    (gnc:timepair-to-datestring from-date-tp)
                    (gnc:timepair-to-datestring to-date-tp)))
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let* ((tree-depth (+ (if (equal? display-depth 'all)
				    (gnc:get-current-group-depth) 
				    display-depth)
				(if do-grouping? 1 0)))
		 ;; calculate the exchange rates
		 (exchange-alist (gnc:make-exchange-alist 
				  report-currency to-date-tp))
		 (exchange-fn (gnc:make-exchange-function exchange-alist))
		 ;; do the processing here
		 (table (gnc:html-build-acct-table 
			 from-date-tp to-date-tp 
			 tree-depth show-subaccts? accounts 
			 #t gnc:accounts-get-comm-total-profit 
			 (_ "Profit") do-grouping? do-subtotals?
			 show-fcur? report-currency exchange-fn)))

	    ;; add the table 
	    (gnc:html-document-add-object! doc table)

	    ;; add currency information
	    (gnc:html-document-add-object! 
	     doc ;;(gnc:html-markup-p
	     (gnc:html-make-exchangerates 
	      report-currency exchange-alist accounts #f)));;)
	  
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
   'name (N_ "Profit And Loss")
   'options-generator pnl-options-generator
   'renderer pnl-renderer))
