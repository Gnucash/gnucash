;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-summary.scm : brief account listing 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
;; Copyright 2000-2001 Bill Gribble <grib@gnumatic.com>
;;
;; Even older original version by  Terry D. Boldt (tboldt@attglobal.net>
;;   Author makes no implicit or explicit guarantee of accuracy of
;;   these calculations and accepts no responsibility for direct
;;   or indirect losses incurred as a result of using this software.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/account-summary.scm")
(gnc:depend  "report-html.scm")

;; account summary report
;; prints a table of account information with clickable 
;; links to open the corresponding register window.

;; first define all option's names such that typos etc. are no longer
;; possible.
(let ((pagename-general (_ "General"))
      (optname-date (_ "Date"))
      (optname-display-depth (_ "Account Display Depth"))
      (optname-show-subaccounts (_ "Always show sub-accounts"))
      (optname-accounts (_ "Account"))
      (optname-include-subbalances (_ "Include Sub-Account balances"))
      (optname-show-foreign (_ "Show Foreign Currencies"))
      (optname-report-currency (_ "Report's currency")))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; options generator
  ;; select accounts to report on, whether to show subaccounts,
  ;; whether to include subtotaled subaccount balances in the report,
  ;; and what date to show the summary for.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-options-generator)
    (let* ((options (gnc:new-options)))
      
      ;; date at which to report balance
      (gnc:options-add-report-date!
       options pagename-general optname-date "a")

      ;; accounts to work on
      (gnc:options-add-account-selection! 
       options pagename-general 
       optname-display-depth optname-show-subaccounts
       optname-accounts "b" 1         
       (lambda ()
	 (let ((current-accounts (gnc:get-current-accounts)))
	   (cond ((not (null? current-accounts)) current-accounts)
		 (else
		  (gnc:group-get-account-list (gnc:get-current-group)))))))

      ;; with or without subaccounts
      (gnc:options-add-include-subaccounts!
       options pagename-general optname-include-subbalances "c")

      ;; all about currencies
      (gnc:options-add-currency-selection!
       options pagename-general 
       optname-show-foreign optname-report-currency
       "f")

      options))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer report-obj)
    (define (get-option optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) pagename-general optname)))
    
    (let ((display-depth (get-option optname-display-depth))
	  (show-subaccts? (get-option optname-show-subaccounts))
	  (accounts (get-option optname-accounts))
          (do-subtotals? (get-option optname-include-subbalances))
	  (show-fcur? (get-option optname-show-foreign))
	  (report-currency (get-option optname-report-currency))
	  ;; FIXME: So which splits are actually included and which
	  ;; are not??  Permanent repair (?): Change the semantics of
	  ;; the date-option to return not the first but the last
	  ;; second of the desired day.
          (date-tp (gnc:timepair-end-day-time 
		    (vector-ref (get-option optname-date) 1)))
          (doc (gnc:make-html-document))
	  (txt (gnc:make-html-text)))
      
      (gnc:html-document-set-title! doc "Account Summary")
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let* ((tree-depth (if (equal? display-depth 'all)
				 (gnc:get-current-group-depth)
				 display-depth))
		 (exchange-alist (gnc:make-exchange-alist 
				  report-currency date-tp))
		 (exchange-fn (gnc:make-exchange-function exchange-alist))
		 ;; do the processing here
		 (table (gnc:html-build-acct-table 
			 #f date-tp 
			 tree-depth show-subaccts? accounts 
			 #t gnc:accounts-get-comm-total-assets 
			 (_ "Net Assets") do-subtotals?
			 show-fcur? report-currency exchange-fn)))

	    ;; set some column headers 
	    (gnc:html-table-set-col-headers!
	     table 
	     (list (gnc:make-html-table-header-cell/size 
		    1 tree-depth (_ "Account name"))
		   (gnc:make-html-table-header-cell/size
		    1 (if show-fcur? 
			  (* 2 tree-depth)
			  tree-depth)
		    (_ "Balance"))))
	    
	    ;; add the table 
	    (gnc:html-document-add-object! doc table)

	    ;; add the currency information
	    (gnc:html-print-exchangerates! 
	     txt report-currency exchange-alist)

	    ;;(if show-fcur?
	    (gnc:html-document-add-object! doc txt))
	  
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
   'name (_ "Account Summary")
   'options-generator accsum-options-generator
   'renderer accsum-renderer))
