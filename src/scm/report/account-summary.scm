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
(let ((pagename-general (N_ "General"))
      (optname-date (N_ "Date"))
      (optname-display-depth (N_ "Account Display Depth"))

      (optname-show-foreign (N_ "Show Foreign Currencies/Shares of Stock"))
      (optname-report-currency (N_ "Report's currency"))

      (pagename-accounts (N_ "Accounts"))
      (optname-show-subaccounts (N_ "Always show sub-accounts"))
      (optname-accounts (N_ "Account"))

      (pagename-display (N_ "Display"))
      (optname-group-accounts (N_ "Group the accounts"))
      (optname-show-parent-balance (N_ "Show balances for parent accounts"))
      (optname-show-parent-total (N_ "Show subtotals")))
  
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

      ;; all about currencies
      (gnc:options-add-currency-selection!
       options pagename-general 
       optname-show-foreign optname-report-currency
       "b")

      ;; accounts to work on
      (gnc:options-add-account-selection! 
       options pagename-accounts 
       optname-display-depth optname-show-subaccounts
       optname-accounts "a" 1
       (lambda ()
         ;; FIXME : gnc:get-current-accounts disappeared
	 (let ((current-accounts '()))
	   (cond ((not (null? current-accounts)) current-accounts)
		 (else
		  (gnc:group-get-account-list (gnc:get-current-group)))))))
      
      ;; with or without grouping
      (gnc:options-add-group-accounts!      
       options pagename-display optname-group-accounts "b" #t)
      
      ;; new options here
      (gnc:register-option 
       options
       (gnc:make-simple-boolean-option
	pagename-display optname-show-parent-balance 
	"c" (N_ "Show balances for parent accounts") #t))

      (gnc:register-option 
       options
       (gnc:make-simple-boolean-option
	pagename-display optname-show-parent-total
	"d" (N_ "Show subtotals for parent accounts") #t))

      ;; Set the general page as default option tab
      (gnc:options-set-default-section options pagename-general)      

      options))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer report-obj)
    (define (get-option pagename optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) pagename optname)))
    
    (let ((display-depth (get-option pagename-accounts 
				     optname-display-depth ))
	  (show-subaccts? (get-option pagename-accounts
				      optname-show-subaccounts))
	  (accounts (get-option pagename-accounts optname-accounts))
          (do-grouping? (get-option pagename-display
				    optname-group-accounts))
          (show-parent-balance? (get-option pagename-display
					    optname-show-parent-balance))
          (show-parent-total? (get-option pagename-display
					  optname-show-parent-total))
	  (show-fcur? (get-option pagename-general optname-show-foreign))
	  (report-currency (get-option pagename-general 
				       optname-report-currency))
          (date-tp (gnc:timepair-end-day-time 
		    (vector-ref (get-option pagename-general 
					    optname-date) 1)))
          (doc (gnc:make-html-document))
	  (txt (gnc:make-html-text)))
      
      (gnc:html-document-set-title! doc (_ "Account Summary"))
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let* ((tree-depth (+ (if (equal? display-depth 'all)
				  (gnc:get-current-group-depth)
				  display-depth)
				(if do-grouping? 1 0)))
		 (exchange-alist (gnc:make-exchange-alist 
				  report-currency date-tp))
		 (exchange-fn (gnc:make-exchange-function exchange-alist))
		 ;; do the processing here
		 (table (gnc:html-build-acct-table 
			 #f date-tp 
			 tree-depth show-subaccts? accounts
			 #t
			 #t gnc:accounts-get-comm-total-assets 
			 (_ "Total") do-grouping? 
			 show-parent-balance? show-parent-total?
			 show-fcur? report-currency exchange-fn)))

	    ;; add the table 
	    (gnc:html-document-add-object! doc table)

	    ;; add the currency information
	    ;(gnc:html-print-exchangerates! 
	    ; txt report-currency exchange-alist)

	    ;;(if show-fcur?
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
   'name (N_ "Account Summary")
   'options-generator accsum-options-generator
   'renderer accsum-renderer))
