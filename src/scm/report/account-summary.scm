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

(let () 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; options generator
  ;; select accounts to report on, whether to show subaccounts,
  ;; whether to include subtotaled subaccount balances in the report,
  ;; and what date to show the summary for.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-options-generator)
    (let* ((options (gnc:new-options))
           (opt-register 
            (lambda (opt)
              (gnc:register-option options opt))))
      
      ;; date at which to report balance
      (opt-register 
       (gnc:make-date-option
        (_ "General") (_ "Date")
        "a" (_ "Select a date to report on")
	(lambda ()
	  (cons 'absolute 
		(gnc:timepair-end-day-time     
		 (gnc:secs->timepair 
		  (car (mktime (localtime (current-time))))))))
        #f 'absolute #f))
      
      (opt-register 
       (gnc:make-multichoice-option
        (_ "General") (_ "Account Display Depth")
        "b" (_ "Show accounts to this depth, overriding any other option.") 1
        (list (list->vector
	       (list 'all
		     (_ "All")
		     (_ "Show all accounts")))
	      (list->vector
               (list 1
                     "1"
                     (_ "Top-level")))
              (list->vector
               (list 2
                     "2"
                     (_ "Second-level")))
              (list->vector
               (list 3
                     "3"
                     (_ "Third-level")))
              (list->vector
               (list 4
                     "4"
                     (_ "Fourth-level")))
              (list->vector
               (list 5
                     "5"
                     (_ "Fifth-level"))))))
      
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General") (_ "Always show sub-accounts")
        "c" 
	(_ "Override account-selection and show sub-accounts of all selected accounts?") 
	#t))

      ;; Semantics of the account selection: An account shows up if (
      ;; the tree-depth is large enough AND ( it is selected in the
      ;; account selector OR ( always show sub-accounts is selected
      ;; AND one of the parents is selected in the account
      ;; selector. )))
      (opt-register 
       (gnc:make-account-list-option
        (_ "General") (_ "Account")
        "d" (_ "Report on these accounts, if display depth allows.")
        (lambda ()
          (let ((current-accounts (gnc:get-current-accounts)))
            (cond ((not (null? current-accounts)) current-accounts)
                  (else
                   ;;(gnc:group-get-subaccounts (gnc:get-current-group))))))
                   (gnc:group-get-account-list (gnc:get-current-group))))))
        #f #t))
      
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General") (_ "Include Sub-Account balances")
        "e" (_ "Include sub-account balances in printed balance?") #t))
      
      (opt-register
       (gnc:make-simple-boolean-option
	(_ "General") (_ "Show Foreign Currencies")
	"f" (_ "Display the account's foreign currency amount?") #f))
      
      (opt-register
       (gnc:make-currency-option 
	(_ "General") (_ "Report's currency") 
	"g" (_ "All other currencies will get converted to this currency.")
	(gnc:locale-default-currency)))
      
      options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Start of report generating code
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer report-obj)
    (define (get-option optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) (_ "General") optname)))
    
    (let ((display-depth (get-option (_ "Account Display Depth")))
	  (show-subaccts? (get-option (_ "Always show sub-accounts")))
	  (accounts (get-option (_ "Account")))
          (do-subtotals? (get-option (_ "Include Sub-Account balances")))
	  (show-fcur? (get-option (_ "Show Foreign Currencies")))
	  (report-currency (get-option (_ "Report's currency")))
	  ;; FIXME: So which splits are actually included and which
	  ;; are not??  Permanent repair (?): Change the semantics of
	  ;; the date-option to return not the first but the last
	  ;; second of the desired day.
          (date-tp (gnc:timepair-end-day-time 
		    (vector-ref (get-option (_ "Date")) 1)))
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
			 #f do-subtotals?
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
	    (for-each 
	     (lambda (pair)
	       (gnc:html-text-append! 
		txt
		(_ "Exchange rate ")
		(gnc:commodity-value->string 
		 (list (car pair) (gnc:numeric-create 1 1)))
		" = "
		(gnc:commodity-value->string 
		 (list report-currency 
		       (gnc:numeric-convert 
			;; FIXME: remove the constant 100000
			(cadr pair) 100000 GNC-RND-ROUND)))))
	     exchange-alist)

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
