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

(define-module (gnucash report balance-sheet))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Balance Sheet"))

;; define all option's names so that they are properly defined
;; in *one* place.
(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-show-parent-balance (N_ "Show balances for parent accounts"))
(define optname-show-parent-total (N_ "Show subtotals"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define optname-show-rates (N_ "Show Exchange Rates"))

;; Moderatly ugly hack here, i.e. this depends on the internal
;; structure of html-table -- if that is changed, this might break.
(define (html-table-merge t1 t2)
  (begin 
    (gnc:html-table-set-data! t1
			      (append
			       (gnc:html-table-data t2)
			       (gnc:html-table-data t1)))
    (gnc:html-table-set-num-rows-internal!
     t1 (+ (gnc:html-table-num-rows t1)
           (gnc:html-table-num-rows t2)))))

(define (accountlist-get-comm-balance-at-date accountlist from date)
;;  (for-each (lambda (x) (display x))
;;	    (list "computing from: " (gnc:print-date from) " to "
;;		  (gnc:print-date date) "\n"))
  (let ((collector (gnc:make-commodity-collector)))
    (for-each (lambda (account)
                (let* (
		       (start-balance
			(gnc:account-get-comm-balance-at-date
			 account from #f))
		       (sb (cadr (start-balance
				  'getpair
				  (gnc:account-get-commodity account)
				  #f)))
		       (end-balance
			(gnc:account-get-comm-balance-at-date 
			 account date #f))
		       (eb (cadr (end-balance
				  'getpair
				  (gnc:account-get-commodity account)
				  #f)))
		       )
;;		  (for-each (lambda (x) (display x))
;;			    (list "Start balance: " sb " : "
;;				  (gnc:account-get-name account) " : end balance: "
;;				  eb "\n"))
                  (collector 'merge end-balance #f)
		  (collector 'minusmerge start-balance #f)
		  ))
              accountlist)
    collector))

;; options generator
(define (balance-sheet-options-generator)
  (let ((options (gnc:new-options)))

    ;; date at which to report balance
    (gnc:options-add-date-interval!
     options gnc:pagename-general 
     optname-from-date optname-to-date "a")

    ;; all about currencies
    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "b")

    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "c" 'weighted-average)

    ;; accounts to work on
    (gnc:options-add-account-selection! 
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type 
        '(bank cash credit asset liability stock mutual-fund currency
               payable receivable equity income expense)
        (gnc:group-get-subaccounts (gnc:get-current-group))))
     #t)
    
    ;; what to show about non-leaf accounts
    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-parent-balance 
      "c" (N_ "Show balances for parent accounts") #t))

    ;; have a subtotal for each parent account?
    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-parent-total
      "d" (N_ "Show subtotals for parent accounts") #f))

    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-foreign 
      "e" (N_ "Display the account's foreign currency amount?") #f))

    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-rates
      "f" (N_ "Show the exchange rates used") #f))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)      

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

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((display-depth (get-option gnc:pagename-accounts 
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))	 
         (show-parent-balance? (get-option gnc:pagename-display
                                           optname-show-parent-balance))
         (show-parent-total? (get-option gnc:pagename-display
                                         optname-show-parent-total))
         (show-fcur? (get-option gnc:pagename-display
                                 optname-show-foreign))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (show-rates? (get-option gnc:pagename-display 
                                  optname-show-rates))
	 (from-date-printable (gnc:date-option-absolute-time
			       (get-option gnc:pagename-general
					   optname-from-date)))
	 (from-date-tp (gnc:timepair-end-day-time
	 		(gnc:timepair-previous-day from-date-printable)))
         (to-date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-to-date))))

         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (asset-accounts
	   (assoc-ref split-up-accounts 'asset))
         (liability-accounts
	   (assoc-ref split-up-accounts 'liability))
         (equity-accounts
          (assoc-ref split-up-accounts 'equity))
         (income-expense-accounts
          (append (assoc-ref split-up-accounts 'income)
                  (assoc-ref split-up-accounts 'expense)))

         (doc (gnc:make-html-document))
         (txt (gnc:make-html-text))
         (tree-depth (if (equal? display-depth 'all)
                         (gnc:get-current-group-depth) 
                         display-depth))
         ;; calculate the exchange rates  
         (exchange-fn #f)
         (totals-get-balance #f))

    ;; Wrapper to call the right html-utility function.
    (define (add-subtotal-line table label balance)
      (if show-fcur?
	  (gnc:html-acct-table-comm-row-helper! 
	   table tree-depth report-currency exchange-fn
	   1 label report-currency 
	   (gnc:sum-collector-stocks balance report-currency exchange-fn)
	   #f #f "primary-subheading" "primary-subheading" #t #f)
	  (gnc:html-acct-table-row-helper! 
	   table tree-depth 1 label 	   
	   (gnc:sum-collector-commodity
	    balance report-currency exchange-fn)
	   #f "primary-subheading" #t #f)))
    
    ;;(gnc:warn "account names" liability-account-names)
    (gnc:html-document-set-title! 
     doc (sprintf #f "%s %s - %s"
		  (get-option gnc:pagename-general gnc:optname-reportname)
		  (gnc:print-date from-date-printable)
                  (gnc:print-date to-date-tp)))

    (if (not (null? accounts))
        ;; Get all the balances for each account group.
        (let* ((asset-balance #f)
               (liability-balance #f)
               (equity-balance #f)
               (sign-reversed-liability-balance #f)
               (neg-net-profit-balance #f)
               (net-profit-balance #f)
	       (neg-retained-earnings-balance #f)
	       (retained-earnings-balance #f)
               (total-equity-balance #f)
               (equity-plus-liability #f)
               (unrealized-gain-collector #f)

               ;; Create the account tables below where their
               ;; percentage time can be tracked.
               (asset-table #f)
               (liability-table #f)
               (equity-table #f))

	  (gnc:report-percent-done 2)
	  (set! totals-get-balance (lambda (account)
                               (gnc:account-get-comm-balance-at-date 
                                account to-date-tp #f)))
	  (gnc:report-percent-done 4)
	  (set! asset-balance 
                (gnc:accounts-get-comm-total-assets 
                 asset-accounts totals-get-balance))
	  (gnc:report-percent-done 6)
	  (set! liability-balance
                (gnc:accounts-get-comm-total-assets 
                 liability-accounts totals-get-balance))
	  (gnc:report-percent-done 8)
	  (set! equity-balance
                (gnc:accounts-get-comm-total-assets 
                 equity-accounts totals-get-balance))
	  (gnc:report-percent-done 10)
	  (set! sign-reversed-liability-balance
                (gnc:make-commodity-collector))
	  (gnc:report-percent-done 12)
	  (set! neg-net-profit-balance 
                (accountlist-get-comm-balance-at-date
                 income-expense-accounts
                 from-date-tp to-date-tp))
	  (set! neg-retained-earnings-balance
		(accountlist-get-comm-balance-at-date
		 income-expense-accounts
		 (cons 0 0) from-date-tp))
	  (gnc:report-percent-done 14)
	  (set! net-profit-balance (gnc:make-commodity-collector))
	  (set! retained-earnings-balance (gnc:make-commodity-collector))
	  (gnc:report-percent-done 16)
	  (set! total-equity-balance (gnc:make-commodity-collector))
	  (gnc:report-percent-done 18)
	  (set! equity-plus-liability (gnc:make-commodity-collector))
	  (set! unrealized-gain-collector (gnc:make-commodity-collector))

	  (gnc:report-percent-done 20)
	  (set! exchange-fn (gnc:case-exchange-fn 
			     price-source report-currency to-date-tp))
	  (gnc:report-percent-done 30)

	  ;;; Arbitrarily declare that the building of these tables
	  ;;; takes 50% of the total amount of time spent building
	  ;;; this report. (from 30%-80%)
	  (set! asset-table 
                (gnc:html-build-acct-table 
                 #f to-date-tp 
                 tree-depth show-subaccts? 
                 asset-accounts
		 30 20
                 #f #f #f #f #f
                 show-parent-balance? show-parent-total?
                 show-fcur? report-currency exchange-fn #t))
	  (set! liability-table 
                (gnc:html-build-acct-table
                 #f to-date-tp
                 tree-depth show-subaccts?
                 liability-accounts
		 50 20
                 #f #f #f #f #f
                 show-parent-balance? show-parent-total?
                 show-fcur? report-currency exchange-fn #t))
	  (set! equity-table
                (gnc:html-build-acct-table
                 #f to-date-tp
                 tree-depth show-subaccts?
                 equity-accounts
		 70 10
                 #f #f #f #f #f 
                 show-parent-balance? show-parent-total?
                 show-fcur? report-currency exchange-fn #t))

          (net-profit-balance 'minusmerge
                                   neg-net-profit-balance
                                   #f)
          (retained-earnings-balance 'minusmerge
                                   neg-retained-earnings-balance
                                   #f)
          (total-equity-balance 'minusmerge equity-balance #f)
          (total-equity-balance 'merge
                                net-profit-balance
                                #f)	    
          (total-equity-balance 'merge
                                retained-earnings-balance
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

          ;; Now concatenate the tables. This first prepend-row has
          ;; to be written out by hand -- we can't use the function
          ;; append-something because we have to prepend.
	  (gnc:report-percent-done 80)
          (gnc:html-table-prepend-row/markup! 
           asset-table 
           "primary-subheading"
           (append
            (list (gnc:html-acct-table-cell tree-depth
                                            (_ "Assets") #t))
            ;; Workaround to force gtkhtml into displaying wide
            ;; enough columns.
            (make-list (* (if show-fcur? 2 1) tree-depth)
                       "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")))

          (add-subtotal-line 
           asset-table (_ "Assets") asset-balance)	    
          
          ;; add a horizontal ruler
          (gnc:html-table-append-ruler! 
           asset-table (* (if show-fcur? 3 2) tree-depth))
          
	  (gnc:report-percent-done 85)
          (add-subtotal-line 
           asset-table (_ "Liabilities") #f)
          (html-table-merge asset-table liability-table)
          (add-subtotal-line
           asset-table (_ "Liabilities") sign-reversed-liability-balance)

          (let* ((weighted-fn
                  (gnc:case-exchange-fn 'weighted-average
                                        report-currency to-date-tp))

                 (value
                  (gnc:gnc-monetary-amount
                   (gnc:sum-collector-commodity asset-balance
                                                report-currency
                                                exchange-fn)))

                 (cost
                  (gnc:gnc-monetary-amount
                   (gnc:sum-collector-commodity asset-balance
                                                report-currency
                                                weighted-fn)))

                 (unrealized-gain (gnc:numeric-sub-fixed value cost)))

            (unrealized-gain-collector 'add report-currency unrealized-gain)
            (equity-plus-liability 'add report-currency unrealized-gain)

            (add-subtotal-line
             asset-table (_ "Unrealized Gains(Losses)")
             unrealized-gain-collector))

          (gnc:html-table-append-ruler! 
           asset-table (* (if show-fcur? 3 2) tree-depth))

	  (gnc:report-percent-done 88)
          (add-subtotal-line
           asset-table (_ "Equity") #f)
          (html-table-merge asset-table equity-table)
	  (add-subtotal-line
	   asset-table (_ "Retained Earnings") retained-earnings-balance)
          (add-subtotal-line
           asset-table (_ "Net Profit") net-profit-balance)
          (add-subtotal-line
           asset-table (_ "Total Equity") total-equity-balance)

          (gnc:html-table-append-ruler! 
           asset-table (* (if show-fcur? 3 2) tree-depth))
          (add-subtotal-line
           asset-table (_ "Liabilities & Equity") equity-plus-liability)
          (gnc:html-document-add-object! doc asset-table)
          
          ;; add currency information
	  (gnc:report-percent-done 90)
          (if show-rates?
              (gnc:html-document-add-object! 
               doc ;;(gnc:html-markup-p
               (gnc:html-make-exchangerates 
                report-currency exchange-fn accounts)))
	  (gnc:report-percent-done 100))
        
        
        ;; error condition: no accounts specified
        
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  (_ "Balance Sheet") (gnc:report-id report-obj))))
    (gnc:report-finished)
    doc))

(gnc:define-report 
 'version 1
 'name reportname
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator balance-sheet-options-generator
 'renderer balance-sheet-renderer)
