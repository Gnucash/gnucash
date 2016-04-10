;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow.scm: cash flow report 
;; 
;; By Herbert Thoma <herbie@hthoma.de>
;;
;; based on balance-sheet.scm by:
;; Robert Merkel <rgmerk@mira.net>
;; and pnl.scm by:
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

(define-module (gnucash report standard-reports cash-flow))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash engine))
(use-modules (gnucash printf))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(export cash-flow-calc-money-in-out)

(define reportname (N_ "Cash Flow"))

;; define all option's names so that they are properly defined
;; in *one* place.
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-include-trading-accounts (N_ "Include Trading Accounts in report"))

;; options generator
(define (cash-flow-options-generator)
  (let ((options (gnc:new-options)))

    ;; date interval
    (gnc:options-add-date-interval!
     options gnc:pagename-general 
     optname-from-date optname-to-date "a")

    ;; all about currencies
    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "b")

    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "c" 'pricedb-nearest)

    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-rates
      "d" (N_ "Show the exchange rates used.") #f))

    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts).") #t))

    ;; accounts to work on
    (gnc:options-add-account-selection! 
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type 
        (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-ASSET
              ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)
     
     ;; Trading accounts?
     (gnc:register-option
      options
      (gnc:make-simple-boolean-option
       gnc:pagename-accounts optname-include-trading-accounts
       "b" (N_ "Include transfers to and from Trading Accounts in the report.")  #f))
    
    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)      

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow-renderer
;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cash-flow-renderer report-obj)
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
         (include-trading-accounts (get-option gnc:pagename-accounts
                               optname-include-trading-accounts))
         (row-num 0)
	 (work-done 0)
	 (work-to-do 0)
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (show-rates? (get-option gnc:pagename-general 
                                  optname-show-rates))
         (show-full-names? (get-option gnc:pagename-general 
                                       optname-show-full-names))
         (from-date-tp (gnc:timepair-start-day-time 
                        (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general
                                     optname-from-date))))
         (to-date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-to-date))))

         ;; calculate the exchange rates
         (exchange-fn (gnc:case-exchange-fn
                       price-source report-currency to-date-tp))

         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
         (txt (gnc:make-html-text)))

    (gnc:html-document-set-title! 
     doc (string-append
	  (get-option gnc:pagename-general gnc:optname-reportname)
	  " - "
	  (sprintf #f (_ "%s to %s")
		   (gnc-print-date from-date-tp) (gnc-print-date to-date-tp))))


    ;; add subaccounts if requested
    (if show-subaccts?
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account accounts))
                  (set! accounts (append accounts sub-accounts))))
            sub-accounts)))


    (if (not (null? accounts))

        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts) 
                               display-depth))

               (money-diff-collector (gnc:make-commodity-collector))
	       (account-disp-list '())

	       (time-exchange-fn #f)
	       (commodity-list (gnc:accounts-get-commodities
				accounts
				report-currency))
	       ;; Get an exchange function that will convert each transaction using the
	       ;; nearest available exchange rate if that is what is specified
	       (time-exchange-fn (gnc:case-exchange-time-fn
				  price-source report-currency
				  commodity-list to-date-tp
				  0 0)))

	  ;; Helper function to convert currencies
	  (define (to-report-currency currency amount date)
	    (gnc:gnc-monetary-amount
	     (time-exchange-fn (gnc:make-gnc-monetary currency amount)
			       report-currency
			       date)))


          (let ((result (cash-flow-calc-money-in-out
			 (list (cons 'accounts accounts)
			       (cons 'to-date-tp to-date-tp)
			       (cons 'from-date-tp from-date-tp)
			       (cons 'report-currency report-currency)
			       (cons 'include-trading-accounts include-trading-accounts)
			       (cons 'to-report-currency to-report-currency)))))
	    (let ((money-in-accounts (cdr (assq 'money-in-accounts result)))
		  (money-in-alist (cdr (assq 'money-in-alist result)))
		  (money-in-collector (cdr (assq 'money-in-collector result)))
		  (money-out-accounts (cdr (assq 'money-out-accounts result)))
		  (money-out-alist (cdr (assq 'money-out-alist result)))
		  (money-out-collector (cdr (assq 'money-out-collector result))))
	      (money-diff-collector 'merge money-in-collector #f)
	      (money-diff-collector 'minusmerge money-out-collector #f)

	      (set! accounts (sort accounts account-full-name<?))
	      (set! money-in-accounts (sort money-in-accounts account-full-name<?))
	      (set! money-out-accounts (sort money-out-accounts account-full-name<?))


	      (set! work-done 0)
	      (set! work-to-do (length accounts))
	      (for-each
	       (lambda (account)
		 (set! work-done (+ 1 work-done))
		 (gnc:report-percent-done (+ 85 (* 5 (/ work-done work-to-do))))
		 (if (<= (gnc-account-get-current-depth account) tree-depth)
		     (let* ((anchor (gnc:html-markup/format
				     (if (and (= (gnc-account-get-current-depth account) tree-depth)
					      (not (eq? (gnc-account-get-children account) '())))
					 (if show-subaccts?
					     (_ "%s and subaccounts")
					     (_ "%s and selected subaccounts"))
					 "%s")
				     (gnc:html-markup-anchor
				      (gnc:account-anchor-text account)
				      (if show-full-names?
					  (gnc-account-get-full-name account)
					  (xaccAccountGetName account))))))

		       (set! account-disp-list (cons anchor account-disp-list))
		       )
		     )
		 )
	       accounts
	       )


	      (gnc:html-document-add-object!
	       doc
	       (gnc:make-html-text (_ "Selected Accounts")))

	      (gnc:html-document-add-object!
	       doc
	       (gnc:make-html-text
		(gnc:html-markup-ul
		 (reverse account-disp-list))))

	      (gnc:html-table-append-ruler! table 2)

	      (gnc:html-table-append-row/markup!
	       table
	       "primary-subheading"
	       (list
		(_ "Money into selected accounts comes from")
		""))

	      (set! row-num 0)
	      (set! work-done 0)
	      (set! work-to-do (length money-in-alist))
	      (for-each
	       (lambda (account)
		 (set! row-num (+ 1 row-num))
		 (set! work-done (+ 1 work-done))
		 (gnc:report-percent-done (+ 90 (* 5 (/ work-done work-to-do))))
		 (let* ((pair (account-in-alist account money-in-alist))
			(acct (car pair)))
		   (gnc:html-table-append-row/markup!
		    table
		    (if (odd? row-num) "normal-row" "alternate-row")
		    (list
					;(gnc:html-account-anchor acct)
		     (gnc:make-html-text
		      (gnc:html-markup-anchor
		       (gnc:account-anchor-text acct)
		       (if show-full-names?
			   (gnc-account-get-full-name acct)
			   (xaccAccountGetName acct))))
		     (gnc:make-html-table-header-cell/markup
		      "number-cell" (gnc:sum-collector-commodity (cadr pair) report-currency exchange-fn))))
		   )
		 )
	       money-in-accounts
	       )

	      (gnc:html-table-append-row/markup!
	       table
	       "grand-total"
	       (list
		(gnc:make-html-table-header-cell/markup "text-cell" (_ "Money In"))
		(gnc:make-html-table-header-cell/markup
		 "total-number-cell" (gnc:sum-collector-commodity money-in-collector report-currency exchange-fn))))

	      (gnc:html-table-append-ruler! table 2)

	      (gnc:html-table-append-row/markup!
	       table
	       "primary-subheading"
	       (list
		(_ "Money out of selected accounts goes to")
		""))

	      (set! row-num 0)
	      (set! work-done 0)
	      (set! work-to-do (length money-out-alist))
	      (for-each
	       (lambda (account)
		 (set! row-num (+ 1 row-num))
		 (set! work-done (+ 1 work-done))
		 (gnc:report-percent-done (+ 95 (* 5 (/ work-done work-to-do))))
		 (let* ((pair (account-in-alist account money-out-alist))
			(acct (car pair)))
		   (gnc:html-table-append-row/markup!
		    table
		    (if (odd? row-num) "normal-row" "alternate-row")
		    (list
					;(gnc:html-account-anchor acct)
		     (gnc:make-html-text
		      (gnc:html-markup-anchor
		       (gnc:account-anchor-text acct)
		       (if show-full-names?
			   (gnc-account-get-full-name acct)
			   (xaccAccountGetName acct))))
		     (gnc:make-html-table-header-cell/markup
		      "number-cell" (gnc:sum-collector-commodity (cadr pair) report-currency exchange-fn))))
		   )
		 )
	       money-out-accounts
	       )

	      (gnc:html-table-append-row/markup!
	       table
	       "grand-total"
	       (list
		(gnc:make-html-table-header-cell/markup "text-cell" (_ "Money Out"))
		(gnc:make-html-table-header-cell/markup
		 "total-number-cell" (gnc:sum-collector-commodity money-out-collector report-currency exchange-fn))))

	      (gnc:html-table-append-ruler! table 2)

	      (gnc:html-table-append-row/markup!
	       table
	       "grand-total"
	       (list
		(gnc:make-html-table-header-cell/markup "text-cell" (_ "Difference"))
		(gnc:make-html-table-header-cell/markup
		 "total-number-cell" (gnc:sum-collector-commodity money-diff-collector report-currency exchange-fn))))

	      (gnc:html-document-add-object! doc table)


	      ;; add currency information
	      (if show-rates?
		  (gnc:html-document-add-object!
		   doc ;;(gnc:html-markup-p
		   (gnc:html-make-exchangerates
		    report-currency exchange-fn accounts))))

	    ))

	    ;; error condition: no accounts specified
	    
	(gnc:html-document-add-object!
	 doc
	 (gnc:html-make-no-account-warning
	  reportname (gnc:report-id report-obj))))

    (gnc:report-finished)
    doc))


;; function to add inflow and outflow of money
(define (cash-flow-calc-money-in-out settings)
  (let* ((accounts (cdr (assq 'accounts settings)))
	 (to-date-tp (cdr (assq 'to-date-tp settings)))
	 (from-date-tp (cdr (assq 'from-date-tp settings)))
	 (report-currency (cdr (assq 'report-currency settings)))
	 (include-trading-accounts (cdr (assq 'include-trading-accounts settings)))
	 (to-report-currency (cdr (assq 'to-report-currency settings)))

	 (is-report-account? (account-in-list-pred accounts))

	 (money-in-accounts '())
	 (money-in-hash (make-hash-table))
	 (money-in-collector (gnc:make-commodity-collector))

	 (money-out-accounts '())
	 (money-out-hash (make-hash-table))
	 (money-out-collector (gnc:make-commodity-collector))

	 (all-splits (gnc:account-get-trans-type-splits-interval accounts '() from-date-tp to-date-tp))
	 (splits-to-do (length all-splits))
	 (splits-seen-table (make-hash-table))
	 (work-done 0))

    (define (split-seen? split)
      (if (split-hashtable-ref splits-seen-table split) #t
	  (begin
	    (split-hashtable-set! splits-seen-table split #t)
	    #f)))

    (define (work-per-split split)
      (set! work-done (+ 1 work-done))
      (if (= (modulo work-done 100) 0)
	  (gnc:report-percent-done (* 85 (/ work-done splits-to-do))))
      (let ((parent (xaccSplitGetParent split)))
	(if (and (gnc:timepair-le (gnc-transaction-get-date-posted parent) to-date-tp)
		 (gnc:timepair-ge (gnc-transaction-get-date-posted parent) from-date-tp))
	    (let* ((parent-description (xaccTransGetDescription parent))
		   (parent-currency (xaccTransGetCurrency parent)))
					;(gnc:debug parent-description
					;           " - "
					;           (gnc-commodity-get-printname parent-currency))
	      (for-each
	       (lambda (s)
		 (let* ((s-account (xaccSplitGetAccount s))
			(s-account-type (xaccAccountGetType s-account))
			(s-amount (xaccSplitGetAmount s))
			(s-value (xaccSplitGetValue s))
			(s-commodity (xaccAccountGetCommodity s-account)))
		   ;; Check if this is a dangling split
		   ;; and print a warning
		   (if (null? s-account)
		       (display
			(string-append
			 "WARNING: s-account is NULL for split: "
			 (gncSplitGetGUID s) "\n")))
					;(gnc:debug (xaccAccountGetName s-account))
		   (if (and	 ;; make sure we don't have
			(not (null? s-account)) ;;  any dangling splits
			(or include-trading-accounts (not (eq? s-account-type ACCT-TYPE-TRADING)))
			(not (is-report-account? s-account)))
		       (if (not (split-seen? s))
			   (begin
			     (if (gnc-numeric-negative-p s-value)
				 (let ((s-account-in-collector (account-hashtable-ref money-in-hash s-account)))
					;(gnc:debug "in:" (gnc-commodity-get-printname s-commodity)
					;	     (gnc-numeric-to-double s-amount)
					;	     (gnc-commodity-get-printname parent-currency)
					;	     (gnc-numeric-to-double s-value))
				   (if (not s-account-in-collector)
				       (begin
					 (set! s-account-in-collector (gnc:make-commodity-collector))
					 (account-hashtable-set! money-in-hash s-account
								 s-account-in-collector)
					 (set! money-in-accounts (cons s-account money-in-accounts))
					 )
				       )
				   (let ((s-report-value (to-report-currency parent-currency
									     (gnc-numeric-neg s-value)
									     (gnc-transaction-get-date-posted
									      parent))))
				     (money-in-collector 'add report-currency s-report-value)
				     (s-account-in-collector 'add report-currency s-report-value))
				   )
				 (let ((s-account-out-collector (account-hashtable-ref money-out-hash s-account)))
					;(gnc:debug "out:" (gnc-commodity-get-printname s-commodity)
					;	     (gnc-numeric-to-double s-amount)
					;	     (gnc-commodity-get-printname parent-currency)
					;	     (gnc-numeric-to-double s-value))
				   (if (not s-account-out-collector)
				       (begin
					 (set! s-account-out-collector (gnc:make-commodity-collector))
					 (account-hashtable-set! money-out-hash s-account
								 s-account-out-collector)
					 (set! money-out-accounts (cons s-account money-out-accounts))
					 )
				       )
				   (let ((s-report-value (to-report-currency parent-currency
									     s-value
									     (gnc-transaction-get-date-posted
									      parent))))
				     (money-out-collector 'add report-currency s-report-value)
				     (s-account-out-collector 'add report-currency s-report-value))
				   )
				 )
			     )
			   )
		       )
		   )
		 )
	       (xaccTransGetSplitList parent)
	       )
	      )
	    )
	)
      )

    (define (calc-money-in-out-internal accounts)
      (for-each work-per-split all-splits))

    ;; And calculate
    (calc-money-in-out-internal accounts)
    ;; Return an association list of results
    (list (cons 'money-in-accounts money-in-accounts)
	  (cons 'money-in-alist (hash-map->list (lambda (k v) (list k v)) money-in-hash))
	  (cons 'money-in-collector money-in-collector)
	  (cons 'money-out-accounts money-out-accounts)
	  (cons 'money-out-alist (hash-map->list (lambda (k v) (list k v)) money-out-hash))
	  (cons 'money-out-collector money-out-collector))))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "f8748b813fab4220ba26e743aedf38da"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cash-flow-options-generator
 'renderer cash-flow-renderer)
