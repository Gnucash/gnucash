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

(use-modules (gnucash printf))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

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
    
    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)      

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global objects
;; objects used by the cash-flow-calculator and the document-renderer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define money-in-alist '())
(define money-in-accounts '())
(define money-in-collector (gnc:make-commodity-collector))

(define money-out-accounts '())
(define money-out-alist '())
(define money-out-collector (gnc:make-commodity-collector))

(define time-exchange-fn #f)

(define work-done 0)
(define work-to-do 0)

;; is account in list of accounts?
(define (same-account? a1 a2)
  (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2)))

(define (same-split? s1 s2)
  (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

(define account-in-list?
  (lambda (account accounts)
    (cond
      ((null? accounts) #f)
      ((same-account? (car accounts) account) #t)
      (else (account-in-list? account (cdr accounts))))))

(define account-in-alist
  (lambda (account alist)
    (cond
      ((null? alist) #f)
      ((same-account? (caar alist) account) (car alist))
      (else (account-in-alist account (cdr alist))))))


;; ------------------------------------------------------------------
;; cash-flow-calculator
;; do the cash flow calculations
;; ------------------------------------------------------------------

;; function to add inflow and outflow of money
(define (calc-money-in-out accounts to-date-tp from-date-tp report-currency)

  (let* (
	  (splits-to-do (gnc:accounts-count-splits accounts))
        )

    (define split-in-list?
      (lambda (split splits)
        (cond
          ((null? splits) #f)
          ((same-split? (car splits) split) #t)
          (else (split-in-list? split (cdr splits))))))

    ;; Helper function to convert currencies
    (define (to-report-currency currency amount date)
      (gnc:gnc-monetary-amount
        (time-exchange-fn
          (gnc:make-gnc-monetary currency amount)
	  report-currency
          date
        )
      )
    )

    ;; ------------------------------------------------------------------
    ;; process all selected accounts
    ;; ------------------------------------------------------------------
    (for-each
      (lambda (account)
        (let* (
                (name (xaccAccountGetName account))
                (curr-commodity (xaccAccountGetCommodity account))
	        (seen-split-list '())
              )
          ;(gnc:debug "calc-money-in-out-internal---" name "---" (gnc-commodity-get-printname curr-commodity))

          ;; -------------------------------------
          ;; process all splits of current account
          ;; -------------------------------------
          (for-each
            (lambda (split)
              ;; ----------------------------------------------------
              ;; update progress indicator
              ;; ----------------------------------------------------
              (set! work-done (+ 1 work-done))
              (gnc:report-percent-done (* 85 (/ work-done splits-to-do)))
              ;; ----------------------------------------------------
              ;; only splits that are within the specified time range
              ;; ----------------------------------------------------
              (let* (
                     (parent (xaccSplitGetParent split))
                     (parent-date-posted (gnc-transaction-get-date-posted parent))
                   )
                (if (and
                      (gnc:timepair-le parent-date-posted to-date-tp)
                      (gnc:timepair-ge parent-date-posted from-date-tp)
                    )
                  (let* (
                          (parent-currency   (xaccTransGetCurrency    parent))
                          (transaction-value (gnc-numeric-zero))
                          (split-value       (xaccSplitGetValue split))
                        )
                    ;(gnc:debug (xaccTransGetDescription parent)
                    ;           " - "
                    ;           (gnc-commodity-get-printname parent-currency))
                    ;; -------------------------------------------------------------
                    ;; get the transaction value - needed to fix bug 622778
                    ;; -------------------------------------------------------------
                    (for-each
                      (lambda (parent-split)
                        (let* (
                                (psv (xaccSplitGetValue parent-split))
                              )
                          (if (gnc-numeric-positive-p psv) ;; meaning: if (psv>0)
                            (set! transaction-value
                              (gnc-numeric-add transaction-value psv GNC-DENOM-AUTO GNC-DENOM-LCD)
                            )
                          )
                        )
                      )
                      (xaccTransGetSplitList parent)
                    )
                    ;; -----------------------------------------
                    ;; process all splits of current transaction
                    ;; -----------------------------------------
                    (for-each
                      (lambda (s)
                        (let* (
                                (s-account   (xaccSplitGetAccount s))
                                (s-value     (xaccSplitGetValue s))
                                (s-commodity (xaccAccountGetCommodity s-account))
                              )
                          ;; -----------------------------------------
	                  ;; Check if this is a dangling split and print a warning
                          ;; -----------------------------------------
                          (if (null? s-account)
			    (display
			     (string-append "WARNING: s-account is NULL for split: " (gncSplitGetGUID s) "\n")
                            )
                          )
                          ;(gnc:debug (xaccAccountGetName s-account))
                          ;; ----------------------------------------------------------------------
                          ;; only splits from or to accounts outside the user selected account list
                          ;; ----------------------------------------------------------------------
                          (if (and	 ;; make sure we don't have
                                (not (null? s-account)) ;;  any dangling splits
			        (not (account-in-list? s-account accounts))
                                ;; only consider splits of opposite sign
                                (gnc-numeric-negative-p (gnc-numeric-mul s-value split-value GNC-DENOM-AUTO GNC-DENOM-REDUCE))
                              )
                            (if (not (split-in-list? s seen-split-list))
                              (let (
                                     (split-transaction-ratio (gnc-numeric-zero))
                                   )
                                ;; -------------------------------------------------------------
                                ;; get the share of the current split from the total transaction- needed to fix bug 622778
                                ;; -------------------------------------------------------------
                                (set! split-transaction-ratio
                                  (if (gnc-numeric-zero-p transaction-value)
                                    ;; If the transaction-value remained zero, then the transaction is
                                    ;; either 0 or we have a negative one-split-transaction.
                                    ;; Either way, it means that we can set the transaction value equal to the split-value,
                                    ;; and, in turn, the transaction ratio is 1.
                                    (gnc:make-gnc-numeric 1 1)
                                    ;; else
                                    (gnc-numeric-abs
                                      (gnc-numeric-div split-value transaction-value GNC-DENOM-AUTO GNC-DENOM-REDUCE)
                                    )
                                  )
                                )
				(set! s-value (gnc-numeric-mul split-transaction-ratio s-value 
				                               (gnc-commodity-get-fraction parent-currency) GNC-RND-ROUND))
			        (set! seen-split-list (cons s seen-split-list))
			        (if (gnc-numeric-negative-p s-value)
                                  ;; -----------------------------------------------
                                  ;; collect the incoming flow
                                  ;; -----------------------------------------------
			          (let (
                                         (pair (account-in-alist s-account money-in-alist))
                                       )
				    ;(gnc:debug "in:" (gnc-commodity-get-printname s-commodity)
				    ;  (gnc-numeric-to-double (xaccSplitGetAmount s))
				    ;  (gnc-commodity-get-printname parent-currency)
				    ;  (gnc-numeric-to-double s-value))
				    (if (not pair)
				      (begin
				        (set! pair (list s-account (gnc:make-commodity-collector)))
				        (set! money-in-alist (cons pair money-in-alist))
				        (set! money-in-accounts (cons s-account money-in-accounts))
				        ;(gnc:debug money-in-alist)
				      )
			            )
				    (let (
                                           (s-account-in-collector (cadr pair))
				           (s-report-value
                                             (to-report-currency
                                               parent-currency
				               (gnc-numeric-neg s-value)
					       parent-date-posted
                                             )
                                           )
                                         )
	                              (money-in-collector 'add report-currency s-report-value)
				      (s-account-in-collector 'add report-currency s-report-value)
                                    )
		                  )
                                  ;; else
                                  ;; -----------------------------------------------
                                  ;; collect the outgoing flow
                                  ;; -----------------------------------------------
			          (let (
                                         (pair (account-in-alist s-account money-out-alist))
                                       )
                                    ;(gnc:debug "out:" (gnc-commodity-get-printname s-commodity)
                                    ;	     (gnc-numeric-to-double (xaccSplitGetAmount s))
                                    ;	     (gnc-commodity-get-printname parent-currency)
                                    ;	     (gnc-numeric-to-double s-value))
				    (if (not pair)
				      (begin
                                       (set! pair (list s-account (gnc:make-commodity-collector)))
                                       (set! money-out-alist (cons pair money-out-alist))
                                       (set! money-out-accounts (cons s-account money-out-accounts))
                                       ;(gnc:debug money-out-alist)
				      )
				    )
				    (let (
                                           (s-account-out-collector (cadr pair))
				           (s-report-value
				             (to-report-currency
					       parent-currency
					       s-value
					       parent-date-posted
                                             )
                                           )
                                         )
				      (money-out-collector 'add report-currency s-report-value)
				      (s-account-out-collector 'add report-currency s-report-value)
                                    )
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
            (xaccAccountGetSplitList account)
          )
        )
      )
      accounts
    )
  )
)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document-renderer
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
         (row-num 0)
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

    ;; helper for sorting of account list
    (define (account-full-name<? a b)
      (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

    ;; return maximum depth over accounts and their children, if any
    (define (accounts-get-children-depth accounts)
      (apply max
	     (map (lambda (acct)
		    (let ((acct-depth (gnc-account-get-current-depth acct)))
		      (+ acct-depth (- (gnc-account-get-tree-depth acct) 1))))
		  accounts)))


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
               (account-disp-list '())

               (money-diff-collector (gnc:make-commodity-collector))
	       (commodity-list #f))


	  ;; Get an exchange function that will convert each transaction using the
	  ;; nearest available exchange rate if that is what is specified
	  (set! commodity-list (gnc:accounts-get-commodities
				accounts
				report-currency))
	  (set! time-exchange-fn (gnc:case-exchange-time-fn
				  price-source report-currency
				  commodity-list to-date-tp
				  0 0))

          ;; -----------------------------------------------------------------
          ;; run the cash flow calculation
          ;; -----------------------------------------------------------------
          (set! money-in-alist '())
          (set! money-in-accounts '())
          (set! money-in-collector (gnc:make-commodity-collector))
          (set! money-out-accounts '())
          (set! money-out-alist '())
          (set! money-out-collector (gnc:make-commodity-collector))
          (calc-money-in-out accounts to-date-tp from-date-tp report-currency)

          ;; -----------------------------------------------------------------
          ;; present the result
          ;; -----------------------------------------------------------------

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

        
        
        ;; error condition: no accounts specified
        
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj))))

    (gnc:report-finished)
    doc))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "f8748b813fab4220ba26e743aedf38da"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cash-flow-options-generator
 'renderer cash-flow-renderer)
