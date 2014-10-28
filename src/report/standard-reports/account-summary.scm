;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-summary.scm : account listing/chart of accounts
;; 
;; Rewritten 2004.07.27 by David Montenegro <sunrise2000@comcast.net>
;;   same license & restrictions apply
;; 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
;; Copyright 2000-2001 Bill Gribble <grib@gnumatic.com>
;;
;; Even older original version by  Terry D. Boldt (tboldt@attglobal.net>
;;   Author makes no implicit or explicit guarantee of accuracy of
;;   these calculations and accepts no responsibility for direct
;;   or indirect losses incurred as a result of using this software.
;; 
;;  * BUGS:
;;    
;;    Does not currently provide all possible account attributes.
;;    
;;    Table does not currently use row style attributes.
;;    
;;    Progress bar functionality is currently mostly broken.
;;    
;;    This code makes the assumption that you want your account
;;    summary to no more than daily resolution.
;;    
;;    The Company Name field does not currently default to the name
;;    in (gnc-get-current-book).
;;    
;;    The variables in this code could use more consistent naming.
;;    
;;    See also all the "FIXME"s in the code.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports account-summary))

(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

;; account summary report prints a table of account information,
;; optionally with clickable links to open the corresponding register
;; window.

(define reportname (N_ "Account Summary"))

(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-party-name (N_ "Company name"))
(define opthelp-party-name (N_ "Name of company/individual."))

(define optname-date (N_ "Date"))
;; FIXME this needs an indent option

(define optname-accounts (N_ "Accounts"))
(define opthelp-accounts
  (N_ "Report on these accounts, if display depth allows."))
(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit
  (N_ "Maximum number of levels in the account tree displayed."))
(define optname-bottom-behavior (N_ "Depth limit behavior"))
(define opthelp-bottom-behavior
  (N_ "How to treat accounts which exceed the specified depth limit (if any)."))

(define optname-parent-balance-mode (N_ "Parent account balances"))
(define optname-parent-total-mode (N_ "Parent account subtotals"))

(define optname-show-zb-accts (N_ "Include accounts with zero total balances"))
(define opthelp-show-zb-accts
  (N_ "Include accounts with zero total (recursive) balances in this report."))
(define optname-omit-zb-bals (N_ "Omit zero balance figures"))
(define opthelp-omit-zb-bals
  (N_ "Show blank space in place of any zero balances which would be shown."))

(define optname-use-rules (N_ "Show accounting-style rules"))
(define opthelp-use-rules
  (N_ "Use rules beneath columns of added numbers like accountants do."))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links (N_ "Shows each account in the table as a hyperlink to its register window."))

(define optname-show-account-bals (N_ "Account Balance"))
(define opthelp-show-account-bals (N_ "Show an account's balance."))
(define optname-show-account-code (N_ "Account Code"))
(define opthelp-show-account-code (N_ "Show an account's account code."))
(define optname-show-account-type (N_ "Account Type"))
(define opthelp-show-account-type (N_ "Show an account's account type."))
(define optname-show-account-desc (N_ "Account Description"))
(define opthelp-show-account-desc (N_ "Show an account's description."))
(define optname-show-account-notes (N_ "Account Notes"))
(define opthelp-show-account-notes (N_ "Show an account's notes."))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account."))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))

;; FIXME: add more account metadata options!

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; options generator
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accsum-options-generator)
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

    ;; date at which to report balance
    (gnc:options-add-report-date!
     options gnc:pagename-general optname-date "c")

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
     (gnc:make-multichoice-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior
      'summarize
      (list (vector 'summarize
		    (N_ "Recursive Balance")
		    (N_ "Show the total balance, including balances in subaccounts, of any account at the depth limit."))
	    (vector 'flatten
		    (N_ "Raise Accounts")
		    (N_ "Shows accounts deeper than the depth limit at the depth limit."))
	    (vector 'truncate
		    (N_ "Omit Accounts")
		    (N_ "Disregard completely any accounts deeper than the depth limit."))
	    )
      )
     )
    
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
      gnc:pagename-display optname-show-account-bals
      "g" opthelp-show-account-bals #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-account-code
      "h" opthelp-show-account-code #t))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-account-desc
      "i" opthelp-show-account-desc #f))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-account-type
      "j" opthelp-show-account-type #f))
    (add-option 
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-account-notes
      "k" opthelp-show-account-notes #f))
    
    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-display)
    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accsum-renderer
;; set up the table and put it in an html document
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accsum-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))
  
  (gnc:report-starting reportname)
  
  (let* (
	 (report-title (get-option gnc:pagename-general optname-report-title))
	 (company-name (get-option gnc:pagename-general optname-party-name))
         (date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-date))))
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
         (use-links? (get-option gnc:pagename-display
				     optname-account-links))
         (use-rules? (get-option gnc:pagename-display
				    optname-use-rules))
         (show-account-code? (get-option gnc:pagename-display
					 optname-show-account-code))
         (show-account-type? (get-option gnc:pagename-display
					 optname-show-account-type))
         (show-account-desc? (get-option gnc:pagename-display
					 optname-show-account-desc))
         (show-account-notes? (get-option gnc:pagename-display
					  optname-show-account-notes))
         (show-account-bals? (get-option gnc:pagename-display
					 optname-show-account-bals))
	 (indent 0)
	 (tabbing #f)
	 
         (doc (gnc:make-html-document))
	 ;; just in case we need this information...
         (tree-depth (if (equal? depth-limit 'all)
                         (gnc:get-current-account-tree-depth) 
			 depth-limit))
         ;; exchange rates calculation parameters
	 (exchange-fn
	  (gnc:case-exchange-fn price-source report-commodity date-tp))
	 )
    
    (gnc:html-document-set-title! 
     doc (string-append company-name " " report-title " "
			(gnc-print-date date-tp))
     )
    
    (if (null? accounts)
	
	;; error condition: no accounts specified
	;; is this *really* necessary??  i'd be fine with an all-zero
	;; account summary that would, technically, be correct....
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  reportname (gnc:report-id report-obj)))
	
	;; otherwise, generate the report...
	(let* (
	       (chart-table #f)                    ;; gnc:html-acct-table
	       (hold-table (gnc:make-html-table))  ;; temporary gnc:html-table
	       (build-table (gnc:make-html-table)) ;; gnc:html-table reported
	       (get-total-balance-fn
		(lambda (account)
		  (gnc:account-get-comm-balance-at-date 
		   account date-tp #f)))
	       (table-env                      ;; parameters for :make-
		(list
		 (list 'start-date #f)
		 (list 'end-date date-tp)
		 (list 'display-tree-depth tree-depth)
		 (list 'depth-limit-behavior bottom-behavior)
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
	  (params                         ;; and -add-account-
		(list
		 (list 'parent-account-balance-mode parent-balance-mode)
		 (list 'zero-balance-display-mode (if omit-zb-bals?
						      'omit-balance
						      'show-balance))
		 (list 'multicommodity-mode (if show-fcur? 'table #f))
		 (list 'rule-mode use-rules?)
		  )
		)
	  
	  ;; FIXME: this filtering is trivial and could probably be
	  ;; greatly simplified (it just collects all selected
	  ;; accounts)...
	  (split-up-accounts (gnc:decompose-accountlist accounts))
	  (all-accounts
	   (append (assoc-ref split-up-accounts ACCT-TYPE-INCOME)
		   (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)
		   (assoc-ref split-up-accounts ACCT-TYPE-ASSET)
		   (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY)
		   (assoc-ref split-up-accounts ACCT-TYPE-EQUITY)
		   ))
	  ;; (all-accounts (map (lambda (X) (cadr X)) split-up-accounts))
	  ;; ^ will not do what we want
	  
	  (account-cols 0)
	  (table-rows 0)
	  (cur-col 0)
	  (foo #f) ;; a dummy variable for when i'm too lazy to type much
	  (add-col #f) ;; thunk to add a column to build-table
	  (hold-table-width 0)
	  )
	  
	  (set! chart-table
		(gnc:make-html-acct-table/env/accts
		 table-env all-accounts))
	  (gnc:html-table-add-account-balances
	   hold-table chart-table params)
	  (set! table-rows (or (gnc:html-acct-table-num-rows chart-table) 0))
	  (set! account-cols
		(if (zero? table-rows)
		    0
		    (or (car (assoc-ref
			      (gnc:html-acct-table-get-row-env chart-table 0)
			      'account-cols))
			0)
		    )
		)
	  
	  (set! add-col
		(lambda(key)
		  (let ((row 0)
			(row-env #f)
			)
		    (while (< row table-rows)
			   (set! row-env
				 (gnc:html-acct-table-get-row-env
				  chart-table row))
			   (gnc:html-table-set-cell!
			    build-table (+ row 1) cur-col ;; +1 for headers
			    (car (assoc-ref row-env key))
			    )
			   (set! row (+ row 1))
			   )
		    )
		  (set! cur-col (+ cur-col 1))
		  )
		)

	  ;; place the column headers
	  (gnc:html-table-append-row!
	   build-table
	   (append
	    (if show-account-code? (list (_ "Code")) '())
	    (if show-account-type? (list (_ "Type")) '())
	    (if show-account-desc? (list (_ "Description")) '())
	    (list (_ "Account title"))
	    )
	   )
	  ;; add any fields to be displayed before the account name
	  (if show-account-code? (add-col 'account-code))
	  (if show-account-type? (add-col 'account-type-string))
	  (if show-account-desc? (add-col 'account-description))
	  
	  (set! hold-table-width
		(if show-account-bals?
		    (gnc:html-table-num-columns hold-table)
		    account-cols
		    )
		)
          (if show-account-bals?
              (gnc:html-table-set-cell/tag!
               build-table 0 (+ cur-col account-cols) "number-header"
	       (_ "Balance"))
              )
	  (let ((row 0))
	    (while (< row table-rows)
		   (gnc:html-table-set-row-markup! build-table (+ row 1)
						   (gnc:html-table-row-markup hold-table row))
		   (let ((col 0))
		     (while (< col hold-table-width)
			    (gnc:html-table-set-cell!
			     build-table (+ row 1) (+ cur-col col)
			     (gnc:html-table-get-cell hold-table row col)
			     )
			    (set! col (+ col 1))
			    )
		     )
		   (set! row (+ row 1))
		   )
	    )
	  (set! cur-col (+ cur-col hold-table-width))
	  (if show-account-notes?
	      (begin
		(gnc:html-table-set-cell/tag!
		 build-table 0 cur-col "text-cell"
		 (_ "Notes"))
		(add-col 'account-notes)
		)
	      )
	  
	  (gnc:html-document-add-object! doc build-table)
	  
          ;; add currency information
          (if show-rates?
              (gnc:html-document-add-object! 
               doc ;;(gnc:html-markup-p
               (gnc:html-make-exchangerates 
                report-commodity exchange-fn 
                (append-map
                 (lambda (a)
		   (gnc-account-get-descendants-sorted a))
                 accounts))))
	  )
	)
    
    (gnc:report-finished)
    doc)
  )

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "3298541c236b494998b236dfad6ad752"
 'options-generator accsum-options-generator
 'renderer accsum-renderer)

;; END

