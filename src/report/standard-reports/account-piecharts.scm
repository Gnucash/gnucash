;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-piecharts.scm: shows piechart of accounts
;;  
;; By Robert Merkel (rgmerk@mira.net) 
;; and Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report account-piecharts))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (ice-9 slib))
(use-modules (ice-9 regex))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define menuname-income (N_ "Income Piechart"))
(define menuname-expense (N_ "Expense Piechart"))
(define menuname-assets (N_ "Asset Piechart"))
(define menuname-liabilities (N_ "Liability Piechart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income
  (N_ "Shows a piechart with the Income per given time interval"))
(define menutip-expense 
  (N_ "Shows a piechart with the Expenses per given time interval"))
(define menutip-assets 
  (N_ "Shows a piechart with the Assets balance at a given time"))
(define menutip-liabilities 
  (N_ "Shows a piechart with the Liabilities \
balance at a given time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income Accounts"))
(define reportname-expense (N_ "Expense Accounts"))
(define reportname-assets (N_ "Assets"))
(define reportname-liabilities (N_ "Liabilities"))

(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long account names"))
(define optname-show-total (N_ "Show Totals"))
(define optname-slices (N_ "Maximum Slices"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-sort-method (N_ "Sort Method"))

;; The option-generator. The only dependance on the type of piechart
;; is the list of account types that the account selection option
;; accepts.
(define (options-generator account-types reverse-balance? do-intervals?)
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-internal-option "__report" "reverse-balance?" reverse-balance?))

    (if do-intervals?
        (gnc:options-add-date-interval!
         options gnc:pagename-general
         optname-from-date optname-to-date "a")
        (gnc:options-add-report-date!
         options gnc:pagename-general
         optname-to-date "a"))

    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "b")
    
    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "c" 'weighted-average)

    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (gnc:filter-accountlist-type 
         account-types
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (gnc:filter-accountlist-type
               account-types
               accounts)))
      #t))

    (gnc:options-add-account-levels! 
     options gnc:pagename-accounts optname-levels "b" 
     (N_ "Show accounts to this depth and not further") 
     2)

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-total
      "b" (N_ "Show the total balance in legend?") #t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "c" (N_ "Maximum number of slices in pie") 7
      2 24 0 1))

    (gnc:options-add-plot-size!
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "d" 500 350)

    (gnc:options-add-sort-method! 
     options gnc:pagename-display
     optname-sort-method "e" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))


;; The rendering function. Since it works for a bunch of different
;; account settings, you have to give the reportname, the
;; account-types to work on and whether this report works on
;; intervals as arguments.
(define (piechart-renderer report-obj reportname report-guid
                           account-types do-intervals?)
  
  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option 
      (gnc:report-options report-obj) section name)))
  
  (gnc:report-starting reportname)

  ;; Get all options
  (let ((to-date-tp (gnc:timepair-end-day-time 
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-to-date))))
        (from-date-tp (if do-intervals?
                          (gnc:timepair-start-day-time 
                           (gnc:date-option-absolute-time 
                            (get-option gnc:pagename-general 
					optname-from-date)))
                          '()))
        (accounts (get-option gnc:pagename-accounts optname-accounts))
        (account-levels (get-option gnc:pagename-accounts optname-levels))
        (report-currency (get-option gnc:pagename-general
				     optname-report-currency))
        (price-source (get-option gnc:pagename-general
                                  optname-price-source))
        (report-title (get-option gnc:pagename-general 
				  gnc:optname-reportname))

        (show-fullname? (get-option gnc:pagename-display optname-fullname))
        (show-total? (get-option gnc:pagename-display optname-show-total))
        (max-slices (inexact->exact
		     (get-option gnc:pagename-display optname-slices)))
        (height (get-option gnc:pagename-display optname-plot-height))
        (width (get-option gnc:pagename-display optname-plot-width))
	(sort-method (get-option gnc:pagename-display optname-sort-method))
	(reverse-balance? (get-option "__report" "reverse-balance?"))

	(work-done 0)
	(work-to-do 0)
        (document (gnc:make-html-document))
        (chart (gnc:make-html-piechart))
        (topl-accounts (gnc:filter-accountlist-type 
                        account-types
                        (gnc-account-get-children-sorted
                         (gnc-get-current-root-account)))))

    ;; Returns true if the account a was selected in the account
    ;; selection option.
    (define (show-acct? a)
      (member a accounts))
    
    ;; Calculates the net balance (profit or loss) of an account
    ;; over the selected reporting period. If subaccts? == #t, all
    ;; subaccount's balances are included as well. Returns a
    ;; commodity-collector.
    (define (profit-fn account subaccts?)
      (if do-intervals?
          (gnc:account-get-comm-balance-interval
           account from-date-tp to-date-tp subaccts?)
          (gnc:account-get-comm-balance-at-date
           account to-date-tp subaccts?)))

    ;; Define more helper variables.
    (let* ((exchange-fn (gnc:case-exchange-fn 
                         price-source report-currency to-date-tp))
           (tree-depth (if (equal? account-levels 'all)
                           (gnc:get-current-account-tree-depth)
                           account-levels))
           (combined '())
           (other-anchor "")
           (print-info (gnc-commodity-print-info report-currency #t)))

      ;; Converts a commodity-collector into one single double
      ;; number, depending on the report currency and the
      ;; exchange-fn calculated above. Returns the absolute value
      ;; as double.
      (define (collector->double c)
        ;; Future improvement: Let the user choose which kind of
        ;; currency combining she want to be done. Right now
        ;; everything foreign gets converted
        ;; (gnc:sum-collector-commodity) based on the weighted
        ;; average of all past transactions.
        (gnc-numeric-to-double
         (gnc:gnc-monetary-amount
          (gnc:sum-collector-commodity 
           c report-currency 
           exchange-fn))))

      (define (count-accounts current-depth accts)
	(if (< current-depth tree-depth)
	    (let ((sum 0))
	      (for-each
	       (lambda (a)
		 (set! sum (+ sum (+ 1 (count-accounts (+ 1 current-depth)
						       (gnc-account-get-children a))))))
	       accts)
	      sum)
	    (length (filter show-acct? accts))))

      ;; Calculates all account's balances. Returns a list of
      ;; balance <=> account pairs, like '((10.0 Earnings) (142.5
      ;; Gifts)). If current-depth >= tree-depth, then the balances
      ;; are calculated *with* subaccount's balances. Else only the
      ;; current account is regarded. Note: All accounts in accts
      ;; and all their subaccounts are processed, but a balances is
      ;; calculated and returned *only* for those accounts where
      ;; show-acct? is true. This is necessary because otherwise we
      ;; would forget an account that is selected but not its
      ;; parent.
      (define (traverse-accounts current-depth accts)
        (if (< current-depth tree-depth)
            (let ((res '()))
              (for-each
               (lambda (a)
                 (begin
		   (set! work-done (+ 1 work-done))
		   (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
                   (if (show-acct? a)
                       (set! res (cons (list (collector->double 
                                              (profit-fn a #f)) a)
                                       res)))
                   (set! res (append
                              (traverse-accounts
                               (+ 1 current-depth)
                               (gnc-account-get-children a))
                              res))))
               accts)
              res)
            (map
             (lambda (a)
	       (set! work-done (+ 1 work-done))
	       (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
               (list (collector->double (profit-fn a #t)) a))
             (filter show-acct? accts))))

      (define (fix-signs combined)
        (map (lambda (pair)
               (if (reverse-balance? (cadr pair))
                   (cons (- (car pair)) (cdr pair))
                   pair))
	     combined))

      ;; Now do the work here.

      (if (not (null? accounts))
          (begin
	    (set! work-to-do (count-accounts 1 topl-accounts))
            (set! combined
		  (sort (filter (lambda (pair) (not (>= 0.0 (car pair))))
				(fix-signs
                                 (traverse-accounts 1 topl-accounts)))
			(cond
			 ((eq? sort-method 'acct-code)
			  (lambda (a b) 
			    (string<? (xaccAccountGetCode (cadr a))
				      (xaccAccountGetCode (cadr b)))))
			 ((eq? sort-method 'alphabetical)
			  (lambda (a b) 
			    (string<? ((if show-fullname?
					   gnc-account-get-full-name
					   xaccAccountGetName) (cadr a))
				      ((if show-fullname?
					   gnc-account-get-full-name
					   xaccAccountGetName) (cadr b)))))
			 (else
			  (lambda (a b) (> (car a) (car b)))))))

            ;; if too many slices, condense them to an 'other' slice
            ;; and add a link to a new pie report with just those
            ;; accounts
            (if (> (length combined) max-slices)
                (let* ((start (take combined (- max-slices 1)))
                       (finish (drop combined (- max-slices 1)))
                       (sum (apply + (unzip1 finish))))
                  (set! combined
                        (append start
                                (list (list sum (_ "Other")))))
                  (let ((options (gnc:make-report-options report-guid))
                        (id #f))
                    ;; now copy all the options
                    (gnc:options-copy-values (gnc:report-options report-obj)
                                             options)
                    ;; and set the destination accounts
                    (gnc:option-set-value
                     (gnc:lookup-option options gnc:pagename-accounts 
                                        optname-accounts)
                     (map cadr finish))
                    (set! id (gnc:make-report report-guid options))
                    ;; set the URL.
                    (set! other-anchor (gnc:report-anchor-text id)))))
            
            ;; set the URLs; the slices are links to other reports
            (let 
                ((urls
                  (map 
                   (lambda (pair)
                     (if (string? (cadr pair))
                         other-anchor
                         (let* ((acct (cadr pair))
                                (subaccts (gnc-account-get-children acct)))
                           (if (null? subaccts)
                               ;; if leaf-account, make this an anchor
                               ;; to the register.
                               (gnc:account-anchor-text (cadr pair))
                               ;; if non-leaf account, make this a link
                               ;; to another report which is run on the
                               ;; immediate subaccounts of this account
                               ;; (and including this account).
                               (gnc:make-report-anchor
                                report-guid
                                report-obj
                                (list
                                 (list gnc:pagename-accounts optname-accounts
                                       (cons acct subaccts))
                                 (list gnc:pagename-accounts optname-levels
                                       (+ 1 tree-depth))
                                 (list gnc:pagename-general 
                                       gnc:optname-reportname
                                       ((if show-fullname?
                                            gnc-account-get-full-name
                                            xaccAccountGetName) acct))))))))
                   combined)))
              (gnc:html-piechart-set-button-1-slice-urls! 
               chart urls)
              (gnc:html-piechart-set-button-1-legend-urls! 
               chart urls))

            (if 
             (not (null? combined))
             (begin
               (gnc:html-piechart-set-title!
                chart report-title)
               (gnc:html-piechart-set-width! chart width)
               (gnc:html-piechart-set-height! chart height)
               (gnc:html-piechart-set-data! chart (unzip1 combined))
               (gnc:html-piechart-set-colors!
                chart (gnc:assign-colors (length combined)))

               (gnc:html-piechart-set-subtitle!
                chart (string-append
                       (if do-intervals?
                           (sprintf #f
                                    (_ "%s to %s")
                                    (gnc-print-date from-date-tp)
                                    (gnc-print-date to-date-tp))
                           (sprintf #f
                                    (_ "Balance at %s")
                                    (gnc-print-date to-date-tp)))
                       (if show-total?
                           (let ((total (apply + (unzip1 combined))))
                             (sprintf
                              #f ": %s"
                              (xaccPrintAmount
                               (double-to-gnc-numeric
                                total
                                (gnc-commodity-get-fraction report-currency)
                                GNC-RND-ROUND)
                               print-info)))
                           "")))
               
               (let ((legend-labels
                      (map 
                       (lambda (pair)
                         (string-append
			  (regexp-substitute/global #f "&"
                           (if (string? (cadr pair))
			       (cadr pair)
			       ((if show-fullname?
				    gnc-account-get-full-name
				    xaccAccountGetName) (cadr pair)))
			       'pre " " (_ "and") " " 'post)
			   (if show-total?
			       (string-append 
				" - "
				(xaccPrintAmount
				 (double-to-gnc-numeric
				  (car pair)
				  (gnc-commodity-get-fraction report-currency)
				  GNC-RND-ROUND)
				 print-info))
			       "")))
                       combined)))
                 (gnc:html-piechart-set-labels! chart legend-labels))

               (gnc:html-document-add-object! document chart))

             (gnc:html-document-add-object!
              document
	      (gnc:html-make-empty-data-warning
	       report-title (gnc:report-id report-obj)))))

          (gnc:html-document-add-object!
           document
	   (gnc:html-make-no-account-warning 
	    report-title (gnc:report-id report-obj))))

      (gnc:report-finished)
      document)))

(for-each 
 (lambda (l)
   (let ((tip-and-rev (cddddr l)))
     (gnc:define-report
      'version 1
      'name (car l)
      'report-guid (car (reverse l))
      'menu-path (if (caddr l)
		     (list gnc:menuname-income-expense)
		     (list gnc:menuname-asset-liability)) 
      'menu-name (cadddr l) 
      'menu-tip (car tip-and-rev)
      'options-generator (lambda () (options-generator (cadr l)  
						       (cadr tip-and-rev)
						       (caddr l)))
      'renderer (lambda (report-obj)
		  (piechart-renderer report-obj 
				     (car l) 
				     (car (reverse l))
				     (cadr l)
				     (caddr l))))))
 (list 
  ;; reportname, account-types, do-intervals?, 
  ;; menu-reportname, menu-tip
  (list reportname-income (list ACCT-TYPE-INCOME) #t menuname-income menutip-income (lambda (x) #t) "e1bd09b8a1dd49dd85760db9d82b045c")
  (list reportname-expense (list ACCT-TYPE-EXPENSE) #t menuname-expense menutip-expense (lambda (x) #f) "9bf1892805cb4336be6320fe48ce5446")
  (list reportname-assets
        (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
              ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
              ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
              ACCT-TYPE-CURRENCY)
        #f menuname-assets menutip-assets (lambda (x) #f) "5c7fd8a1fe9a4cd38884ff54214aa88a")
  (list reportname-liabilities 
        (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
              ACCT-TYPE-CREDITLINE)
        #f menuname-liabilities menutip-liabilities (lambda (x) #t) "3fe6dce77da24c66bdc8f8efdea7f9ac")))
