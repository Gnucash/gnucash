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

(define-module (gnucash report standard-reports account-piecharts))

(use-modules (gnucash utilities)) 
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(define menuname-income (N_ "Income Piechart"))
(define menuname-expense (N_ "Expense Piechart"))
(define menuname-assets (N_ "Asset Piechart"))
(define menuname-securities (N_ "Security Piechart"))
(define menuname-liabilities (N_ "Liability Piechart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income
  (N_ "Shows a piechart with the Income per given time interval"))
(define menutip-expense 
  (N_ "Shows a piechart with the Expenses per given time interval"))
(define menutip-assets 
  (N_ "Shows a piechart with the Assets balance at a given time"))
(define menutip-securities
  (N_ "Shows a piechart with distribution of assets over securities"))
(define menutip-liabilities 
  (N_ "Shows a piechart with the Liabilities \
balance at a given time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income Accounts"))
(define reportname-expense (N_ "Expense Accounts"))
(define reportname-assets (N_ "Assets"))
(define reportname-securities (N_ "Securities"))
(define reportname-liabilities (N_ "Liabilities"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long names"))
(define optname-show-total (N_ "Show Totals"))
(define optname-show-percent (N_ "Show Percents"))
(define optname-slices (N_ "Maximum Slices"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

;; The option-generator. The only dependence on the type of piechart
;; is the list of account types that the account selection option
;; accepts.
(define (options-generator account-types reverse-balance? do-intervals? depth-based?)
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
     optname-price-source "c" 'pricedb-nearest)

    (if do-intervals?
        (add-option
         (gnc:make-multichoice-option
          gnc:pagename-general optname-averaging
          "f" opthelp-averaging
          'None
          (list (vector 'None
                        (N_ "No Averaging")
                        (N_ "Just show the amounts, without any averaging."))
                (vector 'YearDelta
                        (N_ "Yearly")
                        (N_ "Show the average yearly amount during the reporting period."))
                (vector 'MonthDelta
                        (N_ "Monthly")
                        (N_ "Show the average monthly amount during the reporting period."))
                (vector 'WeekDelta
                        (N_ "Weekly")
                        (N_ "Show the average weekly amount during the reporting period."))
                )
          ))
        )

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

    (if depth-based?
      (gnc:options-add-account-levels!
       options gnc:pagename-accounts optname-levels "b"
       (N_ "Show accounts to this depth and not further.")
       2))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a"
      (if depth-based?
        (N_ "Show the full account name in legend?")
        (N_ "Show the full security name in the legend?"))
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-total
      "b" (N_ "Show the total balance in legend?") #t))


     (add-option
      (gnc:make-simple-boolean-option
       gnc:pagename-display optname-show-percent
       "b" (N_ "Show the percentage in legend?") #t))


    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "c" (N_ "Maximum number of slices in pie.") 7
      2 24 0 1))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

    (gnc:options-add-sort-method! 
     options gnc:pagename-display
     optname-sort-method "e" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; Set slice URLs for the depth-based chart types.
(define (set-slice-urls!
          report-obj uuid show-fullname? tree-depth other-anchor accts chart)
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
                      uuid
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
         accts)))
    (gnc:html-piechart-set-button-1-slice-urls!
     chart urls)
    (gnc:html-piechart-set-button-1-legend-urls!
     chart urls)))

;; Get display name for account-based reports.
(define (display-name-accounts show-fullname? acc)
  ((if show-fullname?
       gnc-account-get-full-name
       xaccAccountGetName) acc))

;; Get display name for security-based report.
(define (display-name-security show-fullname? sec)
  ((if show-fullname?
       gnc-commodity-get-fullname
       gnc-commodity-get-mnemonic) sec))


;; Sort comparator for account-based reports.
(define (sort-comparator-accounts sort-method show-fullname?)
  (cond
   ((eq? sort-method 'acct-code)
    (lambda (a b)
      (string<? (xaccAccountGetCode (cadr a))
                (xaccAccountGetCode (cadr b)))))
   ((eq? sort-method 'alphabetical)
    (lambda (a b)
      (string<? (display-name-accounts show-fullname? (cadr a))
                (display-name-accounts show-fullname? (cadr b)))))
   (else
    (lambda (a b) (> (car a) (car b))))))

;; Sort comparator for security-based report.
(define (sort-comparator-security sort-method show-fullname?)
  (cond
   ((eq? sort-method 'acct-code)
    (lambda (a b)
      (string<? (gnc-commodity-get-mnemonic (cadr a))
                (gnc-commodity-get-mnemonic (cadr b)))))
   ((eq? sort-method 'alphabetical)
    (lambda (a b)
      (string<? (display-name-security show-fullname? (cadr a))
                (display-name-security show-fullname? (cadr b)))))
   (else
    (lambda (a b) (> (car a) (car b))))))

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
(define (traverse-accounts account-balance show-acct? work-to-do tree-depth
                           work-done current-depth accts)
  (if (< current-depth tree-depth)
      (let iter ((res '())
                 (remaining accts)
                 (cur-work-done work-done))
        (if (null? remaining)
            (cons cur-work-done res)
            (begin
              (gnc:report-percent-done (* 100 (/ cur-work-done (work-to-do))))
              (let* ((cur (car remaining))
                     (tail (cdr remaining))
                     (subaccts-data (traverse-accounts
                                      account-balance show-acct?
                                      work-to-do tree-depth
                                      cur-work-done
                                      (1+ current-depth)
                                      (gnc-account-get-children cur)))
                     (subaccts-work (car subaccts-data))
                     (subaccts (cdr subaccts-data)))
                (iter
                  (append
                    subaccts
                    (if (show-acct? cur)
                        (cons (list (account-balance cur #f) cur) res)
                        res))
                  tail
                  (1+ subaccts-work))))))
      (let* ((proc-account (lambda (a)
                              (set! work-done (1+ work-done))
                              (gnc:report-percent-done
                                (* 100 (/ work-done (work-to-do))))
                              (list (account-balance a #t) a)))
             (new-accts (map proc-account (filter show-acct? accts))))
        (cons work-done new-accts))))

;; Calculate balances to show grouped by security.  This works similarly
;; to traverse-accounts, but it does not consider the depth and also does not
;; construct data based on the accounts.  Instead, it builds up a map
;; indexed by securities and sums up all balances for each security.
(define (sum-securities account-balance show-acct? work-to-do tree-depth
                        work-done current-depth accts)

  (define table (make-hash-table))
  (define (add! sec balance)
    (let* ((key (gnc-commodity-get-unique-name sec))
           (handle (hash-create-handle! table key (cons 0 sec)))
           (val (cadr handle)))
      (hash-set! table key (cons (+ val balance) sec))))

  (define (traverse! remaining initial-work)
    (if (null? remaining)
      initial-work
      (let* ((cur (car remaining))
             (tail (cdr remaining))
             (cur-work-done (1+ initial-work))
             (subaccts (gnc-account-get-children cur)))
        (gnc:report-percent-done (* 100 (/ cur-work-done (work-to-do))))
        (if (show-acct? cur)
          (add! (xaccAccountGetCommodity cur) (account-balance cur #f)))
        (traverse! tail (traverse! subaccts cur-work-done)))))

  (define (translate key value)
    (list (car value) (cdr value)))

  (let ((final-work (traverse! accts work-done)))
    (cons final-work (hash-map->list translate table))))

;; The rendering function. Since it works for a bunch of different
;; account settings, you have to give the reportname, the
;; account-types to work on and whether this report works on
;; intervals as arguments.
(define (piechart-renderer report-obj reportname report-guid
                           account-types do-intervals? depth-based?
                           display-name sort-comparator get-data)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option 
      (gnc:report-options report-obj) section name)))
  
  (gnc:report-starting reportname)

  ;; Get all options
  (let ((to-date (gnc:time64-end-day-time 
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-to-date))))
        (from-date (if do-intervals?
                          (gnc:time64-start-day-time 
                           (gnc:date-option-absolute-time 
                            (get-option gnc:pagename-general 
					optname-from-date)))
                          '()))
        (accounts (get-option gnc:pagename-accounts optname-accounts))
        (account-levels
          (if depth-based?
            (get-option gnc:pagename-accounts optname-levels)
            'all))
        (report-currency (get-option gnc:pagename-general
				     optname-report-currency))
        (price-source (get-option gnc:pagename-general
                                  optname-price-source))
        (report-title (get-option gnc:pagename-general 
				  gnc:optname-reportname))
        (averaging-selection (if do-intervals?
                                 (get-option gnc:pagename-general
                                             optname-averaging)
                                 'None))

        (show-fullname? (get-option gnc:pagename-display optname-fullname))
        (show-total? (get-option gnc:pagename-display optname-show-total))
        (show-percent? (get-option gnc:pagename-display optname-show-percent))
        (max-slices (inexact->exact
		     (get-option gnc:pagename-display optname-slices)))
        (height (get-option gnc:pagename-display optname-plot-height))
        (width (get-option gnc:pagename-display optname-plot-width))
	(sort-method (get-option gnc:pagename-display optname-sort-method))
	(reverse-balance? (get-option "__report" "reverse-balance?"))

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
           account from-date to-date subaccts?)
          (gnc:account-get-comm-balance-at-date
           account to-date subaccts?)))

    ;; Define more helper variables.
    (let* ((exchange-fn (gnc:case-exchange-fn 
                         price-source report-currency to-date))
           (tree-depth (if (equal? account-levels 'all)
                           (gnc:get-current-account-tree-depth)
                           account-levels))
           (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
           (averaging-multiplier
            (if averaging-fraction-func
                ;; Calculate the divisor of the amounts so that an
                ;; average is shown
                (let* ((start-frac (averaging-fraction-func from-date))
                       (end-frac (averaging-fraction-func (+ 1 to-date)))
                       (diff (- end-frac start-frac)))
                  ;; Extra sanity check to ensure a positive number
                  (if (> diff 0)
                      (/ 1 diff)
                      1))
                ;; No interval-report, or no averaging interval chosen,
                ;; so just use the multiplier one
                1))
           ;; If there is averaging, the report-title is extended
           ;; accordingly.
           (report-title
            (case averaging-selection
              ((YearDelta) (string-append report-title " " (_ "Yearly Average")))
              ((MonthDelta) (string-append report-title " " (_ "Monthly Average")))
              ((WeekDelta) (string-append report-title " " (_ "Weekly Average")))
              (else report-title)))
           (combined '())
           (other-anchor "")
           (print-info (gnc-commodity-print-info report-currency #t)))

      ;; Converts a commodity-collector into one single double
      ;; number, depending on the report's currency and the
      ;; exchange-fn calculated above. Returns the absolute value
      ;; as double, multiplied by the averaging-multiplies (smaller
      ;; than one; multiplication instead of division to avoid
      ;; division-by-zero issues) in case the user wants to see the
      ;; amounts averaged over some value.
      (define (collector->double c)
        ;; Future improvement: Let the user choose which kind of
        ;; currency combining she want to be done. Right now
        ;; everything foreign gets converted
        ;; (gnc:sum-collector-commodity) based on the average
        ;; cost of all holdings.
        (*
         (gnc-numeric-to-double
          (gnc:gnc-monetary-amount
           (gnc:sum-collector-commodity 
            c report-currency 
            exchange-fn)))
         averaging-multiplier))

      ;; Get balance of an account as double number, already converted
      ;; to the report's currency.
      (define (account-balance a subaccts?)
        (collector->double (profit-fn a subaccts?)))

      (define (count-accounts current-depth accts)
	(if (< current-depth tree-depth)
            (let iter ((sum 0)
                       (remaining accts))
              (if (null? remaining)
                  sum
                  (let* ((cur (car remaining))
                         (tail (cdr remaining))
                         (subaccts (count-accounts (1+ current-depth)
                                                   (gnc-account-get-children cur))))
                    (iter (+ sum (1+ subaccts)) tail))))
	    (length (filter show-acct? accts))))

      ;; Get base data to be plotted.
      (define work-to-do (lambda () (count-accounts 1 topl-accounts)))
 
      (define base-data (lambda ()
        (get-data account-balance show-acct? work-to-do tree-depth
                  0 1 topl-accounts)))

      (define (fix-signs combined)
        (map (lambda (pair)
               (if (reverse-balance? (cadr pair))
                   (cons (- (car pair)) (cdr pair))
                   pair))
	     combined))

      ;; Now do the work here.

      (if (not (null? accounts))
          (begin
            (set! combined
		  (sort (filter (lambda (pair) (not (>= 0.0 (car pair))))
				(fix-signs (cdr (base-data))))
                        (sort-comparator sort-method show-fullname?)))

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
                  (if depth-based?
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
                      (set! other-anchor (gnc:report-anchor-text id))))))
            
            ;; set the URLs; the slices are links to other reports
            (if depth-based?
                (set-slice-urls! report-obj report-guid show-fullname?
                                 tree-depth other-anchor combined chart))

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
                           (format #f
                                    (_ "~a to ~a")
                                    (qof-print-date from-date)
                                    (qof-print-date to-date))
                           (format #f
                                    (_ "Balance at ~a")
                                    (qof-print-date to-date)))
                       (if show-total?
                           (let ((total (apply + (unzip1 combined))))
                             (format
                              #f ": ~a"
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
                           (if (string? (cadr pair))
			       (cadr pair)
                               (display-name show-fullname? (cadr pair)))
			   (if show-total?
			       (string-append 
				" - "
				(xaccPrintAmount
				 (double-to-gnc-numeric
				  (car pair)
				  (gnc-commodity-get-fraction report-currency)
				  GNC-RND-ROUND)
				 print-info)
 				 )
 			       "")
 			   (if show-percent?
 				(format
 				 #f "   (~2,2f %)"
 				 (* 100.0 (/ (car pair) (apply + (unzip1 combined)))))
 			       "")
 			       ))
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

(define (build-report!
          name acct-types income-expense? depth-based? menuname menutip
          reverse-balance? uuid)
  (gnc:define-report
    'version 1
    'name name
    'report-guid uuid
    'menu-path (if income-expense?
                   (list gnc:menuname-income-expense)
                   (list gnc:menuname-asset-liability))
    'menu-name menuname
    'menu-tip menutip
    'options-generator (lambda () (options-generator acct-types
                                                     reverse-balance?
                                                     income-expense?
                                                     depth-based?))
    'renderer (lambda (report-obj)
                (piechart-renderer report-obj name uuid
                                   acct-types income-expense? depth-based?
                                   (if depth-based?
                                       display-name-accounts
                                       display-name-security)
                                   (if depth-based?
                                       sort-comparator-accounts
                                       sort-comparator-security)
                                   (if depth-based?
                                       traverse-accounts
                                       sum-securities)))))

(build-report!
  reportname-income
  (list ACCT-TYPE-INCOME)
  #t #t
  menuname-income menutip-income
  (lambda (x) #t)
  "e1bd09b8a1dd49dd85760db9d82b045c")

(build-report!
  reportname-expense
  (list ACCT-TYPE-EXPENSE)
  #t #t
  menuname-expense menutip-expense
  (lambda (x) #f)
  "9bf1892805cb4336be6320fe48ce5446")

(build-report!
  reportname-assets
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
        ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
        ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
        ACCT-TYPE-CURRENCY)
  #f #t
  menuname-assets menutip-assets
  (lambda (x) #f)
  "5c7fd8a1fe9a4cd38884ff54214aa88a")

(build-report!
  reportname-securities
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
        ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
        ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
        ACCT-TYPE-CURRENCY)
  #f #f
  menuname-securities menutip-securities
  (lambda (x) #f)
  "e9418ff64f2c11e5b61d1c7508d793ed")

(build-report!
  reportname-liabilities
  (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
        ACCT-TYPE-CREDITLINE)
  #f #t
  menuname-liabilities menutip-liabilities
  (lambda (x) #t)
  "3fe6dce77da24c66bdc8f8efdea7f9ac")
