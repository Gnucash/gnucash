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

(define-module (gnucash reports standard account-piecharts))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

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
(define optname-levels (N_ "Levels of Subaccounts"))

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
(define (options-generator account-types do-intervals? depth-based?)
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

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
          (list (vector 'None (N_ "No Averaging"))
                (vector 'YearDelta (N_ "Yearly"))
                (vector 'MonthDelta (N_ "Monthly"))
                (vector 'WeekDelta (N_ "Weekly"))))))

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
       (N_ "Maximum number of levels in the account tree displayed.")
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
      (gnc:string-locale<? (xaccAccountGetCode (cadr a))
                           (xaccAccountGetCode (cadr b)))))
   ((eq? sort-method 'alphabetical)
    (lambda (a b)
      (gnc:string-locale<? (display-name-accounts show-fullname? (cadr a))
                           (display-name-accounts show-fullname? (cadr b)))))
   (else
    (lambda (a b) (> (car a) (car b))))))

;; Sort comparator for security-based report.
(define (sort-comparator-security sort-method show-fullname?)
  (cond
   ((eq? sort-method 'acct-code)
    (lambda (a b)
      (gnc:string-locale<? (gnc-commodity-get-mnemonic (cadr a))
                           (gnc-commodity-get-mnemonic (cadr b)))))
   ((eq? sort-method 'alphabetical)
    (lambda (a b)
      (gnc:string-locale<? (display-name-security show-fullname? (cadr a))
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
                           account-types do-intervals? depth-based? reverse-balance?
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

        (document (gnc:make-html-document))
        (chart (gnc:make-html-chart))
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
              ((YearDelta) (string-append report-title " " (G_ "Yearly Average")))
              ((MonthDelta) (string-append report-title " " (G_ "Monthly Average")))
              ((WeekDelta) (string-append report-title " " (G_ "Weekly Average")))
              (else report-title)))
           (combined '())
           (other-anchor ""))

      ;; Converts a commodity-collector into one single inexact
      ;; number, depending on the report's currency and the
      ;; exchange-fn calculated above. Returns the absolute value
      ;; multiplied by the averaging-multiplier (smaller than one;
      ;; multiplication instead of division to avoid division-by-zero
      ;; issues) in case the user wants to see the amounts averaged
      ;; over some value.
      (define (collector->amount c)
        ;; Future improvement: Let the user choose which kind of
        ;; currency combining she want to be done. Right now
        ;; everything foreign gets converted
        ;; (gnc:sum-collector-commodity) based on the average
        ;; cost of all holdings.
        (* (gnc:gnc-monetary-amount
            (gnc:sum-collector-commodity c report-currency exchange-fn))
           averaging-multiplier))

      ;; Get balance of an account as an inexact number converted to,
      ;; and using precision of the report's currency.
      (define (account-balance a subaccts?)
        (collector->amount (profit-fn a subaccts?)))

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
               (if reverse-balance?
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
                                (list (list sum (G_ "Other")))))
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
            
            (if 
             (not (null? combined))
             (let ((urls (and depth-based?
                              (map
                               (lambda (series)
                                 (if (string? (cadr series))
                                     other-anchor
                                     (let* ((acct (cadr series))
                                            (subaccts (gnc-account-get-children acct)))
                                       (if (null? subaccts)
                                           (gnc:account-anchor-text (cadr series))
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
                   (scu (gnc-commodity-get-fraction report-currency)))

               (define (round-scu amt)
                 (gnc-numeric-convert amt scu GNC-HOW-RND-ROUND))

               (gnc:html-chart-set-type! chart 'pie)

               (gnc:html-chart-set-currency-iso!
                chart (gnc-commodity-get-mnemonic report-currency))
               (gnc:html-chart-set-currency-symbol!
                chart (gnc-commodity-get-nice-symbol report-currency))

               (gnc:html-chart-set-title!
                chart (list report-title
                            (string-append
                             (if do-intervals?
                                 (format #f
                                         (G_ "~a to ~a")
                                         (qof-print-date from-date)
                                         (qof-print-date to-date))
                                 (format #f
                                         (G_ "Balance at ~a")
                                         (qof-print-date to-date)))
                             (if show-total?
                                 (let ((total (apply + (unzip1 combined))))
                                   (format
                                    #f ": ~a"
                                    (gnc:monetary->string
                                     (gnc:make-gnc-monetary
                                      report-currency
                                      (round-scu total)))))
                                 ""))))
               (gnc:html-chart-set-width! chart width)
               (gnc:html-chart-set-height! chart height)
               (gnc:html-chart-add-data-series! chart
                                                (G_ "Accounts")
                                                (map round-scu (unzip1 combined))
                                                (gnc:assign-colors (length combined))
                                                'urls urls)
               (gnc:html-chart-set-axes-display! chart #f)

               (gnc:html-chart-set-data-labels!
                chart
                (map
                 (lambda (series)
                   (string-append
                    (if (string? (cadr series))
                        (cadr series)
                        (display-name show-fullname? (cadr series)))
                    (if show-total?
                        (string-append
                         " - "
                         (gnc:monetary->string
                          (gnc:make-gnc-monetary
                           report-currency
                           (round-scu (car series)))))
                        "")
                    (if show-percent?
                        (format #f " (~2,1f%)"
                                (* 100 (/ (car series)
                                          (apply + (unzip1 combined)))))
                        "")))
                 combined))

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
         name acct-types income-expense? depth-based? reverse-balance?
         menuname menutip uuid)
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
                                                     income-expense?
                                                     depth-based?))
    'renderer (lambda (report-obj)
                (piechart-renderer report-obj name uuid
                                   acct-types income-expense? depth-based?
                                   reverse-balance?
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
  #t #t #t
  menuname-income menutip-income
  "e1bd09b8a1dd49dd85760db9d82b045c")

(build-report!
  reportname-expense
  (list ACCT-TYPE-EXPENSE)
  #t #t #f
  menuname-expense menutip-expense
  "9bf1892805cb4336be6320fe48ce5446")

(build-report!
  reportname-assets
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
        ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
        ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
        ACCT-TYPE-CURRENCY)
  #f #t #f
  menuname-assets menutip-assets
  "5c7fd8a1fe9a4cd38884ff54214aa88a")

(build-report!
  reportname-securities
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
        ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
        ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
        ACCT-TYPE-CURRENCY)
  #f #f #f
  menuname-securities menutip-securities
  "e9418ff64f2c11e5b61d1c7508d793ed")

(build-report!
  reportname-liabilities
  (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
        ACCT-TYPE-CREDITLINE)
  #f #t #t
  menuname-liabilities menutip-liabilities
  "3fe6dce77da24c66bdc8f8efdea7f9ac")
