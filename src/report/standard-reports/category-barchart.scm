;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category-barchart.scm: shows barchart of income/expense categories
;;  
;; By Christian Stimming <stimming@tu-harburg.de>
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

;; depends must be outside module scope -- and should eventually go away.
(define-module (gnucash report standard-reports category-barchart))
(use-modules (gnucash report report-system report-collectors))
(use-modules (gnucash report report-system collectors))
(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 regex))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))

(gnc:module-load "gnucash/report/report-system" 0)

;; included since Bug726449
(use-modules (ice-9 regex)) ;; for regexp-substitute/global, used by jpqplot
(load-from-path "html-jqplot") ;; for jqplot-escape-string

;; The option names are defined here to 1. save typing and 2. avoid
;; spelling errors. The *reportnames* are defined here (and not only
;; once at the very end) because I need them to define the "other"
;; report, thus needing them twice.
(define menuname-income (N_ "Income Barchart"))
(define menuname-expense (N_ "Expense Barchart"))
(define menuname-assets (N_ "Asset Barchart"))
(define menuname-liabilities (N_ "Liability Barchart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income 
  (N_ "Shows a barchart with the Income per interval \
developing over time"))
(define menutip-expense 
  (N_ "Shows a barchart with the Expenses per interval \
developing over time"))
(define menutip-assets 
  (N_ "Shows a barchart with the Assets developing over time"))
(define menutip-liabilities 
  (N_ "Shows a barchart with the Liabilities \
developing over time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income Over Time"))
(define reportname-expense (N_ "Expense Over Time"))
(define reportname-assets (N_ "Assets Over Time"))
(define reportname-liabilities (N_ "Liabilities Over Time"))

;; Option names
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long account names"))
(define optname-stacked (N_ "Use Stacked Bars"))
(define optname-slices (N_ "Maximum Bars"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

(define (options-generator account-types reverse-balance? do-intervals?)
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; save off the reverse-balance option
    (add-option
     (gnc:make-internal-option "__report" "reverse-balance?" reverse-balance?))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-interval-choice! 
     options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "c")

    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "d" 'weighted-average)

    (if do-intervals?
        (add-option
         (gnc:make-multichoice-option
          gnc:pagename-general optname-averaging
          "e" opthelp-averaging
          'None
          (list (vector 'None
                        (N_ "No Averaging")
                        (N_ "Just show the amounts, without any averaging."))
                (vector 'MonthDelta
                        (N_ "Monthly")
                        (N_ "Show the average monthly amount during the reporting period."))
                (vector 'WeekDelta
                        (N_ "Weekly")
                        (N_ "Show the average weekly amount during the reporting period."))
                (vector 'DayDelta
                        (N_ "Daily")
                        (N_ "Show the average daily amount during the reporting period."))
                )
          ))
        )


    ;; Accounts tab
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
              (gnc:filter-accountlist-type account-types accounts)))
      #t))
    
    (gnc:options-add-account-levels! 
     options gnc:pagename-accounts optname-levels "c" 
     (N_ "Show accounts to this depth and not further.") 
     2)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-stacked
      "b" 
      (N_ "Show barchart as stacked barchart?") 
      #t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "c" (N_ "Maximum number of bars in the chart.") 8
      2 24 0 1))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "d" (N_ "Display a table of the selected data.")
      #f))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "e" 400 400)

    (gnc:options-add-sort-method! 
     options gnc:pagename-display
     optname-sort-method "f" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.

;; FIXME: the exchange rate should change every time interval, of
;; course, but right now we assume the very last exchange rate to be
;; constant over the whole report period. Note that this might get
;; *really* complicated.

(define (category-barchart-renderer report-obj reportname reportguid 
                                    account-types do-intervals?)
  ;; A helper functions for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option 
      (gnc:report-options report-obj) section name)))
  
  (gnc:report-starting reportname)
  (let ((to-date-tp (gnc:timepair-end-day-time 
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general 
                                  optname-to-date))))
        (from-date-tp (gnc:timepair-start-day-time 
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general 
                                    optname-from-date))))
        (interval (get-option gnc:pagename-general optname-stepsize))
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

        (accounts (get-option gnc:pagename-accounts optname-accounts))
        (account-levels (get-option gnc:pagename-accounts optname-levels))
        
        (stacked? (get-option gnc:pagename-display optname-stacked))
        (show-fullname? (get-option gnc:pagename-display optname-fullname))
        (max-slices (inexact->exact
		     (get-option gnc:pagename-display optname-slices)))
        (height (get-option gnc:pagename-display optname-plot-height))
        (width (get-option gnc:pagename-display optname-plot-width))
	(sort-method (get-option gnc:pagename-display optname-sort-method))
	(reverse-balance? (get-option "__report" "reverse-balance?"))

        (show-table? (get-option gnc:pagename-display (N_ "Show table")))
        (document (gnc:make-html-document))
        (chart (gnc:make-html-barchart))
        (table (gnc:make-html-table))
        (topl-accounts (gnc:filter-accountlist-type 
                        account-types
                        (gnc-account-get-children-sorted
                         (gnc-get-current-root-account)))))
    
    ;; Returns true if the account a was selected in the account
    ;; selection option.
    (define (show-acct? a)
      (member a accounts))

    ;;(gnc:debug accounts)
    (if (not (null? accounts))
        
        ;; Define more helper variables.
        (let* ((commodity-list #f)
               (exchange-fn #f)
               (tree-depth (if (equal? account-levels 'all)
                               (gnc:get-current-account-tree-depth)
                               account-levels))
               (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
               (interval-fraction-func (gnc:date-get-fraction-func interval))
               (averaging-multiplier
                 (if averaging-fraction-func
                     ;; Calculate the divisor of the amounts so that an
                     ;; average is shown
                     (let* ((start-frac-avg (averaging-fraction-func (gnc:timepair->secs from-date-tp)))
                             (end-frac-avg (averaging-fraction-func (+ 1 (gnc:timepair->secs to-date-tp))))
                             (diff-avg (- end-frac-avg start-frac-avg))
                             (start-frac-int (interval-fraction-func (gnc:timepair->secs from-date-tp)))
                             (end-frac-int (interval-fraction-func (+ 1 (gnc:timepair->secs to-date-tp))))
                             (diff-int (- end-frac-int start-frac-int))
                            )
                     ;; Extra sanity check to ensure a number smaller than 1
                     (if (> diff-avg diff-int)
                         (/ diff-int diff-avg)
                         1))
                     1))
               ;; If there is averaging, the report-title is extended
               ;; accordingly.
               (report-title
                 (case averaging-selection
                   ((MonthDelta) (string-append report-title " " (_ "Monthly Average")))
                   ((WeekDelta) (string-append report-title " " (_ "Weekly Average")))
                   ((DayDelta) (string-append report-title " " (_ "Daily Average")))
                   (else report-title)))
               ;; This is the list of date intervals to calculate.
               (dates-list (if do-intervals?
                               (gnc:make-date-interval-list
                                (gnc:timepair-start-day-time from-date-tp) 
                                (gnc:timepair-end-day-time to-date-tp)
                                (gnc:deltasym-to-delta interval))
                               (gnc:make-date-list
                                (gnc:timepair-end-day-time from-date-tp) 
                                (gnc:timepair-end-day-time to-date-tp)
                                (gnc:deltasym-to-delta interval))))
               ;; Here the date strings for the x-axis labels are
               ;; created.
               (date-string-list
                (map (lambda (date-list-item)
                       (gnc-print-date
                        (if do-intervals?
                            (car date-list-item)
                            date-list-item)))
                     dates-list))
               (other-anchor "")
               (all-data '()))
          
          ;; Converts a commodity-collector into one single double
          ;; number, depending on the report's currency and the
          ;; exchange-fn calculated above. Returns a double, multiplied
          ;; by the averaging-multiplies (smaller than one; multiplication
          ;; instead of division to avoid division-by-zero issues) in case
          ;; the user wants to see the amounts averaged over some value.
          (define (collector->double c date)
            ;; Future improvement: Let the user choose which kind of
            ;; currency combining she want to be done.
	    (if (not (gnc:timepair? date))
		(throw 'wrong))
            (*
              (gnc-numeric-to-double
              (gnc:gnc-monetary-amount
                (gnc:sum-collector-commodity
                c report-currency
                (lambda (a b) (exchange-fn a b date)))))
             averaging-multiplier))

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

          ;; Calculates all account's balances. Returns a list of pairs:
          ;; (<account> <balance-list>), like '((Earnings (10.0 11.2))
          ;; (Gifts (12.3 14.5))), where each element of <balance-list>
          ;; is the balance corresponding to one element in
          ;; <dates-list>.
          ;;
          ;; If current-depth >= tree-depth, then the balances are
          ;; calculated *with* subaccount's balances. Else only the
          ;; current account is regarded. Note: All accounts in accts
          ;; and all their subaccounts are processed, but a balances is
          ;; calculated and returned *only* for those accounts where
          ;; show-acct? is true. This is necessary because otherwise we
          ;; would forget an account that is selected but not its
          ;; parent.
	  (define (apply-sign account x)
	    (if (reverse-balance? account) (- x) x))
          (define (calculate-report accounts progress-range)
	    (let* ((the-acount-destination-alist (account-destination-alist accounts
									    account-types
									    tree-depth))
		   (account-reformat
		    (if do-intervals?
			(lambda (account result)
			  (map (lambda (collector datepair)
				 (let ((date (second datepair)))
				   (apply-sign account (collector->double collector date))))
			       result dates-list))
			(lambda (account result)
			  (let ((commodity-collector (gnc:make-commodity-collector)))
			    (collector-end (fold (lambda (next date list-collector)
						   (commodity-collector 'merge next #f)
						   (collector-add list-collector
								  (apply-sign account
									      (collector->double commodity-collector
												 date))))
						 (collector-into-list)
						 result dates-list))))))

		   (the-work (category-by-account-report-work do-intervals?
				dates-list the-acount-destination-alist
				(lambda (account date)
				  (make-gnc-collector-collector))
				account-reformat))
		   (the-report (category-by-account-report-do-work the-work progress-range)))
	      the-report))

          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
	  (gnc:report-percent-done 1)
	  (set! commodity-list (gnc:accounts-get-commodities 
                                (append 
                                 (gnc:acccounts-get-all-subaccounts accounts)
                                 accounts)
                                report-currency))
	  (set! exchange-fn (gnc:case-exchange-time-fn 
                             price-source report-currency 
                             commodity-list to-date-tp
			     5 15))

          ;; Sort the account list according to the account code field.
          (set! all-data (sort
                          (filter (lambda (l)
                                    (not (= 0.0 (apply + (cadr l)))))
                                  (calculate-report accounts (cons 0 90)))
			  (cond
			   ((eq? sort-method 'acct-code)
			    (lambda (a b) 
			      (string<? (xaccAccountGetCode (car a))
					(xaccAccountGetCode (car b)))))
			   ((eq? sort-method 'alphabetical)
			    (lambda (a b) 
			      (string<? ((if show-fullname?
					     gnc-account-get-full-name
					     xaccAccountGetName) (car a))
					((if show-fullname?
					     gnc-account-get-full-name
					     xaccAccountGetName) (car b)))))
			   (else
			    (lambda (a b)
			      (> (apply + (cadr a))
				 (apply + (cadr b))))))))
          ;; Or rather sort by total amount?
          ;;(< (apply + (cadr a)) 
          ;;   (apply + (cadr b))))))
          ;; Other sort criteria: max. amount, standard deviation of amount,
          ;; min. amount; ascending, descending. FIXME: Add user options to
          ;; choose sorting.
          
          
          ;;(gnc:warn "all-data" all-data)

          ;; Proceed if the data is non-zeros
          (if 
           (and (not (null? all-data))
                (gnc:not-all-zeros (map cadr all-data)))
           (begin 
             ;; Set chart title, subtitle etc.
             (gnc:html-barchart-set-title! chart report-title)
             (gnc:html-barchart-set-subtitle!
              chart (sprintf #f
                             (if do-intervals?
                                 (_ "%s to %s")
                                 (_ "Balances %s to %s"))
                             (jqplot-escape-string (gnc-print-date from-date-tp))
                             (jqplot-escape-string (gnc-print-date to-date-tp))))
             (gnc:html-barchart-set-width! chart width)
             (gnc:html-barchart-set-height! chart height)
             
             ;; row labels etc.
             (gnc:html-barchart-set-row-labels! chart date-string-list)
             ;; FIXME: axis labels are not yet supported by
             ;; libguppitank.
             (gnc:html-barchart-set-y-axis-label!
              chart (gnc-commodity-get-mnemonic report-currency))
             (gnc:html-barchart-set-row-labels-rotated?! chart #t)
             (gnc:html-barchart-set-stacked?! chart stacked?)
             ;; If this is a stacked barchart, then reverse the legend.
	     ;; Doesn't do what you'd expect. - DRH
	     ;; It does work, but needs Guppi 0.40.4. - cstim
             (gnc:html-barchart-set-legend-reversed?! chart stacked?)
             
             ;; If we have too many categories, we sum them into a new
             ;; 'other' category and add a link to a new report with just
             ;; those accounts.
             (if (> (length all-data) max-slices)
                 (let* ((start (take all-data (- max-slices 1)))
                        (finish (drop all-data (- max-slices 1)))
                        (other-sum (map 
                                    (lambda (l) (apply + l))
                                    (apply zip (map cadr finish)))))
                   (set! all-data
                         (append start
                                 (list (list (_ "Other") other-sum))))
                   (let* ((options (gnc:make-report-options reportguid))
                          (id #f))
                     ;; now copy all the options
                     (gnc:options-copy-values 
                      (gnc:report-options report-obj) options)
                     ;; and set the destination accounts
                     (gnc:option-set-value
                      (gnc:lookup-option options gnc:pagename-accounts 
                                         optname-accounts)
                      (map car finish))
                     ;; Set the URL to point to this report.
                     (set! id (gnc:make-report reportguid options))
                     (set! other-anchor (gnc:report-anchor-text id)))))
             
             
             ;; This adds the data. Note the apply-zip stuff: This
             ;; transposes the data, i.e. swaps rows and columns. Pretty
             ;; cool, eh? Courtesy of dave_p.
	     (gnc:report-percent-done 92)
             (if (not (null? all-data))
                 (gnc:html-barchart-set-data! 
                  chart 
                  (apply zip (map cadr all-data))))
             
             ;; Labels and colors
	     (gnc:report-percent-done 94)
             (gnc:html-barchart-set-col-labels!
              chart (map (lambda (pair)
                           (if (string? (car pair))
                               (car pair)
                               ((if show-fullname?
                                    gnc-account-get-full-name
                                    xaccAccountGetName) (car pair))))
                         all-data))
             (gnc:html-barchart-set-col-colors! 
              chart
              (gnc:assign-colors (length all-data)))
             
             ;; set the URLs; the slices are links to other reports
	     (gnc:report-percent-done 96)
             (let 
                 ((urls
                   (map 
                    (lambda (pair)
                      (if 
                       (string? (car pair))
                       other-anchor
                       (let* ((acct (car pair))
                              (subaccts 
                               (gnc-account-get-children acct)))
                         (if (null? subaccts)
                             ;; if leaf-account, make this an anchor
                             ;; to the register.
                             (gnc:account-anchor-text acct)
                             ;; if non-leaf account, make this a link
                             ;; to another report which is run on the
                             ;; immediate subaccounts of this account
                             ;; (and including this account).
                             (gnc:make-report-anchor
                              reportguid
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
                    all-data)))
               (gnc:html-barchart-set-button-1-bar-urls! 
                chart (append urls urls))
               ;; The legend urls do the same thing.
               (gnc:html-barchart-set-button-1-legend-urls! 
                chart (append urls urls)))
             
	     (gnc:report-percent-done 98)
             (gnc:html-document-add-object! document chart)
             (if show-table?
                 (begin
                   (gnc:html-table-append-column! table date-string-list)

                   (letrec
                       ((addcol
                         (lambda (col)
                           (if (not (null? col))
                               (begin
                                 (gnc:html-table-append-column!
                                  table (car col))
                                 (addcol (cdr col))
                                 )
                               ))
                         ))
                     (addcol (map cadr all-data))
                     )

                   (gnc:html-table-set-col-headers!
                    table
                    (append
                     (list (_ "Date"))
                     (map (lambda (pair)
                           (if (string? (car pair))
                               (car pair)
                               ((if show-fullname?
                                    gnc-account-get-full-name
                                    xaccAccountGetName) (car pair))))
                          all-data)
                     (if (> (gnc:html-table-num-columns table) 2)
                         (list (_ "Grand Total"))
                         '()
                         )
                     ))

                   (if (> (gnc:html-table-num-columns table) 2)
                       (letrec
                           ((sumtot
                             (lambda (row)
                               (if (null? row)
                                   '()
                                   (cons (sumrow (car row)) (sumtot (cdr row)))
                                   )
                               )
                             )
                            (sumrow
                             (lambda (row)
                               (if (not (null? row))
                                   (+ (car row) (sumrow (cdr row)))
                                   0
                                   )
                               )
                             ))
                         (gnc:html-table-append-column!
                          table
                          (sumtot (apply zip (map cadr all-data)))
                          )
                         )
                       )
                       ;; set numeric columns to align right
                   (for-each
                    (lambda (col)
                      (gnc:html-table-set-col-style!
                       table col "td"
                       'attribute (list "class" "number-cell")))
                    '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
                   (gnc:html-document-add-object! document table)
                   ) ;; begin if
                 )
             )

           ;; else if empty data
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
	     report-title (gnc:report-id report-obj)))))
        
	;; else if no accounts selected
        (gnc:html-document-add-object! 
         document 
	 (gnc:html-make-no-account-warning 
	  report-title (gnc:report-id report-obj))))
    
    (gnc:report-finished)
    document))

;; Export reports

(export category-barchart-income-uuid category-barchart-expense-uuid
	category-barchart-asset-uuid category-barchart-liability-uuid)

(define category-barchart-income-uuid "44f81bee049b4b3ea908f8dac9a9474e")
(define category-barchart-expense-uuid "b1f15b2052c149df93e698fe85a81ea6")
(define category-barchart-asset-uuid "e9cf815f79db44bcb637d0295093ae3d")
(define category-barchart-liability-uuid "faf410e8f8da481fbc09e4763da40bcc")

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
		  (category-barchart-renderer report-obj 
					      (car l)
					      (car (reverse l))
					      (cadr l)
					      (caddr l))))))
 (list 
  ;; reportname, account-types, do-intervals?, 
  ;; menu-reportname, menu-tip
  (list reportname-income (list ACCT-TYPE-INCOME) #t menuname-income menutip-income (lambda (x) #t) category-barchart-income-uuid)
  (list reportname-expense (list ACCT-TYPE-EXPENSE) #t menuname-expense menutip-expense (lambda (x) #f) category-barchart-expense-uuid)
  (list reportname-assets 
        (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
              ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
              ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
              ACCT-TYPE-CURRENCY)
        #f menuname-assets menutip-assets (lambda (x) #f) category-barchart-asset-uuid)
  (list reportname-liabilities 
        (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
              ACCT-TYPE-CREDITLINE)
        #f menuname-liabilities menutip-liabilities (lambda (x) #t) category-barchart-liability-uuid)))
