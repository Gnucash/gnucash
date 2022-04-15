;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daily-reports.scm: reports based on the day of the week
;;
;; Copyright (C) 2003, Andy Wingo <wingo at pobox dot com>
;;
;; based on account-piecharts.scm by Robert Merkel (rgmerk@mira.net)
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

(define-module (gnucash reports example daily-reports))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))

(define menuname-income (N_ "Income vs. Day of Week"))
(define menuname-expense (N_ "Expenses vs. Day of Week"))

;; The menu statusbar tips.
(define menutip-income
  (N_ "Shows a piechart with the total income for each day of the week"))
(define menutip-expense 
  (N_ "Shows a piechart with the total expenses for each day of the week"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income vs. Day of Week"))
(define reportname-expense (N_ "Expenses vs. Day of Week"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Levels of Subaccounts"))
(define optname-subacct (N_ "Include Sub-Accounts"))

(define optname-show-total (N_ "Show Totals"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

;; The option-generator. The only dependence on the type of piechart
;; is the list of account types that the account selection option
;; accepts.
(define (options-generator account-types)
  (let* ((options (gnc:new-options))
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "b")
    
    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "c" 'weighted-average)

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-subacct
      "a" (N_ "Include sub-accounts of all selected accounts.") #t))

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

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-total
      "b" (N_ "Show the total balance in legend?") #t))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height
     "d" (cons 'percent 100.0) (cons 'percent 100.0))

    (gnc:options-set-default-section options gnc:pagename-general)

    options))


;; The rendering function. Since it works for a bunch of different
;; account settings, you have to give the reportname, the
;; account-types to work on and whether this report works on
;; intervals as arguments.
(define (piechart-renderer report-obj reportname
                           account-types)
  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)

  ;; Get all options
  (let* ((to-date (gnc:time64-end-day-time
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-to-date))))
         (from-date (gnc:time64-start-day-time
                        (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general
                                     optname-from-date))))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (dosubs? (get-option gnc:pagename-accounts optname-subacct))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (report-title (get-option gnc:pagename-general
                                   gnc:optname-reportname))

         (show-total? (get-option gnc:pagename-display optname-show-total))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (commodity-list #f)
         (exchange-fn #f)
         (document (gnc:make-html-document))
         (chart (gnc:make-html-chart)))

    (define (monetary->amount foreign-monetary date)
      (gnc:gnc-monetary-amount
       (exchange-fn foreign-monetary report-currency date)))

    (if (not (null? accounts))
        (let* ((query (qof-query-create-for-splits))
               (splits '())
               (daily-totals (list 0 0 0 0 0 0 0))
               ;; Note: the absolute-super-duper-i18n'ed solution
               ;; would be to use the locale-using functions
               ;; date->string of srfi-19, similar to get_wday_name()
               ;; in src/engine/FreqSpeq.c. For now, we simply use
               ;; the normal translations, which show up in the glade
               ;; file src/gnome-utils/gtkbuilder/gnc-frequency.glade anyway.
               (days-of-week (list (G_ "Sunday") (G_ "Monday")
                                   (G_ "Tuesday") (G_ "Wednesday")
                                   (G_ "Thursday") (G_ "Friday") (G_ "Saturday"))))

          (gnc:debug daily-totals)

          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
	  (gnc:report-percent-done 1)
	  (set! commodity-list (gnc:accounts-get-commodities
                                (gnc-accounts-and-all-descendants accounts)
                                report-currency))
          (gnc:report-percent-done 5)
          (set! exchange-fn (gnc:case-exchange-time-fn
                             price-source report-currency
                             commodity-list to-date
                             5 20))
          (gnc:report-percent-done 20)

          ;; initialize the query to find splits in the right
          ;; date range and accounts
          (qof-query-set-book query (gnc-get-current-book))

          ;; for balance purposes, we don't need to do this, but it cleans up
          ;; the table display.
          (xaccQueryAddClearedMatch
           query (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)
          ;; add accounts to the query (include subaccounts
          ;; if requested)
          (gnc:report-percent-done 25)
          (if dosubs?
               (set! accounts
                 (gnc-accounts-and-all-descendants accounts)))
          (gnc:report-percent-done 30)

          (xaccQueryAddAccountMatch
           query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)

          ;; match splits between start and end dates
          (xaccQueryAddDateMatchTT
           query #t from-date #t to-date QOF-QUERY-AND)
          (qof-query-set-sort-order query
                                    (list SPLIT-TRANS TRANS-DATE-POSTED)
                                    (list QUERY-DEFAULT-SORT)
                                    '())

          ;; get the query results
          (set! splits (qof-query-run query))
          (qof-query-destroy query)
	  (gnc:report-percent-done 40)

          ;; each split is analyzed... the amount is converted to
          ;; report-currency, and the date modulo 7 used to find
          ;; weekday, and the correct daily-totals is updated.
          (for-each
           (lambda (split)
             (let* ((date (xaccTransGetDate (xaccSplitGetParent split)))
                    (weekday (modulo (1- (gnc:time64-get-week-day date)) 7))
                    (exchanged (monetary->amount
                                (gnc:make-gnc-monetary
                                 (xaccAccountGetCommodity (xaccSplitGetAccount split))
                                 (xaccSplitGetAmount split))
                                date))
                    (old-amount (list-ref daily-totals weekday)))
               (list-set! daily-totals weekday (+ old-amount exchanged))))
           splits)

          (gnc:report-percent-done 60)

          (let* ((zipped-list (filter (lambda (p) 
                                        (not (zero? (cadr p))))
                                      (zip days-of-week daily-totals)))
                 (labels (map (lambda (p)
                                (if show-total?
                                    (string-append
                                     (car p)
                                     " - "
                                     (gnc:monetary->string
                                      (gnc:make-gnc-monetary
                                       report-currency (cadr p))))
                                    (car p)))
                              zipped-list)))

            (if (not (null? zipped-list))
                (begin
                  (gnc:html-chart-set-type! chart 'pie)
                  (gnc:html-chart-set-width! chart width)
                  (gnc:html-chart-set-height! chart height)

                  (gnc:html-chart-set-title!
                   chart (list
                          report-title
                          (string-append
                           (format #f
                                   (G_ "~a to ~a")
                                   (qof-print-date from-date)
                                   (qof-print-date to-date))
                           (if show-total?
                               (let ((total (apply + daily-totals)))
                                 (format
                                  #f ": ~a"
                                  (gnc:monetary->string
                                   (gnc:make-gnc-monetary
                                    report-currency total))))
                               ""))))

                  (gnc:html-chart-add-data-series!
                   chart
                   reportname
                   (map cadr zipped-list)
                   (gnc:assign-colors (length zipped-list)))
                  (gnc:html-chart-set-data-labels! chart labels)
                  (gnc:html-chart-set-axes-display! chart #f)
                  (gnc:html-document-add-object! document chart))

                (gnc:html-document-add-object!
                 document
                 (gnc:html-make-empty-data-warning
                  report-title (gnc:report-id report-obj))))))

        (gnc:html-document-add-object!
         document
         (gnc:html-make-empty-data-warning
          report-title (gnc:report-id report-obj))))

    (gnc:report-finished)
    document))

(for-each
 (lambda (l)
   (gnc:define-report
    'version 1
    'name (car l)
    'report-guid (car (reverse l))
    'menu-path (list gnc:menuname-example)
    'menu-name (caddr l)
    'menu-tip (car (cdddr l))
    'options-generator (lambda () (options-generator (cadr l)))
    'renderer (lambda (report-obj)
                (piechart-renderer report-obj
                                   (car l)
                                   (cadr l)))))
 (list
  ;; reportname, account-types, menu-reportname, menu-tip
  (list reportname-income (list ACCT-TYPE-INCOME) menuname-income
        menutip-income "5e2d129f28d14df881c3e47e3053f604")
  (list reportname-expense (list ACCT-TYPE-EXPENSE) menuname-expense
        menutip-expense "dde49fed4ca940959ae7d01b72742530")))
