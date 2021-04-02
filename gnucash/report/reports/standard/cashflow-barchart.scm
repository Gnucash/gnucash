;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cashflow-barchart.scm: cash flow barchart report
;;
;; By Jose Marino <jmarino@users.noreply.github.com>
;;
;; based on cash-flow.scm by:
;; Herbert Thoma <herbie@hthoma.de>
;; and net-barchart by:
;; Robert Merkel <rgmerk@mira.net>
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

(define-module (gnucash reports standard cashflow-barchart))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash reports cash-flow-calc))
(use-modules (gnucash report))
(use-modules (srfi srfi-26))

(define reportname (N_ "Cash Flow Barchart"))

;; define all option's names so that they are properly defined
;; in *one* place.
;; Accounts
(define optname-accounts (N_ "Accounts"))
(define optname-include-trading-accounts (N_ "Include Trading Accounts in report"))
;; Display
(define optname-show-in (N_ "Show Money In"))
(define optname-show-out (N_ "Show Money Out"))
(define optname-show-net (N_ "Show Net Flow"))
(define optname-show-table (N_ "Show Table"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
;; General
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))


;; options generator function
(define (cashflow-barchart-options-generator)
  (let* ((options (gnc:new-options))
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

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
     optname-price-source "d" 'pricedb-nearest)

    ;; Accounts tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a" (N_ "Report on these accounts.")
      (lambda ()     ; account getter
        (gnc:filter-accountlist-type
         (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-ASSET
               ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      #f #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-include-trading-accounts
      "b" (N_ "Include transfers to and from Trading Accounts in the report.")  #f))

    ;; Display tab
    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-in
      "a" (N_ "Show money in?") #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-out
      "b" (N_ "Show money out?") #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-net
      "c" (N_ "Show net money flow?") #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-table
      "d" (N_ "Display a table of the selected data.") #f))

    ;; Plot size options
    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "e" (cons 'percent 100.0) (cons 'percent 100.0))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cashflow-barchart-renderer
;; set up the document and add the barchart and table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cashflow-barchart-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (include-trading-accounts (get-option gnc:pagename-accounts
                                               optname-include-trading-accounts))
         (work-done 0)
         (work-to-do 0)
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))

         ;; calculate the exchange rates
         (exchange-fn (gnc:case-exchange-fn
                       price-source report-currency to-date-t64))

         (interval (get-option gnc:pagename-general optname-stepsize))
         (show-in? (get-option gnc:pagename-display optname-show-in))
         (show-out? (get-option gnc:pagename-display optname-show-out))
         (show-net? (get-option gnc:pagename-display optname-show-net))
         (show-table? (get-option gnc:pagename-display optname-show-table))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))

         (dates-list (gnc:make-date-interval-list
                      (gnc:time64-start-day-time from-date-t64)
                      (gnc:time64-end-day-time to-date-t64)
                      (gnc:deltasym-to-delta interval)))
         (report-title (get-option gnc:pagename-general
                                   gnc:optname-reportname))

         (doc (gnc:make-html-document))
         (chart (gnc:make-html-chart))
         (non-zeros #f))

    (if (not (null? accounts))
        (let* ((commodity-list (gnc:accounts-get-commodities
                                accounts
                                report-currency))
               ;; Get an exchange function that will convert each transaction using the
               ;; nearest available exchange rate if that is what is specified
               (time-exchange-fn (gnc:case-exchange-time-fn
                                  price-source report-currency
                                  commodity-list to-date-t64
                                  0 0))
               (date-string-list (map (lambda (date-list-item)       ; date-list-item is (start . end)
                                        (qof-print-date (car date-list-item)))
                                      dates-list))
               (in-list '())
               (out-list '())
               (net-list '())
               (total-in #f)
               (total-out #f)
               (total-net #f))

          ;; Helper function to convert currencies
          (define (to-report-currency currency amount date)
            (gnc:gnc-monetary-amount
             (time-exchange-fn (gnc:make-gnc-monetary currency amount)
                               report-currency
                               date)))
          ;; Sum a collector to return a gnc-monetary
          (define (sum-collector collector)
            (gnc:sum-collector-commodity
             collector report-currency exchange-fn))

          (define (make-cashflow-url idx)
            (gnc:make-report-anchor
             "f8748b813fab4220ba26e743aedf38da"
             report-obj
             (list
              (list "General" "Start Date" (cons 'absolute (car (list-ref dates-list idx))))
              (list "General" "End Date" (cons 'absolute (cadr (list-ref dates-list idx))))
              (list "Accounts" "Account" accounts))))

          (define cashflow-urls
            (map make-cashflow-url (iota (length dates-list))))

          ;; gather money in/out data for all date intervals
          (set! work-done 0)
          (set! work-to-do (length dates-list))
          (for-each
           (lambda (date-pair)
             (set! work-done (+ 1 work-done))
             (gnc:report-percent-done (* 80 (/ work-done work-to-do)))
             (let* ((settings (list (cons 'accounts accounts)
                                    (cons 'from-date-t64 (car date-pair))
                                    (cons 'to-date-t64 (cadr date-pair))
                                    (cons 'report-currency report-currency)
                                    (cons 'include-trading-accounts include-trading-accounts)
                                    (cons 'to-report-currency to-report-currency)))
                    (result (cash-flow-calc-money-in-out settings))
                    (money-in-collector (cdr (assq 'money-in-collector result)))
                    (money-out-collector (cdr (assq 'money-out-collector result)))
                    (money-in (sum-collector money-in-collector))
                    (money-out (sum-collector money-out-collector))
                    (money-net (gnc:monetary+ money-in (gnc:monetary-neg money-out))))
               (set! in-list (cons money-in in-list))
               (set! out-list (cons money-out out-list))
               (set! net-list (cons money-net net-list))))
           dates-list)

          ;; flip result lists (they were built by appending to the front)
          (when show-in?
            (set! in-list (reverse in-list))
            (set! total-in (apply gnc:monetary+ in-list)))
          (when show-out?
            (set! out-list (reverse out-list))
            (set! total-out (apply gnc:monetary+ out-list)))
          (when show-net?
            (set! net-list (reverse net-list))
            (set! total-net (apply gnc:monetary+ net-list)))

          (gnc:report-percent-done 90)

          (gnc:html-chart-set-title!
           chart (list report-title
                       (format #f
                               (G_ "~a to ~a")
                               (qof-print-date from-date-t64)
                               (qof-print-date to-date-t64))))
          (gnc:html-chart-set-width! chart width)
          (gnc:html-chart-set-height! chart height)
          (gnc:html-chart-set-y-axis-label!
           chart (gnc-commodity-get-mnemonic report-currency))
          (gnc:html-chart-set-currency-iso!
           chart (gnc-commodity-get-mnemonic report-currency))
          (gnc:html-chart-set-currency-symbol!
           chart (gnc-commodity-get-nice-symbol report-currency))

          (gnc:html-chart-set-data-labels! chart date-string-list)
          (if show-in?
              (gnc:html-chart-add-data-series! chart
                                               (G_ "Money In")
                                               (map gnc:gnc-monetary-amount in-list)
                                               "#0074D9"
                                               'urls cashflow-urls))
          (if show-out?
              (gnc:html-chart-add-data-series! chart
                                               (G_ "Money Out")
                                               (map gnc:gnc-monetary-amount out-list)
                                               "#FF4136"
                                               'urls cashflow-urls))
          (if show-net?
              (gnc:html-chart-add-data-series! chart
                                               (G_ "Net Flow")
                                               (map gnc:gnc-monetary-amount net-list)
                                               "#2ECC40"
                                               'urls cashflow-urls))

          (gnc:report-percent-done 95)

          (set! non-zeros (gnc:not-all-zeros (append
                                              (map gnc:gnc-monetary-amount
                                                   (append in-list out-list net-list)))))

          ;; If we have no data in the plot, display warning message
          (if non-zeros
              (gnc:html-document-add-object! doc chart)
              (gnc:html-document-add-object!
               doc
               (gnc:html-make-empty-data-warning
                report-title (gnc:report-id report-obj))))

          (if (and non-zeros show-table?)
              (let* ((table (gnc:make-html-table)))

                (define (add-row date in out net)
                  (gnc:html-table-append-row!
                   table
                   (cons date
                         (map (cut gnc:make-html-table-cell/markup "number-cell" <>)
                              (append
                               (if show-in?  (list in)  '())
                               (if show-out? (list out) '())
                               (if show-net? (list net) '()))))))

                (gnc:html-table-set-col-headers!
                 table (append (list (G_ "Date"))
                               (if show-in? (list (G_ "Money In")) '())
                               (if show-out? (list (G_ "Money Out")) '())
                               (if show-net? (list (G_ "Net Flow")) '())))

                (gnc:html-document-add-object!
                 doc (gnc:make-html-text (gnc:html-markup-h3 (G_ "Overview:"))))

                (for-each add-row date-string-list in-list out-list net-list)
                (add-row (G_ "Total") total-in total-out total-net)

                (gnc:html-document-add-object! doc table))))

        ;; else: error condition: no accounts specified
        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          reportname (gnc:report-id report-obj))))

    (gnc:report-finished)

    doc))


;; export to make uuid available to unit test: test-cashflow-barchart
(export cashflow-barchart-uuid)
(define cashflow-barchart-uuid "5426e4d987f6444387fe70880e5b28a0")

(gnc:define-report
 'version 1
 'name reportname
 'report-guid cashflow-barchart-uuid
 'menu-tip (N_ "Shows a barchart with cash flow over time")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cashflow-barchart-options-generator
 'renderer cashflow-barchart-renderer)
