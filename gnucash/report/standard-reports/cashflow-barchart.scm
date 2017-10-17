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

(define-module (gnucash report standard-reports cashflow-barchart))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash engine))
(use-modules (gnucash printf))

(gnc:module-load "gnucash/report/report-system" 0)

;; Define these utilities to avoid using module srfi-1
(define first car)
(define second cadr)

(define reportname (N_ "Cash Flow Barchart"))

;; define all option's names so that they are properly defined
;; in *one* place.
;; Accounts
(define optname-accounts (N_ "Accounts"))
(define optname-include-trading-accounts (N_ "Include Trading Accounts in report"))
;; Display
(define optname-show-inout (N_ "Show Money In/Out"))
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
      gnc:pagename-display optname-show-inout
      "a" (N_ "Show money in/out?") #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-net
      "b" (N_ "Show net money flow?") #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-table
      "c" (N_ "Display a table of the selected data.") #f))

    ;; Plot size options
    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

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
         (row-num 0)
         (work-done 0)
         (work-to-do 0)
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
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

         (interval (get-option gnc:pagename-general optname-stepsize))
         (show-inout? (get-option gnc:pagename-display optname-show-inout))
         (show-net? (get-option gnc:pagename-display optname-show-net))
         (show-table? (get-option gnc:pagename-display optname-show-table))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))

         (dates-list (gnc:make-date-interval-list
                      (gnc:timepair-start-day-time from-date-tp)
                      (gnc:timepair-end-day-time to-date-tp)
                      (gnc:deltasym-to-delta interval)))
         (report-title (get-option gnc:pagename-general
                                   gnc:optname-reportname))

         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
         (txt (gnc:make-html-text))
         (chart (gnc:make-html-barchart))
         (non-zeros #f))

    ;; utility function used to generate chart (from net-barchart.scm)
    (define (add-column! data-list)
      (begin
        (gnc:html-barchart-append-column! chart data-list)
        (if (gnc:not-all-zeros data-list) (set! non-zeros #t))
        #f))

    (if (not (null? accounts))
        (let* ((money-diff-collector (gnc:make-commodity-collector))
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
                                  0 0))
               (date-string-list (map (lambda (date-list-item)       ; date-list-item is (start . end)
                                        (gnc-print-date (car date-list-item)))
                                      dates-list))
               (results-by-date '())
               (in-list '())
               (out-list '())
               (net-list '())
               (in-value-list #f)
               (out-value-list #f)
               (net-value-list #f)
               (in-total-collector (gnc:make-commodity-collector))
               (out-total-collector (gnc:make-commodity-collector))
               (net-total-collector (gnc:make-commodity-collector))
               )

          ;; Helper function to convert currencies
          (define (to-report-currency currency amount date)
            (gnc:gnc-monetary-amount
             (time-exchange-fn (gnc:make-gnc-monetary currency amount)
                               report-currency
                               date)))
          ;; Sum a collector to return a gnc-monetary
          (define (sum-collector collector)
            (gnc:sum-collector-commodity
             collector report-currency exchange-fn)
            )
          ;; Convert gnc:monetary to number (used to generate data for the chart)
          (define (monetary->double monetary)
            (gnc-numeric-to-double (gnc:gnc-monetary-amount monetary))
            )

          ;; gather money in/out data for all date intervals
          (set! work-done 0)
          (set! work-to-do (length dates-list))
          (for-each
           (lambda (date-pair)
             (set! work-done (+ 1 work-done))
             (gnc:report-percent-done (* 80 (/ work-done work-to-do)))
             (let* ((settings (list (cons 'accounts accounts)
                                    (cons 'to-date-tp (second date-pair))
                                    (cons 'from-date-tp (first date-pair))
                                    (cons 'report-currency report-currency)
                                    (cons 'include-trading-accounts include-trading-accounts)
                                    (cons 'to-report-currency to-report-currency)))
                    (result (cashflow-barchart-calc-money-in-out settings))
                    (money-in-collector (cdr (assq 'money-in-collector result)))
                    (money-out-collector (cdr (assq 'money-out-collector result)))
                    (money-net-collector (gnc:make-commodity-collector))
                    (money-in-monetary (sum-collector money-in-collector))
                    (money-out-monetary (sum-collector money-out-collector))
                    (money-net-monetary #f)
                    )
               (money-net-collector 'merge money-in-collector #f)
               (money-net-collector 'minusmerge money-out-collector #f)
               (set! money-net-monetary (sum-collector money-net-collector))
               (set! in-list (cons money-in-monetary in-list))
               (set! out-list (cons money-out-monetary out-list))
               (set! net-list (cons money-net-monetary net-list))
               (in-total-collector 'merge money-in-collector #f)
               (out-total-collector 'merge money-out-collector #f)
               ))
           dates-list)

          (net-total-collector 'merge in-total-collector #f)
          (net-total-collector 'minusmerge out-total-collector #f)

          ;; flip result lists (they were built by appending to the front)
          (set! in-list (reverse in-list))
          (set! out-list (reverse out-list))

          (set! in-value-list (map monetary->double in-list))
          (set! out-value-list (map monetary->double out-list))

          (if show-net?
              (begin
                (set! net-list (reverse net-list))
                (set! net-value-list (map monetary->double net-list)))
              )
          (gnc:report-percent-done 90)

          (gnc:html-barchart-set-title! chart report-title)
          (gnc:html-barchart-set-subtitle!
           chart (sprintf #f
                          (_ "%s to %s")
                          (gnc-print-date from-date-tp)
                          (gnc-print-date to-date-tp)))
          (gnc:html-barchart-set-width! chart width)
          (gnc:html-barchart-set-height! chart height)
          (gnc:html-barchart-set-row-labels! chart date-string-list)
          (gnc:html-barchart-set-y-axis-label!
           chart (gnc-commodity-get-mnemonic report-currency))

          (if show-inout?
              (begin
                (add-column! in-value-list)
                (add-column! out-value-list)
                ))
          (if show-net?
              (add-column! net-value-list))

          ;; Legend labels, colors
          (gnc:html-barchart-set-col-labels!
           chart (append
                  (if show-inout?
                      (list (_ "Money In") (_ "Money Out")) '())
                  (if show-net?
                      (list (_ "Net Flow")) '())))
          (gnc:html-barchart-set-col-colors!
           chart (append
                  (if show-inout?
                      '("blue" "red") '())
                  (if show-net?
                      '("green") '())))
          (gnc:report-percent-done 95)

          ;; If we have no data in the plot, display warning message
          (if non-zeros
              (gnc:html-document-add-object! doc chart)
              (gnc:html-document-add-object!
               doc
               (gnc:html-make-empty-data-warning
                report-title (gnc:report-id report-obj)))
              )

          (if (and non-zeros show-table?)
              (let* ((table (gnc:make-html-table)))
                (set! date-string-list (append date-string-list (list "Total")))
                (set! in-list (append in-list (list (sum-collector in-total-collector))))
                (set! out-list (append out-list (list (sum-collector out-total-collector))))
                (if show-net?
                    (set! net-list (append net-list (list (sum-collector net-total-collector)))))

                (gnc:html-table-set-col-headers!
                 table (append (list (_ "Date"))
                               (if show-inout?
                                   (list (_ "Money In") (_ "Money Out")) '())
                               (if show-net?
                                   (list (_ "Net Flow")) '())
                               ))

                (gnc:html-document-add-object!
                 doc (gnc:make-html-text (gnc:html-markup-h3 (_ "Overview:"))))
                (gnc:html-table-append-column! table date-string-list)

                (if show-inout?
                    (begin
                      (gnc:html-table-append-column! table in-list)
                      (gnc:html-table-append-column! table out-list)
                      ))
                (if show-net?
                    (gnc:html-table-append-column! table net-list))

                ;; set numeric columns to align right
                (for-each
                 (lambda (col)
                   (gnc:html-table-set-col-style!
                    table col "td"
                    'attribute (list "class" "number-cell")))
                 '(1 2 3))

                (gnc:html-document-add-object! doc table)
                )
              )

          )
        ;; else: error condition: no accounts specified
        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          reportname (gnc:report-id report-obj)))
        ) ;; if not null? accounts

    (gnc:report-finished)

    doc))


;; function to add inflow and outflow of money
(define (cashflow-barchart-calc-money-in-out settings)
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
         (splits-seen-table (make-hash-table)))

    (define (split-seen? split)
      (if (split-hashtable-ref splits-seen-table split) #t
          (begin
            (split-hashtable-set! splits-seen-table split #t)
            #f)))

    (define (work-per-split split)
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
                   (if (and      ;; make sure we don't have
                        (not (null? s-account)) ;;  any dangling splits
                        (or include-trading-accounts (not (eq? s-account-type ACCT-TYPE-TRADING)))
                        (not (is-report-account? s-account)))
                       (if (not (split-seen? s))
                           (begin
                             (if (gnc-numeric-negative-p s-value)
                                 (let ((s-account-in-collector (account-hashtable-ref money-in-hash s-account)))
                                        ;(gnc:debug "in:" (gnc-commodity-get-printname s-commodity)
                                        ;            (gnc-numeric-to-double s-amount)
                                        ;            (gnc-commodity-get-printname parent-currency)
                                        ;            (gnc-numeric-to-double s-value))
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
                                        ;            (gnc-numeric-to-double s-amount)
                                        ;            (gnc-commodity-get-printname parent-currency)
                                        ;            (gnc-numeric-to-double s-value))
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

    ;; Calculate money in and out for each split
    (for-each work-per-split all-splits)

    ;; Return an association list of results
    (list
     (cons 'money-in-collector money-in-collector)
     (cons 'money-out-collector money-out-collector))))

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
