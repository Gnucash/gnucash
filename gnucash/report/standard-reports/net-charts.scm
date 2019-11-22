;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; net-charts.scm : Display a time series line or bar chart for
;; either net worth or net profit.
;;
;; By Robert Merkel <rgmerk@mira.net>
;; and Christian Stimming <stimming@tu-harburg.de>
;; and Mike Evans <mikee@saxicooa.co.uk>
;; and Christopher Lam to combine with net-barchart.scm
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

(define-module (gnucash report standard-reports net-charts))

(use-modules (srfi srfi-1))
(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash report standard-reports category-barchart)) ; for guids of called reports
(gnc:module-load "gnucash/report/report-system" 0)


(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))

(define optname-inc-exp (N_ "Show Income/Expense"))
(define optname-show-profit (N_ "Show Net Profit"))

(define optname-sep-bars (N_ "Show Asset & Liability"))
(define optname-net-bars (N_ "Show Net Worth"))

(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define optname-line-width (N_ "Line Width"))
(define opthelp-line-width (N_ "Set line width in pixels."))

(define optname-markers (N_ "Data markers?"))

;;(define optname-x-grid (N_ "X grid"))
(define optname-y-grid (N_ "Grid"))

(define (options-generator inc-exp? linechart?)
  (let* ((options (gnc:new-options))
         ;; This is just a helper function for making options.
         ;; See libgnucash/scm/options.scm for details.
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
     optname-price-source "d" 'weighted-average)

    ;; Account tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (filter
         (if inc-exp?
             gnc:account-is-inc-exp?
             (lambda (account) (not (gnc:account-is-inc-exp? account))))
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (filter
               (if inc-exp?
                   gnc:account-is-inc-exp?
                   (lambda (account)
                     (not (gnc:account-is-inc-exp? account))))
               accounts)))
      #t))

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (if inc-exp? optname-inc-exp optname-sep-bars)
      "a"
      (if inc-exp?
          (N_ "Show Income and Expenses?")
          (N_ "Show the Asset and the Liability bars?"))
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (if inc-exp? optname-show-profit optname-net-bars)
      "b"
      (if inc-exp?
          (N_ "Show the net profit?")
          (N_ "Show a Net Worth bar?"))
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "c" (N_ "Display a table of the selected data.")
      #f))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

    (if linechart?
        (begin

          (add-option
           (gnc:make-number-range-option
            gnc:pagename-display optname-line-width
            "e" opthelp-line-width
            1.5 0.5 5 1 0.1 ))

          (add-option
           (gnc:make-simple-boolean-option
            gnc:pagename-display optname-y-grid
            "f" (N_ "Add grid lines.")
            #t))

          ;;(add-option
          ;; (gnc:make-simple-boolean-option
          ;;  gnc:pagename-display optname-x-grid
          ;;  "g" (N_ "Add vertical grid lines.")
          ;;  #f))

          (add-option
           (gnc:make-simple-boolean-option
            gnc:pagename-display optname-markers
            "g" (N_ "Display a mark for each data point.")
            #t))

          ))

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (net-renderer report-obj inc-exp? linechart?)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (gnc:report-starting "INC/EXP & A/L Charts")
  (let* ((to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-currency (get-option gnc:pagename-general optname-report-currency))
         (price-source (get-option gnc:pagename-general optname-price-source))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (show-sep? (get-option gnc:pagename-display (if inc-exp?
                                                         optname-inc-exp
                                                         optname-sep-bars)))
         (show-net? (get-option gnc:pagename-display (if inc-exp?
                                                         optname-show-profit
                                                         optname-net-bars)))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (markers (if linechart?
                      (get-option gnc:pagename-display optname-markers)))
         (line-width (if linechart?
                         (get-option gnc:pagename-display optname-line-width)))
         (y-grid (if linechart? (get-option gnc:pagename-display optname-y-grid)))
         ;;(x-grid (if linechart? (get-option gnc:pagename-display optname-x-grid)))
         (commodity-list #f)
         (exchange-fn #f)
         (dates-list (gnc:make-date-list
                      ((if inc-exp?
                           gnc:time64-start-day-time
                           gnc:time64-end-day-time)
                       from-date-t64)
                      (gnc:time64-end-day-time to-date-t64)
                      (gnc:deltasym-to-delta interval)))
         (report-title (get-option gnc:pagename-general gnc:optname-reportname))
         (classified-accounts (gnc:decompose-accountlist accounts))
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))
         (document (gnc:make-html-document))
         (chart (if linechart?
                    (gnc:make-html-linechart)
                    (gnc:make-html-barchart)))
         (non-zeros #f))

    (define (add-column! data-list)
      (begin
        ((if linechart?
             gnc:html-linechart-append-column!
             gnc:html-barchart-append-column!)
         chart data-list)
        (if (gnc:not-all-zeros data-list) (set! non-zeros #t))
        #f))

    ;; This exchanges the commodity-collector 'c' to one single
    ;; 'report-currency' according to the exchange-fn. Returns a gnc:monetary
    (define (collector->monetary c date)
      (if (not (number? date))
          (throw 'wrong))
      (gnc:sum-collector-commodity
       c report-currency
       (lambda (a b) (exchange-fn a b date))))

    ;; gets an account alist balances
    ;; output: (list acc bal0 bal1 bal2 ...)
    (define (account->balancelist account)
      (let ((comm (xaccAccountGetCommodity account)))
        (cons account
              (gnc:account-accumulate-at-dates
               account dates-list
               #:split->elt (lambda (s)
                              (gnc:make-gnc-monetary
                               comm (xaccSplitGetNoclosingBalance s)))
               #:nosplit->elt (gnc:make-gnc-monetary comm 0)))))

    ;; This calculates the balances for all the 'account-balances' for
    ;; each element of the list 'dates'. Uses the collector->monetary
    ;; conversion function above. Returns a list of gnc-monetary.
    (define (process-datelist account-balances dates left-col?)

      (define accountlist
        (if inc-exp?
            (if left-col?
                (assoc-ref classified-accounts ACCT-TYPE-INCOME)
                (assoc-ref classified-accounts ACCT-TYPE-EXPENSE))
            (if left-col?
                (assoc-ref classified-accounts ACCT-TYPE-ASSET)
                (assoc-ref classified-accounts ACCT-TYPE-LIABILITY))))

      (define filtered-account-balances
        (filter
         (lambda (a)
           (member (car a) accountlist))
         account-balances))

      (define (acc-balances->list-of-balances lst)
        ;; input: (list (list acc1 bal0 bal1 bal2 ...)
        ;;              (list acc2 bal0 bal1 bal2 ...) ...)
        ;; whereby list of balances are gnc-monetary objects
        ;; output: (list <mon-coll0> <mon-coll1> <mon-coll2>)
        (define (call thunk) (thunk))
        (if (null? lst)
            (map call (make-list (length dates) gnc:make-commodity-collector))
            (apply map gnc:monetaries-add (map cdr lst))))

      (let loop ((dates dates)
                 (acct-balances (acc-balances->list-of-balances filtered-account-balances))
                 (result '()))
        (if (if inc-exp?
                (null? (cdr dates))
                (null? dates))
            (reverse result)
            (loop (cdr dates)
                  (cdr acct-balances)
                  (cons
                   (collector->monetary
                    (if inc-exp?
                        (gnc:collector- (car acct-balances) (cadr acct-balances))
                        (car acct-balances))
                    (if inc-exp? (cadr dates) (car dates)))
                   result)))))

    (gnc:report-percent-done 1)
    (set! commodity-list (gnc:accounts-get-commodities
                          (gnc:accounts-and-all-descendants accounts)
                          report-currency))
    (gnc:report-percent-done 10)
    (set! exchange-fn (gnc:case-exchange-time-fn
                       price-source report-currency
                       commodity-list to-date-t64
                       10 40))
    (gnc:report-percent-done 50)

    (if
     (not (null? accounts))
     (let* ((account-balancelist (map account->balancelist accounts))
            (dummy (gnc:report-percent-done 60))

            (minuend-balances (process-datelist
                               account-balancelist
                               dates-list #t))
            (dummy (gnc:report-percent-done 70))

            (subtrahend-balances (process-datelist
                                  account-balancelist
                                  dates-list #f))
            (dummy (gnc:report-percent-done 80))

            (difference-balances (map gnc:monetary+ minuend-balances subtrahend-balances))

            (dates-list (if inc-exp?
                            (list-head dates-list (1- (length dates-list)))
                            dates-list))

            (date-string-list (map qof-print-date dates-list))
            
            (date-iso-string-list (let ((save-fmt (qof-date-format-get)))
                                    (qof-date-format-set QOF-DATE-FORMAT-ISO)
                                    (let ((retlist (map qof-print-date dates-list)))
                                      (qof-date-format-set save-fmt)
                                      retlist))))

       (gnc:report-percent-done 90)

       ((if linechart?
            gnc:html-linechart-set-title!
            gnc:html-barchart-set-title!)
        chart report-title)

       ((if linechart?
            gnc:html-linechart-set-subtitle!
            gnc:html-barchart-set-subtitle!)
        chart (format #f
                       (_ "~a to ~a")
                       (qof-print-date from-date-t64)
                       (qof-print-date to-date-t64)))

       ((if linechart?
            gnc:html-linechart-set-width!
            gnc:html-barchart-set-width!) chart width)

       ((if linechart?
            gnc:html-linechart-set-height!
            gnc:html-barchart-set-height!) chart height)

       (if linechart?
           (begin
             (gnc:html-linechart-set-row-labels! chart date-iso-string-list)
             (gnc:html-linechart-set-major-grid?! chart y-grid))
           (gnc:html-barchart-set-row-labels! chart date-string-list))

       ((if linechart?
            gnc:html-linechart-set-y-axis-label!
            gnc:html-barchart-set-y-axis-label!)
        chart (gnc-commodity-get-mnemonic report-currency))

       ;; Add the data
       (when show-sep?
         (add-column! (map gnc:gnc-monetary-amount minuend-balances))
         (add-column! (map - (map gnc:gnc-monetary-amount subtrahend-balances))))

       (if show-net?
           (add-column! (map gnc:gnc-monetary-amount difference-balances)))

       ;; Legend labels, colors
       ((if linechart?
            gnc:html-linechart-set-col-labels!
            gnc:html-barchart-set-col-labels!)
        chart (append
               (if show-sep?
                   (if inc-exp?
                       (list (_ "Income") (_ "Expense"))
                       (list (_ "Assets") (_ "Liabilities")))
                   '())
               (if show-net?
                   (if inc-exp?
                       (list (_ "Net Profit"))
                       (list (_ "Net Worth")))
                   '())))

       ((if linechart?
            gnc:html-linechart-set-col-colors!
            gnc:html-barchart-set-col-colors!)
        chart (append
               (if show-sep?
                   '("#0074D9" "#FF4136") '())
               (if show-net?
                   '("#2ECC40") '())))

       ;; Set the line width and markers
       (if linechart?
           (begin
             (gnc:html-linechart-set-line-width! chart line-width)
             (gnc:html-linechart-set-markers?! chart markers)))

       ;; URLs for income/expense or asset/liabilities bars.
       ;;       (if show-sep?
       ;;           (let ((urls
       ;;                  (list
       ;;                   (gnc:make-report-anchor
       ;;                    (if inc-exp?
       ;;                        category-barchart-income-uuid
       ;;                        category-barchart-asset-uuid)
       ;;                    report-obj
       ;;                    (list
       ;;                     (list gnc:pagename-display
       ;;                           (if linechart? "Use Stacked Lines" "Use Stacked Bars") #t)
       ;;                     (list gnc:pagename-general
       ;;                           gnc:optname-reportname
       ;;                           (if inc-exp?
       ;;                               (_ "Income Chart")
       ;;                               (_ "Asset Chart")))))
       ;;                   (gnc:make-report-anchor
       ;;                    (if inc-exp?
       ;;                        category-barchart-expense-uuid
       ;;                        category-barchart-liability-uuid)
       ;;                    report-obj
       ;;                    (list
       ;;                     (list gnc:pagename-display
       ;;                           (if linechart? "Use Stacked Lines" "Use Stacked Bars") #t)
       ;;                     (list gnc:pagename-general
       ;;                           gnc:optname-reportname
       ;;                           (if inc-exp?
       ;;                               (_ "Expense Chart")
       ;;                               (_ "Liability Chart"))))))))
       ;;             ((if linechart?
       ;;                  gnc:html-linechart-set-button-1-line-urls!
       ;;                  gnc:html-barchart-set-button-1-line-urls!)
       ;;              chart urls)
       ;;             ((if linechart?
       ;;                  gnc:html-linechart-set-button-1-legend-urls!
       ;;                  gnc:html-barchart-set-button-1-legend-urls!)
       ;;              chart urls)))

       ;; Test for all-zero data here.
       (if non-zeros
           (begin
             (gnc:html-document-add-object! document chart)
             (if show-table?
                 (let ((table (gnc:make-html-table)))
                   (gnc:html-table-set-style!
                    table "table"
                    'attribute (list "border" 0)
                    'attribute (list "cellspacing" 0)
                    'attribute (list "cellpadding" 4))
                   (gnc:html-table-set-col-headers!
                    table
                    (append
                     (list (_ "Date"))
                     (if show-sep?
                         (if inc-exp?
                             (list (_ "Income") (_ "Expense"))
                             (list (_ "Assets") (_ "Liabilities")))
                         '())
                     (if show-net?
                         (if inc-exp?
                             (list (_ "Net Profit"))
                             (list (_ "Net Worth")))
                         '())))
                   (gnc:html-table-append-column! table date-string-list)
                   (when show-sep?
                     (gnc:html-table-append-column! table minuend-balances)
                     (gnc:html-table-append-column! table subtrahend-balances))

                   (if show-net?
                       (gnc:html-table-append-column! table difference-balances))

                   ;; set numeric columns to align right
                   (for-each
                    (lambda (col)
                      (gnc:html-table-set-col-style!
                       table col "td"
                       'attribute (list "class" "number-cell")))
                    '(1 2 3))

                   (gnc:html-document-add-object! document table))
                 ))
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
             report-title (gnc:report-id report-obj)))))

     ;; else no accounts selected
     (gnc:html-document-add-object!
      document
      (gnc:html-make-no-account-warning
       report-title (gnc:report-id report-obj))))

    (gnc:report-finished)
    document))

;; Export reports

(export net-worth-barchart-uuid)
(export net-worth-linechart-uuid)
(export income-expense-barchart-uuid)

(define net-worth-linechart-uuid "d8b63264186b11e19038001558291366")
(define net-worth-barchart-uuid "cbba1696c8c24744848062c7f1cf4a72")
(define income-expense-barchart-uuid "80769921e87943adade887b9835a7685")

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name (N_ "Net Worth Barchart")
 'report-guid net-worth-barchart-uuid
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator (lambda () (options-generator #f #f))
 'renderer (lambda (report-obj) (net-renderer report-obj #f #f)))

(gnc:define-report
 'version 1
 'name (N_ "Income/Expense Chart")
 'report-guid income-expense-barchart-uuid
 'menu-name (N_ "Income & Expense Barchart")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator (lambda () (options-generator #t #f))
 'renderer (lambda (report-obj) (net-renderer report-obj #t #f)))

(gnc:define-report
 'version 1
 'name (N_ "Net Worth Linechart")
 'report-guid net-worth-linechart-uuid
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator (lambda () (options-generator #f #t))
 'renderer (lambda (report-obj) (net-renderer report-obj #f #t)))

;; Not sure if a line chart makes sense for Income & Expense
;; Feel free to uncomment and try it though
(gnc:define-report
 'version 1
 'name (N_ "Income & Expense Linechart")
 'report-guid "e533c998186b11e1b2e2001558291366"
 'menu-name (N_ "Income & Expense Linechart")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator (lambda () (options-generator #t #t))
 'renderer (lambda (report-obj) (net-renderer report-obj #t #t)))
