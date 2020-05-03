;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balance-forecast.scm
;; Simulate future balance based on scheduled transactions.
;;
;; By Ryan Turner 2019-02-27 <zdbiohazard2@gmail.com>
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

(define-module (gnucash report standard-reports balance-forecast))

(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/report/report-system" 0)

; Name definitions
(define reportname           (N_ "Balance Forecast"))

(define optname-accounts     (N_ "Accounts"))
(define opthelp-accounts     (_ "Report on these accounts."))

(define optname-from-date    (N_ "Start Date"))
(define optname-to-date      (N_ "End Date"))
(define optname-interval     (N_ "Interval"))

(define optname-currency     (N_ "Report's currency"))
(define optname-price        (N_ "Price Source"))

(define optname-plot-width   (N_ "Plot Width"))
(define optname-plot-height  (N_ "Plot Height"))
(define optname-show-markers (N_ "Data markers?"))
(define opthelp-show-markers (_ "Display a mark for each data point."))

(define optname-show-reserve (N_ "Show reserve line"))
(define opthelp-show-reserve (_ "Show reserve line"))

(define optname-reserve      (N_ "Reserve amount"))
(define opthelp-reserve      (_ "The reserve amount is set to a \
minimum balance desired"))

(define optname-show-target  (N_ "Show target line"))
(define opthelp-show-target  (_ "Show target line"))

(define optname-target       (N_ "Target amount above reserve"))
(define opthelp-target       (_ "The target is used to plan for \
a future large purchase, which will be added as a line above the \
reserve amount."))

(define optname-show-minimum (N_ "Show future minimum"))
(define opthelp-show-minimum (_ "The future minimum will add, for each \
date point, a projected minimum balance including scheduled transactions."))

; Options generator
(define (options-generator)
  (let* ((options (gnc:new-options)))
    ; Account selector
    (gnc:register-option options
      (gnc:make-account-list-option
        gnc:pagename-accounts optname-accounts "a" opthelp-accounts
        (lambda ()
          (gnc:filter-accountlist-type
            (list ACCT-TYPE-BANK ACCT-TYPE-CASH)
            (gnc-account-get-descendants-sorted
              (gnc-get-current-root-account))))
        #f #t))

    ; Date range
    (gnc:options-add-date-interval! options
      gnc:pagename-general optname-from-date optname-to-date "a")
    ; Date interval
    (gnc:options-add-interval-choice! options
      gnc:pagename-general optname-interval "b" 'DayDelta)
    ; Report currency
    (gnc:options-add-currency! options
      gnc:pagename-general optname-currency "c")
    ; Price source
    (gnc:options-add-price-source! options
      gnc:pagename-general optname-price "d" 'pricedb-nearest)

    ; Plot size
    (gnc:options-add-plot-size! options gnc:pagename-display
      optname-plot-width optname-plot-height "a"
      (cons 'percent 100.0) (cons 'percent 100.0))
    ; Markers
    (gnc:register-option options (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-markers "b" opthelp-show-markers #f))
    ; Reserve line
    (gnc:register-option options (gnc:make-complex-boolean-option
      gnc:pagename-display optname-show-reserve "c" opthelp-show-reserve #f #f
      (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options gnc:pagename-display optname-reserve x))))
    (gnc:register-option options (gnc:make-number-range-option
      gnc:pagename-display optname-reserve "d" opthelp-reserve
      0 -10E9 10E9 2 0.01))
    ; Purchasing power target
    (gnc:register-option options (gnc:make-complex-boolean-option
      gnc:pagename-display optname-show-target "e" opthelp-show-target #f #f
      (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options gnc:pagename-display optname-target x))))
    (gnc:register-option options (gnc:make-number-range-option
      gnc:pagename-display optname-target "f" opthelp-target
      0 -10E9 10E9 2 0.01))
    ; Future minimum
    (gnc:register-option options (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-minimum "g" opthelp-show-minimum #f))
    (gnc:options-set-default-section options gnc:pagename-general)
    options)
)

; Renderer
(define (document-renderer report-obj)
  ; Option-getting helper function.
  (define (get-option pagename optname)
    (gnc:option-value
      (gnc:lookup-option (gnc:report-options report-obj) pagename optname)))
  (define report-title
    (get-option gnc:pagename-general gnc:optname-reportname))

  (gnc:report-starting report-title)

  (let* ( (document (gnc:make-html-document))
          ; Options
          (accounts (get-option gnc:pagename-accounts optname-accounts))

          (from-date (gnc:time64-start-day-time (gnc:date-option-absolute-time
            (get-option gnc:pagename-general optname-from-date))))
          (to-date (gnc:time64-end-day-time (gnc:date-option-absolute-time
            (get-option gnc:pagename-general optname-to-date))))
          (interval (get-option gnc:pagename-general optname-interval))
          (currency (get-option gnc:pagename-general optname-currency))
          (price (get-option gnc:pagename-general optname-price))

          (plot-width (get-option gnc:pagename-display optname-plot-width))
          (plot-height (get-option gnc:pagename-display optname-plot-height))
          (show-markers (get-option gnc:pagename-display optname-show-markers))
          (show-reserve (get-option gnc:pagename-display optname-show-reserve))
          (reserve (get-option gnc:pagename-display optname-reserve))
          (show-target (get-option gnc:pagename-display optname-show-target))
          (target (get-option gnc:pagename-display optname-target))
          (show-minimum (get-option gnc:pagename-display optname-show-minimum))

          ; Variables
          (chart (gnc:make-html-linechart))
          (series '())
          (intervals (gnc:make-date-interval-list
            from-date to-date (gnc:deltasym-to-delta interval)))
          (accum (gnc:make-commodity-collector))
          (exchange-fn (gnc:case-exchange-time-fn
                        price currency
                        (gnc:accounts-get-commodities accounts #f)
                        to-date #f #f))
          (accounts-balancelist
           (map
            (lambda (acc)
              (gnc:account-get-balances-at-dates acc (map cadr intervals)))
            accounts)))

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((every zero? (map gnc:gnc-monetary-amount (apply append accounts-balancelist)))
      (gnc:html-document-add-object!
       document
       (gnc:html-make-empty-data-warning
        report-title (gnc:report-id report-obj))))

     (else
      ;; initialize the SX balance accumulator with the instantiated SX
      ;; amounts starting from the earliest split date in the list of
      ;; accounts up to the report start date.
      (let* ((accounts-dates (map (compose xaccTransGetDate xaccSplitGetParent car)
                                  (filter pair?
                                          (map xaccAccountGetSplitList accounts))))
             (earliest (and (pair? accounts-dates) (apply min accounts-dates)))
             (sx-hash (if earliest
                          (gnc-sx-all-instantiate-cashflow-all earliest from-date)
                          (make-hash-table))))
        (for-each
         (lambda (account)
           (accum 'add (xaccAccountGetCommodity account)
                  (hash-ref sx-hash (gncAccountGetGUID account) 0)))
         accounts))

      ;; Calculate balances
      (let ((balances
             (map
              (lambda (date accounts-balance)
                (let* ((start-date (car date))
                       (end-date (cadr date))
                       (balance (gnc:make-commodity-collector))
                       (sx-value (gnc-sx-all-instantiate-cashflow-all
                                  start-date end-date)))
                  (for-each
                   (lambda (account account-balance)
                     (accum 'add (xaccAccountGetCommodity account)
                            (hash-ref sx-value (gncAccountGetGUID account) 0))
                     (balance 'add (gnc:gnc-monetary-commodity account-balance)
                              (gnc:gnc-monetary-amount account-balance)))
                   accounts accounts-balance)
                  (balance 'merge accum #f)
                  (gnc:gnc-monetary-amount
                   (gnc:sum-collector-commodity
                    balance currency
                    (lambda (monetary target-curr)
                      (exchange-fn monetary target-curr end-date))))))
              intervals (apply zip accounts-balancelist))))

        ;; Minimum line
        (when show-minimum
          (set! series (cons (list (_ "Minimum") "#0AA") series))
          (gnc:html-linechart-append-column!
           chart (let loop ((balances balances) (result '()))
                   (if (null? balances) (reverse! result)
                       (loop (cdr balances) (cons (apply min balances) result))))))

        ;; Balance line (do this here so it draws over the minimum line)
        (set! series (cons (list (_ "Balance") "#0A0") series))
        (gnc:html-linechart-append-column! chart balances)

        ;; Target line
        (when show-target
          (set! series (cons (list (_ "Target") "#FF0") series))
          (gnc:html-linechart-append-column!
           chart (make-list (length intervals) (+ reserve target))))

        ;; Reserve line
        (when show-reserve
          (set! series (cons (list (_ "Reserve") "#F00") series))
          (gnc:html-linechart-append-column!
           chart (make-list (length intervals) reserve)))

        ;; Set the chart titles
        (gnc:html-linechart-set-title! chart report-title)
        (gnc:html-linechart-set-subtitle!
         chart (format #f (_ "~a to ~a")
                       (qof-print-date from-date) (qof-print-date to-date)))
        ;; Set the chart size
        (gnc:html-linechart-set-width! chart plot-width)
        (gnc:html-linechart-set-height! chart plot-height)
        ;; Set the axis labels
        (gnc:html-linechart-set-y-axis-label!
         chart (gnc-commodity-get-mnemonic currency))
        ;; Set line markers
        (gnc:html-linechart-set-markers?! chart show-markers)
        ;; Set series labels
        (let ((old-fmt (qof-date-format-get)))
          (qof-date-format-set QOF-DATE-FORMAT-ISO)
          (gnc:html-linechart-set-row-labels!
           chart (map qof-print-date (map cadr intervals)))
          (qof-date-format-set old-fmt))
        (gnc:html-linechart-set-col-labels! chart (map car (reverse series)))
        ;; Assign line colors
        (gnc:html-linechart-set-col-colors! chart (map cadr (reverse series)))

        ;; We're done!
        (gnc:html-document-add-object! document chart)
        (gnc:report-finished))))
    document))

(gnc:define-report
  'version 1
  'name reportname
  'report-guid "321d940d487d4ccbb4bd0467ffbadbf2"
  'menu-path (list gnc:menuname-asset-liability)
  'options-generator options-generator
  'renderer document-renderer)
