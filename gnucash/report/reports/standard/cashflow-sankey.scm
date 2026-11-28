;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright Brad Hajek <brad-hajek@users.noreply.github.com> 2026
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
(define-module (gnucash reports standard cashflow-sankey))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))
(use-modules (ice-9 format))
(use-modules (ice-9 hash-table))
(use-modules (ice-9 i18n))
(use-modules (srfi srfi-1))

;; For debugging during development, enable backtraces to get more detailed error information
;; in the GnuCash error logs when something goes wrong.
; (debug-enable 'backtrace)

(define report-title (G_ "Cash Flow Sankey"))

(define optinterval-name (G_ "Date Interval"))
(define optinterval-help (G_ "The date interval for the report."))
(define optinterval-from-date (G_ "Start Date"))
(define optinterval-to-date (G_ "End Date"))

(define optaccountlist-name (G_ "Account List"))
(define optaccountlist-help (G_ "The accounts to include in the report."))

;; Minimum incoming flow value to include in the Sankey diagram
;; i.e., show whole dollars only, ignore tiny flows that add noise
(define optminimum-name (G_ "Minimum Flow Value"))
(define optminimum-help (G_ "The minimum incoming flow value to include in the report."))
(define optminimum-default 1.00)

(define optsvg-width-name (G_ "Plot Width"))
(define optsvg-width-help (G_ "The width of the Sankey diagram."))
(define optsvg-width-default 2000)

(define optsvg-height-name (G_ "Plot Height"))
(define optsvg-height-help (G_ "The height of the Sankey diagram."))
(define optsvg-height-default 1000)

(define optsvg-x-axis-style-name (G_ "Flow X-Axis Style"))
(define optsvg-x-axis-style-help (G_ "The style for the X-axis in the Sankey diagram."))
(define optsvg-x-axis-style-default "fixed")

;; Node colors for different account types, with defaults chosen for good contrast and aesthetics
;; Users can customize these in the report options to fit their preferences or color schemes.
;; GnuCash expects just the hex digits so we'll add the # back when generating the CSS for the report.
(define optnodecolor-income-name (G_ "Income Node Color"))
(define optnodecolor-income-help (G_ "The color used for income nodes in the Sankey diagram."))
(define optnodecolor-income-default "27ae60") ;; Forest Green #27ae60
(define optnodecolor-expense-name (G_ "Expense Node Color"))
(define optnodecolor-expense-help (G_ "The color used for expense nodes in the Sankey diagram."))
(define optnodecolor-expense-default "c0392b") ;; Rich Red #c0392b
(define optnodecolor-asset-name (G_ "Asset Node Color"))
(define optnodecolor-asset-help (G_ "The color used for asset nodes in the Sankey diagram."))
(define optnodecolor-asset-default "2980b9") ;; Ice Blue #2980b9
(define optnodecolor-liability-name (G_ "Liability Node Color"))
(define optnodecolor-liability-help (G_ "The color used for liability nodes in the Sankey diagram."))
(define optnodecolor-liability-default "f6d32d") ;; Golden Yellow #f6d32d
(define optnodecolor-equity-name (G_ "Equity Node Color"))
(define optnodecolor-equity-help (G_ "The color used for equity nodes in the Sankey diagram."))
(define optnodecolor-equity-default "8e44ad") ;; Plum Purple #8e44ad
(define optnodecolor-fallback-name (G_ "Fallback Node Color"))
(define optnodecolor-fallback-help (G_ "The color used for nodes that do not fit into other categories."))
(define optnodecolor-fallback-default "7f8c8d") ;; Slate Grey #7f8c8d

(define accounts-page gnc:pagename-accounts)
(define general-page  gnc:pagename-general)
(define display-page  (G_ "Display"))

(define (extract-sankey-links account-list start-date end-date flow-minimum)
  (let ((links '())
        (transaction-cache (make-hash-table)))
    ;; Cache positive destination split decomposition per transaction so we
    ;; avoid rescanning the same transaction when encountered multiple times.
    (define (get-positive-destinations trans)
      (let ((cached (hash-ref transaction-cache trans #f)))
        (if cached
            cached
            (let loop ((remaining (xaccTransGetSplitList trans))
                       (dest-splits '())
                       (dest-total 0.0))
              (if (null? remaining)
                  (let ((result (cons (reverse dest-splits) dest-total)))
                    (hash-set! transaction-cache trans result)
                    result)
                  (let* ((split (car remaining))
                         (split-val (gnc-numeric-to-double (xaccSplitGetAmount split))))
                    (if (> split-val 0.0)
                        (loop (cdr remaining)
                              (cons (cons split split-val) dest-splits)
                              (+ dest-total split-val))
                        (loop (cdr remaining) dest-splits dest-total))))))))

    (for-each 
      (lambda (account)
        (let ((splits (xaccAccountGetSplitList account)))
          (for-each 
            (lambda (src-split)
              (let* ((trans (xaccSplitGetParent src-split))
                     ;; Transaction dates are already time64 values.
                     (date (xaccTransGetDate trans))
                     ;; Normalize split amount to inexact numeric for arithmetic below.
                     (amount (gnc-numeric-to-double (xaccSplitGetAmount src-split))))

                ;; Compare dates against the discrete start and end variables
                (if (and (>= date start-date) (<= date end-date) (< amount 0.0))
                    (let* ((dest-info (get-positive-destinations trans))
                           (dest-splits (car dest-info))
                           (total-dest-val (cdr dest-info)))

                      (if (>= total-dest-val flow-minimum)
                        (for-each (lambda (dest-entry)
                                    (let* ((dest-split (car dest-entry))
                                           (dest-val (cdr dest-entry))
                                           (dest-acc (xaccSplitGetAccount dest-split))
                                           (flow-val (* (abs amount) (/ dest-val total-dest-val))))
                                      (set! links (cons (list (list (gnc-account-get-full-name account)
                                                                (xaccAccountTypeGetFundamental (xaccAccountGetType account)))
                                                              (list (gnc-account-get-full-name dest-acc)
                                                                (xaccAccountTypeGetFundamental (xaccAccountGetType dest-acc)))
                                                              flow-val)
                                                        links))))
                                  dest-splits))))))
            splits)))
      account-list)
    links))

;; aggregate the values of matching links (same source and destination)
(define (aggregate-links links)
  (let ((agg-by-pair (make-hash-table))
        (pair-order '()))
    (for-each (lambda (link)
                (let* ((src (car (car link)))
                       (src-type (cadr (car link)))
                       (dest (car (cadr link)))
                       (dest-type (cadr (cadr link)))
                       (val (caddr link))
                       (pair-key (format #f "~a\x1f~a" src dest))
                       (existing-link (hash-ref agg-by-pair pair-key #f)))
                  (if existing-link
                      (hash-set! agg-by-pair
                                 pair-key
                                 (list (list src src-type)
                                       (list dest dest-type)
                                       (+ val (caddr existing-link))))
                      (begin
                        (hash-set! agg-by-pair pair-key link)
                        (set! pair-order (cons pair-key pair-order))))))
              links)
    (map (lambda (pair-key)
           (hash-ref agg-by-pair pair-key))
         (reverse pair-order))))

(define (sankey-options-generator)
  (let* ((options (gnc-new-optiondb)))

    (gnc:options-add-date-interval! options
      general-page                              ;; Tab
      optinterval-from-date optinterval-to-date ;; Option Names
      "a")                                      ;; Sorting key

    (gnc-register-multichoice-option options
      general-page                ;; Tab
      optsvg-x-axis-style-name    ;; Option Name
      "b"                         ;; Sorting key
      optsvg-x-axis-style-help    ;; Help text
      optsvg-x-axis-style-default ;; Default value
      (list (vector 'dynamic (G_ "Dynamic"))  ; Position nodes based on their relationships, which is more visually informative but can lead to shifting positions between reports
            (vector 'fixed (G_ "Fixed")))) ; Position nodes based on their account type, which is more consistent but can be less visually intuitive

    (gnc-register-number-range-option options
      general-page        ;; Tab
      optminimum-name     ;; Option Name
      "c"                 ;; Sorting key
      optminimum-help     ;; Help text
      optminimum-default  ;; default
      0.0                 ;; lower bound
      10000.0             ;; upper bound
      0.01)               ;; step size

    (gnc-register-account-list-option options
      accounts-page       ;; Tab
      optaccountlist-name ;; Option Name
      "a"                 ;; Sorting key
      optaccountlist-help ;; Help text
      (gnc:filter-accountlist-type
         ;; select, by default, only income and expense accounts
         (list ACCT-TYPE-BANK ACCT-TYPE-EXPENSE ACCT-TYPE-INCOME ACCT-TYPE-CREDIT)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))

    ;; Plot size options
    (gnc:options-add-plot-size! options
      display-page            ;; Tab
      optsvg-width-name       ;; Option 1 Name
      optsvg-height-name      ;; Option 2 Name
      "a"                     ;; Sorting key
      optsvg-width-default optsvg-height-default) ;; Default values

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-income-name        ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-income-help        ;; Help text
      optnodecolor-income-default)    ;; Default value

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-expense-name       ;; Option Name
      "d"                             ;; Sorting key
      optnodecolor-expense-help       ;; Help text
      optnodecolor-expense-default)   ;; Default value

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-asset-name         ;; Option Name
      "e"                             ;; Sorting key
      optnodecolor-asset-help         ;; Help text
      optnodecolor-asset-default)     ;; Default value

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-liability-name     ;; Option Name
      "f"                             ;; Sorting key
      optnodecolor-liability-help     ;; Help text
      optnodecolor-liability-default) ;; Default value

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-equity-name        ;; Option Name
      "g"                             ;; Sorting key
      optnodecolor-equity-help        ;; Help text
      optnodecolor-equity-default)    ;; Default value

    (gnc-register-color-option options
      display-page                    ;; Tab
      optnodecolor-fallback-name      ;; Option Name
      "h"                             ;; Sorting key
      optnodecolor-fallback-help      ;; Help text
      optnodecolor-fallback-default)  ;; Default value

    (gnc:options-set-default-section options general-page) ;; Set the default tab to "General" when the user opens the report

    options))

(define (sankey-renderer report-obj)
  ;; Helper function for looking up option values.
  (define (op-value section name)
    (let ((options (gnc:report-options report-obj)))
      (gnc-optiondb-lookup-value options section name)))

  (let* (
    ;; Fetching the option values
    (options (gnc:report-options report-obj))
    (from-date (gnc:date-option-absolute-time (op-value general-page optinterval-from-date)))
    (to-date (gnc:date-option-absolute-time (op-value general-page optinterval-to-date)))
    (interval (op-value general-page optinterval-name))
    (flow-minimum (op-value general-page optminimum-name))
    (selected-accounts (op-value accounts-page optaccountlist-name))

    ;; If no accounts were explicitly selected, use all accounts in the book as the default
    (accounts (if (null? selected-accounts)
      (gnc-account-get-descendants-sorted (gnc-book-get-root-account (gnc-get-current-book)))
        selected-accounts))

    ;; Plot dimensions, concverting from percentage if needed.
    ;; The Sankey diagram will scale with the SVG, so percentages are relative to the default size.
    (svg-width-type (car (op-value display-page optsvg-width-name)))
    (svg-width-value (cdr (op-value display-page optsvg-width-name)))
    (svg-width (if (eq? svg-width-type 'percent)
                    (* svg-width-value optsvg-width-default 0.01)
                    svg-width-value))

    (svg-height-type (car (op-value display-page optsvg-height-name)))
    (svg-height-value (cdr (op-value display-page optsvg-height-name)))
    (svg-height (if (eq? svg-height-type 'percent)
                    (* svg-height-value optsvg-height-default 0.01)
                    svg-height-value))

    (x-axis-style (symbol->string (op-value general-page optsvg-x-axis-style-name)))

    ;; Prepare colors, converting from hex strings to CSS format (e.g., "27ae60" -> "#27ae60")
    (income-color (format #f "#~a" (op-value display-page optnodecolor-income-name)))
    (expense-color (format #f "#~a" (op-value display-page optnodecolor-expense-name)))
    (asset-color (format #f "#~a" (op-value display-page optnodecolor-asset-name)))
    (liability-color (format #f "#~a" (op-value display-page optnodecolor-liability-name)))
    (equity-color (format #f "#~a" (op-value display-page optnodecolor-equity-name)))
    (fallback-color (format #f "#~a" (op-value display-page optnodecolor-fallback-name)))

    ;; Format the from and to dates as strings for display in the report
    ;; as well as convert them to time64 values for data extraction/comparison
    (from-date-string (gnc-print-time64 from-date "%x"))
    (to-date-string (gnc-print-time64 to-date "%x"))
    (from-date-t64 (gnc:time64-start-day-time from-date))
    (to-date-t64 (gnc:time64-end-day-time to-date))

    ;; Extract the sankey links based on the selected accounts and date range,
    ;; then convert to a JavaScript array format for embedding in the HTML/JS
    (data-links (extract-sankey-links accounts from-date-t64 to-date-t64 flow-minimum))
    (links (aggregate-links data-links))

    ;; Create the report object that will hold the HTML content
    (report (gnc:make-html-document))
    (sankey (gnc:make-html-sankey)))

    ;; Now we construct the HTML report, embedding the JavaScript and data for the Sankey diagram.
    ;; The JS code will run in the context of the report's HTML document
    (gnc:html-document-set-title! report report-title)
    ;; (gnc:html-document-add-object! report (gnc:html-render-options-changed options))

    (gnc:html-sankey-set-from-date! sankey from-date-string)
    (gnc:html-sankey-set-to-date! sankey to-date-string)
    (gnc:html-sankey-set-width! sankey svg-width)
    (gnc:html-sankey-set-height! sankey svg-height)
    (gnc:html-sankey-set-x-axis-style! sankey x-axis-style)
    (gnc:html-sankey-set-income-color! sankey income-color)
    (gnc:html-sankey-set-expense-color! sankey expense-color)
    (gnc:html-sankey-set-asset-color! sankey asset-color)
    (gnc:html-sankey-set-liability-color! sankey liability-color)
    (gnc:html-sankey-set-equity-color! sankey equity-color)
    (gnc:html-sankey-set-fallback-color! sankey fallback-color)
    (gnc:html-sankey-set-links! sankey links)

    (gnc:html-document-add-object! report sankey)
  report))

;; --- 4. REGISTRATION ---
(gnc:define-report
 'version 1
 'name report-title
 'report-guid "8374f6b5434442679347f43cb08d2092"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator sankey-options-generator
 'renderer sankey-renderer)
