;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By Brad Hajek <brad-hajek@users.noreply.github.com>
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

;; For debugging during development, enable backtraces to get more detailed error information
;; in the GnuCash error logs when something goes wrong.
(debug-enable 'backtrace)

(define report-title (N_ "Cash Flow Sankey"))

(define optinterval-name (N_ "Date Interval"))
(define optinterval-help (N_ "The date interval for the report."))
(define optinterval-from-date (N_ "Start Date"))
(define optinterval-to-date (N_ "End Date"))

(define optaccountlist-name (N_ "Account List"))
(define optaccountlist-help (N_ "The accounts to include in the report."))

;; Minimum incoming flow value to include in the Sankey diagram
;; i.e., show whole dollars only, ignore tiny flows that add noise
(define optminimum-name (N_ "Minimum Flow Value"))
(define optminimum-help (N_ "The minimum incoming flow value to include in the report."))
(define optminimum-default 1.00)

;; Node colors for different account types, with defaults chosen for good contrast and aesthetics
;; Users can customize these in the report options to fit their preferences or color schemes.
;; GnuCash expects just the hex digits so we'll add the # back when generating the CSS for the report.
(define optnodecolor-income-name (N_ "Income Node Color"))
(define optnodecolor-income-help (N_ "The color used for income nodes in the Sankey diagram."))
(define optnodecolor-income-default "27ae60") ;; Forest Green #27ae60
(define optnodecolor-expense-name (N_ "Expense Node Color"))
(define optnodecolor-expense-help (N_ "The color used for expense nodes in the Sankey diagram."))
(define optnodecolor-expense-default "c0392b") ;; Rich Red #c0392b
(define optnodecolor-asset-name (N_ "Asset Node Color"))
(define optnodecolor-asset-help (N_ "The color used for asset nodes in the Sankey diagram."))
(define optnodecolor-asset-default "2980b9") ;; Ice Blue #2980b9
(define optnodecolor-liability-name (N_ "Liability Node Color"))
(define optnodecolor-liability-help (N_ "The color used for liability nodes in the Sankey diagram."))
(define optnodecolor-liability-default "f6d32d") ;; Golden Yellow #f6d32d
(define optnodecolor-equity-name (N_ "Equity Node Color"))
(define optnodecolor-equity-help (N_ "The color used for equity nodes in the Sankey diagram."))
(define optnodecolor-equity-default "8e44ad") ;; Plum Purple #8e44ad
(define optnodecolor-fallback-name (N_ "Fallback Node Color"))
(define optnodecolor-fallback-help (N_ "The color used for nodes that do not fit into other categories."))
(define optnodecolor-fallback-default "7f8c8d") ;; Slate Grey #7f8c8d

(define accounts-page    gnc:pagename-accounts)
(define general-page     gnc:pagename-general)
(define colors-page      (N_ "Node Colors"))

(define (extract-sankey-links account-list start-date end-date flow-minimum)
  (let ((links '()))
    (for-each 
      (lambda (account)
        (let ((splits (xaccAccountGetSplitList account)))
          (for-each 
            (lambda (src-split)
              (let* ((trans (xaccSplitGetParent src-split))
                     (date (gnc:time64-start-day-time (gnc:date-option-absolute-time (xaccTransGetDate trans))))
                     (amount (xaccSplitGetAmount src-split)))

                ;; Compare dates against the discrete start and end variables
                (if (and (>= date start-date) (<= date end-date) (< amount 0))
                    (let* ((all-splits (xaccTransGetSplitList trans))
                           (dest-splits (filter (lambda (s) (> (gnc-numeric-to-double (xaccSplitGetAmount s)) 0)) all-splits))
                           (total-dest-val (apply + (map (lambda (s) (gnc-numeric-to-double (xaccSplitGetAmount s))) dest-splits))))

                      (if (>= total-dest-val flow-minimum)
                        (for-each (lambda (dest-split)
                                    (let* ((dest-acc (xaccSplitGetAccount dest-split))
                                           (dest-val (gnc-numeric-to-double (xaccSplitGetAmount dest-split)))
                                           (flow-val (* (abs amount) (/ dest-val total-dest-val))))
                                      (set! links (cons (list (gnc-account-get-full-name account)
                                                              (gnc-account-get-full-name dest-acc)
                                                              flow-val)
                                                        links))))
                                  dest-splits))))))
            splits)))
      account-list)
    links))

(define (links->js-array links)
  (string-append "["
    (string-join 
      (map (lambda (link)
            (format #f "['~a', '~a', ~,2f]" (car link) (cadr link) (caddr link)))
          links)
      ",")
    "]"))

(define (sankey-options-generator)
  (let* ((options (gnc-new-optiondb)))

    (gnc:options-add-date-interval! options
      general-page                              ;; Tab
      optinterval-from-date optinterval-to-date ;; Option Names
      "b")                                      ;; Sorting key

    (gnc-register-number-range-option options
      general-page        ;; Tab
      optminimum-name     ;; Option Name
      "a"                 ;; Sorting key
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
         (list ACCT-TYPE-EXPENSE ACCT-TYPE-INCOME)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-income-name        ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-income-help        ;; Help text
      optnodecolor-income-default)    ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-expense-name       ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-expense-help       ;; Help text
      optnodecolor-expense-default)   ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-asset-name         ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-asset-help         ;; Help text
      optnodecolor-asset-default)     ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-liability-name     ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-liability-help     ;; Help text
      optnodecolor-liability-default) ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-equity-name        ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-equity-help        ;; Help text
      optnodecolor-equity-default)    ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-fallback-name      ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-fallback-help      ;; Help text
      optnodecolor-fallback-default)  ;; Default value

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

    ;; Prepare colors, converting from hex strings to CSS format (e.g., "27ae60" -> "#27ae60")
    (income-color (format #f "#~a" (op-value colors-page optnodecolor-income-name)))
    (expense-color (format #f "#~a" (op-value colors-page optnodecolor-expense-name)))
    (asset-color (format #f "#~a" (op-value colors-page optnodecolor-asset-name)))
    (liability-color (format #f "#~a" (op-value colors-page optnodecolor-liability-name)))
    (equity-color (format #f "#~a" (op-value colors-page optnodecolor-equity-name)))
    (fallback-color (format #f "#~a" (op-value colors-page optnodecolor-fallback-name)))

    ;; Format the from and to dates as strings for display in the report
    ;; as well as convert them to time64 values for data extraction/comparison
    (from-date-string (gnc-print-time64 from-date "%x"))
    (to-date-string (gnc-print-time64 to-date "%x"))
    (from-date-t64 (gnc:time64-start-day-time from-date))
    (to-date-t64 (gnc:time64-end-day-time to-date))

    ;; Extract the sankey links based on the selected accounts and date range,
    ;; then convert to a JavaScript array format for embedding in the HTML/JS
    (data-links (extract-sankey-links accounts from-date-t64 to-date-t64 flow-minimum))
    (js-data (links->js-array data-links))
    ;;(js-data "[]") ;; Placeholder until we can get the data extraction working

    ;; Create the report object that will hold the HTML content
    (report (gnc:make-html-document))
    (sankey (gnc:make-html-sankey)))

    ;; Now we construct the HTML report, embedding the JavaScript and data for the Sankey diagram.
    ;; The JS code will run in the context of the report's HTML document
    (gnc:html-document-set-title! report report-title)
    ;; (gnc:html-document-add-object! report (gnc:html-render-options-changed options))

    (gnc:html-sankey-set-from-date! sankey from-date-string)
    (gnc:html-sankey-set-to-date! sankey to-date-string)
    (gnc:html-sankey-set-income-color! sankey income-color)
    (gnc:html-sankey-set-expense-color! sankey expense-color)
    (gnc:html-sankey-set-asset-color! sankey asset-color)
    (gnc:html-sankey-set-liability-color! sankey liability-color)
    (gnc:html-sankey-set-equity-color! sankey equity-color)
    (gnc:html-sankey-set-fallback-color! sankey fallback-color)
    (gnc:html-sankey-set-js-data! sankey js-data)

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
