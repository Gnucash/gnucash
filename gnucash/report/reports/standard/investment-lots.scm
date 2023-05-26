;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; investment-lots.scm
;; by Brent McBride (mcbridebt@hotmail.com) Nov 2022
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


(define-module (gnucash reports standard investment-lots))

(use-modules (ice-9 format))
(use-modules (ice-9 match))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))           ;for gnc-prefs-is-extra-enabled
(use-modules (gnucash engine))
(use-modules (gnucash html))
(use-modules (gnucash report))
(use-modules (gnucash utilities))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))            ;for let-values
(use-modules (srfi srfi-13))            ;for string-trim

(define pagename-chart (N_ "Chart"))
(define pagename-columns (N_ "Columns"))
(define pagename-validation (N_ "Validation"))

;; Accounts
(define optname-accounts (N_ "Accounts"))
(define optname-zero-shares (N_ "Include accounts with no shares"))
(define optname-include-lotless-accounts (N_ "Include accounts with no lots"))

;; Chart
(define optname-show-chart (N_ "Show Chart"))
(define optname-chart-type (N_ "Chart type"))
(define optname-chart-location (N_ "Chart location"))
(define optname-plot-width (N_ "Plot width"))
(define optname-plot-height (N_ "Plot height"))

;; Columns
(define optname-show-lot-guid-column (N_ "Show lot GUID column"))
(define optname-show-date-columns (N_ "Show date columns"))
(define optname-show-bought-columns (N_ "Show bought columns"))
(define optname-show-sold-columns (N_ "Show sold columns"))
(define optname-show-end-columns (N_ "Show end columns"))
(define optname-show-realized-gain-columns
    (N_ "Show realized gain column(s)"))
(define optname-show-unrealized-gain-columns
    (N_ "Show unrealized gain column(s)"))
(define optname-group-gains-by-age
    (N_ "Group gains by age (short term and long term)"))
(define optname-long-term-years (N_ "Long term gains age (years)"))

;; Display
(define optname-show-long-account-names (N_ "Show long account names"))
(define optname-show-mnemonics (N_ "Show mnemonic in amounts"))
(define optname-include-closed-lots (N_ "Include closed lots"))
(define optname-show-blanks-for-zeros
    (N_ "Show blanks instead of zeros in table cells"))
(define optname-show-split-rows (N_ "Show lot split rows"))

;; General
(define reportname (N_ "Investment Lots"))
(define optname-from-date (N_ "Start date"))
(define optname-to-date (N_ "End date"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price source"))

;; Warnings
(define optname-include-only-accounts-with-warnings
    (N_ "Include only accounts with warnings"))
(define optname-warn-if-multiple-bought-splits
    (N_ "Warn if a lot has more than one bought split"))
(define optname-warn-if-balance-negative
    (N_ "Warn if a lot's balance drops below zero"))
(define optname-warn-if-lot-title-blank
    (N_ "Warn if a lot has a blank title"))
(define optname-warn-if-gains-mismatch
    (N_ "Warn if the 'Realized Gain/Loss' split(s) sum does not match the computed gains"))
(define optname-warn-type-if-split-not-in-lot
    (N_ "Warn if a split is not assigned to a lot"))
(define optname-warn-if-balance-mismatch
    (N_ "Warn if the account balance does not match the computed lots' end balance"))

(define colname-lot-title (N_ "Lot Title"))
(define colname-opened (N_ "Opened"))
(define colname-closed (N_ "Closed"))
(define colname-lot-guid (N_ "GUID"))
(define colname-bought-amount (N_ "Bought Amount"))
(define colname-bought-value (N_ "Bought Value (Basis)"))
(define colname-bought-price (N_ "Bought Average Price"))
(define colname-sold-splits (N_ "Sold Splits"))
(define colname-sold-amount (N_ "Sold Amount"))
(define colname-sold-basis (N_ "Sold Basis"))
(define colname-sold-value (N_ "Sold Value"))
(define colname-short-term-sold-amount (N_ "ST Sold Amount"))
(define colname-short-term-sold-basis (N_ "ST Sold Basis"))
(define colname-short-term-sold-value (N_ "ST Sold Value"))
(define colname-long-term-sold-amount (N_ "LT Sold Amount"))
(define colname-long-term-sold-basis (N_ "LT Sold Basis"))
(define colname-long-term-sold-value (N_ "LT Sold Value"))
(define colname-sold-price (N_ "Sold Average Price"))
(define colname-end-amount (N_ "End Amount"))
(define colname-end-basis (N_ "End Basis"))
(define colname-end-value (N_ "End Value"))
(define colname-realized-gain (N_ "Realized Gain"))
(define colname-short-term-realized-gain (N_ "ST Realized Gain"))
(define colname-long-term-realized-gain (N_ "LT Realized Gain"))
(define colname-realized-roi (N_ "Realized ROI"))
(define colname-unrealized-gain (N_ "Unrealized Gain"))
(define colname-short-term-unrealized-gain (N_ "ST Unrealized Gain"))
(define colname-long-term-unrealized-gain (N_ "LT Unrealized Gain"))
(define colname-unrealized-roi (N_ "Unrealized ROI"))

(define label-account-total (N_ "Account Lots Total"))
(define label-grand-total (N_ "Grand Total"))

;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for the report's parameters.
(define (options-generator)
  (let* ((options (gnc-new-optiondb)))

    ;; Accounts tab
    (gnc-register-account-list-limited-option options
      gnc:pagename-accounts
      optname-accounts
      "a"
      (N_ "Stock Accounts to report on.")
      ;; default-getter
      (filter gnc:account-is-stock?
              (gnc-account-get-descendants-sorted
               (gnc-get-current-root-account)))
    (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL))

    (gnc-register-simple-boolean-option options
        gnc:pagename-accounts
        optname-zero-shares
        "b"
        (N_ "Include accounts that have a zero share balances.")
        #t)

    (gnc-register-simple-boolean-option options
        gnc:pagename-accounts
        optname-include-lotless-accounts
        "c"
        (N_ "Include accounts with no lots")
        #f)

    ;; Chart tab
    (gnc-register-complex-boolean-option options
        pagename-chart
        optname-show-chart
        "a"
        (N_ "Include a chart that shows lot gains, grouped by account and gain type")
        #t (lambda (x)
             (for-each
              (lambda (name)
                (gnc-optiondb-set-option-selectable-by-name
                 options pagename-chart name x))
              (list optname-chart-type
                    optname-chart-location
                    optname-plot-width
                    optname-plot-height))))

    (gnc-register-multichoice-option options
        pagename-chart
        optname-chart-type
        "b"
        (N_ "What kind of chart to include")
        "bar-stacked"
        (list (vector 'bar (N_ "Bar Chart"))
              (vector 'bar-stacked (N_ "Stacked Bar Chart"))))

    (gnc-register-multichoice-option options
        pagename-chart
        optname-chart-location
        "c"
        (N_ "Where to place the chart")
        "top"
        (list (vector 'top (N_ "Top"))
              (vector 'bottom (N_ "Bottom"))))

    (gnc:options-add-plot-size!
      options
      pagename-chart
      optname-plot-width
      optname-plot-height
      "d"
      (cons 'percent 100.0)
      (cons 'percent 50.0))

    ;; Columns tab
    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-lot-guid-column
        "a"
        (N_ "Show the lot GUID table column")
        #f)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-date-columns
        "b"
        (N_ "Show the lot open and close table columns")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-bought-columns
        "c"
        (N_ "Show purchase-related table columns")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-sold-columns
        "d"
        (N_ "Show sale-related table columns")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-end-columns
        "e"
        (N_ "Show end date amount and value table columns")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-realized-gain-columns
        "f"
        (N_ "Show realized gain table column(s) for sold shares")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-columns
        optname-show-unrealized-gain-columns
        "g"
        (N_ "Show unrealized gain table column(s) for unsold shares")
        #t)

    (gnc-register-multichoice-callback-option options
        pagename-columns
        optname-group-gains-by-age
        "h"
        (N_ "Group gains (and sales?) by long-term (LT) and short-term (ST)")
        "gains-only"
        (list (vector 'no (N_ "No"))
              (vector 'gains-only (N_ "Gains Only"))
              (vector 'gains-and-sales (N_ "Gains and Sales")))
        (lambda (x)
          (gnc-optiondb-set-option-selectable-by-name
           options pagename-columns optname-long-term-years
           (not (eq? x 'no)))))

    ;; Note: Different governments may have different rules regarding how long
    ;; shares must be held to qualify for different tax treatment. So make
    ;; configurable the boundary between short-term and long-term capital
    ;; gains.
    (gnc-register-number-range-option options
      pagename-columns
      optname-long-term-years
      "i"
      (N_ "Commodities held longer than this many years count as long-term (LT).")
      1 ;; default-value. For USA federal taxes, shares held longer than 1
        ;; year are long-term.
      0 ;; lower-bound
      10E9 ;; upper-bound
      1) ;; step-size

    ;; Display tab
    (gnc-register-simple-boolean-option options
        gnc:pagename-display
        optname-show-long-account-names
        "a"
        (N_ "Show long (instead of short) account names")
        #t)

    (gnc-register-simple-boolean-option options
        gnc:pagename-display
        optname-show-mnemonics
        "b"
        (N_ "Show mnemonics with commodity amounts")
        #t)

    (gnc-register-simple-boolean-option options
        gnc:pagename-display
        optname-include-closed-lots
        "c"
        (N_ "Include closed lots in addition to open lots")
        #t)

    (gnc-register-simple-boolean-option options
        gnc:pagename-display
        optname-show-blanks-for-zeros
        "d"
        (N_ "Show blank text instead of zero values for inner table cells. Does not apply to footer rows.")
        #t)

    (gnc-register-simple-boolean-option options
        gnc:pagename-display
        optname-show-split-rows
        "e"
        (N_ "Add a row for each split belonging to a lot, under the lot row.")
        #f)

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-from-date optname-to-date "a")

    (gnc:options-add-currency!
      options
      gnc:pagename-general
      optname-report-currency
      "b")

    (gnc-register-multichoice-option options
      gnc:pagename-general
      optname-price-source
      "c" (N_ "The source of price information.") "pricedb-before"
      (list (vector 'pricedb-before (N_ "Last up through report date"))
            (vector 'pricedb-nearest (N_ "Closest to report date"))
            (vector 'pricedb-latest (N_ "Most recent"))))

    ;; Validation tab
    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-include-only-accounts-with-warnings
        "a"
        (N_ "Only show accounts that contain warnings. This is useful for quickly finding potential lot errors.")
        #f)

    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-warn-if-multiple-bought-splits
        "b"
        (N_ "Lots with more than one purchase split are not well formed. It may make ambiguous the capital gains age")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-warn-if-balance-negative
        "c"
        (N_ "Lots with a negative balance are not well formed.")
        #t)

    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-warn-if-lot-title-blank
        "d"
        (N_ "Lot titles are optional. This warning applies to titles that are empty or only whitespace.")
        #f) ;; Defaulting to false, since lot titles are not required.

    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-warn-if-gains-mismatch
        "e"
        (N_ "Detect possible errors in 'Realized Gain/Loss' splits that are created when adding a sale split to a lot")
        #t)

    (gnc-register-multichoice-option options
      pagename-validation
      optname-warn-type-if-split-not-in-lot
      "f" (N_ "Detect splits that have not been assigned to a lot.") "count"
      (list (vector 'no (N_ "No"))
            (vector 'count (N_ "Count"))
            (vector 'list (N_ "List"))))

    (gnc-register-simple-boolean-option options
        pagename-validation
        optname-warn-if-balance-mismatch
        "g"
        (N_ "Balance mismatches may indicate a split that is not yet included in a lot")
        #t)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (investment-lots-renderer report-obj)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value
      (gnc:lookup-option (gnc:report-options report-obj) section name)))

  ;; Given a price list and a currency find the price for that currency on the
  ;; list. If there is none for the requested currency, return the first one.
  (define (find-price price-list currency)
    (if (eqv? price-list '())
      #f
      (let loop ((price-list price-list)
                 (first-price (car price-list)))
        (match price-list
          (() first-price)
          ((price . rest)
            (cond
              ((gnc-commodity-equiv currency (gnc-price-get-currency price))
                  price)
              ((gnc-commodity-equiv currency (gnc-price-get-commodity price))
                  (gnc-price-invert price))
              (else
                  (loop rest first-price))))))))

  (let* (;; Accounts options
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (include-empty-accounts
            (get-option gnc:pagename-accounts optname-zero-shares))
         (include-lotless-accounts
            (get-option gnc:pagename-accounts
                optname-include-lotless-accounts))

         ;; Chart options
         (show-chart (get-option pagename-chart optname-show-chart))
         (chart-type (get-option pagename-chart optname-chart-type))
         (chart-location (get-option pagename-chart optname-chart-location))         
         (chart-height (get-option pagename-chart optname-plot-height))
         (chart-width (get-option pagename-chart optname-plot-width))

         ;; Column options
         (show-lot-guid-column
            (get-option pagename-columns optname-show-lot-guid-column))
         (show-date-columns
            (get-option pagename-columns optname-show-date-columns))
         (show-bought-columns
            (get-option pagename-columns optname-show-bought-columns))
         (show-sold-columns
            (get-option pagename-columns optname-show-sold-columns))
         (show-end-columns
            (get-option pagename-columns optname-show-end-columns))
         (show-realized-gain-columns
            (get-option pagename-columns optname-show-realized-gain-columns))
         (show-unrealized-gain-columns
            (get-option pagename-columns
                optname-show-unrealized-gain-columns))
         (group-gains-and-sales-by-age
            (get-option pagename-columns optname-group-gains-by-age))
         (long-term-years
            (get-option pagename-columns optname-long-term-years))

         ;; Display options
         (include-closed-lots
            (get-option gnc:pagename-display optname-include-closed-lots))
         (show-long-account-names
            (get-option gnc:pagename-display optname-show-long-account-names))
         (show-mnemonics
            (get-option gnc:pagename-display optname-show-mnemonics))
         (show-blanks-for-zeros
            (get-option gnc:pagename-display optname-show-blanks-for-zeros))
         (show-split-rows
            (get-option gnc:pagename-display optname-show-split-rows))

         ;; General options
         (from-date (gnc:time64-end-day-time
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-from-date))))
         (to-date (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general optname-to-date))))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                    optname-price-source))

         ;; Validation options
         (include-only-accounts-with-warnings
            (get-option pagename-validation
                optname-include-only-accounts-with-warnings))
         (warn-if-multiple-bought-splits
            (get-option pagename-validation
                optname-warn-if-multiple-bought-splits))
         (warn-if-balance-negative
            (get-option pagename-validation optname-warn-if-balance-negative))
         (warn-if-lot-title-blank
            (get-option pagename-validation optname-warn-if-lot-title-blank))
         (warn-if-gains-mismatch
            (get-option pagename-validation optname-warn-if-gains-mismatch))
         (warn-type-if-split-not-in-lot
            (get-option pagename-validation
                optname-warn-type-if-split-not-in-lot))
         (warn-if-balance-mismatch
            (get-option pagename-validation optname-warn-if-balance-mismatch))

         (warn-if-split-not-in-lot
            (not (eq? warn-type-if-split-not-in-lot 'no)))
         (group-gains-by-age
            (not (eq? group-gains-and-sales-by-age 'no)))
         (group-sales-by-age
            (eq? group-gains-and-sales-by-age 'gains-and-sales))
         (report-currency-fraction
            (gnc-commodity-get-fraction report-currency))

         (price-db (gnc-pricedb-get-db (gnc-get-current-book)))
         (price-fn
            (case price-source
              ((pricedb-latest)
                (lambda (commodity)
                  (find-price
                      (gnc-pricedb-lookup-latest-any-currency
                          price-db commodity)
                      report-currency)))
              ((pricedb-nearest)
                (lambda (commodity)
                  (find-price
                      (gnc-pricedb-lookup-nearest-in-time-any-currency-t64
                          price-db commodity
                          (time64CanonicalDayTime to-date))
                      report-currency)))
              ((pricedb-before)
                (lambda (commodity)
                  (find-price
                      (gnc-pricedb-lookup-nearest-before-any-currency-t64
                            price-db commodity
                            (time64CanonicalDayTime to-date))
                      report-currency)))))
         (exchange-fn (gnc:case-exchange-fn
                        price-source
                        report-currency
                        to-date))

         (get-report-value-zero (lambda ()
            (gnc-numeric-create 0 report-currency-fraction)))

         ;; Note: To the user, the report appears to contain a vertical list
         ;; of tables, one per investment account. But these account tables
         ;; are actually implemented as a single html table, with empty spacer
         ;; rows inserted between. Using a single table keeps the columns
         ;; aligned, which makes the report easier to read.
         (table (gnc:make-html-table))

         (chart (if show-chart
                  (gnc:make-html-chart)))
         (colors (gnc:assign-colors (length accounts)))

         (document (gnc:make-html-document)))

    ;; Returns whether a commodity purchased on bought-date and sold on
    ;; sold-date qualifies for long-term capital gains treatment. The boundary
    ;; between short and long term is configurable, but otherwise this logic
    ;; is USA federal government-specific, as per
    ;; https://www.irs.gov/publications/p550#en_US_2021_publink100010540:
    ;;
    ;; "If you hold investment property more than 1 year, any capital gain or
    ;;  loss is a long-term capital gain or loss. If you hold the property 1
    ;;  year or less, any capital gain or loss is a short-term capital gain or
    ;;  loss. To determine how long you held the investment property, begin
    ;;  counting on the date after the day you acquired the property. The day
    ;;  you disposed of the property is part of your holding period."
    (define (long-term? bought-date sold-date)
      (if (and bought-date
               (not (null? bought-date))
               sold-date
               (not (null? sold-date)))
        ;; Note: gnc:date-year-delta handles the complexity of dealing with
        ;; leap years.
        (let ((years-held (gnc:date-year-delta bought-date sold-date)))
          (> years-held long-term-years))
        #f))

    ;; Gets the account name.
    (define (account->name account)
      (if show-long-account-names
        (gnc-account-get-full-name account)
        (xaccAccountGetName account)))

    ;; Gets anchor linked to the account, with the account name as anchor
    ;; text.
    (define (to-account-anchor account)
      (gnc:html-markup-anchor
        (gnc:account-anchor-text account)
        (account->name account)))

    ;; Gets a formatted display string for the given currency and amount, e.g.
    ;; "$(1,000.21)"
    (define (amount->monetary-string currency amount)
      (xaccPrintAmount
        amount
        (gnc-commodity-print-info currency show-mnemonics)))

    ;; Gets the lot title.
    (define (lot->title lot)
      (gnc-lot-get-title lot))

    ;; Gets the lot guid.
    (define (lot->guid lot)
      (if lot
        (gncLotReturnGUID lot)
        #f))

    ;; Gets the split's transaction date.
    (define (split->date split)
      (xaccTransGetDate (xaccSplitGetParent split)))

    ;; Gets an html table cell containing the value, formatted as a number
    ;; (i.e. right justified, etc.)
    ;; is-total determines whether total cell styling (i.e. bold) is used.
    (define (to-number-cell value is-total)
      (gnc:make-html-table-cell/markup
        (if is-total "total-number-cell" "number-cell")
        (cond
          ((integer? value)
            (format #f "~d" value)) ;; convert to string to not show decimals.
          (else value))))

    ;; Gets an html table cell containing the value, formatted as a column
    ;; header.
    (define (to-header-cell value)
      (gnc:make-html-table-cell/markup "column-heading-center" value))

    ;; Gets an html table cell containing an anchor with the specified text
    ;; and that links to the specified split.
    (define (to-split-cell text split)
      (if text
        (to-number-cell
          (if split
            (gnc:html-split-anchor split text)
            text)
          #f) ;; is-total
        #f))

    ;; Returns the given value in the given currency, converted to the
    ;; report's currency.
    (define (value->report-currency-value value currency)
      (gnc:gnc-monetary-amount
        (exchange-fn
          (gnc:make-gnc-monetary
            currency
            value) ;; foreign
          report-currency))) ;; domestic

    ;; Gets a gnc-monetary for the given value and the report currency.
    (define (value->monetary value)
      (gnc:make-gnc-monetary report-currency value))

    ;; Gets the display string for value, formatted as the report's currency,
    ;; e.g. "$(1,000.21)".
    (define (value->monetary-string value)
      (xaccPrintAmount
        value
        (gnc-commodity-print-info report-currency #t))) ;; show-mnemonics

    ;; Gets all splits for the given account, bounded by to-date. Splits
    ;; before from-date are also included (needed to calculate running
    ;; balance and basis during the report date window).
    (define (get-all-splits account)
      (let ((query (qof-query-create-for-splits)))
        (qof-query-set-book query (gnc-get-current-book))
        (xaccQueryAddClearedMatch query
          (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)
        (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
        (xaccQueryAddDateMatchTT query
            #f ; use_start.
            0  ; start. Note: Intentionally not using from-date.
            #t ; use-end
            to-date QOF-QUERY-AND)
        (let ((result (qof-query-run query)))
          (qof-query-destroy query)
          (gnc:debug (format #f "Found ~a splits." (length result)))
          result)))

    ;; Returns a pair where the first item is a list of lots for the given
    ;; splits. The second item is the number of splits that are not assigned
    ;; to a lot.
    (define (get-all-lots splits)
      (define lots-seen (make-hash-table))
      (let loop ((splits splits)
                 (lots '())
                 (unassigned-splits '()))
        (match splits
          (()
            (gnc:debug (format #f "Found ~a lots and ~a unassigned splits"
                                  (length lots)
                                  (length unassigned-splits)))
            (list (reverse lots) unassigned-splits))
          ((split . rest)
            (let ((lot (xaccSplitGetLot split)))
              (loop rest
                  (cond
                    ((or (null? lot)
                         (hash-ref lots-seen lot))
                      lots)
                    (else
                      (hash-set! lots-seen lot #t)
                      (cons lot lots)))
                  (cond
                    ((null? lot)
                      (cons split unassigned-splits))
                    (else unassigned-splits))))))))

    ;; Returns the lot splits, ordered first by transaction date and then
    ;; ordering purchases before sales.
    (define (lot->splits lot)
      (sort-list!
        ;; Prune out splits that are after to-date.
        (let loop ((splits (gnc-lot-get-split-list lot))
                   (result '()))
          (match splits
            (() result)
            ((split . rest)
              (loop rest
                (if (<= (split->date split) to-date)
                  (cons split result)
                  result)))))
        (lambda (s1 s2)
          (let* ((t1 (xaccSplitGetParent s1))
                  (t2 (xaccSplitGetParent s2))
                  (date1 (xaccTransGetDate t1))
                  (date2 (xaccTransGetDate t2))
                  ;; Do not call xaccTransOrder to set t-order. It not only
                  ;; sorts by date posted, but by other fields that we don't
                  ;; care about here (i.e. num, date entered, description, and
                  ;; guid). When two transactions have the same date, we want
                  ;; t-order to be zero, regardless of those other fields, so
                  ;; that the secondary sorting logic (purchase or sale) takes
                  ;; effect.
                  ;;   (t-order (xaccTransOrder t1 t2))
                  (t-order (cond
                              ((< date1 date2) -1)
                              ((> date1 date2) 1)
                              (else 0))))
            (if (= t-order 0)
              ;; The two splits share the same transaction date. Order
              ;; purchases before sales.
              (let ((is-purchase-s1
                        (gnc-numeric-positive-p (xaccSplitGetAmount s1)))
                    (is-purchase-s2
                        (gnc-numeric-positive-p (xaccSplitGetAmount s2))))
                (cond
                  ((and is-purchase-s1 is-purchase-s2)
                    ;; They are both purchases and on the same date. So go
                    ;; ahead and let xaccTransOrder be the tiebreaker (not
                    ;; that it matters much).
                    (<= (xaccTransOrder t1 t2) 0))
                  (else is-purchase-s1)))
              (<= t-order 0))))))

      ;; Gets the price's time.
      (define (price->time price)
        (gnc-price-get-time64 price))

      ;; Gets the price's value.
      (define (price->value price)
        (gnc-price-get-value price))

      ;; Gets the price's currency.
      (define (price->currency price)
        (gnc-price-get-currency price))

      ;; Gets the price's guid.
      (define (price->guid price)
        (gncPriceGetGUID price))

      ;; Gets a gnc-monetary with the price's currency and value.
      (define (price->monetary price)
        (gnc:make-gnc-monetary
          (price->currency price)
          (price->value price)))

      ;; Returns the given price's value, converted to the
      ;; report's currency, if different.
      (define (price->report-currency-value price)
        (value->report-currency-value
          (price->value price)
          (price->currency price)))

      ;; Returns the given price as a formatted string, in the report's
      ;; currency.
      (define (price->report-currency-monetary-string price)
        (value->monetary-string (price->report-currency-value price)))

      ;; Returns the given price as an anchor whose text is the formatted
      ;; value and that links to the price editor.
      (define (to-price-anchor price)
        (if (and (not (null? price))
                  price
                 (price->guid price))
          (gnc:html-markup/format
            (N_ "  End price: ~a~a on ~a")
            (gnc:html-markup-anchor
              (gnc-build-url URL-TYPE-PRICE
                (string-append "price-guid=" (price->guid price))
                "")
              (price->monetary price))
            (if (not (gnc-commodity-equiv report-currency
                          (price->currency price)))
              ;; The price is not already in the report currency, so also
              ;; display the price converted to the report currency.
              (format #f " [~a]"
                (price->report-currency-monetary-string price))
              "")
            (qof-print-date (price->time price)))
          (N_ "No price found")))

    ;; Gets the average price (i.e. value/amount). Returns 0 if the equation
    ;; is undefined (i.e. protects against divide by zero errors.) Returns
    ;; #f if either amount or value are #f.
    (define (get-average-price amount value)
      (if (and
            amount
            value)
          (if (gnc-numeric-zero-p amount)
            (get-report-value-zero)
            (gnc-numeric-div value amount GNC-DENOM-AUTO GNC-DENOM-REDUCE))
        #f))

    ;; Gets a list of visible table column headers. Note that report options
    ;; control which columns to show. Also, some column headers will be blank
    ;; for the grand total header (such as amount columns, since multiple
    ;; accounts may have different commodities, so combining their amounts
    ;; would not make sense).
    (define (get-column-header-list is-grand-total)
      (append
        (list (if is-grand-total #f colname-lot-title))
        (if show-lot-guid-column
          (list (if is-grand-total #f colname-lot-guid))
          '())
        (if show-date-columns
          (list
            (if is-grand-total #f colname-opened)
            (if is-grand-total #f colname-closed))
          '())
        (if show-bought-columns
          (list
            (if is-grand-total #f colname-bought-amount)
            colname-bought-value
            (if is-grand-total #f colname-bought-price))
          '())
        (if show-sold-columns
          (append
            (list colname-sold-splits)
            (if group-sales-by-age
              (list
                (if is-grand-total #f colname-short-term-sold-amount)
                colname-short-term-sold-basis
                colname-short-term-sold-value
                (if is-grand-total #f colname-long-term-sold-amount)
                colname-long-term-sold-basis
                colname-long-term-sold-value)
              (list
                (if is-grand-total #f colname-sold-amount)
                colname-sold-basis
                colname-sold-value))
            (list (if is-grand-total #f colname-sold-price))
          )
          '())
        (if show-end-columns
          (list
            (if is-grand-total #f colname-end-amount)
            colname-end-basis
            colname-end-value)
          '())
        (if show-realized-gain-columns
          (if group-gains-by-age
            (list
              colname-short-term-realized-gain
              colname-long-term-realized-gain
              colname-realized-roi)
            (list
              colname-realized-gain
              colname-realized-roi))
          '())
        (if show-unrealized-gain-columns
          (if group-gains-by-age
            (list
              colname-short-term-unrealized-gain
              colname-long-term-unrealized-gain
              colname-unrealized-roi)
            (list
              colname-unrealized-gain
              colname-unrealized-roi))
          '())))

    ;; The number of table columns.
    (define column-count (length (get-column-header-list #f))) ;is-grand-total

    ;; Gets the row style for even/odd rows.
    (define (get-row-style is-odd-row)
      (if is-odd-row "normal-row" "alternate-row"))

    ;; Adds a header row to table.
    (define (add-header-row table is-grand-total)
      (gnc:html-table-append-row/markup!
        table
        "normal-row"
        (map to-header-cell
          (get-column-header-list is-grand-total))))

    ;; Adds a warning row to table.
    (define (add-warning-row table warning)
      (let ((cell
              (gnc:make-html-table-cell/size
                1 ;; rowspan
                column-count ;; colspan
                ;; If the warning is a string then convert it to html text.
                ;; Otherwise, use it as-is.
                (if (string? warning)
                  (gnc:make-html-text warning)
                  warning)))
            ;; If the warning is not plain text, indent it.
            (indent? (not (string? warning))))
        (gnc:html-table-cell-set-style!
          cell "td"
          'attribute
          (list "class" (string-append
                          "total-label-cell neg" ;; bold, red, left justified
                          (if indent? " indented" ""))))
        (gnc:html-table-append-row! table (list cell))))

    ;; Copies the rows (with their styles) from one table to another
    ;; table. If row-style is provided, it is used instead of copying the
    ;; source row's style.
    (define (copy-table-rows from-table to-table row-style)
      (let loop ((row-num 0)
                 (rows (reverse (gnc:html-table-data from-table))))
        (match rows
          (() #f)
          ((row . rest)
            (gnc:html-table-append-row/markup! to-table
              (or row-style
                  (gnc:html-table-row-markup from-table row-num)) row)
            (loop (+ row-num 1) rest)))))

    ;; Adds a data row to table.
    (define (add-data-row
              table
              amount-currency
              is-bold
              is-odd-row
              first-text
              lot
              open-date-cell
              close-date-cell
              bought-amount
              bought-value
              sold-split-count
              short-term-sold-amount
              short-term-sold-basis
              short-term-sold-value
              long-term-sold-amount
              long-term-sold-basis
              long-term-sold-value
              end-amount
              end-basis
              end-value
              short-term-realized-gain
              long-term-realized-gain
              short-term-unrealized-gain
              long-term-unrealized-gain)
      ;; Helper function for converting a numeric value to an html table cell.
      (define (to-cell val format-val-fn)
        (if (or (not val)
            (and (not is-bold) ;; total rows are bold. Don't replace zeros.
                  show-blanks-for-zeros
                  (= val 0)))
          #f ;; show a blank cell
          (to-number-cell
            (format-val-fn)
            is-bold)))

      ;; Converts a value (denominated in the account's commodity) to an html
      ;; table cell.
      (define (amount->cell amount)
        (to-cell
          amount
          (lambda ()
            (amount->monetary-string amount-currency amount))))

      ;; Converts an integer to an html table cell.
      (define (integer->cell number)
        (to-cell
          number
          (lambda () number)))

      (define (percentage->cell number)
        ;; This formats negative percentages similar to numbers: red with
        ;; parentheses. But other reports don't do that? Better to be
        ;; consistent. Plus the below logic is kludgy.
        ;; (let* ((neg? (< number 0))
        ;;        (text (if neg?
        ;;           (format #f "(~,1f%)" (- 0 number))
        ;;           (format #f "~,1f%" number)))
        ;;        (style (if is-bold "total-number-cell" "number-cell")))
        ;;   (if neg?
        ;;     (set! style (string-append style "-neg")))
        ;;   (if (and (not is-bold) ;; Don't replace zeros for total rows.
        ;;             show-blanks-for-zeros
        ;;             (= val 0))
        ;;     #f ;; show a blank cell
        ;;     (gnc:make-html-table-cell/markup style text))))
        (to-cell
          number
          (lambda ()
              (format #f "~,1f%" number))))

      ;; Converts a value (denominated in the report's currency) to an html
      ;; table cell.
      (define (value->cell value)
        (to-cell
          value
          (lambda ()
            (value->monetary value))))

      ;; Helper function for adding capital gains columns
      (define (get-gains-fn show-columns basis short-gain long-gain)
        (append
          (if show-columns
            (let* ((total-gain (gnc-numeric-add-fixed
                                    short-gain
                                    long-gain))
                    (roi (percentage->cell
                            (cond
                              ((or (not basis)
                                   (not total-gain))
                                 #f)
                              ((gnc-numeric-zero-p basis)
                                0)
                              (else
                                (* 100 (/ total-gain basis)))))))
              (if group-gains-by-age
                (list
                  (value->cell short-gain)
                  (value->cell long-gain)
                  roi)
                (list
                  (value->cell total-gain)
                  roi)))
            '())))

      (if is-bold
        (gnc:html-table-append-ruler!
          table
          column-count)) ;; colspan

      (let* ((sold-basis (gnc-numeric-add-fixed
                            short-term-sold-basis
                            long-term-sold-basis))
             (cells
        (append
          (list
            (if is-bold
              (gnc:make-html-table-cell/markup
                  "total-number-cell" first-text)
              first-text))
          (if show-lot-guid-column
            (list (lot->guid lot))
            '())
          (if show-date-columns
            (list
              open-date-cell
              close-date-cell)
            '())
          (if show-bought-columns
            (list
              (amount->cell bought-amount)
              (value->cell bought-value)
              (value->cell (get-average-price bought-amount bought-value)))
            '())
          (if show-sold-columns
            (let ((sold-amount
                    (gnc-numeric-add-fixed
                      short-term-sold-amount
                      long-term-sold-amount))
                  (sold-value
                    (gnc-numeric-add-fixed
                      short-term-sold-value
                      long-term-sold-value)))
              (append
                (list (integer->cell sold-split-count))
                (if group-sales-by-age
                  (list
                    (amount->cell short-term-sold-amount)
                    (value->cell short-term-sold-basis)
                    (value->cell short-term-sold-value)
                    (amount->cell long-term-sold-amount)
                    (value->cell long-term-sold-basis)
                    (value->cell long-term-sold-value)
                  )
                  (list
                    (amount->cell sold-amount)
                    (value->cell sold-basis)
                    (value->cell sold-value)))
                (list (value->cell
                        (get-average-price sold-amount sold-value)))))
            '())
          (if show-end-columns
            (list
              (amount->cell end-amount)
              (value->cell end-basis)
              (value->cell end-value))
            '())
          (get-gains-fn
            show-realized-gain-columns
            sold-basis
            short-term-realized-gain
            long-term-realized-gain)
          (get-gains-fn
            show-unrealized-gain-columns
            end-basis
            short-term-unrealized-gain
            long-term-unrealized-gain))))
      (gnc:html-table-append-row/markup!
        table
        (if is-bold "grand-total" (get-row-style is-odd-row))
        cells)))

    ;; Adds the specified number of blank padding rows to the table.
    (define (add-padding-rows row-count)
      (if (> row-count 0)
        (begin
          (gnc:html-table-append-row/markup!
            table
            "normal-row"
            (list
              (gnc:make-html-table-cell/size
                1 ;; rowspan
                column-count ;; colspan
                (gnc:make-html-text (gnc:html-markup-p)))))
          (add-padding-rows (- row-count 1)))))

    ;; Structure that holds stats about one or more lots. Each instance may
    ;; represent one of the following:
    ;;   - One lot
    ;;   - All lots within an account (account total)
    ;;   - All lots within all account in the report (grand total)
    ;; Multiple lot collectors may be merged into a single account collector.
    ;; Multiple account collectors may be merged into a single grand total
    ;; collector.
    (define (create-lot-stats-collector currency-fraction)
      (let* ((get-amount-zero (lambda ()
                (gnc-numeric-create 0 currency-fraction)))
             (bought-split-count 0)
             (old-bought-amount (get-amount-zero))
             (old-bought-value (get-report-value-zero))
             (bought-amount (get-amount-zero))
             (bought-value (get-report-value-zero))
             (sold-split-count 0)
             (short-term-sold-amount (get-amount-zero))
             (short-term-sold-basis (get-report-value-zero))
             (short-term-sold-value (get-report-value-zero))
             (short-term-realized-gain (get-report-value-zero))
             (long-term-sold-amount (get-amount-zero))
             (long-term-sold-basis (get-report-value-zero))
             (long-term-sold-value (get-report-value-zero))
             (long-term-realized-gain (get-report-value-zero))
             (end-amount (get-amount-zero))
             (end-basis (get-report-value-zero))
             (end-value (get-report-value-zero))
             (unrealized-gain (get-report-value-zero))
             (short-term-unrealized-gain (get-report-value-zero))
             (long-term-unrealized-gain (get-report-value-zero))
             (has-warnings #f)
             (is-active-in-window #f)
             (currency '())

             ;; private:
             (splits-realized-gain (get-report-value-zero))
             (splits-table (gnc:make-html-table))
             (first-negative-split '())
             (earliest-bought-split '())
             (earliest-bought-split-date '())
             (latest-bought-split-date '())
             (last-sold-split '())

             ;; For lot collectors only:
             (lot '())

             ;; For account collectors only:
             (account '())
             (unassigned-splits '()))

        ;; Returns whether this instance pertains to a single lot.
        (define (get-is-lot-stats)
          (not (null? lot)))

        ;; Returns whether this instance pertains to all lots within a
        ;; single account.
        (define (get-is-account-stats)
          (and (not (get-is-lot-stats))
               (not (null? account))))

        ;; Returns whether this instance pertains to all lots within all
        ;; accounts (i.e. the grand total).
        (define (get-is-grand-total-stats)
          (not (or (get-is-lot-stats)
                   (get-is-account-stats))))

        ;; Initializes the instance with an account's context.
        (define (init-for-account accnt unassigned-splts)
          (set! account accnt)
          (set! unassigned-splits unassigned-splts)
          (set! is-active-in-window (not (null? unassigned-splits))))

        ;; Adds to the given table a row for the given split.
        (define (add-split-row
                  split
                  trans-date
                  bought-amount
                  bought-value
                  sold-amount
                  sold-basis
                  sold-value
                  sold-gain
                  is-long-term)
          (if show-split-rows
            (let ((date-cell
                    (to-split-cell (qof-print-date trans-date) split))
                  (title-cell
                    (gnc:make-html-table-cell
                      (gnc:make-html-text (N_ "split")))))
              ;; Indent the split title text.
              (gnc:html-table-cell-set-style!
                title-cell "td"
                'attribute
                (list "class" "indented"))


              (add-data-row
                splits-table
                currency
                #f ;; is-bold
                #t ;; is-odd-row
                title-cell
                #f ;; lot
                (if bought-amount date-cell #f) ;; open-date
                (if sold-amount date-cell #f)  ;; close-date
                bought-amount
                bought-value
                #f ;; sold-split-count
                (if (and is-long-term sold-amount) 0 sold-amount)
                (if (and is-long-term sold-basis) 0 sold-basis)
                (if (and is-long-term sold-value) 0 sold-value)
                (if (and is-long-term sold-amount) sold-amount 0)
                (if (and is-long-term sold-basis) sold-basis 0)
                (if (and is-long-term sold-value) sold-value 0)
                end-amount
                end-basis
                #f ;; end-value
                ;; short-term-realized-gain
                (if (and is-long-term sold-gain) 0 sold-gain)
                ;; long-term-realized-gain
                (if (and is-long-term sold-gain) sold-gain 0)
                #f ;; short-term-unrealized-gain
                #f)))) ;; long-term-unrealized-gain

        ;; Adds the stats to the given html table.
        (define (add-to-table table is-odd-row)
          (let* ((is-lot-row (get-is-lot-stats))
                 (is-account-row (get-is-account-stats))
                 (is-grand-total-row (get-is-grand-total-stats))
                 (open-date-cell
                    (cond ((and is-lot-row
                                (not (null? earliest-bought-split-date)))
                            (to-split-cell
                              (qof-print-date earliest-bought-split-date)
                              earliest-bought-split))
                          (else #f)))
                 (close-date-cell
                    (cond ((and is-lot-row
                                (gnc-numeric-zero-p end-amount)
                                (not (null? last-sold-split)))
                            (to-split-cell
                              (qof-print-date (split->date last-sold-split))
                              last-sold-split))
                          (else #f)))
                  (first-text
                    (cond
                      (is-lot-row (lot->title lot))
                      (is-account-row label-account-total)
                      (is-grand-total-row label-grand-total))))
            (add-data-row
              table
              (if (not is-grand-total-row) currency #f)
              (not is-lot-row) ;; is-bold
              is-odd-row
              first-text
              (if is-lot-row lot #f)
              open-date-cell
              close-date-cell
              (if (not is-grand-total-row) bought-amount #f)
              bought-value
              sold-split-count
              (if (not is-grand-total-row) short-term-sold-amount #f)
              short-term-sold-basis
              short-term-sold-value
              (if (not is-grand-total-row) long-term-sold-amount #f)
              long-term-sold-basis
              long-term-sold-value
              (if (not is-grand-total-row) end-amount #f)
              end-basis
              end-value
              short-term-realized-gain
              long-term-realized-gain
              short-term-unrealized-gain
              long-term-unrealized-gain)

            (if is-lot-row
              (copy-table-rows splits-table table (get-row-style is-odd-row)))

            (add-warnings-to-table table)

            (not is-odd-row)))

        ;; Checks for warnings and, if found, adds them to the given table
        ;; and sets the has-warnings flag.
        (define (add-warnings-to-table table)
          (let ((warnings
                  (cond
                    ((get-is-lot-stats) (get-lot-warnings))
                    ((get-is-account-stats) (get-account-warnings))
                    (else '()))))
            (if (not (null? warnings))
              (begin
                (gnc:debug (format #f "Found ~d warning(s)."
                              (length warnings)))
                (set! has-warnings #t)
                (for-each
                  (lambda (warning) (add-warning-row table warning))
                  warnings)))))

        ;; Gets lot validation warnings.
        (define (get-lot-warnings)
          (append
            ;; Multiple bought splits in the same lot may make
            ;; ambiguous the lot's age, for distinguishing between
            ;; long-term and short-term capital gains.
            (if (and
                  warn-if-multiple-bought-splits
                  (> bought-split-count 1))
              (list (format #f
                  (G_ "Warning: Above lot has ~a bought splits. Consider separating them into their own lots.")
                  bought-split-count))
              '())

            ;; Warn for negative balances.
            (if (and
                  warn-if-balance-negative
                  (not (null? first-negative-split)))
              (list (format #f
                      (G_ "Warning: Above lot's balance is negative on ~a. Consider removing the responsible sale split from the lot and then scrubbing.")
                      (qof-print-date
                        (split->date first-negative-split))))
              '())

            ;; Warn for blank (empty or only whitespace) lot titles.
            (if (and
                  warn-if-lot-title-blank
                  (string-null? (string-trim (lot->title lot))))
              (list (G_ "Warning: Above lot's title is blank."))
              '())

            ;; Warn if the report-computed gains do not match the
            ;; "Realized Gain/Loss" split.
            (if warn-if-gains-mismatch
              (let* ((all-terms-realized-gain (gnc-numeric-add-fixed
                                                short-term-realized-gain
                                                long-term-realized-gain))
                    (gain-discrepancy (gnc-numeric-sub-fixed
                                        all-terms-realized-gain
                                        splits-realized-gain)))
                (if (not (gnc-numeric-zero-p gain-discrepancy))
                  (list
                    (format #f
                      (G_ "Warning: Above lot's computed gain ~a is not equal to the \"Realized Gain/Loss\" split(s) sum ~a. Difference: ~a")
                      (value->monetary-string all-terms-realized-gain)
                      (value->monetary-string splits-realized-gain)
                      (value->monetary-string gain-discrepancy)))
                  '()))
              '())))

        ;; Gets account validation warnings.
        (define (get-account-warnings)
          (let ((account-end-balance
                  (xaccAccountGetBalanceAsOfDate account to-date))
                (unassigned-split-count (length unassigned-splits)))
            (append
              ;; Warn for splits that are not assigned to a lot.
              (if (and warn-if-split-not-in-lot
                      (> unassigned-split-count 0))
                (case warn-type-if-split-not-in-lot
                  ((count)
                    ;; Show only the number of unassigned splits.
                    (list (format #f (G_ "Warning: ~a split(s) are not assigned to a lot. Do lots need to be scrubbed?")
                            unassigned-split-count)))

                  ((list)
                   (cons
                    (format #f (G_ "Warning: The following ~a split(s) are not assigned to a lot. Do lots need to be scrubbed?") unassigned-split-count)

                      ;; Also list out the unassigned splits.
                      (map (lambda (split)
                            (let* ((trans (xaccSplitGetParent split))
                                   (date (xaccTransGetDate trans))
                                   ; Convert split value to the report's
                                   ; currency.
                                   (value
                                      (value->report-currency-value
                                        (xaccSplitGetValue split)
                                        (xaccTransGetCurrency trans)))
                                   (amount (xaccSplitGetAmount split)))
                              (gnc:make-html-span
                                (gnc:html-split-anchor
                                  split
                                  (qof-print-date date))
                                (format #f (G_ ": amount ~a, value ~a")
                                  (amount->monetary-string currency amount)
                                  (value->monetary-string value)))))
                            unassigned-splits)))
                  (else
                    (gnc:error (format #f
                                "Bad warn-type-if-split-not-in-lot value: ~a"
                                warn-type-if-split-not-in-lot))
                    '()))
                '())

            ;; Warn if the report's computed gain does not match that in the
            ;; "Realized Gain/Loss" split.
            (if warn-if-balance-mismatch
              (let ((amount-discrepancy (gnc-numeric-sub-fixed
                                            end-amount
                                            account-end-balance)))
                (if (not (gnc-numeric-zero-p amount-discrepancy))
                  (list
                    (format #f
                      (G_ "Warning: End amount ~a is not equal to actual account balance ~a. Difference: ~a. Do lots need to be scrubbed?")
                      (amount->monetary-string currency end-amount)
                      (amount->monetary-string currency account-end-balance)
                      (amount->monetary-string currency amount-discrepancy)))
                  '()))
              '()))))

        ;; Merges in the lot.
        (define (merge-lot l curr price)
          (set! lot l)
          (set! currency curr)
          (gnc:debug (format #f "Merging lot '~a'" (lot->title lot)))

          ;; Merge in each of the lot's splits.
          (for-each
            (lambda (split)
              (let* ((trans (xaccSplitGetParent split))
                     (trans-date (xaccTransGetDate trans)))
                (merge-split
                    split
                    trans-date
                    (xaccTransGetCurrency trans))))
            (lot->splits lot))

          (set! end-value
            (if price
              (gnc-numeric-mul
                  end-amount
                  ;; Ensure the price is in the report's
                  ;; currency.
                  (price->report-currency-value price)
                  report-currency-fraction
                  GNC-RND-ROUND)
              (get-report-value-zero)))
          (set! unrealized-gain (gnc-numeric-sub-fixed
                                  end-value
                                  end-basis))

          ;; Whether the lot shares have been held long enough (as of
          ;; the report end date) to qualify as long term.
          (let ((is-lot-long-term?
                  (long-term? latest-bought-split-date to-date)))
            (set! short-term-unrealized-gain
              (if is-lot-long-term?
                (get-report-value-zero) ;; zero if long term
                unrealized-gain))
            (set! long-term-unrealized-gain
              (if is-lot-long-term?
                unrealized-gain
                (get-report-value-zero)))) ;; zero if short term

          ;; A lot is active in the report's date window if the window
          ;; contains any lot sold splits, or if the lot has any shares on the
          ;; end date. Inactive lots are not included in the report.
          (set! is-active-in-window
            (or (> sold-split-count 0)
                (gnc-numeric-positive-p end-amount)
                has-warnings))

          (gnc:debug (format #f "  Lot '~a' is-active-in-window=~a"
                             (lot->title lot)
                             is-active-in-window)))

        ;; Merges in the given split.
        (define (merge-split split trans-date trans-currency)
          (let* (; Convert split value to the report's currency.
                 (value
                    (value->report-currency-value
                      (xaccSplitGetValue split)
                      trans-currency))
                 (amount (xaccSplitGetAmount split))
                 (is-purchase (gnc-numeric-positive-p amount))
                 (is-sale (gnc-numeric-negative-p amount))
                 (is-realized-gain (gnc-numeric-zero-p amount)))

            (cond
              (is-purchase
                (merge-purchase-split split trans-date amount value))

              (is-sale
                (merge-sale-split
                  split
                  trans-date
                  ;; Covert amount and value to positive numbers.
                  (gnc-numeric-neg amount)
                  (gnc-numeric-neg value)))

              ;; A "Realized Gain/Loss" split has zero amount. Sum its value
              ;; to validate against the report-computed gain value.
              ((and is-realized-gain
                    (>= trans-date from-date))
                (set! splits-realized-gain
                    (gnc-numeric-add-fixed splits-realized-gain value))))))

        ;; Merges in the sale split info.
        (define (merge-sale-split split trans-date amount value)
          (let* ((sold-frac (gnc-numeric-div
                              amount
                              end-amount
                              GNC-DENOM-AUTO
                              GNC-DENOM-REDUCE))
                 (basis (gnc-numeric-mul
                          sold-frac
                          end-basis
                          report-currency-fraction
                          GNC-RND-ROUND))
                 ;; Note: The gains are computed (with rounding happening) per
                 ;; sale split, rather than per lot. This may compound
                 ;; rounding errors at the lot level, but is more consistent
                 ;; with capital gains tax granularity: Each sale is a
                 ;; potentially taxable event.
                 (gain (gnc-numeric-sub
                          value
                          basis
                          report-currency-fraction
                          GNC-RND-ROUND))
                 (is-long-term
                    (long-term? latest-bought-split-date trans-date)))
            (gnc:debug
              (format #f
                "sold-amount: [~0,5f], sold-value: [~0,5f], gain: [~0,5f]"
                (gnc-numeric-to-double amount)
                (gnc-numeric-to-double value)
                (gnc-numeric-to-double gain)))

            (set! end-basis (gnc-numeric-sub-fixed end-basis basis))
            (set! end-amount (gnc-numeric-sub-fixed end-amount amount))

          (cond
            ((>= trans-date from-date)
              ;; Remember if a sale within the report window causes the
              ;; lot's balance to go negative.
              (if (and (null? first-negative-split)
                      (gnc-numeric-negative-p end-amount))
              (set! first-negative-split split))

              (cond
                (is-long-term
                  (set! long-term-sold-amount
                    (gnc-numeric-add-fixed long-term-sold-amount amount))
                  (set! long-term-sold-basis
                    (gnc-numeric-add-fixed long-term-sold-basis basis))
                  (set! long-term-sold-value
                    (gnc-numeric-add-fixed long-term-sold-value value))
                  (set! long-term-realized-gain
                    (gnc-numeric-add-fixed long-term-realized-gain gain)))
                (else
                  (set! short-term-sold-amount
                    (gnc-numeric-add-fixed short-term-sold-amount amount))
                  (set! short-term-sold-basis
                    (gnc-numeric-add-fixed short-term-sold-basis basis))
                  (set! short-term-sold-value
                    (gnc-numeric-add-fixed short-term-sold-value value))
                  (set! short-term-realized-gain
                    (gnc-numeric-add-fixed short-term-realized-gain gain))))

              (add-split-row
                split
                trans-date
                #f      ;; bought-amount
                #f      ;; bought-value
                amount  ;; sold-amount
                basis   ;; sold-basis
                value   ;; sold-value
                gain    ;; sold-gain
                is-long-term)

              (set! sold-split-count (+ sold-split-count 1))
              (set! last-sold-split split)))))

        ;; Merges in the purchase split info.
        (define (merge-purchase-split split trans-date amount value)
          ;; Track the first purchase split. Note: could just call
          ;; gnc-lot-get-earliest-split, but that would do another loop though
          ;; the split list. And it could return a sale split, if the lot is
          ;; malformed.
          (if (or (null? earliest-bought-split)
                  (< trans-date earliest-bought-split-date))
            (begin
              (set! earliest-bought-split split)
              (set! earliest-bought-split-date trans-date)))
          ;; Also track the latest bought split date. If the lot contains
          ;; multiple purchase splits, then the latest date will be used to
          ;; determine whether the lot is long or short term.
          (if (or (null? latest-bought-split-date)
                  (> trans-date latest-bought-split-date))
            (set! latest-bought-split-date trans-date))
          (set! end-basis (gnc-numeric-add-fixed end-basis value))
          (set! end-amount (gnc-numeric-add-fixed end-amount amount))
          (cond
            ((>= trans-date from-date)
              (set! bought-amount
                (gnc-numeric-add-fixed bought-amount amount))
              (set! bought-value
                (gnc-numeric-add-fixed bought-value value))
              (add-split-row
                split
                trans-date
                amount  ;; bought-amount
                value   ;; bought-value
                #f      ;; sold-amount
                #f      ;; sold-basis
                #f      ;; sold-value
                #f      ;; sold-gain
                #f))    ;; is-long-term

            ;; The split is from before the report's start date.
            ;; So we won't include it in the report table, but
            ;; we still need to count it for basis calculations.
            (else
              (set! old-bought-value
                (gnc-numeric-add-fixed old-bought-value value))
              (set! old-bought-amount
                (gnc-numeric-add-fixed old-bought-amount amount))))

          ;; Note that this also counts purchases before the report
          ;; start date.
          (set! bought-split-count (+ bought-split-count 1)))

        ;; Helper function that merges in the given stats.
        (define (merge-stats stats)
          (let ((include-amounts
                  (cond
                    ((get-is-lot-stats)
                      (gnc:error
                        "error: lot may not be set on the target collector")
                      #t)
                    ((get-is-account-stats) #t)
                    ;; grand total stats may pertain to accounts that have
                    ;; different commodities, so don't combine their amounts.
                    (else #f))))
            (set! bought-split-count
              (+ bought-split-count (stats 'get-bought-split-count)))
            (set! old-bought-value
              (gnc-numeric-add-fixed
                old-bought-value
                (stats 'get-old-bought-value)))
            (set! bought-value
              (gnc-numeric-add-fixed bought-value (stats 'get-bought-value)))
            (set! sold-split-count
              (gnc-numeric-add-fixed
                sold-split-count
                (stats 'get-sold-split-count)))
            (set! short-term-sold-basis
              (gnc-numeric-add-fixed
                short-term-sold-basis
                (stats 'get-short-term-sold-basis)))
            (set! short-term-sold-value
              (gnc-numeric-add-fixed
                short-term-sold-value
                (stats 'get-short-term-sold-value)))
            (set! short-term-realized-gain
              (gnc-numeric-add-fixed
                short-term-realized-gain
                (stats 'get-short-term-realized-gain)))
            (set! long-term-sold-basis
              (gnc-numeric-add-fixed
                long-term-sold-basis
                (stats 'get-long-term-sold-basis)))
            (set! long-term-sold-value
              (gnc-numeric-add-fixed
                long-term-sold-value
                (stats 'get-long-term-sold-value)))
            (set! long-term-realized-gain
              (gnc-numeric-add-fixed
                long-term-realized-gain
                (stats 'get-long-term-realized-gain)))
            (set! end-basis
              (gnc-numeric-add-fixed end-basis (stats 'get-end-basis)))
            (set! end-value
              (gnc-numeric-add-fixed end-value (stats 'get-end-value)))
            (set! unrealized-gain
              (gnc-numeric-add-fixed
                unrealized-gain
                (stats 'get-unrealized-gain)))
            (set! short-term-unrealized-gain
              (gnc-numeric-add-fixed
                short-term-unrealized-gain
                (stats 'get-short-term-unrealized-gain)))
            (set! long-term-unrealized-gain
              (gnc-numeric-add-fixed
                long-term-unrealized-gain
                (stats 'get-long-term-unrealized-gain)))
            (set! has-warnings
              (or has-warnings
                  (stats 'get-has-warnings)))
            (set! is-active-in-window
              (or is-active-in-window
                  (stats 'get-is-active-in-window)))

            (if include-amounts
              (begin
                (set! old-bought-amount
                  (gnc-numeric-add-fixed
                    old-bought-amount
                    (stats 'get-old-bought-amount)))
                (set! bought-amount
                  (gnc-numeric-add-fixed
                    bought-amount
                    (stats 'get-bought-amount)))
                (set! short-term-sold-amount
                  (gnc-numeric-add-fixed
                    short-term-sold-amount
                    (stats 'get-short-term-sold-amount)))
                (set! long-term-sold-amount
                  (gnc-numeric-add-fixed
                    long-term-sold-amount
                    (stats 'get-long-term-sold-amount)))
                (set! end-amount
                  (gnc-numeric-add-fixed end-amount (stats 'get-end-amount)))
                ;; The amounts are being combined, so they must all pertain to
                ;; the same currency. Copy it, if not already set.
                (if (null? currency)
                  (set! currency (stats 'get-currency)))))))

        ;; Dispatch function.
        (lambda args
          (apply
            (case (car args)
              ((init-for-account) init-for-account)
              ((merge-lot) merge-lot)
              ((merge-stats) merge-stats)
              ((add-to-table) add-to-table)

              ((get-bought-split-count) (lambda () bought-split-count))
              ((get-old-bought-amount) (lambda () old-bought-amount))
              ((get-old-bought-value) (lambda () old-bought-value))
              ((get-bought-amount) (lambda () bought-amount))
              ((get-bought-value) (lambda () bought-value))
              ((get-sold-split-count) (lambda () sold-split-count))
              ((get-short-term-sold-amount) (lambda () short-term-sold-amount))
              ((get-short-term-sold-basis) (lambda () short-term-sold-basis))
              ((get-short-term-sold-value) (lambda () short-term-sold-value))
              ((get-long-term-sold-amount) (lambda () long-term-sold-amount))
              ((get-long-term-sold-basis) (lambda () long-term-sold-basis))
              ((get-long-term-sold-value) (lambda () long-term-sold-value))
              ((get-short-term-realized-gain)
                  (lambda () short-term-realized-gain))
              ((get-long-term-realized-gain) (lambda () long-term-realized-gain))
              ((get-end-amount) (lambda () end-amount))
              ((get-end-basis) (lambda () end-basis))
              ((get-end-value) (lambda () end-value))
              ((get-unrealized-gain) (lambda () unrealized-gain))
              ((get-short-term-unrealized-gain)
                  (lambda () short-term-unrealized-gain))
              ((get-long-term-unrealized-gain)
                  (lambda () long-term-unrealized-gain))
              ((get-has-warnings) (lambda () has-warnings))
              ((get-is-active-in-window) (lambda () is-active-in-window))
              ((get-currency) (lambda () currency))
              (else (gnc:error (format #f "Bad action: ~a" (car args)))))
            (cdr args)))))

    ;; Create a stats collector for the grand total (all accounts) row.
    (define all-accounts-info
        (create-lot-stats-collector 0))

    ;; Adds the specified account to the report.
    (define (add-account-report account)
      (let* ((currency (xaccAccountGetCommodity account))
             (currency-fraction (gnc-commodity-get-fraction currency))
             ;; Note that this price may not be in the report-currency. Use
             ;; (price->report-currency-value price) to convert.
             (price (price-fn currency))
             (splits (get-all-splits account))
             (lot-pair (get-all-lots splits))
             (lots (car lot-pair))
             (unassigned-splits (cadr lot-pair))

             (get-account-balance (lambda()
                (let ((unit-collector (gnc:account-get-comm-balance-at-date
                                        account
                                        to-date
                                        #f))) ;; include-children?
                  (cadr (unit-collector 'getpair currency #f)))))
             ;; Gets whether the account should be included in the report,
             ;; based on the account balance and the option to ignore empty
             ;; accounts.
             (check-account-balance (lambda()
                (or
                  include-empty-accounts
                  (not (gnc-numeric-zero-p (get-account-balance))))))
             ;; Gets whether the account should be included in the report,
             ;; based on the number of lots and the option to ignore accounts
             ;; with no lots.
             (check-lot-count (lambda()
                (or include-lotless-accounts
                    (not (null? lots))))))

      ;; Adds the current account to the chart.
      (define (add-account-to-chart
                short-term-realized-gain
                long-term-realized-gain
                short-term-unrealized-gain
                long-term-unrealized-gain)
        (if show-chart
          (let ((gain-values
                  (append
                    (if show-realized-gain-columns
                      (if group-gains-by-age
                        (list
                            short-term-realized-gain
                            long-term-realized-gain)
                        (list (gnc-numeric-add-fixed
                            short-term-realized-gain
                            long-term-realized-gain)))
                      '())
                    (if show-unrealized-gain-columns
                      (if group-gains-by-age
                        (list
                            short-term-unrealized-gain
                            long-term-unrealized-gain)
                        (list (gnc-numeric-add-fixed
                            short-term-unrealized-gain
                            long-term-unrealized-gain)))
                      '()))))
            (gnc:html-chart-add-data-series!
                chart
                (account->name account)
                (map (compose gnc:gnc-monetary-amount value->monetary) gain-values)
                (car colors))
            (set! colors (cdr colors)))))

      ;; Adds to the given table rows for the given lots.
      (define (add-lots-rows table lots)
        (if (not lots) (gnc:error "lots is not specified"))
        (let* (;; Add rows at first to a temp table. Later, it may be copied
               ;; to the visible table.
               (account-table (gnc:make-html-table))
               (account-lots-info
                (create-lot-stats-collector currency-fraction)))

          (account-lots-info 'init-for-account account unassigned-splits)

          ;; Add account name anchor.
          (gnc:html-table-append-row/markup!
            account-table
            "grand-total"
            (list (gnc:make-html-table-cell/size/markup
                    1 ;; rowspan
                    column-count ;; colspan
                    "column-heading-left" ;; left, bold
                    (gnc:make-html-text (to-account-anchor account)))))

          ;; Add commodity price anchor.
          (gnc:html-table-append-row/markup!
            account-table
            "normal-row"
            (list (gnc:make-html-table-cell/size/markup
                    1 ;; rowspan
                    column-count ;; colspan
                    "text-cell"
                    (gnc:make-html-text (to-price-anchor price)))))

          (add-header-row account-table #f) ;; is-grand-total

          ;; Add one row per included lot.
          (let loop ((lots lots)
                     (is-odd-row #t))
            (match lots
              (() #f)
              ((lot . rest)
                (let ((lot-info (create-lot-stats-collector
                                    currency-fraction)))
                  (lot-info 'merge-lot lot currency price)

                  (loop rest
                    ;; Determine whether the lot should be included in the
                    ;; report.
                    (if (and (lot-info 'get-is-active-in-window)
                              (or include-closed-lots
                                  (not (gnc-numeric-zero-p
                                        (lot-info 'get-end-amount)))))
                      (let ((next-is-odd-row
                        ;; Add lot totals row, followed by any lot warnings.
                        (lot-info 'add-to-table account-table is-odd-row)))

                        ;; Merge the lot stats into the account stats
                        ;; collector.
                        (account-lots-info 'merge-stats lot-info)

                        next-is-odd-row)
                      is-odd-row))))))

          ;; Add account totals (footer) row, followed by any account-level
          ;; warnings.
          (account-lots-info 'add-to-table account-table #t)

          (if (and
                (account-lots-info 'get-is-active-in-window)
                (or (not include-only-accounts-with-warnings)
                    (account-lots-info 'get-has-warnings)))
            (begin
              (add-padding-rows 3)

              ;; Copy temp table rows to the shown table.
              (copy-table-rows account-table table #f)

              ;; Merge the account stats into the grand total collector.
              (all-accounts-info 'merge-stats account-lots-info)

              (add-account-to-chart
                (account-lots-info 'get-short-term-realized-gain)
                (account-lots-info 'get-long-term-realized-gain)
                (account-lots-info 'get-short-term-unrealized-gain)
                (account-lots-info 'get-long-term-unrealized-gain))))))

      ;; Add lots table
      (if (and (check-account-balance)
               (check-lot-count))
        (add-lots-rows table lots))))

    (gnc:html-document-set-style-text! document
      "td.indented { text-indent: 2em }")

    (gnc:html-document-set-title!
      document
      (format #f
        (G_ "~a, ~a to ~a")
        (get-option gnc:pagename-general gnc:optname-reportname)
        (qof-print-date from-date)
        (qof-print-date to-date)))

    (cond
      ((not (null? accounts))
        (cond
          (show-chart
            (let ((labels
                  (append
                    (if show-realized-gain-columns
                      (if group-gains-by-age
                        (list
                            colname-short-term-realized-gain
                            colname-long-term-realized-gain)
                        (list colname-realized-gain))
                      '())
                    (if show-unrealized-gain-columns
                        (if group-gains-by-age
                          (list
                              colname-short-term-unrealized-gain
                              colname-long-term-unrealized-gain)
                          (list colname-unrealized-gain))
                        '()))))
            (gnc:html-chart-set-title! chart
              (list (N_ "Account Lot Gains")
                (format #f
                  (G_ "~a to ~a")
                  (qof-print-date from-date)
                  (qof-print-date to-date))))
            (gnc:html-chart-set-type! chart 'bar)
            (gnc:html-chart-set-width! chart chart-width)
            (gnc:html-chart-set-height! chart chart-height)
            (gnc:html-chart-set-data-labels! chart labels)
            (gnc:html-chart-set-y-axis-label!
              chart (gnc-commodity-get-mnemonic report-currency))
            (gnc:html-chart-set-currency-iso!
              chart (gnc-commodity-get-mnemonic report-currency))
            (gnc:html-chart-set-currency-symbol!
              chart (gnc-commodity-get-nice-symbol report-currency))
            (gnc:html-chart-set-stacking?! chart
              (eq? chart-type 'bar-stacked)))))

        (for-each add-account-report accounts)

        (add-padding-rows 3)
        (gnc:html-table-append-ruler! table column-count)
        (gnc:html-table-append-ruler! table column-count)
        (add-header-row table #t) ;; is-grand-total

        (all-accounts-info 'add-to-table table #t)))

    (when (and show-chart (eq? chart-location 'top))
        (gnc:html-document-add-object! document chart)
        (add-padding-rows 3))

    (gnc:html-document-add-object! document table)

    (when (and show-chart (eq? chart-location 'bottom))
      (add-padding-rows 3)
      (gnc:html-document-add-object! document chart))

    document))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "ab2acc24afd14630a551f98f1a35fa81"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer investment-lots-renderer)
