;; -*- scheme -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asset-allocation.scm
;; Asset allocation by holdings and the shared allocation report engine.
;; The accounts variant supplies explicit account groups to this module.
;; Copyright (C) 2026 Brent McBride (mcbridebt@hotmail.com)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard asset-allocation)
  #:export (allocation-options-generator
            allocation-renderer
            allocation-read-category-data
            allocation-valid-category-definitions?
            default-allocation-setup?
            prepend-allocation-setup!))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-13))
(use-modules (ice-9 format))

(define reportname (N_ "Asset Allocation"))
(define report-guid "9cd88bff015943a8b26de06a4cae5d91")
(define pagename-chart (N_ "Chart"))
(define pagename-categories (N_ "Categories"))
(define optname-definitions (N_ "Categories"))
(define optname-start-date (N_ "Start Date"))
(define optname-end-date (N_ "End Date"))
(define optname-step (N_ "Step Size"))
(define optname-view (N_ "Time Frame"))
(define optname-chart (N_ "Chart Type"))
(define optname-measure (N_ "Measure"))
(define optname-scale (N_ "Units"))
(define optname-tooltip (N_ "Tooltip"))
(define optname-data-labels (N_ "Data Labels"))
(define optname-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-transaction-prices (N_ "Use transaction prices when missing"))
(define optname-show-table (N_ "Show Table"))
(define optname-width (N_ "Plot Width"))
(define optname-height (N_ "Plot Height"))

;; Every category at a date shares the same validity decision. A partial or
;; negative allocation must not look complete merely because one series is
;; valid.
(define-record-type <allocation-point>
  (make-allocation-point date amounts unpriced chartable?)
  allocation-point?
  (date point-date)
  (amounts point-amounts)
  (unpriced point-unpriced)
  (chartable? point-chartable?))

(define (register-allocation-general-options options)
  (gnc:options-add-date-interval!
   options gnc:pagename-general optname-start-date optname-end-date "b")
  (gnc-set-option options gnc:pagename-general optname-start-date
                  '(relative . start-cal-year))
  (gnc-set-option options gnc:pagename-general optname-end-date
                  '(relative . end-cal-year))
  (gnc:options-add-interval-choice!
   options gnc:pagename-general optname-step "d" 'MonthDelta)
  (gnc:options-add-currency!
   options gnc:pagename-general optname-currency "e")
  (gnc:options-add-price-source!
   options gnc:pagename-general optname-price-source "f" 'pricedb-before)
  (gnc-register-simple-boolean-option
   options gnc:pagename-general optname-transaction-prices "g"
   (N_ "When the selected price source \
cannot value a security, use its most recent usable transaction price on or \
before the valuation date from the selected stock and mutual fund accounts. \
Transaction values may reflect cost basis rather than market prices.") #t))

(define (register-allocation-display-options options)
  (gnc:options-add-plot-size!
   options pagename-chart optname-width optname-height "e"
   (cons 'percent 100.0) (cons 'percent 75.0))
  (gnc-register-simple-boolean-option
   options gnc:pagename-display optname-show-table "c"
   (N_ "Show the allocation table below the chart.") #t))

(define* (allocation-chart-choices #:key (history? #f) (drift? #f))
  (cond (history? '(bars stacked-bar line unstacked-line))
        (drift? '(bars stacked-bar))
        (else '(bars doughnut pie stacked-bar))))

(define* (allocation-default-chart #:key (history? #f) (drift? #f))
  (if history? (if drift? 'unstacked-line 'line)
      (if drift? 'bars 'doughnut)))

(define* (allocation-select-chart selected #:key (history? #f) (drift? #f))
  (if (memq selected (allocation-chart-choices
                      #:history? history? #:drift? drift?))
      selected
      (allocation-default-chart #:history? history? #:drift? drift?)))

(define (allocation-render-chart-type selected history?)
  (case selected
    ((bars) (if history? 'grouped-bar 'horizontal-bar))
    ((stacked-bar) (if history? 'bar 'stacked-bar))
    (else selected)))

;; Read pending widget choices, not just saved values. Filtering the chart
;; menu must not apply other controls before the user presses Apply or OK.
(define (update-allocation-chart-controls options targets?)
  (define (choice name)
    (gnc-optiondb-lookup-choice options pagename-chart name))
  (let* ((history? (eq? (choice optname-view) 'time-series))
         (drift? (and targets? (eq? (choice optname-measure) 'drift)))
         (chart (allocation-select-chart
                 (choice optname-chart)
                 #:history? history? #:drift? drift?)))
    (gnc-optiondb-set-option-choices
     options pagename-chart optname-chart
     (allocation-chart-choices #:history? history? #:drift? drift?)
     (allocation-default-chart #:history? history? #:drift? drift?))
    (gnc-optiondb-set-option-selectable-by-name
     options pagename-chart optname-scale
      (or history? drift? (eq? chart 'bars) (eq? chart 'stacked-bar)))
    (gnc-optiondb-set-option-selectable-by-name
      options pagename-chart optname-tooltip #t)
    (gnc-optiondb-set-option-selectable-by-name
     options pagename-chart optname-data-labels
     (and (not history?) (not drift?)
          (or (eq? chart 'pie) (eq? chart 'doughnut)
              (eq? chart 'stacked-bar))))))

(define (register-allocation-chart-options options targets?)
  (define (changed value)
    (update-allocation-chart-controls options targets?))
  (when targets?
    (gnc-register-multichoice-callback-option
     options pagename-chart optname-measure "0"
     (N_ "Show allocations or their differences from category targets.")
     "allocation"
     (list (vector 'allocation (N_ "Allocation"))
           (vector 'drift (N_ "Drift from Target"))) changed))
  (gnc-register-multichoice-callback-option
   options pagename-chart optname-chart "b"
   (N_ "Chart type for the selected time frame and measure. Bars are \
grouped by category in a time series.") "doughnut"
   (list (vector 'bars (N_ "Bars"))
         (vector 'doughnut (N_ "Doughnut"))
         (vector 'pie (N_ "Pie"))
         (vector 'stacked-bar (N_ "Stacked Bars"))
         (vector 'line (N_ "Stacked Lines"))
         (vector 'unstacked-line (N_ "Unstacked Lines"))) changed)
  (gnc-register-multichoice-option
   options pagename-chart optname-scale "d"
   (N_ "Show allocations or drift as percentages or monetary values.")
   "percent"
   (list (vector 'percent (N_ "Percentages"))
         (vector 'value (N_ "Currency"))))
    (gnc-register-multichoice-option
     options pagename-chart optname-tooltip "d1"
    (N_ "Information shown when hovering over chart data.")
     "name-percent-value"
     (list (vector 'name (N_ "Name"))
       (vector 'name-percent (N_ "Name + Percentage"))
       (vector 'name-value (N_ "Name + Value"))
       (vector 'name-percent-value (N_ "Name + Percentage + Value"))))
    (gnc-register-multichoice-option
    options pagename-chart optname-data-labels "d2"
    (N_ "Information shown on pie or doughnut slices and snapshot stacked \
  bar segments. Values are amounts in the report currency.") "percent"
     (list (vector 'none (N_ "None"))
       (vector 'percent (N_ "Percentage"))
       (vector 'value (N_ "Value"))
       (vector 'both (N_ "Percentage + Value"))))
  (gnc-register-multichoice-callback-option
   options pagename-chart optname-view "a"
   (N_ "Snapshot uses End Date and ignores Start Date and Step Size. \
Time Series uses Start Date, End Date, and Step Size.") "snapshot"
   (list (vector 'snapshot (N_ "Snapshot"))
         (vector 'time-series (N_ "Time Series"))) changed)
  (update-allocation-chart-controls options targets?))

;; Return common date, currency, chart and display options. Each optional
;; registrar receives the database and adds its report's category/account UI.
;; TARGETS? enables the holdings report's measure selector and drift choices.
(define* (allocation-options-generator
          #:optional category-registrar account-registrar
          #:key (targets? #f))
  (let ((options (gnc-new-optiondb)))
    (register-allocation-general-options options)
    (register-allocation-chart-options options targets?)
    (when account-registrar (account-registrar options))
    (when category-registrar (category-registrar options))
    (register-allocation-display-options options)
    (gnc:options-set-default-section options pagename-categories)
    options))

;; Read (explicit (name member ...)) or (explicit-targets (name target member
;; ...)). Members are account GUIDs or commodity unique names; target is a
;; percentage or #f. By default strip targets for grouping. INCLUDE-TARGETS?
;; retains them. Read/target errors return #f; callers validate names and
;; member IDs separately. Persisted forms are never evaluated.
(define* (allocation-read-category-data text #:key (include-targets? #f))
  (define (valid-target-entry? entry)
    (and (list? entry) (>= (length entry) 2)
         (let ((target (cadr entry)))
           (or (eq? target #f) (and (real? target) (<= 0 target 100))))))
  (define (without-target entry)
    (cons (car entry) (cddr entry)))
  (catch #t
    (lambda ()
      (let ((data (call-with-input-string text read)))
        (if (and (pair? data) (eq? (car data) 'explicit-targets))
            (and (list? data) (every valid-target-entry? (cdr data))
                 (if include-targets? data
                     (cons 'explicit (map without-target (cdr data)))))
            data)))
    (lambda args #f)))

(define (allocation-category-targets options categories)
  (let* ((data (allocation-read-category-data
                (gnc-optiondb-lookup-value options pagename-categories
                                           optname-definitions)
                #:include-targets? #t))
         (entries (if (and (pair? data) (eq? (car data) 'explicit-targets))
                      (cdr data) '())))
    ;; Groups retain definition order even when their display names change.
    ;; An automatically appended Uncategorized has no implicit target.
    (take (append (map cadr entries) (make-list (length categories) #f))
          (length categories))))

;; Return actual minus target in percentage points, or 'null for an invalid
;; date or missing target. Chart builders convert percentage points to ratios.
(define (allocation-point-drift point targets)
  (let ((total (apply + (point-amounts point))))
    (if (and (point-chartable? point) (positive? total))
        (map (lambda (amount target)
               (if (number? target) (- (* 100 (/ amount total)) target)
                   'null))
             (point-amounts point) targets)
        (make-list (length targets) 'null))))

;; Stacked charts need bounds for each sign's sum, not just individual values.
;; A minimum extent keeps a perfectly on-target chart nondegenerate.
(define (allocation-drift-extent drifts stacked?)
  (define (signed-totals values)
    (let ((numbers (filter number? values)))
      (list (apply + (filter positive? numbers))
            (apply + (filter negative? numbers)))))
  (let ((values (if stacked? (append-map signed-totals drifts)
                    (concatenate drifts))))
    (max 1 (ceiling (apply max 0 (map abs (filter number? values)))))))

;; Exact zero needs a point marker because a zero-height bar is invisible.
;; Omit it from the bar series so hovering never displays duplicate values.
(define (populate-drift-snapshot-chart! chart categories values anchor)
  (define (zero-value? value)
    (and (number? value) (zero? value)))
  (let ((colors (gnc:assign-colors (length categories)))
        (urls (map anchor categories)))
    (gnc:html-chart-set! chart '(options indexAxis) "x")
    (gnc:html-chart-set! chart '(options scales y type) "linear")
    (gnc:html-chart-set-x-axis-label! chart (G_ "Category"))
    (gnc:html-chart-set! chart '(options scales x title font size) 14)
    (gnc:html-chart-set! chart '(options scales y title font size) 14)
    (gnc:html-chart-set! chart '(options plugins legend display) #f)
    (gnc:html-chart-set-data-labels! chart (map car categories))
    (gnc:html-chart-add-data-series!
     chart (G_ "Drift from Target")
     (map (lambda (value) (if (zero-value? value) 'null value)) values)
     colors 'urls urls)
    (when (any zero-value? values)
      (gnc:html-chart-add-data-series!
       chart (G_ "Drift from Target")
       (map (lambda (value) (if (zero-value? value) 0 'null)) values)
       colors 'type 'line 'showLine #f 'fill #f
      'pointStyle 'circle 'pointRadius 5 'pointHoverRadius 7
      'pointHitRadius 8
       'borderWidth 2 'order -1 'urls urls))))

;; Build signed drift series. DRIFTS contains percentage points for each date;
;; monetary mode scales each date by its own total, retaining 'null gaps.
(define* (make-allocation-drift-chart categories points drifts category-anchor
                                      #:key (history? #f) (percent? #t)
                                      (chart-type (if history?
                                                      'unstacked-line
                                                      'horizontal-bar)))
  (let* ((chart (gnc:make-html-chart))
         (colors (gnc:assign-colors (length categories)))
         (stacked? (if (memq chart-type '(line bar stacked-bar)) #t #f))
         (line? (if (memq chart-type '(line unstacked-line)) #t #f))
         (numeric-axis 'y)
         (chart-drifts
          (map (lambda (point values)
                 (let ((scale (if percent? 1
                                  (apply + (point-amounts point)))))
                   (map (lambda (value)
                          (if (number? value) (* scale (/ value 100)) value))
                        values)))
               points drifts))
         (extent (allocation-drift-extent
                  (if percent? drifts chart-drifts) stacked?))
         (axis-extent (if percent? (/ extent 100) extent)))
    (gnc:html-chart-set-type! chart (if line? 'line 'bar))
    (gnc:html-chart-set-stacking?! chart stacked?)
    (when percent?
      (gnc:html-chart-set-format-style! chart "percent")
      (gnc:html-chart-set! chart '(options numberFormat
                                           maximumFractionDigits) 2))
    (gnc:html-chart-set! chart `(options scales ,numeric-axis ticks
                                         useNumberCellStyle) #t)
    (gnc:html-chart-set!
     chart `(options scales ,numeric-axis suggestedMin) (- axis-extent))
    (gnc:html-chart-set! chart
                         `(options scales ,numeric-axis suggestedMax)
                         axis-extent)
    (gnc:html-chart-set! chart `(options scales ,numeric-axis title text)
                         (G_ "Drift from Target"))
    (if (or history? stacked?)
        (begin
          (gnc:html-chart-set! chart '(options plugins legend display) #t)
          (gnc:html-chart-set! chart '(options plugins legend position)
                               'bottom)
          (gnc:html-chart-set-tooltip-mode! chart (if history? 'index
                                                      'nearest))
          (when stacked?
            (gnc:html-chart-set! chart '(options plugins legend reverse) #t))
          (gnc:html-chart-set-data-labels!
           chart (if history? (map (compose qof-print-date point-date) points)
                     (list (G_ "Total"))))
          (for-each
           (lambda (category values color index)
             (apply gnc:html-chart-add-data-series!
                    chart (car category) values color 'urls (category-anchor
                                                             category)
                    (append (if stacked? (list 'order (- index)) '())
                            (if line?
                                (list 'fill (if stacked? "origin" #f)
                                      'borderColor color 'borderWidth 2
                                      'pointRadius 2
                                      'tension 0 'spanGaps #f)
                                '()))))
           categories (apply map list chart-drifts) colors
           (iota (length categories))))
        (populate-drift-snapshot-chart!
         chart categories (car chart-drifts) category-anchor))
    chart))

(define (make-allocation-drift-table categories points targets drifts
                                     history? category-link money)
  (define (number-cell value)
    (gnc:make-html-table-cell/markup "number-cell" value))
  (define (drift-cell value)
    (gnc:make-html-table-cell/markup
     (if (and (number? value) (negative? value)) "number-cell-neg"
         "number-cell")
     (if (number? value) (format #f "~,2f%" value) "")))
  (let ((table (gnc:make-html-table)))
    (if history?
        (begin
          (gnc:html-table-set-col-headers!
           table (cons (G_ "Date / Difference %")
                       (map (compose gnc:html-string-sanitize car)
                            categories)))
          (for-each
           (lambda (point values)
             (gnc:html-table-append-row!
              table (cons (qof-print-date (point-date point)) (map
                                                               drift-cell
                                                               values))))
           points drifts))
        (let* ((point (car points))
               (total (apply + (point-amounts point))))
          (gnc:html-table-set-col-headers!
           table (list (G_ "Category") (G_ "Target %") (G_ "Actual %")
                       (G_ "Difference %") (G_ "Target Value") (G_ "Actual \
Value")
                       (G_ "Difference")))
          (for-each
           (lambda (category amount target drift)
             (let ((target-value (and (number? target) (* total (/ target
                                                                   100)))))
               (gnc:html-table-append-row!
                table (list (category-link category)
                               (number-cell
                                (if (number? target)
                                 (format #f "~,2f%" target) ""))
                               (number-cell
                                (if (and (point-chartable? point)
                                   (positive? total))
                                 (format #f "~,2f%" (* 100 (/ amount total)))
                                 ""))
                            (drift-cell drift)
                            (number-cell (if (and target-value
                                                  (point-chartable? point))
                                             (money target-value) ""))
                            (number-cell (money amount))
                            (number-cell (if (and target-value
                                                  (point-chartable? point))
                                             (money (- amount
                                                       target-value)) ""))))))
           categories (point-amounts point) targets (car drifts))))
    table))

;; Check unique nonblank names and string member IDs, without resolving IDs.
;; Return #t for valid definitions, including an empty category list.
;; Empty member lists are accepted when requested by the report.
(define* (allocation-valid-category-definitions?
          definitions #:key (allow-empty-members? #f))
  (and (list? definitions)
       (every (lambda (category)
                (and (list? category) (pair? category)
                     (string? (car category))
                     (not (string-null? (string-trim-both (car category))))
                     (or allow-empty-members? (pair? (cdr category)))
                     (every string? (cdr category))))
              definitions)
       (= (length definitions)
          (length (delete-duplicates (map car definitions))))))

(define (category-collectors-at-dates accounts dates)
  ;; Membership is explicit; descendants are never included implicitly.
  (let ((collectors (map (lambda (date) (gnc:make-commodity-collector))
                         dates)))
    (for-each
     (lambda (account)
       (for-each
        (lambda (collector balance)
          (collector 'add (xaccAccountGetCommodity account) balance))
        collectors
        (gnc:account-accumulate-at-dates account dates #:nosplit->elt 0)))
     accounts)
    collectors))

;; Index positive transaction prices from selected security accounts only.
;; Vectors are sorted oldest first so date lookups can use binary search.
(define (allocation-transaction-price-index accounts end-date)
  (let ((index (make-hash-table)))
    (for-each
     (lambda (split)
       (let ((price (xaccSplitGetSharePrice split)))
         (when (and (not (zero? (xaccSplitGetAmount split))) (positive?
                                                              price))
           (let* ((transaction (xaccSplitGetParent split))
                  (key (gnc-commodity-get-unique-name
                        (xaccAccountGetCommodity (xaccSplitGetAccount
                                                  split))))
                  (trade (vector (xaccTransGetDate transaction)
                                 (xaccTransGetCurrency transaction) price)))
             (hash-set! index key (cons trade (hash-ref index key '())))))))
     (gnc:get-match-commodity-splits-sorted
      (filter (lambda (account)
                (memv (xaccAccountGetType account)
                      (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)))
              accounts)
      end-date #f))
    (hash-for-each
     (lambda (key trades) (hash-set! index key (list->vector (reverse
                                                              trades))))
     index)
    index))

;; Return the last trade index on/before DATE, or -1. Equal timestamps choose
;; the last entry in the date-sorted vector, matching the transaction query.
(define (allocation-last-trade trades date)
  (let loop ((low 0) (high (vector-length trades)))
    (if (= low high)
        (1- low)
        (let ((middle (quotient (+ low high) 2)))
          (if (<= (vector-ref (vector-ref trades middle) 0) date)
              (loop (1+ middle) high)
              (loop low middle))))))

(define (allocation-transaction-unit-price trades domestic date convert)
  (let loop ((index (allocation-last-trade trades date)))
    (and (>= index 0)
         (let* ((trade (vector-ref trades index))
                (price (convert (gnc:make-gnc-monetary
                                 (vector-ref trade 1) (vector-ref trade 2))
                                domestic))
                (amount (gnc:gnc-monetary-amount price)))
           (if (positive? amount) amount (loop (1- index)))))))

;; Lazily index trades and cache a unit price (including failed lookups) for
;; each commodity/report-currency/date. Multiply by each caller's own balance.
(define (make-transaction-price-fn accounts end-date)
  (let ((index #f) (cache (make-hash-table)))
    (lambda (foreign domestic date convert)
      (let* ((commodity (gnc-commodity-get-unique-name
                         (gnc:gnc-monetary-commodity foreign)))
             (key (list commodity (gnc-commodity-get-unique-name domestic)
                        date))
             (cached (hash-ref cache key 'missing))
             (price
              (if (eq? cached 'missing)
                  (begin
                    (unless index
                      (set! index (allocation-transaction-price-index
                                   accounts end-date)))
                    (let ((trades (hash-ref index commodity)))
                      (let ((value (and trades
                                        (allocation-transaction-unit-price
                                         trades domestic date
                                         convert))))
                        (hash-set! cache key value)
                        value)))
                  cached)))
        (and price (gnc:make-gnc-monetary
                    domestic (* (gnc:gnc-monetary-amount foreign) price)))))))

(define (allocation-price-details points getter)
  (filter-map
   (lambda (point)
     (and (pair? (getter point))
          (format #f "~a: ~{~a~^, ~}" (qof-print-date (point-date point))
                  (map gnc-commodity-get-printname (getter point)))))
   points))

(define (allocation-totals points)
  (map (lambda (point) (apply + (point-amounts point))) points))

(define (number-cell value)
  (gnc:make-html-table-cell/markup "number-cell" value))

(define (percentage amount total)
  (if (zero? total) "" (format #f "~,1f%" (* 100 (/ amount total)))))

(define (category-anchor category)
  (if (= (length category) 2)
      (gnc:account-anchor-text (cadr category)) ""))

(define (category-link category)
  (let ((name (gnc:html-string-sanitize (car category))))
    (if (= (length category) 2)
        (gnc:make-html-text
         (gnc:html-markup-anchor (category-anchor category) name))
        name)))

(define (add-allocation-history-series! chart categories points percent?
                                        chart-type)
  (let ((totals (allocation-totals points))
        (colors (gnc:assign-colors (length categories)))
        (stacked? (not (memq chart-type '(unstacked-line grouped-bar))))
        (line? (memq chart-type '(line unstacked-line))))
    (for-each
     (lambda (category values color index)
       (apply gnc:html-chart-add-data-series!
              chart (car category)
              (map (lambda (amount total point)
                     (cond
                      ((not (point-chartable? point)) 'null)
                      ;; A zero amount is real data; a zero-total percentage
                      ;; is undefined and must be a gap.
                      (percent? (if (zero? total) 'null (/ amount total)))
                      (else amount)))
                   values totals points)
              color 'urls (category-anchor category)
              (append
               (if stacked? (list 'order (- index)) '())
               (if line?
                   (list 'fill (if stacked? "stack" #f)
                         'borderColor color 'borderWidth 2
                         'pointRadius 2 'tension 0 'spanGaps #f)
                   '()))))
     categories (apply map list (map point-amounts points)) colors
     (iota (length categories)))))

(define (populate-allocation-history-chart! chart categories points percent?
                                            chart-type currency)
  (let ((stacked? (not (memq chart-type '(unstacked-line grouped-bar)))))
    (gnc:html-chart-set-stacking?! chart stacked?)
    (when stacked?
      (gnc:html-chart-set! chart '(options plugins legend reverse) #t))
    (gnc:html-chart-set-tooltip-mode! chart 'index)
    (gnc:html-chart-set-data-labels!
     chart (map (compose qof-print-date point-date) points))
    (gnc:html-chart-set-y-axis-label!
     chart (if percent? (G_ "Percent") (gnc-commodity-get-mnemonic currency)))
    (gnc:html-chart-set! chart '(options scales y beginAtZero) #t)
    (when percent?
      (gnc:html-chart-set-format-style! chart "percent")
      (gnc:html-chart-set! chart '(options scales y min) 0)
      (gnc:html-chart-set! chart '(options scales y max) 1))
    (add-allocation-history-series! chart categories points percent?
                                    chart-type)))

;; Chart renderers return fragments in reverse output order. Wrap the shared
;; chart so report customizations execute after its canvas is initialized.
(define (allocation-chart-render chart document)
  (let ((id (symbol->string (gensym "allocation-chart"))))
    (gnc:html-chart-set! chart '(plugins) #())
    (append
     (list
      (format #f "<script>
(function () {
  var canvas = document.getElementById('~a').querySelector('canvas');
  var chart = Chart.getChart(canvas);
  var money = new Intl.NumberFormat(undefined, {
    style: 'currency', currency: chart.options.allocationCurrency
  });
  var percent = new Intl.NumberFormat(undefined, {
    style: 'percent',
    minimumFractionDigits: chart.options.allocationDrift ? 2 : 1,
    maximumFractionDigits: chart.options.allocationDrift ? 2 : 1
  });
  function monetaryLabel(amount) {
    var parts = money.formatToParts(amount);
    if (parts.length > 1 && parts[0].type === 'currency' &&
        parts[0].value.endsWith('$') && parts[1].type === 'literal' &&
        parts[1].value.trim() === '') {
      // Match GnuCash's leading-space placement for dollar currency symbols.
      parts.unshift(parts.splice(1, 1)[0]);
    }
    return parts.map(function (part) { return part.value; }).join('');
  }
  var stacked = chart.config.type === 'bar' && chart.options.scales.x.stacked;
  chart.options.plugins.tooltip.callbacks.title = function (items) {
    if (!items.length) {
      return '';
    }
    return stacked && !chart.options.allocationHistory ?
      items[0].dataset.label : items[0].label;
  };
  chart.options.plugins.tooltip.callbacks.label = function (context) {
    var index = context.dataIndex;
    var mode = chart.options.allocationTooltip;
    var name = chart.options.allocationHistory ? context.dataset.label : '';
    if (mode === 'name') {
      return name;
    }
    var fraction = context.dataset.allocationPercentages[index];
    var amount = context.dataset.allocationAmounts[index];
    var value;
    if (mode === 'name-percent') {
      value = typeof fraction === 'number' ? percent.format(fraction) : '';
    } else if (mode === 'name-value') {
      value = typeof amount === 'number' ? monetaryLabel(amount) : '';
    } else {
      value = typeof amount === 'number' ? monetaryLabel(amount) +
        (typeof fraction === 'number' ?
          ' (' + percent.format(fraction) + ')' : '') : '';
    }
    return value && name ? name + ': ' + value : value;
  };
    if ((chart.config.type === 'pie' || chart.config.type === 'doughnut' ||
      stacked) &&
      chart.options.allocationDataLabels &&
      chart.options.allocationDataLabels !== 'none') {
    var labels = new Intl.NumberFormat(undefined, {
      style: 'percent', maximumFractionDigits: 0
    });
    chart.config.plugins.push({
      id: 'allocationPercentages',
      afterDatasetsDraw: function (active) {
        var context = active.ctx;
        var font = Chart.helpers.toFont(active.options.font);
        context.save();
        context.font = font.string;
        context.textAlign = 'center';
        context.textBaseline = 'middle';
        var occupied = [];
        active.getSortedVisibleDatasetMetas().forEach(function (meta) {
          var dataset = active.data.datasets[meta.index];
          var values = dataset.allocationPercentages;
          meta.data.forEach(function (element, index) {
            if ((!stacked && !active.getDataVisibility(index)) ||
                !(values[index] > 0)) {
              return;
            }
            context.font = font.string;
            var mode = active.options.allocationDataLabels;
            var lines = mode === 'value' ? [] :
              [labels.format(values[index])];
            if (mode === 'value' || mode === 'both') {
              lines.push(monetaryLabel(dataset.allocationAmounts[index]));
            }
            var width = Math.max.apply(null, lines.map(function (text) {
              return context.measureText(text).width;
            }));
            var center = element.getCenterPoint();
            var area = active.chartArea;
            var insetX = Math.min(width / 2 + 3,
                                  (area.right - area.left) / 2);
            var insetY = Math.min(font.lineHeight * lines.length / 2 + 3,
                                  (area.bottom - area.top) / 2);
            var horizontal = Math.max(area.left + insetX,
              Math.min(area.right - insetX, center.x));
            var vertical = Math.max(area.top + insetY,
              Math.min(area.bottom - insetY, center.y));
            for (var attempt = 0; attempt < occupied.length; attempt++) {
              var overlap = occupied.find(function (box) {
                return horizontal + insetX > box.left &&
                  horizontal - insetX < box.right &&
                  vertical + insetY > box.top &&
                  vertical - insetY < box.bottom;
              });
              if (!overlap) {
                break;
              }
              var below = overlap.bottom + insetY + 2;
              vertical = below <= area.bottom - insetY ? below :
                Math.max(area.top + insetY, overlap.top - insetY - 2);
            }
            occupied.push({left: horizontal - insetX,
              right: horizontal + insetX, top: vertical - insetY,
              bottom: vertical + insetY});
            context.fillStyle = 'rgba(255, 255, 255, 0.5)';
            context.fillRect(horizontal - width / 2 - 3,
              vertical - font.lineHeight * lines.length / 2 - 3,
              width + 6, font.lineHeight * lines.length + 6);
            context.fillStyle = '#222';
            lines.forEach(function (text, line) {
              var position = vertical +
                (line - (lines.length - 1) / 2) * font.lineHeight;
              context.fillText(text, horizontal, position);
            });
          });
        });
        context.restore();
      }
    });
    chart.update();
  }
})();
</script>" id)
      "</div>")
     (gnc:html-chart-render chart document)
     (list (format #f "<div id='~a' style='display:contents'>" id)))))

(define (populate-allocation-snapshot-chart! chart categories points
                                             chart-type
                                             percent? currency)
  (let* ((total (apply + (point-amounts (car points))))
         (colors (gnc:assign-colors (length categories)))
         (stacked? (eq? chart-type 'stacked-bar))
         (bars? (or stacked? (eq? chart-type 'horizontal-bar)))
         (entries (filter (lambda (entry) (positive? (cadr entry)))
                          (map list categories (point-amounts (car points))
                               colors))))
    (gnc:html-chart-set-axes-display! chart bars?)
    (when bars?
      (gnc:html-chart-set! chart '(options indexAxis) "x")
      (gnc:html-chart-set! chart '(options scales x type) "category")
      (gnc:html-chart-set! chart '(options scales y type) "linear")
      (gnc:html-chart-set! chart '(options scales y beginAtZero) #t)
      (gnc:html-chart-set-x-axis-label! chart (if stacked? "" (G_
                                                               "Category")))
      (gnc:html-chart-set-y-axis-label!
       chart (if percent? (G_ "Percent") (gnc-commodity-get-mnemonic
                                          currency)))
      (when percent?
        (gnc:html-chart-set-format-style! chart "percent")
        (gnc:html-chart-set! chart '(options numberFormat
                                             maximumFractionDigits) 2)
        (gnc:html-chart-set! chart '(options scales y min) 0)
        (gnc:html-chart-set! chart '(options scales y max) 1))
      (when stacked?
        (gnc:html-chart-set-tooltip-mode! chart 'nearest)
        (gnc:html-chart-set! chart '(options scales x stacked) #t)
        (gnc:html-chart-set! chart '(options scales y stacked) #t)
        (gnc:html-chart-set! chart '(options plugins legend position)
                             "bottom"))
      (gnc:html-chart-set! chart '(options plugins legend display) stacked?))
    (gnc:html-chart-set! chart '(options allocationCurrency)
                         (gnc-commodity-get-mnemonic currency))
    (gnc:html-chart-set! chart '(options interaction intersect) #t)
    (gnc:html-chart-set-data-labels!
     chart (if stacked? (list (G_ "Total")) (map caar entries)))
    (if stacked?
        (for-each
         (lambda (entry)
           (let* ((amount (cadr entry))
                  (fraction (/ amount total)))
             (gnc:html-chart-add-data-series!
              chart (caar entry) (list (if percent? fraction amount))
              (list (caddr entry))
              'allocationAmounts (vector amount)
              'allocationPercentages (vector fraction)
              'urls (list (category-anchor (car entry))))))
         entries)
        (gnc:html-chart-add-data-series!
         chart (G_ "Value")
         (map (lambda (entry) (if percent? (/ (cadr entry) total) (cadr
                                                                   entry)))
              entries)
         (map caddr entries)
         'allocationAmounts (list->vector (map cadr entries))
         'allocationPercentages
         (list->vector (map (lambda (entry) (/ (cadr entry) total)) entries))
         'urls (map (lambda (entry) (category-anchor (car entry)))
                    entries)))))

(define (populate-allocation-history-table! table categories points
                                            percent? money)
  (let ((unpriced? (any (lambda (point) (pair? (point-unpriced point)))
                        points)))
    (gnc:html-table-set-col-headers!
     table (append (list (G_ "Date"))
                   (map (compose gnc:html-string-sanitize car) categories)
                   (list (if (and unpriced? (not percent?))
                             (G_ "Priced Total") (G_ "Total")))))
    (for-each
     (lambda (point total)
       (let ((incomplete? (pair? (point-unpriced point))))
         (gnc:html-table-append-row!
          table
          (cons (qof-print-date (point-date point))
                (map (lambda (amount)
                       (number-cell
                        (if percent?
                            (if incomplete? "" (percentage amount total))
                            (money amount))))
                     (append (point-amounts point) (list total)))))))
     points (allocation-totals points))))

(define (populate-allocation-snapshot-table! table categories points money)
  (let ((values (point-amounts (car points)))
        (total (apply + (point-amounts (car points))))
        (incomplete? (pair? (point-unpriced (car points)))))
    (gnc:html-table-set-col-headers!
     table (list (G_ "Category")
                 (if incomplete? (G_ "Priced Value") (G_ "Value"))
                 (G_ "Percent")))
    (for-each
     (lambda (category amount)
       (gnc:html-table-append-row!
        table (list (category-link category) (number-cell (money amount))
                    (number-cell (if incomplete? "" (percentage amount
                                                                total))))))
     categories values)
    (gnc:html-table-append-row/markup!
     table "total-row"
     (map (lambda (cell)
            (for-each
             (lambda (tag)
               (gnc:html-table-cell-set-style!
                cell tag 'attribute '("style" "font-weight: bold;")))
             '("td" "number-cell" "number-cell-neg"))
            cell)
          (list (gnc:make-html-table-cell
                 (if incomplete? (G_ "Priced Total") (G_ "Total")))
                (number-cell (money total))
                (number-cell (if incomplete? "" (percentage total
                                                            total))))))))

(define (allocation-value-point date collectors currency price-source
                                transaction-price-fn)
  (let* ((unpriced '())
         (price (gnc:case-price-fn price-source currency date))
         ;; Balance conversion rounds to currency precision. Resolve the unit
         ;; rate instead, so a valid sub-cent value is neither lost nor
         ;; unpriced.
         (convert (lambda (foreign domestic)
                    (gnc:make-gnc-monetary
                     domestic
                     (* (gnc:gnc-monetary-amount foreign)
                        (price (gnc:gnc-monetary-commodity foreign))))))
         (exchange-fn
          (lambda (foreign domestic)
            (let ((converted (convert foreign domestic)))
              (if (or (zero? (gnc:gnc-monetary-amount foreign))
                      (not (zero? (gnc:gnc-monetary-amount converted))))
                  converted
                  (let* ((commodity (gnc:gnc-monetary-commodity foreign))
                         (fallback
                          (and transaction-price-fn
                               (not (gnc-commodity-is-currency commodity))
                               (transaction-price-fn foreign domestic date
                                                     convert))))
                    (if fallback
                        fallback
                        (begin
                          (set! unpriced
                                (lset-adjoin equal? unpriced commodity))
                          converted)))))))
         (values
          (map (lambda (collector)
                 (apply +
                        (collector
                         'format
                         (lambda (commodity amount)
                           (gnc:gnc-monetary-amount
                            (exchange-fn
                             (gnc:make-gnc-monetary commodity amount)
                             currency)))
                         #f)))
               collectors)))

    (make-allocation-point
     date values unpriced
     (and (null? unpriced) (not (any negative? values))))))

(define (allocation-pricing-warnings message points history?)
  (let ((unpriced-points
         (filter (lambda (point) (pair? (point-unpriced point))) points)))
    (when (pair? unpriced-points)
      (let ((details (allocation-price-details unpriced-points
                                               point-unpriced)))
        (message
         (if history?
             (format #f (G_ "Some holdings have no usable nonzero price or \
exchange rate: ~{~a~^; ~}. Incomplete dates appear as gaps in the chart. \
Value columns include priced holdings only; percentages for incomplete \
dates are omitted.") details)
             (format #f (G_ "Some holdings have no usable nonzero price or \
exchange rate: ~{~a~^; ~}. The chart is omitted. Value columns include \
priced holdings only; percentages for incomplete dates are omitted.")
                     details)))))))

(define (allocation-target-warning categories targets)
  (if (every number? targets)
      (let* ((total (apply + targets))
             (display-total (if (< (abs (- total 100)) 0.005)
                                (format #f "~,6f" total)
                                (format #f "~,2f" total))))
        (format #f (G_ "Allocation targets total ~a%, but must total 100%. \
Update Target % in Categories. The drift chart is omitted; allocation \
values remain available in the table.")
                (string-trim-right
                 (string-trim-right display-total #\0) #\.)))
      (gnc:make-html-text
       (gnc:html-markup-p (G_ "Allocation targets are missing for these \
categories:"))
       (gnc:html-markup-ul
        (filter-map (lambda (category target)
                      (and (not (number? target))
                           (gnc:html-string-sanitize (car category))))
                    categories targets))
       (gnc:html-markup-p
        (G_ "Update Target % in Categories. The drift chart is omitted; \
allocation values remain available in the table.")))))

(define (allocation-valid-targets? targets)
  (and (every number? targets)
       (< (abs (- (apply + targets) 100)) 0.000001)))

(define (allocation-chart-warning categories points targets history? drifts)
  (let ((chart-points (filter point-chartable? points))
        (unpriced? (any (lambda (point) (pair? (point-unpriced point)))
                        points))
        (negative-values?
         (any (lambda (point) (any negative? (point-amounts point))) points)))
    (cond
     ((and drifts (not (allocation-valid-targets? targets)))
      (allocation-target-warning categories targets))
     ((and drifts (not (any number? (concatenate drifts))))
      (G_ "No dates have complete, nonnegative values and a positive total. \
The drift chart is omitted."))
     ((and (not history?) unpriced?) 'unpriced)
     ((and history? (null? chart-points))
      (G_ "No dates have complete, nonnegative category values. The chart \
is omitted."))
     ((and (not history?) negative-values?)
      (G_ "One or more categories have negative values. The chart is \
omitted; signed balances are shown in the table."))
     ((every (lambda (point) (every zero? (point-amounts point)))
             chart-points)
      'empty)
     (else #f))))

(define (allocation-value-points categories dates currency price-source
                                 transaction-price-fn)
  (let* ((category-collectors
          (map (lambda (category index)
                  (let ((collectors
                      (category-collectors-at-dates (cdr category) dates)))
                   (gnc:report-percent-done
                    (* 50 (/ (1+ index) (length categories))))
                   collectors))
               categories (iota (length categories))))
         (points
          (map
           (lambda (date collectors index)
             (let ((point (allocation-value-point
                           date collectors currency price-source
                           transaction-price-fn)))
               (gnc:report-percent-done (+ 50 (* 50 (/ (1+ index) (length
                                                                   dates)))))
               point))
           dates (apply map list category-collectors) (iota (length dates)))))
    points))

;; Attach monetary and fractional tooltip values in the chart's series layout.
;; Drift tooltips use signed differences, not the underlying allocation.
(define (allocation-set-tooltip-data! chart points drifts category-series?)
  (define (series-values rows index)
    (list->vector
     (if category-series?
         (map (lambda (values) (list-ref values index)) rows)
         (car rows))))
  (let* ((percentages
          (if drifts
              (map (lambda (values)
                     (map (lambda (value)
                            (if (number? value) (/ value 100) 'null)) values))
                   drifts)
              (map (lambda (point)
                     (let ((total (apply + (point-amounts point))))
                       (map (lambda (amount)
                              (if (zero? total) 'null (/ amount total)))
                            (point-amounts point)))) points)))
         (amounts
          (if drifts
              (map (lambda (point values)
                     (let ((total (apply + (point-amounts point))))
                       (map (lambda (value)
                              (if (number? value) (* total value) 'null))
                            values))) points percentages)
              (map point-amounts points))))
    (for-each
     (lambda (index)
       (gnc:html-chart-set! chart
                            `(data datasets (,index) allocationAmounts)
                            (series-values amounts index))
       (gnc:html-chart-set! chart
                            `(data datasets (,index) allocationPercentages)
                            (series-values percentages index)))
     (iota (vector-length (gnc:html-chart-get chart '(data datasets)))))))

;; Wrap each chart to install report tooltips and optional snapshot labels.
(define* (make-allocation-chart-object
          options currency categories points
          #:key (history? #f) chart-type (percent? #f) drifts)
  (define (get-option name)
    (gnc-optiondb-lookup-value options pagename-chart name))
  (let ((chart
         (if drifts
             (make-allocation-drift-chart
              categories points drifts category-anchor
              #:history? history? #:percent? percent? #:chart-type chart-type)
             (gnc:make-html-chart))))
    (unless drifts
      (gnc:html-chart-set-type!
       chart (case chart-type
               ((line unstacked-line) 'line)
               ((horizontal-bar grouped-bar stacked-bar bar) 'bar)
               (else chart-type))))
    (gnc:html-chart-set! chart '(options plugins title display) #f)
    (gnc:html-chart-set-width! chart (get-option optname-width))
    (gnc:html-chart-set-height! chart (get-option optname-height))
    (gnc:html-chart-set-currency-iso!
     chart (gnc-commodity-get-mnemonic currency))
    (gnc:html-chart-set-currency-symbol!
     chart (gnc-commodity-get-nice-symbol currency))
    (gnc:html-chart-set! chart '(options allocationCurrency)
               (gnc-commodity-get-mnemonic currency))
    (gnc:html-chart-set! chart '(options allocationTooltip)
               (symbol->string (get-option optname-tooltip)))
    (gnc:html-chart-set! chart '(options allocationHistory) history?)
    (gnc:html-chart-set! chart '(options allocationDrift) (if drifts #t #f))
        (when (and (not history?) (not drifts)
          (memq chart-type '(pie doughnut stacked-bar)))
       (gnc:html-chart-set! chart '(options allocationDataLabels)
              (symbol->string (get-option optname-data-labels))))
    (unless drifts
      (if history?
          (populate-allocation-history-chart!
           chart categories points percent? chart-type currency)
          (populate-allocation-snapshot-chart!
           chart categories points chart-type percent? currency)))
    (when (or history? drifts)
      (allocation-set-tooltip-data!
       chart points drifts (or history? (eq? chart-type 'stacked-bar))))
    (gnc:make-html-object-internal allocation-chart-render chart)))

(define* (allocation-render-data! report-obj document categories dates
                                  #:key (history? #f) chart-type
                                  (percent? #f) (targets '()) (drift? #f))
  (define options (gnc:report-options report-obj))
  (define (get-option section name)
    (gnc-optiondb-lookup-value options section name))
  (define accounts (append-map cdr categories))
  (define currency (get-option gnc:pagename-general optname-currency))
  (define title (get-option gnc:pagename-general gnc:optname-reportname))
  (define (message text)
    (gnc:html-document-add-object!
     document (gnc:make-html-text
               (gnc:html-markup-p (gnc:html-string-sanitize text)))))
  (define (money amount)
    ;; Round in Scheme before crossing the fixed-width gnc_numeric boundary.
    (let ((fraction (gnc-commodity-get-fraction currency)))
      (gnc:make-gnc-monetary
       currency (if (exact? amount)
                    (/ (round (* amount fraction)) fraction)
                    (double-to-gnc-numeric amount fraction
                                           GNC-HOW-RND-ROUND)))))
  (define end-date (last dates))
  (let* ((price-source (get-option gnc:pagename-general optname-price-source))
         (transaction-price-fn
          (and (get-option gnc:pagename-general optname-transaction-prices)
               (make-transaction-price-fn accounts end-date)))
         (points (allocation-value-points
                  categories dates currency price-source
                  transaction-price-fn))
         (drifts (and drift? (map (lambda (point) (allocation-point-drift
                                                   point targets)) points)))
         (warning (allocation-chart-warning categories points targets
                                            history? drifts)))
    (allocation-pricing-warnings message points history?)

    ;; A bad snapshot cannot be charted at all. History retains its complete
    ;; dates, but gaps every series wherever the allocation is incomplete.
    (cond
     ((eq? warning 'unpriced) #f)
     ((eq? warning 'empty)
      (gnc:html-document-add-object!
       document (gnc:html-make-empty-data-warning title (gnc:report-id
                                                         report-obj))))
     (warning
      (if (string? warning)
          (message warning)
          (gnc:html-document-add-object! document warning))
      (when (and drift? (not (allocation-valid-targets? targets)))
        (gnc:html-document-add-object!
         document (gnc:make-html-text
                   (gnc:html-make-options-link (gnc:report-id report-obj))))))
     (else
      (gnc:html-document-add-object!
       document
       (make-allocation-chart-object
        options currency categories points
        #:history? history? #:chart-type chart-type #:percent? percent?
        #:drifts drifts))))

    ;; The table remains useful for signed and partial values even when the
    ;; chart is suppressed. Avoid building it when the user has hidden it.
    (when (get-option gnc:pagename-display optname-show-table)
      (let ((table (if drift?
                       (make-allocation-drift-table categories points
                                                    targets drifts history?
                                                    category-link money)
                       (gnc:make-html-table))))
        (unless drift?
          (if history?
              (populate-allocation-history-table!
               table categories points percent? money)
              (populate-allocation-snapshot-table!
               table categories points money)))
        (gnc:html-document-add-object! document table)))))

;; Render validated (name account ...) groups without expanding descendants.
;; An empty group list stays empty; SUPPLIED-ERROR displays validation failure
;; instead of valuing groups. TARGETS? reads persisted targets for drift mode.
;; Return an HTML document; this function brackets report progress reporting.
(define* (allocation-renderer report-obj categories #:optional supplied-error
                              #:key (targets? #f))
  (define options (gnc:report-options report-obj))
  (define (get-option section name)
    (gnc-optiondb-lookup-value options section name))
  (define document (gnc:make-html-document))
  (define accounts (append-map cdr categories))
  (define time-frame (get-option pagename-chart optname-view))
  (define values-mode (get-option pagename-chart optname-scale))
  (define history?
    (eq? time-frame 'time-series))
  (define drift?
    (and targets? (eq? (get-option pagename-chart optname-measure) 'drift)))
  (define chart-type
    (let ((selected (allocation-select-chart
                     (get-option pagename-chart optname-chart)
                     #:history? history? #:drift? drift?)))
      (gnc-set-option options pagename-chart optname-chart selected)
      (allocation-render-chart-type selected history?)))
  (define targets (if drift? (allocation-category-targets options
                                                          categories) '()))
  (define percent?
    (and (or history? drift? (memq chart-type '(horizontal-bar stacked-bar)))
         (eq? values-mode 'percent)))
  (define (option-date name)
    (gnc:time64-end-day-time
     (gnc:date-option-absolute-time (get-option gnc:pagename-general name))))
  (define end-date (option-date optname-end-date))
  ;; Snapshot must not resolve or validate the unused Start Date option.
  (define start-date (if history? (option-date optname-start-date) end-date))
  (define date-label
    (if history? (gnc-date-interval-format start-date end-date)
        (qof-print-date end-date)))
  (define title (get-option gnc:pagename-general gnc:optname-reportname))
  (define (message text)
    (gnc:html-document-add-object!
     document (gnc:make-html-text
               (gnc:html-markup-p (gnc:html-string-sanitize text)))))
  (gnc:report-starting title)
  (gnc:html-document-set-title!
   document (gnc:html-string-sanitize
             (format #f "~a - ~a" title date-label)))
  (cond
   (supplied-error (message supplied-error))
   ((null? categories)
    (message (G_ "Create categories and select their accounts in the \
Categories options tab.")))
   ((any not accounts)
    (message (G_ "A category refers to an account that no longer exists. \
Update its account selection in Categories.")))
   ((not (= (length accounts) (length (delete-duplicates accounts))))
    (message (G_ "Each account may belong to only one category and may be \
selected only once.")))
   ((> start-date end-date)
    (message (G_ "Start Date must not be after End Date.")))
   (else
    (let ((dates (if history?
                     (map gnc:time64-end-day-time
                          (gnc:make-date-list
                           start-date end-date
                           (gnc:deltasym-to-delta
                            (get-option gnc:pagename-general optname-step))))
                     (list end-date))))
      (allocation-render-data!
       report-obj document categories dates
       #:history? history? #:chart-type chart-type #:percent? percent?
       #:targets targets #:drift? drift?))))
  (gnc:report-finished)
  document)

(define optname-categories (N_ "Categories"))
(define optname-accounts (N_ "Accounts"))
(define optname-uncategorized "Uncategorized Category")

(define (security-account? account)
  (and (memv (xaccAccountGetType account) (list ACCT-TYPE-STOCK
                                                ACCT-TYPE-MUTUAL))
       (not (gnc-commodity-is-currency (xaccAccountGetCommodity account)))))

(define (holding-account? account)
  (or (security-account? account)
      (and (memv (xaccAccountGetType account)
                 (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH
                       ACCT-TYPE-CURRENCY))
           (gnc-commodity-is-currency (xaccAccountGetCommodity account)))))

(define (holding-id account)
  ;; Namespace plus symbol distinguishes securities with identical tickers.
  (gnc-commodity-get-unique-name (xaccAccountGetCommodity account)))

(define (default-accounts)
  (filter holding-account?
          (gnc-account-get-descendants-sorted
           (gnc-get-current-root-account))))

(define (default-category-definitions accounts)
  (list 'explicit-targets
        (append (list (G_ "Uncategorized") 100)
                (sort (delete-duplicates (map holding-id accounts))
                      string<?))))

(define (options-generator)
  (let* ((accounts (default-accounts))
         (options
          (allocation-options-generator
           (lambda (options)
             (gnc-register-security-categories-option
              options pagename-categories optname-categories "a"
              (N_ "Create named categories and select their holdings. Each \
security or currency may belong to only one category. The Accounts tab \
controls which holdings are included.")
              (format #f "~s" (default-category-definitions accounts))))
           (lambda (options)
             (gnc-register-account-list-limited-option
              options gnc:pagename-accounts optname-accounts "a"
              (N_ "Select security and cash accounts to include. Only \
selected accounts are valued. Use Select Children to include descendants. \
Holding categories apply across all selected accounts.") accounts
              (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH
                    ACCT-TYPE-CURRENCY ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL))
             (GncOption-set-always-save
              (gnc-lookup-option
               options gnc:pagename-accounts optname-accounts)
              #t))
           #:targets? #t)))
    ;; Persist the initial fallback key even when it equals the default.
    ;; Existing names and grouping must not depend on the current translation.
    (gnc-register-string-option
     options pagename-categories optname-uncategorized "z" ""
     (G_ "Uncategorized"))
    (let ((key (gnc-lookup-option
                options pagename-categories optname-uncategorized)))
      (GncOption-set-always-save key #t)
      (GncOption-make-internal key))
    options))

(define (group-holdings accounts definitions uncategorized-name)
  ;; These fresh groups may be mutated; the persisted definitions must not be.
  (let* ((groups (map (lambda (category) (list (car category))) definitions))
         (by-holding (make-hash-table))
         (existing-uncategorized (assoc uncategorized-name groups))
         (uncategorized
          (or existing-uncategorized (list uncategorized-name))))
    (for-each
     (lambda (definition group)
      (for-each (lambda (id) (hash-set! by-holding id group))
           (cdr definition)))
     definitions groups)
    ;; Combine a commodity across all selected accounts in a single category.
    (for-each
     (lambda (account)
       (let ((group (hash-ref by-holding (holding-id account) uncategorized)))
         (set-cdr! group (cons account (cdr group)))))
     accounts)
    (let ((result (if (or existing-uncategorized (null? (cdr uncategorized)))
                      groups
                      (append groups (list uncategorized)))))
      (for-each (lambda (group) (set-cdr! group (reverse (cdr group))))
                result)
      result)))

;; Report malformed or obsolete member IDs before constructing account groups.
(define (holding-selection-error accounts definitions)
  (define valid?
    (allocation-valid-category-definitions? definitions
                                            #:allow-empty-members? #t))
  (define members (if valid? (append-map cdr definitions) '()))
  (define commodities (gnc-commodity-table-get-table (gnc-get-current-book)))
  (cond
     ((not valid?)
      (G_ "Each category must have a unique, nonempty name."))
     ((not (= (length members) (length (delete-duplicates members))))
      (G_ "Each holding may belong to only one category and may be selected \
only once."))
     ((any (lambda (id)
             (let ((commodity (gnc-commodity-table-lookup-unique
                               commodities id)))
               ;; SWIG represents a missing commodity as the empty list.
               (or (not commodity) (null? commodity))))
           members)
      (G_ "A category refers to a holding that no longer exists. \
Update its holding selection in Categories."))
     ((null? accounts)
      (G_ "Select security or cash accounts in the Accounts options \
tab. Use Select Children to include the holdings beneath a parent account."))
     (else #f)))

;; Clearing explicit members still leaves the default setup: unassigned
;; holdings fall back to this same category without changing the saved data.
(define (default-allocation-setup? category-data uncategorized-name)
  (let* ((data (allocation-read-category-data
               category-data #:include-targets? #t))
         (category (and (list? data) (pair? data)
                        (eq? (car data) 'explicit-targets)
                        (= (length data) 2) (cadr data))))
    (and category (string=? (car category) uncategorized-name)
         (real? (cadr category)) (= (cadr category) 100))))

(define* (prepend-allocation-setup! report-obj document
          #:key (accounts? #f))
  (define description
    (if accounts?
        (G_ ": Create categories, assign accounts, and set target weights.")
        (G_ ": Create categories, assign holdings, and set target weights.")))
  (let ((url (gnc-build-url
              URL-TYPE-OPTIONS
              (format #f "report-id=~a" (gnc:report-id report-obj))
              pagename-categories)))
    (gnc:html-document-set-objects!
     document
     (cons
      (gnc:make-html-object
       (gnc:make-html-text
        (gnc:html-markup-p
         (gnc:html-markup-anchor url (G_ "Set up your asset allocation"))
        description)))
      (gnc:html-document-objects document)))))

;; Adapt the selected holdings to the shared (name account ...) groups.
(define (renderer report-obj)
  (define options (gnc:report-options report-obj))
  (define (get-option section name)
    (gnc-optiondb-lookup-value options section name))
  (define uncategorized-name
    (get-option pagename-categories optname-uncategorized))
  (define accounts
    (filter holding-account?
            (get-option gnc:pagename-accounts optname-accounts)))
  (define category-data
    (get-option pagename-categories optname-categories))
  (define stored (allocation-read-category-data category-data))
  (define definitions
    (and (list? stored) (pair? stored) (eq? (car stored) 'explicit)
         (cdr stored)))
  (define error-message (holding-selection-error accounts definitions))
  (define categories
    (if error-message '()
        (group-holdings accounts definitions uncategorized-name)))
  (let ((document (allocation-renderer
                   report-obj categories error-message #:targets? #t)))
    (when (and (not error-message)
               (default-allocation-setup? category-data uncategorized-name))
      (prepend-allocation-setup! report-obj document))
    (when (null? accounts)
      (gnc:html-document-add-object!
       document (gnc:make-html-text
                 (gnc:html-make-options-link (gnc:report-id report-obj)
                                             gnc:pagename-accounts))))
    document))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid report-guid
 'menu-path (list gnc:menuname-asset-liability)
 'menu-tip (N_ "Shows asset allocation by grouping securities and \
currencies into categories across selected accounts, with optional targets \
and drift, at a specified date or over time.")
 'options-generator options-generator
 'renderer renderer)