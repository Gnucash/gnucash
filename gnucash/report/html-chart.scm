;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-chart.scm : generate HTML programmatically, with support
;; for simple style elements.
;;
;; Added dependency on guile-json to help construct options. Migrated
;; from obsolete jquery and jqplot to modern chartjs instead in 2018
;; by Christopher Lam
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash json builder))            ;for building JSON options

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; utility functions for nested list handling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 match))

;; nested-alist-set! parameters are
;; lst - a nested alist e.g. (list (cons 'key1 'val1)
;;                                 (cons 'key2 (list (cons 'key2-sub1 'val2a))))
;; path - a list of symbols or single-element list
;;        e.g. '(key2 key2-sub1)
;;             '(key2 key2-sub2 (0))
;;             '(key3 key3-sub1 key3-sub1-sub1)
;; newval - the cdr of the innermost cons cell specified by the path
;;
;; see test-html-chart.scm for usage examples
(define (nested-alist-set! lst path newval)
  (define (path->nested-alist path newval)
    (let loop ((path (reverse path)) (result newval))
      (match path
        (() result)
        ((((? number? idx)) . tail)
         (let ((v (make-vector (1+ idx))))
           (vector-set! v idx result)
           (loop tail v)))
        ((head . tail) (loop tail (list (cons head result)))))))

  (let loop ((nested-lst lst) (path path))
    (define (out-of-bound? n) (and (number? n) (>= n (vector-length nested-lst))))
    (define (existing? n) (and (number? n) (pair? (vector-ref nested-lst n))))
    (if (null? nested-lst) (throw 'invalid-state))
    (match path
      (() (throw 'invalid-state))
      ((((? out-of-bound? idx)) . _) (throw 'index-too-high idx))
      ((((? existing? idx)) . tail) (loop (vector-ref nested-lst idx) tail))
      ((((? number? idx)) . tail) (vector-set! nested-lst idx
                                               (path->nested-alist tail newval)))
      ((head . tail)
       (let ((pair (assq head nested-lst)))
         (cond
          ((not pair) (list-cdr-set! nested-lst (1- (length nested-lst))
                                     (path->nested-alist path newval)))
          ((null? tail) (set-cdr! pair newval))
          (else (loop (cdr pair) tail))))))))

(define (nested-alist-get lst path)
  (let loop ((nested-lst lst) (path path))
    (define (out-of-bound? n) (and (number? n) (>= n (vector-length nested-lst))))
    (match path
      (() nested-lst)
      ((((? out-of-bound? idx)) . _) (throw 'index-too-high idx))
      ((((? number? idx)) . tail) (loop (vector-ref nested-lst idx) tail))
      ((head . tail)
       (let ((pair (assq head nested-lst)))
         (if pair
             (loop (cdr pair) tail)
             (throw 'invalid-path path)))))))

;; helper for setting data - guile-json expects vectors to be
;; transformed into JSON arrays; convert list to vector. if not list,
;; leave alone.
(define (list-to-vec v)
  (if (list? v)
      (list->vector v)
      v))

;; The html-chart specifies options and data to create a chart.  The
;; old chart toolkits guppi and jqplot seemed to require restrictive
;; specific formats for the options and data, and modern toolkit
;; chartjs is more permissive. Nonetheless some of the restrictions
;; remain.

;; At minimum the html-chart will require setting the following
;; fields:
;; type - one of 'bar 'line 'pie
;; width - pair
;; height - pair

(define <html-chart>
  (make-record-type "<html-chart>"
                    '(width
                      height
                      chart-options
                      currency-iso
                      currency-symbol
                      custom-x-axis-ticks?
                      custom-y-axis-ticks?)))

(define gnc:html-chart?
  (record-predicate <html-chart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-chart> class
;;  generate the <object> form for an html chart.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-chart-internal
  (record-constructor <html-chart>))

(define (gnc:make-html-chart)
  (gnc:make-html-chart-internal
   '(percent . 100)  ;;width
   '(percent . 100)  ;;height
   (list             ;;chartjs options object
    (cons 'type 'bar)
    (cons 'data (list
                 (cons 'labels #())
                 (cons 'datasets #())))
    (cons 'options (list
                    (cons 'maintainAspectRatio #f)
                    (cons 'chartArea (list
                                      (cons 'backgroundColor "#fffdf6")))
                    (cons 'legend (list
                                   (cons 'position 'right)
                                   (cons 'reverse #f)
                                   (cons 'labels (list
                                                  (cons 'fontColor 'black)))))

                    (cons 'elements (list
                                     (cons 'line (list
                                                  (cons 'tension 0)))
                                     (cons 'point (list
                                                   (cons 'pointStyle #f)))))
                    (cons 'tooltips (list
                                     (cons 'callbacks (list
                                                       (cons 'label #f)))))

                    (cons 'scales (list
                                   (cons 'xAxes (vector
                                                 (list
                                                  (cons 'display #t)
                                                  (cons 'type 'category)
                                                  (cons 'distribution 'series)
                                                  (cons 'offset #t)
                                                  (cons 'gridLines (list
                                                                    (cons 'display #t)
                                                                    (cons 'lineWidth 1.5)))
                                                  (cons 'scaleLabel (list
                                                                     (cons 'display #t)
                                                                     (cons 'labelString "")))
                                                  (cons 'ticks (list
                                                                (cons 'fontSize 12)
                                                                (cons 'maxRotation 30))))
                                                 ;; the following another xAxis at the top
                                                 '((position . top)
                                                   (ticks . ((display . #f)))
                                                   (gridLines . ((display . #f)
                                                                 (drawTicks . #f))))
                                                 ))
                                   (cons 'yAxes (vector
                                                 (list
                                                  (cons 'stacked #f)
                                                  (cons 'display #t)
                                                  (cons 'gridLines (list
                                                                    (cons 'display #t)
                                                                    (cons 'lineWidth 1.5)))
                                                  (cons 'scaleLabel (list
                                                                     (cons 'display 1.5)
                                                                     (cons 'labelString "")))
                                                  (cons 'ticks (list
                                                                (cons 'fontSize 10)
                                                                (cons 'beginAtZero #t))))
                                                 ;; the following another yAxis on the right
                                                 '((position . right)
                                                   (ticks . ((display . #f)))
                                                   (gridLines . ((display . #f)
                                                                 (drawTicks . #f))))
                                                 ))))
                    (cons 'title (list
                                  (cons 'display #t)
                                  (cons 'fontSize 16)
                                  (cons 'fontStyle "")
                                  (cons 'text ""))))))
   "XXX"     ;currency-iso
   "\u00A4"  ;currency-symbol
   #t        ;custom x-axis ticks?
   #t        ;custom y-axis ticks?
   ))

(define gnc:html-chart-width
  (record-accessor <html-chart> 'width))

(define gnc:html-chart-set-width!
  (record-modifier <html-chart> 'width))

(define gnc:html-chart-height
  (record-accessor <html-chart> 'height))

(define gnc:html-chart-set-height!
  (record-modifier <html-chart> 'height))

(define (gnc:html-chart-type chart)
  (gnc:html-chart-get chart '(type)))

(define (gnc:html-chart-set-type! chart type)
  (gnc:html-chart-set! chart '(type) type))

(define (gnc:html-chart-title chart)
  (gnc:html-chart-get chart '(options title text)))

(define-public (gnc:html-chart-set-title! chart title)
  (gnc:html-chart-set! chart '(options title text) title))

(define-public (gnc:html-chart-set-data-labels! chart labels)
  (gnc:html-chart-set! chart '(data labels) labels))

(define-public (gnc:html-chart-set-axes-display! chart display?)
  (gnc:html-chart-set! chart '(options scales xAxes (0) display) display?)
  (gnc:html-chart-set! chart '(options scales yAxes (0) display) display?))

(export gnc:html-chart-add-data-series!)
;; e.g.:
;; (gnc:html-chart-add-data-series! chart "label" list-of-numbers color
;;  'fill #t
;;  'urls "gnc-report:id=13#")
;;
;; chart - html-chart object
;; "label" - data series label eg. "Income"
;; data    - a list-of-numbers specifying series data
;; color   - a string specifying series colour
;;
;; other keys may be specified, see ChartJS documentation
;; 'fill        - fill bar, or fill under line?
;; 'borderWidth - line width
;; 'urls        - either a string (same url for whole data series)
;;                or a list-of-string (individual url for each data point)
;;                see javascript onClick event handler how they are decoded
(define* (gnc:html-chart-add-data-series! chart label data color . rest)
  (let loop ((rest rest)
             (newseries (list
                         (cons 'data (list-to-vec data))
                         (cons 'label label)
                         (cons 'backgroundColor (list-to-vec color))
                         (cons 'borderColor (list-to-vec color)))))
    (match rest
      (() (gnc:html-chart-set!
           chart '(data datasets)
           (list->vector
            (append (vector->list
                     (or (gnc:html-chart-get chart '(data datasets)) #()))
                    (list newseries)))))
      ((key val . rest) (loop rest (assq-set! newseries key (list-to-vec val)))))))

(define-public (gnc:html-chart-clear-data-series! chart)
  (gnc:html-chart-set! chart '(data datasets) #()))

(define-public (gnc:html-chart-set-x-axis-label! chart label)
  (gnc:html-chart-set! chart '(options scales xAxes (0) scaleLabel labelString) label))

(define-public (gnc:html-chart-set-stacking?! chart stack?)
  (gnc:html-chart-set! chart '(options scales xAxes (0) stacked) stack?)
  (gnc:html-chart-set! chart '(options scales yAxes (0) stacked) stack?))

(define-public (gnc:html-chart-set-grid?! chart grid?)
  (gnc:html-chart-set! chart '(options scales xAxes (0) gridLines display) grid?)
  (gnc:html-chart-set! chart '(options scales yAxes (0) gridLines display) grid?))

(define-public (gnc:html-chart-set-y-axis-label! chart label)
  (gnc:html-chart-set! chart '(options scales yAxes (0) scaleLabel labelString) label))

(define gnc:html-chart-get-options-internal
  (record-accessor <html-chart> 'chart-options))

(define gnc:html-chart-set-options-internal!
  (record-modifier <html-chart> 'chart-options))

(define (gnc:html-chart-get chart path)
  (let ((options (gnc:html-chart-get-options-internal chart)))
    (nested-alist-get options path)))

(define (gnc:html-chart-set! chart path val)
  (let ((options (gnc:html-chart-get-options-internal chart))
        (val-vec (list-to-vec val)))
    (nested-alist-set! options path val-vec)
    (gnc:html-chart-set-options-internal! chart options)))

(define gnc:html-chart-currency-iso
  (record-accessor <html-chart> 'currency-iso))

(define gnc:html-chart-set-currency-iso!
  (record-modifier <html-chart> 'currency-iso))

(define gnc:html-chart-currency-symbol
  (record-accessor <html-chart> 'currency-symbol))

(define gnc:html-chart-set-currency-symbol!
  (record-modifier <html-chart> 'currency-symbol))

(define gnc:html-chart-custom-x-axis-ticks?
  (record-accessor <html-chart> 'custom-x-axis-ticks?))

(define-public gnc:html-chart-set-custom-x-axis-ticks?!
  (record-modifier <html-chart> 'custom-x-axis-ticks?))

(define gnc:html-chart-custom-y-axis-ticks?
  (record-accessor <html-chart> 'custom-y-axis-ticks?))

(define-public gnc:html-chart-set-custom-y-axis-ticks?!
  (record-modifier <html-chart> 'custom-y-axis-ticks?))

(define JS-Number-to-String "
// The following snippet from MDN
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toLocaleString
var toLocaleStringSupportsOptions = (typeof Intl == 'object' && Intl && typeof Intl.NumberFormat == 'function');

// format a number e.g. 2.5 into monetary e.g. \"$2.50\"
function numformat(amount) {
  if (toLocaleStringSupportsOptions) {
      return amount.toLocaleString(undefined, {style:'currency', currency:curriso});
  } else {
      return currsym + amount.toLocaleString();
  }
}
")


(define JS-setup "
function tooltipLabel(tooltipItem,data) {
  var datasetLabel = data.datasets[tooltipItem.datasetIndex].label || 'Other';
  var label = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
  switch (typeof(label)) {
    case 'number':
      return datasetLabel + ': ' + numformat(label);
    default:
      return '';
  }
}

function tooltipTitle(array,data) {
  return chartjsoptions.data.labels[array[0].index]; }

// draw the background color
Chart.pluginService.register({
  beforeDraw: function (chart, easing) {
    if (chart.config.options.chartArea && chart.config.options.chartArea.backgroundColor) {
      var ctx = chart.chart.ctx;
      var chartArea = chart.chartArea;
      ctx.save();
      ctx.fillStyle = chart.config.options.chartArea.backgroundColor;
      ctx.fillRect(chartArea.left, chartArea.top, chartArea.right - chartArea.left, chartArea.bottom - chartArea.top);
      ctx.restore();
    }
  }
})

document.getElementById(chartid).onclick = function(evt) {
  var activepoints = myChart.getElementAtEvent(evt);
  var anchor = document.getElementById(jumpid);
  switch (activepoints.length)  {
    case 0:
      anchor.href = '';
      anchor.textContent = '';
      anchor.style = 'display: none';
      break;
    default:
      var index = activepoints[0]['_index'];
      var datasetIndex = activepoints[0]['_datasetIndex'];
      var datasetURLs = myChart.data.datasets[datasetIndex].urls;
      // console.log('index=',index,'datasetIndex=',datasetIndex);
      anchor.style = 'position:absolute; top:' + (evt.clientY - 30) + 'px; left:' + (evt.clientX - 20) + 'px; display: block; padding: 5px; border-radius: 5px; background: #4E9CAF; text-align:center; color:white; z-index: 999;';
      switch (typeof(datasetURLs)) {
        case 'string':
          anchor.href = datasetURLs;
          anchor.textContent = loadstring;
          break;
        case 'object':
          anchor.href = datasetURLs[index];
          anchor.textContent = loadstring;
          break;
        default:
          anchor.href = '';
          anchor.textContent = '';
          anchor.style = 'display: none';
      }
  }
}\n\n")

(define (get-options-string chart)
  (scm->json-string
   (gnc:html-chart-get-options-internal chart)
   #:pretty #t))

(define (gnc:html-chart-render chart doc)

  (define (size->str size)
    (string-append
     (number->string (cdr size))
     (case (car size)
       ((pixels) "px")
       ((percent) "%"))))

  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         ;; Use a unique chart-id for each chart. This prevents charts
         ;; clashing on multi-column reports
         (id (guid-new-return)))

    (push (gnc:html-js-include
           (gnc-path-find-localized-html-file "chartjs/Chart.bundle.min.js")))

    (push (format #f "<div style='width:~a;height:~a;'>\n"
                  (size->str (gnc:html-chart-width chart))
                  (size->str (gnc:html-chart-height chart))))
    (push (format #f "<a id='jump-~a' href='' style='display:none'></a>\n" id))
    (push (format #f "<canvas id='chart-~a'></canvas>\n" id))
    (push "</div>\n")
    (push (format #f "<script id='script-~a'>\n" id))
    (push (format #f "var curriso = ~s;\n" (gnc:html-chart-currency-iso chart)))
    (push (format #f "var currsym = ~s;\n" (gnc:html-chart-currency-symbol chart)))
    (push (format #f "var chartid = 'chart-~a';\n" id))
    (push (format #f "var jumpid = 'jump-~a';\n" id))
    (push (format #f "var loadstring = ~s;\n" (_ "Load")))
    (push (format #f "var chartjsoptions = ~a;\n\n"
                  (get-options-string chart)))

    (push JS-Number-to-String)
    (when (gnc:html-chart-custom-y-axis-ticks? chart)
      (push "chartjsoptions.options.scales.yAxes[0].ticks.callback = yAxisDisplay;\n")
      (push "function yAxisDisplay(value,index,values) { return numformat(value); };\n"))
    (when (gnc:html-chart-custom-x-axis-ticks? chart)
      (push "chartjsoptions.options.scales.xAxes[0].ticks.callback = xAxisDisplay;\n")
      (push "function xAxisDisplay(value,index,values) { return chartjsoptions.data.labels[index]; };\n"))

    (push "chartjsoptions.options.tooltips.callbacks.label = tooltipLabel;\n")
    (push "chartjsoptions.options.tooltips.callbacks.title = tooltipTitle;\n")
    (push "Chart.defaults.global.defaultFontFamily = \"'Trebuchet MS', Arial, Helvetica, sans-serif\";\n")
    (push JS-setup)

    (push "var myChart = new Chart(chartid, chartjsoptions);\n")
    (push "</script>")

    retval))
