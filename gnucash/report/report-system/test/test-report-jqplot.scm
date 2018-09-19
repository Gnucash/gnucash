(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash core-utils))
	
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
;;(use-modules (ice-9 regex)) ;; for regexp-substitute/global
(use-modules (srfi srfi-1)) ;; make the (drop lst i) command available

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-jqplot") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                        ;; will create Testing/Temporary/test-report-jqplot.log
    ;;(test-assert "Jqplot - Availability" (test-check1))
    ;; in order to run this test successfully
    ;; set GNC_DOC_PATH and make it point to $PREFIX/share/gnucash
    ;; e.g.:
    ;; GNC_DOC_PATH="/opt/gnucash-cmake/share/gnucash"
    ;; export GNC_DOC_PATH

    ;; the following tests are independent of GNC_DOC_PATH
    (test-bar-chart-zero-detection)
    (test-line-chart-zero-detection)
    (test-bar-chart-data)
    (test-line-chart-data)
    (test-bar-chart-rendering)
    (test-line-chart-rendering)
    (test-pie-chart-rendering)
    (test-scatter-chart-rendering)
    (test-end "Testing/Temporary/test-report-jqplot")
)

   ;; html bar charts and line charts
   ;;
   ;; The bar chart and line chart data is modelled in a table matrix.
   ;; The rows give the dates for the data.
   ;; The colums give the accounts from which the data is fetched.
   ;;
   ;; Example:
   ;;               Acct 1    Acct 2    Acct 3
   ;; 1990-01-01      1.00      4.00      7.00
   ;; 1990-02-01      2.00      5.00      8.00
   ;; 1990-03-01      3.00      6.00      9.00
   ;;
   ;; The data is coded is a list of rows, each row is a list
   ;; of columns and stored in the data field of the
   ;; html-barchart/linechart record.
   ;;
   ;; The following list keeps the date from the example above:
   ;;
   ;; (
   ;;   (1.00 4.00 7.00)
   ;;   (2.00 5.00 8.00)
   ;;   (3.00 6.00 9.00)
   ;; )
   ;;
   ;; The row labels (here: 1990-01-01 ... 1990-03-01) and the
   ;; column labels (here: Acct 1 ... Acct 3) are stored as separate
   ;; lists in the fields row-labels and col-labels.


;; -----------------------------------------------------------------------

(define (test-check1)

   ;; Test jqplot availability
   ;;
   ;; The HTML charts in the GnuCash reports use jqplot libraries.
   ;; The location of the jqplot libraries needs to be put into
   ;; the report HTML pages.
   ;;
   ;; Example: 
   ;; <script language="javascript" type="text/javascript" src="file:///<path>/<jqplot-library-name>"></script>
   ;;
   ;; This html-string will be returned by (gnc:html-js-include "jqplot-library-name"),
   ;; which is using a call to gnc-path-find-localized-html-file
   ;; to resolve <path>.
   ;;
   ;; If no path is found, a string without path and library name
   ;; is returned:
   ;; <script language="javascript" type="text/javascript" src="file:///"></script>
   ;;
   ;; This means, in the failure case always the same string is
   ;; returned, and in the success case always a longer string is
   ;; return compared to the failure case.
   ;;
   ;; This test first evaluates the string length of the failure
   ;; case, stores the result, and uses this value for comparison
   ;; for the target test calls to gnc:html-js-include.
   ;;
   ;; If the return string is longer than the failure case string
   ;; the test is passed.
   ;;
   ;; Note:
   ;; This test evaluates the success of the path resolution.
   ;; It does not evaluate the correctness of the path resolution.

  (let (
         (sl (string-length (gnc:html-js-include "invalid-file-name")))
         (slc (string-length (gnc:html-css-include "invalid-file-name")))
       )
    (and 
      ;;(= sl 78)
      (< sl (string-length (gnc:html-js-include "jqplot/jquery.min.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jquery.jqplot.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.barRenderer.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.cursor.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.categoryAxisRenderer.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.highlighter.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.canvasTextRenderer.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.canvasAxisTickRenderer.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.dateAxisRenderer.js")))
      (< sl (string-length (gnc:html-js-include "jqplot/jqplot.pieRenderer.js")))
      ;;(= slc 58)
      (< slc (string-length (gnc:html-css-include "jqplot/jquery.jqplot.css")))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-bar-chart-zero-detection)

  (test-begin "Jqplot - HTML Bar Chart zero-data detection")

  (let (
         (html-bar-chart (gnc:make-html-barchart))
       )
    ;; no data, no rendering - check that zero values are detected
    (gnc:html-barchart-set-data! html-bar-chart '())
    (test-assert "HTML Bar Chart - zero detection 1" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))
    (gnc:html-barchart-set-data! html-bar-chart '(() () ()))
    (test-assert "HTML Bar Chart - zero detection 2" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))
    (gnc:html-barchart-set-data! html-bar-chart 0)
    (test-assert "HTML Bar Chart - zero detection 3" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))
    (gnc:html-barchart-set-data! html-bar-chart 0.00)
    (test-assert "HTML Bar Chart - zero detection 4" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))
    (gnc:html-barchart-set-data! html-bar-chart '((0) (0) (0)))
    (test-assert "HTML Bar Chart - zero detection 5" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))
    (gnc:html-barchart-set-data! html-bar-chart '((0.00) (0.00) (0.00)))
    (test-assert "HTML Bar Chart - zero detection 6" (not (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart))))

    ;; check that non-zero values are detected
    (gnc:html-barchart-set-data! html-bar-chart 2)
    (test-assert "HTML Bar Chart - zero detection 7" (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart)))
    (gnc:html-barchart-set-data! html-bar-chart 2.00)
    (test-assert "HTML Bar Chart - zero detection 8" (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart)))
    (gnc:html-barchart-set-data! html-bar-chart '((0) (2) (0)))
    (test-assert "HTML Bar Chart - zero detection 9" (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart)))
    (gnc:html-barchart-set-data! html-bar-chart '((0.00) (0.00) (2.00)))
    (test-assert "HTML Bar Chart - zero detection 10" (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart)))
    (gnc:html-barchart-set-data! html-bar-chart '(() () (3)))
    (test-assert "HTML Bar Chart - zero detection 11" (gnc:not-all-zeros (gnc:html-barchart-data html-bar-chart)))
  )

  (test-end "Jqplot - HTML Bar Chart zero-data detection")
)

;; -----------------------------------------------------------------------

(define (test-line-chart-zero-detection)

  (test-begin "Jqplot - HTML Line Chart zero-data detection")

  (let (
         (html-line-chart (gnc:make-html-linechart))
       )
    ;; no data, no rendering - check that zero values are detected
    (gnc:html-linechart-set-data! html-line-chart '())
    (test-assert "HTML Line Chart - zero detection 1" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (gnc:html-linechart-set-data! html-line-chart '(() () ()))
    (test-assert "HTML Line Chart - zero detection 2" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (gnc:html-linechart-set-data! html-line-chart 0)
    (test-assert "HTML Line Chart - zero detection 3" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (gnc:html-linechart-set-data! html-line-chart 0.00)
    (test-assert "HTML Line Chart - zero detection 4" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (gnc:html-linechart-set-data! html-line-chart '((0) (0) (0)))
    (test-assert "HTML Line Chart - zero detection 5" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (gnc:html-linechart-set-data! html-line-chart '((0.00) (0.00) (0.00)))
    (test-assert "HTML Line Chart - zero detection 6" (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart))))
    (not (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
    ;; check that non-zero values are detected
    (gnc:html-linechart-set-data! html-line-chart 2)
    (test-assert "HTML Line Chart - zero detection 7" (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
    (gnc:html-linechart-set-data! html-line-chart 2.00)
    (test-assert "HTML Line Chart - zero detection 8" (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
    (gnc:html-linechart-set-data! html-line-chart '((0) (2) (0)))
    (test-assert "HTML Line Chart - zero detection 9" (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
    (gnc:html-linechart-set-data! html-line-chart '((0.00) (0.00) (2.00)))
    (test-assert "HTML Line Chart - zero detection 10" (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
    (gnc:html-linechart-set-data! html-line-chart '(() () (3)))
    (test-assert "HTML Line Chart - zero detection 11" (gnc:not-all-zeros (gnc:html-linechart-data html-line-chart)))
  )

  (test-end "Jqplot - HTML Line Chart zero-data detection")
)

;; -----------------------------------------------------------------------

(define (test-bar-chart-data)

  (test-begin "Jqplot - HTML Bar Chart Data")

  (let (
         (html-bar-chart (gnc:make-html-barchart))
       )
    ;; prepare test data
    (gnc:html-barchart-set-data! html-bar-chart '((5.00)))
    (gnc:html-barchart-append-row! html-bar-chart '(6.00))
    (gnc:html-barchart-prepend-row! html-bar-chart '(4.00))
    (gnc:html-barchart-append-column! html-bar-chart '(7.00 8.00 9.00))
    (gnc:html-barchart-prepend-column! html-bar-chart '(1.00 2.00 3.00))
    ;; check test data
    (test-equal "HTML Bar Chart - data row 0"
      '(1.0 4.0 7.0)
      (car (gnc:html-barchart-data html-bar-chart)))
    (test-equal "HTML Bar Chart - data row 1"
      '(2.0 5.0 8.0)
      (cadr (gnc:html-barchart-data html-bar-chart)))
    (test-equal "HTML Bar Chart - data row 2"
      '(3.0 6.0 9.0)
      (caddr (gnc:html-barchart-data html-bar-chart)))
  )

  (test-end "Jqplot - HTML Bar Chart Data")
)

;; -----------------------------------------------------------------------

(define (test-line-chart-data)

  (test-begin "Jqplot - HTML Line Chart Data")

  (let (
         (html-line-chart (gnc:make-html-linechart))
       )
    ;; prepare test data
    (gnc:html-linechart-set-data! html-line-chart '((5.00)))
    (gnc:html-linechart-append-row! html-line-chart '(6.00))
    (gnc:html-linechart-prepend-row! html-line-chart '(4.00))
    (gnc:html-linechart-append-column! html-line-chart '(7.00 8.00 9.00))
    (gnc:html-linechart-prepend-column! html-line-chart '(1.00 2.00 3.00))
    ;; check test data
    (test-equal "HTML Line Chart - data row 0"
      '(1.0 4.0 7.0)
      (car (gnc:html-linechart-data html-line-chart)))
    (test-equal "HTML Line Chart - data row 1"
      '(2.0 5.0 8.0)
      (cadr (gnc:html-linechart-data html-line-chart)))
    (test-equal "HTML Line Chart - data row 2"
      '(3.0 6.0 9.0)
      (caddr (gnc:html-linechart-data html-line-chart)))
  )

  (test-begin "Jqplot - HTML Line Chart Data")
)

;; -----------------------------------------------------------------------

(define (test-bar-chart-rendering)

  (test-begin "Jqplot - HTML Bar Chart Rendering")

  (let (
         (html-bar-chart (gnc:make-html-barchart))
       )
    ;; prepare test data
    (gnc:html-barchart-set-width! html-bar-chart (cons 'pixels 800))
    (gnc:html-barchart-set-height! html-bar-chart (cons 'pixels 400))
    (gnc:html-barchart-set-title! html-bar-chart "Test bar Chart")
    (gnc:html-barchart-set-subtitle! html-bar-chart "Created 2018-08-02")
    (gnc:html-barchart-set-x-axis-label! html-bar-chart "Test x-Axis Label barchart")
    (gnc:html-barchart-set-y-axis-label! html-bar-chart "Test y-Axis Label barchart")
    (gnc:html-barchart-set-col-labels! html-bar-chart '("Col1" "Col2" "Col3"))
    (gnc:html-barchart-set-row-labels! html-bar-chart '("Row1" "Row2" "Row3"))
    (gnc:html-barchart-set-data! html-bar-chart '((5.00)))

    ;; do the test
    (test-equal "Jqplot - Render Bar Chart"
      (string-concatenate (drop (gnc:html-document-tree-collapse (gnc:html-barchart-render html-bar-chart #f)) 11))
"\" style=\"width:800px;height:400px;\"></div>\n\
<script id=\"source\">\n\
$(function () {var data = [];var series = [];\n\
var d0 = [];\n\
  d0.push([1, 5.0]);\n\
data.push(d0);\n\
series.push({ label: \"Col1\" });\n\
\n\
var all_ticks = [\"Row1\",\"Row2\",\"Row3\",];\n\
var options = {\n\
                   shadowAlpha: 0.07,\n\
                   stackSeries: false,\n\
                   legend: {\n\
                        show: true,\n\
                        placement: \"outsideGrid\", },\n\
                   seriesDefaults: {\n\
                        renderer: $.jqplot.BarRenderer,\n\
                        rendererOptions: {\n\
                            shadowAlpha: 0.04,\n\
                            shadowDepth: 3,\n\
                        },\n\
                        fillToZero: true,\n\
                   },\n\
                   series: series,\n\
                   axesDefaults: {\n\
                   },\n\
                   grid: {\n\
                   },\n\
                   axes: {\n\
                       xaxis: {\n\
                           renderer:$.jqplot.CategoryAxisRenderer,\n\
                           tickRenderer: $.jqplot.CanvasAxisTickRenderer,\n\
                           tickOptions: {\n\
                               angle: -30,\n\
                               fontSize: '10pt',\n\
                           },\n\
                       },\n\
                       yaxis: {\n\
                           autoscale: true,\n\
                       },\n\
                   },\n\
                   highlighter: {\n\
                       tooltipContentEditor: formatTooltip,\n\
                   },\n\
                   cursor:{\n\
                       show: true,\n\
                       showTooltip: false,\n\
                       zoom: true,\n\
                   },\n\
                   seriesColors: false,\n\
                };\n\
  options.stackSeries = false;\n\
  options.title = \"Test bar Chart\";\n\
  options.title += ' <br />' + \"Created 2018-08-02\";\n\
  options.axes.xaxis.label = \"Test x-Axis Label barchart\";\n\
  options.axes.yaxis.label = \"Test y-Axis Label barchart\";\n\
  options.axes.xaxis.ticks = all_ticks;\n\
$.jqplot.config.enablePlugins = true;\n\
$(document).ready(function() {\n\
var plot = $.jqplot('chart-968125', data, options);\n\
plot.axes.xaxis.ticks = getVisualTicks();\n\
plot.replot();\n\
var timer;\n\
var load_timer;\n\
\n\
// var win_width = $(window).width();\n\
// var win_height = $(window).height();\n\
// console.log( 'Window Width ' + win_width + ' Height ' + win_height);\n\
\n\
// var doc_width = document.body.clientWidth;\n\
// var doc_height = document.body.clientHeight;\n\
// console.log( 'Doc Width ' + doc_width + ' Height ' + doc_height);\n\
\n\
$(window).resize(function () {\n\
    clearTimeout(timer);\n\
    timer = setTimeout(function () {\n\
        plot.replot({resetAxes: true });\n\
        $.each(plot.series, function(index, series) {\n\
            series.barWidth = undefined;\n\
        });\n\
        plot.axes.xaxis.ticks = getVisualTicks();\n\
//        console.log( 'Resize Timer!' );\n\
        plot.replot();\n\
    }, 100);\n\
    });\n\
\n\
$(window).on('load', function () {\n\
    var hasVScroll = document.body.scrollHeight > document.body.clientHeight;\n\
    clearTimeout(load_timer);\n\
    load_timer = setTimeout(function () {\n\
//        console.log( 'Load Timer!' );\n\
        if(hasVScroll)\n\
        {\n\
//            console.log( 'Load Timer Replot!' );\n\
            plot.replot();\n\
        }\n\
    },100);\n\
    });\n\
});\n\
\n\
function formatTooltip(str, seriesIndex, pointIndex) {\n\
    if (options.axes.xaxis.ticks[pointIndex] !== undefined)\n\
        x = options.axes.xaxis.ticks[pointIndex];\n\
    else\n\
        x = pointIndex;\n\
    y = data[seriesIndex][pointIndex][1].toFixed(2);\n\
    return options.series[seriesIndex].label + '<br/>' + x + '<br/><b>' + y + '</b>';\n\
}\n\
\n\
function getVisualTicks() {\n\
    var chart_width = document.getElementById(\"chart-968125\").getElementsByClassName(\"jqplot-zoom-canvas\")[0].width;\n\
    var num_ticks = all_ticks.length;\n\
    var label_width = 25;\n\
    var num_labels = chart_width / label_width;\n\
    var show_every_nth_label = Math.ceil (num_ticks / num_labels);\n\
    var visual_ticks = [];\n\
\n\
    if (show_every_nth_label == 0)\n\
        show_every_nth_label = 1;\n\
    for (counter = 0; counter < all_ticks.length; counter++) {\n\
        if ((counter % show_every_nth_label) == 0)\n\
            visual_ticks.push (all_ticks[counter]);\n\
        else\n\
            visual_ticks.push (' ');\n\
    }\n\
//    console.log( 'getVis chart_width ' + chart_width );\n\
    return visual_ticks;\n\
}\n\
});\n\
</script>")
  )
  (test-end "Jqplot - HTML Bar Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-line-chart-rendering)

  (test-end "Jqplot - HTML Line Chart Rendering")

  (let (
         (html-line-chart (gnc:make-html-linechart))
       )
    ;; prepare test data
    (gnc:html-linechart-set-width! html-line-chart (cons 'pixels 800))
    (gnc:html-linechart-set-height! html-line-chart (cons 'pixels 400))
    (gnc:html-linechart-set-title! html-line-chart "Test line Chart")
    (gnc:html-linechart-set-subtitle! html-line-chart "Created 2018-08-02")
    (gnc:html-linechart-set-x-axis-label! html-line-chart "Test x-Axis Label linechart")
    (gnc:html-linechart-set-y-axis-label! html-line-chart "Test y-Axis Label linechart")
    (gnc:html-linechart-set-col-labels! html-line-chart '("Col1" "Col2" "Col3"))
    (gnc:html-linechart-set-row-labels! html-line-chart '("Row1" "Row2" "Row3"))
    (gnc:html-linechart-set-data! html-line-chart '((5.00)))

    ;; do the test
    (test-equal "Jqplot - Render Line Chart"
      (string-concatenate (drop (gnc:html-document-tree-collapse (gnc:html-linechart-render html-line-chart #f)) 10))
      "\" style=\"width:800px;height:400px;\"></div>\n\
<script id=\"source\">\n\
$(function () {var data = [];var series = [];\n\
var d0 = [];\n\
  d0.push([\"Row1\", 5.0]);\n\
data.push(d0);\n\
series.push({ label: \"Col1\" });\n\
\n\
var options = {\n\
                   shadowAlpha: 0.07,\n\
                   legend: {\n\
                        show: true,\n\
                        placement: \"outsideGrid\", },\n\
                   seriesDefaults: {\n\
                        lineWidth: 1.5,\n\
                        showMarker: true,\n\
                   },\n\
                   series: series,\n\
                   axesDefaults: {\n\
                   },        \n\
                   grid: {\n\
                   },\n\
                   axes: {\n\
                       xaxis: {\n\
                           renderer:$.jqplot.DateAxisRenderer,\n\
                           tickRenderer: $.jqplot.CanvasAxisTickRenderer,\n\
                           tickOptions: {\n\
                               angle: -30,\n\
                               fontSize: '10pt',\n\
                           },\n\
                       },\n\
                       yaxis: {\n\
                           autoscale: true,\n\
                       },\n\
                   },\n\
                   highlighter: {\n\
                       tooltipContentEditor: formatTooltip,\n\
                       tooltipLocation: 'ne',\n\
                   },\n\
                   cursor: {\n\
                       show: true,\n\
                       zoom: true\n\
                   },\n\
                   seriesColors: false,\n\
                };\n\
  options.stackSeries = false;\n\
  options.seriesDefaults.showMarker = true;\n\
  options.axesDefaults.drawMajorGridlines = true;\n\
  options.axesDefaults.drawMinorGridlines = true;\n\
  options.title = \"Test line Chart\";\n\
  options.title += ' <br />' + \"Created 2018-08-02\";\n\
  options.axes.xaxis.label = \"Test x-Axis Label linechart\";\n\
  options.axes.yaxis.label = \"Test y-Axis Label linechart\";\n\
  options.axes.xaxis.tickOptions.formatString = '%m/%d/%y';\n\
$.jqplot.config.enablePlugins = true;\n\
$(document).ready(function() {\n\
var plot = $.jqplot('chart-208691', data, options);\n\
plot.replot();\n\
var timer;\n\
var load_timer;\n\
\n\
// var win_width = $(window).width();\n\
// var win_height = $(window).height();\n\
// console.log( 'Window Width ' + win_width + ' Height ' + win_height);\n\
\n\
// var doc_width = document.body.clientWidth;\n\
// var doc_height = document.body.clientHeight;\n\
// console.log( 'Doc Width ' + doc_width + ' Height ' + doc_height);\n\
\n\
$(window).resize(function () {\n\
    clearTimeout(timer);\n\
    timer = setTimeout(function () {\n\
//        console.log( 'Resize Timer!' );\n\
        plot.replot();\n\
    }, 100);\n\
    });\n\
\n\
$(window).on('load', function () {\n\
    var hasVScroll = document.body.scrollHeight > document.body.clientHeight;\n\
    clearTimeout(load_timer);\n\
    load_timer = setTimeout(function () {\n\
//        console.log( 'Load Timer!' );\n\
        if(hasVScroll)\n\
        {\n\
//            console.log( 'Load Timer Replot!' );\n\
            plot.replot();\n\
        }\n\
    },100);\n\
    });\n\
});\n\
\n\
function formatTooltip(str, seriesIndex, pointIndex) {\n\
    x = $.jqplot.DateTickFormatter (options.axes.xaxis.tickOptions.formatString,\n\
                                    data[seriesIndex][pointIndex][0]);\n\
    y = data[seriesIndex][pointIndex][1].toFixed(2);\n\
    return options.series[seriesIndex].label + ' ' + x + '<br><b>' + y + '</b>';\n\
}\n\
});\n\
</script>")
  )
  (test-end "Jqplot - HTML Line Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-pie-chart-rendering)

  (test-begin "Jqplot - HTML Pie Chart Rendering")
  (let (
         (html-pie-chart (gnc:make-html-piechart))
       )
    ;; prepare test data
    (gnc:html-piechart-set-width! html-pie-chart (cons 'pixels 800))
    (gnc:html-piechart-set-height! html-pie-chart (cons 'pixels 400))
    (gnc:html-piechart-set-title! html-pie-chart "Test Pie Chart")
    (gnc:html-piechart-set-subtitle! html-pie-chart "Created 2017-08-09")
    (gnc:html-piechart-set-data! html-pie-chart '(100 100.5 "100" "100.5" 'any-tag #f))
    (gnc:html-piechart-set-labels! html-pie-chart '("Pie Chart Label 1" "Pie Chart Label 2" "Pie Chart Label 3" "Pie Chart Label 4" "Pie Chart Label 5" "Pie Chart Label 6"))

    ;; do the test
    (test-equal "Jqplot - Render Pie Chart"
      (string-concatenate (drop (gnc:html-document-tree-collapse (gnc:html-piechart-render html-pie-chart #f)) 6))
      "\" style=\"width:800px;height:400px;\"></div>\n\
<script id=\"source\">\n\
$(function () {var data = [];\n\
  data.push([\"Pie Chart Label 1\",100.0]);\n\
  data.push([\"Pie Chart Label 2\",100.5]);\n\
  data.push([\"Pie Chart Label 3\",100]);\n\
  data.push([\"Pie Chart Label 4\",100.5]);\n\
  data.push([\"Pie Chart Label 5\",0.0]);\n\
  data.push([\"Pie Chart Label 6\",0.0]);\n\
var options = {\n\
                    seriesDefaults: {\n\
                        renderer: $.jqplot.PieRenderer,\n\
                    },\n\
                    legend: {\n\
                         show: true,\n\
                         placement: \"outsideGrid\", },\n\
                    highlighter: {\n\
                         show: false },\n\
                    cursor: {\n\
                         showTooltip: false },\n\
                    seriesColors: false,\n\
                   };\n\
  options.title = \"Test Pie Chart\";\n\
  options.title += ' (' + \"Created 2017-08-09\" + ')';\n\
$.jqplot.config.enablePlugins = true;\n$(document).ready(function() {\n\
var plot = $.jqplot('chart-326253', [data], options);\n\
plot.replot();\n\
var timer;\n\
\n\
// var win_width = $(window).width();\n\
// var win_height = $(window).height();\n\
// console.log( 'Window Width ' + win_width + ' Height ' + win_height);\n\
\n\
// var doc_width = document.body.clientWidth;\n\
// var doc_height = document.body.clientHeight;\n\
// console.log( 'Doc Width ' + doc_width + ' Height ' + doc_height);\n\
\n\
$(window).resize(function () {\n\
    clearTimeout(timer);\n    timer = setTimeout(function () {\n\
//        console.log( 'Resize Timer!' );\n\
        plot.replot();\n\
    }, 100);\n\
    });\n\
});\n\
});\n\
</script>")
  )
  (test-end "Jqplot - HTML Pie Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-scatter-chart-rendering)

  (test-begin "Jqplot - HTML Scatter Chart Rendering")

  (let (
         (html-scatter-chart (gnc:make-html-scatter))
       )
    ;; prepare test data
    (gnc:html-scatter-set-width! html-scatter-chart (cons 'pixels 800))
    (gnc:html-scatter-set-height! html-scatter-chart (cons 'pixels 400))
    (gnc:html-scatter-set-title! html-scatter-chart "Test Scatter Chart")
    (gnc:html-scatter-set-subtitle! html-scatter-chart "Created 2017-08-10")
    (gnc:html-scatter-set-x-axis-label! html-scatter-chart "Test x-Axis Label Scatterchart")
    (gnc:html-scatter-set-y-axis-label! html-scatter-chart "Test y-Axis Label Scatterchart")
    (gnc:html-scatter-set-y-axis-label! html-scatter-chart "Test y-Axis Label Scatterchart")
    (gnc:html-scatter-add-datapoint! html-scatter-chart '(1.1 1.1))
    (gnc:html-scatter-add-datapoint! html-scatter-chart '(2.2 2.2))
    (gnc:html-scatter-add-datapoint! html-scatter-chart '(3.3 3.3))
    (gnc:html-scatter-add-datapoint! html-scatter-chart '(4.4 4.4))
    (gnc:html-scatter-add-datapoint! html-scatter-chart '(5.5 5.5))

    ;; do the test
    (test-equal "Jqplot - Render Scatter Chart"
      (string-concatenate (drop (gnc:html-document-tree-collapse (gnc:html-scatter-render html-scatter-chart #f)) 5))
      "\" style=\"width:800px;height:400px;\"></div>\n\
<script id=\"source\">\n\
$(function () {var data = [];var series = [];\n\
  data.push([5.5, 5.5]);\n\
  data.push([4.4, 4.4]);\n\
  data.push([3.3, 3.3]);\n\
  data.push([2.2, 2.2]);\n\
  data.push([1.1, 1.1]);\n\
var options = {\n\
                    legend: { show: false, },\n\
                    seriesDefaults: {\n\
                        markerOptions: {\n\
                            style: '#f',\n\
                            color: '#ffffff', },\n\
                    },\n\
                    series: series,\n\
                    axesDefaults: {\n\
                    },        \n\
                    axes: {\n\
                        xaxis: {\n\
                        },\n\
                        yaxis: {\n\
                            autoscale: true,\n\
                        },\n\
                    },\n\
                };\n\
  options.title = \"Test Scatter Chart\";\n\
  options.title += ' (' + \"Created 2017-08-10\" + ')';\n\
  options.axes.xaxis.label = \"Test x-Axis Label Scatterchart\";\n\
  options.axes.yaxis.label = \"Test y-Axis Label Scatterchart\";\n\
$.jqplot.config.enablePlugins = true;\n\
$(document).ready(function() {\n\
var plot = $.jqplot('chart-871844', [data], options);\n\
plot.replot();\n\
var timer;\n\
\n\
// var win_width = $(window).width();\n\
// var win_height = $(window).height();\n\
// console.log( 'Window Width ' + win_width + ' Height ' + win_height);\n\
\n\
// var doc_width = document.body.clientWidth;\n\
// var doc_height = document.body.clientHeight;\n\
// console.log( 'Doc Width ' + doc_width + ' Height ' + doc_height);\n\
\n\
$(window).resize(function () {\n\
    clearTimeout(timer);\n\
    timer = setTimeout(function () {\n\
//        console.log( 'Resize Timer!' );\n\
        plot.replot();\n\
    }, 100);\n\
    });\n\
});\n\
});\n\
</script>")
  )
  (test-end "Jqplot - HTML Scatter Chart Rendering")
)
