;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-barchart.scm : generate HTML programmatically, with support
;; for simple style elements. 
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
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

(use-modules (gnucash report))

(define <html-barchart>
  (make-record-type "<html-barchart>"
                    '(width 
                      height 
                      title 
                      subtitle 
                      x-axis-label
                      y-axis-label
                      col-labels
                      row-labels 
                      col-colors 
		      legend-reversed?
                      row-labels-rotated?
		      stacked?
                      data
		      button-1-bar-urls
                      button-2-bar-urls 
		      button-3-bar-urls
		      button-1-legend-urls
                      button-2-legend-urls 
		      button-3-legend-urls)))

(define gnc:html-barchart? 
  (record-predicate <html-barchart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-barchart> class
;;  generate the <object> form for a barchart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-barchart-internal
  (record-constructor <html-barchart>))

(define (gnc:make-html-barchart)
  (gnc:make-html-barchart-internal '(pixels . -1) '(pixels . -1) #f #f #f #f '() '() '() 
				   #f #f #f '() #f #f #f #f #f #f))

(define gnc:html-barchart-data
  (record-accessor <html-barchart> 'data))

(define gnc:html-barchart-set-data!
  (record-modifier <html-barchart> 'data))

(define gnc:html-barchart-width
  (record-accessor <html-barchart> 'width))

(define gnc:html-barchart-set-width!
  (record-modifier <html-barchart> 'width))

(define gnc:html-barchart-height
  (record-accessor <html-barchart> 'height))

(define gnc:html-barchart-set-height!
  (record-modifier <html-barchart> 'height))

(define gnc:html-barchart-x-axis-label
  (record-accessor <html-barchart> 'x-axis-label))

(define gnc:html-barchart-set-x-axis-label!
  (record-modifier <html-barchart> 'x-axis-label))

(define gnc:html-barchart-y-axis-label
  (record-accessor <html-barchart> 'y-axis-label))

(define gnc:html-barchart-set-y-axis-label!
  (record-modifier <html-barchart> 'y-axis-label))

(define gnc:html-barchart-row-labels
  (record-accessor <html-barchart> 'row-labels))

(define gnc:html-barchart-set-row-labels!
  (record-modifier <html-barchart> 'row-labels))

(define gnc:html-barchart-row-labels-rotated?
  (record-accessor <html-barchart> 'row-labels-rotated?))

(define gnc:html-barchart-set-row-labels-rotated?!
  (record-modifier <html-barchart> 'row-labels-rotated?))

(define gnc:html-barchart-stacked?
  (record-accessor <html-barchart> 'stacked?))

(define gnc:html-barchart-set-stacked?!
  (record-modifier <html-barchart> 'stacked?))

(define gnc:html-barchart-col-labels
  (record-accessor <html-barchart> 'col-labels))

(define gnc:html-barchart-set-col-labels!
  (record-modifier <html-barchart> 'col-labels))

(define gnc:html-barchart-col-colors
  (record-accessor <html-barchart> 'col-colors))

(define gnc:html-barchart-set-col-colors!
  (record-modifier <html-barchart> 'col-colors))

(define gnc:html-barchart-legend-reversed?
  (record-accessor <html-barchart> 'legend-reversed?))

(define gnc:html-barchart-set-legend-reversed?!
  (record-modifier <html-barchart> 'legend-reversed?))

(define gnc:html-barchart-title
  (record-accessor <html-barchart> 'title))

(define gnc:html-barchart-set-title!
  (record-modifier <html-barchart> 'title))

(define gnc:html-barchart-subtitle
  (record-accessor <html-barchart> 'subtitle))

(define gnc:html-barchart-set-subtitle!
  (record-modifier <html-barchart> 'subtitle))

;; Note: ATM you can specify one url per column, but this url will be
;; used for all of the rows. Otherwise we could have cols*rows urls
;; (quite a lot), but this first requires fixing
;; guppi_bar_1_callback() in gnome/gnc-html-guppi.c .
;; FIXME url's haven't been working since GnuCash 1.x
;;       GnuCash 2.x switched from guppy to goffice, which
;;       made it very hard to remain the url functionality
;;       At this point I (gjanssens) is in the process of
;;       moving from goffice to jqplot for our charts
;;       which perhaps may allow urls again in the charts
;;       I'm keeping the parameters below around to remind
;;       us this still has to be investigated again
(define gnc:html-barchart-button-1-bar-urls
  (record-accessor <html-barchart> 'button-1-bar-urls))

(define gnc:html-barchart-set-button-1-bar-urls!
  (record-modifier <html-barchart> 'button-1-bar-urls))

(define gnc:html-barchart-button-2-bar-urls
  (record-accessor <html-barchart> 'button-2-bar-urls))

(define gnc:html-barchart-set-button-2-bar-urls!
  (record-modifier <html-barchart> 'button-2-bar-urls))

(define gnc:html-barchart-button-3-bar-urls
  (record-accessor <html-barchart> 'button-3-bar-urls))

(define gnc:html-barchart-set-button-3-bar-urls!
  (record-modifier <html-barchart> 'button-3-bar-urls))

(define gnc:html-barchart-button-1-legend-urls
  (record-accessor <html-barchart> 'button-1-legend-urls))

(define gnc:html-barchart-set-button-1-legend-urls!
  (record-modifier <html-barchart> 'button-1-legend-urls))

(define gnc:html-barchart-button-2-legend-urls
  (record-accessor <html-barchart> 'button-2-legend-urls))

(define gnc:html-barchart-set-button-2-legend-urls!
  (record-modifier <html-barchart> 'button-2-legend-urls))

(define gnc:html-barchart-button-3-legend-urls
  (record-accessor <html-barchart> 'button-3-legend-urls))

(define gnc:html-barchart-set-button-3-legend-urls!
  (record-modifier <html-barchart> 'button-3-legend-urls))

(define (gnc:html-barchart-append-row! barchart newrow)
  (let ((dd (gnc:html-barchart-data barchart)))
    (set! dd (append dd (list newrow)))
    (gnc:html-barchart-set-data! barchart dd)))

(define (gnc:html-barchart-prepend-row! barchart newrow)
  (let ((dd (gnc:html-barchart-data barchart)))
    (set! dd (cons newrow dd))
    (gnc:html-barchart-set-data! barchart dd)))

(define (gnc:html-barchart-append-column! barchart newcol)
  (let ((colnum 0)
        (rownum 0)
        (rows (gnc:html-barchart-data barchart))
        (this-row #f)
        (new-row #f))
    ;; find out how many cols are already there in the deepest row
    (for-each 
     (lambda (row)
       (let ((l (length row)))
         (if (> l colnum)
             (set! colnum l))))
     rows)
    
    ;; append the elements of 'newrow' to the rowumns 
    (for-each
     (lambda (newelt)
       ;; find the row, or append one 
       (if (not (null? rows))
           (begin
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))
       
       ;; make sure the rowumn is long enough, then append the data 
       (let loop ((l (length this-row))
                  (r (reverse this-row)))
         (if (< l colnum)
             (loop (+ l 1) (cons #f r))
             (set! this-row 
                   (reverse (cons newelt r)))))
       (if new-row
           (gnc:html-barchart-append-row! barchart this-row)
           (list-set! (gnc:html-barchart-data barchart) rownum this-row))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:not-all-zeros data)
  (define (myor list)
    (begin 
      (gnc:debug "list" list)
      (if (null? list) #f
	  (or (car list) (myor (cdr list))))))

  (cond ((number? data) (not (= 0 data)))
	((list? data) (myor (map gnc:not-all-zeros data)))
	(else #f)))

(define (gnc:html-barchart-prepend-column! barchart newcol)
  (let ((rows (gnc:html-barchart-data barchart))
        (this-row #f)
        (new-row #f)
        (rownum 0))
    (for-each 
     (lambda (elt)
       (if (not (null? rows))
           (begin 
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))
       (if new-row
           (gnc:html-barchart-append-row! barchart (list elt))
           (list-set! (gnc:html-barchart-data barchart) rownum
                      (cons elt this-row)))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-barchart-render barchart doc)
  (define (ensure-numeric elt)
    (cond ((number? elt)
           (exact->inexact elt))
          ((string? elt)
           (with-input-from-string elt
             (lambda ()
               (let ((n (read)))
                 (if (number? n) n 0.0)))))
          (#t 
           0.0)))
  
  (define (catenate-escaped-strings nlist)
    (if (not (list? nlist))
        ""
        (with-output-to-string
          (lambda ()
            (for-each 
             (lambda (s)
               (let ((escaped 
                      (regexp-substitute/global 
                       #f " " 
                       (regexp-substitute/global 
                        #f "\\\\" s
                        'pre "\\\\" 'post)
                       'pre "\\ " 'post)))
                 (display escaped)
                 (display " ")))
             nlist)))))

  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (title (gnc:html-barchart-title barchart))
         (subtitle (gnc:html-barchart-subtitle barchart))
         (url-1
          (catenate-escaped-strings 
           (gnc:html-barchart-button-1-bar-urls barchart)))
         (url-2
          (catenate-escaped-strings 
           (gnc:html-barchart-button-2-bar-urls barchart)))
         (url-3
          (catenate-escaped-strings 
           (gnc:html-barchart-button-3-bar-urls barchart)))
         (legend-1
          (catenate-escaped-strings 
           (gnc:html-barchart-button-1-legend-urls barchart)))
         (legend-2
          (catenate-escaped-strings 
           (gnc:html-barchart-button-2-legend-urls barchart)))
         (legend-3
          (catenate-escaped-strings 
           (gnc:html-barchart-button-3-legend-urls barchart)))
         (x-label (gnc:html-barchart-x-axis-label barchart))
         (y-label (gnc:html-barchart-y-axis-label barchart))
         (data (gnc:html-barchart-data barchart))
	 (dummy1 (gnc:debug "data " data))
         (row-labels (catenate-escaped-strings 
                      (gnc:html-barchart-row-labels barchart)))
         (col-labels (catenate-escaped-strings 
                      (gnc:html-barchart-col-labels barchart)))
         ;; convert color list to string with valid js array of strings, example: "\"blue\", \"red\""
         (colors-str (string-join (map (lambda (color)
					 (string-append "\"" color "\""))
				       (gnc:html-barchart-col-colors barchart)) ", "))
         (series-data-start (lambda (series-index)
                         (push "var d")
                         (push series-index)
                         (push " = [];\n")))
         (series-data-add (lambda (series-index x y)
                         (push (string-append
                               "  d"
                               (number->string series-index)
                               ".push(["
                               (number->string x)
                               ", "
                               (number->string y)
                               "]);\n"))))
         (series-data-end (lambda (series-index label)
                         (push "data.push(d")
                         (push series-index)
                         (push ");\n")
                         (push (format #f "series.push({ label: ~s });\n\n"
                                       (gnc:html-string-sanitize label)))
                         ))
         ; Use a unique chart-id for each chart. This prevents chart
         ; clashed on multi-column reports
         (chart-id (string-append "chart-" (number->string (random 999999)))))
    (if (and (list? data)
             (not (null? data))
             (gnc:not-all-zeros data))
        (begin
            (push (gnc:html-js-include "jqplot/jquery.min.js"))
            (push (gnc:html-js-include "jqplot/jquery.jqplot.js"))
            (push (gnc:html-js-include "jqplot/jqplot.barRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.cursor.js"))
            (push (gnc:html-js-include "jqplot/jqplot.categoryAxisRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.highlighter.js"))
            (push (gnc:html-js-include "jqplot/jqplot.canvasTextRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.canvasAxisTickRenderer.js"))

            (push (gnc:html-css-include "jqplot/jquery.jqplot.css"))
            (push "<div id=\"")(push chart-id)(push "\" style=\"width:")
            (push (cdr (gnc:html-barchart-width barchart)))
            (if (eq? 'pixels (car (gnc:html-barchart-width barchart)))
                 (push "px;height:")
                 (push "%;height:"))

            (push (cdr (gnc:html-barchart-height barchart)))
            (if (eq? 'pixels (car (gnc:html-barchart-height barchart)))
                 (push "px;\"></div>\n")
                 (push "%;\"></div>\n"))
            (push "<script id=\"source\">\n$(function () {")

            (push "var data = [];")
            (push "var series = [];\n")

            (if (and data (list? data))
              (let ((rows (length data))
                    (cols 0))
                (let loop ((col 0) (rowcnt 1))
                  (series-data-start col)
                  (if (list? (car data))
                      (begin 
                        (set! cols (length (car data)))))    
                  (for-each
                    (lambda (row)
                      (if (<= rowcnt rows)
                        (series-data-add col rowcnt
                                       (ensure-numeric (list-ref-safe row col)))
                      )
                      (set! rowcnt (+ rowcnt 1)))
                    data)
                  (series-data-end col (list-ref-safe (gnc:html-barchart-col-labels barchart) col))
                  (if (< col (- cols 1))
                      (loop (+ 1 col) 1)))))


            (push "var all_ticks = [")
            (for-each
                (lambda (val)
                    (push "\"")
                    (push val)
                    (push "\","))
                (gnc:html-barchart-row-labels barchart))
            (push "];\n")
            (push "var options = {
                   shadowAlpha: 0.07,
                   stackSeries: false,
                   legend: {
                        show: true,
                        placement: \"outsideGrid\", },
                   seriesDefaults: {
                        renderer: $.jqplot.BarRenderer,
                        rendererOptions: {
                            shadowAlpha: 0.04,
                            shadowDepth: 3,
                        },
                        fillToZero: true,
                   },
                   series: series,
                   axesDefaults: {
                   },
                   grid: {
                   },
                   axes: {
                       xaxis: {
                           renderer:$.jqplot.CategoryAxisRenderer,
                           tickRenderer: $.jqplot.CanvasAxisTickRenderer,
                           tickOptions: {
                               angle: -30,
                               fontSize: '10pt',
                           },
                       },
                       yaxis: {
                           autoscale: true,
                       },
                   },
                   highlighter: {
                       tooltipContentEditor: formatTooltip,
                   },
                   cursor:{
                       show: true,
                       showTooltip: false,
                       zoom: true,
                   },
                   seriesColors: false,
                };\n")

            (push "  options.stackSeries = ")
            (push (if (gnc:html-barchart-stacked? barchart)
                "true;\n"
                "false;\n"))

            (if title
                (push (format #f "  options.title = ~s;\n"
                              (gnc:html-string-sanitize title))))

            (if subtitle
                (push (format #f "  options.title += ' <br />' + ~s;\n"
                               (gnc:html-string-sanitize subtitle))))


            (if (and (string? x-label) (> (string-length x-label) 0))
              (begin 
                (push "  options.axes.xaxis.label = \"")
                (push x-label)
                (push "\";\n")))
            (if (and (string? y-label) (> (string-length y-label) 0))
              (begin 
                (push "  options.axes.yaxis.label = \"")
                (push y-label)
                (push "\";\n")))
            (push "  options.axes.xaxis.ticks = all_ticks;\n")
            (if (not (equal? colors-str ""))
                (begin            ; example: options.seriesColors= ["blue", "red"];
                  (push "options.seriesColors = [")
                  (push colors-str)
                  (push "];\n")
                  (push "options.negativeSeriesColors = [")
                  (push colors-str)
                  (push "];\n")
                  )
                )


            (push "$.jqplot.config.enablePlugins = true;\n")
            (push "$(document).ready(function() {
var plot = $.jqplot('")(push chart-id)(push"', data, options);
plot.axes.xaxis.ticks = getVisualTicks();
plot.replot();
var timer;
var load_timer;

// var win_width = $(window).width();
// var win_height = $(window).height();
// console.log( 'Window Width ' + win_width + ' Height ' + win_height);

// var doc_width = document.body.clientWidth;
// var doc_height = document.body.clientHeight;
// console.log( 'Doc Width ' + doc_width + ' Height ' + doc_height);

$(window).resize(function () {
    clearTimeout(timer);
    timer = setTimeout(function () {
        plot.replot({resetAxes: true });
        $.each(plot.series, function(index, series) {
            series.barWidth = undefined;
        });
        plot.axes.xaxis.ticks = getVisualTicks();
//        console.log( 'Resize Timer!' );
        plot.replot();
    }, 100);
    });

$(window).on('load', function () {
    var hasVScroll = document.body.scrollHeight > document.body.clientHeight;
    clearTimeout(load_timer);
    load_timer = setTimeout(function () {
//        console.log( 'Load Timer!' );
        if(hasVScroll)
        {
//            console.log( 'Load Timer Replot!' );
            plot.replot();
        }
    },100);
    });
});

function formatTooltip(str, seriesIndex, pointIndex) {
    if (options.axes.xaxis.ticks[pointIndex] !== undefined)
        x = options.axes.xaxis.ticks[pointIndex];
    else
        x = pointIndex;
    y = data[seriesIndex][pointIndex][1].toFixed(2);
    return options.series[seriesIndex].label + '<br/>' + x + '<br/><b>' + y + '</b>';
}

function getVisualTicks() {
    var chart_width = document.getElementById(\"")(push chart-id)(push"\").getElementsByClassName(\"jqplot-zoom-canvas\")[0].width;
    var num_ticks = all_ticks.length;
    var label_width = 25;
    var num_labels = chart_width / label_width;
    var show_every_nth_label = Math.ceil (num_ticks / num_labels);
    var visual_ticks = [];

    if (show_every_nth_label == 0)
        show_every_nth_label = 1;
    for (counter = 0; counter < all_ticks.length; counter++) {
        if ((counter % show_every_nth_label) == 0)
            visual_ticks.push (all_ticks[counter]);
        else
            visual_ticks.push (' ');
    }
//    console.log( 'getVis chart_width ' + chart_width );
    return visual_ticks;
}\n")

            (push "});\n</script>")

            (gnc:msg (string-join (reverse (map (lambda (e) (if (number? e) (number->string e) e)) retval)) ""))
 
        )
        (begin 
          (gnc:warn "barchart has no non-zero data.")
            " "))
    retval))
