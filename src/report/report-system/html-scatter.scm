;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-scatter.scm : generate HTML programmatically, with support
;; for simple style elements. 
;; Copyright 2001 Christian Stimming <stimming@tuhh.de>
;;
;; Adapted from html-barchart.scm which is 
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

(load-from-path "html-jqplot")

(define <html-scatter>
  (make-record-type "<html-scatter>"
                    '(width
                      height
                      title
                      subtitle 
                      x-axis-label
                      y-axis-label
                      ;; a list of x-y-value lists.
                      data 
                      ;; Valid marker names are:
                      ;; diamond, circle, square, x, plus, dash,
                      ;; filledDiamond, filledCircle, filledSquare
                      marker
                      ;; The color of the markers outline. Should be a hex string,
                      ;; as returned by gnc:color-option->hex-string, prefixed by
                      ;; #, like "#ff0000" for red
                      markercolor
                      )))

(define gnc:html-scatter? 
  (record-predicate <html-scatter>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-scatter> class
;;  generate the <object> form for a scatter plot. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-scatter-internal
  (record-constructor <html-scatter>))

(define (gnc:make-html-scatter)
  (gnc:make-html-scatter-internal -1 -1 #f #f #f #f '() #f #f))

(define gnc:html-scatter-width
  (record-accessor <html-scatter> 'width))

(define gnc:html-scatter-set-width!
  (record-modifier <html-scatter> 'width))

(define gnc:html-scatter-height
  (record-accessor <html-scatter> 'height))

(define gnc:html-scatter-set-height!
  (record-modifier <html-scatter> 'height))

(define gnc:html-scatter-title
  (record-accessor <html-scatter> 'title))

(define gnc:html-scatter-set-title!
  (record-modifier <html-scatter> 'title))

(define gnc:html-scatter-subtitle
  (record-accessor <html-scatter> 'subtitle))

(define gnc:html-scatter-set-subtitle!
  (record-modifier <html-scatter> 'subtitle))

(define gnc:html-scatter-x-axis-label
  (record-accessor <html-scatter> 'x-axis-label))

(define gnc:html-scatter-set-x-axis-label!
  (record-modifier <html-scatter> 'x-axis-label))

(define gnc:html-scatter-y-axis-label
  (record-accessor <html-scatter> 'y-axis-label))

(define gnc:html-scatter-set-y-axis-label!
  (record-modifier <html-scatter> 'y-axis-label))

(define gnc:html-scatter-data
  (record-accessor <html-scatter> 'data))

(define gnc:html-scatter-set-data!
  (record-modifier <html-scatter> 'data))

(define gnc:html-scatter-marker
  (record-accessor <html-scatter> 'marker))

(define gnc:html-scatter-set-marker!
  (record-modifier <html-scatter> 'marker))

(define gnc:html-scatter-markercolor
  (record-accessor <html-scatter> 'markercolor))

(define gnc:html-scatter-set-markercolor!
  (record-modifier <html-scatter> 'markercolor))

(define (gnc:html-scatter-add-datapoint! scatter newpoint)
  (if (and (list? newpoint)
	   (not (null? newpoint)))
      (gnc:html-scatter-set-data!
       scatter
       (cons newpoint (gnc:html-scatter-data scatter)))))

;; The Renderer
(define (gnc:html-scatter-render scatter doc)
  (define (ensure-numeric elt)
    (cond ((number? elt)
           (exact->inexact elt))
          ((string? elt)
           (with-input-from-string elt
             (lambda ()
               (let ((n (read)))
                 (if (number? n) n 0.0)))))
          ((gnc:gnc-numeric? elt)
           (gnc-numeric-to-double elt))
          (#t 
           0.0)))
  
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (title (gnc:html-scatter-title scatter))
         (subtitle (gnc:html-scatter-subtitle scatter))
         (x-label (gnc:html-scatter-x-axis-label scatter))
         (y-label (gnc:html-scatter-y-axis-label scatter))
         (data (gnc:html-scatter-data scatter))
         (marker (gnc:html-scatter-marker scatter))
         (markercolor (string-append "#" (gnc:html-scatter-markercolor scatter)))
         ; Use a unique chart-id for each chart. This prevents chart
         ; clashed on multi-column reports
         (chart-id (string-append "chart-" (number->string (random 999999)))))
    (if (and (list? data)
             (not (null? data)))
        (begin
            (push (gnc:html-js-include "jqplot/jquery.min.js"))
            (push (gnc:html-js-include "jqplot/jquery.jqplot.js"))
            (push (gnc:html-css-include "jqplot/jquery.jqplot.css"))

            (push "<div id=\"")(push chart-id)(push "\" style=\"width:")
            (push (gnc:html-scatter-width scatter))
            (push "px;height:")
            (push (gnc:html-scatter-height scatter))
            (push "px;\"></div>\n")
            (push "<script id=\"source\">\n$(function () {")

            (push "var data = [];")
            (push "var series = [];\n")

            (if (and data (list? data))
              (let ((x-data (map-in-order car data))
                    (y-data (map-in-order cadr data)))
                (for-each (lambda (x y)
                         (push "  data.push([")
                         (push (ensure-numeric x))
                         (push ", ")
                         (push (ensure-numeric y))
                         (push "]);\n"))
                       x-data y-data)
            ))


            (push "var options = {
                    legend: { show: false, },
                    seriesDefaults: {
                        markerOptions: {
                            style: '")
            (push marker)
            (push "',
                            color: '")
            (push markercolor)
            (push "', },
                    },
                    series: series,
                    axesDefaults: {
                    },        
                    axes: {
                        xaxis: {
                        },
                        yaxis: {
                            autoscale: true,
                        },
                    },
                };\n")

            (if title
              (begin
                (push "  options.title = \"")
                (push title) (push "\";\n")))

            (if subtitle
              (begin
                (push "  options.title += \" (")
                (push subtitle) (push ")\";\n")))

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


            (push "$.jqplot.config.enablePlugins = true;\n")
            (push "var plot = $.jqplot('")(push chart-id)(push "', [data], options);\n")

            (push "});\n</script>"))
        (begin
          (gnc:warn "Scatter chart has no non-zero data")
            " "))
    retval))
