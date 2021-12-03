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

(define-module (gnucash report html-scatter))

(use-modules (gnucash utilities))
(use-modules (gnucash report html-chart)
             (gnucash report report-utilities))

(export <html-scatter>)
(export gnc:html-scatter-add-datapoint!)
(export gnc:html-scatter-data)
(export gnc:html-scatter-height)
(export gnc:html-scatter-marker)
(export gnc:html-scatter-markercolor)
(export gnc:html-scatter-render)
(export gnc:html-scatter-set-data!)
(export gnc:html-scatter-set-height!)
(export gnc:html-scatter-set-marker!)
(export gnc:html-scatter-set-markercolor!)
(export gnc:html-scatter-set-subtitle!)
(export gnc:html-scatter-set-title!)
(export gnc:html-scatter-set-width!)
(export gnc:html-scatter-set-x-axis-label!)
(export gnc:html-scatter-set-y-axis-label!)
(export gnc:html-scatter-subtitle)
(export gnc:html-scatter-title)
(export gnc:html-scatter-width)
(export gnc:html-scatter-x-axis-label)
(export gnc:html-scatter-y-axis-label)
(export gnc:html-scatter?)
(export gnc:make-html-scatter)
(export gnc:make-html-scatter-internal)

(define <html-scatter>
  (make-record-type '<html-scatter>
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

(define-syntax-rule (gnc:guard-html-chart api)
  ;; this macro applied to old html-bar/line/scatter/pie apis will
  ;; guard a report writer from passing html-chart objects. this
  ;; should be removed in 5.x series.
  (let ((old-api api))
    (set! api
      (lambda args
        (if (and (pair? args) (gnc:html-chart? (car args)))
            (gnc:warn "using old-api " (procedure-name api) " on html-chart object. set options via gnc:html-chart-set! or its shortcuts gnc:html-chart-set-title! etc, and set data via gnc:html-chart-add-data-series! see sample-graphs.scm for examples.")
            (apply old-api args))))))

(define gnc:html-scatter? 
  (record-predicate <html-scatter>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-scatter> class
;;  generate the <object> form for a scatter plot. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-scatter-internal
  (record-constructor <html-scatter>))

(define (gnc:make-html-scatter)
  (issue-deprecation-warning
   "(gnc:make-html-scatter) is deprecated in 4.x. use gnc:make-html-chart instead.")
  (gnc:make-html-scatter-internal '(pixels . -1) '(pixels . -1) #f #f #f #f '() #f #f))

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
  (let* ((chart (gnc:make-html-chart))
         (mcolor (gnc:html-scatter-markercolor scatter))
         (data  (gnc:html-scatter-data scatter)))
    (cond
     ((and (pair? data) (gnc:not-all-zeros data))
      (gnc:html-chart-set-type! chart 'scatter)
      (gnc:html-chart-set-width! chart (gnc:html-scatter-width scatter))
      (gnc:html-chart-set-height! chart (gnc:html-scatter-height scatter))
      (gnc:html-chart-set-data-labels! chart (make-list (length data) #f))
      (gnc:html-chart-add-data-series! chart "scatter"
                                       (map
                                        (lambda (datum)
                                          (list
                                           (cons 'x (car datum))
                                           (cons 'y (cadr datum))))
                                        data)
                                       (make-list (length data) mcolor)
                                       'showLine #t
                                       'fill #f
                                       'borderColor mcolor)
      (gnc:html-chart-set-title! chart (list
                                        (gnc:html-scatter-title scatter)
                                        (gnc:html-scatter-subtitle scatter)))
      (gnc:html-chart-set! chart
                           '(options elements point pointStyle)
                           (case (gnc:html-scatter-marker scatter)
                             ((filleddiamond diamond) "rectRot")
                             ((filledcircle circle) "circle")
                             ((filledsquare square) "rect")
                             ((cross) "crossRot")
                             ((plus) "cross")
                             ((dash) "line")
                             (else #f)))
      (gnc:html-chart-set! chart '(options scales xAxes (0) type) "linear")
      (gnc:html-chart-render chart doc))

     (else
      (gnc:warn "null-data, not rendering scatter")
      ""))))

(gnc:guard-html-chart gnc:html-scatter-width)
(gnc:guard-html-chart gnc:html-scatter-set-width!)
(gnc:guard-html-chart gnc:html-scatter-height)
(gnc:guard-html-chart gnc:html-scatter-set-height!)
(gnc:guard-html-chart gnc:html-scatter-title)
(gnc:guard-html-chart gnc:html-scatter-set-title!)
(gnc:guard-html-chart gnc:html-scatter-subtitle)
(gnc:guard-html-chart gnc:html-scatter-set-subtitle!)
(gnc:guard-html-chart gnc:html-scatter-x-axis-label)
(gnc:guard-html-chart gnc:html-scatter-set-x-axis-label!)
(gnc:guard-html-chart gnc:html-scatter-y-axis-label)
(gnc:guard-html-chart gnc:html-scatter-set-y-axis-label!)
(gnc:guard-html-chart gnc:html-scatter-data)
(gnc:guard-html-chart gnc:html-scatter-set-data!)
(gnc:guard-html-chart gnc:html-scatter-marker)
(gnc:guard-html-chart gnc:html-scatter-set-marker!)
(gnc:guard-html-chart gnc:html-scatter-markercolor)
(gnc:guard-html-chart gnc:html-scatter-set-markercolor!)
(gnc:guard-html-chart gnc:html-scatter-add-datapoint!)
(gnc:guard-html-chart gnc:html-scatter-render)
