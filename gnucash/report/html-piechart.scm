;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-piechart.scm : generate HTML programmatically, with support
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

(define-module (gnucash report html-piechart))

(use-modules (gnucash utilities))
(use-modules (gnucash report html-chart)
             (gnucash report report-utilities))

(export <html-piechart>)
(export gnc:html-piechart?)
(export gnc:make-html-piechart-internal)
(export gnc:make-html-piechart)
(export gnc:html-piechart-data)
(export gnc:html-piechart-set-data!)
(export gnc:html-piechart-width)
(export gnc:html-piechart-set-width!)
(export gnc:html-piechart-height)
(export gnc:html-piechart-set-height!)
(export gnc:html-piechart-labels)
(export gnc:html-piechart-set-labels!)
(export gnc:html-piechart-colors)
(export gnc:html-piechart-set-colors!)
(export gnc:html-piechart-title)
(export gnc:html-piechart-set-title!)
(export gnc:html-piechart-subtitle)
(export gnc:html-piechart-set-subtitle!)
(export gnc:html-piechart-button-1-slice-urls)
(export gnc:html-piechart-set-button-1-slice-urls!)
(export gnc:html-piechart-button-2-slice-urls)
(export gnc:html-piechart-set-button-2-slice-urls!)
(export gnc:html-piechart-button-3-slice-urls)
(export gnc:html-piechart-set-button-3-slice-urls!)
(export gnc:html-piechart-button-1-legend-urls)
(export gnc:html-piechart-set-button-1-legend-urls!)
(export gnc:html-piechart-button-2-legend-urls)
(export gnc:html-piechart-set-button-2-legend-urls!)
(export gnc:html-piechart-button-3-legend-urls)
(export gnc:html-piechart-set-button-3-legend-urls!)
(export gnc:html-piechart-render)

(define <html-piechart>
  (make-record-type '<html-piechart>
                    '(width
                      height
                      title
                      subtitle
                      data
                      colors
                      labels
                      button-1-slice-urls
                      button-2-slice-urls 
                      button-3-slice-urls
                      button-1-legend-urls
                      button-2-legend-urls 
                      button-3-legend-urls)))


(define gnc:html-piechart? 
  (record-predicate <html-piechart>))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-piechart> class
;;  generate the <object> form for a piechart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-piechart-internal
  (record-constructor <html-piechart>))

(define (gnc:make-html-piechart)
  (issue-deprecation-warning
   "(gnc:make-html-piechart) is deprecated in 4.x. use gnc:make-html-chart instead.")
  (gnc:make-html-piechart-internal '(pixels . -1) '(pixels . -1) #f #f #f #f #f #f #f #f #f #f #f))

(define gnc:html-piechart-data
  (record-accessor <html-piechart> 'data))

(define gnc:html-piechart-set-data!
  (record-modifier <html-piechart> 'data))

(define gnc:html-piechart-width
  (record-accessor <html-piechart> 'width))

(define gnc:html-piechart-set-width!
  (record-modifier <html-piechart> 'width))

(define gnc:html-piechart-height
  (record-accessor <html-piechart> 'height))

(define gnc:html-piechart-set-height!
  (record-modifier <html-piechart> 'height))

(define gnc:html-piechart-labels
  (record-accessor <html-piechart> 'labels))

(define gnc:html-piechart-set-labels!
  (record-modifier <html-piechart> 'labels))

(define gnc:html-piechart-colors
  (record-accessor <html-piechart> 'colors))

(define gnc:html-piechart-set-colors!
  (record-modifier <html-piechart> 'colors))

(define gnc:html-piechart-title
  (record-accessor <html-piechart> 'title))

(define gnc:html-piechart-set-title!
  (record-modifier <html-piechart> 'title))

(define gnc:html-piechart-subtitle
  (record-accessor <html-piechart> 'subtitle))

(define gnc:html-piechart-set-subtitle!
  (record-modifier <html-piechart> 'subtitle))

;; FIXME url's haven't been working since GnuCash 1.x
;;       GnuCash 2.x switched from guppy to goffice, which
;;       made it very hard to remain the url functionality
;;       At this point I (gjanssens) is in the process of
;;       moving from goffice to jqplot for our charts
;;       which perhaps may allow urls again in the charts
;;       I'm keeping the parameters below around to remind
;;       us this still has to be investigated again
(define gnc:html-piechart-button-1-slice-urls
  (record-accessor <html-piechart> 'button-1-slice-urls))

(define gnc:html-piechart-set-button-1-slice-urls!
  (record-modifier <html-piechart> 'button-1-slice-urls))

(define gnc:html-piechart-button-2-slice-urls
  (record-accessor <html-piechart> 'button-2-slice-urls))

(define gnc:html-piechart-set-button-2-slice-urls!
  (record-modifier <html-piechart> 'button-2-slice-urls))

(define gnc:html-piechart-button-3-slice-urls
  (record-accessor <html-piechart> 'button-3-slice-urls))

(define gnc:html-piechart-set-button-3-slice-urls!
  (record-modifier <html-piechart> 'button-3-slice-urls))

(define gnc:html-piechart-button-1-legend-urls
  (record-accessor <html-piechart> 'button-1-legend-urls))

(define gnc:html-piechart-set-button-1-legend-urls!
  (record-modifier <html-piechart> 'button-1-legend-urls))

(define gnc:html-piechart-button-2-legend-urls
  (record-accessor <html-piechart> 'button-2-legend-urls))

(define gnc:html-piechart-set-button-2-legend-urls!
  (record-modifier <html-piechart> 'button-2-legend-urls))

(define gnc:html-piechart-button-3-legend-urls
  (record-accessor <html-piechart> 'button-3-legend-urls))

(define gnc:html-piechart-set-button-3-legend-urls!
  (record-modifier <html-piechart> 'button-3-legend-urls))

(define (gnc:html-piechart-render piechart doc)
  (let* ((chart (gnc:make-html-chart))
         (title (gnc:html-piechart-title piechart))
         (subtitle (gnc:html-piechart-subtitle piechart))
         (data  (gnc:html-piechart-data piechart))
         (colors (gnc:html-piechart-colors piechart)))
    (cond
     ((and (pair? data) (gnc:not-all-zeros data))
      (gnc:html-chart-set-type! chart 'pie)
      (gnc:html-chart-set-axes-display! chart #f)
      (gnc:html-chart-set-width! chart (gnc:html-piechart-width piechart))
      (gnc:html-chart-set-height! chart (gnc:html-piechart-height piechart))
      (gnc:html-chart-set-data-labels! chart (gnc:html-piechart-labels piechart))
      (gnc:html-chart-add-data-series! chart "" data colors)
      (gnc:html-chart-set-title! chart (list title subtitle))
      (gnc:html-chart-render chart doc))

     (else
      (gnc:warn "null-data, not rendering piechart")
      ""))))

(gnc:guard-html-chart gnc:html-piechart-data)
(gnc:guard-html-chart gnc:html-piechart-set-data!)
(gnc:guard-html-chart gnc:html-piechart-width)
(gnc:guard-html-chart gnc:html-piechart-set-width!)
(gnc:guard-html-chart gnc:html-piechart-height)
(gnc:guard-html-chart gnc:html-piechart-set-height!)
(gnc:guard-html-chart gnc:html-piechart-labels)
(gnc:guard-html-chart gnc:html-piechart-set-labels!)
(gnc:guard-html-chart gnc:html-piechart-colors)
(gnc:guard-html-chart gnc:html-piechart-set-colors!)
(gnc:guard-html-chart gnc:html-piechart-title)
(gnc:guard-html-chart gnc:html-piechart-set-title!)
(gnc:guard-html-chart gnc:html-piechart-subtitle)
(gnc:guard-html-chart gnc:html-piechart-set-subtitle!)
(gnc:guard-html-chart gnc:html-piechart-button-1-slice-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-1-slice-urls!)
(gnc:guard-html-chart gnc:html-piechart-button-2-slice-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-2-slice-urls!)
(gnc:guard-html-chart gnc:html-piechart-button-3-slice-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-3-slice-urls!)
(gnc:guard-html-chart gnc:html-piechart-button-1-legend-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-1-legend-urls!)
(gnc:guard-html-chart gnc:html-piechart-button-2-legend-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-2-legend-urls!)
(gnc:guard-html-chart gnc:html-piechart-button-3-legend-urls)
(gnc:guard-html-chart gnc:html-piechart-set-button-3-legend-urls!)
(gnc:guard-html-chart gnc:html-piechart-render)
