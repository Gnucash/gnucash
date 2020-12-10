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

(define-module (gnucash report html-barchart))

(use-modules (gnucash utilities))
(use-modules (gnucash report html-chart)
             (gnucash report report-utilities))

(export <html-barchart>)
(export gnc:html-barchart? )
(export gnc:make-html-barchart-internal)
(export gnc:make-html-barchart)
(export gnc:html-barchart-data)
(export gnc:html-barchart-set-data!)
(export gnc:html-barchart-width)
(export gnc:html-barchart-set-width!)
(export gnc:html-barchart-height)
(export gnc:html-barchart-set-height!)
(export gnc:html-barchart-x-axis-label)
(export gnc:html-barchart-set-x-axis-label!)
(export gnc:html-barchart-y-axis-label)
(export gnc:html-barchart-set-y-axis-label!)
(export gnc:html-barchart-row-labels)
(export gnc:html-barchart-set-row-labels!)
(export gnc:html-barchart-row-labels-rotated?)
(export gnc:html-barchart-set-row-labels-rotated?!)
(export gnc:html-barchart-stacked?)
(export gnc:html-barchart-set-stacked?!)
(export gnc:html-barchart-col-labels)
(export gnc:html-barchart-set-col-labels!)
(export gnc:html-barchart-col-colors)
(export gnc:html-barchart-set-col-colors!)
(export gnc:html-barchart-legend-reversed?)
(export gnc:html-barchart-set-legend-reversed?!)
(export gnc:html-barchart-title)
(export gnc:html-barchart-set-title!)
(export gnc:html-barchart-subtitle)
(export gnc:html-barchart-set-subtitle!)
(export gnc:html-barchart-button-1-bar-urls)
(export gnc:html-barchart-set-button-1-bar-urls!)
(export gnc:html-barchart-button-2-bar-urls)
(export gnc:html-barchart-set-button-2-bar-urls!)
(export gnc:html-barchart-button-3-bar-urls)
(export gnc:html-barchart-set-button-3-bar-urls!)
(export gnc:html-barchart-button-1-legend-urls)
(export gnc:html-barchart-set-button-1-legend-urls!)
(export gnc:html-barchart-button-2-legend-urls)
(export gnc:html-barchart-set-button-2-legend-urls!)
(export gnc:html-barchart-button-3-legend-urls)
(export gnc:html-barchart-set-button-3-legend-urls!)
(export gnc:html-barchart-append-row!)
(export gnc:html-barchart-prepend-row!)
(export gnc:html-barchart-append-column!)
(export gnc:html-barchart-prepend-column!)
(export gnc:html-barchart-render barchart)

(define <html-barchart>
  (make-record-type '<html-barchart>
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

(define gnc:html-barchart? 
  (record-predicate <html-barchart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-barchart> class
;;  generate the <object> form for a barchart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-barchart-internal
  (record-constructor <html-barchart>))

(define (gnc:make-html-barchart)
  (issue-deprecation-warning
   "(gnc:make-html-barchart) is deprecated in 4.x. use gnc:make-html-chart instead.")
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
  (let* ((chart (gnc:make-html-chart))
         (data (gnc:html-barchart-data barchart)))
    (cond
     ((and (pair? data) (gnc:not-all-zeros data))
      (gnc:html-chart-set-type! chart 'bar)
      (gnc:html-chart-set-width! chart (gnc:html-barchart-width barchart))
      (gnc:html-chart-set-height! chart (gnc:html-barchart-height barchart))
      (gnc:html-chart-set-data-labels! chart (gnc:html-barchart-row-labels barchart))
      (for-each
       (lambda (label series color)
         (gnc:html-chart-add-data-series! chart label series color))
       (gnc:html-barchart-col-labels barchart)
       data
       (gnc:html-barchart-col-colors barchart))
      (gnc:html-chart-set-title! chart (list
                                        (gnc:html-barchart-title barchart)
                                        (gnc:html-barchart-subtitle barchart)))
      (gnc:html-chart-set-stacking?! chart (gnc:html-barchart-stacked? barchart))
      (gnc:html-chart-render chart doc))

     (else
      (gnc:warn "null-data, not rendering barchart")
      ""))))

(gnc:guard-html-chart gnc:html-barchart-data)
(gnc:guard-html-chart gnc:html-barchart-set-data!)
(gnc:guard-html-chart gnc:html-barchart-width)
(gnc:guard-html-chart gnc:html-barchart-set-width!)
(gnc:guard-html-chart gnc:html-barchart-height)
(gnc:guard-html-chart gnc:html-barchart-set-height!)
(gnc:guard-html-chart gnc:html-barchart-x-axis-label)
(gnc:guard-html-chart gnc:html-barchart-set-x-axis-label!)
(gnc:guard-html-chart gnc:html-barchart-y-axis-label)
(gnc:guard-html-chart gnc:html-barchart-set-y-axis-label!)
(gnc:guard-html-chart gnc:html-barchart-row-labels)
(gnc:guard-html-chart gnc:html-barchart-set-row-labels!)
(gnc:guard-html-chart gnc:html-barchart-row-labels-rotated?)
(gnc:guard-html-chart gnc:html-barchart-set-row-labels-rotated?!)
(gnc:guard-html-chart gnc:html-barchart-stacked?)
(gnc:guard-html-chart gnc:html-barchart-set-stacked?!)
(gnc:guard-html-chart gnc:html-barchart-col-labels)
(gnc:guard-html-chart gnc:html-barchart-set-col-labels!)
(gnc:guard-html-chart gnc:html-barchart-col-colors)
(gnc:guard-html-chart gnc:html-barchart-set-col-colors!)
(gnc:guard-html-chart gnc:html-barchart-legend-reversed?)
(gnc:guard-html-chart gnc:html-barchart-set-legend-reversed?!)
(gnc:guard-html-chart gnc:html-barchart-title)
(gnc:guard-html-chart gnc:html-barchart-set-title!)
(gnc:guard-html-chart gnc:html-barchart-subtitle)
(gnc:guard-html-chart gnc:html-barchart-set-subtitle!)
(gnc:guard-html-chart gnc:html-barchart-button-1-bar-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-1-bar-urls!)
(gnc:guard-html-chart gnc:html-barchart-button-2-bar-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-2-bar-urls!)
(gnc:guard-html-chart gnc:html-barchart-button-3-bar-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-3-bar-urls!)
(gnc:guard-html-chart gnc:html-barchart-button-1-legend-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-1-legend-urls!)
(gnc:guard-html-chart gnc:html-barchart-button-2-legend-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-2-legend-urls!)
(gnc:guard-html-chart gnc:html-barchart-button-3-legend-urls)
(gnc:guard-html-chart gnc:html-barchart-set-button-3-legend-urls!)
(gnc:guard-html-chart gnc:html-barchart-append-row!)
(gnc:guard-html-chart gnc:html-barchart-prepend-row!)
(gnc:guard-html-chart gnc:html-barchart-append-column!)
(gnc:guard-html-chart gnc:html-barchart-prepend-column!)
(gnc:guard-html-chart gnc:html-barchart-render)
