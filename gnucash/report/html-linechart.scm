;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-linechart.scm : generate HTML programmatically, with support
;; for simple style elements.
;; Copyright 2008 Sven Henkel <shenkel@gmail.com>
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

(define-module (gnucash report html-linechart))

(use-modules (srfi srfi-1))
(use-modules (gnucash utilities))
(use-modules (gnucash report html-chart)
             (gnucash report report-utilities))

(export <html-linechart>)
(export gnc:html-linechart? )
(export gnc:make-html-linechart-internal)
(export gnc:make-html-linechart)
(export gnc:html-linechart-data)
(export gnc:html-linechart-set-data!)
(export gnc:html-linechart-width)
(export gnc:html-linechart-set-width!)
(export gnc:html-linechart-height)
(export gnc:html-linechart-set-height!)
(export gnc:html-linechart-x-axis-label)
(export gnc:html-linechart-set-x-axis-label!)
(export gnc:html-linechart-y-axis-label)
(export gnc:html-linechart-set-y-axis-label!)
(export gnc:html-linechart-row-labels)
(export gnc:html-linechart-set-row-labels!)
(export gnc:html-linechart-row-labels-rotated?)
(export gnc:html-linechart-set-row-labels-rotated?!)
(export gnc:html-linechart-stacked?)
(export gnc:html-linechart-set-stacked?!)
(export gnc:html-linechart-markers?)
(export gnc:html-linechart-set-markers?!)
(export gnc:html-linechart-major-grid?)
(export gnc:html-linechart-set-major-grid?!)
(export gnc:html-linechart-minor-grid?)
(export gnc:html-linechart-set-minor-grid?!)
(export gnc:html-linechart-col-labels)
(export gnc:html-linechart-set-col-labels!)
(export gnc:html-linechart-col-colors)
(export gnc:html-linechart-set-col-colors!)
(export gnc:html-linechart-legend-reversed?)
(export gnc:html-linechart-set-legend-reversed?!)
(export gnc:html-linechart-title)
(export gnc:html-linechart-set-title!)
(export gnc:html-linechart-subtitle)
(export gnc:html-linechart-set-subtitle!)
(export gnc:html-linechart-button-1-line-urls)
(export gnc:html-linechart-set-button-1-line-urls!)
(export gnc:html-linechart-button-2-line-urls)
(export gnc:html-linechart-set-button-2-line-urls!)
(export gnc:html-linechart-button-3-line-urls)
(export gnc:html-linechart-set-button-3-line-urls!)
(export gnc:html-linechart-button-1-legend-urls)
(export gnc:html-linechart-set-button-1-legend-urls!)
(export gnc:html-linechart-button-2-legend-urls)
(export gnc:html-linechart-set-button-2-legend-urls!)
(export gnc:html-linechart-button-3-legend-urls)
(export gnc:html-linechart-set-button-3-legend-urls!)
(export gnc:html-linechart-append-row!)
(export gnc:html-linechart-prepend-row!)
(export gnc:html-linechart-append-column!)
(export gnc:html-linechart-prepend-column!)
(export gnc:html-linechart-render linechart)
(export gnc:html-linechart-set-line-width!)
(export gnc:html-linechart-line-width)

(define <html-linechart>
  (make-record-type '<html-linechart>
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
                      markers?
                      major-grid?
                      minor-grid?
                      data
                      button-1-line-urls
                      button-2-line-urls
                      button-3-line-urls
                      button-1-legend-urls
                      button-2-legend-urls
                      button-3-legend-urls
                      line-width)))

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

(define gnc:html-linechart?
  (record-predicate <html-linechart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-linechart> class
;;  generate the <object> form for a linechart.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-linechart-internal
  (record-constructor <html-linechart>))

(define (gnc:make-html-linechart)
  (issue-deprecation-warning
   "(gnc:make-html-linechart) is deprecated in 4.x. use gnc:make-html-chart instead.")
  (gnc:make-html-linechart-internal
    '(pixels . -1)  ;;width
    '(pixels . -1)  ;;height
    #f   ;;title
    #f   ;;subtitle
    #f   ;;x-axis-label
    #f   ;;y-axis-label
    '()  ;;col-labels
    '()  ;;row-labels
    '()  ;;col-colors
    #f   ;;legend-reversed?
    #f   ;;row-labels-rotated?
    #f   ;;stacked?
    #t   ;;markers?
    #t   ;;major-grid?
    #t   ;;minor-grid?
    '()  ;;data
    #f   ;;button-1-line-urls
    #f   ;;button-2-line-urls
    #f   ;;button-3-line-urls
    #f   ;;button-1-legend-urls
    #f   ;;button-2-legend-urls
    #f   ;;button-3-legend-urls
    1.5  ;;line-width
  )
)

(define gnc:html-linechart-data
  (record-accessor <html-linechart> 'data))

(define gnc:html-linechart-set-data!
  (record-modifier <html-linechart> 'data))

(define gnc:html-linechart-width
  (record-accessor <html-linechart> 'width))

(define gnc:html-linechart-set-width!
  (record-modifier <html-linechart> 'width))

(define gnc:html-linechart-height
  (record-accessor <html-linechart> 'height))

(define gnc:html-linechart-set-height!
  (record-modifier <html-linechart> 'height))

(define gnc:html-linechart-x-axis-label
  (record-accessor <html-linechart> 'x-axis-label))

(define gnc:html-linechart-set-x-axis-label!
  (record-modifier <html-linechart> 'x-axis-label))

(define gnc:html-linechart-y-axis-label
  (record-accessor <html-linechart> 'y-axis-label))

(define gnc:html-linechart-set-y-axis-label!
  (record-modifier <html-linechart> 'y-axis-label))

(define gnc:html-linechart-row-labels
  (record-accessor <html-linechart> 'row-labels))

(define gnc:html-linechart-set-row-labels!
  (record-modifier <html-linechart> 'row-labels))

(define gnc:html-linechart-row-labels-rotated?
  (record-accessor <html-linechart> 'row-labels-rotated?))

(define gnc:html-linechart-set-row-labels-rotated?!
  (record-modifier <html-linechart> 'row-labels-rotated?))

(define gnc:html-linechart-stacked?
  (record-accessor <html-linechart> 'stacked?))

(define gnc:html-linechart-set-stacked?!
  (record-modifier <html-linechart> 'stacked?))

(define gnc:html-linechart-markers?
  (record-accessor <html-linechart> 'markers?))

(define gnc:html-linechart-set-markers?!
  (record-modifier <html-linechart> 'markers?))

(define gnc:html-linechart-major-grid?
  (record-accessor <html-linechart> 'major-grid?))

(define gnc:html-linechart-set-major-grid?!
  (record-modifier <html-linechart> 'major-grid?))

(define gnc:html-linechart-minor-grid?
  (record-accessor <html-linechart> 'minor-grid?))

(define gnc:html-linechart-set-minor-grid?!
  (record-modifier <html-linechart> 'minor-grid?))

(define gnc:html-linechart-col-labels
  (record-accessor <html-linechart> 'col-labels))

(define gnc:html-linechart-set-col-labels!
  (record-modifier <html-linechart> 'col-labels))

(define gnc:html-linechart-col-colors
  (record-accessor <html-linechart> 'col-colors))

(define gnc:html-linechart-set-col-colors!
  (record-modifier <html-linechart> 'col-colors))

(define gnc:html-linechart-legend-reversed?
  (record-accessor <html-linechart> 'legend-reversed?))

(define gnc:html-linechart-set-legend-reversed?!
  (record-modifier <html-linechart> 'legend-reversed?))

(define gnc:html-linechart-title
  (record-accessor <html-linechart> 'title))

(define gnc:html-linechart-set-title!
  (record-modifier <html-linechart> 'title))

(define gnc:html-linechart-subtitle
  (record-accessor <html-linechart> 'subtitle))

(define gnc:html-linechart-set-subtitle!
  (record-modifier <html-linechart> 'subtitle))

;; Note: ATM you can specify one url per column, but this url will be
;; used for all of the rows. Otherwise we could have cols*rows urls
;; (quite a lot), but this first requires fixing
;; guppi_line_1_callback() in gnome/gnc-html-guppi.c .
;; FIXME url's haven't been working since GnuCash 1.x
;;       GnuCash 2.x switched from guppy to goffice, which
;;       made it very hard to remain the url functionality
;;       At this point I (gjanssens) is in the process of
;;       moving from goffice to jqplot for our charts
;;       which perhaps may allow urls again in the charts
;;       I'm keeping the parameters below around to remind
;;       us this still has to be investigated again
(define gnc:html-linechart-button-1-line-urls
  (record-accessor <html-linechart> 'button-1-line-urls))

(define gnc:html-linechart-set-button-1-line-urls!
  (record-modifier <html-linechart> 'button-1-line-urls))

(define gnc:html-linechart-button-2-line-urls
  (record-accessor <html-linechart> 'button-2-line-urls))

(define gnc:html-linechart-set-button-2-line-urls!
  (record-modifier <html-linechart> 'button-2-line-urls))

(define gnc:html-linechart-button-3-line-urls
  (record-accessor <html-linechart> 'button-3-line-urls))

(define gnc:html-linechart-set-button-3-line-urls!
  (record-modifier <html-linechart> 'button-3-line-urls))

(define gnc:html-linechart-button-1-legend-urls
  (record-accessor <html-linechart> 'button-1-legend-urls))

(define gnc:html-linechart-set-button-1-legend-urls!
  (record-modifier <html-linechart> 'button-1-legend-urls))

(define gnc:html-linechart-button-2-legend-urls
  (record-accessor <html-linechart> 'button-2-legend-urls))

(define gnc:html-linechart-set-button-2-legend-urls!
  (record-modifier <html-linechart> 'button-2-legend-urls))

(define gnc:html-linechart-button-3-legend-urls
  (record-accessor <html-linechart> 'button-3-legend-urls))

(define gnc:html-linechart-set-button-3-legend-urls!
  (record-modifier <html-linechart> 'button-3-legend-urls))

(define gnc:html-linechart-line-width
  (record-accessor <html-linechart> 'line-width))

(define gnc:html-linechart-set-line-width!
  (record-modifier <html-linechart> 'line-width))

(define (gnc:html-linechart-append-row! linechart newrow)
  (let ((dd (gnc:html-linechart-data linechart)))
    (set! dd (append dd (list newrow)))
    (gnc:html-linechart-set-data! linechart dd)))

(define (gnc:html-linechart-prepend-row! linechart newrow)
  (let ((dd (gnc:html-linechart-data linechart)))
    (set! dd (cons newrow dd))
    (gnc:html-linechart-set-data! linechart dd)))

(define (gnc:html-linechart-append-column! linechart newcol)
  (let ((colnum 0)
        (rownum 0)
        (rows (gnc:html-linechart-data linechart))
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
           (gnc:html-linechart-append-row! linechart this-row)
           (list-set! (gnc:html-linechart-data linechart) rownum this-row))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-linechart-prepend-column! linechart newcol)
  (let ((rows (gnc:html-linechart-data linechart))
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
           (gnc:html-linechart-append-row! linechart (list elt))
           (list-set! (gnc:html-linechart-data linechart) rownum
                      (cons elt this-row)))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-linechart-render linechart doc)
  (let* ((chart (gnc:make-html-chart))
         (data (gnc:html-linechart-data linechart))
         (line-width (gnc:html-linechart-line-width linechart))
         (radius (if (gnc:html-linechart-markers? linechart) 3 0)))
    (cond
     ((and (pair? data) (gnc:not-all-zeros data))
      (gnc:html-chart-set-type! chart 'line)
      (gnc:html-chart-set-width! chart (gnc:html-linechart-width linechart))
      (gnc:html-chart-set-height! chart (gnc:html-linechart-height linechart))
      (gnc:html-chart-set-data-labels! chart (gnc:html-linechart-row-labels linechart))
      (for-each
       (lambda (label series color)
         (gnc:html-chart-add-data-series! chart label series color
                                          'borderWidth line-width
                                          'pointRadius radius
                                          'fill #f))
       (gnc:html-linechart-col-labels linechart)
       (apply zip data)
       (gnc:html-linechart-col-colors linechart))
      (gnc:html-chart-set-title! chart (list
                                        (gnc:html-linechart-title linechart)
                                        (gnc:html-linechart-subtitle linechart)))
      (gnc:html-chart-set-stacking?! chart (gnc:html-linechart-stacked? linechart))
      (gnc:html-chart-render chart doc))

     (else
      (gnc:warn "null-data, not rendering linechart")
      ""))))


(gnc:guard-html-chart gnc:html-linechart-data)
(gnc:guard-html-chart gnc:html-linechart-set-data!)
(gnc:guard-html-chart gnc:html-linechart-width)
(gnc:guard-html-chart gnc:html-linechart-set-width!)
(gnc:guard-html-chart gnc:html-linechart-height)
(gnc:guard-html-chart gnc:html-linechart-set-height!)
(gnc:guard-html-chart gnc:html-linechart-x-axis-label)
(gnc:guard-html-chart gnc:html-linechart-set-x-axis-label!)
(gnc:guard-html-chart gnc:html-linechart-y-axis-label)
(gnc:guard-html-chart gnc:html-linechart-set-y-axis-label!)
(gnc:guard-html-chart gnc:html-linechart-row-labels)
(gnc:guard-html-chart gnc:html-linechart-set-row-labels!)
(gnc:guard-html-chart gnc:html-linechart-row-labels-rotated?)
(gnc:guard-html-chart gnc:html-linechart-set-row-labels-rotated?!)
(gnc:guard-html-chart gnc:html-linechart-stacked?)
(gnc:guard-html-chart gnc:html-linechart-set-stacked?!)
(gnc:guard-html-chart gnc:html-linechart-markers?)
(gnc:guard-html-chart gnc:html-linechart-set-markers?!)
(gnc:guard-html-chart gnc:html-linechart-major-grid?)
(gnc:guard-html-chart gnc:html-linechart-set-major-grid?!)
(gnc:guard-html-chart gnc:html-linechart-minor-grid?)
(gnc:guard-html-chart gnc:html-linechart-set-minor-grid?!)
(gnc:guard-html-chart gnc:html-linechart-col-labels)
(gnc:guard-html-chart gnc:html-linechart-set-col-labels!)
(gnc:guard-html-chart gnc:html-linechart-col-colors)
(gnc:guard-html-chart gnc:html-linechart-set-col-colors!)
(gnc:guard-html-chart gnc:html-linechart-legend-reversed?)
(gnc:guard-html-chart gnc:html-linechart-set-legend-reversed?!)
(gnc:guard-html-chart gnc:html-linechart-title)
(gnc:guard-html-chart gnc:html-linechart-set-title!)
(gnc:guard-html-chart gnc:html-linechart-subtitle)
(gnc:guard-html-chart gnc:html-linechart-set-subtitle!)
(gnc:guard-html-chart gnc:html-linechart-button-1-line-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-1-line-urls!)
(gnc:guard-html-chart gnc:html-linechart-button-2-line-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-2-line-urls!)
(gnc:guard-html-chart gnc:html-linechart-button-3-line-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-3-line-urls!)
(gnc:guard-html-chart gnc:html-linechart-button-1-legend-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-1-legend-urls!)
(gnc:guard-html-chart gnc:html-linechart-button-2-legend-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-2-legend-urls!)
(gnc:guard-html-chart gnc:html-linechart-button-3-legend-urls)
(gnc:guard-html-chart gnc:html-linechart-set-button-3-legend-urls!)
(gnc:guard-html-chart gnc:html-linechart-append-row!)
(gnc:guard-html-chart gnc:html-linechart-prepend-row!)
(gnc:guard-html-chart gnc:html-linechart-append-column!)
(gnc:guard-html-chart gnc:html-linechart-prepend-column!)
(gnc:guard-html-chart gnc:html-linechart-render)
(gnc:guard-html-chart gnc:html-linechart-set-line-width!)
(gnc:guard-html-chart gnc:html-linechart-line-width)
