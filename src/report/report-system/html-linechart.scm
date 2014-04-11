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

(define <html-linechart>
  (make-record-type "<html-linechart>"
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
                      button-3-legend-urls)))

(define gnc:html-linechart? 
  (record-predicate <html-linechart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-linechart> class
;;  generate the <object> form for a guppi linechart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-linechart-internal
  (record-constructor <html-linechart>))

(define (gnc:make-html-linechart)
  (gnc:make-html-linechart-internal -1 -1 #f #f #f #f '() '() '() 
				   #f #f #f #f #f #f '() 
				   #f #f #f #f #f #f))

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

(define (gnc:not-all-zeros data)
  (define (myor list)
    (begin 
      (gnc:debug "list" list)
      (if (null? list) #f
	  (or (car list) (myor (cdr list))))))

  (cond ((number? data) (not (= 0 data)))
	((list? data) (myor (map gnc:not-all-zeros data)))
	(else #f)))

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
         (title (gnc:html-linechart-title linechart))
         (subtitle (gnc:html-linechart-subtitle linechart))
         (url-1
          (catenate-escaped-strings 
           (gnc:html-linechart-button-1-line-urls linechart)))
         (url-2
          (catenate-escaped-strings 
           (gnc:html-linechart-button-2-line-urls linechart)))
         (url-3
          (catenate-escaped-strings 
           (gnc:html-linechart-button-3-line-urls linechart)))
         (legend-1
          (catenate-escaped-strings 
           (gnc:html-linechart-button-1-legend-urls linechart)))
         (legend-2
          (catenate-escaped-strings 
           (gnc:html-linechart-button-2-legend-urls linechart)))
         (legend-3
          (catenate-escaped-strings 
           (gnc:html-linechart-button-3-legend-urls linechart)))
         (x-label (gnc:html-linechart-x-axis-label linechart))
         (y-label (gnc:html-linechart-y-axis-label linechart))
         (data (gnc:html-linechart-data linechart))
	 (dummy1 (gnc:debug "data " data))
         (row-labels (catenate-escaped-strings 
                      (gnc:html-linechart-row-labels linechart)))
         (col-labels (catenate-escaped-strings 
                      (gnc:html-linechart-col-labels linechart)))
         (col-colors (catenate-escaped-strings 
                      (gnc:html-linechart-col-colors linechart))))
    (if (and (list? data)
             (not (null? data))
	     (gnc:not-all-zeros data))
        (begin 
          (push "<object classid=\"gnc-guppi-line\" width=")
          (push (gnc:html-linechart-width linechart))
          (push " height=") 
          (push (gnc:html-linechart-height linechart))
          (push ">\n")
          (if title
              (begin 
                (push "  <param name=\"title\" value=\"")
                (push title) (push "\">\n")))
          (if subtitle
              (begin 
                (push "  <param name=\"subtitle\" value=\"")
                (push subtitle) (push "\">\n")))
          (if url-1
              (begin 
                (push "  <param name=\"line_urls_1\" value=\"")
                (push url-1)
                (push "\">\n")))
          (if url-2
              (begin 
                (push "  <param name=\"line_urls_2\" value=\"")
                (push url-2)
                (push "\">\n")))
          (if url-3
              (begin 
                (push "  <param name=\"line_urls_3\" value=\"")
                (push url-3)
                (push "\">\n")))
          (if legend-1 
              (begin 
                (push "  <param name=\"legend_urls_1\" value=\"")
                (push legend-1)
                (push "\">\n")))
          (if legend-2
              (begin 
                (push "  <param name=\"legend_urls_2\" value=\"")
                (push legend-2)
                (push "\">\n")))
          (if legend-3 
              (begin 
                (push "  <param name=\"legend_urls_3\" value=\"")
                (push legend-3)
                (push "\">\n")))
          (if (and data (list? data))
              (let ((rows (length data))
                    (cols 0))
                (push "  <param name=\"data_rows\" value=\"")
                (push rows) (push "\">\n")
                (if (list? (car data))
                    (begin 
                      (set! cols (length (car data)))
                      (push "  <param name=\"data_cols\" value=\"")
                      (push cols)
                      (push "\">\n")))           
                (push "  <param name=\"data\" value=\"")
                (let loop ((col 0))
                  (for-each 
                   (lambda (row)
                     (push (ensure-numeric (list-ref-safe row col)))
                     (push " "))
                   data)
                  (if (< col (- cols 1))
                      (loop (+ 1 col))))
                (push "\">\n")))
          (if (and (string? x-label) (> (string-length x-label) 0))
              (begin 
                (push "  <param name=\"x_axis_label\" value=\"")
                (push x-label)
                (push "\">\n")))
          (if (and (string? y-label) (> (string-length y-label) 0))
              (begin 
                (push "  <param name=\"y_axis_label\" value=\"")
                (push y-label)
                (push "\">\n")))
          (if (and (string? col-colors) (> (string-length col-colors) 0))
              (begin 
                (push "  <param name=\"col_colors\" value=\"")
                (push col-colors)
                (push "\">\n")))
          (if (and (string? row-labels) (> (string-length row-labels) 0))
              (begin 
                (push "  <param name=\"row_labels\" value=\"")
                (push row-labels)
                (push "\">\n")))
          (if (and (string? col-labels) (> (string-length col-labels) 0))
              (begin 
                (push "  <param name=\"col_labels\" value=\"")
                (push col-labels)
                (push "\">\n")))
	  (push "  <param name=\"rotate_row_labels\" value=\"")
	  (push (if (gnc:html-linechart-row-labels-rotated? linechart)
		    "1\">\n"
		    "0\">\n"))
	  (push "  <param name=\"stacked\" value=\"")
	  (push (if (gnc:html-linechart-stacked? linechart)
		    "1\">\n"
		    "0\">\n"))
          (push "  <param name=\"markers\" value=\"")
	  (push (if (gnc:html-linechart-markers? linechart)
		    "1\">\n"
		    "0\">\n"))
          (push "  <param name=\"major_grid\" value=\"")
	  (push (if (gnc:html-linechart-major-grid? linechart)
		    "1\">\n"
		    "0\">\n"))
          (push "  <param name=\"minor_grid\" value=\"")
	  (push (if (gnc:html-linechart-minor-grid? linechart)
		    "1\">\n"
		    "0\">\n"))
	  (push "  <param name=\"legend_reversed\" value=\"")
	  (push (if (gnc:html-linechart-legend-reversed? linechart)
		    "1\">\n"
		    "0\">\n"))
          (push "Unable to push line chart\n")
          (push "</object> &nbsp;\n"))
        (begin 
	  (gnc:warn "linechart has no non-zero data.")
	  " "))
    retval))
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

(define <html-linechart>
  (make-record-type "<html-linechart>"
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
                      button-3-legend-urls)))

(define gnc:html-linechart?
  (record-predicate <html-linechart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-linechart> class
;;  generate the <object> form for a guppi linechart.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-linechart-internal
  (record-constructor <html-linechart>))

(define (gnc:make-html-linechart)
  (gnc:make-html-linechart-internal -1 -1 #f #f #f #f '() '() '()
                                  #f #f #f #f #f #f '()
                                  #f #f #f #f #f #f))

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

(define (gnc:not-all-zeros data)
  (define (myor list)
    (begin
      (gnc:debug "list" list)
      (if (null? list) #f
         (or (car list) (myor (cdr list))))))

  (cond ((number? data) (not (= 0 data)))
       ((list? data) (myor (map gnc:not-all-zeros data)))
       (else #f)))

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
         (title (gnc:html-linechart-title linechart))
         (subtitle (gnc:html-linechart-subtitle linechart))
         (url-1
          (catenate-escaped-strings
           (gnc:html-linechart-button-1-line-urls linechart)))
         (url-2
          (catenate-escaped-strings
           (gnc:html-linechart-button-2-line-urls linechart)))
         (url-3
          (catenate-escaped-strings
           (gnc:html-linechart-button-3-line-urls linechart)))
         (legend-1
          (catenate-escaped-strings
           (gnc:html-linechart-button-1-legend-urls linechart)))
         (legend-2
          (catenate-escaped-strings
           (gnc:html-linechart-button-2-legend-urls linechart)))
         (legend-3
          (catenate-escaped-strings
           (gnc:html-linechart-button-3-legend-urls linechart)))
         (x-label (gnc:html-linechart-x-axis-label linechart))
         (y-label (gnc:html-linechart-y-axis-label linechart))
         (data (gnc:html-linechart-data linechart))
        (dummy1 (gnc:debug "data " data))
         (row-labels (catenate-escaped-strings
                      (gnc:html-linechart-row-labels linechart)))
         (col-labels (catenate-escaped-strings
                      (gnc:html-linechart-col-labels linechart)))
         (col-colors (catenate-escaped-strings
                      (gnc:html-linechart-col-colors linechart))))
    (if (and (list? data)
             (not (null? data))
            (gnc:not-all-zeros data))
        (begin
          (push "<object classid=\"gnc-guppi-line\" width=")
          (push (gnc:html-linechart-width linechart))
          (push " height=")
          (push (gnc:html-linechart-height linechart))
          (push ">\n")
          (if title
              (begin
                (push "  <param name=\"title\" value=\"")
                (push title) (push "\">\n")))
          (if subtitle
              (begin
                (push "  <param name=\"subtitle\" value=\"")
                (push subtitle) (push "\">\n")))
          (if url-1
              (begin
                (push "  <param name=\"line_urls_1\" value=\"")
                (push url-1)
                (push "\">\n")))
          (if url-2
              (begin
                (push "  <param name=\"line_urls_2\" value=\"")
                (push url-2)
                (push "\">\n")))
          (if url-3
              (begin
                (push "  <param name=\"line_urls_3\" value=\"")
                (push url-3)
                (push "\">\n")))
          (if legend-1
              (begin
                (push "  <param name=\"legend_urls_1\" value=\"")
                (push legend-1)
                (push "\">\n")))
          (if legend-2
              (begin
                (push "  <param name=\"legend_urls_2\" value=\"")
                (push legend-2)
                (push "\">\n")))
          (if legend-3
              (begin
                (push "  <param name=\"legend_urls_3\" value=\"")
                (push legend-3)
                (push "\">\n")))
          (if (and data (list? data))
              (let ((rows (length data))
                    (cols 0))
                (push "  <param name=\"data_rows\" value=\"")
                (push rows) (push "\">\n")
                (if (list? (car data))
                    (begin
                      (set! cols (length (car data)))
                      (push "  <param name=\"data_cols\" value=\"")
                      (push cols)
                      (push "\">\n")))
                (push "  <param name=\"data\" value=\"")
                (let loop ((col 0))
                  (for-each
                   (lambda (row)
                     (push (ensure-numeric (list-ref-safe row col)))
                     (push " "))
                   data)
                  (if (< col (- cols 1))
                      (loop (+ 1 col))))
                (push "\">\n")))
          (if (and (string? x-label) (> (string-length x-label) 0))
              (begin
                (push "  <param name=\"x_axis_label\" value=\"")
                (push x-label)
                (push "\">\n")))
          (if (and (string? y-label) (> (string-length y-label) 0))
              (begin
                (push "  <param name=\"y_axis_label\" value=\"")
                (push y-label)
                (push "\">\n")))
          (if (and (string? col-colors) (> (string-length col-colors) 0))
              (begin
                (push "  <param name=\"col_colors\" value=\"")
                (push col-colors)
                (push "\">\n")))
          (if (and (string? row-labels) (> (string-length row-labels) 0))
              (begin
                (push "  <param name=\"row_labels\" value=\"")
                (push row-labels)
                (push "\">\n")))
          (if (and (string? col-labels) (> (string-length col-labels) 0))
              (begin
                (push "  <param name=\"col_labels\" value=\"")
                (push col-labels)
                (push "\">\n")))
         (push "  <param name=\"rotate_row_labels\" value=\"")
         (push (if (gnc:html-linechart-row-labels-rotated? linechart)
                   "1\">\n"
                   "0\">\n"))
         (push "  <param name=\"stacked\" value=\"")
         (push (if (gnc:html-linechart-stacked? linechart)
                   "1\">\n"
                   "0\">\n"))
          (push "  <param name=\"markers\" value=\"")
         (push (if (gnc:html-linechart-markers? linechart)
                   "1\">\n"
                   "0\">\n"))
          (push "  <param name=\"major_grid\" value=\"")
         (push (if (gnc:html-linechart-major-grid? linechart)
                   "1\">\n"
                   "0\">\n"))
          (push "  <param name=\"minor_grid\" value=\"")
         (push (if (gnc:html-linechart-minor-grid? linechart)
                   "1\">\n"
                   "0\">\n"))
         (push "  <param name=\"legend_reversed\" value=\"")
         (push (if (gnc:html-linechart-legend-reversed? linechart)
                   "1\">\n"
                   "0\">\n"))
          (push "Unable to push line chart\n")
          (push "</object> &nbsp;\n"))
        (begin
         (gnc:warn "linechart has no non-zero data.")
         " "))
    retval))
