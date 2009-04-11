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
                      ;; "none", "circle", "diamond", "cross", "x",
                      ;; "square", "asterisk" and some more.
                      ;; The full list can be found in
                      ;; goffice/goffice/utils/go-marker.c, marker_shapes[]
                      ;; Marker names prefixed by filled, e.g. "filled square",
                      ;; are filled in the same color as the outline
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
         (title (gnc:html-scatter-title scatter))
         (subtitle (gnc:html-scatter-subtitle scatter))
         (x-label (gnc:html-scatter-x-axis-label scatter))
         (y-label (gnc:html-scatter-y-axis-label scatter))
         (data (gnc:html-scatter-data scatter))
         (marker (gnc:html-scatter-marker scatter))
         (markercolor (string-append "#" (gnc:html-scatter-markercolor scatter))))
    (if (and (list? data)
             (not (null? data)))
        (begin 
          (push "<object classid=\"")(push GNC-CHART-SCATTER)(push "\" width=")
          (push (gnc:html-scatter-width scatter))
          (push " height=") 
          (push (gnc:html-scatter-height scatter))
          (push ">\n")
          (if title
              (begin 
                (push "  <param name=\"title\" value=\"")
                (push title) (push "\">\n")))
          (if subtitle
              (begin 
                (push "  <param name=\"subtitle\" value=\"")
                (push subtitle) (push "\">\n")))
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
          (if marker
              (begin 
                (push "  <param name=\"marker\" value=\"")
                (push marker)
		(push "\">\n")))
          (if markercolor
              (begin 
                (push "  <param name=\"color\" value=\"")
                (push markercolor)
		(push "\">\n")))
          (if (and data (list? data))
              (let ((datasize (length data))
		    (x-data (map-in-order car data))
		    (y-data (map-in-order cadr data)))
                (push "  <param name=\"datasize\" value=\"")
                (push datasize) (push "\">\n")
                (push "  <param name=\"x_data\" value=\"")
		(for-each (lambda (x)
				     (push (ensure-numeric x))
				     (push " "))
				   x-data)
                (push "\">\n")
		(push "  <param name=\"y_data\" value=\"")
		(for-each (lambda (x)
				     (push (ensure-numeric x))
				     (push " "))
				   y-data)
		(push "\">\n")))
          (push "Unable to push bar chart\n")
          (push "</object> &nbsp;\n"))
        " ")
    retval))
