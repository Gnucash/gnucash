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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "html-piechart.scm")

(define <html-piechart>
  (make-record-type "<html-piechart>"
                    '(width height title subtitle data colors labels)))

(define gnc:html-piechart? 
  (record-predicate <html-piechart>))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-piechart> class
;;  generate the <object> form for a guppi piechart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-piechart-internal
  (record-constructor <html-piechart>))

(define (gnc:make-html-piechart)
  (gnc:make-html-piechart-internal -1 -1 #f #f #f #f #f))

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

(define (gnc:html-piechart-render piechart doc)
  (define (ensure-positive-numbers nlist)
    (map
     (lambda (elt)
       (cond ((number? elt)
              (abs elt))
             ((string? elt)
              (with-input-from-string elt
                (lambda ()
                  (let ((n (read)))
                    (if (number? n) (abs n) 0.0)))))
             ((gnc:gnc-numeric? elt)
              (abs (gnc:numeric-to-double elt)))
             (#t 
              0.0)))
     nlist))
  
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
  
  (with-output-to-string
    (lambda ()
      (let ((title (gnc:html-piechart-title piechart))
            (subtitle (gnc:html-piechart-subtitle piechart))
            (data 
             (ensure-positive-numbers (gnc:html-piechart-data piechart)))
            (labels 
             (catenate-escaped-strings (gnc:html-piechart-labels piechart)))
            (colors 
             (catenate-escaped-strings (gnc:html-piechart-colors piechart))))
        (if (and (list? data) 
                 (not (null? data)))
            (begin 
              (display "<object classid=\"gnc-guppi-pie\" width=")
              (display (gnc:html-piechart-width piechart))
              (display " height=") 
              (display (gnc:html-piechart-height piechart))
              (display ">\n")
              (if title
                  (begin 
                    (display "  <param name=\"title\" value=\"")
                    (display title) (display "\">\n")))
              (if subtitle
                  (begin 
                    (display "  <param name=\"subtitle\" value=\"")
                    (display subtitle) (display "\">\n")))
              (if (and data (list? data))
                  (begin 
                    (display "  <param name=\"datasize\" value=\"")
                    (display (length data)) (display "\">\n")
                    (display "  <param name=\"data\" value=\"")
                    (for-each-in-order 
                     (lambda (datum)
                       (display datum)
                       (display " "))
                     data)
                    (display "\">\n")))
              (if (and (string? colors)
                       (> (string-length colors) 0))
                  (begin 
                    (display "  <param name=\"colors\" value=\"")
                    (dispaly colors)
                    (display "\">\n")))
              (if (and (string? labels)
                       (> (string-length labels) 0))
                  (begin 
                    (display "  <param name=\"labels\" value=\"")
                    (display labels)
                    (display "\">\n")))
              (display "Unable to display pie chart\n")
              (display "</object>"))
            " ")))))

