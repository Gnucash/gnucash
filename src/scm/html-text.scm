;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-text.scm : generate HTML programmatically, with support
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

(require 'record)
(require 'hash-table)

(gnc:support "html-text.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-text> class
;;  just plain-old text.  some utilities to add markup.
;;  markup is done lazily (at rendering time) to allow for various styles.
;;  the markup just returns a thunk which you can call later with the
;;  doc as arg to get the string out. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-text> 
  (make-record-type "<html-text>"
                    '(body style)))
(define gnc:html-text? 
  (record-predicate <html-text>))

(define gnc:make-html-text-internal
  (record-constructor <html-text>))

(define (gnc:make-html-text . body)
  (gnc:make-html-text-internal 
   body
   (make-hash-table 7)))

(define gnc:html-text? 
  (record-predicate <html-text>))

(define gnc:html-text-body
  (record-accessor <html-text> 'body))

(define gnc:html-text-set-body-internal!
  (record-modifier <html-text> 'body))

(define (gnc:html-text-set-body! txt . rest)
  (gnc:html-text-set-body-internal! txt rest))

(define gnc:html-text-style
  (record-accessor <html-text> 'style))

(define gnc:html-text-set-style-internal!
  (record-modifier <html-text> 'style))

(define (gnc:html-text-set-style! text tag . rest)
  (let ((newstyle #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (hash-set! (gnc:html-text-style text) tag newstyle)))

(define (gnc:html-text-append! text . body) 
  (gnc:html-text-set-body-internal!
   text
   (append (gnc:html-text-body text) body)))

(define (gnc:html-markup tag . entities)
  (lambda (doc)
    (apply gnc:html-text-render-markup
           doc
           tag
           #f
           #t
           entities)))

(define (gnc:html-markup/attr tag attr . entities)
  (lambda (doc)
    (apply gnc:html-text-render-markup
           doc
           tag
           attr
           #t
           entities)))

(define (gnc:html-markup/no-end tag . entities)
  (lambda (doc)
    (apply gnc:html-text-render-markup
           doc
           tag
           #f
           #f
           entities)))

(define (gnc:html-markup/attr/no-end tag attr . entities)
  (lambda (doc)
    (apply gnc:html-text-render-markup
           doc
           tag
           attr
           #f
           entities)))

(define (gnc:html-markup-p . rest)
  (apply gnc:html-markup "p" rest))

(define (gnc:html-markup-tt . rest)
  (apply gnc:html-markup "tt" rest))

(define (gnc:html-markup-b . rest)
  (apply gnc:html-markup "b" rest))

(define (gnc:html-markup-i . rest)
  (apply gnc:html-markup "i" rest))

(define (gnc:html-markup-h1 . rest)
  (apply gnc:html-markup "h1" rest))

(define (gnc:html-markup-h2 . rest)
  (apply gnc:html-markup "h2" rest))

(define (gnc:html-markup-h3 . rest)
  (apply gnc:html-markup "h3" rest))

(define (gnc:html-markup-br)
  (gnc:html-markup/no-end "br"))

(define (gnc:html-markup-hr)
  (gnc:html-markup/no-end "hr"))

(define (gnc:html-markup-ul items)
  (apply gnc:html-markup "ul"
         (map 
          (lambda (obj)
            (gnc:html-markup "li" obj))
          items)))


(define (gnc:html-markup-anchor href . rest)
  (apply gnc:html-markup/attr 
         "a" 
         (string-append "href=\"" href "\"")
         rest))

(define (gnc:html-markup-img src . rest)
  (gnc:html-markup/attr/no-end 
   "img" 
   (with-output-to-string
     (lambda ()
       (display "src=") (display src)
       (display " ")
       (for-each 
        (lambda (kvp)
          (display (car kvp))
          (display "=")
          (display (cadr kvp))
          (display " "))
        rest)))))

(define (gnc:html-text-render p doc)
  (with-output-to-string
    (lambda ()
      (gnc:html-document-push-style doc (gnc:html-text-style p))
      (for-each-in-order 
       (lambda (elt)
         (if (procedure? elt)
             (display (elt doc))
             (display (gnc:html-document-render-data doc elt))))
       (gnc:html-text-body p))
      (gnc:html-document-pop-style doc))))

(define (gnc:html-text-render-markup doc markup attrib end-tag? . entities)
  (with-output-to-string 
    (lambda ()
      (display (gnc:html-document-markup-start doc markup attrib))
      (for-each-in-order 
       (lambda (elt) 
         (if (procedure? elt)
             (display (elt doc))
             (display (gnc:html-document-render-data doc elt))))
       entities)
      (if end-tag? 
          (display (gnc:html-document-markup-end doc markup))))))

