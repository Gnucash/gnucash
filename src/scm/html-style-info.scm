;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-style-info.scm : generate HTML programmatically, with support
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

(gnc:support "html-style-info.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <html-markup-style-info> class 
;; this is what's stored for tags in the style hash tables. 
;; literal data types have their own style record type,
;; <html-data-style-info>
;;
;;  constructor takes pairs of args :
;;  (gnc:make-html-markup-style-info field1 value1 field2 value2 ...)
;; 
;;  values for field (should be passed as symbols):
;;  tag         : string for start tag
;;  attributes  : hash of attribute to value (unsafe!)
;;  attribute   : single attribute-value pair in a list 
;;  font-face   : string for <font face="">
;;  font-size   : string for <font size="">
;;  font-color  : color (a valid HTML color spec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-markup-style-info> 
  (make-record-type "<html-markup-style-info>"
                    '(tag 
                      attributes
                      font-face 
                      font-size 
                      font-color
                      inheritable?)))

(define gnc:html-markup-style-info?
  (record-predicate <html-markup-style-info>))

(define gnc:make-html-markup-style-info-internal 
  (record-constructor <html-markup-style-info>))

(define (gnc:make-html-markup-style-info . rest)
  (let ((retval (gnc:make-html-markup-style-info-internal 
                 #f (make-hash-table 7) #f #f #f #t)))
    (apply gnc:html-markup-style-info-set! retval rest)
    retval))

(define (gnc:html-markup-style-info-set! style . rest)
  (let loop ((arglist rest))
    (if (and (list? arglist)
             (not (null? arglist))
             (not (null? (cdr arglist))))
        (let* ((field (car arglist))
               (value (cadr arglist)))
          (if (eq? field 'attribute)
              (if (list? value)
                  (gnc:html-markup-style-info-set-attribute!
                   style (car value) (cadr value)))
              (let ((modifier 
                     (record-modifier <html-markup-style-info> field)))
                (modifier style value)))
          (loop (cddr arglist)))))
  style)

(define gnc:html-markup-style-info-tag
  (record-accessor <html-markup-style-info> 'tag))

(define gnc:html-markup-style-info-set-tag!
  (record-modifier <html-markup-style-info> 'tag))

(define gnc:html-markup-style-info-attributes
  (record-accessor <html-markup-style-info> 'attributes))

(define gnc:html-markup-style-info-set-attributes!
  (record-modifier <html-markup-style-info> 'attributes))

(define gnc:html-markup-style-info-font-face
  (record-accessor <html-markup-style-info> 'font-face))

(define gnc:html-markup-style-info-set-font-face!
  (record-modifier <html-markup-style-info> 'font-face))

(define gnc:html-markup-style-info-font-size
  (record-accessor <html-markup-style-info> 'font-size))

(define gnc:html-markup-style-info-set-font-size!
  (record-modifier <html-markup-style-info> 'font-size))

(define gnc:html-markup-style-info-font-color
  (record-accessor <html-markup-style-info> 'font-color))

(define gnc:html-markup-style-info-set-font-color!
  (record-modifier <html-markup-style-info> 'font-color))

(define gnc:html-markup-style-info-inheritable? 
  (record-accessor <html-markup-style-info> 'inheritable?))

(define gnc:html-markup-style-info-set-inheritable?!
  (record-modifier <html-markup-style-info> 'inheritable?))

(define (gnc:html-markup-style-info-set-attribute! info attr val)
  (hash-set! (gnc:html-markup-style-info-attributes info) attr val))

(define (gnc:html-markup-style-info-merge s1 s2) 
  (if (not (gnc:html-markup-style-info? s1))
      s2    
      (if (not (gnc:html-markup-style-info? s2))
          s1
          (let ((st
                 (gnc:make-html-markup-style-info-internal
                  (gnc:html-markup-style-info-tag s1)
                  (make-hash-table 7)
                  (gnc:html-markup-style-info-font-face s1)
                  (gnc:html-markup-style-info-font-size s1)
                  (gnc:html-markup-style-info-font-color s1)
                  (gnc:html-markup-style-info-inheritable? s1))))

            ;; merge the tag name and attributes.  If the child is
            ;; overriding the parent's key, don't inherit the parent's
            ;; attributes.
            (let ((s1t (gnc:html-markup-style-info-tag s1))
                  (s2t (gnc:html-markup-style-info-tag s2)))
              (if (not s1t)
                  (begin 
                    (gnc:html-markup-style-info-set-tag! st s2t)
                    ;; merge the attributes 
                    (hash-fold 
                     (lambda (key value prior)
                       (gnc:html-markup-style-info-set-attribute! 
                        st key value)
                       #t)
                     #t
                     (gnc:html-markup-style-info-attributes s2))
                    (hash-fold 
                     (lambda (key value prior)
                       (gnc:html-markup-style-info-set-attribute! 
                        st key value)
                       #t)
                     #t
                     (gnc:html-markup-style-info-attributes s1)))))
            
            (let ((s1s (gnc:html-markup-style-info-font-face s1))
                  (s2s (gnc:html-markup-style-info-font-face s2)))
              (if (not s1s)
                  (gnc:html-markup-style-info-set-font-face! st s2s)))
            (let ((s1s (gnc:html-markup-style-info-font-size s1))
                  (s2s (gnc:html-markup-style-info-font-size s2)))
              (if (not s1s)
                  (gnc:html-markup-style-info-set-font-size! st s2s)))
            (let ((s1s (gnc:html-markup-style-info-font-color s1))
                  (s2s (gnc:html-markup-style-info-font-color s2)))
              (if (not s1s)
                  (gnc:html-markup-style-info-set-font-color! st s2s)))
            st))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-data-style-info> class 
;;
;;  literal data is rendered using the html-data-style for that type.
;;  ATM the rendering style is defined using a thunk to do the
;;  rendering and some data to pass in addition to the object to be
;;  rendered.
;;
;;  the renderer is a function of two arguments.  The first arg is the
;;  data to be rendered and the second is the 'data' specified in the
;;  style.  The return should be an HTML string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-data-style-info> 
  (make-record-type "<html-data-style-info>"
                    '(renderer data))) 

(define gnc:html-data-style-info? 
  (record-predicate <html-data-style-info>))

(define gnc:make-html-data-style-info
  (record-constructor <html-data-style-info>))

(define gnc:html-data-style-info? 
  (record-predicate <html-data-style-info>))

(define gnc:html-data-style-info-renderer 
  (record-accessor <html-data-style-info> 'renderer))

(define gnc:html-data-style-info-set-renderer!
  (record-modifier <html-data-style-info> 'renderer))

(define gnc:html-data-style-info-data 
  (record-accessor <html-data-style-info> 'data))

(define gnc:html-data-style-info-set-data!
  (record-modifier <html-data-style-info> 'data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  default renderers for some data types.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:default-html-string-renderer datum params)
  datum)

(define (gnc:default-html-gnc-numeric-renderer datum params)
;  (gnc:numeric-to-string datum))
 (sprintf #f "%.2f" (gnc:numeric-to-double datum)))

(define (gnc:default-html-number-renderer datum params)
  (sprintf #f "%.2f" datum))


