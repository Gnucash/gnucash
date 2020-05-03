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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 match))

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
;;  font-face   : string for <font face=""> (deprecate)
;;  font-size   : string for <font size=""> (deprecate)
;;  font-color  : color (a valid HTML color spec) (deprecate)
;;  closing-font-tag: private data (computed from font-face,
;;                                  font-size, font-color)
;;                    don't set directly, please! (deprecate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <html-markup-style-info> 
  (make-record-type "<html-markup-style-info>"
                    '(tag 
                      attributes
                      font-face 
                      font-size 
                      font-color
		      closing-font-tag?
                      inheritable?)))

(define gnc:html-markup-style-info?
  (record-predicate <html-markup-style-info>))

(define gnc:make-html-markup-style-info-internal 
  (record-constructor <html-markup-style-info>))

(define (gnc:make-html-markup-style-info . rest)
  (let ((retval (gnc:make-html-markup-style-info-internal 
                 #f (make-hash-table) #f #f #f #f #t)))
    (apply gnc:html-markup-style-info-set! retval rest)
    retval))

(define (gnc:html-markup-style-info-set! style . rest)
  (let loop ((arglist rest))
    (match arglist
      (('attribute (key . val) . rest)
       (gnc:html-markup-style-info-set-attribute!
        style key (and (pair? val) (car val)))
       (loop rest))

      ((field value . rest)
       (when (memq field '(font-size font-face font-color))
         (issue-deprecation-warning "font-face/size/color deprecated.")
         (gnc:html-markup-style-info-set-closing-font-tag! style (and value #t)))
       ((record-modifier <html-markup-style-info> field) style value)
       (loop rest))

      (else style))))

(define gnc:html-markup-style-info-tag
  (record-accessor <html-markup-style-info> 'tag))

(define gnc:html-markup-style-info-set-tag!
  (record-modifier <html-markup-style-info> 'tag))

(define gnc:html-markup-style-info-attributes
  (record-accessor <html-markup-style-info> 'attributes))

(define gnc:html-markup-style-info-set-attributes!
  (record-modifier <html-markup-style-info> 'attributes))

(define gnc:html-markup-style-info-font-face
  ;; deprecated
  (record-accessor <html-markup-style-info> 'font-face))

(define gnc:html-markup-style-info-set-font-face-internal!
  ;; deprecated
  (record-modifier <html-markup-style-info> 'font-face))

(define (gnc:html-markup-style-info-set-font-face! record value)
  (issue-deprecation-warning
   "gnc:html-markup-style-info-set-font-face! is unused")
  (gnc:html-markup-style-info-set-closing-font-tag! record value)
  (gnc:html-markup-style-info-set-font-face-internal! record value))

(define gnc:html-markup-style-info-font-size
  ;; deprecated
  (record-accessor <html-markup-style-info> 'font-size))

(define gnc:html-markup-style-info-set-font-size-internal!
  ;; deprecated
  (record-modifier <html-markup-style-info> 'font-size))

(define (gnc:html-markup-style-info-set-font-size! record value)
  (issue-deprecation-warning
   "gnc:html-markup-style-info-set-font-size! is unused")
  (gnc:html-markup-style-info-set-closing-font-tag! record value)
  (gnc:html-markup-style-info-set-font-size-internal! record value))

(define gnc:html-markup-style-info-font-color
  ;; deprecated
  (record-accessor <html-markup-style-info> 'font-color))

(define gnc:html-markup-style-info-set-font-color-internal!
  ;; deprecated
  (record-modifier <html-markup-style-info> 'font-color))

(define (gnc:html-markup-style-info-set-font-color! record value)
  (issue-deprecation-warning
   "gnc:html-markup-style-info-set-font-color! is unused")
  (begin
    (gnc:html-markup-style-info-set-closing-font-tag!
     record (not (eq? value #f)))
    (gnc:html-markup-style-info-set-font-color-internal! record value)))

(define gnc:html-markup-style-info-closing-font-tag
  ;; deprecated
  (record-accessor <html-markup-style-info> 'closing-font-tag?))

(define gnc:html-markup-style-info-set-closing-font-tag!
  ;; deprecated
  (record-modifier <html-markup-style-info> 'closing-font-tag?))

(define gnc:html-markup-style-info-inheritable? 
  (record-accessor <html-markup-style-info> 'inheritable?))

(define gnc:html-markup-style-info-set-inheritable?!
  (record-modifier <html-markup-style-info> 'inheritable?))

(define (gnc:html-markup-style-info-set-attribute! info attr val)
  (hash-set! (gnc:html-markup-style-info-attributes info) attr val))

(define (gnc:html-markup-style-info-merge s1 s2) 
  (cond
   ((not (gnc:html-markup-style-info? s1)) s2)
   ((not (gnc:html-markup-style-info? s2)) s1)
   (else
    (gnc:make-html-markup-style-info-internal
     ;; tag
     (or (gnc:html-markup-style-info-tag s1)
         (gnc:html-markup-style-info-tag s2))
     ;; attributes: if the child is overriding the
     ;; parent tag, don't initialize the attribute table
     ;; to the parent's attributes.  Otherwise, load
     ;; parent attrs then load child attrs over them.
     (let ((ht (make-hash-table)))
       (unless (gnc:html-markup-style-info-tag s1)
         (hash-for-each
          (lambda (k v)
            (hash-set! ht k v))
          (gnc:html-markup-style-info-attributes s2)))
       (hash-for-each
        (lambda (k v) (hash-set! ht k v))
        (gnc:html-markup-style-info-attributes s1))
       ht)
     ;; font face
     (or (gnc:html-markup-style-info-font-face s1)
         (gnc:html-markup-style-info-font-face s2))
     ;; font size
     (or (gnc:html-markup-style-info-font-size s1)
         (gnc:html-markup-style-info-font-size s2))
     ;; color
     (or (gnc:html-markup-style-info-font-color s1)
         (gnc:html-markup-style-info-font-color s2))
     ;; closing font tag
     (or (gnc:html-markup-style-info-closing-font-tag s1)
         (gnc:html-markup-style-info-closing-font-tag s2))
     ;; inheritable (get this always from child)
     (gnc:html-markup-style-info-inheritable? s1)))))

(define (gnc:html-style-info-merge s1 s2)
  (cond
   ((or (gnc:html-markup-style-info? s1) (gnc:html-markup-style-info? s2))
    (gnc:html-markup-style-info-merge s1 s2))

   ((or (gnc:html-data-style-info? s1) (gnc:html-data-style-info? s2))
    (gnc:html-data-style-info-merge s1 s2))

   (else #f)))

(define (gnc:html-data-style-info-merge s1 s2)
  (if (gnc:html-data-style-info? s1) s1 s2))

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
                    '(renderer data inheritable?))) 

(define gnc:html-data-style-info? 
  (record-predicate <html-data-style-info>))

(define gnc:make-html-data-style-info-internal
  (record-constructor <html-data-style-info>))

(define (gnc:make-html-data-style-info renderer data)
  (gnc:make-html-data-style-info-internal renderer data #t))

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

(define gnc:html-data-style-info-inheritable? 
  (record-accessor <html-data-style-info> 'inheritable?))

(define gnc:html-data-style-info-set-inheritable?!
  (record-modifier <html-data-style-info> 'inheritable?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  default renderers for some data types.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:default-html-string-renderer datum params)
  datum)

(define (gnc:default-html-gnc-numeric-renderer datum params)
  (xaccPrintAmount datum (gnc-default-print-info #f)))

(define (gnc:default-html-gnc-monetary-renderer datum params)
  (xaccPrintAmount                                                 
   (gnc:gnc-monetary-amount datum)                                    
   (gnc-commodity-print-info (gnc:gnc-monetary-commodity datum) #t)))

(define (gnc:default-html-number-renderer datum params)  
  (xaccPrintAmount
   (double-to-gnc-numeric datum 100 GNC-RND-ROUND)
   (gnc-default-print-info #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <html-style-table> class
;;
;; this used to just be bare hash tables stuck in the <html-object>
;; but since we now support caching and compilation I think it 
;; deserves a record structure. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-style-table>
  (make-record-type "<html-style-table>"
                    '(primary compiled inheritable)))

(define gnc:html-style-table? 
  (record-predicate <html-style-table>))

(define gnc:make-html-style-table-internal 
  (record-constructor <html-style-table>))

(define (gnc:make-html-style-table)
  (gnc:make-html-style-table-internal (make-hash-table) #f #f))

(define gnc:html-style-table-primary
  (record-accessor <html-style-table> 'primary))

(define gnc:html-style-table-compiled
  (record-accessor <html-style-table> 'compiled))

(define gnc:html-style-table-set-compiled!
  (record-modifier <html-style-table> 'compiled))

(define gnc:html-style-table-inheritable
  (record-accessor <html-style-table> 'inheritable))

(define gnc:html-style-table-set-inheritable!
  (record-modifier <html-style-table> 'inheritable))

(define (gnc:html-style-table-compiled? table)
  (gnc:html-style-table-compiled table))

(define (gnc:html-style-table-compile table antecedents)
  ;; merge a key-value pair from an antecedent into the 
  ;; compiled table.  Only add values to the inheritable table
  ;; that are inheritable. 
  (define (key-merger key value ign)
    (let* ((compiled (gnc:html-style-table-compiled table))
           (inheritable (gnc:html-style-table-inheritable table))
           (old-val (hash-ref compiled key))
           (new-val (gnc:html-style-info-merge old-val value)))
      (hash-set! compiled key new-val)
      (if (and (gnc:html-markup-style-info? value)
               (gnc:html-markup-style-info-inheritable? value))
          (hash-set! inheritable key new-val))
      (if (and (gnc:html-data-style-info? value)
               (gnc:html-data-style-info-inheritable? value))
          (hash-set! inheritable key new-val))))
  
  ;; walk up the list of antecedents merging in style info
  (define (compile-worker table-list)
    (let ((next (car table-list)))
      (if (gnc:html-style-table-compiled? next)
          (begin
            (hash-fold key-merger #f (gnc:html-style-table-compiled next))
            #t)
          (begin 
            (hash-fold key-merger #f (gnc:html-style-table-primary next))
            (if (not (null? (cdr table-list)))
                (compile-worker (cdr table-list))
                #t)))))
  ;; make the compiled hash table 
  (gnc:html-style-table-set-compiled! table (make-hash-table))
  (gnc:html-style-table-set-inheritable! table (make-hash-table))
  
  ;; merge the contents of the primary hash into the compiled table 
  (hash-fold key-merger #f (gnc:html-style-table-primary table))
  
  ;; now merge in the antecedents 
  (if (not (null? antecedents))
      (compile-worker antecedents)))

  
(define (gnc:html-style-table-uncompile table)
  (gnc:html-style-table-set-compiled! table #f)
  (gnc:html-style-table-set-inheritable! table #f))

(define (gnc:html-style-table-fetch table antecedents markup)
  (define (get-inheritable-style ht)
    (let ((s (hash-ref ht markup)))
      (if (or (and (gnc:html-markup-style-info? s)
                   (gnc:html-markup-style-info-inheritable? s))
              (and (gnc:html-data-style-info? s)
                   (gnc:html-data-style-info-inheritable? s)))
          s #f)))

  (define (fetch-worker style antecedents)
    (cond
     ((null? antecedents) style)
     ((not (car antecedents)) (fetch-worker style (cdr antecedents)))
     ((gnc:html-style-table-compiled? (car antecedents))
      (gnc:html-style-info-merge
       style (hash-ref (gnc:html-style-table-inheritable (car antecedents)) markup)))
     (else
      (fetch-worker
       (gnc:html-style-info-merge
        style (get-inheritable-style
               (gnc:html-style-table-primary (car antecedents))))
       (cdr antecedents)))))

  (if (and table (gnc:html-style-table-compiled? table))
      (hash-ref (gnc:html-style-table-compiled table) markup)
      (fetch-worker
       (and table (hash-ref (gnc:html-style-table-primary table) markup))
       antecedents)))

(define (gnc:html-style-table-set! table markup style-info)
  (hash-set! (gnc:html-style-table-primary table) markup style-info))
