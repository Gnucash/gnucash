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

(define-module (gnucash report html-style-info))

(use-modules (ice-9 match))
(use-modules (srfi srfi-9))
(use-modules (gnucash utilities))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))

(export <html-data-style-info>)
(export <html-markup-style-info>)
(export <html-style-table>)
(export gnc:default-html-gnc-monetary-renderer)
(export gnc:default-html-gnc-numeric-renderer)
(export gnc:default-html-number-renderer)
(export gnc:default-html-string-renderer)
(export gnc:html-data-style-info-data)
(export gnc:html-data-style-info-inheritable?)
(export gnc:html-data-style-info-merge)
(export gnc:html-data-style-info-renderer)
(export gnc:html-data-style-info-set-data!)
(export gnc:html-data-style-info-set-inheritable?!)
(export gnc:html-data-style-info-set-renderer!)
(export gnc:html-data-style-info?)
(export gnc:html-data-style-info?)
(export gnc:html-markup-style-info-attributes)
(export gnc:html-markup-style-info-inheritable?)
(export gnc:html-markup-style-info-merge)
(export gnc:html-markup-style-info-set!)
(export gnc:html-markup-style-info-set-attribute!)
(export gnc:html-markup-style-info-set-attributes!)
(export gnc:html-markup-style-info-set-inheritable?!)
(export gnc:html-markup-style-info-set-tag!)
(export gnc:html-markup-style-info-tag)
(export gnc:html-markup-style-info?)
(export gnc:html-style-info-merge)
(export gnc:html-style-table-compile)
(export gnc:html-style-table-compiled)
(export gnc:html-style-table-compiled?)
(export gnc:html-style-table-fetch)
(export gnc:html-style-table-inheritable)
(export gnc:html-style-table-primary)
(export gnc:html-style-table-set!)
(export gnc:html-style-table-set-compiled!)
(export gnc:html-style-table-set-inheritable!)
(export gnc:html-style-table-uncompile)
(export gnc:html-style-table?)
(export gnc:make-html-data-style-info)
(export gnc:make-html-data-style-info-internal)
(export gnc:make-html-markup-style-info)
(export gnc:make-html-markup-style-info-internal)
(export gnc:make-html-style-table)
(export gnc:make-html-style-table-internal)
(export gnc:default-price-renderer)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <html-markup-style-info>
  (make-html-markup-style-info-internal tag attributes inheritable?)
  html-markup-style-info?
  (tag style-info-tag style-info-set-tag)
  (attributes style-info-attributes style-info-set-attributes)
  (inheritable? style-info-inheritable? style-info-set-inheritable?))

(define gnc:make-html-markup-style-info-internal make-html-markup-style-info-internal)
(define gnc:html-markup-style-info? html-markup-style-info?)
(define gnc:html-markup-style-info-tag style-info-tag)
(define gnc:html-markup-style-info-set-tag! style-info-set-tag)
(define gnc:html-markup-style-info-attributes style-info-attributes)
(define gnc:html-markup-style-info-set-attributes! style-info-set-attributes)
(define gnc:html-markup-style-info-inheritable? style-info-inheritable?)
(define gnc:html-markup-style-info-set-inheritable?! style-info-set-inheritable?)

(define (gnc:make-html-markup-style-info . rest)
  (let ((retval (gnc:make-html-markup-style-info-internal #f (make-hash-table) #t)))
    (apply gnc:html-markup-style-info-set! retval rest)
    retval))

(define (gnc:html-markup-style-info-set! style . rest)
  (define allowable-fields (record-type-fields <html-markup-style-info>))
  (define (not-a-field? fld) (not (memq fld allowable-fields)))
  (let loop ((arglist rest))
    (match arglist
      (('attribute (key val) . rest)
       (gnc:html-markup-style-info-set-attribute! style key val)
       (loop rest))

      (('attribute (key) . rest)
       (gnc:html-markup-style-info-set-attribute! style key #f)
       (loop rest))

      (((? not-a-field? fld) . _)
       (gnc:error "gnc:html-markup-style-info-set! " fld " is not a valid field"))

      ((field value . rest)
       ((record-modifier <html-markup-style-info> field) style value)
       (loop rest))

      (else style))))

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

(define-record-type <html-data-style-info>
  (make-html-data-style-info-internal renderer data inheritable?)
  data-style-info?
  (renderer html-data-style-info-renderer html-data-style-info-set-renderer)
  (data html-data-style-info-data html-data-style-info-set-data)
  (inheritable? html-data-style-info-inherit html-data-style-info-set-inherit))

(define gnc:make-html-data-style-info-internal make-html-data-style-info-internal)
(define gnc:html-data-style-info? data-style-info?)
(define gnc:html-data-style-info-renderer html-data-style-info-renderer)
(define gnc:html-data-style-info-set-renderer! html-data-style-info-set-renderer)
(define gnc:html-data-style-info-data html-data-style-info-data)
(define gnc:html-data-style-info-set-data! html-data-style-info-set-data)
(define gnc:html-data-style-info-inheritable? html-data-style-info-inherit)
(define gnc:html-data-style-info-set-inheritable?! html-data-style-info-set-inherit)

(define (gnc:make-html-data-style-info renderer data)
  (gnc:make-html-data-style-info-internal renderer data #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  default renderers for some data types.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:default-html-string-renderer datum params)
  datum)

(define (gnc:default-html-gnc-numeric-renderer datum params)
  (xaccPrintAmount datum (gnc-default-print-info #f)))

;; renders a price to target currency
(define (gnc:default-price-renderer currency price)
  (xaccPrintAmount price (gnc-price-print-info currency #t)))

(define (gnc:default-html-gnc-monetary-renderer datum params)
  (let* ((comm (gnc:gnc-monetary-commodity datum))
         (scu (gnc-commodity-get-fraction comm))
         (amount (gnc:gnc-monetary-amount datum))
         (amt-display (if (exact? amount)
                          (gnc-numeric-convert amount scu GNC-HOW-RND-ROUND)
                          amount)))
    (xaccPrintAmount amt-display (gnc-commodity-print-info comm #t))))

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

(define-record-type <html-style-table>
  (make-html-style-table primary compiled inheritable)
  html-style-table?
  (primary html-style-table-primary)
  (compiled html-style-table-compiled html-style-table-set-compiled!)
  (inheritable html-style-table-inheritable html-style-table-set-inheritable!))

(define gnc:html-style-table? html-style-table?)
(define gnc:make-html-style-table-internal make-html-style-table)
(define gnc:html-style-table-primary html-style-table-primary)
(define gnc:html-style-table-set-compiled! html-style-table-set-compiled!)
(define gnc:html-style-table-inheritable html-style-table-inheritable)
(define gnc:html-style-table-set-inheritable! html-style-table-set-inheritable!)
(define gnc:html-style-table-compiled html-style-table-compiled)
(define gnc:html-style-table-compiled? gnc:html-style-table-compiled)
(define (gnc:make-html-style-table)
  (gnc:make-html-style-table-internal (make-hash-table) #f #f))

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
