;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-document.scm : generate HTML programmatically, with support
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


(gnc:support "html-document.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-document> class 
;;  this is the top-level object representing an entire HTML document.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-document> 
  (make-record-type "<html-document>" 
                    '(style-sheet style-stack style title objects)))

(define gnc:html-document? 
  (record-predicate <html-document>))

(define gnc:make-html-document-internal
  (record-constructor <html-document>))

(define (gnc:make-html-document)
  (gnc:make-html-document-internal 
   #f                    ;; the stylesheet 
   '()                   ;; style stack
   (make-hash-table 7)   ;; document style info
   ""                    ;; document title
   '()                   ;; subobjects 
   ))

(define gnc:html-document-set-title!
  (record-modifier <html-document> 'title))

(define gnc:html-document-title
  (record-accessor <html-document> 'title))

(define gnc:html-document-set-style-sheet!
  (record-modifier <html-document> 'style-sheet))

(define gnc:html-document-style-sheet
  (record-accessor <html-document> 'style-sheet))

(define gnc:html-document-set-style-stack!
  (record-modifier <html-document> 'style-stack))

(define gnc:html-document-style-stack
  (record-accessor <html-document> 'style-stack))

(define gnc:html-document-set-style-internal!
  (record-modifier <html-document> 'style))

(define gnc:html-document-style
  (record-accessor <html-document> 'style))

(define gnc:html-document-set-objects!
  (record-modifier <html-document> 'objects))

(define gnc:html-document-objects
  (record-accessor <html-document> 'objects))

(define gnc:html-document?
  (record-predicate <html-document>))

(define (gnc:html-document-set-style! doc tag . rest)
  (let ((newstyle #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (hash-set! (gnc:html-document-style doc) tag newstyle)))


(define (gnc:html-document-render doc) 
  (let ((stylesheet (gnc:html-document-style-sheet doc)))
    (if stylesheet 
        ;; if there's a style sheet, let it do the rendering 
        (gnc:html-style-sheet-render stylesheet doc)
     
        ;; otherwise, do the trivial render. 
        (with-output-to-string 
          (lambda ()
            (gnc:html-document-push-style doc (gnc:html-document-style doc))
            (display "<html>\n")
            (display "<head>\n")
            (let ((title (gnc:html-document-title doc)))
              (if title 
                  (display (string-append "<title>" title "</title>\n"))))
            (display "</head>\n")

            ;; this lovely little number just makes sure that <body>
            ;; attributes like bgcolor get included 
            (display ((gnc:html-markup/no-end "body") doc))
            
            ;; now render the children
            (for-each-in-order 
             (lambda (child) 
               (display (gnc:html-object-render child doc)))
             (gnc:html-document-objects doc))
            (display "</body>\n")
            (display "</html>\n")
            (gnc:html-document-pop-style doc))))))
    
(define (gnc:html-document-push-style doc style)
  (gnc:html-document-set-style-stack! 
   doc (cons style (gnc:html-document-style-stack doc))))

(define (gnc:html-document-pop-style doc)
  (if (not (null? (gnc:html-document-style-stack doc)))
      (gnc:html-document-set-style-stack! 
       doc (cdr (gnc:html-document-style-stack doc)))))

(define (gnc:html-document-set-markup-style! doc tag style-info)
  (hash-set! (gnc:html-document-style doc) tag style-info)) 

(define (gnc:html-document-add-object! doc obj)
  (gnc:html-document-set-objects! 
   doc
   (append (gnc:html-document-objects doc) 
           (list (gnc:make-html-object obj)))))

(define (gnc:html-document-append-objects! doc objects)
  (gnc:html-document-set-objects!
   doc
   (append (gnc:html-document-objects doc) objects)))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  markup-rendering functions : markup-start and markup-end return
;;  pre-body and post-body HTML for the given markup tag.
;;  the optional rest arguments are lists of attribute-value pairs:
;;  (gnc:html-document-markup-start doc "markup" 
;;                                 '("attr1" "value1") '("attr2" "value2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-document-markup-start doc markup . rest)
  (let ((childinfo #f)
        (extra-attrib 
         (if (not (null? rest))
             rest #f))
        (show-result #f))
    
    ;; merge the style stack to get a complete markup-style-info 
    ;; record ... should we cache these? 
    (for-each-in-order 
     (lambda (newstyle)
       (if newstyle 
           (let ((parentinfo 
                  (hash-ref newstyle markup)))
             (if (and (gnc:html-markup-style-info? parentinfo)
                      (or 
                       (gnc:html-markup-style-info-inheritable? parentinfo)
                       (eq? newstyle 
                            (car (gnc:html-document-style-stack doc)))))
                 (set! childinfo 
                       (gnc:html-markup-style-info-merge 
                        childinfo parentinfo))))))
     (gnc:html-document-style-stack doc))
    
    (if (not childinfo) 
        (set! childinfo 
              (gnc:make-html-markup-style-info)))
    
    ;; now generate the start tag
    (let ((tag   (gnc:html-markup-style-info-tag childinfo))
          (attr  (gnc:html-markup-style-info-attributes childinfo))
          (face  (gnc:html-markup-style-info-font-face childinfo))
          (size  (gnc:html-markup-style-info-font-size childinfo))
          (color (gnc:html-markup-style-info-font-color childinfo)))

      ;; "" tags mean "show no tag"; #f tags means use default.
      (cond ((not tag)
             (set! tag markup))
            ((string=? tag "")
             (set! tag #f)))
      
      (with-output-to-string
        (lambda ()
          (if tag 
              (begin 
                (display "\n<")
                (display tag)
                (if attr
                    (hash-fold 
                     (lambda (key value prior)
                       (display (sprintf #f " %a=%a" key value))
                       #t)
                     #f
                     attr))
                (if extra-attrib
                    (for-each
                     (lambda (attr)
                       (if (string? attr)
                           (begin
                             (display " ") (display attr))))
                     extra-attrib))
                (display ">")))
          (if (or face size color)
              (begin 
                (display "<font ")
                (if face
                    (begin 
                      (display "face=\"") (display face) (display "\" ")))
                (if size
                    (begin 
                      (display "size=\"") (display size) (display "\" ")))
                (if color
                    (begin 
                      (display "color=\"") (display color) (display "\" ")))
                (display ">"))))))))

(define (gnc:html-document-markup-end doc markup)
  (let ((childinfo #f))
    ;; merge the style stack to get a complete markup-style-info 
    ;; record ... should we cache these? 
    (for-each-in-order 
     (lambda (newstyle)
       (if newstyle 
           (let ((parentinfo 
                  (hash-ref newstyle markup)))
             (if (and (gnc:html-markup-style-info? parentinfo)
                      (or
                       (gnc:html-markup-style-info-inheritable? parentinfo)
                       (eq? newstyle
                            (car (gnc:html-document-style-stack doc)))))
                 (set! childinfo 
                       (gnc:html-markup-style-info-merge 
                        childinfo parentinfo))))))
     (gnc:html-document-style-stack doc))
    
    (if (not childinfo) 
        (set! childinfo 
              (gnc:make-html-markup-style-info)))
    
    ;; now generate the end tag
    (let ((tag   (gnc:html-markup-style-info-tag childinfo))
          (face  (gnc:html-markup-style-info-font-face childinfo))
          (size  (gnc:html-markup-style-info-font-size childinfo))
          (color (gnc:html-markup-style-info-font-color childinfo)))
      ;; "" tags mean "show no tag"; #f tags means use default.
      (cond ((not tag)
             (set! tag markup))
            ((string=? tag "")
             (set! tag #f)))
      (with-output-to-string
        (lambda ()
          (if (or face size color)
              (display "</font>\n"))
          (if tag 
              (begin 
                (display "</")
                (display tag)
                ;; newline after every close tag... just temporary
                (display ">\n"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-document-render-data 
;;  looks up the relevant data style and renders the data accordingly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-document-render-data doc data)
  (let ((style-info #f)
        (data-type #f))
    (cond 
     ((number? data)
      (set! data-type "<number>"))
     ((string? data)
      (set! data-type "<string>"))
     ((boolean? data)
      (set! data-type "<boolean>"))
     ((record? data)
      (set! data-type (record-type-name (record-type-descriptor data)))))
    
    ;; for data, we just want to find the first style stack
    ;; frame that has a renderer 
    (let loop ((stack (gnc:html-document-style-stack doc)))
      (let* ((style (car stack))
             (info #f))
        (if style 
            (set! info (hash-ref style data-type)))
        (if (and info 
                 (gnc:html-data-style-info? info))
            (set! style-info info)
            (if (not (null? (cdr stack)))
                (loop (cdr stack))))))
    
    (if (not style-info)
        (set! style-info
              (gnc:make-html-data-style-info
               (lambda (datum parms)
                 (sprintf #f "%a %a" data-type datum))
               #f)))
    
    ((gnc:html-data-style-info-renderer style-info)
     data (gnc:html-data-style-info-data style-info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-object> class
;;  this is the parent of all the html object types.  You should not
;;  be creating <html-object> directly... use the specific type you
;;  want.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-object>
  (make-record-type "<html-object>"
                    '(renderer data)))
(define gnc:html-object? 
  (record-predicate <html-object>))

(define gnc:make-html-object-internal
  (record-constructor <html-object>))

(define (gnc:make-html-object obj)
  (let ((o #f))
    (if (not (record? obj))
        ;; for literals (strings/numbers)
        (set! o
              (gnc:make-html-object-internal 
               (lambda (obj doc)
                 (gnc:html-document-render-data doc obj))
               ;; if the object is #f, make it a placeholder
               (if obj obj "&nbsp;")))        
        (cond 
         ((gnc:html-text? obj)
          (set! o (gnc:make-html-object-internal 
                   gnc:html-text-render obj)))
         ((gnc:html-table? obj)
          (set! o (gnc:make-html-object-internal 
                   gnc:html-table-render obj)))
         ((gnc:html-table-cell? obj)
          (set! o (gnc:make-html-object-internal 
                   gnc:html-table-cell-render obj)))
         ((gnc:html-barchart? obj)
          (set! o (gnc:make-html-object-internal 
                   gnc:html-barchart-render obj)))
         ((gnc:html-piechart? obj)
          (set! o (gnc:make-html-object-internal 
                   gnc:html-piechart-render obj)))
         ((gnc:html-object? obj)
          (set! o obj))
         
         ;; other record types that aren't HTML objects 
         (#t 
          (set! o
                (gnc:make-html-object-internal 
                 (lambda (obj doc)
                   (gnc:html-document-render-data doc obj))
                 obj)))))
    o))

(define gnc:html-object-renderer
  (record-accessor <html-object> 'renderer))

(define gnc:html-object-set-renderer!
  (record-modifier <html-object> 'renderer))

(define gnc:html-object-data
  (record-accessor <html-object> 'data))

(define gnc:html-object-set-data!
  (record-modifier <html-object> 'data))

(define (gnc:html-object-render obj doc)
  (if (gnc:html-object? obj) 
      ((gnc:html-object-renderer obj) (gnc:html-object-data obj) doc)
      (let ((htmlo (gnc:make-html-object obj)))
        (gnc:html-object-render htmlo doc))))

