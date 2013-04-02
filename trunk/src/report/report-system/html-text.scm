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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-text> class
;;  just plain-old text.  some utilities to add markup.
;;  markup is done lazily (at rendering time) to allow for various styles.
;;  the markup just returns a thunk which you can call later with the
;;  doc as arg to get the string out. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash printf))

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
   (gnc:make-html-style-table)))

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
    (gnc:html-style-table-set! (gnc:html-text-style text) tag newstyle)))

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

;; This creates an open html tag which must be explicitly closed later.
(define (gnc:html-markup/open-tag-only tag . entities)
  (lambda (doc)
    (apply gnc:html-text-render-markup-noclose
           doc
           tag
           #f
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

;; I'm not entirely pleased about the way this works, but I can't
;; really see a way around it.  It still works within the style
;; system, but it flattens out its children's lists prematurely.  Has
;; to, to pass them as args to sprintf.

(define (gnc:html-markup/format format . entities)
  (lambda (doc)
    (apply 
     sprintf #f format 
     (map 
      (lambda (elt)
        (let ((rendered-elt 
               (cond ((procedure? elt)
                      (elt doc))
                     ((gnc:html-object? elt)
                      (gnc:html-object-render elt doc))
                     (#t 
                      (gnc:html-document-render-data doc elt)))))
          (cond 
           ((string? rendered-elt)
            rendered-elt)
           ((list? rendered-elt)
            (apply string-append
                   (gnc:html-document-tree-collapse rendered-elt)))
           (#t 
            (format "hold on there podner. form='~s'\n" rendered-elt)
            ""))))
      entities))))

(define (gnc:html-markup-p . rest)
  (apply gnc:html-markup "p" rest))

(define (gnc:html-markup-tt . rest)
  (apply gnc:html-markup "tt" rest))

(define (gnc:html-markup-em . rest)
  (apply gnc:html-markup "em" rest))

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
       (display "src=\"") (display src) (display"\"")
       (display " ")
       (for-each 
        (lambda (kvp)
          (display (car kvp))
          (display "=\"")
          (display (cadr kvp))
          (display "\" "))
        rest)))))

(define (gnc:html-text-render p doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval)))))
    (gnc:html-style-table-compile (gnc:html-text-style p)
                                  (gnc:html-document-style-stack doc))
    (gnc:html-document-push-style doc (gnc:html-text-style p))
    (for-each 
     (lambda (elt)
       (cond ((procedure? elt)
              (push (elt doc)))
             (#t 
              (push (gnc:html-document-render-data doc elt)))))
     (gnc:html-text-body p))
    (gnc:html-document-pop-style doc)
    (gnc:html-style-table-uncompile (gnc:html-text-style p))
    retval))

;; XXX It would be better to merge this with the original html-text-render-markup below it,
;; but that would require a fair amount of work to refactor so that it works correctly.
(define (gnc:html-text-render-markup-noclose doc markup attrib end-tag? . entities)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval)))))
    (push (gnc:html-document-markup-start doc markup end-tag? attrib))
    (for-each 
     (lambda (elt)
       (cond ((procedure? elt)
              (push (elt doc)))
             (#t 
              (push (gnc:html-document-render-data doc elt)))))
     entities)
    retval))

(define (gnc:html-text-render-markup doc markup attrib end-tag? . entities)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval)))))
    (push (gnc:html-document-markup-start doc markup end-tag? attrib))
    (for-each 
     (lambda (elt)
       (cond ((procedure? elt)
              (push (elt doc)))
             (#t 
              (push (gnc:html-document-render-data doc elt)))))
     entities)
    (if end-tag? 
        (push (gnc:html-document-markup-end doc markup)))
    retval))


