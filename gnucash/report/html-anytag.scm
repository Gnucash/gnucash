;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-anytag.scm : generate HTML programmatically, with support
;; for simple style elements. 
;; Copyright 2018 Christopher Lam
;;
;; This module allows any html tag to be created such as
;; <div> <span>
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

(define-module (gnucash report html-anytag))

(use-modules (srfi srfi-9))
(use-modules (gnucash core-utils))
(use-modules (gnucash report html-style-info))
(use-modules (gnucash report html-document))

(export <html-anytag>)
(export gnc:html-anytag?)
(export gnc:html-anytag-data)
(export gnc:html-anytag-set-data!)
(export gnc:html-anytag-style)
(export gnc:html-anytag-append-data!)
(export gnc:html-anytag-set-style!)
(export gnc:html-anytag-render div doc)
(export gnc:make-html-div)
(export gnc:make-html-div/markup)
(export gnc:make-html-span)
(export gnc:make-html-span/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-anytag> class
;;  wrapper around HTML anytags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <html-anytag>
  (make-html-anytag tag data style)
  html-anytag?
  (tag html-anytag-tag html-anytag-set-tag!)
  (data html-anytag-data html-anytag-set-data!)
  (style html-anytag-style html-anytag-set-style!))

(define gnc:html-anytag? html-anytag?)
(define gnc:make-html-anytag-internal make-html-anytag)
(define gnc:html-anytag-tag html-anytag-tag)
(define gnc:html-anytag-set-tag! html-anytag-set-tag!)
(define gnc:html-anytag-data html-anytag-data)
(define gnc:html-anytag-set-data! html-anytag-set-data!)
(define gnc:html-anytag-style html-anytag-style)
(define gnc:html-anytag-set-style-internal! html-anytag-set-style!)

(define (gnc:make-html-anytag tag . data)
  (gnc:make-html-anytag-internal tag data (gnc:make-html-style-table)))

(define (gnc:html-anytag-append-data! anytag . data)
  (gnc:html-anytag-set-data!
   anytag (append (gnc:html-anytag-data anytag) data)))

(define (gnc:html-anytag-set-style! anytag tag . rest)
  (let ((style (gnc:html-anytag-style anytag))
        (newstyle (if (and (= (length rest) 2)
                           (procedure? (car rest)))
                      (apply gnc:make-html-data-style-info rest)
                      (apply gnc:make-html-markup-style-info rest))))
    (gnc:html-style-table-set! style tag newstyle)))

(define (gnc:html-anytag-render anytag doc)
  (let* ((tag (gnc:html-anytag-tag anytag))
         (retval '())
         (push (lambda (l) (set! retval (cons l retval)))))
    ;; compile the anytag style to make other compiles faster
    (gnc:html-style-table-compile
     (gnc:html-anytag-style anytag) (gnc:html-document-style-stack doc))

    (gnc:html-document-push-style doc (gnc:html-anytag-style anytag))

    (push (gnc:html-document-markup-start doc tag #t))
    (let ((data (gnc:html-anytag-data anytag)))
      (for-each
       (lambda (datum) (push (gnc:html-object-render datum doc)))
       (if (list? data)
           data
           (list data))))
    (push (gnc:html-document-markup-end doc tag))

    (gnc:html-document-pop-style doc)

    retval))

(define (gnc:make-html-div/markup class . data)
  (let ((anytag (apply gnc:make-html-anytag "div" data)))
    (gnc:html-anytag-set-style! anytag "div"
                                'attribute (list "class" class))
    anytag))

(define (gnc:make-html-span/markup class . data)
  (let ((anytag (apply gnc:make-html-anytag "span" data)))
    (gnc:html-anytag-set-style! anytag "span"
                                'attribute (list "class" class))
    anytag))

;;ideally should have been (gnc:make-html-anytag "div" data) but it
;;will inherit parent div class.
(define (gnc:make-html-div . data)
  (apply gnc:make-html-div/markup (cons "" data)))

(define (gnc:make-html-span . data)
  (apply gnc:make-html-span/markup (cons "" data)))
