;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-style-sheet.scm : generate HTML programmatically, with support
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

(use-modules (gnucash gettext))

(define *gnc:_style-sheet-templates_* (make-hash-table 23))
(define *gnc:_style-sheets_* (make-hash-table 23))

(define <html-style-sheet-template> 
  (make-record-type "<html-style-sheet-template>" 
                    '(version name options-generator renderer)))

(define gnc:html-style-sheet-template? 
  (record-predicate <html-style-sheet-template>))

(define gnc:html-style-sheet-template-version
  (record-accessor <html-style-sheet-template> 'version))

(define gnc:html-style-sheet-template-set-version!
  (record-modifier <html-style-sheet-template> 'version))

(define gnc:html-style-sheet-template-name
  (record-accessor <html-style-sheet-template> 'name))

(define gnc:html-style-sheet-template-set-name!
  (record-modifier <html-style-sheet-template> 'name))

(define gnc:html-style-sheet-template-options-generator
  (record-accessor <html-style-sheet-template> 'options-generator))

(define gnc:html-style-sheet-template-set-options-generator!
  (record-modifier <html-style-sheet-template> 'options-generator))

(define gnc:html-style-sheet-template-renderer
  (record-accessor <html-style-sheet-template> 'renderer))

(define gnc:html-style-sheet-template-set-renderer!
  (record-modifier <html-style-sheet-template> 'renderer))

(define (gnc:html-style-sheet-template-find tname)
  (hash-ref *gnc:_style-sheet-templates_* tname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  define-html-style-sheet 
;;  actually defines an <html-style-sheet-template>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:define-html-style-sheet . args)
  (let ((ss 
         ((record-constructor <html-style-sheet-template>) #f #f #f #f)))
    (let loop ((left args))
      (if (and (list? left)
               (not (null? left))
               (not (null? (cdr left))))
          (let* ((field (car left))
                 (value (cadr left))
                 (mod (record-modifier <html-style-sheet-template> field)))
            (mod ss value)
            (loop (cddr left)))))
    
    ;; store the style sheet template 
    (hash-set! *gnc:_style-sheet-templates_* 
               (gnc:html-style-sheet-template-name ss) 
               ss)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <html-style-sheet> methods 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-style-sheet> 
  (make-record-type "<html-style-sheet>" 
                    '(name type options renderer style)))

(define gnc:html-style-sheet? 
  (record-predicate <html-style-sheet>))

(define gnc:html-style-sheet-name
  (record-accessor <html-style-sheet> 'name))

(define gnc:html-style-sheet-set-name!
  (record-modifier <html-style-sheet> 'name))

(define gnc:html-style-sheet-type
  (record-accessor <html-style-sheet> 'type))

(define gnc:html-style-sheet-set-type!
  (record-modifier <html-style-sheet> 'type))

(define gnc:html-style-sheet-options
  (record-accessor <html-style-sheet> 'options))

(define gnc:html-style-sheet-set-options!
  (record-modifier <html-style-sheet> 'options))

(define gnc:html-style-sheet-renderer
  (record-accessor <html-style-sheet> 'renderer))

(define gnc:html-style-sheet-set-renderer!
  (record-modifier <html-style-sheet> 'renderer))

(define gnc:make-html-style-sheet-internal
  (record-constructor <html-style-sheet>))

(define gnc:html-style-sheet-style
  (record-accessor <html-style-sheet> 'style))

(define gnc:current-saved-stylesheets
  (gnc-build-dotgnucash-path "stylesheets-2.0"))

(define (gnc:save-style-sheet-options) 
  (let ((port (false-if-exception
               (open gnc:current-saved-stylesheets
                     (logior O_WRONLY O_CREAT O_TRUNC)))))
    (if (not port)
        (gnc:warn (_ "Can't save style sheet"))
        (begin
          (hash-fold
           (lambda (id ss-obj p)
             (let ((code 
                    (string-append 
                     (format #f "(let ((template (gnc:html-style-sheet-template-find ~S)))\n" 
                             (gnc:html-style-sheet-type ss-obj))
                     "  (if template \n"
                     "    (let ((options ((gnc:html-style-sheet-template-options-generator template)))) \n"
                     (gnc:generate-restore-forms 
                      (gnc:html-style-sheet-options ss-obj) "options")
                     (format #f " (gnc:restore-html-style-sheet ~S ~S options))))\n"
                             (gnc:html-style-sheet-name ss-obj)
                             (gnc:html-style-sheet-type ss-obj)))))
               (display code port))
             #f) #f *gnc:_style-sheets_*)
          (close port)))))

(define (gnc:html-style-sheet-set-style! sheet tag . rest)
  (let ((newstyle #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (gnc:html-style-table-set! 
     (gnc:html-style-sheet-style sheet) tag newstyle)))

(define (gnc:make-html-style-sheet template-name style-sheet-name)
  (let* ((template (gnc:html-style-sheet-template-find template-name)))
    (if template
        (let ((rv (gnc:make-html-style-sheet-internal 
                   style-sheet-name template-name 
                   ((gnc:html-style-sheet-template-options-generator template))
                   (gnc:html-style-sheet-template-renderer template)
                   (gnc:make-html-style-table))))
          ;; set up the fallback data styles for every rendered document 
          (gnc:html-style-sheet-set-style! 
           rv "<string>" 
           gnc:default-html-string-renderer #f)
          
          (gnc:html-style-sheet-set-style! 
           rv "<gnc-numeric>" 
           gnc:default-html-gnc-numeric-renderer #f)
          
          (gnc:html-style-sheet-set-style!
           rv "<number>" 
           gnc:default-html-number-renderer #f)
          
          (gnc:html-style-sheet-set-style!
           rv "<gnc-monetary>" 
           gnc:default-html-gnc-monetary-renderer #f)

          ;; store it in the style sheet hash 
          (hash-set! *gnc:_style-sheets_* style-sheet-name rv)
          rv)
        #f)))

(define (gnc:restore-html-style-sheet style-sheet-name template-name options)
  (let* ((template (gnc:html-style-sheet-template-find template-name)))
    (if template
        (let ((rv (gnc:make-html-style-sheet-internal 
                   style-sheet-name template-name 
                   options
                   (gnc:html-style-sheet-template-renderer template)
                   (gnc:make-html-style-table))))
          ;; set up the fallback data styles for every rendered document 
          (gnc:html-style-sheet-set-style! 
           rv "<string>" 
           gnc:default-html-string-renderer #f)
          
          (gnc:html-style-sheet-set-style! 
           rv "<gnc-numeric>" 
           gnc:default-html-gnc-numeric-renderer #f)
          
          (gnc:html-style-sheet-set-style!
           rv "<number>" 
           gnc:default-html-number-renderer #f)
          
          (gnc:html-style-sheet-set-style!
           rv "<gnc-monetary>" 
           gnc:default-html-gnc-monetary-renderer #f)
          
          ;; store it in the style sheet hash 
          (hash-set! *gnc:_style-sheets_* style-sheet-name rv)
          rv)
        #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-style-sheet-render 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-style-sheet-render sheet doc . rest)
  ;; render the document (returns an <html-document>)
  (let ((newdoc ((gnc:html-style-sheet-renderer sheet) 
                 (gnc:html-style-sheet-options sheet)
                 doc))
        (headers? (if (null? rest) #f (if (car rest) #t #f))))

    ;; Copy values over to stylesheet-produced document.  note that this is a
    ;; bug that should probably better be fixed by having the stylesheets
    ;; emit documents that are correct.  this, however, is a slightly easier
    ;; place to enforce it. :p
    (gnc:html-document-set-title! newdoc (gnc:html-document-title doc))
    (gnc:html-document-set-headline! newdoc (gnc:html-document-headline doc))
    
    ;; push the style sheet's default styles 
    (gnc:html-document-push-style newdoc (gnc:html-style-sheet-style sheet))
    
    ;; swap the original document's default styles with the style 
    ;; sheet document styles 
    ;; ... first push the style sheet template document's style on the 
    ;; stack (compiling before pushing)
    (gnc:html-style-table-compile (gnc:html-document-style newdoc)
                                  (gnc:html-document-style-stack newdoc))
    (gnc:html-document-push-style newdoc (gnc:html-document-style newdoc))
    
    ;; ... then set the rendered document's style to be the user
    ;; document style, which will get pushed when the trivial style
    ;; sheet renders it
    (gnc:html-document-set-style-internal! 
     newdoc (gnc:html-document-style doc))
    
    ;; render the ssdocument (using the trivial stylesheet).  since
    ;; the objects from 'doc' are now in newdoc, this renders the whole
    ;; package.
    (gnc:html-document-render newdoc headers?)))

(define (gnc:get-html-style-sheets)
  (let* ((ss '()))
    (hash-for-each (lambda (k v) (set! ss (cons v ss))) 
                   *gnc:_style-sheets_*)
    (sort ss
          (lambda (a b)
            (string<? (gnc:html-style-sheet-name a)
                      (gnc:html-style-sheet-name b))))))

(define (gnc:get-html-templates)
  (let* ((ss '()))
    (hash-for-each (lambda (k v) 
                     (set! ss (cons v ss))) 
                   *gnc:_style-sheet-templates_*)
    (sort ss
          (lambda (a b)
            (string<? (gnc:html-style-sheet-template-name a)
                      (gnc:html-style-sheet-template-name b))))))

(define (gnc:html-style-sheet-find tname)
  (hash-ref *gnc:_style-sheets_* tname))

(define (gnc:html-style-sheet-remove sheet)
  (if (not (string=? (gnc:html-style-sheet-name sheet) (N_ "Default")))
      (hash-remove! *gnc:_style-sheets_* (gnc:html-style-sheet-name sheet))))
