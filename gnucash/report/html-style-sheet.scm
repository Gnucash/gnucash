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

(define-module (gnucash report html-style-sheet))

(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash report html-document))
(use-modules (gnucash report html-style-info))

(export <html-style-sheet-template>)
(export gnc:html-style-sheet-template?)
(export gnc:html-style-sheet-template-version)
(export gnc:html-style-sheet-template-set-version!)
(export gnc:html-style-sheet-template-name)
(export gnc:html-style-sheet-template-set-name!)
(export gnc:html-style-sheet-template-options-generator)
(export gnc:html-style-sheet-template-set-options-generator!)
(export gnc:html-style-sheet-template-renderer)
(export gnc:html-style-sheet-template-set-renderer!)
(export gnc:html-style-sheet-template-find)
(export gnc:define-html-style-sheet)
(export <html-style-sheet>)
(export gnc:html-style-sheet?)
(export gnc:html-style-sheet-name)
(export gnc:html-style-sheet-set-name!)
(export gnc:html-style-sheet-type)
(export gnc:html-style-sheet-set-type!)
(export gnc:html-style-sheet-options)
(export gnc:html-style-sheet-set-options!)
(export gnc:html-style-sheet-renderer)
(export gnc:html-style-sheet-set-renderer!)
(export gnc:make-html-style-sheet-internal)
(export gnc:html-style-sheet-style)
(export gnc:html-style-sheet-set-style!)
(export gnc:make-html-style-sheet)
(export gnc:restore-html-style-sheet)
(export gnc:html-style-sheet-apply-changes)
(export gnc:html-style-sheet-render)
(export gnc:get-html-style-sheets)
(export gnc:get-html-templates)
(export gnc:html-style-sheet-find)
(export gnc:save-style-sheet-options)
(export gnc:html-style-sheet-remove)

(define *gnc:_style-sheet-templates_* (make-hash-table 23))
(define *gnc:_style-sheets_* (make-hash-table 23))

(define-record-type <html-style-sheet-template>
  (make-ss-template version name options-generator renderer)
  ss-template?
  (version ss-template-version ss-template-set-version!)
  (name ss-template-name ss-template-set-name!)
  (options-generator ss-template-options-generator ss-template-set-options-generator!)
  (renderer ss-template-renderer ss-template-set-renderer!))

(define gnc:html-style-sheet-template? ss-template?)
(define gnc:html-style-sheet-template-version ss-template-version)
(define gnc:html-style-sheet-template-set-version! ss-template-set-version!)
(define gnc:html-style-sheet-template-name ss-template-name)
(define gnc:html-style-sheet-template-set-name! ss-template-set-name!)
(define gnc:html-style-sheet-template-options-generator ss-template-options-generator)
(define gnc:html-style-sheet-template-set-options-generator! ss-template-set-options-generator!)
(define gnc:html-style-sheet-template-renderer ss-template-renderer)
(define gnc:html-style-sheet-template-set-renderer! ss-template-set-renderer!)

(define (gnc:html-style-sheet-template-find tname)
  (hash-ref *gnc:_style-sheet-templates_* tname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  define-html-style-sheet 
;;  actually defines an <html-style-sheet-template>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:define-html-style-sheet . args)
  (define allowable-fields (record-type-fields <html-style-sheet-template>))
  (define (not-a-field? fld) (not (memq fld allowable-fields)))
  (let loop ((args args) (ss (make-ss-template #f #f #f #f)))
    (match args
      (()
       (hash-set! *gnc:_style-sheet-templates_*
                  (gnc:html-style-sheet-template-name ss) ss))

      (((? not-a-field? fld) . _)
       (gnc:error "gnc:define-html-style-sheet " fld " is not a valid field"))

      ((field value . rest)
       ((record-modifier <html-style-sheet-template> field) ss value)
       (loop rest ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <html-style-sheet> methods 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <html-style-sheet>
  (make-html-ss name type options renderer style)
  html-ss?
  (name ss-name ss-set-name!)
  (type ss-type ss-set-type!)
  (options ss-options ss-set-options!)
  (renderer ss-renderer ss-set-renderer!)
  (style ss-style))

(define gnc:make-html-style-sheet-internal make-html-ss)
(define gnc:html-style-sheet? html-ss?)
(define gnc:html-style-sheet-name ss-name)
(define gnc:html-style-sheet-set-name! ss-set-name!)
(define gnc:html-style-sheet-type ss-type)
(define gnc:html-style-sheet-set-type! ss-set-type!)
(define gnc:html-style-sheet-options ss-options)
(define gnc:html-style-sheet-set-options! ss-set-options!)
(define gnc:html-style-sheet-renderer ss-renderer)
(define gnc:html-style-sheet-set-renderer! ss-set-renderer!)
(define gnc:html-style-sheet-style ss-style)

(define gnc:current-saved-stylesheets
  (gnc-build-userdata-path "stylesheets-2.0"))

(define (gnc:save-style-sheet-options)
  (let ((port (false-if-exception
               (open gnc:current-saved-stylesheets
                     (logior O_WRONLY O_CREAT O_TRUNC)))))
    (if (not port)
        (gnc:warn (G_ "Can't save style sheet"))
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
  (gnc:html-style-table-set!
   (gnc:html-style-sheet-style sheet) tag
   (if (and (= (length rest) 2)
            (procedure? (car rest)))
       (apply gnc:make-html-data-style-info rest)
       (apply gnc:make-html-markup-style-info rest))))

(define (make-html-style-sheet-internal template-name style-sheet-name options)
  (define template (gnc:html-style-sheet-template-find template-name))
  (define fallback-styles
    (list (cons "<string>" gnc:default-html-string-renderer)
          (cons "<gnc-numeric>" gnc:default-html-gnc-numeric-renderer)
          (cons "<number>" gnc:default-html-number-renderer)
          (cons ':gnc-monetary gnc:default-html-gnc-monetary-renderer)))
  (and template
       (let ((ss (gnc:make-html-style-sheet-internal
                  style-sheet-name template-name
                  (or options
                      ((gnc:html-style-sheet-template-options-generator template)))
                  (gnc:html-style-sheet-template-renderer template)
                  (gnc:make-html-style-table))))
         (for-each (lambda (pair)
                     (gnc:html-style-sheet-set-style! ss (car pair) (cdr pair) #f))
                   fallback-styles)
         (hash-set! *gnc:_style-sheets_* style-sheet-name ss)
         ss)))

(define (gnc:make-html-style-sheet template-name style-sheet-name)
  (make-html-style-sheet-internal template-name style-sheet-name #f))

(define (gnc:restore-html-style-sheet style-sheet-name template-name options)
  (make-html-style-sheet-internal template-name style-sheet-name options))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-style-sheet-render 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-style-sheet-render sheet doc . rest)
  ;; render the document (returns an <html-document>)
  (let ((newdoc ((gnc:html-style-sheet-renderer sheet) 
                 (gnc:html-style-sheet-options sheet)
                 doc))
        (headers? (and (pair? rest) (car rest))))

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
  (sort (map cdr (hash-map->list cons *gnc:_style-sheets_*))
        (lambda (a b)
          (gnc:string-locale<? (gnc:html-style-sheet-name a)
                               (gnc:html-style-sheet-name b)))))

(define (gnc:get-html-templates)
  (sort (map cdr (hash-map->list cons *gnc:_style-sheet-templates_*))
        (lambda (a b)
          (gnc:string-locale<? (gnc:html-style-sheet-template-name a)
                               (gnc:html-style-sheet-template-name b)))))

(define (gnc:html-style-sheet-find tname)
  (hash-ref *gnc:_style-sheets_* tname))

(define (gnc:html-style-sheet-remove sheet)
  (unless (string=? (gnc:html-style-sheet-name sheet) (N_ "Default"))
    (hash-remove! *gnc:_style-sheets_* (gnc:html-style-sheet-name sheet))))
