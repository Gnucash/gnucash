;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report.scm : structures/utilities for representing reports 
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

(require 'hash-table)
(require 'record)

(gnc:support "report.scm")

;; This hash should contain all the reports available and will be used
;; to generate the reports menu whenever a new window opens and to
;; figure out what to do when a report needs to be generated.
;;
;; The key is the string naming the report and the value is the 
;; report definition structure.
(define *gnc:_report-templates_* (make-hash-table 23))

;; this is a hash of 'report ID' to instantiated report.  the 
;; report id is generated at report-record creation time. 
(define *gnc:_reports_* (make-hash-table 23))
(define *gnc:_report-next-serial_* 0)

(define (gnc:report-menu-setup win)
  (define menu (gnc:make-menu "_Reports" (list "_Accounts")))
  (define menu-namer (gnc:new-menu-namer))
  
  (define (add-report-menu-item name report)
    (let* ((title (string-append (_ "Report") ": " (_ name)))
           (item #f))
      
      (set! item
            (gnc:make-menu-item
             ((menu-namer 'add-name) name)
             (sprintf #f (_ "Display the %s report") name)
             (list "_Reports" "")
             (lambda ()
               (let ((rept
                      (gnc:make-report (gnc:report-template-name report))))
                 (gnc:report-window rept)))))
      (gnc:add-extension item)))  
  
  ;; add the menu option to edit style sheets 
  (gnc:add-extension menu)
  (gnc:add-extension
   (gnc:make-menu-item 
    ((menu-namer 'add-name) (_ "Style Sheets..."))
    (_ "Edit report style sheets.")
    (list "_Reports" "")
    (lambda ()
      (gnc:style-sheet-dialog-open))))
  
  (gnc:add-extension 
   (gnc:make-separator (list "_Reports" "")))
  
  ;; push reports (new items added on top of menu)
  (hash-for-each add-report-menu-item *gnc:_report-templates_*))

(define <report-template>
  (make-record-type "<report-template>"
                    ;; The data items in a report record
                    '(version name options-generator renderer)))

(define (gnc:define-report . args) 
  ;; For now the version is ignored, but in the future it'll let us
  ;; change behaviors without breaking older reports.
  ;;
  ;; The generator should be a function that accepts one argument, a
  ;; set of options, and generates the report.
  ;;
  ;; This code must return as its final value a string representing
  ;; the contents of the HTML document.  preferably this should be
  ;; generated via the <html-document> class, but it's not required.
  
  (define (blank-report)
    ;; Number of #f's == Number of data members
    ((record-constructor <report-template>) #f #f #f #f))
  
  (define (args-to-defn in-report-rec args)
    (let ((report-rec (if in-report-rec
                          in-report-rec
                          (blank-report))))
      (if (null? args)
          in-report-rec
          (let ((id (car args))
               (value (cadr args))
               (remainder (cddr args)))
            ((record-modifier <report-template> id) report-rec value)
            (args-to-defn report-rec remainder)))))
  
  (let ((report-rec (args-to-defn #f args)))
    (if (and report-rec
             (gnc:report-template-name report-rec))
        (hash-set! *gnc:_report-templates_*
                   (gnc:report-template-name report-rec) report-rec)
        (gnc:warn "gnc:define-report: bad report"))))

(define gnc:report-template-version
  (record-accessor <report-template> 'version))
(define gnc:report-template-name
  (record-accessor <report-template> 'name))
(define gnc:report-template-options-generator
  (record-accessor <report-template> 'options-generator))
(define gnc:report-template-renderer
  (record-accessor <report-template> 'renderer))

(define (gnc:report-template-new-options report-template)
  (let ((generator (gnc:report-template-options-generator report-template))
        (stylesheet 
         (gnc:make-multichoice-option 
          (N_ "General") (N_ "Stylesheet") "0a"
          (N_ "Select a stylesheet for the report.")
          (string->symbol (N_ "Default"))
          (map 
           (lambda (ss)
             (vector 
              (string->symbol (gnc:html-style-sheet-name ss))
              (gnc:html-style-sheet-name ss)
              (string-append (gnc:html-style-sheet-name ss) 
                             (_ " Stylesheet"))))
           (gnc:get-html-style-sheets)))))
    (if (procedure? generator)
        (let ((options (generator)))
          (gnc:register-option options stylesheet)
          options)
        (let ((options (gnc:new-options)))
          (gnc:register-option options stylesheet)
          options))))

(define <report> 
  (make-record-type "<report>" '(type id options children dirty? ctext)))

(define gnc:report-type 
  (record-accessor <report> 'type))

(define gnc:report-set-type!
  (record-modifier <report> 'type))

(define gnc:report-id 
  (record-accessor <report> 'id))

(define gnc:report-set-id!
  (record-modifier <report> 'id))

(define gnc:report-options 
  (record-accessor <report> 'options))

(define gnc:report-set-options!
  (record-modifier <report> 'options))

(define gnc:report-children 
  (record-accessor <report> 'children))

(define gnc:report-set-children!
  (record-modifier <report> 'children))

(define (gnc:report-add-child! report child)
  (gnc:report-set-children! report 
                            (cons child (gnc:report-children report))))

(define gnc:report-dirty? 
  (record-accessor <report> 'dirty?))

(define gnc:report-set-dirty?!
  (record-modifier <report> 'dirty?))

(define gnc:report-ctext 
  (record-accessor <report> 'ctext))

(define gnc:report-set-ctext!
  (record-modifier <report> 'ctext))

(define (gnc:make-report template-name . rest)
  (let ((r ((record-constructor <report>) template-name #f #f '() #t #f))
        (template (hash-ref *gnc:_report-templates_* template-name))
        (id *gnc:_report-next-serial_*))
    (gnc:report-set-id! r id)
    (set! *gnc:_report-next-serial_* (+ 1 id))
    (let ((options 
           (if (not (null? rest))
               (car rest)
               (gnc:report-template-new-options template))))
      (gnc:report-set-options! r options))
    
    (hash-set! *gnc:_reports_* (gnc:report-id r) r)
    id))

(define (gnc:report-remove-by-id id)
  (let ((r (hash-ref *gnc:_reports_* id)))
    (for-each 
     (lambda (child)
       (gnc:report-remove-by-id (gnc:report-id child)))
     (gnc:report-children r))
    (hash-remove! *gnc:_reports_* id)))

(define (gnc:find-report id) 
  (hash-ref  *gnc:_reports_* id))

(define (gnc:report-tree-collapse tree)
  (let ((retval '()))
    (define (do-list list)
      (for-each
       (lambda (elt)
         (if (string? elt)
             (set! retval (cons elt retval))
             (if (not (list? elt))
                 (set! retval
                       (cons (with-output-to-string 
                               (lambda () (display elt)))
                             retval))
                 (do-list elt))))
       list))
    (do-list tree)
    retval))


(define (gnc:backtrace-if-exception proc . args)
  (define (dumper key . args)
    (let ((stack (make-stack #t dumper)))
      (display-backtrace stack (current-error-port))
      (apply display-error stack (current-error-port) args)
      (throw 'ignore)))
  
  (catch 
   'ignore
   (lambda () 
     (lazy-catch #t 
                 (lambda () (apply proc args))
                 dumper))
   (lambda (key . args)
     #f)))

(define (gnc:report-run id)
  (gnc:backtrace-if-exception 
   (lambda ()
     (let ((report (gnc:find-report id))
           (start-time (gettimeofday)))
       (if report
           (if (and (not (gnc:report-dirty? report))
                    (gnc:report-ctext report))
               ;; if there's clean cached text, return it 
               (begin 
                 (gnc:report-ctext report))
               
               ;; otherwise, rerun the report 
               (let ((template (hash-ref *gnc:_report-templates_* 
                                         (gnc:report-type report))))
                 (if template
                     (let* ((renderer (gnc:report-template-renderer template))
                            (stylesheet-name
                             (symbol->string (gnc:option-value
                                              (gnc:lookup-option 
                                               (gnc:report-options report)
                                               (N_ "General") 
                                               (N_ "Stylesheet")))))
                            (stylesheet 
                             (gnc:html-style-sheet-find stylesheet-name))
                            (doc (renderer report))
                            (html #f)
                            (formlist #f)
                            (collapsed-list #f))
                       
                       (gnc:html-document-set-style-sheet! doc stylesheet)
                       (set! formlist (gnc:html-document-render doc))
                       (set! collapsed-list 
                             (gnc:report-tree-collapse formlist))
                       (set! html (apply string-append collapsed-list))
                       (gnc:report-set-ctext! report html)
                       (gnc:report-set-dirty?! report #f)
                       
                       (display "total time to run report: ")
                       (display (gnc:time-elapsed start-time (gettimeofday)))
                       (newline)

                       html)
                     #f)))
           #f)))))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:report-menu-setup)
