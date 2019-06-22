;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report-impl.scm : structures/utilities for representing reports
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
(define-module (gnucash report report-core))

(eval-when (compile load eval expand)
  (load-extension "libgnc-report" "scm_init_sw_report_module"))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(use-modules (gnucash json parser))
(use-modules (gnucash json builder))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (gnucash report report-register-hooks))
(use-modules (gnucash report html-style-sheet))
(use-modules (gnucash report html-document))
(use-modules (gnucash report html-utilities))

(load-and-reexport (sw_report))

(export <report>)
(export gnc:all-report-template-guids)
(export gnc:custom-report-template-guids)
(export gnc:define-report)
(export gnc:delete-report)
(export gnc:find-report-template)
(export gnc:is-custom-report-type)
(export gnc:make-report)
(export gnc:make-report-options)
(export gnc:menuname-asset-liability)
(export gnc:menuname-budget)
(export gnc:menuname-business-reports)
(export gnc:menuname-custom)
(export gnc:menuname-example)
(export gnc:menuname-experimental)
(export gnc:menuname-income-expense)
(export gnc:menuname-multicolumn)
(export gnc:menuname-reports)
(export gnc:menuname-taxes)
(export gnc:optname-invoice-number)
(export gnc:optname-reportname)
(export gnc:pagename-accounts)
(export gnc:pagename-display)
(export gnc:pagename-general)
(export gnc:rename-report)
(export gnc:report-ctext)
(export gnc:report-dirty?)
(export gnc:report-editor-widget)
(export gnc:report-embedded-list)
(export gnc:report-export-thunk)
(export gnc:report-export-types)
(export gnc:report-id)
(export gnc:report-menu-name)
(export gnc:report-name)
(export gnc:report-needs-save?)
(export gnc:report-options)
(export gnc:report-render-html)
(export gnc:render-report)
(export gnc:report-run)
(export gnc:report-serialize)
(export gnc:report-set-ctext!)
(export gnc:report-set-dirty?!)
(export gnc:report-set-editor-widget!)
(export gnc:report-set-id!)
(export gnc:report-set-needs-save?!)
(export gnc:report-set-options!)
(export gnc:report-set-stylesheet!)
(export gnc:report-set-type!)
(export gnc:report-stylesheet)
(export gnc:report-template-export-thunk)
(export gnc:report-template-export-types)
(export gnc:report-template-has-unique-name?)
(export gnc:report-template-in-menu?)
(export gnc:report-template-is-custom/template-guid?)
(export gnc:report-template-menu-name)
(export gnc:report-template-menu-name/report-guid)
(export gnc:report-template-menu-path)
(export gnc:report-template-menu-tip)
(export gnc:report-template-name)
(export gnc:report-template-new-options)
(export gnc:report-template-new-options/report-guid)
(export gnc:report-template-options-changed-cb)
(export gnc:report-template-options-cleanup-cb)
(export gnc:report-template-options-generator)
(export gnc:report-template-renderer)
(export gnc:report-template-renderer/report-guid)
(export gnc:report-template-report-guid)
(export gnc:report-template-set-report-guid!)
(export gnc:report-template-version)
(export gnc:report-templates-for-each)
(export gnc:report-to-template-new)
(export gnc:report-to-template-update)
(export gnc:report-type)
(export gnc:restore-report-by-guid)
(export gnc:restore-report-by-guid-with-custom-template)

;; Terminology in this file:
;; report-template: a report definition of some form. This can be a report
;;      included in gnucash by default, or a new report definition added by
;;      the user in the .gnucash directory or a custom report
;; custom report: like a report-template, but saved with a different set
;;      of default options. A better name would probably be "preconfigured
;;      report" or something similar. These templates are managed by the
;;      user via the "Preconfigured Reports" menu item
;; report: an instantiation of a report-template (custom or otherwise). One
;;      specific instance of a template, loaded and configured by the user
;;      while the program is running.
;; saved report: a report that was still open at the time a book is closed.
;;      GnuCash dumps the current settings and template id for such a report
;;      in a meta file in .gnucash/books. When the book is reopened, the template
;;      id and settings are used to restore the report to the state it was
;;      in before the book was closed.
;;
;; This file will define record types for report-templates and reports. From what
;; I understand the latter is used mostly to handle saved reports as defined above,
;; while the former manages report-templates (including custom-reports).

;; This hash should contain all the reports available and will be used
;; to generate the reports menu whenever a new window opens and to
;; figure out what to do when a report needs to be generated.
;;
;; The key is the report guid and the
;; value is the report definition structure.
(define *gnc:_report-templates_* (make-hash-table 23))

;; Define those strings here to make changes easier and avoid typos.
(define gnc:menuname-reports "Reports/StandardReports")
(define gnc:menuname-asset-liability (N_ "_Assets & Liabilities"))
(define gnc:menuname-income-expense (N_ "_Income & Expense"))
(define gnc:menuname-budget (N_ "B_udget"))
(define gnc:menuname-taxes (N_ "_Taxes"))
(define gnc:menuname-example (N_ "E_xamples"))
(define gnc:menuname-experimental (N_ "_Experimental"))
(define gnc:menuname-multicolumn (N_ "_Multicolumn"))
(define gnc:menuname-custom (N_ "_Custom"))
(define gnc:pagename-general (N_ "General"))
(define gnc:pagename-accounts (N_ "Accounts"))
(define gnc:pagename-display (N_ "Display"))
(define gnc:optname-reportname (N_ "Report name"))
(define gnc:optname-stylesheet (N_ "Stylesheet"))
(define gnc:menuname-business-reports (N_ "_Business"))
(define gnc:optname-invoice-number (N_ "Invoice Number"))

;; A <report-template> represents one of the available report types.
(define-record-type <report-template>
  (make-new-record-template version name report-guid parent-type options-generator
                            options-cleanup-cb options-changed-cb
                            renderer in-menu? menu-path menu-name
                            menu-tip export-types export-thunk)
  report-template?
  (version report-template-version)
  (report-guid report-template-report-guid report-template-set-report-guid!)
  (name report-template-name report-template-set-name)
  (parent-type report-template-parent-type report-template-set-parent-type!)
  (options-generator report-template-options-generator)
  (options-cleanup-cb report-template-options-cleanup-cb)
  (options-changed-cb report-template-options-changed-cb)
  (renderer report-template-renderer)
  (in-menu? report-template-in-menu?)
  (menu-path report-template-menu-path)
  (menu-name report-template-menu-name)
  (menu-tip report-template-menu-tip)
  (export-types report-template-export-types)
  (export-thunk report-template-export-thunk))

(define (make-report-template)
  (make-new-record-template #f #f #f #f #f #f #f #f #t #f #f #f #f #f))
(define gnc:report-template-version report-template-version)
(define gnc:report-template-report-guid report-template-report-guid)
(define gnc:report-template-set-report-guid! report-template-set-report-guid!)
(define gnc:report-template-name report-template-name)
(define gnc:report-template-set-name report-template-set-name)
(define gnc:report-template-parent-type report-template-parent-type)
(define gnc:report-template-set-parent-type! report-template-set-parent-type!)
(define gnc:report-template-options-generator report-template-options-generator)
(define gnc:report-template-options-cleanup-cb report-template-options-cleanup-cb)
(define gnc:report-template-options-changed-cb report-template-options-changed-cb)
(define gnc:report-template-renderer report-template-renderer)
(define gnc:report-template-in-menu? report-template-in-menu?)
(define gnc:report-template-menu-path report-template-menu-path)
(define gnc:report-template-menu-name report-template-menu-name)
(define gnc:report-template-menu-tip report-template-menu-tip)
(define gnc:report-template-export-types report-template-export-types)
(define gnc:report-template-export-thunk report-template-export-thunk)

;; define strings centrally to ease code clarity
(define rpterr-dupe
  (G_ "One of your reports has a report-guid that is a duplicate. Please check the report system, especially your saved reports, for a report with this report-guid: "))
(define rpterr-guid1 (G_ "Wrong report definition: "))
(define rpterr-guid2 (G_ " Report is missing a GUID."))

(define (gui-error str)
  (if (gnucash-ui-is-running)
      (gnc-error-dialog '() str)
      (gnc:error "report-core.scm error: " str)))
(define (gui-warning str)
  (if (gnucash-ui-is-running)
      (gnc-warning-dialog '() str)
      (gnc:warn "report-core.scm warning: " str)))
(define (gui-error-missing-template template-name)
  (gui-error
   (string-append
    "Report Failed! One of your previously opened reports has failed \
to open. The template on which it was based: " template-name ", was \
not found.")))

;; if args is supplied, it is a list of field names and values
(define (gnc:define-report . args)
  ;; For now the version is ignored, but in the future it'll let us
  ;; change behaviors without breaking older reports.
  ;;
  ;; The renderer should be a function that accepts one argument, a
  ;; set of options, and generates the report. the renderer must
  ;; return as its final value an <html-document> object.
  (define report-rec (make-report-template))
  (define allowable-fields (record-type-fields <report-template>))
  (define (not-a-field? fld) (not (memq fld allowable-fields)))
  (define (xor . args) (fold (lambda (a b) (if a (if b #f a) b)) #f args))

  (let loop ((args args))
    (match args
      (()
       (let ((report-guid (gnc:report-template-report-guid report-rec))
             (report-name (gnc:report-template-name report-rec)))
         (cond
          ;; missing report-guid: is an error
          ((not report-guid)
           (gui-error (string-append rpterr-guid1 report-name rpterr-guid2)))

          ;; dupe: report-guid is a duplicate
          ((hash-ref *gnc:_report-templates_* report-guid)
           (gui-error (string-append rpterr-dupe report-guid)))

          ;; has export-type but no export-thunk. or vice versa.
          ((xor (gnc:report-template-export-thunk report-rec)
                (gnc:report-template-export-types report-rec))
           (gui-error (format #f "Export needs both thunk and types: ~a" report-guid)))

          ;; good: new report definition, store into report-templates hash
          (else
           (hash-set! *gnc:_report-templates_* report-guid report-rec)))))

      (((? not-a-field? fld) . _)
       (gnc:error "gnc:define-report: " fld " is not a valid field"))

      ((field val . rest)
       ((record-modifier <report-template> field) report-rec val)
       (loop rest)))))

(define (gnc:report-template-new-options/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (and templ
         (gnc:report-template-new-options templ))))

(define (gnc:report-template-menu-name/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (and templ
         (or (gnc:report-template-menu-name templ)
             (gnc:report-template-name templ)))))

(define (gnc:report-template-renderer/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (and templ
         (gnc:report-template-renderer templ))))

(define (gnc:report-template-new-options report-template)
  (let ((generator (gnc:report-template-options-generator report-template))
        (namer
         (gnc:make-string-option
          gnc:pagename-general gnc:optname-reportname "0a"
          (N_ "Enter a descriptive name for this report.")
          (G_ (gnc:report-template-name report-template))))
        (stylesheet
         (gnc:make-multichoice-option
          gnc:pagename-general gnc:optname-stylesheet "0b"
          (N_ "Select a stylesheet for the report.")
          (string->symbol (N_ "Default"))
          (map
           (lambda (ss)
             (vector
              (string->symbol (gnc:html-style-sheet-name ss))
              (gnc:html-style-sheet-name ss)))
           (gnc:get-html-style-sheets)))))

    (let ((options (if (procedure? generator)
                       (or (gnc:backtrace-if-exception generator)
                           (begin
                             (gnc:warn "BUG DETECTED: Scheme exception raised in "
                                       "report options generator procedure named "
                                       (procedure-name generator))
                             (gnc:new-options)))
                       (gnc:new-options))))
      (or (gnc:lookup-option options gnc:pagename-general gnc:optname-reportname)
          (gnc:register-option options namer))
      (or (gnc:lookup-option options gnc:pagename-general gnc:optname-stylesheet)
          (gnc:register-option options stylesheet))
      options)))

;; A <report> represents an instantiation of a particular report type.
(define-record-type <report>
  (make-report type id options dirty? needs-save? editor-widget ctext custom-template)
  report?
  (type report-type report-set-type!)
  (id report-id report-set-id!)
  (options report-options report-set-options!)
  (dirty? report-dirty? report-set-dirty?!)
  (needs-save? report-needs-save? report-set-needs-save?!)
  (editor-widget report-editor-widget report-set-editor-widget!)
  (ctext report-ctext report-set-ctext!)
  (custom-template report-custom-template report-set-custom-template!))

(define gnc:report-type report-type)
(define gnc:report-set-type! report-set-type!)
(define gnc:report-id report-id)
(define gnc:report-set-id! report-set-id!)
(define gnc:report-options report-options)
(define gnc:report-set-options! report-set-options!)
(define gnc:report-needs-save? report-needs-save?)
(define gnc:report-set-needs-save?! report-set-needs-save?!)
(define gnc:report-dirty? report-dirty?)
(define gnc:report-set-dirty?-internal! report-set-dirty?!)
(define gnc:report-editor-widget report-editor-widget)
(define gnc:report-set-editor-widget! report-set-editor-widget!)
(define gnc:report-ctext report-ctext)
(define gnc:report-set-ctext! report-set-ctext!)
(define gnc:report-custom-template report-custom-template)
(define gnc:report-set-custom-template! report-set-custom-template!)

(define (gnc:report-set-dirty?! report val)
  (gnc:report-set-dirty?-internal! report val)
  (let* ((template (hash-ref *gnc:_report-templates_* (gnc:report-type report)))
         (cb (gnc:report-template-options-changed-cb template)))
    (if (and cb (procedure? cb))
        (cb report))))

;; gnc:make-report instantiates a report from a report-template.
;; The actual report is stored away in a hash-table -- only the id is returned.
(define (gnc:make-report template-id . rest)
  (let* ((template-parent (gnc:report-template-parent-type
                           (hash-ref *gnc:_report-templates_* template-id)))
         (report-type (or template-parent template-id))
         (custom-template (if template-parent template-id ""))
         (r (make-report
             report-type     ;; type
             #f              ;; id
             #f              ;; options
             #t              ;; dirty
             #f              ;; needs-save
             #f              ;; editor-widget
             #f              ;; ctext
             custom-template ;; custom-template
             ))
         (template (hash-ref *gnc:_report-templates_* template-id)))
    (let ((options (if (null? rest)
                       (gnc:report-template-new-options template)
                       (car rest))))
      (gnc:report-set-options! r options)
      (gnc:options-register-callback
       #f #f
       (lambda ()
         (gnc:report-set-dirty?! r #t)
         (let ((cb (gnc:report-template-options-changed-cb template)))
           (if cb (cb r))))
       options))
    (gnc:report-set-id! r (gnc-report-add r))
    (gnc:report-id r)))


(define (gnc:restore-report-by-guid id template-id template-name options)
  (issue-deprecation-warning "gnc:restore-report-by-guid is now deprecated.
 use gnc:restore-report-by-guid-with-custom-template instead.")
  (if options
      (let* ((r (make-report template-id id options #t #t #f #f ""))
             (report-id (gnc-report-add r)))
        (if (number? report-id)
            (gnc:report-set-id! r report-id))
        report-id)
      (begin
        (gui-error-missing-template template-name)
        #f)))

(define (gnc:restore-report-by-guid-with-custom-template
         id template-id template-name custom-template-id options)
  (if options
      (let* ((r (make-report template-id id options #t #t #f #f custom-template-id))
             (report-id (gnc-report-add r)))
        (if (number? report-id)
            (gnc:report-set-id! r report-id))
        report-id)
      (begin
        (gui-error-missing-template template-name)
        #f)))

(define (gnc:make-report-options template-id)
  (let ((template (hash-ref *gnc:_report-templates_* template-id)))
    (and template
         (gnc:report-template-new-options template))))

;; A convenience wrapper to get the report-template's export types from
;; an instantiated report.
(define (gnc:report-export-types report)
  (let ((template (hash-ref *gnc:_report-templates_*
                            (gnc:report-type report))))
    (and template
         (gnc:report-template-export-types template))))

;; A convenience wrapper to get the report-template's export thunk from
;; an instantiated report.
(define (gnc:report-export-thunk report)
  (let ((template (hash-ref *gnc:_report-templates_*
                            (gnc:report-type report))))
    (and template
         (gnc:report-template-export-thunk template))))

(define (gnc:report-menu-name report)
  (let ((template (hash-ref *gnc:_report-templates_*
                            (gnc:report-type report))))
    (and template
         (or (gnc:report-template-menu-name template)
             (gnc:report-name report)))))

(define (gnc:report-name report)
  (let* ((opt (gnc:report-options report)))
    (and opt
         (gnc:option-value
          (gnc:lookup-option opt gnc:pagename-general gnc:optname-reportname)))))

(define (gnc:report-stylesheet report)
  (gnc:html-style-sheet-find
   (symbol->string (gnc:option-value
                    (gnc:lookup-option
                     (gnc:report-options report)
                     gnc:pagename-general
                     gnc:optname-stylesheet)))))

(define (gnc:report-set-stylesheet! report stylesheet)
  (gnc:option-set-value
   (gnc:lookup-option
    (gnc:report-options report)
    gnc:pagename-general
    gnc:optname-stylesheet)
   (string->symbol
    (gnc:html-style-sheet-name stylesheet))))


;; Load and save helper functions

;; list of all report guids in existence (includes standard & custom
;; reports, but not instantiated ones)
(define (gnc:all-report-template-guids)
  (map car (hash-map->list cons *gnc:_report-templates_*)))

;; return a list of the custom report template guids.
(define (gnc:custom-report-template-guids)
  (map car (gnc:custom-report-templates-list)))

(define (gnc:find-report-template guid)
  (hash-ref *gnc:_report-templates_* guid))

(define (gnc:report-template-is-custom/template-guid? guid)
  (assoc guid (gnc:custom-report-templates-list)))

(define (gnc:is-custom-report-type report)
  (gnc:report-template-is-custom/template-guid? (gnc:report-custom-template report)))

;; list of reports saved within the saved-reports; returns a list of
;; pairs whose cars = guid <string> and cdrs = report-template <record>
(define (gnc:custom-report-templates-list)
  (filter (compose gnc:report-template-parent-type cdr)
          (hash-map->list cons *gnc:_report-templates_*)))

;; This function should be called right before changing a custom-template's name
;; to test if the new name is unique among the existting custom reports.
;; If not the calling function can prevent the name from being updated.
(define (gnc:report-template-has-unique-name? templ-guid new-name)
  (or (not new-name)
      (not (any
            (lambda (tmpl)
              (and (not (equal? (car tmpl) templ-guid))
                   (equal? (gnc:report-template-name (cdr tmpl)) new-name)))
            (gnc:custom-report-templates-list)))))

;; Generate a unique custom template name using the given string as a base
;; If this string already exists as a custom template name, a
;; number will be appended to it.
(define (gnc:report-template-make-unique-name new-name)
  (let loop ((name new-name)
             (counter 1))
    (if (gnc:report-template-has-unique-name? #f name)
        name
        (loop (string-append new-name (number->string counter))
              (1+ counter)))))


;; Load and save functions

(define (template->scm tmpl saved-options)
  ;; converts a template to an scm representation, including any subreports
  ;; saved-options will save a report instance options into scm
  (let* ((options (or saved-options (gnc:report-template-new-options tmpl)))
         (guid (gnc:report-template-report-guid tmpl))
         (name (gnc:report-template-name tmpl))
         (menupath (gnc:report-template-menu-path tmpl))
         (sub-reports
          (map
           (lambda (subreport-id)
             (let* ((sub (gnc-report-find subreport-id))
                    (sub-type (gnc:report-type sub))
                    (sub-template (hash-ref *gnc:_report-templates_* sub-type))
                    (sub-template-name (gnc:report-template-name sub-template))
                    (sub-cleanup-cb (gnc:report-template-options-cleanup-cb
                                     sub-template)))
               (if sub-cleanup-cb (sub-cleanup-cb sub))
               (list
                (cons 'type sub-type)
                (cons 'name sub-template-name)
                (cons 'options (list->vector
                                (gnc:options-scm->list
                                 (gnc:report-options sub)))))))
           (or (gnc:report-embedded-list options) '()))))
    (list
     (cons 'name name)
     (cons 'parenttype (gnc:report-template-parent-type tmpl))
     (cons 'menupath (and menupath (list->vector menupath)))
     (cons 'sub-reports (list->vector sub-reports))
     (cons 'options (list->vector (gnc:options-scm->list options))))))

;; internal function used in scm->template and gnc:json->report
;; modifies an options object according to other parameters
(define (options-modify! options saved-options-list sub-reports)
  (gnc:options-list->scm options saved-options-list)
  (let* ((report-list (gnc:lookup-option options "__general" "report-list")))
    (if report-list
        (gnc:option-set-value
         report-list
         (map
          (lambda (sub option-list)
            (let* ((sub-type (assoc-ref sub "type"))
                   (sub-name (assoc-ref sub "name"))
                   (sub-saved-options (vector->list (assoc-ref sub "options")))
                   (sub-new-options
                    (gnc:report-template-new-options/report-guid sub-type sub-name)))
              (gnc:options-list->scm sub-new-options sub-saved-options)
              (cons (gnc:restore-report-by-guid-with-custom-template
                     #f sub-type sub-name "" sub-new-options)
                    (cdr option-list))))
          sub-reports
          (gnc:option-value report-list)))))
  options)

(define (scm->template scm guid)
  ;; converts an scm representation from above template->scm to a
  ;; full report definition. outputs the report-id
  (let* ((name (assoc-ref scm "name"))
         (parenttype (assoc-ref scm "parenttype"))
         (sub-reports (vector->list (assoc-ref scm "sub-reports")))
         (menupath (vector->list (assoc-ref scm "menupath")))
         (saved-options (vector->list (assoc-ref scm "options")))
         (options (gnc:report-template-new-options/report-guid parenttype name)))

    (options-modify! options saved-options sub-reports)
    (gnc:define-report
     'version 1
     'name name
     'report-guid guid
     'parent-type parenttype
     'options-generator (lambda () options)
     'menu-path menupath
     'renderer (gnc:report-template-renderer/report-guid parenttype name))))

(export gnc:report->json)
(define (gnc:report->json report)
  (let* ((report-type (gnc:report-type report))
         (template (hash-ref *gnc:_report-templates_* report-type))
         (cleanup-cb (gnc:report-template-options-cleanup-cb template)))
    (if cleanup-cb (cleanup-cb report))
    (scm->json-string
     (list (cons 'guid (gnc:report-type report))
           (cons 'name (gnc:report-template-name
                        (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
           (cons 'template (template->scm template (gnc:report-options report)))))))

(export gnc:json->report)
(define (gnc:json->report json)
  (catch #t
    (lambda ()
      (let* ((rep (json-string->scm json))
             (guid (assoc-ref rep "guid"))
             (name (assoc-ref rep "name"))
             (template (assoc-ref rep "template"))
             (saved-options-list (vector->list (assoc-ref template "options")))
             (sub-reports (vector->list (assoc-ref template "sub-reports")))
             (options (gnc:report-template-new-options/report-guid guid name)))
        (options-modify! options saved-options-list sub-reports)
        (gnc:restore-report-by-guid-with-custom-template
         #f guid name template options)))

    (lambda args
      (gui-error (format #f "error ~a parsing json: ~a" args json))
      #f)))

;; Generate guile code required to recreate an instatiated report
(define (gnc:report-serialize report)
  (issue-deprecation-warning
   "gnc:report-serialize is now obsolete.")
  ;; clean up the options if necessary.  this is only needed
  ;; in special cases.
  (let* ((report-type (gnc:report-type report))
         (template (hash-ref *gnc:_report-templates_* report-type))
         (thunk (gnc:report-template-options-cleanup-cb template)))
    (if thunk
        (thunk report)))

  ;; save them
  (string-append
   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
   (format #f ";; options for report ~S\n" (gnc:report-name report))
   (format
    #f "(let ((options (gnc:report-template-new-options/report-guid ~S ~S)))\n"
    (gnc:report-type report)
    (gnc:report-template-name
     (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
   (gnc:generate-restore-forms (gnc:report-options report) "options")
   (format
    #f "  (gnc:restore-report-by-guid-with-custom-template ~S ~S ~S ~S options)\n"
    (gnc:report-id report) (gnc:report-type report)
    (gnc:report-template-name
     (hash-ref *gnc:_report-templates_* (gnc:report-type report)))
    (gnc:report-custom-template report))
   ")"))

;; Generate guile code required to recreate embedded report instances
(define (gnc:report-serialize-embedded embedded-reports)
  (issue-deprecation-warning
   "gnc:report-serialize-embedded is now obsolete.")
  (let* ((result-string ""))
    (if embedded-reports
        (begin
          (for-each
           (lambda (subreport-id)
             (let* ((subreport (gnc-report-find subreport-id))
                    (subreport-type (gnc:report-type subreport))
                    (subreport-template (hash-ref *gnc:_report-templates_* subreport-type))
                    (subreport-template-name (gnc:report-template-name subreport-template))
                    (thunk (gnc:report-template-options-cleanup-cb subreport-template)))
               ;; clean up the options if necessary.  this is only needed
               ;; in special cases.
               (if thunk
                   (thunk subreport))
               ;; save them
               (set! result-string
                 (string-append
                  result-string
                  "\n      ;;;; Options for embedded report\n"
                  "      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
                  (format #f "      ;; options for report ~S\n" (gnc:report-name subreport))
                  (format #f "      (let ((options (gnc:report-template-new-options/report-guid ~S ~S)))"
                          subreport-type
                          subreport-template-name)
                  (gnc:generate-restore-forms (gnc:report-options subreport) "options")
                  (format #f "\n        (set! new-embedded-report-ids\n          (append\n            new-embedded-report-ids\n              (list (gnc:restore-report-by-guid-with-custom-template #f ~S ~S ~S options))\n          )\n        )\n"
                          subreport-type
                          subreport-template-name
                          (gnc:report-custom-template subreport))
                  "      )\n"))))
           embedded-reports)
          ;;(set! result-string (string-append result-string (gnc:update-section-general)))
          (set! result-string
            (string-append
             result-string
             "\n"
             "      ;;;; Update Section: __general\n"
             "      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
             "      (let*\n"
             "        (\n"
             "          (option (gnc:lookup-option options \"__general\" \"report-list\"))\n"
             "          (saved-report-list (gnc:option-value option))\n"
             "        )\n"
             "        (\n"
             "          (lambda (option)\n"
             "            (if option ((gnc:option-setter option) (map (lambda (x y) (cons x (cdr y))) new-embedded-report-ids saved-report-list)))\n"
             "          )\n"
             "          option\n"
             "        )\n"
             "      )\n"))))
    result-string))

(define (gnc:report-template-serialize-internal name type templ-name options guid)
  (issue-deprecation-warning
   "gnc:report-template-serialize-internal is now obsolete.")
  (let* ((embedded-serialized (gnc:report-serialize-embedded (gnc:report-embedded-list options)))
         (result (string-append
                  ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
                  (format #f ";; Options for saved report ~S, based on template ~S\n"
                          name type)
                  (format
                   #f "(let ()\n  (define (options-gen)\n    (let\n         (\n           (options (gnc:report-template-new-options/report-guid ~S ~S))\n           (new-embedded-report-ids '()) ;; only used with Multicolumn View Reports\n         )"
                   type templ-name)
                  (gnc:generate-restore-forms options "options")
                  (if embedded-serialized
                      embedded-serialized
                      "")
                  "\n      options\n    )\n  )\n"
                  (format
                   #f "  (gnc:define-report \n    'version 1\n    'name ~S\n    'report-guid ~S\n    'parent-type ~S\n    'options-generator options-gen\n    'menu-path (list gnc:menuname-custom)\n    'renderer (gnc:report-template-renderer/report-guid ~S ~S)\n  )\n)\n\n"
                   name
                   (or guid
                       (guid-new-return)) ;; when saving a report, we need to create a guid for it for later reloading
                   type
                   type
                   templ-name))))
    (gnc:debug result)
    result))

;; Convert an instantiated report into a report template
;; and generate the guile code required to recreate this template
(define (gnc:report-template-serialize-from-report report)
  (issue-deprecation-warning
   "gnc:report-template-serialize-internal is now obsolete.")
  ;; clean up the options if necessary.  this is only needed
  ;; in special cases.
  (let* ((template (hash-ref *gnc:_report-templates_* (gnc:report-type report)))
         (thunk (gnc:report-template-options-cleanup-cb template)))
    (if thunk
        (thunk report)))

  ;; save them
  (let* ((name (gnc:report-template-make-unique-name (gnc:report-name report)))
         (type (gnc:report-type report))
         (templ-name (gnc:report-template-name
                      (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
         (options (gnc:report-options report)))
    (gnc:report-template-serialize-internal name type templ-name options #f)))

;; Generate guile code required to recreate a report template
;; Note: multi column report templates encapsulate instantiated
;; reports, not other report templates this means that the template
;; recreation code must also contain the code to instantiate these
;; embedded report instances. This results in a mix of template and
;; instatiated reports in the saved reports file...
(define (gnc:report-template-serialize report-template)
  (issue-deprecation-warning
   "gnc:report-template-serialize is now obsolete.")
  (let* ((name (gnc:report-template-name report-template))
         (type (gnc:report-template-parent-type report-template))
         (templ-name (gnc:report-template-name
                      (hash-ref *gnc:_report-templates_* type)))
         (options (gnc:report-template-new-options report-template))
         (guid (gnc:report-template-report-guid report-template)))
    (gnc:report-template-serialize-internal name type templ-name options guid)))

;; Convert a report into a report template and save this template in the savefile
;; Under specific conditions the we will attempt to replace the current report's
;; template instead of simply adding a new template to the file.
;; These condititions are:
;; 1. the report is an instance of an existing custom report template
;;    (ie a template that is stored in the savefile already)
;; 2. an overwrite is requested by setting overwrite? to #t
(define (gnc:report-to-template report overwrite?)
  ;; This implements the Save Report Configuration tasks
  (let* ((custom-template-id (gnc:report-custom-template report))
         (overwrite-ok? (and (gnc:report-template-is-custom/template-guid?
                              custom-template-id)
                             overwrite?))
         (save-result
          (let* ((template (hash-ref *gnc:_report-templates_* (gnc:report-type report)))
                 (thunk (gnc:report-template-options-cleanup-cb template)))
            (if thunk (thunk report))
            (let* ((name (gnc:report-template-make-unique-name (gnc:report-name report)))
                   (type (gnc:report-type report))
                   (templ-name (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
                   (options (gnc:report-options report))
                   (options-gen
                    (lambda ()
                      (let* ((options-new (gnc:report-template-new-options/report-guid type templ-name))
                             (options-list (gnc:options-scm->list options))
                             (embedded-reports (gnc:report-embedded-list options)))
                        (when embedded-reports
                          (let ((embedded-report-ids
                                 (map
                                  (lambda (subreport-id)
                                    (let* ((subreport (gnc-report-find subreport-id))
                                           (subreport-type (gnc:report-type subreport))
                                           (subreport-template (hash-ref *gnc:_report-templates_* subreport-type))
                                           (subreport-template-name (gnc:report-template-name subreport-template))
                                           (thunk (gnc:report-template-options-cleanup-cb subreport-template)))
                                      (if thunk (thunk subreport))
                                      (let ((embedded-options-new
                                             (gnc:report-template-new-options/report-guid
                                              subreport-type subreport-template-name))
                                            (embedded-options-list
                                             (gnc:options-scm->list
                                              (gnc:report-options subreport))))
                                        (gnc:restore-report-by-guid-with-custom-template
                                         #f subreport-type
                                         subreport-template-name
                                         (gnc:report-custom-template subreport)
                                         options-new))))
                                  embedded-reports)))
                            (option (gnc:lookup-option options-new "__general" "report-list"))
                            (if option
                                ((gnc:option-setter option)
                                 (map
                                  (lambda (x y)
                                    (cons x (cdr y)))
                                  embedded-report-ids
                                  (gnc:option-value option))))))
                        options-new))))
              (gnc:define-report
               'version 1
               'name name
               'report-guid (guid-new-return)
               'parent-type type
               'options-generator options-gen
               'menu-path (list gnc:menuname-custom)
               'renderer (gnc:report-template-renderer/report-guid type templ-name))))))

    (and (record? save-result)
         (begin
           ;; If it's ok to overwrite the old template, delete it now.
           (if overwrite-ok?
               (let ((templ-name
                      (gnc:report-template-name
                       (hash-ref *gnc:_report-templates_* custom-template-id))))
                 ;; We're overwriting, which needs some additional steps
                 ;; 1. Remove the newly generated template from the template list again
                 (hash-remove! *gnc:_report-templates_*
                               (gnc:report-template-report-guid save-result))
                 ;; 2. We still have the template record available
                 ;; though, so adapt it to the template we want to
                 ;; override (ie update guid and name)
                 (gnc:report-template-set-report-guid! save-result custom-template-id)
                 (gnc:report-template-set-name save-result templ-name)
                 ;; 3. Overwrite the template with the new one
                 (hash-set! *gnc:_report-templates_* custom-template-id save-result)))

           ;; Regardless of how we got here, we now have a new template to write
           ;; so let's write it
           (and (gnc:save-all-reports)
                (let ((templ-guid (gnc:report-template-report-guid save-result)))
                  ;; Indicate the report was instantiated from the new template
                  (gnc:report-set-custom-template! report templ-guid)
                  ;; Inform the calling function of the new template's guid
                  templ-guid))))))

;; Convert a report into a new report template and add this template to the save file
(define (gnc:report-to-template-new report)
  (gnc:report-to-template report #f))

;; Get the current report's template and try to update it with the report's current
;; settings. This will only be possible if the report was already based on a
;; custom report template. If that's not the case, a new template will be added instead.
(define (gnc:report-to-template-update report)
  (gnc:report-to-template report #t))

(define (gnc:report-template-save-to-savefile report-template)
  (issue-deprecation-warning
   "gnc:report-template-save-to-savefile is now obsolete.")
  (let ((saved-form (gnc:report-template-serialize report-template)))
    (gnc-saved-reports-write-to-file saved-form #f)))

;; save all custom reports into book.  return #t if all templates were
;; saved successfully. error conditions mean some templates are not saved.
(define (gnc:save-all-reports)
  (let ((saved (N_ "Saved book custom reports"))
        (book (gnc-get-current-book)))
    (qof-book-options-delete book '("custom-templates")) ;reset kvp root

    ;; loop through all custom-templates in memory
    (let loop ((custom-templates (gnc:custom-report-templates-list))
               (errors '())
               (guids '()))
      (cond
       ;; end of custom-templates. report errors, and store the guids.
       ((null? custom-templates)
        (qof-book-set-option book (scm->json-string (list->vector guids))
                             '("custom-templates" "guids"))
        (cond
         ((null? errors)
          ;; (gnc:gui-msg saved (_ saved))
          #t)

         (else
          (gui-error (string-append
                      (_ "Error saving report(s): ")
                      (string-join errors ", ")))
          #f)))

       (else
        ;; there are custom-templates remaining.
        (let* ((tmpl (car custom-templates))
               (name (gnc:report-template-name (cdr tmpl)))
               (guid (gnc:report-template-report-guid (cdr tmpl))))

          (catch #t
            (lambda ()
              (let ((repstring (scm->json-string (template->scm (cdr tmpl) #f))))
                (cond
                 ((> (string-length repstring) 4000)
                  ;; serialized report string exceeds 4K. skip it.
                  (loop (cdr custom-templates)
                        (cons (string-append "report too complex:" name) errors)
                        guids))
                 (else
                  ;; successfully generated string. save it.
                  (qof-book-set-option book repstring (list "custom-templates" guid))
                  (loop (cdr custom-templates)
                        errors
                        (cons guid guids))))))
            (lambda args
              ;; crash. log error and continue.
              (loop (cdr custom-templates)
                    (cons (format #f "~a: ~s" name args) errors)
                    guids)))))))))

;; the following function will: (1) save the initial custom-reports
;; into global-saved-reports. These are from SAVED_REPORTS. (2) this
;; function is called upon file-load, upon which it should query if
;; book has custom-reports; if it does, it will clear the
;; in-memory custom-reports and replace with the book-custom-reports
;; (3) if the current book has no book-custom-reports, it reloads from
;; global-saved-reports.
(export gnc:load-book-custom-templates)
(define gnc:load-book-custom-templates
  (let ((global-saved-reports #f))
    (lambda ()
      (unless global-saved-reports
        ;; global-saved-reports *cannot* be initialized during initial
        ;; function definition because saved-reports hasn't been
        ;; loaded yet. we must initialize upon first call to
        ;; gnc:load-book-custom-templates
        (set! global-saved-reports (gnc:custom-report-templates-list)))
      (let* ((loaded (N_ "Loaded book custom reports"))
             (global (N_ "Loaded global custom reports"))
             (book (gnc-get-current-book))
             (book-reports (qof-book-get-option book '("custom-templates" "guids"))))

        (cond
         ;; book has saved-reports. remove custom-templates and load from book.
         (book-reports
          (for-each
           (lambda (guid)
             (hash-remove! *gnc:_report-templates_* guid))
           (gnc:custom-report-template-guids))

          (for-each
           (lambda (guid)
             (let* ((saved-template-string
                     (qof-book-get-option book (list "custom-templates" guid)))
                    (saved-report (json-string->scm saved-template-string)))
               (scm->template saved-report guid)))
           (vector->list (json-string->scm book-reports)))
          (gnc:gui-msg loaded (_ loaded)))

         ;; book has no saved-reports. revert to global saved-reports.
         (else
          (for-each
           (lambda (global)
             (hash-set! *gnc:_report-templates_* (car global) (cdr global)))
           global-saved-reports)
          (gnc:gui-msg global (_ global))))))))

;; gets the renderer from the report template;
;; gets the stylesheet from the report;
;; renders the html doc and caches the resulting string;
;; returns the html string.
;; Now accepts either an html-doc or finished HTML from the renderer -
;; the former requires further processing, the latter is just returned.
(define (gnc:report-render-html report headers?)
  (if (and (not (gnc:report-dirty? report))
           (gnc:report-ctext report))
      (gnc:report-ctext report)
      (let ((template (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
        (and template
             (let* ((renderer (gnc:report-template-renderer template))
                    (stylesheet (gnc:report-stylesheet report))
                    (doc (renderer report))
                    (html (cond
                           ((string? doc) doc)
                           (else
                            (gnc:html-document-set-style-sheet! doc stylesheet)
                            (gnc:html-document-render doc headers?)))))
               (gnc:report-set-ctext! report html) ;; cache the html
               (gnc:report-set-dirty?! report #f)  ;; mark it clean
               html)))))

;; render report. will return a 2-element list: either (list html #f)
;; where html is the report html string, or (list #f captured-error)
;; where captured-error is the error string.
(define (gnc:render-report report)
  (define (get-report) (gnc:report-render-html report #t))
  (gnc:apply-with-error-handling get-report '()))

;; looks up the report by id and renders it with gnc:report-render-html
;; marks the cursor busy during rendering; returns the html
(define (gnc:report-run id)
  (issue-deprecation-warning "gnc:report-run is deprecated. use gnc:render-report instead.")
  (let ((report (gnc-report-find id))
        (html #f))
    (gnc-set-busy-cursor '() #t)
    (gnc:backtrace-if-exception
     (lambda ()
       (if report (set! html (gnc:report-render-html report #t)))))
    (gnc-unset-busy-cursor '())
    html))


;; "thunk" should take the report-type and the report template record
(define (gnc:report-templates-for-each thunk)
  (hash-for-each
   (lambda (report-id template)
     (thunk report-id template))
   *gnc:_report-templates_*))

;; return the list of reports embedded in the specified report
(define (gnc:report-embedded-list options)
  (let* ((option (gnc:lookup-option options "__general" "report-list")))
    (and option
         (let ((opt-value (gnc:option-value option)))
           (map car opt-value)))))

;; delete an existing report from the hash table and then call to
;; resave the saved-reports file... report is gone
(define (gnc:delete-report template-guid)
  (if (hash-ref *gnc:_report-templates_* template-guid)
      (begin
        (gnc:debug "Deleting report " template-guid)
        (hash-remove! *gnc:_report-templates_* template-guid)
        (gnc:save-all-reports))))

;; rename an existing report from the hash table and then
;; resave the saved-reports file
(define (gnc:rename-report template-guid new-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-guid)))
    (when templ
      (gnc:debug "Renaming report " template-guid)
      (gnc:report-template-set-name templ new-name)
      (gnc:save-all-reports))))

;;
;; gnucash-cli helper and exported functions
;;

(define (show-selected-reports pred? port)
  (for-each
   (lambda (template)
     (format port "* ~a ~a\n"
             (if (gnc:report-template-parent-type template) "C" " ")
             (gnc:report-template-name template)))
   (sort (hash-fold (lambda (k v p) (if (pred? v) (cons v p) p)) '()
                    *gnc:_report-templates_*)
         (lambda (a b) (gnc:string-locale<? (gnc:report-template-name a)
                                            (gnc:report-template-name b))))))

(define (stderr-log tmpl . args)
  (apply format (current-error-port) tmpl args)
  #f)

(define (template-export report template export-type dry-run?)
  (let* ((report-guid (gnc:report-template-report-guid template))
         (parent-template-guid (gnc:report-template-parent-type template))
         (template (if parent-template-guid
                       (hash-ref *gnc:_report-templates_* parent-template-guid)
                       template))
         (export-thunk (gnc:report-template-export-thunk template))
         (export-types (gnc:report-template-export-types template)))

    (cond
     ((not export-thunk)
      (stderr-log "Only the following reports have export code:\n")
      (show-selected-reports gnc:report-template-export-thunk (current-error-port))
      (stderr-log "Use -R show to describe report\n"))
     ((not (assoc export-type export-types))
      (stderr-log "Export-type disallowed: ~a. Allowed types: ~a\n"
                  export-type (string-join (map car export-types) ", ")))
     (dry-run? #t)
     (else
      (display "Running export..." (current-error-port))
      (let ((output (export-thunk
                     (gnc-report-find (gnc:make-report report-guid))
                     (assoc-ref export-types export-type))))
        (display "done!\n" (current-error-port))
        output)))))

(define (reportname->templates report)
  (or (and=> (gnc:find-report-template report) list)
      (hash-fold
       (lambda (k v p) (if (equal? (gnc:report-template-name v) report) (cons v p) p))
       '() *gnc:_report-templates_*)))

(define-public (gnc:cmdline-report-list port)
  (show-selected-reports gnc:report-template-in-menu? port))

(define-public (gnc:cmdline-report-show report port)
  (let ((templates (reportname->templates report)))
    (cond
     ((null? templates)
      (stderr-log "Cannot find ~s. Valid reports:\n" report)
      (gnc:cmdline-report-list (current-error-port)))
     (else
      (for-each
       (lambda (template)
         (let* ((options-gen (gnc:report-template-options-generator template))
                (parent-guid (gnc:report-template-parent-type template))
                (parent-template (and parent-guid
                                      (hash-ref *gnc:_report-templates_* parent-guid)))
                (export-types (gnc:report-template-export-types
                               (or parent-template template))))
           (format port "\n* name: ~a\n  guid: ~a\n~a~a~a"
                   (gnc:report-template-name template)
                   (gnc:report-template-report-guid template)
                   (if parent-template
                       (format #f "  parent-template: ~a\n"
                               (gnc:report-template-name parent-template))
                       "")
                   (if export-types
                       (format #f "  export-types: ~a\n"
                               (string-join (map car export-types) ", ")) "")
                   (gnc:html-render-options-changed (options-gen) #t))))
       templates)))))

;; In: report - string matching reportname
;; In: export-type - string matching export type (eg CSV TXF etc)
;; Out: if args are valid and runs a single report: #t, otherwise: #f
(define-public (gnc:cmdline-check-report report export-type)
  (let ((templates (reportname->templates report)))
    (cond
     ((null? templates)
      (stderr-log "Cannot find ~s. Valid reports:\n" report)
      (gnc:cmdline-report-list (current-error-port))
      (stderr-log "\n"))

     ((pair? (cdr templates))
      (stderr-log "~s matches multiple reports. Select guid instead:\n" report)
      (gnc:cmdline-report-show report (current-error-port))
      (stderr-log "\n"))

     (export-type (template-export report (car templates)
                                   export-type #t))
     (else #t))))

;; In: report - string matching reportname
;; In: export-type - string matching export type (eg CSV TXF etc)
;; Out: if error, #f
(define-public (gnc:cmdline-template-export report export-type)
  (match (reportname->templates report)
    ((template) (template-export report template export-type #f))
    (_ (gnc:error report " does not match unique report") #f)))

;; In: report - string matching reportname
;; Out: a number, or #f if error
(define-public (gnc:cmdline-get-report-id report)
  (match (reportname->templates report)
    ((template) (gnc:make-report (gnc:report-template-report-guid template)))
    (_ (gnc:error report " does not match unique report") #f)))
