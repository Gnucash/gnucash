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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash main))
(use-modules (gnucash printf))
(use-modules (sw_report_system))

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
(define gnc:menuname-utility (N_ "_Sample & Custom"))
(define gnc:menuname-custom (N_ "_Custom"))
(define gnc:pagename-general (N_ "General"))
(define gnc:pagename-accounts (N_ "Accounts"))
(define gnc:pagename-display (N_ "Display"))
(define gnc:optname-reportname (N_ "Report name"))
(define gnc:optname-stylesheet (N_ "Stylesheet"))

;; We want to warn users if they've got an old-style, non-guid custom
;; report-template, but only once
(define gnc:old-style-report-warned #f)

;; A <report-template> represents one of the available report types.
(define <report-template>
  (make-record-type "<report-template>"
                    ;; The data items in a report record
                    '(version name report-guid parent-type options-generator
                              options-cleanup-cb options-changed-cb
                              renderer in-menu? menu-path menu-name
                              menu-tip export-types export-thunk)))

;; if args is supplied, it is a list of field names and values
(define (gnc:define-report . args)
  ;; For now the version is ignored, but in the future it'll let us
  ;; change behaviors without breaking older reports.
  ;;
  ;; The renderer should be a function that accepts one argument, a
  ;; set of options, and generates the report. the renderer must
  ;; return as its final value an <html-document> object.

  (define (blank-report)
    ((record-constructor <report-template>)
     #f                         ;; version
     #f                         ;; name
     #f                         ;; report-guid
     #f                         ;; parent-type (meaning guid of report-template this template is based on)
     #f                         ;; options-generator
     #f                         ;; options-cleanup-cb
     #f                         ;; options-changed-cb
     #f                         ;; renderer
     #t                         ;; in-menu?
     #f                         ;; menu-path
     #f                         ;; menu-name
     #f                         ;; menu-tip
     #f                         ;; export-types
     #f                         ;; export-thunk
     ))

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
	     ;; only process reports that have a report-guid
             (gnc:report-template-report-guid report-rec))
	(let* ((report-guid (gnc:report-template-report-guid report-rec))
	       (name (gnc:report-template-name report-rec))
	       (tmpl (hash-ref *gnc:_report-templates_* report-guid)))
	  (if (not tmpl)
	      (hash-set! *gnc:_report-templates_*
			 report-guid report-rec)
	      (begin
		;; FIXME: We should pass the top-level window
		;; instead of the '() to gnc-error-dialog, but I
		;; have no idea where to get it from.
		(gnc-error-dialog '() (string-append (_ "One of your reports has a report-guid that is a duplicate. Please check the report system, especially your saved reports, for a report with this report-guid: ") report-guid))
		)))
	(begin
	  (if (gnc:report-template-name report-rec)
	      (begin
		;; we've got an old style report with no report-id, give it an arbitrary one
		(gnc:report-template-set-report-guid! report-rec (guid-new-return))
		
		;; we also need to give it a parent-type, so that it will restore from the open state properly
		;; we'll key that from the only known good way to tie back to the original report -- the renderer
		(hash-for-each
		 (lambda (id rec) 
		   (if (and (equal? (gnc:report-template-renderer rec) 
				    (gnc:report-template-renderer report-rec))
			    (not (gnc:report-template-parent-type rec)))
		       (begin
			 (gnc:debug "gnc:define-report: setting parent-type of " (gnc:report-template-name report-rec) " to " (gnc:report-template-report-guid rec))
			 (gnc:report-template-set-parent-type! report-rec (gnc:report-template-report-guid rec))
			 (gnc:debug "done setting, is now " (gnc:report-template-parent-type report-rec))))) 
		 *gnc:_report-templates_*)


		;; re-save this old-style report in the new format
		(gnc:report-template-save-to-savefile report-rec)
		(gnc:debug "complete saving " (gnc:report-template-name report-rec) " in new format")
		))


	  
	  (if (not gnc:old-style-report-warned)
	      (begin
		(set! gnc:old-style-report-warned #t)
		(gnc-error-dialog '() (string-append (_ "The GnuCash report system has been upgraded. Your old saved reports have been transfered into a new format. If you experience trouble with saved reports, please contact the GnuCash development team.")))))
	  (hash-set! *gnc:_report-templates_*
		     (gnc:report-template-report-guid report-rec) report-rec)
	  (gnc:warn "gnc:define-report: old-style report. setting guid for " (gnc:report-template-name report-rec) " to " (gnc:report-template-report-guid report-rec)))
	)))

(define gnc:report-template-version
  (record-accessor <report-template> 'version))
(define gnc:report-template-report-guid
  (record-accessor <report-template> 'report-guid))
(define gnc:report-template-set-report-guid!
  (record-modifier <report-template> 'report-guid))
(define gnc:report-template-name
  (record-accessor <report-template> 'name))
(define gnc:report-template-set-name
  (record-modifier <report-template> 'name))
(define gnc:report-template-parent-type
  (record-accessor <report-template> 'parent-type))
(define gnc:report-template-set-parent-type!
  (record-modifier <report-template> 'parent-type))
(define gnc:report-template-options-generator
  (record-accessor <report-template> 'options-generator))
(define gnc:report-template-options-cleanup-cb
  (record-accessor <report-template> 'options-cleanup-cb))
(define gnc:report-template-options-changed-cb
  (record-accessor <report-template> 'options-changed-cb))
(define gnc:report-template-renderer
  (record-accessor <report-template> 'renderer))
(define gnc:report-template-in-menu?
  (record-accessor <report-template> 'in-menu?))
(define gnc:report-template-menu-path
  (record-accessor <report-template> 'menu-path))
(define gnc:report-template-menu-name
  (record-accessor <report-template> 'menu-name))
(define gnc:report-template-menu-tip
  (record-accessor <report-template> 'menu-tip))
(define gnc:report-template-export-types
  (record-accessor <report-template> 'export-types))
(define gnc:report-template-export-thunk
  (record-accessor <report-template> 'export-thunk))

(define (gnc:report-template-new-options/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
        (gnc:report-template-new-options templ)
        #f)))

(define (gnc:report-template-menu-name/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
        (or (gnc:report-template-menu-name templ)
            (gnc:report-template-name templ))
        #f)))

(define (gnc:report-template-renderer/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
        (gnc:report-template-renderer templ)
        #f)))

(define (gnc:report-template-new-options report-template)
  (let ((generator (gnc:report-template-options-generator report-template))
        (namer 
         (gnc:make-string-option 
          gnc:pagename-general gnc:optname-reportname "0a"
          (N_ "Enter a descriptive name for this report.")
          (_ (gnc:report-template-name report-template))))
        (stylesheet 
         (gnc:make-multichoice-option 
          gnc:pagename-general gnc:optname-stylesheet "0b"
          (N_ "Select a stylesheet for the report.")
          (string->symbol (N_ "Default"))
          (map 
           (lambda (ss)
             (vector 
              (string->symbol (gnc:html-style-sheet-name ss))
              (gnc:html-style-sheet-name ss)
              (string-append (gnc:html-style-sheet-name ss) 
                             " " (_ "stylesheet."))))
           (gnc:get-html-style-sheets)))))

    (let ((options
           (if (procedure? generator)
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
(define <report>
  (make-record-type "<report>"
                    '(type id options dirty? needs-save? editor-widget ctext custom-template)))

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

(define gnc:report-needs-save? 
  (record-accessor <report> 'needs-save?))

(define gnc:report-set-needs-save?!
  (record-modifier <report> 'needs-save?))

(define gnc:report-dirty? 
  (record-accessor <report> 'dirty?))

(define gnc:report-set-dirty?-internal!
  (record-modifier <report> 'dirty?))

(define (gnc:report-set-dirty?! report val)
  (gnc:report-set-dirty?-internal! report val)
  (let* ((template (hash-ref *gnc:_report-templates_* 
                             (gnc:report-type report)))
         (cb  (gnc:report-template-options-changed-cb template)))
    (if (and cb (procedure? cb))
        (cb report))))

(define gnc:report-editor-widget 
  (record-accessor <report> 'editor-widget))

(define gnc:report-set-editor-widget!
  (record-modifier <report> 'editor-widget))

;; ctext is for caching the rendered html
(define gnc:report-ctext 
  (record-accessor <report> 'ctext))

(define gnc:report-set-ctext!
  (record-modifier <report> 'ctext))

(define gnc:report-custom-template
  (record-accessor <report> 'custom-template))

(define gnc:report-set-custom-template!
  (record-modifier <report> 'custom-template))

;; gnc:make-report instantiates a report from a report-template.
;; The actual report is stored away in a hash-table -- only the id is returned.
(define (gnc:make-report template-id . rest)
  (let* ((template-parent (gnc:report-template-parent-type (hash-ref *gnc:_report-templates_* template-id)))
	 (report-type (if template-parent
			  template-parent
			  template-id))
         (custom-template (if template-parent
                              template-id
                              ""))
	 (r ((record-constructor <report>) 
            report-type ;; type
            #f            ;; id
            #f            ;; options
            #t            ;; dirty
            #f            ;; needs-save
            #f            ;; editor-widget
            #f            ;; ctext
            custom-template ;; custom-template
            ))
        (template (hash-ref *gnc:_report-templates_* template-id))
        )
    (let ((options 
           (if (not (null? rest))
               (car rest)
               (gnc:report-template-new-options template))))
      (gnc:report-set-options! r options)
      (gnc:options-register-callback 
       #f #f 
       (lambda () 
         (gnc:report-set-dirty?! r #t)
         (let ((cb (gnc:report-template-options-changed-cb template)))
           (if cb
               (cb r))))
       options))

    (gnc:report-set-id! r (gnc-report-add r))
    (gnc:report-id r))
  )


(define (gnc:restore-report-by-guid id template-id template-name options)
  (if options
      (let ((r ((record-constructor <report>)
		 template-id id options #t #t #f #f "")))
	 (gnc-report-add r))
      (begin
	(gnc-error-dialog '() (string-append "Report Failed! One of your previously opened reports has failed to open. The template on which it was based: " template-name ", was not found."))
	#f))
  )

(define (gnc:restore-report-by-guid-with-custom-template id template-id template-name custom-template-id options)
  (if options
      (let ((r ((record-constructor <report>)
                 template-id id options #t #t #f #f custom-template-id)))
         (gnc-report-add r))
      (begin
        (gnc-error-dialog '() (string-append "Report Failed! One of your previously opened reports has failed to open. The template on which it was based: " template-name ", was not found."))
        #f))
  )

(define (gnc:make-report-options template-id)
  (let ((template (hash-ref *gnc:_report-templates_* template-id)))
    (if template
        (gnc:report-template-new-options template)
        #f)))

;; A convenience wrapper to get the report-template's export types from
;; an instantiated report.
(define (gnc:report-export-types report)
  (let ((template (hash-ref *gnc:_report-templates_* 
                            (gnc:report-type report))))
    (if template
        (gnc:report-template-export-types template)
        #f)))

;; A convenience wrapper to get the report-template's export thunk from
;; an instantiated report.
(define (gnc:report-export-thunk report)
  (let ((template (hash-ref *gnc:_report-templates_* 
                            (gnc:report-type report))))
    (if template
        (gnc:report-template-export-thunk template)
        #f)))

(define (gnc:report-menu-name report)
  (let ((template (hash-ref *gnc:_report-templates_* 
                            (gnc:report-type report))))
    (if template
        (or (gnc:report-template-menu-name template)
	    (gnc:report-name report))
        #f)))

(define (gnc:report-name report) 
  (let* ((opt (gnc:report-options report)))
    (if opt
        (gnc:option-value
         (gnc:lookup-option opt gnc:pagename-general gnc:optname-reportname))
        #f)))

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

(define (gnc:all-report-template-guids)
   (hash-fold 
    (lambda (k v p)
      (cons k p)) 
    '() *gnc:_report-templates_*))

;; return a list of the custom report template guids.
(define (gnc:custom-report-template-guids)
   (hash-fold
    (lambda (k v p)
       (if (gnc:report-template-parent-type v)
	  (begin
	    (gnc:debug "template " v)
	    (cons k p))
	  p))
      '() *gnc:_report-templates_*))

(define (gnc:find-report-template report-type) 
  (hash-ref *gnc:_report-templates_* report-type))

(define (gnc:report-template-is-custom/template-guid? guid)
  (let* ((custom-template (if (string? guid) (if (string-null? guid) #f (hash-ref *gnc:_report-templates_* guid)) #f))
         (parent-type (if custom-template (gnc:report-template-parent-type custom-template) #f)))

        (if parent-type
            #t
            #f)))

(define (gnc:is-custom-report-type report)
  (gnc:report-template-is-custom/template-guid? (gnc:report-custom-template report)))

;; This function should be called right before changing a custom-template's name
;; to test if the new name is unique among the existting custom reports.
;; If not the calling function can prevent the name from being updated.
(define (gnc:report-template-has-unique-name? templ-guid new-name)
  (let* ((unique? #t))

    (if new-name 
      (hash-for-each 
       (lambda (id rec)
         (if (and (not (equal? templ-guid id))
                  (gnc:report-template-is-custom/template-guid? id)
                  (equal? new-name (gnc:report-template-name rec)))
             (set! unique? #f)))
       *gnc:_report-templates_*))
    unique?))

;; Generate a unique custom template name using the given string as a base
;; If this string already exists as a custom template name, a
;; number will be appended to it.
(define (gnc:report-template-make-unique-name new-name)
  (let* ((unique-name new-name)
         (counter 0)
         (unique? (gnc:report-template-has-unique-name? #f unique-name)))

    (while (not unique?)
      (begin
           (set! counter (+ counter 1))
           (set! unique-name (string-append new-name (number->string counter)))
           (set! unique? (gnc:report-template-has-unique-name? #f unique-name))))
    unique-name))


;; Load and save functions


;; Generate guile code required to recreate an instatiated report
(define (gnc:report-serialize report)
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
    (gnc:report-type report) (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
   (gnc:generate-restore-forms (gnc:report-options report) "options")
   ;; 2.6->2.4 compatibility code prefix
   ;; Temporary check to make the new report saving code more or less backwards
   ;; compatible with older gnucash versions. This can be removed again in 2.8.
   "(if (defined? 'gnc:restore-report-by-guid-with-custom-template)\n"
   ;; end of 2.6->2.4 compatibility code prefix.
   (format 
    #f "  (gnc:restore-report-by-guid-with-custom-template ~S ~S ~S ~S options)\n"
    (gnc:report-id report) (gnc:report-type report)
    (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report)))
    (gnc:report-custom-template report))
   ;; 2.6->2.4 compatibility code suffix
   (format 
    #f "  (gnc:restore-report-by-guid ~S ~S ~S options))\n"
    (gnc:report-id report) (gnc:report-type report)
    (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
    ;; end of 2.6->2.4 compatibility code suffix.
   ")"
  ))

;; Generate guile code required to recreate embedded report instances
(define (gnc:report-serialize-embedded embedded-reports)
  (let* ((result-string ""))
    (if embedded-reports
        (for-each
         (lambda (subreport-id)
           (let*
               ((subreport (gnc-report-find subreport-id))
                (subreport-options-text (gnc:report-serialize subreport)))
             (set! result-string (string-append
                                  result-string
                                  ";;;; Options for embedded report\n"
                                  subreport-options-text))))
         embedded-reports))
    result-string))

(define (gnc:report-template-serialize-internal name type templ-name options guid)
  (let* ((embedded-serialized (gnc:report-serialize-embedded (gnc:report-embedded-list options)))
         (result (string-append 
   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
   (format #f ";; Options for saved report ~S, based on template ~S\n"
           name type)
   (format
    #f "(let ()\n (define (options-gen)\n  (let ((options (gnc:report-template-new-options/report-guid ~S ~S)))\n"
    type templ-name)
   (gnc:generate-restore-forms options "options")
   (if embedded-serialized
       embedded-serialized
       "")
   "  options))\n"
   (format 
    #f " (gnc:define-report \n  'version 1\n  'name ~S\n  'report-guid ~S\n  'parent-type ~S\n  'options-generator options-gen\n  'menu-path (list gnc:menuname-custom)\n  'renderer (gnc:report-template-renderer/report-guid ~S ~S)))\n\n"
    name
    (if guid
        guid
        (guid-new-return)) ;; when saving a report, we need to create a guid for it for later reloading
    type
    type
    templ-name))))
    (gnc:debug result)
    result))

;; Convert an instantiated report into a report template
;; and generate the guile code required to recreate this template
(define (gnc:report-template-serialize-from-report report)
  ;; clean up the options if necessary.  this is only needed 
  ;; in special cases.  
  (let* ((template 
          (hash-ref  *gnc:_report-templates_* 
                     (gnc:report-type report)))
         (thunk (gnc:report-template-options-cleanup-cb template)))
    (if thunk 
        (thunk report)))
  
  ;; save them
  (let* ((name (gnc:report-template-make-unique-name (gnc:report-name report)))
         (type (gnc:report-type report))
         (templ-name (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
         (options (gnc:report-options report)))
    (gnc:report-template-serialize-internal name type templ-name options #f)))

;; Generate guile code required to recreate a report template
;; Note: multi column report templates encapsulate instantiated reports, not other report templates
;;       this means that the template recreation code must also contain the code to instantiate
;;       these embedded report instances. This results in a mix of template and instatiated reports
;;       in the saved reports file...
(define (gnc:report-template-serialize report-template)
  (let* ((name (gnc:report-template-name report-template))
         (type (gnc:report-template-parent-type report-template))
         (templ-name (gnc:report-template-name (hash-ref *gnc:_report-templates_* type)))
         (options (gnc:report-template-new-options report-template))
         (guid (gnc:report-template-report-guid report-template)))
    (gnc:report-template-serialize-internal name type templ-name options guid)))

;; Convert a report into a report template and save this template in the savefile
;; Under specific conditions the we will attempt to replace the current report's
;; template instead of simply adding a new template to the file.
;; These condititions are:
;; 1. the report is an instance of an existing custom report template
;;    (ie a template that is stored in the savefile already)
;; 2. an overwrite is requestes by setting overwrite? to #t
(define (gnc:report-to-template report overwrite?)
  (let* ((custom-template-id (gnc:report-custom-template report))
         (overwrite-ok? (and (gnc:report-template-is-custom/template-guid? custom-template-id) overwrite?))
         ;; Generate a serialized report-template with a random guid
         (saved-form (gnc:report-template-serialize-from-report report))
         ;; Immediatly evaluate the serialized report template to
         ;; - check if it's error free and can be deserialized
         ;; - load it into the runtime for immediate use by the user
         ;; (Bug #342206)
         (save-result (eval-string saved-form)))

    (if (record? save-result)
        (begin
          ;; If it's ok to overwrite the old template, delete it now.
          (if overwrite-ok?
            (let ((templ-name (gnc:report-template-name (hash-ref *gnc:_report-templates_* custom-template-id))))
              ;; We're overwriting, which needs some additional steps
              ;; 1. Remove the newly generated template from the template list again
              (hash-remove! *gnc:_report-templates_* (gnc:report-template-report-guid save-result))
              ;; 2. We still have the template record available though, so adapt it to
              ;;    the template we want to override (ie update guid and name)
              (gnc:report-template-set-report-guid! save-result custom-template-id)
              (gnc:report-template-set-name save-result templ-name)
              ;; 3. Overwrite the template with the new one
              (hash-set! *gnc:_report-templates_* custom-template-id save-result)
              ))

          ;; Regardless of how we got here, we now have a new template to write
          ;; so let's write it
          (if (gnc:save-all-reports)
              (let ((templ-guid (gnc:report-template-report-guid save-result)))
                   ;; Indicate the report was instantiated from the new template
                   (gnc:report-set-custom-template! report templ-guid)
                   ;; Inform the calling function of the new template's guid
                   templ-guid)
              #f))
        #f)))

;; Convert a report into a new report template and add this template to the save file
(define (gnc:report-to-template-new report)
  (gnc:report-to-template report #f))

;; Get the current report's template and try to update it with the report's current
;; settings. This will only be possible if the report was already based on a
;; custom report template. If that's not the case, a new template will be added instead.
(define (gnc:report-to-template-update report)
  (gnc:report-to-template report #t))

(define (gnc:report-template-save-to-savefile report-template)
  (let ((saved-form (gnc:report-template-serialize report-template)))
          (gnc-saved-reports-write-to-file saved-form #f)))

;; save all custom reports, moving the old version of the
;; saved-reports file aside as a backup
;; return #t if all templates were saved successfully
(define (gnc:save-all-reports)
  (let ((save-ok? #t))
       (gnc-saved-reports-backup)
       (gnc-saved-reports-write-to-file "" #t)
       (hash-for-each (lambda (k v)
                        (if (gnc:report-template-parent-type v)
                            (begin
                              (gnc:debug "saving report " k)
                              (if (not (gnc:report-template-save-to-savefile v))
                                  (set! save-ok? #f)
                              ))))
                      *gnc:_report-templates_*)
    save-ok?))


;; gets the renderer from the report template;
;; gets the stylesheet from the report;
;; renders the html doc and caches the resulting string;
;; returns the html string.
;; Now accepts either an html-doc or finished HTML from the renderer -
;; the former requires further processing, the latter is just returned.
(define (gnc:report-render-html report headers?)
  (if (and (not (gnc:report-dirty? report))
           (gnc:report-ctext report))
      ;; if there's clean cached text, return it 
      ;;(begin
      (gnc:report-ctext report)
      ;;  )
      
      ;; otherwise, rerun the report 
      (let ((template (hash-ref *gnc:_report-templates_* 
                                (gnc:report-type report)))
	    (doc #f))
        (set! doc (if template
                      (let* ((renderer (gnc:report-template-renderer template))
                             (stylesheet (gnc:report-stylesheet report))
                             (doc (renderer report))
                             (html #f))
                        (if (string? doc)
                          (set! html doc)
                          (begin 
                            (gnc:html-document-set-style-sheet! doc stylesheet)
                            (set! html (gnc:html-document-render doc headers?))))
                        (gnc:report-set-ctext! report html) ;; cache the html
                        (gnc:report-set-dirty?! report #f)  ;; mark it clean
                        html)
                      #f))
	doc))) ;; YUK! inner doc is html-doc object; outer doc is a string.

;; looks up the report by id and renders it with gnc:report-render-html
;; marks the cursor busy during rendering; returns the html
;; Note: the final html document is post-processed to ensure there's only one single
;;       inclusion of the jquery/jqplot libraries. This is only needed to fix multicolumn
;;       reports with multiple charts, but doing it more generally is an
;;       acceptable hack until a cleaner solution can be found (bug #704525)
(define (gnc:report-run id)
  (let ((report (gnc-report-find id))
	(html #f))
    (gnc-set-busy-cursor '() #t)
    (gnc:backtrace-if-exception 
     (lambda ()
       (if report
	   (begin 
	     (set! html (gnc:report-render-html report #t))
             (set! html (gnc:substring-replace-from-to html "jquery.min.js" "" 2 -1))
             (set! html (gnc:substring-replace-from-to html "jquery.jqplot.js" "" 2 -1))
           ))))
    (gnc-unset-busy-cursor '())
    html))


;; "thunk" should take the report-type and the report template record
(define (gnc:report-templates-for-each thunk)
  (hash-for-each (lambda (report-id template) (thunk report-id template))
                 *gnc:_report-templates_*))

;; return the list of reports embedded in the specified report
(define (gnc:report-embedded-list options)
  (let* ((option (gnc:lookup-option options "__general" "report-list")))
    (if option
	(let ((opt-value (gnc:option-value option)))
	  (map (lambda (x) (car x)) opt-value))
	#f)))

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
    (if templ
      (begin
        (gnc:debug "Renaming report " template-guid)
        (gnc:report-template-set-name templ new-name)
        (gnc:save-all-reports)))))

;; Legacy functions
;;;;;;;;;;;;;;;;;;;

;; Legacy : the following 3 functions are only needed to
;; load a saved-reports file version 2.0

(define (gnc:report-template-new-options/name template-name)
  (let ((templ #f))
    (hash-for-each 
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
           (set! templ (hash-ref *gnc:_report-templates_* id)))) *gnc:_report-templates_*)
    (if templ
        (gnc:report-template-new-options templ)
        #f)))

(define (gnc:report-template-menu-name/name template-name)
  (let ((templ #f))
    (hash-for-each
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
           (set! templ (hash-ref *gnc:_report-templates_* id)))) *gnc:_report-templates_*)
    (if templ
        (or (gnc:report-template-menu-name templ)
            (gnc:report-template-name templ))
        #f)))

(define (gnc:report-template-renderer/name template-name)
  (let ((templ #f))
    (hash-for-each
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
           (set! templ (hash-ref *gnc:_report-templates_* id)))) *gnc:_report-templates_*)
    (if templ
        (gnc:report-template-renderer templ)
        #f)))

;; Used internally only to convert a report template name into a corresponding guid
;; Note that this may fail if several reports exist with the same name
(define (gnc:report-template-name-to-id template-name)
  (let ((template-id #f))
    (hash-for-each
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
           (set! template-id id))) *gnc:_report-templates_*)
    template-id))

;; We want to warn users when we are trying to restore reports stored in the legacy
;; format (based on name instead of guid), but only once
(define gnc:old-style-restore-warned #f)

;; Legacy: this function is needed only to restore
;; a saved report when loading a book last saved in GnuCash 2.2
(define (gnc:restore-report id template-name options)
  (if options
      (let ((r ((record-constructor <report>)
                (gnc:report-template-name-to-id template-name) id options #t #t #f #f "")))
           ;; Warn user (one time) we're attempting to restore old style reports
           (if (not gnc:old-style-restore-warned)
               (begin
                 (set! gnc:old-style-restore-warned #t)
                 (gnc-warning-dialog '() (string-append (_ "Some reports stored in a legacy format were found. This format is not supported anymore so these reports may not have been restored properly.")))))
           (gnc-report-add r))
      (begin
        (gnc-error-dialog '() (string-append "Report Failed! One of your previously opened reports has failed to open. The template on which it was based: " template-name ", was not found."))
        #f)))
