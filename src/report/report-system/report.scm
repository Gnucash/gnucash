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
(use-modules (sw_report_system))

;; This hash should contain all the reports available and will be used
;; to generate the reports menu whenever a new window opens and to
;; figure out what to do when a report needs to be generated.
;;
;; The key is the string naming the report (the report "type") and the
;; value is the report definition structure.
(define *gnc:_report-templates_* (make-hash-table 23))

;; Define those strings here to make changes easier and avoid typos.
(define gnc:menuname-reports "Reports/StandardReports")
(define gnc:menuname-asset-liability (N_ "_Assets & Liabilities"))
(define gnc:menuname-income-expense (N_ "_Income & Expense"))
(define gnc:menuname-taxes (N_ "_Taxes"))
(define gnc:menuname-utility (N_ "_Sample & Custom"))
(define gnc:menuname-custom (N_ "_Custom"))
(define gnc:pagename-general (N_ "General"))
(define gnc:pagename-accounts (N_ "Accounts"))
(define gnc:pagename-display (N_ "Display"))
(define gnc:optname-reportname (N_ "Report name"))

;; we want to warn users if they've got an old-style, non-guid saved
;; report, but only once
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
     #f                         ;; report-guid for backwards compat of newer reports
     #f                         ;; parent-type for backwards compat of newer reports
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
		 *gnc:_report-templates_*)))
	  
	  (if (not gnc:old-style-report-warned)
	      (begin
		(set! gnc:old-style-report-warned #t)
		(gnc-error-dialog '() (string-append (_ "Your report system includes one or more reports without a proper report-guid field. This report may break without warning in future versions of GnuCash. Please review your saved reports file and update those reports.")))))
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
(define gnc:report-template-parent-type
  (record-accessor <report-template> 'parent-type))
(define gnc:report-template-set-parent-type!
  (record-modifier <report-template> 'parent-type))
(define gnc:report-template-set-name
  (record-modifier <report-template> 'name))
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

(define (gnc:report-template-name-to-id template-name)
  (let ((template-id #f))
    (hash-for-each
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
	   (set! template-id id))) *gnc:_report-templates_*)
    template-id))

(define (gnc:report-template-new-options/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
        (gnc:report-template-new-options templ)
        #f)))

(define (gnc:report-template-new-options/name template-name)
  (let ((templ #f))
    (hash-for-each 
     (lambda (id rec)
       (if (equal? template-name (gnc:report-template-name rec))
	   (set! templ (hash-ref *gnc:_report-templates_* id)))) *gnc:_report-templates_*)
    (if templ
	(gnc:report-template-new-options templ)
	#f)))

(define (gnc:report-template-menu-name/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
	(or (gnc:report-template-menu-name templ)
	    (gnc:report-template-name templ))
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

(define (gnc:report-template-renderer/report-guid template-id template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-id)))
    (if templ
	(gnc:report-template-renderer templ)
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

(define (gnc:report-template-new-options report-template)
  (let ((generator (gnc:report-template-options-generator report-template))
        (namer 
         (gnc:make-string-option 
          gnc:pagename-general gnc:optname-reportname "0a"
          (N_ "Enter a descriptive name for this report")
          (_ (gnc:report-template-name report-template))))
        (stylesheet 
         (gnc:make-multichoice-option 
          gnc:pagename-general (N_ "Stylesheet") "0b"
          (N_ "Select a stylesheet for the report.")
          (string->symbol (N_ "Default"))
          (map 
           (lambda (ss)
             (vector 
              (string->symbol (gnc:html-style-sheet-name ss))
              (gnc:html-style-sheet-name ss)
              (string-append (gnc:html-style-sheet-name ss) 
                             " " (_ "Stylesheet"))))
           (gnc:get-html-style-sheets)))))

    (if (procedure? generator)
        (let ((options (generator)))
          (gnc:register-option options stylesheet)
          (gnc:register-option options namer)
          options)
        (let ((options (gnc:new-options)))
          (gnc:register-option options stylesheet)
          (gnc:register-option options names)
          options))))

;; A <report> represents an instantiation of a particular report type.
(define <report>
  (make-record-type "<report>"
                    '(type id options dirty? needs-save? editor-widget ctext)))

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

;; gnc:make-report instantiates a report from a report-template.
;; The actual report is stored away in a hash-table -- only the id is returned.
(define (gnc:make-report template-name . rest)
  (let* ((template-parent (gnc:report-template-parent-type (hash-ref *gnc:_report-templates_* template-name)))
	 (report-type (if template-parent
			  template-parent
			  template-name))
	 (r ((record-constructor <report>) 
            report-type ;; type
            #f            ;; id
            #f            ;; options
            #t            ;; dirty
            #f            ;; needs-save
            #f            ;; editor-widget
            #f            ;; ctext
            ))
        (template (hash-ref *gnc:_report-templates_* template-name))
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

;; This is the function that is called when saved reports are evaluated.
(define (gnc:restore-report id template-name options)
  (let ((r ((record-constructor <report>)
            (gnc:report-template-name-to-id template-name) id options #t #t #f #f)))
    (gnc-report-add r))
  )

(define (gnc:restore-report-by-guid id template-id template-name options)
  (let ((r ((record-constructor <report>)
            template-id id options #t #t #f #f)))
    (gnc-report-add r))
  )

(define (gnc:make-report-options template-name)
  (let ((template (hash-ref *gnc:_report-templates_* template-name)))
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
                     (N_ "Stylesheet"))))))

(define (gnc:report-set-stylesheet! report stylesheet)
  (gnc:option-set-value
   (gnc:lookup-option 
    (gnc:report-options report)
    gnc:pagename-general 
    (N_ "Stylesheet"))
   (string->symbol 
    (gnc:html-style-sheet-name stylesheet))))

(define (gnc:all-report-template-names)
  (sort 
   (hash-fold 
    (lambda (k v p)
      (cons k p)) 
    '() *gnc:_report-templates_*)
   string<?))

(define (gnc:find-report-template report-type) 
  (hash-ref *gnc:_report-templates_* report-type))

(define (gnc:report-generate-restore-forms report)
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
   (simple-format #f ";; options for report ~S\n" (gnc:report-name report))
   (simple-format
    #f "(let ((options (gnc:report-template-new-options/report-guid ~S ~S)))\n"
    (gnc:report-type report) (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
   (gnc:generate-restore-forms (gnc:report-options report) "options")
   (simple-format 
    #f "  (gnc:restore-report-by-guid ~S ~S ~S options))\n"
    (gnc:report-id report) (gnc:report-type report) (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))))

(define (gnc:report-generate-saved-forms report)
  ;; clean up the options if necessary.  this is only needed 
  ;; in special cases.  
  (let* ((template 
          (hash-ref  *gnc:_report-templates_* 
                     (gnc:report-type report)))
         (thunk (gnc:report-template-options-cleanup-cb template)))
    (if thunk 
        (thunk report)))
  
  ;; save them 
  (string-append 
   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
   (simple-format #f ";; Options for saved report ~S, based on template ~S\n"
		  (gnc:report-name report) (gnc:report-type report))
   (simple-format
    #f "(let ()\n (define (options-gen)\n  (let ((options (gnc:report-template-new-options/report-guid ~S ~S)))\n"
    (gnc:report-type report) (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))
   (gnc:generate-restore-forms (gnc:report-options report) "options")
   "  options))\n"
   (simple-format 
    #f " (gnc:define-report \n  'version 1\n  'name ~S\n  'report-guid ~S\n  'parent-type ~S\n  'options-generator options-gen\n  'menu-path (list gnc:menuname-custom)\n  'renderer (gnc:report-template-renderer/report-guid ~S ~S)))\n\n"
    (gnc:report-name report)
    (guid-new-return) ;; when saving a report, we need to create a guid for it for later reloading
    (gnc:report-type report) ;;a saved report also needs its type stored separately to reference the template
    (gnc:report-type report)
    (gnc:report-template-name (hash-ref *gnc:_report-templates_* (gnc:report-type report))))))

(define gnc:current-saved-reports
  (gnc-build-dotgnucash-path "saved-reports-2.0"))

(define (gnc:report-save-to-savefile report)
  (let* ((conf-file-name gnc:current-saved-reports)
         (saved-form (gnc:report-generate-saved-forms report))
         ;; Immediate evaluate the saved form to both load it into the
         ;; runtime, but also so we can check if it's "allowed" to actually
         ;; be written to the saved reports file by inspecting the result.
         ;; #Bug#342206.
         (save-result (eval-string saved-form)))
    (if (record? save-result)
        (begin
          (display saved-form
                   (open-file conf-file-name "a"))
          (force-output)
	  (let ((report-name (gnc:report-name report)))
	    (gnc-info-dialog
	     '()
	     (sprintf 
	      #f (_ "Your report \"%s\" has been saved into the configuration file \"%s\".  The report will be available in the menu Reports -> Custom at the next startup of GnuCash.")
	      (if (and report-name (not (string-null? report-name)))
		  (gnc:gettext report-name)
		  (gnc:gettext "Untitled"))
	      conf-file-name)))
	  ))))

;; gets the renderer from the report template;
;; gets the stylesheet from the report;
;; renders the html doc and caches the resulting string;
;; returns the html string.
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
                        (gnc:html-document-set-style-sheet! doc stylesheet)
                        (set! html (gnc:html-document-render doc headers?))
                        (gnc:report-set-ctext! report html) ;; cache the html
                        (gnc:report-set-dirty?! report #f)  ;; mark it clean
                        html)
                      #f))
	doc))) ;; YUK! inner doc is html-doc object; outer doc is a string.

;; looks up the report by id and renders it with gnc:report-render-html
;; marks the cursor busy during rendering; returns the html
(define (gnc:report-run id)
  (let ((report (gnc-report-find id))
	(html #f))
    (gnc-set-busy-cursor '() #t)
    (gnc:backtrace-if-exception 
     (lambda ()
       (if report
	   (begin 
	     (set! html (gnc:report-render-html report #t))))))
    (gnc-unset-busy-cursor '())
    html))


;; "thunk" should take the report-type and the report template record
(define (gnc:report-templates-for-each thunk)
  (hash-for-each (lambda (report-id template) (thunk report-id template))
                 *gnc:_report-templates_*))

;; return the list of reports embedded in the specified report
(define (gnc:report-embedded-list report)
  (let* ((options (gnc:report-options report))
	 (option (gnc:lookup-option options "__general" "report-list")))
    (if option
	(let ((opt-value (gnc:option-value option)))
	  (map (lambda (x) (car x)) opt-value))
	#f)))
