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

;; Define those strings here to make changes easier and avoid typos.
(define gnc:menuname-asset-liability
  (N_ "_Assets & Liabilities"))
(define gnc:menuname-income-expense 
  (N_ "_Income & Expense"))
(define gnc:menuname-taxes (N_ "_Taxes"))
(define gnc:pagename-general (N_ "General"))
(define gnc:pagename-accounts (N_ "Accounts"))
(define gnc:pagename-display (N_ "Display"))
(define gnc:optname-reportname (N_ "Report name"))



(define (gnc:report-menu-setup)
  ;; since this menu gets added to every child window, we say it 
  ;; comes after the "_File" menu. 
  (define menu (gnc:make-menu (N_ "New _Report")
                              (list "_File" "New _Account Tree")))
  (define menu-namer (gnc:new-menu-namer))
  (define tax-menu (gnc:make-menu gnc:menuname-taxes
                                  (list "_File" "New _Report" "")))
  (define income-expense-menu
    (gnc:make-menu gnc:menuname-income-expense
                   (list "_File" "New _Report" "")))
  (define asset-liability-menu
    (gnc:make-menu gnc:menuname-asset-liability
                   (list "_File" "New _Report" "")))
  (define menu-hash (make-hash-table 23))

  (define (add-report-menu-item name report)
    (if (gnc:report-template-in-menu? report)
        (let ((title (string-append (_ "Report") ": " (_ name)))
              (menu-path (gnc:report-template-menu-path report))
              (menu-name (gnc:report-template-menu-name report))
              (menu-tip (gnc:report-template-menu-tip report))
              (item #f))

          (if (not menu-path)
              (set! menu-path '(""))
              (set! menu-path
                    (append menu-path '(""))))

          (set! menu-path (append (list "_File" "New _Report") menu-path))

          (if menu-name (set! name menu-name))

          (if (not menu-tip)
              (set! menu-tip
                    (sprintf #f (_ "Display the %s report") name)))

          (set! item
                (gnc:make-menu-item
                 ((menu-namer 'add-name) name)
                 menu-tip
                 menu-path
                 (lambda ()
                   (let ((rept (gnc:make-report
                                (gnc:report-template-name report))))
                     (gnc:main-window-open-report rept #f)))))
          (gnc:add-extension item))))

  (gnc:add-extension menu)

  ;; add the menu option to edit style sheets 
  (gnc:add-extension
   (gnc:make-menu-item 
    ((menu-namer 'add-name) (_ "Style Sheets..."))
    (_ "Edit report style sheets.")
    (list "_Settings" "")
    (lambda ()
      (gnc:style-sheet-dialog-open))))

;  (gnc:add-extension tax-menu)
  (gnc:add-extension income-expense-menu)
  (gnc:add-extension asset-liability-menu)

  ;; push reports (new items added on top of menu)
  (hash-for-each add-report-menu-item *gnc:_report-templates_*)

  ;; the Welcome to Gnucash-1.6 extravaganza 
  (gnc:add-extension 
   (gnc:make-menu-item 
    ((menu-namer 'add-name) (_ "Welcome Extravaganza")) 
    (_ "Welcome-to-gnucash screen")
    (list "_File" "New _Report" "")
    (lambda ()
      (gnc:make-welcome-report)))))

(define <report-template>
  (make-record-type "<report-template>"
                    ;; The data items in a report record
                    '(version name options-generator options-editor
                              renderer in-menu? menu-path menu-name
                              menu-tip export-thunk)))

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
     #f                         ;; options-generator
     gnc:default-options-editor ;; options-editor
     #f                         ;; renderer
     #t                         ;; in-menu?
     #f                         ;; menu-path
     #f                         ;; menu-name
     #f                         ;; menu-tip
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
(define gnc:report-template-options-editor
  (record-accessor <report-template> 'options-editor))
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
(define gnc:report-template-export-thunk
  (record-accessor <report-template> 'export-thunk))

(define (gnc:report-template-new-options/name template-name)
  (let ((templ (hash-ref *gnc:_report-templates_* template-name)))
    (if templ
        (gnc:report-template-new-options templ)
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
                             (_ " Stylesheet"))))
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

(define <report>
  (make-record-type "<report>"
                    '(type id options parent children 
                           dirty? display-list editor-widget ctext)))

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

(define gnc:report-set-parent-by-id!
  (record-modifier <report> 'parent))

(define (gnc:report-set-parent! report parent)
  (gnc:report-set-parent-by-id! report (gnc:report-id parent)))

(define gnc:report-parent
  (record-accessor <report> 'parent))

(define gnc:report-set-children!
  (record-modifier <report> 'children))

(define (gnc:report-add-child! report child)
  (gnc:report-set-children! 
   report (cons (gnc:report-id child) (gnc:report-children report))))

(define (gnc:report-add-child-by-id! report child)
  (gnc:report-set-children! 
   report (cons child (gnc:report-children report))))

(define gnc:report-dirty? 
  (record-accessor <report> 'dirty?))

(define gnc:report-set-dirty?-internal!
  (record-modifier <report> 'dirty?))

(define (gnc:report-set-dirty?! report val)
  (gnc:report-set-dirty?-internal! report val)
  (if val 
      (begin 
        (if (gnc:report-parent report)
            (gnc:report-set-dirty?! 
             (gnc:find-report (gnc:report-parent report)) val))

        ;; reload the window 
        (for-each 
         (lambda (win)
           (gnc:report-window-reload win))
         (gnc:report-display-list report)))))

(define gnc:report-display-list 
  (record-accessor <report> 'display-list))

(define gnc:report-set-display-list!
  (record-modifier <report> 'display-list))

(define gnc:report-editor-widget 
  (record-accessor <report> 'editor-widget))

(define gnc:report-set-editor-widget!
  (record-modifier <report> 'editor-widget))

(define gnc:report-ctext 
  (record-accessor <report> 'ctext))

(define gnc:report-set-ctext!
  (record-modifier <report> 'ctext))

(define (gnc:report-register-display report window)
  (if (and window report
           (not (member window (gnc:report-display-list report))))
      (begin 
        (gnc:report-set-display-list! 
         report 
         (cons window (gnc:report-display-list report)))
        (for-each 
         (lambda (rep)
           (gnc:report-register-display (gnc:find-report rep) window))
         (gnc:report-children report))

        (let ((parent (gnc:find-report (gnc:report-parent report))))
          (if parent
              (gnc:report-register-display parent window))))))

(define (gnc:report-unregister-display report window)
  (if (and report window
           (member window (gnc:report-display-list report)))
      (begin
        (gnc:report-set-display-list! 
         report 
         (delete window (gnc:report-display-list report)))
        (for-each 
         (lambda (rep)
           (gnc:report-unregister-display (gnc:find-report rep) window))
         (gnc:report-children report))
        (let ((parent (gnc:find-report (gnc:report-parent report))))
          (if parent 
              (gnc:report-unregister-display parent window))))))
  
(define (gnc:report-edit-options report) 
  (let* ((editor-widg (gnc:report-editor-widget report))
         (displist (gnc:report-display-list report)))
    (if editor-widg
        (gnc:report-raise-editor report)
        (begin
          (if (gnc:report-options report) 
              (begin 
                (set! editor-widg
                      ((gnc:report-options-editor report)
                       (gnc:report-options report)
                       report))
                (gnc:report-set-editor-widget! report editor-widg)
                (if (and editor-widg (not (null? displist)))
                    (for-each 
                     (lambda (repwin) 
                       (gnc:report-window-add-edited-report repwin report))
                     displist)))
              (gnc:warning-dialog "This report has no options."))))))
    

(define (gnc:make-report template-name . rest)
  (let ((r ((record-constructor <report>) 
            template-name ;; type
            #f            ;; id
            #f            ;; options
            #f            ;; parent
            '()           ;; children
            #t            ;; dirty
            '()           ;; display-list
            #f            ;; editor-widget
            #f            ;; ctext
            ))
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

(define (gnc:restore-report id template-name parent children options)
  (let ((r ((record-constructor <report>)
            template-name id options parent children #t '() #f #f)))
    (if (>= id *gnc:_report-next-serial_*)
        (set! *gnc:_report-next-serial_* (+ id 1)))
    (hash-set! *gnc:_reports_* id r)))


(define (gnc:make-report-options template-name)
  (let ((template (hash-ref *gnc:_report-templates_* template-name)))
    (if template
        (gnc:report-template-new-options template)
        #f)))

(define (gnc:report-options-editor report) 
  (let ((template 
         (hash-ref  *gnc:_report-templates_* 
                    (gnc:report-type report))))
    (if template
        (gnc:report-template-options-editor template)
        #f)))

(define (gnc:report-export-thunk report)
  (let ((template 
         (hash-ref  *gnc:_report-templates_* 
                    (gnc:report-type report))))
    (if template
        (gnc:report-template-export-thunk template)
        #f)))

(define (gnc:report-name report) 
  (gnc:option-value
   (gnc:lookup-option (gnc:report-options report)
                      gnc:pagename-general gnc:optname-reportname)))

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

(define (gnc:report-remove-by-id id)
  (let ((r (hash-ref *gnc:_reports_* id)))
    (for-each 
     (lambda (child)
       (gnc:report-remove-by-id child))
     (gnc:report-children r))
    (hash-remove! *gnc:_reports_* id)))

(define (gnc:find-report id) 
  (hash-ref *gnc:_reports_* id))

(define (gnc:report-generate-restore-forms-complete report)
  (define (find-root r)
    (let* ((pid (gnc:report-parent r))
           (p (if pid (gnc:find-report pid) #f)))
      (if (not p) r (find-root p))))

  (define (generate-forms/children r)
    (apply 
     string-append 
     (gnc:report-generate-restore-forms r)
     (map 
      (lambda (c)
        (let ((child (gnc:find-report c)))
          (generate-forms/children child)))
      (gnc:report-children r))))

  (let ((toplevel (find-root report)))
    (string-append 
     ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
     (simple-format #f ";; options for report ~A and all parents/children \n" 
                    (gnc:report-name report))
     "(let () \n"
     (generate-forms/children toplevel)
     (simple-format 
      #f "  \"gnc-report:id=~S\"" (gnc:report-id report))
     ")\n")))

(define (gnc:report-generate-restore-forms report)
  (string-append 
   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
   (simple-format #f ";; options for report ~S\n" (gnc:report-name report))
   (simple-format
    #f "(let ((options (gnc:report-template-new-options/name ~S)))\n"
    (gnc:report-type report))
   (gnc:generate-restore-forms (gnc:report-options report) "options")
   (simple-format 
    #f "  (gnc:restore-report ~S ~S ~S '~S options))\n"
    (gnc:report-id report) (gnc:report-type report)
    (gnc:report-parent report)
    (gnc:report-children report))))

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


(define (gnc:report-render-html report headers?)
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
                   (stylesheet (gnc:report-stylesheet report))
                   (doc (renderer report))
                   (html #f))
              (gnc:html-document-set-style-sheet! doc stylesheet)
              (set! html (gnc:html-document-render doc headers?))
              (gnc:report-set-ctext! report html)
              (gnc:report-set-dirty?! report #f)              
              html)
            #f))))

(define (gnc:report-run id)
  (gnc:backtrace-if-exception 
   (lambda ()
     (let ((report (gnc:find-report id))
           (start-time (gettimeofday))
           (html #f))
       (if report
           (begin 
             (set! html (gnc:report-render-html report #t))
             (display "total time to run report: ")
             (display (gnc:time-elapsed start-time (gettimeofday)))
             (newline)
;;             (display html) (newline)
             html)
           #f)))))
              
(gnc:hook-add-dangler gnc:*ui-startup-hook* gnc:report-menu-setup)
