;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-report-gnome-spec)
  :use-module (g-wrap))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-glib-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((mod (gw:new-module "gw-report-gnome")))
  (define (standard-c-call-gen result func-call-code)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (add-standard-result-handlers! type c->scm-converter)
    (define (standard-pre-handler result)
      (let* ((ret-type-name (gw:result-get-proper-c-type-name result))
             (ret-var-name (gw:result-get-c-name result)))
        (list "{\n"
              "    " ret-type-name " " ret-var-name ";\n")))
    
    (gw:type-set-pre-call-result-ccodegen! type standard-pre-handler)
    
    (gw:type-set-post-call-result-ccodegen!
     type
     (lambda (result)
       (let* ((scm-name (gw:result-get-scm-name result))
              (c-name (gw:result-get-c-name result)))
         (list
          (c->scm-converter scm-name c-name)
          "  }\n")))))
  
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-depends-on mod "gw-engine")
  (gw:module-depends-on mod "gw-glib")
  (gw:module-depends-on mod "gw-gnome-utils")

  (gw:module-set-guile-module! mod '(g-wrapped gw-report-gnome))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <window-report.h>\n")))

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:report-window*>
              "gnc_report_window*" "const gnc_report_window*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:report-window
   '<gw:void>
   "reportWindow"
   '((<gw:int> report-id))
   "Show report window")

  (gw:wrap-function
   mod
   'gnc:report-window-reload
   '<gw:void>
   "gnc_report_window_reload"
   '((<gnc:report-window*> wind))
   "Force reload of a report window")

  (gw:wrap-function
   mod
   'gnc:report-window-add-edited-report
   '<gw:void>
   "gnc_report_window_add_edited_report"
   '((<gnc:report-window*> wind) (<gw:scm> report))
   "Add a report to the list of reports with open editors")

  (gw:wrap-function
   mod
   'gnc:print-report
   '<gw:void>
   "gnc_print_report"
   '((<gw:int> report-id))
   "Print a report with dialog support")

  (gw:wrap-function
   mod
   'gnc:report-raise-editor
   '<gw:void>
   "gnc_report_raise_editor"
   '((<gw:scm> report))
   "Raise the report's editor window")
  )
