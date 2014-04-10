(define-module (g-wrapped gw-report-gnome-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((ws (gw:new-wrapset "gw-report-gnome")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-gnome-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-report-gnome))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <dialog-column-view.h>\n"
      "#include <dialog-style-sheet.h>\n"
      "#include <window-report.h>\n")))

  (gw:wrap-as-wct ws
                  '<gnc:report-window*>
                  "gnc_report_window*" "const gnc_report_window*")

  (gw:wrap-function
   ws
   'gnc:report-window
   '<gw:void>
   "reportWindow"
   '((<gw:int> report-id))
   "Show report window")

  (gw:wrap-function
   ws
   'gnc:report-window-reload
   '<gw:void>
   "gnc_report_window_reload"
   '((<gnc:report-window*> wind))
   "Force reload of a report window")

  (gw:wrap-function
   ws
   'gnc:report-window-add-edited-report
   '<gw:void>
   "gnc_report_window_add_edited_report"
   '((<gnc:report-window*> wind) (<gw:scm> report))
   "Add a report to the list of reports with open editors")

  (gw:wrap-function
   ws
   'gnc:print-report
   '<gw:void>
   "gnc_print_report"
   '((<gw:int> report-id))
   "Print a report with dialog support")

  (gw:wrap-function
   ws
   'gnc:report-raise-editor
   '<gw:void>
   "gnc_report_raise_editor"
   '((<gw:scm> report))
   "Raise the report's editor window")

  (gw:wrap-function
   ws
   'gnc:main-window-open-report
   '<gw:void>
   "gnc_main_window_open_report"
   '((<gw:int> report-id) (<gw:bool> top-level))
   "Show report window")

  (gw:wrap-function
   ws
   'gnc:default-options-editor
   '<gnc:UIWidget>
   "gnc_report_window_default_params_editor"
   '((<gw:scm> options) (<gw:scm> report))
   "Default options editor window for reports")

  (gw:wrap-function
   ws
   'gnc:style-sheet-dialog-open
   '<gw:void>
   "gnc_style_sheet_dialog_open"
   '()
   "Show the style sheet editor window.")

  (gw:wrap-function
   ws
   'gnc:column-view-edit-options
   '<gnc:UIWidget>
   "gnc_column_view_edit_options"
   '((<gw:scm> options) (<gw:scm> view))
   "Create an editor for a column-view options object")
  )
