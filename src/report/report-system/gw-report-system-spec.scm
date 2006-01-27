(define-module (g-wrapped gw-report-system-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(use-modules (g-wrapped gw-engine-spec))

(let ((ws (gw:new-wrapset "gw-report-system")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-report-system))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <gnc-report.h>\n"
      )))

  (gw:wrap-function
   ws
   'gnc:find-report
   '<gw:scm>
   "gnc_report_find"
   '((<gw:int> id))
   "Find report by id")

  (gw:wrap-function
   ws
   'gnc:report-add
   '<gw:void>
   "gnc_report_add"
   '((<gw:int> id) (<gw:scm> report))
   "Add report with id")

  )
