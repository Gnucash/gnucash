;;; -*-scheme-*-

;(debug-enable 'backtrace)
;(debug-enable 'debug)
;(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-dialog-tax-table-spec)
  :use-module (g-wrap))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-business-core-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((ws (gw:new-wrapset "gw-dialog-tax-table")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-depends-on ws "gw-business-core")
  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-gnome-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-dialog-tax-table))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <dialog-tax-table.h>\n"
      )))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var) 
     (if client-wrapset
         '()
         (gw:inline-scheme '(use-modules (gnucash dialog-tax-table))))))
  
  ;;
  ;; dialog-tax-table.h
  ;;

  (gw:wrap-function
   ws
   'gnc:tax-table-new
   '<gw:void>
   "gnc_ui_tax_table_window_new"
   '((<gnc:Book*> book))
   "Dialog: Edit the Tax Tables.")
)
