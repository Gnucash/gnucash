;;; -*-scheme-*-

;(debug-enable 'backtrace)
;(debug-enable 'debug)
;(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-gnome-search-spec)
  :use-module (g-wrap))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((ws (gw:new-wrapset "gw-gnome-search")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-gnome-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-gnome-search))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <dialog-search.h>\n"
      )))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var) 
     (if client-wrapset
         '()
         (gw:inline-scheme '(use-modules (gnucash gnome-search))))))

  (gw:wrap-function
   ws
   'gnc:search-dialog-test
   '<gw:void>
   "gnc_search_dialog_test"
   '()
   "Dialog: create a test Search Dialog.")

)
