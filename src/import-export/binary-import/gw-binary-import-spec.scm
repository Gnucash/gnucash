
(define-module (g-wrapped gw-app-utils-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "gw-binary-import")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-binary-import))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset) 
     "#include <druid-commodity.h>\n"))

  (gw:wrap-function
   ws
   'gnc:import-legacy-commodities
   '<gw:void>
   "gnc_import_legacy_commodities"
   '(((<gw:mchars> caller-owned const) filename))
   "Launch the legacy-commodity import druid")
)
