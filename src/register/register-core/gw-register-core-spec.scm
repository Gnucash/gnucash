(define-module (g-wrapped gw-register-core-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(let ((ws (gw:new-wrapset "gw-register-core")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-register-core))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset) 
     (if (eq? wrapset client-wrapset)
         '()
         "#include <basiccell.h>\n")))
  
  (gw:wrap-as-wct ws '<gnc:basic-cell> "BasicCell*" "const BasicCell*")

  (gw:wrap-function
   ws
   'gnc:basic-cell-get-value
   '(<gw:mchars> callee-owned const)
   "gnc_basic_cell_get_value"
   '((<gnc:basic-cell> cell))
   "Return the value of the cell."))
