(define-module (g-wrapped gw-kvp-spec))

;; g-wrap modules
(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

;; g-wrap wrapped modules
(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

;; other wrapped modules
(use-modules (g-wrapped gw-engine-spec))

(let ((ws (gw:new-wrapset "gw-kvp")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-depends-on ws "gw-engine")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-kvp))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <kvp_frame.h>\n"
      "#include <kvp-scm.h>\n"
      "#include <Transaction.h>\n")))

  (gw:wrap-as-wct ws '<gnc:kvp-frame*> "kvp_frame*" "const kvp_frame*")

  (gw:wrap-simple-type
   ws
   '<gnc:kvp-value*> "kvp_value*"
   '("gnc_kvp_value_ptr_p(" scm-var ")")
   '(c-var " = gnc_scm_to_kvp_value_ptr(" scm-var ");\n")
   '(scm-var " = gnc_kvp_value_ptr_to_scm(" c-var ");\n"))
  
  (gw:wrap-function
   ws
   'gnc:transaction-get-slots
   '<gnc:kvp-frame*>
   "xaccTransGetSlots"
   '((<gnc:Transaction*> s))
   "Get the transaction's slots.")

  (gw:wrap-function
   ws
   'gnc:kvp-frame-set-slot
   '<gw:void>
   "kvp_frame_set_slot"
   '((<gnc:kvp-frame*> k)
     ((<gw:mchars> caller-owned const) c)
     (<gnc:kvp-value*> v))
   "Sets the slot c in frame k to the value v")

  (gw:wrap-function
   ws
   'gnc:kvp-frame-get-slot
   '<gnc:kvp-value*>
   "kvp_frame_get_slot"
   '((<gnc:kvp-frame*> k) ((<gw:mchars> caller-owned const) c))
   "Gets the slot c from frame k"))
