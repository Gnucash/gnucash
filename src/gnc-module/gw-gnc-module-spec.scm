(define-module (g-wrapped gw-gnc-module-spec))
(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(let ((ws (gw:new-wrapset "gw-gnc-module")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-gnc-module))

  (gw:wrap-as-wct ws '<gnc:module> "GNCModule" "const GNCModule")
                           
  (gw:wrapset-add-cs-declarations! 
   ws 
   (lambda (wrapset client-wrapset) 
     (list "#include \"gnc-module.h\"\n")))
  
  (gw:wrap-function
   ws 'gnc:module-system-refresh
   '<gw:void> "gnc_module_system_refresh"
   '() "Reload the database of modules in the GNC_MODULE_PATH.")
  
  (gw:wrap-function
   ws 'gnc:module-load 
   '<gnc:module> "gnc_module_load"
   '(((<gw:mchars> caller-owned) module-name) (<gw:int> interface))
   "Load and initialize a gnc-module")

  (gw:wrap-function
   ws 'gnc:module-unload
   '<gw:bool> "gnc_module_unload"
   '((<gnc:module> module))
   "Unreference a gnc-module. Module will unload when refcount goes to 0")

  (gw:wrap-function
   ws 'gnc:module-lookup
   '<gw:void*> "gnc_module_lookup"
   '((<gnc:module> module) 
     ((<gw:mchars> caller-owned) symbol))
   "Look up a symbol in the module.  module must be loaded already."))

