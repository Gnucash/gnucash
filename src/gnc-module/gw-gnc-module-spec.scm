;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(let ((mod (gw:new-module "gw-gnc-module")))
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-set-guile-module! mod '(g-wrapped gw-gnc-module))

  (gw:wrap-non-native-type mod '<gnc:module> "GNCModule" "const GNCModule")
                           
  (gw:module-set-declarations-ccodegen! 
   mod 
   (lambda (unused) 
     (list "#include \"gnc-module.h\"\n")))
  
  (gw:wrap-function
   mod 'gnc:module-system-refresh
   '<gw:void> "gnc_module_system_refresh"
   '() "Reload the database of modules in the GNC_MODULE_PATH.")
  
  (gw:wrap-function
   mod 'gnc:module-load 
   '<gnc:module> "gnc_module_load"
   '((<gw:m-chars-caller-owned> module-name) (<gw:int> interface))
   "Load and initialize a gnc-module")

  (gw:wrap-function
   mod 'gnc:module-unload
   '<gw:bool> "gnc_module_unload"
   '((<gnc:module> module))
   "Unreference a gnc-module. Module will unload when refcount goes to 0")

  (gw:wrap-function
   mod 'gnc:module-lookup
   '<gw:void*> "gnc_module_lookup"
   '((<gnc:module> module) 
     (<gw:m-chars-caller-owned> symbol))
   "Look up a symbol in the module.  module must be loaded already.")
  
  )

