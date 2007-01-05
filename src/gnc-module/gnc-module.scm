;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gnc-module.scm
;;;  Guile module which allows initialization of the gnucash module
;;;  system from Scheme 
;;;
;;;  Copyright 2001 Linux Developers Group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash gnc-module))
(load-extension "libgncmodule" "scm_init_sw_gnc_module_module")
(use-modules (sw_gnc_module))

(define gnc:module-system-init gnc-module-system-init)
(define gnc:module-system-refresh gnc-module-system-refresh)
(define gnc:module-load gnc-module-load)
(define gnc:module-load-optional gnc-module-load-optional)
(define gnc:module-unload gnc-module-unload)

(export gnc:module-system-init)
(export gnc:module-system-refresh)
(export gnc:module-load)
(export gnc:module-load-optional)
(export gnc:module-unload)
