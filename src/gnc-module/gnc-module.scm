;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gnc-module.scm
;;;  Guile module which allows initialization of the gnucash module
;;;  system from Scheme 
;;;
;;;  Copyright 2001 Linux Developers Group
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash gnc-module))
(use-modules (g-wrapped gw-gnc-module))

(export gnc:module-system-init)

;; symbols from gw-gnc-module
(export gnc:module-system-refresh)
(export gnc:module-load)
(export gnc:module-load-optional)
(export gnc:module-unload)
(export gnc:module-lookup)

(define (gnc:module-system-init)
  (let ((lib (if (or (string=? (version) "1.3")
                     (string=? (version) "1.3.4"))
                 (dynamic-link "libgncmodule.so")
                 (dynamic-link "libgncmodule"))))
    (if lib
        (dynamic-call "gnc_module_system_init" lib)
        (throw 'gnc:module-system-init-failed))))
