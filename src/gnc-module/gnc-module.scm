;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gnc-module.scm
;;;  Guile module which allows initialization of the gnucash module
;;;  system from Scheme 
;;;
;;;  Copyright 2001 Linux Developers Group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash gnc-module))
(use-modules (g-wrapped gw-gnc-module))

(export gnc:module-system-init)

(if (not (defined? 're-export))
    (begin
      (defmacro re-export names
	`(eval export names))
      (export re-export)))

;; symbols from gw-gnc-module
(re-export gnc:module-system-refresh)
(re-export gnc:module-load)
(re-export gnc:module-load-optional)
(re-export gnc:module-unload)
(re-export gnc:module-lookup)

(define (gnc:module-system-init)
  (let ((lib (if (or (string=? (version) "1.3")
                     (string=? (version) "1.3.4"))
                 (dynamic-link "libgncmodule.so")
                 (dynamic-link "libgncmodule"))))
    (if lib
        (dynamic-call "gnc_module_system_init" lib)
        (throw 'gnc:module-system-init-failed))))
