;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  core-utils.scm
;;;  Guile module for core-utils
;;;
;;;  Copyright 2006 Chris Shoemaker <c.shoemaker@cox.net>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash core-utils))
(load-extension "libcore-utils" "scm_init_sw_core_utils_module")
(use-modules (sw_core_utils))

(re-export gnc-is-debugging)
(re-export g-find-program-in-path)
(re-export gnc-utf8-strip-invalid)
