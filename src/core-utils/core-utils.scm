;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  core-utils.scm
;;;  Guile module for core-utils
;;;
;;;  Copyright 2006 Chris Shoemaker <c.shoemaker@cox.net>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash core-utils))
(load-extension "libcore-utils" "scm_init_sw_core_utils_module")
(use-modules (sw_core_utils))

(export gnc-is-debugging)
(export g-find-program-in-path)
(export gnc-utf8-strip-invalid)
