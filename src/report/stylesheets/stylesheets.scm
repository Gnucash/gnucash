;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  stylesheets.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report stylesheets))

(use-modules (gnucash bootstrap) (g-wrapped gw-gnc)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash report stylesheet-plain))
(use-modules (gnucash report stylesheet-fancy))
