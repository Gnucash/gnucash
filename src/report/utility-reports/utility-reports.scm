;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report utility-reports))

(use-modules (gnucash main) (g-wrapped gw-gnc)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash report hello-world))
(use-modules (gnucash report iframe-url))
(use-modules (gnucash report view-column))
(use-modules (gnucash report welcome-to-gnucash))

(export gnc:make-welcome-report)
