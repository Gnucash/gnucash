(use-modules (gnucash gnc-module))

(gnc:module-system-init)
(gnc:module-load "gnucash/foo" 0)
(exit (eq? 10 (foo-hello)))
