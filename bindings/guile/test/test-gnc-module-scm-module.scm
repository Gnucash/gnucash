(use-modules (gnucash gnc-module))

(gnc:module-system-init)
(if (not (gnc:module-load "gnucash/foo" 0)) (exit -1))

(exit (foo:scheme-hello))
