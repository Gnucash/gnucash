(use-modules (gnucash gnc-module))

(gnc:module-system-init)
(gnc:module-load "gnucash/foo" 0)
(foo-hello)
(foo:scheme-hello)

(gnc:module-load "gnucash/bar" 0)
(foo-hello)
(foo:scheme-hello)
(bar-hello)
(bar:scheme-hello)

(exit 0)
