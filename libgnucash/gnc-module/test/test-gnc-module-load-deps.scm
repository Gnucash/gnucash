(use-modules (gnucash unittest-support))
(define log-domain "gnc.module")
(define check (new-TestErrorStruct))
(define log-level (G-LOG-LEVEL-WARNING))
(define msg "Module '../../../libgnucash/gnc-module/test/misc-mods/.libs/libgncmod_futuremodsys.so' requires newer module system\n")
(TestErrorStruct-log-domain-set check log-domain)
(TestErrorStruct-log-level-set check log-level)
(TestErrorStruct-msg-set check msg)
(define handler (test-set-checked-handler log-domain log-level check))
(use-modules (gnucash gnc-module))

(gnc:module-system-init)
(gnc:module-load "gnucash/baz" 0)

(baz-hello)
(foo-hello)
(baz:scheme-hello)
(foo:scheme-hello)

(g-log-remove-handler log-domain handler)

(exit 0)
