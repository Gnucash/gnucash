;; test-scm-multi.scm : load multiple gnucash modules from Scheme 

(gnc:module-load "gnucash/foo" 0)
(foo:hello)
(foo:scheme-hello)

(gnc:module-load "gnucash/bar" 0)
(foo:hello)
(foo:scheme-hello)
(bar:hello)
(bar:scheme-hello)

