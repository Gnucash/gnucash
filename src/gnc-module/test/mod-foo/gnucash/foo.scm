;; test of a Scheme module called gnc-mod-foo, which should get 
;; loaded by the Gnucash module "foo"

(define-module (gnucash foo))

(export foo:scheme-hello)

(define (foo:scheme-hello)
  #t)



