;; test of a Scheme module called gnc-mod-baz, which should get 
;; loaded by the Gnucash module "baz"

(define-module (gnucash baz))

(export baz:scheme-hello)

(define (baz:scheme-hello)
  #t)



