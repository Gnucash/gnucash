;; test of a Scheme module called gnc-mod-bar, which should get 
;; loaded by the Gnucash module "bar"

(define-module (gnucash bar))

(export bar:scheme-hello)

(define (bar:scheme-hello)
  #t)



