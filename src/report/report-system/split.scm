(define-module (gnucash report report-system split))
(use-modules (gnucash gnc-module))
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (gnc:module-load "gnucash/engine" 0)))
  (else
    (gnc:module-load "gnucash/engine" 0)))
(use-modules (sw_engine))

(export split-same?)
(export split-in-list?)

(define (split-same? s1 s2) 
  (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

(define split-in-list? 
  (lambda (split splits)
    (cond 
     ((null? splits) #f)
     ((split-same? (car splits) split) #t)
     (else (split-in-list? split (cdr splits))))))

