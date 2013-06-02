(define-module (gnucash report report-system list-extras))
(use-modules (srfi srfi-1))

(export list-min-max)
(export list-leaves)
(export function-compose)

(define (list-min-max list ordered?)
  (define (helper list min max)
    (if (null? list) (cons min max)
	(let ((elt (car list)))
	  (helper (cdr list)
		(if (ordered? elt min) elt min)
		(if (ordered? elt max) max elt)))))
  (helper (cdr list) (car list) (car list)))

(define (list-leaves list)
  (if (not (pair? list))
      (cons list '())
      (fold (lambda (next acc)
	      (append (list-leaves next)
		      acc))
	    '()
	    list)))

(define (function-compose f1 f2)
  (lambda a
    (f1 (apply f2 a))))
