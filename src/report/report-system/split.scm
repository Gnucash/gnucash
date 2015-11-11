(define-module (gnucash report report-system split))
(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/engine" 0))

(use-modules (sw_engine))
(use-modules (srfi srfi-1))

(export split-same?)
(export split-in-list?)
(export split-hashtable-ref)
(export split-hashtable-set!)

(define (split-same? s1 s2) 
  (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

(define split-in-list? 
  (lambda (split splits)
    (cond 
     ((null? splits) #f)
     ((split-same? (car splits) split) #t)
     (else (split-in-list? split (cdr splits))))))

;; Split hashtable. Because we do gncSplitGetGUID so often, it
;; turns out to be a bit quicker to store a (hash, split) pair
;; instead of just the split.
(define (split-assoc split alist)
  (find (lambda (pair) (split-same? (cdr split) (cdr (car pair)))) alist))
(define (split-hash split size)
  (remainder (car split) size))

(define (split-hashtable-ref table split)
  (hashx-ref split-hash split-assoc table
	     (cons (string-hash (gncSplitGetGUID split)) split)))

(define (split-hashtable-set! table split value)
  (hashx-set! split-hash split-assoc table
	      (cons (string-hash (gncSplitGetGUID split)) split) value))

