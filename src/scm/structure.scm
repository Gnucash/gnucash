;;; $Id$
;;; Some functions to help build structures

;;; define-mystruct is used to build an association list that defines
;;; the layout of a structure...
(define (define-mystruct lst)
  (define alist '())  ;; Association list
  (define count 0)    ;; Number of entries
  (define (add-item item)
    (set! alist (cons (cons item count) alist))
    (set! count (+ 1 count)))
  (add-item 'gensymid)
  (for-each add-item lst)
  alist)
;;; Use as follows:
;;; (define qif-split-structure  (define-mystruct '(category memo
;;; amount percent)))  
;;;

(define (build-mystruct-instance structinfo)
  ;;;  struct-instance is the vector for the data...
  (define struct-instance (make-vector (length structinfo) #f))
  (define (get-item field-id)   ;;; Look up entry based on ID
    (let ((assocv (assoc field-id structinfo)))
      (if assocv
	  (vector-ref struct-instance (cdr assocv))
	  (begin
	    (display (string-append "No such field as "
				    (symbol->string field-id)
				    " in "))
	    (display structinfo)
	    (newline)
	    #f))))


  (define (set-item! field-id value)   ;;; Plunk in new value
    (let ((assocv (assoc field-id structinfo)))
      (if assocv
	  (vector-set! struct-instance (cdr assocv) value)
	  #f)))

  (define (actions action field . value) ;;; now, methods to be applied
    (cond
     ((eq? action 'get)
      (let ((item (get-item field)))
	(if item
	    (car item)
	    #f)))
     ((eq? action 'put)
      (set-item! field value))
     (else
      (list structinfo struct-instance))))
  (set-item! 'gensymid (list (gensym)))  ;;; Attach a unique identifier
  actions)

(if testing?
    (begin
      (display "Testing structur.scm - define-mystruct, build-mystruct-instance")
      (newline)
      (let* ((ms (define-mystruct '(f1 f2 f3)))
	     (mi (build-mystruct-instance ms)))
	(mi 'put 'f1 122)
	(mi 'put 'f3 "hello")
	(display "Empty list entry:") (display (mi 'get 'f2)) (newline)
	(display "and two that aren't (f1 f3):")
	(display (list (mi 'get 'f1) (mi 'get 'f3))) (newline)
	(display "Whole thang:")
	(display (mi 'whole 'thang)) (newline)
	(display "Overlay 'f3 with 42, add to 'f1 value")
	(mi 'put 'f3 42)
	(display (number->string (+ (mi 'get 'f1) (mi 'get 'f3)))) (newline))))


