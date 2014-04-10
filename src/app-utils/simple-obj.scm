;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  simple-obj.scm
;;;  rudimentary "class" system for straight Scheme 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  this is an extremely rudimentary object system.  Each object is a
;;  cons cell, where the car is a symbol with the class name and the
;;  cdr is a vector of the slots.  
;;
;; the "class object" is an instance of simple-class which just has
;; the name of the class and an alist of slot names to vector indices
;; as its slots.
;;
;; by convention, I name class objects (defined with make-simple-class)
;; <class-name> with class-smybol 'class-name.  For example,
;;
;; (define <test-class> (make-simple-class 'test-class '(slot-1 slot-2)))
;; (define t (make-simple-obj <test-class>))
;; t ==> (test-class . #(#f #f))

;; the 'simple-class' class.  
(define (make-simple-class class-symbol slot-names) 
  (make-record-type (symbol->string class-symbol) slot-names))

(define (simple-obj-getter class slot)  
  (record-accessor class slot))

(define (simple-obj-setter class slot)
  (record-modifier class slot))

(define (simple-obj-print obj)
  (write obj))

(define (simple-obj-to-list obj)
  (let ((retval '()))
    (for-each 
     (lambda (slot)
       (let ((thunk (record-accessor (record-type-descriptor obj) slot)))
         (set! retval (cons (thunk obj) retval))))
     (record-type-fields (record-type-descriptor obj)))
    (reverse retval)))

(define (simple-obj-from-list list type)
  (let ((retval (make-simple-obj type)))
    (for-each 
     (lambda (slot)
       (let ((thunk (record-modifier type slot)))
         (thunk retval (car list)))
       (set! list (cdr list)))
     (record-type-fields type))
    retval))


(define (make-simple-obj class)
  (let ((ctor (record-constructor class))
        (field-defaults 
         (map (lambda (v) #f) (record-type-fields class))))
    (apply ctor field-defaults)))
                 
