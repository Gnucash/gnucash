;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  simple-obj.scm
;;;  rudimentary "class" system for straight Scheme 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/simple-obj.scm")

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
  (let ((slots (make-vector 3))
        (slot-hash (make-hash-table 11))
        (slot-counter 0))
    (vector-set! slots 0 class-symbol)
    (vector-set! slots 1 slot-hash)
    (vector-set! slots 2 slot-names)
    (for-each 
     (lambda (elt)                        
       (hash-set! slot-hash elt slot-counter)
       (set! slot-counter (+ 1 slot-counter)))
     slot-names)
    (cons 'simple-class slots)))

(define (simple-class? self)
  (and (pair? self) (eq? (car self) 'simple-class)))

(define (simple-obj-getter obj class slot)  
  (let ((slot-num (hash-ref (vector-ref (cdr class) 1) slot)))
    (vector-ref (cdr obj) slot-num)))

;;   (if (and (pair? obj) 
;;            (simple-class? class))
;;       (if (eq? (vector-ref (cdr class) 0) (car obj))
;;           (let ((slot-num-pair (assq  slot (vector-ref (cdr class) 1))))
;;             (if slot-num-pair                 
;;                 (if (vector? (cdr obj))
;;                     (vector-ref (cdr obj) (cdr slot-num-pair))
;;                     (error "simple-obj-getter: data field not a vector??"))
;;                 (error "simple-obj-getter: no slot " slot " in class " 
;;                        class)))
;;           (error "simple-obj-getter: object " obj " is not of class " 
;;                  class))
;;       (error "simple-obj-getter: bad object/class " obj class)))

(define (simple-obj-setter obj class slot value)
  (let ((slot-num (hash-ref (vector-ref (cdr class) 1) slot)))
    (vector-set! (cdr obj) slot-num value)))

;;   (if (and (pair? obj)
;;            (simple-class? class))
;;       (if (eq? (vector-ref (cdr class) 0) (car obj))
;;           (let ((slot-num-pair (assq slot (vector-ref (cdr class) 1))))
;;             (if slot-num-pair
;;                 (if (vector? (cdr obj))
;;                     (vector-set! (cdr obj) (cdr slot-num-pair) value)
;;                     (error "simple-obj-setter: data field not a vector??"))
;;                 (error "simple-obj-setter: no slot " slot " in class "
;;                        class)))
;;           (error "simple-obj-setter: object " obj " is not of class "
;;                  class))
;;       (error "simple-obj-setter: bad object/class " obj class)))


(define (simple-obj-print obj class)
  (display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;") (newline)
  (for-each 
   (lambda (slot)
    (display "   ")
    (display slot)
    (display " : ")
    (write (simple-obj-getter obj class slot))
    (newline))
   (vector-ref (cdr class) 2)))

(define (simple-obj-type obj)
  (if (pair? obj)
      (car obj)
      #f))

(define (make-simple-obj class)
  (if (simple-class? class)
      (cons (vector-ref (cdr class) 0)
            (make-vector (length (vector-ref (cdr class) 2)) #f))))

