;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  simple-obj.scm
;;;  rudimentary "class" system for straight Scheme 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
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
  (issue-deprecation-warning "make-simple-class is deprecated. use make-record-type.")
  (make-record-type (symbol->string class-symbol) slot-names))

(define (simple-obj-getter class slot)  
  (issue-deprecation-warning "simple-obj-getter is deprecated. use record-accessor.")
  (record-accessor class slot))

(define (simple-obj-setter class slot)
  (issue-deprecation-warning "simple-obj-setter is deprecated. use record-modifier.")
  (record-modifier class slot))

(define (simple-obj-print obj)
  (issue-deprecation-warning "simple-obj-print is deprecated. use write.")
  (write obj))

(define (simple-obj-to-list obj)
  (issue-deprecation-warning "simple-obj-to-list is deprecated. use record-type->list in qif-guess-map.scm")
  (let ((retval '()))
    (for-each 
     (lambda (slot)
       (let ((thunk (record-accessor (record-type-descriptor obj) slot)))
         (set! retval (cons (thunk obj) retval))))
     (record-type-fields (record-type-descriptor obj)))
    (reverse retval)))

(define (simple-obj-from-list list type)
  (issue-deprecation-warning "simple-obj-from-list-obj is deprecated. use list->record-type in qif-guess-map.scm")
  (let ((retval (make-simple-obj type)))
    (for-each 
     (lambda (slot)
       (let ((thunk (record-modifier type slot)))
         (thunk retval (car list)))
       (set! list (cdr list)))
     (record-type-fields type))
    retval))


(define (make-simple-obj class)
  (issue-deprecation-warning "make-simple-obj is deprecated. use construct in qif-objects.scm")
  (let ((ctor (record-constructor class))
        (field-defaults 
         (map (lambda (v) #f) (record-type-fields class))))
    (apply ctor field-defaults)))
                 
