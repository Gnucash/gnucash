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

(define-module (gnucash report report-system collectors))
(use-modules (srfi srfi-1))

(export make-filter)
(export filter-satisfies)
(export filter-id)
(export assert-filter)
(export make-equal-filter)
(export make-predicate-filter)

(export make-collector)
(export collector-accumulate-from)
(export collector-count-from)
(export collector-into-list)
(export collector-per-property)
(export collector-filtered-list)
(export collector-split)
(export make-mapper-collector)
(export make-list-collector)
(export collector-from-slotset)
(export labelled-collector-from-slotset)
(export collector-add)
(export collector-end)
(export assert-collector)
(export collector-add-all)
(export collector-where)
(export collector-reformat)
(export collector-print)
(export collector-do)
(export function-state->collector)
(export make-eq-set-collector)
(export make-extreme-collector)

(export make-slotset)
(export slotset?)
(export slotset-slots)
(export slotset-slot)
(export hashmap->slotset)
(export alist->slotset)
(export slotset-check)
(export slotset-map-input)

(export binary-search-lt)

;; Filters
(define (make-filter id predicate)
  (list 'filter id predicate))

(define (filter? filter)
  (eq? (car filter) 'filter))

(define (assert-filter filter)
  (if (filter? filter) #t
      (throw (list "not a filter" filter))))

(define (filter-satisfies filter object)
  (assert-filter filter)
  (let ((predicate (third filter)))
    (predicate object)))

(define (filter-id filter)
  (assert-filter filter)
  (second filter))

(define (make-predicate-filter id predicate)
  (make-filter id predicate))


(define (make-equal-filter x)
  (make-filter x
	       (lambda (value)
		 (equal? x value))))

;;
;; SlotSet
;;

(define (make-slotset value->slot slots)
  (if (not (procedure? value->slot))
      (throw 'not-a-procedure value->slot))
  (if (not (pair? slots))
      (throw 'not-a-list slots))
  (list 'slotset value->slot slots))

(define (slotset? slotset)
  (eq? (car slotset) 'slotset))

(define (assert-slotset slotset)
  (if (slotset? slotset) #t
      (throw (list "not a slotset" slotset))))

(define (slotset-slots slotset)
  (assert-slotset slotset)
  (third slotset))

(define (slotset-slot slotset value)
  (assert-slotset slotset)
  ((second slotset) value))

(define (slotset-map-input mapfn orig-slotset)
  (let ((orig-slotset-slot (second orig-slotset))
	(orig-slotset-slots (third orig-slotset)))
    (make-slotset (lambda (v) (orig-slotset-slot (mapfn v)))
		  orig-slotset-slots)))

(define (hashmap->slotset hashmap)
  (make-slotset (lambda (v)
		  (hash-ref hashmap v))
		(hashmap->list (lambda (key value) value) hashmap)))

(define (alist->slotset alist)
  (make-slotset (lambda (v) (assoc-ref alist v))
		(hash-map->list (lambda (key value) key)
				(fold (lambda (val h)
					(hash-set! h val val)
					h)
				      (make-hash-table)
				      (map cdr alist)))))

(define (slotset-check slotset)
  (assert-slotset slotset)
  (make-slotset (lambda (value)
		  (let ((result (slotset-slot value)))
		    (if (member result (third slotset))
			(throw (list 'slotset-to-non-value))
			result)))
		(third slotset)))
;;
;; Collectors
;;

(define (make-collector f1 f2)
  (list 'collector f1 f2))

(define (collector-add collector value)
  (assert-collector collector)
  (let ((result ((second collector) value)))
    (assert-collector result)
    result))

(define (collector-end collector)
  (assert-collector collector)
  (let ((fn (third collector)))
    (fn)))

(define (collector-print stream name collector)
  (make-collector (lambda (value) (format stream "(add ~a ~a)\n" name value)
			  (collector-print stream name (collector-add collector value)))
		  (lambda () (let ((result (collector-end collector)))
			       (format stream "(result ~a ~a)\n" name result)
			       result))))


(define (collector? collector)
  (and (list? collector)
       (eq? (car collector) 'collector)))

(define (assert-collector collector)
  (if (collector? collector) #t
      (throw 'error (list "not a collector" collector))))

(define (collector-add-all collector values)
  (if (null-list? values) (collector-end collector)
      (collector-add-all (collector-add collector (car values))
			 (cdr values))))

(define (collector-accumulate-from total)
  (make-collector (lambda (x) (collector-accumulate-from (+ total x)))
		  (lambda () total)))

(define (collector-count-from total)
  (make-collector (lambda (x) (collector-count-from (+ total 1)))
		  (lambda () total)))

(define (collector-into-list)
  (define (collect-into l)
    (make-collector (lambda (x) (collect-into (cons x l)))
		    (lambda () (reverse! l))))
  (collect-into '()))

(define (collector-per-property items make-property-filter make-per-property-collector)
  (let ((collectors (map (lambda (item)
			   (cons (make-property-filter item)
				 (make-per-property-collector item)))
			 items)))
    (collector-filtered-list collectors)))

(define (collector-filtered-list filter-collector-pairs)
  (define (mapfn sublist value)
    (let ((pair (car sublist))
	  (rest (cdr sublist)))
      (if (filter-satisfies (car pair) value)
	  (cons (cons (car pair) (collector-add (cdr pair) value))
		rest)
	  (cons pair (mapfn rest value)))))
  (make-collector
   (lambda (value)
     (collector-filtered-list (mapfn filter-collector-pairs value)))
   (lambda () (map (lambda (pair)
		     (cons (filter-id (car pair))
			   (collector-end (cdr pair))))
		   filter-collector-pairs))))

;; Breaks a sequence of items into a list of collectors by property

(define (collector-split prop-fn make-per-split-collector)
  (let ((list '()))
    (define collector (make-collector (lambda (value)
					(let* ((prop (prop-fn value))
					       (elt (assoc prop list)))
					  (if elt
					      (begin
						(set-cdr! elt (collector-add (cdr elt) value))
						collector)
					      (begin (set! list (cons (cons prop
									    (collector-add (make-per-split-collector prop)
										    value))
							       list))
					      collector))))
				      (lambda ()
					(map (lambda (pair) (cons (car pair)
								  (collector-end (cdr pair))))
					     list))))
    collector))

(define (make-eq-set-collector list)
  (define collector (make-collector
		     (lambda (value)
		       (if (memq value list) collector
			   (make-eq-set-collector (cons value list))))
		     (lambda () list)))
  collector)

(define (make-extreme-collector ordering current)
  (define collector (make-collector (lambda (value)
				      (if (ordering value current)
					  (make-extreme-collector ordering value)
					  collector))
				    (lambda () current)))
  collector)


(define (collector-where pred collector)
  (define new-collector
    (make-collector (lambda (value)
		      (if (pred value)
			  (begin ;(format #t "accept ~a\n" value)
			    (collector-where pred
					     (collector-add collector value)))
			  new-collector))
		    (lambda () (collector-end collector))))
  new-collector)

(define (make-mapper-collector mapfn collector)
  (make-collector (lambda (value)
		    (make-mapper-collector mapfn (collector-add collector (mapfn value))))
		  (lambda () (collector-end collector))))

(define (collector-reformat formatter collector)
  (make-collector (lambda (value)
		    (collector-reformat formatter (collector-add collector value)))
		  (lambda () (formatter (collector-end collector)))))


(define (make-list-collector collectors)
  (make-collector (lambda (value)
		    (make-list-collector (map (lambda (inner-collector)
					   (collector-add inner-collector value))
					 collectors)))
		  (lambda () (map collector-end collectors))))


(define (collector-from-slotset slotset slot-collector)
  (define (make-table)
    (let ((valuemap (make-hash-table)))
      (for-each (lambda (slot)
		  (hash-set! valuemap slot (slot-collector slot)))
		(slotset-slots slotset))
      valuemap))
  (let ((valuemap (make-table)))
    (define collector
      (make-collector (lambda (value)
			(let* ((slot (slotset-slot slotset value)))
			  (hash-set! valuemap slot
				     (collector-add (hash-ref valuemap slot)
						    value)))
			collector)
		      (lambda () (map (lambda (slot)
					(collector-end (hash-ref valuemap slot)))
				      (slotset-slots slotset)))))
    collector))

(define (labelled-collector-from-slotset slotset slot-collector)
  (collector-from-slotset slotset
			  (lambda (slot)
			    (collector-reformat (lambda (result)
						  (cons slot result))
						(slot-collector slot)))))


(define (function-state->collector fn state)
  (make-collector (lambda (value)
		    (let ((next (fn value state)))
		      (function-state->collector fn next)))
		  (lambda ()
		    state)))

(define (collector-do collector . other-collectors)
  (collector-reformat (lambda (final)
			(car final))
		      (make-list-collector (cons collector other-collectors))))


;; Binary search. Returns highest index with content less than or
;; equal to the supplied value.

(define (binary-search-lt <= value vector)
  (define (search low high)
    (let* ((midpoint (+ low (ceiling (/ (- high low) 2))))
	   (midvalue (vector-ref vector midpoint)))
      (if (= low high)
	  (if (<= midvalue value)
	      low #f)
	  (if (<= midvalue value)
	      (search midpoint high)
	      (search low (- midpoint 1))))))
  (if (= 0 (vector-length vector)) #f
      (search 0 (- (vector-length vector) 1))))
