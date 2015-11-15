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

(debug-set! stack 50000)
(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (srfi srfi-1))

(use-modules (gnucash report report-system collectors))
(use-modules (gnucash engine test test-extras))

(define (run-test)
  (and (test test-empty)
       (test test-one)
       (test test-two)
       (test test-make-eq-set)
       (test test-make-extreme-collector)
       (test test-collector-split)
       (test test-make-mapper-collector)
       (test test-make-list-collector)
       (test test-slotset)
       (test test-collector-from-slotset)
       (test test-binary-search-lt)
       (test test-collector-into-list)
       (test test-function-state->collector)
       (test test-collector-do)
       #t))


(define (test-slotset)
  (let* ((values '(2 4 6))
	 (slotset (make-slotset (lambda (x) (* 2 x)) values)))
    (and (equal? values (slotset-slots slotset))
		 (equal? 2 (slotset-slot slotset 1)))))

(define (test-empty)
  (let ((c (empty-collector)))
    (let ((empty (collector-end c)))
      (and (equal? 4 (length empty))
	   (equal? 0 (collector-add-all (collector-accumulate-from 0)
				       (map cdr empty)))))))

(define (test-one)
  (define c (empty-collector))
  (set! c (collector-add c 1))
  (and (equal? 1 (collector-add-all (collector-accumulate-from 0)

		    (map cdr (collector-end c))))
       (equal? 4 (length (collector-end c)))))

(define (test-two)
  (define c (empty-collector))
  (set! c (collector-add c 2))
  (and (equal? 2 (collector-add-all (collector-accumulate-from 0)
				    (map cdr (collector-end c))))
       (equal? 4 (length (collector-end c)))))

(define (empty-collector)
  (define (equal-predicate a)
    (lambda (x)
      (equal? a x)))
  (collector-per-property '(1 2 3 4)
			  make-equal-filter
			  (lambda (value) (collector-accumulate-from 0))))

(define (test-make-eq-set)
  (let ((c (make-eq-set-collector '())))
    (and (null-list? (collector-end c))
	 (let ((c1 (collector-add c 1)))
	   (equal? '(1) (collector-end c1)))
	 (equal? '(1) (collector-add-all c '(1 1 1)))
	 (let ((result (collector-add-all c '(1 2))))
	   (and (member 1 result)
		(member 2 result)
		(= (length result) 2))))))

(define (test-make-extreme-collector)
  (let ((c (make-extreme-collector > 0)))
    (and (equal? 0 (collector-end c))
	 (equal? 0 (collector-add-all c '(-1)))
	 (equal? 1 (collector-add-all c '(1)))
	 (equal? 5 (collector-add-all c '(5)))
	 (equal? 5 (collector-add-all c '(1 5)))
	 (equal? 5 (collector-add-all c '(5 1)))
	 #t)))

(define (test-collector-split)
  (let* ((c (collector-split (lambda (x) x)
				     (lambda (x) (collector-count-from 0))))
	 (all (collector-add-all c '(1 2 3 4 5 1 2))))
    (and (equal? 5 (length all))
	 #t)))

(define (test-make-mapper-collector)
  (let ((double-and-add (make-mapper-collector (lambda (x) (* x 2))
					       (collector-accumulate-from 0))))
    (and (equal? 0 (collector-end double-and-add))
	 (equal? 2 (collector-add-all double-and-add '(1)))
	 #t)))

(define (test-make-list-collector)
  (let ((c1 (collector-accumulate-from 0))
	(c2 (collector-count-from 0)))
    (and (equal? '(10 4) (collector-add-all (make-list-collector (list c1 c2)) '(1 2 3 4))))))


(define (test-collector-into-list)
  (define (check l)
    (equal? l (collector-add-all (collector-into-list) l)))
  (and (check '())
       (check '(1))
       (check '(1 2))
       (check '(1 2 3))
       (check '(1 2 3 4))))

(define (test-collector-from-slotset)
  ;;(define (add-trace name collector)
  ;;   (collector-print #t name collector))

  (define (make-slotset-counter values)
    (let ((slotset (make-slotset (lambda (x) x) values)))
      (labelled-collector-from-slotset slotset
			      (lambda (n)
				(collector-count-from 0)))))
  (and (let ((values '(1 2)))
		 (equal? '((1 . 0) (2 . 0))
			 (collector-add-all (make-slotset-counter values)
					    '())))
       (let ((values '(1 2)))
	 (equal? '((1 . 1) (2 . 1))
		 (collector-add-all (make-slotset-counter values)
				    '(1 2))))
       (let ((values '(1 2)))
	 (equal? '((1 . 3) (2 . 2))
		 (collector-add-all (make-slotset-counter values)
				    '(1 2 1 2 1))))))


(use-modules (ice-9 streams))

(define (stream-range from to)
  (make-stream (lambda (current)
		 (if (> current to) '()
		     (cons current (+ current 1))))
	       from))

(define (slow-search <= value vector)
  (define (search n)
    (if (= n (vector-length vector)) (- n 1)
	(if (<= (vector-ref vector n) value)
	    (search (+ n 1))
	    (if (= n 0) #f (- n 1)))))
  (if (= 0 (vector-length vector)) #f
      (search 0)))

(define (test-binary-search-lt)
  (define (search value vector)
    (let ((binary-value (binary-search-lt <= value vector))
	  (slow-value (slow-search <= value vector))
	  (length (vector-length vector)))
      (if (equal? binary-value slow-value) binary-value
	  (begin (format #t "Mismatch ~a ~a, expected ~a, found ~a\n" value vector slow-value binary-value)
		 (throw 'mismatch)))
      binary-value))
  (and (and (equal? #f (search 1 #()))
	    (equal? #f (search 0 #(1)))
	    (equal? 0 (search 1 #(1)))
	    (equal? 0 (search 2 #(1)))
	    (equal? #f (search 0 #(1 3)))
	    (equal? 0 (search 1 #(1 3)))
	    (equal? 0 (search 2 #(1 3)))
	    (equal? 1 (search 3 #(1 3)))
	    (equal? 1 (search 4 #(1 3))))
       (let* ((values (stream-range 0 20))
	      (vectors (stream-map (lambda (n)
				     (let ((vector (make-vector n)))
				       (stream-for-each (lambda (index)
							  (vector-set! vector index (+ (* index 2) 1)))
							(stream-range 0 (- n 1)))
				       vector))
				   values))
	      (tested-vectors (stream-map (lambda (vector)
					    (stream-for-each
					     (lambda (value)
					       (search value vector))
					     (stream-range 0 (+ (* (vector-length vector) 2) 1))))
					  vectors)))
	 (stream-for-each (lambda (x) x) tested-vectors))))

(define (test-function-state->collector)
  (define (count v current-count) (+ current-count 1))
  (define (check-count l)
    (= (length l) (collector-add-all (function-state->collector count 0) l)))
  (check-count '())
  (check-count '(1))
  (check-count '(1 2 3)))

(define (test-collector-do)
  (let ((count 0))
    (let ((add-to-list-and-count (collector-do (collector-into-list)
					       (function-state->collector (lambda (v n)
									    (set! count (+ n 1))
									    (+ n 1))
									  0))))
      (let* ((orig '(one two three))
	     (collected (collector-add-all add-to-list-and-count orig)))
	(format #t "~a ~a ~a\n" count collected orig)
	(and (equal? orig collected)
	     (= count (length orig)))))))
