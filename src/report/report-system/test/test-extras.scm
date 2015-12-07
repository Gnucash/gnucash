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

(define-module (gnucash report report-system test test-extras))

(use-modules (gnucash gnc-module))
(use-modules (gnucash engine test test-extras))

(export pattern-streamer)

(export create-option-set)
(export option-set-setter)
(export option-set-getter)

(export tbl-column-count)
(export tbl-row-count)
(export tbl-ref)
(export tbl-ref->number)

;;
;; Random report test related syntax and the like
;;

;;
;; Table parsing
;;
(use-modules (ice-9 regex))
(use-modules (ice-9 streams))

(define (values-for-keywords pos regex-list text)
  (make-stream (lambda (pos-keywords-pair)
		 (let ((current-pos (car pos-keywords-pair))
			(regex-list (cdr pos-keywords-pair)))
		   (if (null? regex-list)
		       '()
		       (let ((match (string-match (caar regex-list) text current-pos)))
			 (if (not match)
			     '()
			     (let ((new-state (cons (match:end match)
						    (cdr regex-list)))
				   (next-value (cons (match:end match)
						     (map (lambda (item)
							    (match:substring match item))
							  (cdar regex-list)))))
			       (cons next-value new-state)))))))
	       (cons pos regex-list)))

(define (pattern-streamer start-text regex-list text)
  (define (stream-next index)
    ;;(format #t "Next.  Index: ~a\n" index)
    (let ((head-index (string-contains text start-text index)))
      ;; (format #t "head index ~a ~a --> ~a\n" start-text index head-index)
      (if (not head-index) '()
	  (let ((values (stream->list (values-for-keywords head-index regex-list text))))
	    (if (null? values) '()
		(let ((new-state (car (car (last-pair values))))
		      (next-value (map cdr values)))
		  (cons next-value new-state)))))))
  ;;(format #t "Stream ~a\n" text)
  (make-stream stream-next 0))

;; silly table functions
(define (tbl-column-count tbl)
  (length (car tbl)))

(define (tbl-row-count tbl)
  (length tbl))

(define (tbl-ref tbl row-index column-index)
  (list-ref (list-ref tbl row-index) column-index))

(define (tbl-ref->number tbl row-index column-index)
  (string->number (car (tbl-ref tbl row-index column-index))))

;;
;; Test sinks
;;

(define (make-test-sink) (list 'sink 0 '()))

(define (test-sink-count sink)
  (second sink))

(define (test-sink-count! sink value)
  (set-car! (cdr sink) value))

(define (test-sink-messages sink)
  (third sink))

(define (test-sink-messages! sink messages)
  (set-car! (cdr (cdr sink)) messages))

(define (test-sink-check sink message flag)
  (test-sink-count! sink (+ (test-sink-count sink) 1))
  (if flag #t
      (test-sink-messages! sink (cons message (test-sink-messages sink)))))

(define (test-sink-report sink)
  (format #t "Completed ~a tests ~a\n"
	  (test-sink-count sink)
	  (if (null? (test-sink-messages sink)) "PASS" "FAIL"))
  (if (null? (test-sink-messages sink)) #t
      (begin (for-each (lambda (delayed-message)
			 (delayed-format-render #t delayed-message))
		       (test-sink-messages sink))
	     #f)))

(define (delayed-format . x) x)

(define (delayed-format-render stream msg)
  (apply format stream msg))

;;
;; options
;;


(define (create-option-set)
  (make-hash-table) )

(define (option-set-setter option-set)
  (lambda (category name value)
    (hash-set! option-set (list category name) value)))

(define (option-set-getter option-set)
  (lambda (category name)
    (hash-ref option-set (list category name))))

;;
;;
;;

(define (report-show-options stream expense-options)
  (gnc:options-for-each (lambda (option)
			  (format stream "Option: ~a.~a Value ~a\n"
				  (gnc:option-section option)
				  (gnc:option-name option)
				  (gnc:option-value option)))
			expense-options))

