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
