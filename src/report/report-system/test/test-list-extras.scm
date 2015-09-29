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
(use-modules (gnucash report report-system list-extras))
(use-modules (gnucash report report-system test test-extras))

(define (run-test)
    (test test-list-min-max))

(define (test-list-min-max)
  (and (equal? (cons 1 1) (list-min-max (list 1) <))
       (equal? (cons 1 2) (list-min-max (list 1 2) <))       
       (equal? (cons 1 2) (list-min-max (list 2 1) <))       
       (equal? (cons 1 2) (list-min-max (list 1 1 2) <))
       (equal? (cons 1 2) (list-min-max (list 1 2 1) <))
       (equal? (cons 1 2) (list-min-max (list 1 2 2) <))
       (equal? (cons 1 2) (list-min-max (list 2 1 1) <))
       (equal? (cons 1 2) (list-min-max (list 2 2 1) <))
       (equal? (cons 1 3) (list-min-max (list 1 1 3) <))
       (equal? (cons 1 3) (list-min-max (list 1 2 3) <))
       (equal? (cons 1 3) (list-min-max (list 1 3 2) <))
       (equal? (cons 1 3) (list-min-max (list 2 3 1) <))
       (equal? (cons 1 3) (list-min-max (list 3 2 1) <))))
