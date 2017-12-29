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

(setenv "GNC_UNINSTALLED" "1")
(debug-set! stack 50000)
(load-from-path "c-interface")
(use-modules (gnucash engine test test-extras))

(define (test-func a b)
    (list (/ a b) 6))

(define (run-test)
    (and (test test-call-with-error-handling)
         (test test-eval-string-with-error-handling)
         (test test-apply-with-error-handling)))

(define (test-call-with-error-handling)
  (and (eq? #f (cadr (gnc:call-with-error-handling test-func (list 4 5))))
       (eq? #f (cadr (gnc:call-with-error-handling "(test-func 4 5)" '())))
       (eq? #f (car (gnc:call-with-error-handling test-func (list 4 0))))
       (eq? #f (car (gnc:call-with-error-handling "(test-func 4 0)" '())))))

(define (test-eval-string-with-error-handling)
  (and (eq? #f (cadr (gnc:eval-string-with-error-handling "(test-func 4 5)")))
       (eq? #f (car (gnc:eval-string-with-error-handling "(test-func 4 0)")))))

(define (test-apply-with-error-handling)
  (and (eq? #f (cadr (gnc:apply-with-error-handling test-func (list 4 5))))
       (eq? #f (car (gnc:apply-with-error-handling test-func (list 4 0))))))
