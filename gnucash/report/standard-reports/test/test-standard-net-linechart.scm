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
;(use-modules (gnucash report new-reports reports-2))

(use-modules (gnucash gnc-module))

;; Guile 2 needs to load external modules at compile time
;; otherwise the N_ syntax-rule won't be found at compile time
;; causing the test to fail
;; That's what the wrapper below is meant for:
(cond-expand
   (guile-2
    (define-syntax-rule (begin-for-syntax form ...)
      (eval-when (load compile eval expand) (begin form ...))))
   (else
    (define begin-for-syntax begin)))

(begin-for-syntax (gnc:module-load "gnucash/report/report-system" 0))
(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash report report-system test test-extras))

(use-modules (gnucash report standard-reports test test-generic-net-linechart))
(use-modules (gnucash report standard-reports net-linechart))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (run-net-asset-test net-worth-linechart-uuid))

