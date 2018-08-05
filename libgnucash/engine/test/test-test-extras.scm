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
(use-modules (gnucash engine test test-extras))
(use-modules (ice-9 streams))
(use-modules (gnucash engine))
(use-modules (sw_engine))

(define (run-test)
 (test-create-account-structure))

(define (test-create-account-structure)
  (let ((env (create-test-env)))
    (let ((accounts (env-create-account-structure env (list "Assets"
							    (list (cons 'type ACCT-TYPE-ASSET))
							    (list "Bank Account")
							    (list "Savings"
								  (list "Instant")
								  (list "30 day notice"))))))
      (and (= 3 (length accounts))
	   (equal? "Assets" (xaccAccountGetName (car accounts)))
	   ))))




