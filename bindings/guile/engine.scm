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

(define-module (gnucash engine))

(eval-when (compile load eval expand)
  (load-extension "libgnucash-guile" "gnc_guile_bindings_init"))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (gnucash core-utils))

(load-and-reexport (sw_engine)
                   (gnucash engine business-core)
                   (gnucash engine gnc-numeric))


(export gnc:price-ref)

(define price-guardian (make-guardian))

(define (reclaim-prices)
  (let ((price (price-guardian)))
    (when price
      (gnc-price-unref price)
      (reclaim-prices))))

(add-hook! after-gc-hook reclaim-prices)

(define (gnc:price-ref price)
  (gnc-price-ref price)
  (price-guardian price))
