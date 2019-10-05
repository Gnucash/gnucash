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

(eval-when
      (compile load eval expand)
      (load-extension "libgnucash-guile" "gnc_guile_bindings_init"))
(use-modules (sw_engine))

; Export the swig-wrapped symbols in the public interface of this module
(let ((i (module-public-interface (current-module))))
     (module-use! i (resolve-interface '(sw_engine))))

;; gnc-numeric.scm
(export GNC-RND-FLOOR)
(export GNC-RND-CEIL)
(export GNC-RND-TRUNC)
(export GNC-RND-PROMOTE)
(export GNC-RND-ROUND-HALF-DOWN)
(export GNC-RND-ROUND-HALF-UP)
(export GNC-RND-ROUND)
(export GNC-RND-NEVER)
(export GNC-DENOM-AUTO)
(export GNC-DENOM-REDUCE)
(export GNC-DENOM-FIXED)
(export GNC-DENOM-LCD)
(export GNC-DENOM-SIGFIG)
(export GNC-DENOM-SIGFIGS)
(export GNC-ERROR-OK)
(export GNC-ERROR-ARG)
(export GNC-ERROR-OVERFLOW)
(export GNC-ERROR-DENOM-DIFF)
(export GNC-ERROR-REMAINDER)
(export <gnc-monetary>)
(export gnc:gnc-monetary?)
(export gnc:make-gnc-monetary)
(export gnc:gnc-monetary-commodity)
(export gnc:gnc-monetary-amount)
(export gnc:monetary-neg)

;; commodity-table.scm
(export GNC_COMMODITY_NS_CURRENCY)
(export GNC_COMMODITY_NS_NASDAQ)
(export GNC_COMMODITY_NS_NYSE)
(export GNC_COMMODITY_NS_AMEX)
(export GNC_COMMODITY_NS_EUREX)
(export GNC_COMMODITY_NS_MUTUAL)

;; engine-utilities.scm
(export gnc:account-map-descendants)
(export gnc:account-map-children)

(export account-full-name<?)
(export accounts-get-children-depth)

; business-core.scm
(export gnc:owner-get-address)
(export gnc:owner-get-name-dep)
(export gnc:owner-get-address-dep)
(export gnc:owner-get-name-and-address-dep)
(export gnc:owner-get-owner-id)
(export gnc:owner-from-split)

(load-from-path "gnucash/engine/gnc-numeric")
(load-from-path "gnucash/engine/commodity-table")
(load-from-path "gnucash/engine/engine-utilities")
(load-from-path "gnucash/engine/business-core")
