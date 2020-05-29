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

(use-modules (srfi srfi-1)
             (srfi srfi-13))

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

; business-core.scm
(export gnc:owner-get-address)
(export gnc:owner-get-name-dep)
(export gnc:owner-get-address-dep)
(export gnc:owner-get-name-and-address-dep)
(export gnc:owner-get-owner-id)
(export gnc:owner-from-split)
(export gnc:split->owner)

(load-from-path "gnucash/engine/gnc-numeric")
(load-from-path "gnucash/engine/commodity-table")
(load-from-path "gnucash/engine/business-core")

;; A few account related utility functions which used to be in engine-utilities.scm
(define (gnc:account-map-descendants thunk account)
        (let ((descendants (or (gnc-account-get-descendants-sorted account) '())))
             (map thunk descendants)))

(define (gnc:account-map-children thunk account)
        (let ((children (or (gnc-account-get-children-sorted account) '())))
             (map thunk children)))

;; account related functions
;; helper for sorting of account list
(define (account-full-name<? a b)
        (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

;; return maximum depth over accounts and their children, if any
(define (accounts-get-children-depth accounts)
        (apply max
            (map (lambda (acct)
                         (let ((acct-depth (gnc-account-get-current-depth acct)))
                              (+ acct-depth (- (gnc-account-get-tree-depth acct) 1))))
                 accounts)))

(export gnc:account-map-descendants)
(export gnc:account-map-children)
(export account-full-name<?)
(export accounts-get-children-depth)
