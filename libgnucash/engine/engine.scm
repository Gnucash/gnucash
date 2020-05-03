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
      (load-extension "libgncmod-engine" "scm_init_sw_engine_module"))
(use-modules (sw_engine))

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

(export GNC_COMMODITY_NS_CURRENCY)
(export GNC_COMMODITY_NS_NASDAQ)
(export GNC_COMMODITY_NS_NYSE)
(export GNC_COMMODITY_NS_AMEX)
(export GNC_COMMODITY_NS_EUREX)
(export GNC_COMMODITY_NS_MUTUAL)

(export gnc:url->loaded-session)

;; engine-utilities.scm
(export gnc:account-map-descendants)
(export gnc:account-map-children)

(export account-same?)                  ;deprecated
(export account-in-list?)               ;deprecated
(export account-in-list-pred)           ;deprecated
(export account-in-alist)               ;deprecated
(export account-full-name<?)
(export accounts-get-children-depth)
(export account-hashtable-ref)          ;deprecated
(export account-hashtable-set!)         ;deprecated

(export split-same?)                    ;deprecated
(export split-in-list?)                 ;deprecated
(export split-hashtable-ref)            ;deprecated
(export split-hashtable-set!)           ;deprecated

(export gnc:split-structure)
(export gnc:make-split-scm)
(export gnc:split-scm?)
(export gnc:split-scm-get-split-guid)
(export gnc:split-scm-get-account-guid)
(export gnc:split-scm-get-transaction-guid)
(export gnc:split-scm-get-memo)
(export gnc:split-scm-get-action)
(export gnc:split-scm-get-reconcile-state)
(export gnc:split-scm-get-reconciled-date)
(export gnc:split-scm-get-amount)
(export gnc:split-scm-get-value)
(export gnc:split-scm-set-split-guid)
(export gnc:split-scm-set-account-guid)
(export gnc:split-scm-set-transaction-guid)
(export gnc:split-scm-set-memo)
(export gnc:split-scm-set-action)
(export gnc:split-scm-set-reconcile-state)
(export gnc:split-scm-set-reconciled-date)
(export gnc:split-scm-set-amount)
(export gnc:split-scm-set-value)
(export gnc:split->split-scm)
(export gnc:split-scm-onto-split)
(export gnc:transaction-structure)
(export gnc:make-transaction-scm)
(export gnc:transaction-scm?)
(export gnc:transaction-scm-get-transaction-guid)
(export gnc:transaction-scm-get-currency)
(export gnc:transaction-scm-get-date-entered)
(export gnc:transaction-scm-get-date-posted)
(export gnc:transaction-scm-get-num)
(export gnc:transaction-scm-get-description)
(export gnc:transaction-scm-get-notes)
(export gnc:transaction-scm-get-split-scms)
(export gnc:transaction-scm-get-split-scm)
(export gnc:transaction-scm-get-other-split-scm)
(export gnc:transaction-scm-set-transaction-guid)
(export gnc:transaction-scm-set-currency)
(export gnc:transaction-scm-set-date-entered)
(export gnc:transaction-scm-set-date-posted)
(export gnc:transaction-scm-set-num)
(export gnc:transaction-scm-set-description)
(export gnc:transaction-scm-set-notes)
(export gnc:transaction-scm-set-split-scms)
(export gnc:transaction-scm-append-split-scm)
(export gnc:transaction->transaction-scm)
(export trans-splits)
(export gnc:transaction-scm-onto-transaction)

(load-from-path "gnc-numeric")
(load-from-path "commodity-table")
(load-from-path "engine-interface")
(load-from-path "engine-utilities")
