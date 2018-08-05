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

(define-module (gnucash unittest-support))
(eval-when
      (compile load eval expand)
      (load-extension "libtest-core-guile" "scm_init_unittest_support_module"))
(use-modules (unittest_support))

(re-export TestErrorStruct-log-level-set)
(re-export TestErrorStruct-log-level-get)
(re-export TestErrorStruct-log-domain-set)
(re-export TestErrorStruct-log-domain-get)
(re-export TestErrorStruct-msg-set)
(re-export TestErrorStruct-msg-get)
(re-export new-TestErrorStruct)
(re-export delete-TestErrorStruct)
(re-export G-LOG-FLAG-RECURSION)
(re-export G-LOG-FLAG-FATAL)
(re-export G-LOG-LEVEL-ERROR)
(re-export G-LOG-LEVEL-CRITICAL)
(re-export G-LOG-LEVEL-WARNING)
(re-export G-LOG-LEVEL-MESSAGE)
(re-export G-LOG-LEVEL-INFO)
(re-export G-LOG-LEVEL-DEBUG)
(re-export G-LOG-LEVEL-MASK)
(re-export test-add-error)
(re-export test-clear-error-list)
(re-export g-log-remove-handler)
(re-export test-set-checked-handler)
(re-export test-set-null-handler)
(re-export test-set-list-handler)
