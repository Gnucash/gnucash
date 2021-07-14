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

(export gnc-pricedb-lookup-latest-before-t64)
(export gnc-pricedb-lookup-latest-before-any-currency-t64)
(export gnc:account-map-descendants)
(export gnc:account-map-children)
(export account-full-name<?)
(export accounts-get-children-depth)

(define (gnc-pricedb-lookup-latest-before-t64 . args)
  (issue-deprecation-warning "gnc-pricedb-lookup-latest-before-t64 has been renamed to gnc-pricedb-lookup-nearest-before-t64")
  (apply gnc-pricedb-lookup-nearest-before-t64 args))

(define (gnc-pricedb-lookup-latest-before-any-currency-t64 . args)
  (issue-deprecation-warning "gnc-pricedb-lookup-latest-before-any-currency-t64 has been renamed to gnc-pricedb-lookup-nearest-before-any-currency-t64")
  (apply gnc-pricedb-lookup-nearest-before-any-currency-t64 args))

;; A few account related utility functions which used to be in engine-utilities.scm
(define (gnc:account-map-descendants thunk account)
  (issue-deprecation-warning "gnc:account-map-descendants is deprecated.")
  (map thunk (or (gnc-account-get-descendants-sorted account) '())))

(define (gnc:account-map-children thunk account)
  (issue-deprecation-warning "gnc:account-map-children is deprecated.")
  (map thunk (or (gnc-account-get-children-sorted account) '())))

;; account related functions
;; helper for sorting of account list
(define (account-full-name<? a b)
  (issue-deprecation-warning
   "account-full-name<? is deprecated. use gnc:account-full-name<? instead.")
  (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

;; return maximum depth over accounts and their children, if any
(define (accounts-get-children-depth accounts)
  (issue-deprecation-warning "accounts-get-children-depth is deprecated. use \
gnc:accounts-get-children-depth instead.")
  (1- (apply max
             (map (lambda (acct)
                    (+ (gnc-account-get-current-depth acct)
                       (gnc-account-get-tree-depth acct)))
                  accounts))))
