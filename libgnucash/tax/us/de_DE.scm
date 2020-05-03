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

(define-module (gnucash tax de_DE))

(use-modules (gnucash gnc-module))
(eval-when
      (compile load eval expand)
      (load-extension "libgncmod-engine" "scm_init_sw_engine_module")
      (load-extension "libgncmod-app-utils" "scm_init_sw_app_utils_module"))
(use-modules (sw_app_utils))
(use-modules (sw_engine))
(use-modules (gnucash app-utils))

(export gnc:txf-get-payer-name-source)
(export gnc:txf-get-form)
(export gnc:txf-get-description)
(export gnc:txf-get-format)
(export gnc:txf-get-multiple)
(export gnc:txf-get-category-key)
(export gnc:txf-get-line-data)
(export gnc:txf-get-last-year)
(export gnc:txf-get-help)
(export gnc:txf-get-codes)
(export gnc:txf-get-tax-entity-type)
(export gnc:txf-get-tax-entity-type-description)
(export gnc:txf-get-tax-entity-type-codes)
(export gnc:txf-get-code-info)
(export txf-help-categories)

(export txf-income-categories)
(export txf-expense-categories)
(export txf-asset-categories)
(export txf-liab-eq-categories)

(load-from-path "txf-de_DE")
(load-from-path "txf-help-de_DE")
