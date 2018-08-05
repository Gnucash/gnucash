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

(define-module (gnucash gnome-utils))

(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))

(eval-when
      (compile load eval expand)
      (load-extension "libgncmod-gnome-utils" "scm_init_sw_gnome_utils_module"))
(use-modules (sw_gnome_utils))
(gnc:module-load "gnucash/app-utils" 0)

;; from gnc-menu-extensions.scm
(export gnc:extension-type)
(export gnc:extension-name)
(export gnc:extension-guid)
(export gnc:extension-documentation)
(export gnc:extension-path)
(export gnc:extension-script)
(export gnc:make-menu-item)
(export gnc:make-menu)
(export gnc:make-separator)

(load-from-path "gnc-menu-extensions")

