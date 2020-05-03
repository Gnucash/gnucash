;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gnc-module.scm
;;;  Guile module which allows initialization of the gnucash module
;;;  system from Scheme 
;;;
;;;  Copyright 2001 Linux Developers Group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(define-module (gnucash gnc-module))

;; Guile 2 needs to find the symbols from the extension at compile time already
(eval-when
      (compile load eval expand)
      (load-extension "libgnc-module" "scm_init_sw_gnc_module_module"))

(use-modules (sw_gnc_module))

(define gnc:module-system-init gnc-module-system-init)
(define gnc:module-system-refresh gnc-module-system-refresh)
(define gnc:module-load gnc-module-load)
(define gnc:module-load-optional gnc-module-load-optional)
(define gnc:module-unload gnc-module-unload)

(export gnc:module-system-init)
(export gnc:module-system-refresh)
(export gnc:module-load)
(export gnc:module-load-optional)
(export gnc:module-unload)
(export gnc:module-begin-syntax)

;; Guile 2 needs to load external modules at compile time
(define-syntax-rule (gnc:module-begin-syntax form ...)
      (eval-when (load compile eval expand) (begin form ...)))
