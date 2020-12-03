;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  core-utils.scm
;;;  Guile module for core-utils
;;;
;;;  Copyright 2006 Chris Shoemaker <c.shoemaker@cox.net>
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


(define-module (gnucash core-utils))

(eval-when (compile load eval expand)
  (load-extension "libgnucash-guile" "gnc_guile_bindings_init"))

(use-modules (ice-9 i18n))

(export N_)
(export G_)
(export NG_)
(export C_)
(export load-and-reexport)
(export gnc:string-locale<?)
(export gnc:string-locale>?)
(export gnc:version)

;; loads modules and re-exports all its public interface into the
;; current module
(define-syntax load-and-reexport
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (use-modules (mod ...))
       ...
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(load-and-reexport (sw_core_utils))

(define gnc:version (gnc-version))

;; gettext functions
(define G_ gnc:gettext)
(define NG_ gnc:ngettext)
(define C_ gnc:C-gettext)
(define N_ identity)

;; the following will define _ to call gnc:gettext for guile up to
;; 2.2. It may be removed in the future when minimum guile is 3.0.
(cond-expand
  (guile-3)
  (guile-2
   (define-public (_ x)
     (issue-deprecation-warning "Using _ to call gettext is disallowed in guile-3 and will be removed in the future. Use G_ instead.")
     (gnc:gettext x))))

(define gnc:string-locale<? string-locale<?)
(define gnc:string-locale>? string-locale>?)
