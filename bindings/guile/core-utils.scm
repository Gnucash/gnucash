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

(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (ice-9 i18n))
(use-modules (ice-9 regex))

(export N_)
(export G_)
(export NG_)
(export C_)
(export load-and-reexport)
(export gnc:string-locale<?)
(export gnc:string-locale>?)
(export gnc:version)
(export gnc:format)

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

;; Custom unbound-variable exception printer: instead of generic "In
;; procedure module-lookup: Unbound variable: varname", it will first
;; search all available modules to identify missing (use-modules) in
;; header, and offer hint to add it. This is adapted from Guix source.
(define (known-variable-definition variable)
  (define seen (make-hash-table))
  (let lp ((modules (list (resolve-module '() #f #f #:ensure #f))) (retval '()))
    (match modules
      (() retval)
      (((? (cut hash-ref seen <>)) . tail) (lp tail retval))
      ((head tail ...)
       (hash-set! seen head #t)
       (let ((next (append tail (hash-map->list (lambda (name module) module)
                                                (module-submodules head)))))
         (match (and=> (module-public-interface head)
                       (cut module-local-variable <> variable))
           (#f (lp next retval))
           (_ (lp next (cons (module-name head) retval)))))))))

(define (print-unbound-variable-error port key args default-printer)
  (match args
    ((proc message (variable) _ ...)
     (format port "Unbound variable: ~a. " variable)
     (match (known-variable-definition variable)
       (() (format port "It is a typo, or inaccessible in current module."))
       ((mod) (format port "Did you forget (use-modules ~s)?" mod))
       (modules (format port "It is defined in one of the following modules\n")
                (for-each (cut format port "(use-modules ~s)\n" <>) modules))))
    (_ (default-printer))))

(set-exception-printer! 'unbound-variable print-unbound-variable-error)

;; format.
(define %regex (make-regexp "[$][{]([[:alnum:]]+)[}]"))
(define (gnc:format str . bindings)
  (define hash (make-hash-table))
  (define (substitute m)
    (or (hashq-ref hash (string->symbol (match:substring m 1)))
        (warn "invalid identifier" (match:substring m 0))))
  (let lp ((bindings bindings))
    (match bindings
      (() (regexp-substitute/global #f %regex str 'pre substitute 'post))
      (((? symbol? k) v . rest) (hashq-set! hash k (format #f "~a" v)) (lp rest))
      (_ (error "gnc:format syntax error")))))
