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

(define (no-op-deprecation-warning)
    (issue-deprecation-warning "* WARNING * Guile wrappers for the gnc module system have been deprecated.")
    (issue-deprecation-warning "This particular function call is now a no-op. Please use equivalent (use-modules ...) calls instead."))

(define-public (gnc:module-system-init)
    (no-op-deprecation-warning))
(define-public (gnc:module-system-refresh)
    (no-op-deprecation-warning))
(define-public (gnc:module-load-optional)
    (no-op-deprecation-warning))
(define-public (gnc:module-unload)
    (no-op-deprecation-warning))
(define-public (gnc:module-load gnc-mod-name mod-sys-version)
    (let* ((mod-name-split (string-split gnc-mod-name #\/))
           (mod-name-str (string-join mod-name-split " "))
           (scm-mod-name (map string->symbol mod-name-split)))
        (cond
            ((string=? gnc-mod-name "gnucash/app-utils")
             (issue-deprecation-warning "* WARNING * 'gnc:module-load (\"gnucash/app-utils\" 0)' has been deprecated and will be removed in gnucash 5.0.")
             (issue-deprecation-warning "Use '(use-modules (gnucash engine)(gnucash app-utils))' instead.")
             (issue-deprecation-warning "Use of the '(gnucash engine)' guile module is optional and depends on whether or not you use functions from this module in your code or not.")
             (use-modules (gnucash engine)(gnucash app-utils)))
            ((or
                (string=? gnc-mod-name "gnucash/tax/de_DE")
                (string=? gnc-mod-name "gnucash/tax/us"))
             (set! scm-mod-name (list 'gnucash 'locale (list-ref scm-mod-name 2) 'tax))
             (set! mod-name-str (string-join (map symbol->string scm-mod-name) " "))
             (issue-deprecation-warning (string-concatenate (list "* WARNING * '(gnc:module-load \"" gnc-mod-name "\" 0)' has been deprecated.")))
             (issue-deprecation-warning (string-concatenate (list "Use '(use-modules (" mod-name-str "))' instead.")))
             (module-use! (current-module) (resolve-interface scm-mod-name)))
            ((or
                (string=? gnc-mod-name "gnucash/gnome-utils")
                (string=? gnc-mod-name "gnucash/html")
                (string=? gnc-mod-name "gnucash/report/report-system"))
             (when (string=? gnc-mod-name"gnucash/report/report-system")
                (set! mod-name-str "gnucash report"))
                (set! scm-mod-name (list 'gnucash 'report))
             (issue-deprecation-warning (string-concatenate (list "* WARNING * '(gnc:module-load \"" gnc-mod-name "\" 0)' has been deprecated.")))
             (issue-deprecation-warning (string-concatenate (list "Use '(use-modules (gnucash engine)(gnucash app-utils)(" mod-name-str "))' instead.")))
             (issue-deprecation-warning "Use of the '(gnucash engine)' or '(gnucash app-utils)' guile modules is optional and depends on whether or not you use functions from this module in your code or not.")
             (use-modules (gnucash engine)(gnucash app-utils))
             (module-use! (current-module) (resolve-interface scm-mod-name)))
            (else
             (issue-deprecation-warning (string-concatenate (list "* WARNING * '(gnc:module-load \"" gnc-mod-name "\" 0)' has been deprecated.")))
             (issue-deprecation-warning (string-concatenate ( list "Use '(use-modules (" mod-name-str "))' instead.")))
             (issue-deprecation-warning "Additional guile modules may have to be loaded depending on your specific code.")
             (module-use! (current-module) (resolve-interface scm-mod-name))))))
