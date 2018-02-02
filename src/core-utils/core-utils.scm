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

;; Guile 2 needs to find the symbols from the extension at compile time already
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))
  (else
    (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))

(use-modules (sw_core_utils))

(re-export gnc-prefs-is-debugging-enabled)
(re-export gnc-path-get-bindir)
(re-export gnc-path-get-stdreportsdir)
(re-export gnc-path-find-localized-html-file)
(re-export gnc-build-dotgnucash-path)
(re-export gnc-build-report-path)
(re-export gnc-build-stdreports-path)
(re-export gnc-utf8?)
(re-export gnc-utf8-strip-invalid-strdup)
(re-export gnc-locale-from-utf8)
(re-export gnc-locale-to-utf8)
(re-export gnc-scm-log-warn)
(re-export gnc-scm-log-error)
(re-export gnc-scm-log-msg)
(re-export gnc-scm-log-debug)
(re-export gnc-locale-default-iso-currency-code)

(re-export gnc-prefs-set-bool)
(re-export gnc-prefs-set-int)
(re-export gnc-prefs-set-int64)
(re-export gnc-prefs-set-float)
(re-export gnc-prefs-set-string)
(re-export gnc-prefs-set-coords)
(re-export gnc-prefs-get-string)

(define-public gnc:version (gnc-version))
