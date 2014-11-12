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

(define-module (gnucash gettext))

;; Load a few different modules depending on the version of guile
(cond-expand
  (guile-2
    ;; Our app-utils gnc module must be evaluated at compile time
    ;; Without it sw_app_utils can't be evaluated below
    (eval-when
      (compile load eval expand)
      (load-extension "libgncmod-app-utils" "scm_init_sw_app_utils_module")))
  (else
    ;; Syncase is deprecated and redundant in guile 2
    (use-modules (ice-9 syncase))))
(use-modules (sw_app_utils))

;; gettext functions
(define gnc:gettext gnc-gettext-helper)
(define _ gnc:gettext)
(define-syntax N_
  (syntax-rules ()
    ((_ x) x)))


(if (< (string->number (major-version)) 2)
    (export-syntax N_))

(export gnc:gettext)
(export _)
(export N_)