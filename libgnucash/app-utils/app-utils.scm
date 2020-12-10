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

(define-module (gnucash app-utils))

(eval-when (compile load eval expand)
  (load-extension "libgnc-app-utils" "scm_init_sw_app_utils_module"))

(use-modules (srfi srfi-1))
(use-modules (gnucash utilities))
(use-modules (gnucash engine))
(use-modules (gnucash core-utils))


(load-and-reexport (sw_app_utils)
                   (gnucash app-utils date-utilities)
                   (gnucash app-utils business-options)
                   (gnucash app-utils business-prefs)
                   (gnucash app-utils options)
                   (gnucash app-utils c-interface))

(re-export HOOK-SAVE-OPTIONS)

(export gnc:get-debit-string)
(export gnc:get-credit-string)
(export gnc:config-file-format-version)

;; Symbols deprecated in 4.x, to remove for 5.x
(define (gnc:get-debit-string acct-type)
    (issue-deprecation-warning "gnc:get-debit-string is deprecated in 4.x. Please use (gnucash engine)'s gnc-account-get-debit-string instead.")
    (gnc-account-get-debit-string acct-type))
(define (gnc:get-credit-string acct-type)
    (issue-deprecation-warning "gnc:get-credit-string is deprecated in 4.x. Please use (gnucash engine)'s gnc-account-get-credit-string instead.")
    (gnc-account-get-debit-string acct-type))
(define (gnc:config-file-format-version version)
    (issue-deprecation-warning "gnc:config-file-format-version is deprecated in 4.x and will be removed from a future version.")
    #t)
