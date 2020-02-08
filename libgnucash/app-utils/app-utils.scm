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

(export gnc:make-internal-option)
(export gnc:make-query-option)
(export gnc:make-color-option)
(export gnc:make-dateformat-option)
(export gnc:dateformat-get-format)

(export gnc:color->html)
(export gnc:color-option->html)
(export gnc:color-option->hex-string)
(export gnc:new-options)

(export gnc:register-option)
(export gnc:unregister-option)
(export gnc:options-register-callback)
(export gnc:options-register-c-callback)
(export gnc:options-unregister-callback-id)
(export gnc:options-for-each)
(export gnc:options-for-each-general)
(export gnc:lookup-option)
(export gnc:generate-restore-forms)
(export gnc:options-fancy-date)
(export gnc:options-scm->kvp)
(export gnc:options-kvp->scm)
(export gnc:options-clear-changes)
(export gnc:options-touch)
(export gnc:options-run-callbacks)
(export gnc:options-set-default-section)
(export gnc:options-get-default-section)
(export gnc:options-copy-values)
(export gnc:send-options)

(define (gnc:option-get-value book category key)
  ;;Access an option directly
  (qof-book-get-option book
                       (if (list? key)
                           (append (list category) key)
                           (list category key))))
(export gnc:option-get-value)

;; gw-engine-spec.scm
(re-export HOOK-SAVE-OPTIONS)

(export gnc:get-debit-string)
(export gnc:get-credit-string)
(export gnc:config-file-format-version)
