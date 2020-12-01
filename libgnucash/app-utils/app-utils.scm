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
(use-modules (sw_app_utils))
(use-modules (gnucash app-utils date-utilities)
             (gnucash app-utils options)
             (gnucash app-utils c-interface))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (sw_app_utils)
                   (gnucash app-utils date-utilities)
                   (gnucash app-utils options)
                   (gnucash app-utils c-interface))

;; gw-engine-spec.scm
(re-export HOOK-SAVE-OPTIONS)

(define gnc:*kvp-option-path* (list KVP-OPTION-PATH))
(export gnc:*kvp-option-path*)

;; Business options
(define gnc:*business-label* (N_ "Business"))
(define gnc:*company-name* (N_ "Company Name"))
(define gnc:*company-addy* (N_ "Company Address"))
(define gnc:*company-id* (N_ "Company ID"))
(define gnc:*company-phone* (N_ "Company Phone Number"))
(define gnc:*company-fax* (N_ "Company Fax Number"))
(define gnc:*company-url* (N_ "Company Website URL"))
(define gnc:*company-email* (N_ "Company Email Address"))
(define gnc:*company-contact* (N_ "Company Contact Person"))
(define gnc:*fancy-date-label* (N_ "Fancy Date Format"))
(define gnc:*fancy-date-format* (N_ "custom"))
(define gnc:*tax-label* (N_ "Tax"))
(define gnc:*tax-nr-label* (N_ "Tax Number"))

(define (gnc:company-info book key)
  ;; Access company info from key-value pairs for current book
 (gnc:option-get-value book gnc:*business-label* key))

(define (gnc:fancy-date-info book key)
  ;; Access fancy date info from key-value pairs for current book
 (gnc:option-get-value book gnc:*business-label* (list gnc:*fancy-date-label* key)))

(export gnc:*business-label* gnc:*company-name*  gnc:*company-addy* 
        gnc:*company-id*     gnc:*company-phone* gnc:*company-fax* 
        gnc:*company-url*    gnc:*company-email* gnc:*company-contact*
        gnc:*fancy-date-label* gnc:*fancy-date-format*
        gnc:company-info gnc:fancy-date-info)

(define gnc:*option-section-accounts* OPTION-SECTION-ACCOUNTS)
(define gnc:*option-name-trading-accounts* OPTION-NAME-TRADING-ACCOUNTS)
(define gnc:*option-name-currency-accounting* OPTION-NAME-CURRENCY-ACCOUNTING)
(define gnc:*option-name-book-currency* OPTION-NAME-BOOK-CURRENCY)
(define gnc:*option-name-default-gains-policy* OPTION-NAME-DEFAULT-GAINS-POLICY)
(define gnc:*option-name-default-gain-loss-account* OPTION-NAME-DEFAULT-GAINS-LOSS-ACCT-GUID)
(define gnc:*option-name-auto-readonly-days* OPTION-NAME-AUTO-READONLY-DAYS)
(define gnc:*option-name-num-field-source* OPTION-NAME-NUM-FIELD-SOURCE)

(export gnc:*option-section-accounts* gnc:*option-name-trading-accounts*
        gnc:*option-name-currency-accounting* gnc:*option-name-book-currency*
        gnc:*option-name-default-gains-policy*
        gnc:*option-name-default-gain-loss-account*
        gnc:*tax-label* gnc:*tax-nr-label*
        gnc:*option-name-auto-readonly-days* gnc:*option-name-num-field-source*)

(define gnc:*option-section-budgeting* OPTION-SECTION-BUDGETING)
(define gnc:*option-name-default-budget* OPTION-NAME-DEFAULT-BUDGET)

(export gnc:*option-section-budgeting* gnc:*option-name-default-budget*)

(load-from-path "gnucash/app-utils/business-options")


;; Symbols deprecated in 4.x, to remove for 5.x
(define-public (gnc:get-debit-string acct-type)
    (issue-deprecation-warning "gnc:get-debit-string is deprecated in 4.x. Please use (gnucash engine)'s gnc-account-get-debit-string instead.")
    (gnc-account-get-debit-string acct-type))
(define-public (gnc:get-credit-string acct-type)
    (issue-deprecation-warning "gnc:get-credit-string is deprecated in 4.x. Please use (gnucash engine)'s gnc-account-get-credit-string instead.")
    (gnc-account-get-debit-string acct-type))
(define-public (gnc:config-file-format-version version)
    (issue-deprecation-warning "gnc:config-file-format-version is deprecated in 4.x and will be removed from a future version.")
    #t)
