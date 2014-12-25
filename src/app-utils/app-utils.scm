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
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (load-extension "libgncmod-app-utils" "scm_init_sw_app_utils_module")))
  (else ))
(use-modules (sw_app_utils))
(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

;; Guile 2 needs to find the symbols from the c module at compile time already
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (gnc:module-load "gnucash/engine" 0)))
  (else
    (gnc:module-load "gnucash/engine" 0)))

;; gettext.scm
(re-export gnc:gettext)
(re-export _)
(re-export N_)

;; c-interface.scm
(export gnc:error->string)
(export gnc:make-string-database)

;; options.scm
(export gnc:make-option)
(export gnc:option-section)
(export gnc:option-name)
(export gnc:option-sort-tag)
(export gnc:option-type)
(export gnc:option-documentation)
(export gnc:option-getter)
(export gnc:option-setter)
(export gnc:option-default-getter)
(export gnc:option-generate-restore-form)
(export gnc:option-scm->kvp)
(export gnc:set-option-scm->kvp)
(export gnc:option-kvp->scm)
(export gnc:set-option-kvp->scm)
(export gnc:option-value-validator)
(export gnc:option-data)
(export gnc:option-data-fns)
(export gnc:option-set-changed-callback)
(export gnc:option-strings-getter)
(export gnc:option-widget-changed-proc)
(export gnc:option-value)
(export gnc:option-set-value)
(export gnc:option-index-get-name)
(export gnc:option-index-get-description)
(export gnc:option-index-get-value)
(export gnc:option-value-get-index)
(export gnc:option-number-of-indices)
(export gnc:option-default-value)
(export gnc:restore-form-generator)
(export gnc:value->string)
(export gnc:make-string-option)
(export gnc:make-text-option)
(export gnc:make-font-option)
(export gnc:make-currency-option)
(export gnc:make-commodity-option)
(export gnc:make-simple-boolean-option)
(export gnc:make-complex-boolean-option)
(export gnc:make-pixmap-option)
(export gnc:make-date-option)
(export gnc:make-budget-option)
(export gnc:get-rd-option-data-subtype)
(export gnc:get-rd-option-data-show-time)
(export gnc:get-rd-option-data-rd-list)
(export gnc:date-option-get-subtype)
(export gnc:date-option-show-time?)
(export gnc:date-option-value-type)
(export gnc:date-option-absolute-time)
(export gnc:date-option-relative-time)
(export gnc:make-account-list-option)
(export gnc:make-account-list-limited-option)
(export gnc:make-account-sel-option)
(export gnc:make-account-sel-limited-option)
(export gnc:multichoice-list-lookup)
(export gnc:make-multichoice-option)
(export gnc:make-multichoice-callback-option)
(export gnc:make-radiobutton-option)
(export gnc:make-radiobutton-callback-option)
(export gnc:make-list-option)
(export gnc:options-make-end-date!)
(export gnc:options-make-date-interval!)

(export gnc:make-number-range-option)
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
(export gnc:options-register-callback)
(export gnc:options-register-c-callback)
(export gnc:options-unregister-callback-id)
(export gnc:options-for-each)
(export gnc:options-for-each-general)
(export gnc:lookup-option)
(export gnc:generate-restore-forms)
(export gnc:options-scm->kvp)
(export gnc:options-kvp->scm)
(export gnc:options-clear-changes)
(export gnc:options-touch)
(export gnc:options-run-callbacks)
(export gnc:options-set-default-section)
(export gnc:options-get-default-section)
(export gnc:options-copy-values)
(export gnc:send-options)
(export gnc:save-options)

;; config-var.scm
(export gnc:make-config-var)
(export gnc:config-var-description-get)
(export gnc:config-var-action-func-get)
(export gnc:config-var-equality-func-get)
(export gnc:config-var-modified?)
(export gnc:config-var-modified?-set!)
(export gnc:config-var-default-value-get)
(export gnc:config-var-default-value-set!)
(export gnc:config-var-value-get)
(export gnc:config-var-value-set!)
(export gnc:config-var-value-is-default?)

;; prefs.scm
(export gnc:get-debit-string)
(export gnc:get-credit-string)
(export gnc:config-file-format-version)

;; gw-engine-spec.scm
(re-export HOOK-SAVE-OPTIONS)

;; date-utilities.scm

(export gnc:reldate-list)
(export gnc:timepair->secs)
(export gnc:secs->timepair)
(export gnc:timepair->date)
(export gnc:date->timepair)
(export gnc:timepair?)
(export gnc:date-get-year)
(export gnc:date-get-quarter)
(export gnc:date-get-month-day)
(export gnc:date-get-month)
(export gnc:date-get-week-day)
(export gnc:date-get-year-day)
(export gnc:timepair-get-year)
(export gnc:timepair-get-quarter)
(export gnc:timepair-get-month-day)
(export gnc:timepair-get-month)
(export gnc:timepair-get-week-day)
(export gnc:timepair-get-week)
(export gnc:timepair-get-year-day)
(export gnc:date-get-year-string)
(export gnc:date-get-quarter-string)
(export gnc:date-get-quarter-year-string)
(export gnc:date-get-month-string)
(export gnc:date-get-month-year-string)
(export gnc:date-get-week-year-string)
(export gnc:leap-year?)
(export gnc:days-in-year)
(export gnc:days-in-month)
(export gnc:date-to-year-fraction)
(export gnc:date-year-delta)
(export gnc:date-to-month-fraction)
(export gnc:date-to-week-fraction)
(export gnc:date-to-week)
(export gnc:date-to-day-fraction)
(export gnc:date-get-fraction-func)
(export moddatek)
(export decdate)
(export incdate)
(export gnc:timepair-later)
(export gnc:timepair-lt)
(export gnc:timepair-earlier)
(export gnc:timepair-gt)
(export gnc:timepair-le)
(export gnc:timepair-ge)
(export gnc:timepair-eq)
(export gnc:timepair-earlier-date)
(export gnc:timepair-later-date)
(export gnc:timepair-le-date)
(export gnc:timepair-ge-date)
(export gnc:timepair-eq-date)
(export gnc:make-date-interval-list)
(export gnc:make-date-list)
(export make-zdate)
(export SecDelta )
(export DayDelta)
(export WeekDelta )
(export TwoWeekDelta)
(export MonthDelta)
(export QuarterDelta)
(export HalfYearDelta)
(export YearDelta )
(export ThirtyDayDelta)
(export NinetyDayDelta)
(export gnc:deltasym-to-delta)
(export gnc:timepair-delta)
(export gnc:time-elapsed)
(export gnc:timepair-start-day-time)
(export gnc:timepair-end-day-time)
(export gnc:timepair-previous-day)
(export gnc:timepair-next-day)
(export gnc:reldate-get-symbol)
(export gnc:reldate-get-string)
(export gnc:reldate-get-desc)
(export gnc:reldate-get-fn)
(export gnc:make-reldate-hash)
(export gnc:reldate-string-db)
(export gnc:relative-date-values)
(export gnc:relative-date-hash)
(export gnc:get-absolute-from-relative-date)
(export gnc:get-relative-date-strings)
(export gnc:get-relative-date-string)
(export gnc:get-relative-date-desc)
(export gnc:get-start-cal-year)
(export gnc:get-end-cal-year)
(export gnc:get-start-prev-year)
(export gnc:get-end-prev-year)
(export gnc:get-start-this-month)
(export gnc:get-end-this-month)
(export gnc:get-start-prev-month)
(export gnc:get-end-prev-month)
(export gnc:get-start-current-quarter)
(export gnc:get-end-current-quarter)
(export gnc:get-start-prev-quarter)
(export gnc:get-end-prev-quarter)
(export gnc:get-today)
(export gnc:get-one-month-ago)
(export gnc:get-three-months-ago)
(export gnc:get-six-months-ago)
(export gnc:get-one-year-ago)
(export gnc:reldate-initialize)
(export gnc:get-end-next-month)
(export gnc:get-end-next-quarter)
(export gnc:get-end-next-year)
(export gnc:get-one-month-ahead)
(export gnc:get-one-year-ahead)
(export gnc:get-six-months-ahead)
(export gnc:get-start-next-month)
(export gnc:get-start-next-quarter)
(export gnc:get-start-next-year)
(export gnc:get-three-months-ahead)

;; hooks 
(export gnc:hook-run-danglers)		;; from hooks.scm
(re-export gnc-hook-add-scm-dangler)
(re-export HOOK-REPORT)

;; simple-obj
(export make-simple-class)
(export simple-obj-getter)
(export simple-obj-setter)
(export simple-obj-print)
(export simple-obj-to-list)
(export simple-obj-from-list)
(export make-simple-obj)

(define gnc:*kvp-option-path* (list KVP-OPTION-PATH))
(export gnc:*kvp-option-path*)

(load-from-path "c-interface")
(load-from-path "config-var")
(load-from-path "options")
(load-from-path "hooks")
(load-from-path "prefs")
(load-from-path "date-utilities")
(load-from-path "simple-obj")

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

(define (gnc:company-info key)
  ;; Access company info from key-value pairs for current book
  (kvp-frame-get-slot-path-gslist
    (qof-book-get-slots (gnc-get-current-book))
    (append gnc:*kvp-option-path* (list gnc:*business-label* key))))

(export gnc:*business-label* gnc:*company-name*  gnc:*company-addy* 
        gnc:*company-id*     gnc:*company-phone* gnc:*company-fax* 
        gnc:*company-url*    gnc:*company-email* gnc:*company-contact*
        gnc:company-info)

(define gnc:*option-section-accounts* OPTION-SECTION-ACCOUNTS)
(define gnc:*option-name-trading-accounts* OPTION-NAME-TRADING-ACCOUNTS)
(define gnc:*option-name-auto-readonly-days* OPTION-NAME-AUTO-READONLY-DAYS)
(define gnc:*option-name-num-field-source* OPTION-NAME-NUM-FIELD-SOURCE)

(export gnc:*option-section-accounts* gnc:*option-name-trading-accounts*
        gnc:*option-name-auto-readonly-days* gnc:*option-name-num-field-source*)

(define gnc:*option-section-budgeting* OPTION-SECTION-BUDGETING)
(define gnc:*option-name-default-budget* OPTION-NAME-DEFAULT-BUDGET)

(export gnc:*option-section-budgeting* gnc:*option-name-default-budget*)

(load-from-path "business-options")
(load-from-path "business-prefs")
