(define-module (g-wrapped gw-app-utils-spec))

(use-modules (g-wrap))
(use-modules (g-wrapped gw-engine-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-app-utils")))
  (define (standard-c-call-gen result func-call-code)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (add-standard-result-handlers! type c->scm-converter)
    (define (standard-pre-handler result)
      (let* ((ret-type-name (gw:result-get-proper-c-type-name result))
             (ret-var-name (gw:result-get-c-name result)))
        (list "{\n"
              "    " ret-type-name " " ret-var-name ";\n")))
    
    (gw:type-set-pre-call-result-ccodegen! type standard-pre-handler)
    
    (gw:type-set-post-call-result-ccodegen!
     type
     (lambda (result)
       (let* ((scm-name (gw:result-get-scm-name result))
              (c-name (gw:result-get-c-name result)))
         (list
          (c->scm-converter scm-name c-name)
          "  }\n")))))
  
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-depends-on mod "gw-engine")

  (gw:module-set-guile-module! mod '(g-wrapped gw-app-utils))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?) 
     (list 
      "#include <global-options.h>\n"
      "#include <option-util.h>\n"
      "#include <global-options.h>\n"
      "#include <option-util.h>\n"
      "#include <gnc-euro.h>\n"
      "#include <gnc-exp-parser.h>\n"
      "#include <gnc-ui-util.h>\n"
      "#include <gnc-gettext-util.h>\n"
      "#include <gnc-helpers.h>\n"
      "#include <gnc-component-manager.h>\n"
      )))
  (let ((wt (gw:wrap-type
             mod
             '<gnc:print-amount-info-scm>
             "GNCPrintAmountInfo" "const GNCPrintAmountInfo")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x) (list "gnc_printinfo_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x) (list "gnc_scm2printinfo(" x ")"))))
         (list c-name
               " = "
               (old-func scm-name)
               ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x) (list "gnc_printinfo2scm(" x ")"))))
         (list scm-name
               " = "
               (old-func c-name)
               ";\n")))))
  
  (gw:wrap-non-native-type 
   mod '<gnc:OptionChangeCallback> 
   "GNCOptionChangeCallback" "const GNCOptionChangeCallback")


  (gw:wrap-function
   mod
   'gnc:get-current-group
   '<gnc:AccountGroup*>
   "gnc_get_current_group"
   '()
   "Get the current top-level group.")

  (gw:wrap-function
   mod
   'gnc:get-current-book
   '<gnc:Book*>
   "gnc_get_current_book"
   '()
   "Get the current top-level book.")

  (gw:wrap-function
   mod
   'gnc:exp-parser-init
   '<gw:void>
   "gnc_exp_parser_init"
   '()
   "Initialize the expression parser.")

  (gw:wrap-function
   mod
   'gnc:exp-parser-shutdown
   '<gw:void>
   "gnc_exp_parser_shutdown"
   '()
   "Shutdown the expression parser and free any associated memory.")

  (gw:wrap-function
   mod
   'gnc:gettext-helper
   '(<gw:m-chars-caller-owned> gw:const)
   "gnc_gettext_helper"
   '(((<gw:m-chars-caller-owned> gw:const) string))
   "Returns the translated version of string")

  (gw:wrap-function
   mod
   'gnc:c-options-init
   '<gw:void>
   "gnc_options_init"
   '()
   "Initialize the C side options code.")

  (gw:wrap-function
   mod
   'gnc:c-options-shutdown
   '<gw:void>
   "gnc_options_shutdown"
   '()
   "Shutdown the C side options code.")

  (gw:wrap-function
   mod
   'gnc:set-option-selectable-by-name
   '<gw:void>
   "gnc_set_option_selectable_by_name"
   '(((<gw:m-chars-caller-owned> gw:const) section)
     ((<gw:m-chars-caller-owned> gw:const) name)
     (<gw:bool> selectable))
   "Set the appropriate option widget to be selectable or not selectable, depending on if <gw:bool> selectable is true or false respectively.")

  (gw:wrap-function
   mod
   'gnc:option-db-set-option-selectable-by-name
   '<gw:void>
   "gnc_option_db_set_option_selectable_by_name"
   '((<gw:scm> guile-options)
     ((<gw:m-chars-caller-owned> gw:const) section)
     ((<gw:m-chars-caller-owned> gw:const) name)
     (<gw:bool> selectable))
   "Set the appropriate option widget to be selectable or not selectable, depending on if <gw:bool> selectable is true or false respectively.")

  (gw:wrap-function
   mod
   'gnc:default-currency
   '(<gnc:commodity*> gw:const)
   "gnc_default_currency"
   '()
   "Return the default currency set by the user.")

  (gw:wrap-function
   mod
   'gnc:amount->string
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccPrintAmount"
   '((<gnc:numeric> amount)
     (<gnc:print-amount-info-scm> info))
   "Print amount using current locale. The info argument
determines formatting details.")

  (gw:wrap-function
   mod
   'gnc:option-refresh-ui
   '<gw:void>
   "gncp_option_refresh_ui"
   '((<gw:scm> option))
   "Refresh the gui option with the current values.")

  (gw:wrap-function
   mod
   'gnc:option-invoke-callback
   '<gw:void>
   "gncp_option_invoke_callback"
   '((<gnc:OptionChangeCallback> callback) (<gw:void*> data))
   "Invoke the c option callback on the given data.")

  (gw:wrap-function
   mod
   'gnc:option-db-register-option
   '<gw:void>
   "gncp_option_db_register_option"
   '((<gw:int> db_handle) (<gw:scm> option))
   "Register the option with the option database db_handle.")

  (gw:wrap-function
   mod
   'gnc:locale-decimal-places
   '<gw:int>
   "gnc_locale_decimal_places"
   '()
   "Return the number of decimal places for this locale.")

  (gw:wrap-function
   mod
   'gnc:locale-default-currency
   '(<gnc:commodity*> gw:const)
   "gnc_locale_default_currency"
   '()
   "Return the default currency for the current locale.")

  (gw:wrap-function
   mod
   'gnc:locale-default-iso-currency-code
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_locale_default_iso_currency_code"
   '()
   "Return the default iso currency code for the current locale.")

  (gw:wrap-function
   mod
   'gnc:suspend-gui-refresh
   '<gw:void>
   "gnc_suspend_gui_refresh"
   '()
   "Suspend gui refresh events.")

  (gw:wrap-function
   mod
   'gnc:resume-gui-refresh
   '<gw:void>
   "gnc_resume_gui_refresh"
   '()
   "Resume gui refresh events.")

  (gw:wrap-function
   mod
   'gnc:account-get-full-name
   '(<gw:m-chars-caller-owned>)
   "gnc_account_get_full_name"
   '((<gnc:Account*> account))
   "Return the fully-qualified name of the account.")

  (gw:wrap-function
   mod
   'gnc:default-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_print_info"
   '((<gw:bool> use_symbol))
   "Return the default print info object.")

  (gw:wrap-function
   mod
   'gnc:commodity-print-info
   '<gnc:print-amount-info-scm>
   "gnc_commodity_print_info"
   '((<gnc:commodity*> commodity) (<gw:bool> use_symbol))
   "Return the default print info for commodity.")

  (gw:wrap-function
   mod
   'gnc:account-print-info
   '<gnc:print-amount-info-scm>
   "gnc_account_print_info"
   '((<gnc:Account*> account) (<gw:bool> use_symbol))
   "Return a print info for printing account balances.")

  (gw:wrap-function
   mod
   'gnc:split-amount-print-info
   '<gnc:print-amount-info-scm>
   "gnc_split_amount_print_info"
   '((<gnc:Split*> split) (<gw:bool> use_symbol))
   "Return a print info for printing split amounts.")

  (gw:wrap-function
   mod
   'gnc:split-value-print-info
   '<gnc:print-amount-info-scm>
   "gnc_split_value_print_info"
   '((<gnc:Split*> split) (<gw:bool> use_symbol))
   "Return a print info for print split value quantities.")

  (gw:wrap-function
   mod
   'gnc:default-share-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_share_print_info"
   '()
   "Return a print info for printing generic share quantities.")

  (gw:wrap-function
   mod
   'gnc:default-price-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_price_print_info"
   '()
   "Return a print info for printing generic price quantities.")

  (gw:wrap-function
   mod
   'gnc:account-reverse-balance?
   '<gw:bool>
   "gnc_reverse_balance"
   '((<gnc:Account*> account))
   "Given an account, find out whether the balance should be reversed for display")

  (gw:wrap-function
   mod
   'gnc:is-euro-currency
   '<gw:bool>
   "gnc_is_euro_currency"
   '((<gnc:commodity*> currency))
   "Check if a given currency is a EURO currency")

  (gw:wrap-function
   mod
   'gnc:is-euro-currency-code
   '<gw:bool>
   "gnc_is_euro_currency_code"
   '((<gw:m-chars-caller-owned> gw:const))
   "Check if a given currency is a EURO currency")

  (gw:wrap-function
   mod
   'gnc:convert-to-euro
   '<gnc:numeric>
   "gnc_convert_to_euro"
   '((<gnc:commodity*> currency) (<gnc:numeric> value))
   "Convert the value from the given currency to EURO")

  (gw:wrap-function
   mod
   'gnc:convert-from-euro
   '<gnc:numeric>
   "gnc_convert_from_euro"
   '((<gnc:commodity*> currency) (<gnc:numeric> value))
   "Convert the value from EURO to the given currency")

  (gw:wrap-function
   mod
   'gnc:euro-currency-get-rate
   '<gnc:numeric>
   "gnc_euro_currency_get_rate"
   '((<gnc:commodity*> currency))
   "Returns the exchange rate from the given currency to EURO")

  (gw:wrap-function
   mod
   'gnc:get-euro
   '<gnc:commodity*>
   "gnc_get_euro"
   '()
   "Returns the commodity EURO")

  (gw:wrap-function
   mod
   'gnc:account-separator-char
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_get_account_separator_string"
   '()
   "Returns a string with the user-selected account separator")
)
