(define-module (g-wrapped gw-app-utils-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-engine-spec))

(let ((ws (gw:new-wrapset "gw-app-utils")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")

  (gw:wrapset-depends-on ws "gw-engine")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-app-utils))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset) 
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
      "#include <gnc-component-manager.h>\n")))

  (gw:wrap-simple-type ws '<gnc:print-amount-info-scm> "GNCPrintAmountInfo"
                       '("gnc_printinfo_p(" scm-var ")")
                       '(c-var " = gnc_scm2printinfo(" scm-var ");\n")
                       '(scm-var " = gnc_printinfo2scm(" c-var ");\n"))

  (gw:wrap-as-wct ws
                  '<gnc:OptionChangeCallback>
                  "GNCOptionChangeCallback" "const GNCOptionChangeCallback")

  (gw:wrap-as-wct ws '<gnc:OptionDB*> "GNCOptionDB*" "const GNCOptionDB*")

  (gw:wrap-function
   ws
   'gnc:get-current-group
   '<gnc:AccountGroup*>
   "gnc_get_current_group"
   '()
   "Get the current top-level group.")

  (gw:wrap-function
   ws
   'gnc:get-current-book
   '<gnc:Book*>
   "gnc_get_current_book"
   '()
   "Get the current top-level book.")

  (gw:wrap-function
   ws
   'gnc:get-current-session
   '<gnc:Session*>
   "gnc_get_current_session"
   '()
   "Get the current session.")

  (gw:wrap-function
   ws
   'gnc:exp-parser-init
   '<gw:void>
   "gnc_exp_parser_init"
   '()
   "Initialize the expression parser.")

  (gw:wrap-function
   ws
   'gnc:exp-parser-shutdown
   '<gw:void>
   "gnc_exp_parser_shutdown"
   '()
   "Shutdown the expression parser and free any associated memory.")

  (gw:wrap-function
   ws
   'gnc:parse-amount
   '<gw:scm>
   "gnc_parse_amount_helper"
   '(((<gw:mchars> caller-owned const) str)
     (<gw:bool> monetary))
   "Parse the expression and return either a gnc numeric or #f.")

  (gw:wrap-function
   ws
   'gnc:setup-gettext
   '<gw:void>
   "gnc_setup_gettext"
   '()
   "Runs bindtextdomain and textdomain.")

  (gw:wrap-function
   ws
   'gnc:gettext-helper
   '(<gw:mchars> caller-owned const)
   "gnc_gettext_helper"
   '(((<gw:mchars> caller-owned const) str))
   "Returns the translated version of string")

  (gw:wrap-function
   ws
   'gnc:option-db-new
   '<gnc:OptionDB*>
   "gnc_option_db_new"
   '((<gw:scm> guile-options))
   "Create an option DB with the set of guile options")

  (gw:wrap-function
   ws
   'gnc:option-db-destroy
   '<gw:void>
   "gnc_option_db_destroy"
   '((<gnc:OptionDB*> option-db))
   "Destroy the OptionDB")

  (gw:wrap-function
   ws
   'gnc:c-options-init
   '<gw:void>
   "gnc_options_init"
   '()
   "Initialize the C side options code.")

  (gw:wrap-function
   ws
   'gnc:c-options-shutdown
   '<gw:void>
   "gnc_options_shutdown"
   '()
   "Shutdown the C side options code.")

  (gw:wrap-function
   ws
   'gnc:set-option-selectable-by-name
   '<gw:void>
   "gnc_set_option_selectable_by_name"
   '(((<gw:mchars> caller-owned const) section)
     ((<gw:mchars> caller-owned const) name)
     (<gw:bool> selectable))
   "Set the appropriate option widget to be selectable or not selectable, depending on if <gw:bool> selectable is true or false respectively.")

  (gw:wrap-function
   ws
   'gnc:option-db-set-option-selectable-by-name
   '<gw:void>
   "gnc_option_db_set_option_selectable_by_name"
   '((<gw:scm> guile-options)
     ((<gw:mchars> caller-owned const) section)
     ((<gw:mchars> caller-owned const) name)
     (<gw:bool> selectable))
   "Set the appropriate option widget to be selectable or not selectable, depending on if <gw:bool> selectable is true or false respectively.")

  (gw:wrap-function
   ws
   'gnc:default-currency
   '(<gnc:commodity*> const)
   "gnc_default_currency"
   '()
   "Return the default currency set by the user.")

  (gw:wrap-function
   ws
   'gnc:amount->string
   '(<gw:mchars> callee-owned const)
   "xaccPrintAmount"
   '((<gnc:numeric> amount)
     (<gnc:print-amount-info-scm> info))
   "Print amount using current locale. The info argument
determines formatting details.")

  (gw:wrap-function
   ws
   'gnc:option-refresh-ui
   '<gw:void>
   "gncp_option_refresh_ui"
   '((<gw:scm> option))
   "Refresh the gui option with the current values.")

  (gw:wrap-function
   ws
   'gnc:option-invoke-callback
   '<gw:void>
   "gncp_option_invoke_callback"
   '((<gnc:OptionChangeCallback> callback) (<gw:void*> data))
   "Invoke the c option callback on the given data.")

  (gw:wrap-function
   ws
   'gnc:option-db-register-option
   '<gw:void>
   "gncp_option_db_register_option"
   '((<gw:int> db_handle) (<gw:scm> option))
   "Register the option with the option database db_handle.")

  (gw:wrap-function
   ws
   'gnc:locale-decimal-places
   '<gw:int>
   "gnc_locale_decimal_places"
   '()
   "Return the number of decimal places for this locale.")

  (gw:wrap-function
   ws
   'gnc:locale-default-currency
   '(<gnc:commodity*> const)
   "gnc_locale_default_currency"
   '()
   "Return the default currency for the current locale.")

  (gw:wrap-function
   ws
   'gnc:locale-default-iso-currency-code
   '(<gw:mchars> callee-owned const)
   "gnc_locale_default_iso_currency_code"
   '()
   "Return the default iso currency code for the current locale.")

  (gw:wrap-function
   ws
   'gnc:register-gui-component
   '<gw:int>
   "gnc_register_gui_component_scm"
   '(((<gw:mchars> caller-owned const) component-class)
     (<gw:scm> refresh-handler)
     (<gw:scm> close-handler))
   "Register a gui component with the component manager.")

  (gw:wrap-function
   ws
   'gnc:gui-component-watch-entity
   '<gw:void>
   "gnc_gui_component_watch_entity_direct"
   '((<gw:int> component-id)
     (<gnc:guid-scm> entity)
     (<gnc:event-type> event-type-mask))
   "Watch the events for a particular entity.")

  (gw:wrap-function
   ws
   'gnc:gui-component-watch-entity-type
   '<gw:void>
   "gnc_gui_component_watch_entity_type"
   '((<gw:int> component-id)
     ((<gw:mchars> caller-owned const) id-type)
     (<gnc:event-type> event-type-mask))
   "Watch the events for a particular entity type.")

  (gw:wrap-function
   ws
   'gnc:unregister-gui-component
   '<gw:void>
   "gnc_unregister_gui_component"
   '((<gw:int> component-id))
   "Unregister a gui component by id.")

  (gw:wrap-function
   ws
   'gnc:suspend-gui-refresh
   '<gw:void>
   "gnc_suspend_gui_refresh"
   '()
   "Suspend gui refresh events.")

  (gw:wrap-function
   ws
   'gnc:resume-gui-refresh
   '<gw:void>
   "gnc_resume_gui_refresh"
   '()
   "Resume gui refresh events.")

  (gw:wrap-function
   ws
   'gnc:gui-refresh-all
   '<gw:void>
   "gnc_gui_refresh_all"
   '()
   "Refresh all gui components.")

  (gw:wrap-function
   ws
   'gnc:gui-refresh-suspended
   '<gw:bool>
   "gnc_gui_refresh_suspended"
   '()
   "Return #t if refreshes are suspended.")

  (gw:wrap-function
   ws
   'gnc:close-gui-component
   '<gw:void>
   "gnc_close_gui_component"
   '((<gw:int> component-id))
   "Close a gui component by id.")

  (gw:wrap-function
   ws
   'gnc:account-get-full-name
   '(<gw:mchars> caller-owned)
   "gnc_account_get_full_name"
   '((<gnc:Account*> account))
   "Return the fully-qualified name of the account.")

  (gw:wrap-function
   ws
   'gnc:default-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_print_info"
   '((<gw:bool> use_symbol))
   "Return the default print info object.")

  (gw:wrap-function
   ws
   'gnc:commodity-print-info
   '<gnc:print-amount-info-scm>
   "gnc_commodity_print_info"
   '((<gnc:commodity*> commodity) (<gw:bool> use_symbol))
   "Return the default print info for commodity.")

  (gw:wrap-function
   ws
   'gnc:account-print-info
   '<gnc:print-amount-info-scm>
   "gnc_account_print_info"
   '((<gnc:Account*> account) (<gw:bool> use_symbol))
   "Return a print info for printing account balances.")

  (gw:wrap-function
   ws
   'gnc:split-amount-print-info
   '<gnc:print-amount-info-scm>
   "gnc_split_amount_print_info"
   '((<gnc:Split*> split) (<gw:bool> use_symbol))
   "Return a print info for printing split amounts.")

  (gw:wrap-function
   ws
   'gnc:split-value-print-info
   '<gnc:print-amount-info-scm>
   "gnc_split_value_print_info"
   '((<gnc:Split*> split) (<gw:bool> use_symbol))
   "Return a print info for print split value quantities.")

  (gw:wrap-function
   ws
   'gnc:default-share-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_share_print_info"
   '()
   "Return a print info for printing generic share quantities.")

  (gw:wrap-function
   ws
   'gnc:default-price-print-info
   '<gnc:print-amount-info-scm>
   "gnc_default_price_print_info"
   '()
   "Return a print info for printing generic price quantities.")

  (gw:wrap-function
   ws
   'gnc:account-reverse-balance?
   '<gw:bool>
   "gnc_reverse_balance"
   '((<gnc:Account*> account))
   "Given an account, find out whether the balance should be reversed for display")

  (gw:wrap-function
   ws
   'gnc:is-euro-currency
   '<gw:bool>
   "gnc_is_euro_currency"
   '((<gnc:commodity*> currency))
   "Check if a given currency is a EURO currency")

  (gw:wrap-function
   ws
   'gnc:is-euro-currency-code
   '<gw:bool>
   "gnc_is_euro_currency_code"
   '(((<gw:mchars> caller-owned const) str))
   "Check if a given currency is a EURO currency")

  (gw:wrap-function
   ws
   'gnc:convert-to-euro
   '<gnc:numeric>
   "gnc_convert_to_euro"
   '((<gnc:commodity*> currency) (<gnc:numeric> value))
   "Convert the value from the given currency to EURO")

  (gw:wrap-function
   ws
   'gnc:convert-from-euro
   '<gnc:numeric>
   "gnc_convert_from_euro"
   '((<gnc:commodity*> currency) (<gnc:numeric> value))
   "Convert the value from EURO to the given currency")

  (gw:wrap-function
   ws
   'gnc:euro-currency-get-rate
   '<gnc:numeric>
   "gnc_euro_currency_get_rate"
   '((<gnc:commodity*> currency))
   "Returns the exchange rate from the given currency to EURO")

  (gw:wrap-function
   ws
   'gnc:get-euro
   '<gnc:commodity*>
   "gnc_get_euro"
   '()
   "Returns the commodity EURO")

  (gw:wrap-function
   ws
   'gnc:account-separator-char
   '(<gw:mchars> callee-owned const)
   "gnc_get_account_separator_string"
   '()
   "Returns a string with the user-selected account separator"))
