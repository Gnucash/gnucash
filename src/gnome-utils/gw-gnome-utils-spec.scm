(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-gnome-utils-spec))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-app-utils-spec))

(let ((ws (gw:new-wrapset "gw-gnome-utils")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-app-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-gnome-utils))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset) 
     (list
      "#include <dialog-utils.h>\n"
      "#include <druid-utils.h>\n"
      "#include <gnc-amount-edit.h>\n"
      "#include <gnc-date-edit.h>\n"
      "#include <gnc-gnome-utils.h>\n"
      "#include <gnc-gui-query.h>\n"
      "#include <gnc-html.h>\n"
      "#include <gnc-mdi-utils.h>\n"
      "#include <gnc-menu-extensions.h>\n"
      "#include <gnc-ui.h>\n"
      "#include <print-session.h>\n"
      )))


  (gw:wrap-function
   ws
   'gnc:gnome-init
   '<gw:scm>
   "gnc_gnome_init"
   '(((<gw:mchars> caller-owned const) arg0)
     ((<gw:mchars> caller-owned const) progname)
     ((<gw:mchars> caller-owned const) version)
     (<gw:scm> command-line))
   "Initialize the GnuCash gnome system.")

  (gw:wrap-function
   ws
   'gnc:gnome-shutdown
   '<gw:void>
   "gnc_gnome_shutdown"
   '()
   "Shutdown the GnuCash gnome system.")

  (gw:wrap-as-wct ws '<gnc:UIWidget> "gncUIWidget" "const gncUIWidget")
  (gw:wrap-as-wct ws '<gnc:mdi-info*> "GNCMDIInfo*" "const GNCMDIInfo*")

  (gw:wrap-function
   ws
   'gnc:mdi-has-apps?
   '<gw:bool>
   "gnc_mdi_has_apps"
   '()
   "Return true if there are gnc mdi app windows open.")

  (gw:wrap-function
   ws
   'gnc:mdi-get-current
   '<gnc:mdi-info*>
   "gnc_mdi_get_current"
   '()
   "Return the main window data structure for the application.")

  (gw:wrap-function
   ws
   'gnc:mdi-save 
   '<gw:void>
   "gnc_mdi_save" '((<gnc:mdi-info*> mi) 
                    ((<gw:mchars> caller-owned) bookname))
   "Save the MDI window configuration for the specified book")

  (gw:wrap-function
   ws
   'gnc:mdi-restore
   '<gw:void>
   "gnc_mdi_restore" '((<gnc:mdi-info*> mi) 
                       ((<gw:mchars> caller-owned const) bookname))
   "Restore MDI window configuration for the specified book")


  (gw:wrap-as-wct ws '<gnc:PrintSession*> "PrintSession*" "const PrintSession*")
  (gw:wrap-as-wct ws '<gnc:PrintDialog*> "PrintDialog*" "const PrintDialog*")
  (gw:wrap-as-wct ws '<gnc:PaperDialog*> "PaperDialog*" "const PaperDialog*")

  (gw:wrap-function
   ws
   'gnc:print-session-create
   '<gnc:PrintSession*>
   "gnc_print_session_create"
   '()
   "Start a new print session.")

  (gw:wrap-function
   ws
   'gnc:print-session-destroy
   '<gw:void>
   "gnc_print_session_destroy"
   '((<gnc:PrintSession*> p))
   "Free a print session's resources")

  (gw:wrap-function
   ws
   'gnc:print-session-moveto
   '<gw:void>
   "gnc_print_session_moveto"
   '((<gnc:PrintSession*> p) (<gw:double> x) (<gw:double> y))
   "Move the current point")

  (gw:wrap-function
   ws
   'gnc:print-session-text
   '<gw:void>
   "gnc_print_session_text"
   '((<gnc:PrintSession*> p) ((<gw:mchars> caller-owned const) text))
   "Show some text in Courier 16")

  (gw:wrap-function
   ws
   'gnc:print-session-done
   '<gw:void>
   "gnc_print_session_done"
   '((<gnc:PrintSession*> p))
   "Let the print context know you're finished with it.")

  (gw:wrap-function
   ws
   'gnc:print-session-print
   '<gw:void>
   "gnc_print_session_print"
   '((<gnc:PrintSession*> p))
   "Show the GNOME print dialog to start printing.")

  (gw:wrap-function
   ws
   'gnc:error-dialog
   '<gw:void>
   "gnc_error_dialog"
   '(((<gw:mchars> caller-owned const) message))
   "Show ok dialog box with given error message.")

  (gw:wrap-function
   ws
   'gnc:verify-dialog
   '<gw:bool>
   "gnc_verify_dialog"
   '((<gw:bool> yes_is_default)
     ((<gw:mchars> caller-owned const) message))
   "Show yes/no dialog box with given message.")

  (gw:wrap-function
   ws
   'gnc:info-dialog
   '<gw:void>
   "gnc_info_dialog"
   '(((<gw:mchars> caller-owned const) message))
   "Show ok dialog box with given message.")

  (gw:wrap-function
   ws
   'gnc:warning-dialog
   '<gw:void>
   "gnc_warning_dialog"
   '(((<gw:mchars> caller-owned const) message))
   "Show warning dialog box with given message.")

  (gw:wrap-function
   ws
   'gnc:add-extension
   '<gw:void>
   "gnc_add_extension"
   '((<gw:scm> extension))
   "Add a menu extension.")

  (gw:wrap-function 
   ws
   'gnc:html-encode-string 
   '(<gw:gchars> caller-owned)
   "gnc_html_encode_string" '(((<gw:mchars> caller-owned const) bookname)))

  (gw:wrap-function
   ws
   'gnc:choose-radio-option-dialog-parented
   '<gw:int>
   "gnc_choose_radio_option_dialog_parented"
   '((<gnc:UIWidget> parent)
     ((<gw:mchars> caller-owned const) title)
     ((<gw:mchars> caller-owned const) msg)
     (<gw:int> default-choice)
     ((gw:glist-of (<gw:mchars> callee-owned) callee-owned) choices))
   "Show a dialog offering different mutually exclusive choices
in a radio list.")

  ;; gnc-amount-edit.h
  (gw:wrap-as-wct ws
                  '<gnc:GNCAmountEdit>
                  "GNCAmountEdit*" "const GNCAmountEdit*")

  (gw:wrap-function
   ws
   'gnc:amount-edit-new
   '<gnc:UIWidget>
   "gnc_amount_edit_new"
   '()
   "Return a new amount edit widget.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-gtk-entry
   '<gnc:UIWidget>
   "gnc_amount_edit_gtk_entry"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Return the gtk entry for a gnc amount edit widget.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-set-amount
   '<gw:void>
   "gnc_amount_edit_set_amount"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gnc:numeric> amount))
   "Set the amount of an amount edit widget.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-get-amount
   '<gnc:numeric>
   "gnc_amount_edit_get_amount"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Return the amount in an amount edit widget.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-evaluate
   '<gw:bool>
   "gnc_amount_edit_evaluate"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Evaluate the contents of an amount edit widget and return
#t if it is a valid entry.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-set-print-info
   '<gw:void>
   "gnc_amount_edit_set_print_info"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gnc:print-amount-info-scm> print-info))
   "Set the print info used by the amount edit.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-set-fraction
   '<gw:void>
   "gnc_amount_edit_set_fraction"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gw:int> fraction))
   "Set the fraction used by the amount edit widget.")

  (gw:wrap-function
   ws
   'gnc:amount-edit-set-evaluate-on-enter
   '<gw:void>
   "gnc_amount_edit_set_evaluate_on_enter"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gw:bool> evaluate-on-enter))
   "Set whether the edit widget evaluates on enter.")


  ;; gnc-date-edit.h
  (gw:wrap-as-wct ws '<gnc:GNCDateEdit> "GNCDateEdit*" "const GNCDateEdit*")

  (gw:wrap-function
   ws
   'gnc:date-edit-new
   '<gnc:UIWidget>
   "gnc_date_edit_new_ts"
   '((<gnc:time-pair> date) (<gw:bool> show-time) (<gw:bool> use-24-hour-format))
   "Return a new date edit widget.")

  (gw:wrap-function
   ws
   'gnc:date-edit-set-time
   '<gw:void>
   "gnc_date_edit_set_time_ts"
   '((<gnc:GNCDateEdit> date-edit)
     (<gnc:time-pair> time))
   "Set the time used by the date edit widget.")

  (gw:wrap-function
   ws
   'gnc:date-edit-get-date
   '<gnc:time-pair>
   "gnc_date_edit_get_date_ts"
   '((<gnc:GNCDateEdit> date-edit))
   "Return the date of the date-edit widget.")

  (gw:wrap-function
   ws
   'gnc:date-edit-get-date-end
   '<gnc:time-pair>
   "gnc_date_edit_get_date_end_ts"
   '((<gnc:GNCDateEdit> date-edit))
   "Return the date of the date-edit widget at the end of the day.")


  ;; druid-utils.h
  (gw:wrap-as-wct ws '<gnc:GnomeDruid> "GnomeDruid*" "const GnomeDruid*")

  (gw:wrap-function
   ws
   'gnc:druid-set-title-image
   '<gw:void>
   "gnc_druid_set_title_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:mchars> caller-owned) image-path))
   "Set the title image of a druid.")

  (gw:wrap-function
   ws
   'gnc:druid-set-logo-image
   '<gw:void>
   "gnc_druid_set_logo_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:mchars> caller-owned) logo-path))
   "Set the logo image of a druid.")

  (gw:wrap-function
   ws
   'gnc:druid-set-watermark-image
   '<gw:void>
   "gnc_druid_set_watermark_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:mchars> caller-owned) watermark-path))
   "Set the watermark image of a druid.")

  (gw:wrap-function
   ws
   'gnc:druid-set-colors
   '<gw:void>
   "gnc_druid_set_colors"
   '((<gnc:GnomeDruid> druid))
   "Set the colors of a druid.")


  ;; dialog-utils.h
  (gw:wrap-as-wct ws '<gnc:GtkCList> "GtkCList*" "const GtkCList*")

  (gw:wrap-function
   ws
   'gnc:clist-set-check
   '<gw:void>
   "gnc_clist_set_check"
   '((<gnc:GtkCList> clist)
     (<gw:int> row)
     (<gw:int> col)
     (<gw:bool> checked))
   "Set the check status of a clist cell.")

  (gw:wrap-function
   ws
   'gnc:clist-columns-autosize
   '<gw:void>
   "gnc_clist_columns_autosize"
   '((<gnc:GtkCList> clist))
   "Autosize the columns of a clist including the titles.")
)
