(define-module (g-wrapped gw-gnome-utils-spec))

(use-modules (g-wrap))
(use-modules (g-wrapped gw-glib-spec))
(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-app-utils-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-gnome-utils")))
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
  (gw:module-depends-on mod "gw-glib")
  (gw:module-depends-on mod "gw-app-utils")

  (gw:module-set-guile-module! mod '(g-wrapped gw-gnome-utils))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?) 
     (list
      "#include <dialog-utils.h>\n"
      "#include <druid-utils.h>\n"
      "#include <gnc-amount-edit.h>\n"
      "#include <gnc-date-edit.h>\n"
      "#include <gnc-gui-query.h>\n"
      "#include <gnc-html.h>\n"
      "#include <gnc-mdi-utils.h>\n"
      "#include <gnc-menu-extensions.h>\n"
      "#include <gnc-ui.h>\n"
      "#include <print-session.h>\n"
      )))

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:UIWidget>
              "gncUIWidget" "const gncUIWidget")))
    #t)

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:mdi-info*>
              "GNCMDIInfo*" "const GNCMDIInfo*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:mdi-has-apps?
   '<gw:bool>
   "gnc_mdi_has_apps"
   '()
   "Return true if there are gnc mdi app windows open.")

  (gw:wrap-function
   mod
   'gnc:mdi-get-current
   '<gnc:mdi-info*>
   "gnc_mdi_get_current"
   '()
   "Return the main window data structure for the application.")

  (gw:wrap-function
   mod
   'gnc:mdi-save 
   '<gw:void>
   "gnc_mdi_save" '((<gnc:mdi-info*> mi) 
                    ((<gw:m-chars-caller-owned>) bookname))
   "Save the MDI window configuration for the specified book")

  (gw:wrap-function
   mod
   'gnc:mdi-restore
   '<gw:void>
   "gnc_mdi_restore" '((<gnc:mdi-info*> mi) 
                       ((<gw:m-chars-caller-owned> gw:const) bookname))
   "Restore MDI window configuration for the specified book")


  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:PrintSession*>
              "PrintSession*" "const PrintSession*")))
    #t)

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:PrintDialog*>
              "PrintDialog*" "const PrintDialog*")))
    #t)

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:PaperDialog*>
              "PaperDialog*" "const PaperDialog*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:print-session-create
   '<gnc:PrintSession*>
   "gnc_print_session_create"
   '()
   "Start a new print session.")

  (gw:wrap-function
   mod
   'gnc:print-session-destroy
   '<gw:void>
   "gnc_print_session_destroy"
   '((<gnc:PrintSession*> p))
   "Free a print session's resources")

  (gw:wrap-function
   mod
   'gnc:print-session-moveto
   '<gw:void>
   "gnc_print_session_moveto"
   '((<gnc:PrintSession*> p) (<gw:double> x) (<gw:double> y))
   "Move the current point")

  (gw:wrap-function
   mod
   'gnc:print-session-text
   '<gw:void>
   "gnc_print_session_text"
   '((<gnc:PrintSession*> p) ((<gw:m-chars-caller-owned> gw:const) text))
   "Show some text in Courier 16")

  (gw:wrap-function
   mod
   'gnc:print-session-done
   '<gw:void>
   "gnc_print_session_done"
   '((<gnc:PrintSession*> p))
   "Let the print context know you're finished with it.")

  (gw:wrap-function
   mod
   'gnc:print-session-print
   '<gw:void>
   "gnc_print_session_print"
   '((<gnc:PrintSession*> p))
   "Show the GNOME print dialog to start printing.")

  (gw:wrap-function
   mod
   'gnc:error-dialog
   '<gw:void>
   "gnc_error_dialog"
   '(((<gw:m-chars-caller-owned> gw:const) message))
   "Show ok dialog box with given error message.")

  (gw:wrap-function
   mod
   'gnc:verify-dialog
   '<gw:bool>
   "gnc_verify_dialog"
   '(((<gw:m-chars-caller-owned> gw:const) message)
     (<gw:bool> yes_is_default))
   "Show yes/no dialog box with given message.")

  (gw:wrap-function
   mod
   'gnc:info-dialog
   '<gw:void>
   "gnc_info_dialog"
   '(((<gw:m-chars-caller-owned> gw:const) message))
   "Show ok dialog box with given message.")

  (gw:wrap-function
   mod
   'gnc:warning-dialog
   '<gw:void>
   "gnc_warning_dialog"
   '(((<gw:m-chars-caller-owned> gw:const) message))
   "Show warning dialog box with given message.")

  (gw:wrap-function
   mod
   'gnc:add-extension
   '<gw:void>
   "gnc_add_extension"
   '((<gw:scm> extension))
   "Add a menu extension.")

  (gw:wrap-function 
   mod
   'gnc:html-encode-string 
   '<glib:g-chars-caller-owned> 
   "gnc_html_encode_string" '(((<gw:m-chars-caller-owned> gw:const) bookname)))

  (gw:wrap-function
   mod
   'gnc:choose-radio-option-dialog-parented
   '<gw:int>
   "gnc_choose_radio_option_dialog_parented"
   '((<gnc:UIWidget> parent)
     ((<gw:m-chars-caller-owned> gw:const) title)
     ((<gw:m-chars-caller-owned> gw:const) msg)
     (<gw:int> default-choice)
     (<gnc:list-of-string> choices))
   "Show a dialog offering different mutually exclusive choices
in a radio list.")


  ;; gnc-amount-edit.h
  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:GNCAmountEdit>
              "GNCAmountEdit*" "const GNCAmountEdit*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:amount-edit-new
   '<gnc:UIWidget>
   "gnc_amount_edit_new"
   '()
   "Return a new amount edit widget.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-gtk-entry
   '<gnc:UIWidget>
   "gnc_amount_edit_gtk_entry"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Return the gtk entry for a gnc amount edit widget.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-set-amount
   '<gw:void>
   "gnc_amount_edit_set_amount"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gnc:numeric> amount))
   "Set the amount of an amount edit widget.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-get-amount
   '<gnc:numeric>
   "gnc_amount_edit_get_amount"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Return the amount in an amount edit widget.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-evaluate
   '<gw:bool>
   "gnc_amount_edit_evaluate"
   '((<gnc:GNCAmountEdit> amount-edit))
   "Evaluate the contents of an amount edit widget and return
#t if it is a valid entry.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-set-print-info
   '<gw:void>
   "gnc_amount_edit_set_print_info"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gnc:print-amount-info-scm> print-info))
   "Set the print info used by the amount edit.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-set-fraction
   '<gw:void>
   "gnc_amount_edit_set_fraction"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gw:int> fraction))
   "Set the fraction used by the amount edit widget.")

  (gw:wrap-function
   mod
   'gnc:amount-edit-set-evaluate-on-enter
   '<gw:void>
   "gnc_amount_edit_set_evaluate_on_enter"
   '((<gnc:GNCAmountEdit> amount-edit)
     (<gw:bool> evaluate-on-enter))
   "Set whether the edit widget evaluates on enter.")


  ;; gnc-date-edit.h
  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:GNCDateEdit>
              "GNCDateEdit*" "const GNCDateEdit*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:date-edit-new
   '<gnc:UIWidget>
   "gnc_date_edit_new_ts"
   '((<gnc:time-pair> date)
     (<gw:bool> show-time)
     (<gw:bool> use-24-hour-format))
   "Return a new date edit widget.")

  (gw:wrap-function
   mod
   'gnc:date-edit-set-time
   '<gw:void>
   "gnc_date_edit_set_time_ts"
   '((<gnc:GNCDateEdit> date-edit)
     (<gnc:time-pair> time))
   "Set the time used by the date edit widget.")

  (gw:wrap-function
   mod
   'gnc:date-edit-get-date
   '<gnc:time-pair>
   "gnc_date_edit_get_date_ts"
   '((<gnc:GNCDateEdit> date-edit))
   "Return the date of the date-edit widget.")

  (gw:wrap-function
   mod
   'gnc:date-edit-get-date-end
   '<gnc:time-pair>
   "gnc_date_edit_get_date_end_ts"
   '((<gnc:GNCDateEdit> date-edit))
   "Return the date of the date-edit widget at the end of the day.")


  ;; druid-utils.h
  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:GnomeDruid>
              "GnomeDruid*" "const GnomeDruid*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:druid-set-title-image
   '<gw:void>
   "gnc_druid_set_title_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:m-chars-caller-owned>) image-path))
   "Set the title image of a druid.")

  (gw:wrap-function
   mod
   'gnc:druid-set-logo-image
   '<gw:void>
   "gnc_druid_set_logo_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:m-chars-caller-owned>) logo-path))
   "Set the logo image of a druid.")

  (gw:wrap-function
   mod
   'gnc:druid-set-watermark-image
   '<gw:void>
   "gnc_druid_set_watermark_image"
   '((<gnc:GnomeDruid> druid)
     ((<gw:m-chars-caller-owned>) watermark-path))
   "Set the watermark image of a druid.")

  (gw:wrap-function
   mod
   'gnc:druid-set-colors
   '<gw:void>
   "gnc_druid_set_colors"
   '((<gnc:GnomeDruid> druid))
   "Set the colors of a druid.")


  ;; dialog-utils.h
  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:GtkCList>
              "GtkCList*" "const GtkCList*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:clist-set-check
   '<gw:void>
   "gnc_clist_set_check"
   '((<gnc:GtkCList> clist)
     (<gw:int> row)
     (<gw:int> col)
     (<gw:bool> checked))
   "Set the check status of a clist cell.")

  (gw:wrap-function
   mod
   'gnc:clist-columns-autosize
   '<gw:void>
   "gnc_clist_columns_autosize"
   '((<gnc:GtkCList> clist))
   "Autosize the columns of a clist including the titles.")
)
