;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-gnc-spec)
  :use-module (g-wrap))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-glib-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((mod (gw:new-module "gw-gnc")))
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
  (gw:module-depends-on mod "gw-gnome-utils")

  (gw:module-set-guile-module! mod '(g-wrapped gw-gnc))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <glib.h>\n"
      "#include <gnc-ui.h>\n"
      "#include <gnc-ui-util.h>\n"
      "#include <gnc-menu-extensions.h>\n"
      "#include <date.h>\n"
      "#include <guile-util.h>\n"
      "#include <gnc-engine.h>\n"
      "#include <gnc-commodity.h>\n"
      "#include <gnc-numeric.h>\n"
      "#include <window-main.h>\n"
      "#include <gnc-gui-query.h>\n"
      "#include <dialog-new-user.h>\n"
      "#include <dialog-print-check.h>\n"
      "#include <dialog-progress.h>\n"
      "#include <dialog-totd.h>\n"
      "#include <dialog-commodity.h>\n"
      "#include <druid-hierarchy.h>\n"
      "#include <top-level.h>\n"
      "#include <window-help.h>\n"
      "#include <window-main.h>\n"
      "#include <gnc-html.h>\n"
      "#include <dialog-find-transactions.h>\n"
      "#include <dialog-scheduledxaction.h>\n"
      "#include <dialog-sxsincelast.h>\n" )))

  (gw:wrap-function
   mod
   'gnc:ui-hierarchy-druid
   '<gw:void>
   "gnc_ui_hierarchy_druid"
   '()
   "Open the hierarchy druid for importing an account hierarchy.")

  (gw:wrap-function
   mod
   'gnc:ui-is-running?
   '<gw:bool>
   "gnucash_ui_is_running"
   '()
   "Predicate to determine if the UI is running.")

  (gw:wrap-function
   mod
   'gnc:ui-is-terminating?
   '<gw:bool>
   "gnucash_ui_is_terminating"
   '()
   "Predicate to determine if the UI is in the process of terminating.")

  (gw:wrap-function
   mod
   'gnc:new-user-dialog
   '<gw:void>
   "gnc_ui_new_user_dialog"
   '()
   "Show the new user dialog.")

  (gw:wrap-function
   mod
   'gnc:start-ui-event-loop
   '<gw:int>
   "gnc_ui_start_event_loop"
   '()
   "Start the UI event loop.")

  (gw:wrap-function
   mod
   'gnc:ui-init
   '<gw:void>
   "gnucash_ui_init"
   '()
   "Initialize the lower level ui parts.")

  (gw:wrap-function
   mod
   'gnc:ui-shutdown
   '<gw:void>
   "gnc_ui_shutdown"
   '()
   "Shutdown the UI.")

  (gw:wrap-function
   mod
   'gnc:ui-destroy
   '<gw:void>
   "gnc_ui_destroy"
   '()
   "Destroy the UI.")

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

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:PrintCheckDialog*>
              "PrintCheckDialog*" "const PrintCheckDialog*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:print-check-dialog-create
   '<gnc:PrintCheckDialog*>
   "gnc_ui_print_check_dialog_create"
   '((<gw:scm> callback))
   "Pop up a dialog to set up printing a check.")
  
  (gw:wrap-function
   mod
   'gnc:ui-totd-dialog-create-and-run
   '<gw:void>
   "gnc_ui_totd_dialog_create_and_run"
   '()
   "Create and run the \"Tip Of The Day\" dialog")

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:ProgressDialog*>
              "GNCProgressDialog *" "const GNCProgressDialog *")))
    #t)

  (gw:wrap-function
   mod
   'gnc:progress-dialog-new
   '<gnc:ProgressDialog*>
   "gnc_progress_dialog_new"
   '((<gnc:UIWidget> parent) (<gw:bool> use_ok_button))
   "Create and return a progress dialog. The parent may be NULL.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-title
   '<gw:void>
   "gnc_progress_dialog_set_title"
   '((<gnc:ProgressDialog*> progress)
     ((<gw:m-chars-caller-owned> gw:const) title))
   "Set the title of 'progress' to 'title'.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-heading
   '<gw:void>
   "gnc_progress_dialog_set_heading"
   '((<gnc:ProgressDialog*> progress)
     ((<gw:m-chars-caller-owned> gw:const) heading))
   "Set the heading of 'progress' to 'heading'.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-limits
   '<gw:void>
   "gnc_progress_dialog_set_limits"
   '((<gnc:ProgressDialog*> progress)
     (<gw:float> min)
     (<gw:float> max))
   "Set the mininum and maximum range of 'progress'.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-activity-mode
   '<gw:void>
   "gnc_progress_dialog_set_activity_mode"
   '((<gnc:ProgressDialog*> progress)
     (<gw:bool> activity-mode))
   "Set the mininum and maximum range of 'progress'.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-cancel-scm-func
   '<gw:void>
   "gnc_progress_dialog_set_cancel_scm_func"
   '((<gnc:ProgressDialog*> progress)
     (<gw:scm> cancel_func))
   "Set the guile cancel callback function. This callback is invoked
with no arguments when the user hits the cancel button. If the callback
returns #t, the dialog is closed, but not destroyed.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-set-value
   '<gw:void>
   "gnc_progress_dialog_set_value"
   '((<gnc:ProgressDialog*> progress) (<gw:float> value))
   "Set the value of the progress dialog to 'value'.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-update
   '<gw:void>
   "gnc_progress_dialog_update"
   '((<gnc:ProgressDialog*> progress))
   "Update the progress bar, calling any pending cancel callback.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-finish
   '<gw:void>
   "gnc_progress_dialog_finish"
   '((<gnc:ProgressDialog*> progress))
   "Set the progress dialog to the finished state. The OK button becomes
sensitive and the dialog closes after the user clicks it.")

  (gw:wrap-function
   mod
   'gnc:progress-dialog-destroy
   '<gw:void>
   "gnc_progress_dialog_destroy"
   '((<gnc:ProgressDialog*> progress))
   "Destroy the progess dialog. You must call this function in
order to destroy the dialog. The dialog will not be destroyed
by the user closing the window.")

  (gw:wrap-function
   mod
   'gnc:sx-editor
   '<gw:void>
   "gnc_ui_scheduled_xaction_dialog_create" '()
   "Open the Scheduled Transaction Editor" )

  (gw:wrap-function
   mod
   'gnc:sx-sincelast-create
   '<gw:bool>
   "gnc_ui_sxsincelast_dialog_create" '()
   "Wrapper to open the since-last-run dialog from a book-open hook." )

  (gw:wrap-function
   mod
   'gnc:sx-since-last-run-wrapper
   '<gw:bool>
   "gnc_ui_sxsincelast_guile_wrapper" '(((<gw:m-chars-caller-owned>) bookfile))
   "Wrapper to open the since-last-run dialog from a book-open hook." )
  )
