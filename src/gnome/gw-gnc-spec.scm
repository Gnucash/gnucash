(define-module (g-wrapped gw-gnc-spec))
(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((ws (gw:new-wrapset "gw-gnc")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-gnome-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-gnc))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <glib.h>\n"
      "#include <gnc-ui.h>\n"
      "#include <gnc-ui-util.h>\n"
      "#include <gnc-menu-extensions.h>\n"
      "#include <gnc-date.h>\n"
      "#include <guile-util.h>\n"
      "#include <gnc-engine.h>\n"
      "#include <gnc-commodity.h>\n"
      "#include <gnc-numeric.h>\n"
      "#include <dialog-totd.h>\n"
      "#include <gnc-gui-query.h>\n"
      "#include <dialog-new-user.h>\n"
      "#include <dialog-progress.h>\n"
      "#include <dialog-commodity.h>\n"
      "#include <druid-hierarchy.h>\n"
      "#include <top-level.h>\n"
      "#include <gnc-html.h>\n"
      "#include <gnc-main-window.h>\n"
      "#include <gnc-window.h>\n"
      "#include <gnc-plugin-account-tree.h>\n"
      "#include <gnc-splash.h>\n"
      "#include <dialog-scheduledxaction.h>\n"
      "#include <dialog-sxsincelast.h>\n" )))

  (gw:wrap-function
   ws
   'gnc:ui-is-running?
   '<gw:bool>
   "gnucash_ui_is_running"
   '()
   "Predicate to determine if the UI is running.")

  (gw:wrap-function
   ws
   'gnc:ui-is-terminating?
   '<gw:bool>
   "gnucash_ui_is_terminating"
   '()
   "Predicate to determine if the UI is in the process of terminating.")

  (gw:wrap-function
   ws
   'gnc:new-user-dialog
   '<gw:void>
   "gnc_ui_new_user_dialog"
   '()
   "Show the new user dialog.")

  (gw:wrap-function
   ws
   'gnc:start-ui-event-loop
   '<gw:int>
   "gnc_ui_start_event_loop"
   '()
   "Start the UI event loop.")

  (gw:wrap-function
   ws
   'gnc:gui-init
   '<gw:scm>
   "gnc_gui_init"
   '((<gw:scm> command-line))
   "Initialize the remaining parts of the lower level ui. Returns main-window and remaining command line.")

  (gw:wrap-function
   ws
   'gnc:gui-init-splash
   '<gw:scm>
   "gnc_gui_init_splash"
   '((<gw:scm> command-line))
   "Initialize the lower level ui parts and put up the splash screen. Returns remaining command line.")

  (gw:wrap-function
   ws
   'gnc:update-splash-screen
   '<gw:void>
   "gnc_update_splash_screen"
   '(((<gw:mchars> caller-owned const) string))
   "Update the progress box on the splash screen dialog.")

  (gw:wrap-function
   ws
   'gnc:destroy-splash-screen
   '<gw:void>
   "gnc_destroy_splash_screen"
   '()
   "Destroy the splash screen dialog.")

  (gw:wrap-function
   ws
   'gnc:gui-shutdown
   '<gw:void>
   "gnc_gui_shutdown"
   '()
   "Shutdown the UI.")

  (gw:wrap-function
   ws
   'gnc:gui-destroy
   '<gw:void>
   "gnc_gui_destroy"
   '()
   "Destroy the UI.")

  (gw:wrap-as-wct ws
                  '<gnc:ProgressDialog*>
                  "GNCProgressDialog *" "const GNCProgressDialog *")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-new
   '<gnc:ProgressDialog*>
   "gnc_progress_dialog_new"
   '((<gnc:UIWidget> parent) (<gw:bool> use_ok_button))
   "Create and return a progress dialog. The parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-title
   '<gw:void>
   "gnc_progress_dialog_set_title"
   '((<gnc:ProgressDialog*> progress)
     ((<gw:mchars> caller-owned const) title))
   "Set the title of 'progress' to 'title'.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-heading
   '<gw:void>
   "gnc_progress_dialog_set_heading"
   '((<gnc:ProgressDialog*> progress)
     ((<gw:mchars> caller-owned const) heading))
   "Set the heading of 'progress' to 'heading'.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-limits
   '<gw:void>
   "gnc_progress_dialog_set_limits"
   '((<gnc:ProgressDialog*> progress)
     (<gw:float> min)
     (<gw:float> max))
   "Set the mininum and maximum range of 'progress'.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-activity-mode
   '<gw:void>
   "gnc_progress_dialog_set_activity_mode"
   '((<gnc:ProgressDialog*> progress)
     (<gw:bool> activity-mode))
   "Set the mininum and maximum range of 'progress'.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-cancel-scm-func
   '<gw:void>
   "gnc_progress_dialog_set_cancel_scm_func"
   '((<gnc:ProgressDialog*> progress)
     (<gw:scm> cancel_func))
   "Set the guile cancel callback function. This callback is invoked
with no arguments when the user hits the cancel button. If the callback
returns #t, the dialog is closed, but not destroyed.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-set-value
   '<gw:void>
   "gnc_progress_dialog_set_value"
   '((<gnc:ProgressDialog*> progress) (<gw:float> value))
   "Set the value of the progress dialog to 'value'.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-update
   '<gw:void>
   "gnc_progress_dialog_update"
   '((<gnc:ProgressDialog*> progress))
   "Update the progress bar, calling any pending cancel callback.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-finish
   '<gw:void>
   "gnc_progress_dialog_finish"
   '((<gnc:ProgressDialog*> progress))
   "Set the progress dialog to the finished state. The OK button becomes
sensitive and the dialog closes after the user clicks it.")

  (gw:wrap-function
   ws
   'gnc:progress-dialog-destroy
   '<gw:void>
   "gnc_progress_dialog_destroy"
   '((<gnc:ProgressDialog*> progress))
   "Destroy the progess dialog. You must call this function in
order to destroy the dialog. The dialog will not be destroyed
by the user closing the window.")

)
