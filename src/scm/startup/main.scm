
(define (gnc:startup)
  (gnc:debug "starting up.")
  #t)

(define (gnc:shutdown exit-status)
  (gnc:debug "Shutdown -- exit-status: " exit-status)
  
  (gnc:hook-run-danglers gnc:*shutdown-hook*)
  (_gnc_shutdown_ exit-status)
  (exit exit-status))

;;;; Now the fun begins.

(gnc:startup)

(if (not (gnc:handle-command-line-args))
    (gnc:shutdown 1))

;;; Now we can load a bunch of files.

(gnc:load "hooks.scm")
(gnc:load "doc.scm")

;;; Load the system and user configs

(if (not (gnc:load-system-config-if-needed))
    (gnc:shutdown 1))

(if (not (gnc:load-user-config-if-needed))
    (gnc:shutdown 1))

(gnc:hook-run-danglers gnc:*startup-hook*)

(if (or (gnc:config-var-value-get gnc:*arg-show-usage*)
         (gnc:config-var-value-get gnc:*arg-show-help*))
     (begin
       (gnc:prefs-show-usage)
       (gnc:shutdown 0)))

(if (not (= (gnucash_lowlev_app_init) 0))
    (gnc:shutdown 0))

(if (pair? gnc:*command-line-files*)
     ;; You can only open single files right now...
     (gnucash_ui_open_file (car gnc:*command-line-files*))
     (gnucash_ui_select_file))

(gnucash_lowlev_app_main)

(gnc:shutdown 0)
