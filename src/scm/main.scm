
(define (gnc:startup)
  (gnc:debug "starting up.")
  (if (not (gnc:handle-command-line-args))
      (gnc:shutdown 1))
  
  ;; Now we can load a bunch of files.
  
  (gnc:load "doc.scm")
  (gnc:load "extensions.scm")      ; Should this be here or somewhere else?
  (gnc:load "text-export.scm")
  (gnc:load "importqif.scm")
  (gnc:load "test.scm")
  
  ;; Load the system and user configs
  (if (not (gnc:load-system-config-if-needed))
      (gnc:shutdown 1))
  
  (if (not (gnc:load-user-config-if-needed))
      (gnc:shutdown 1))

  (gnc:hook-run-danglers gnc:*startup-hook*)
  
  (if (or (gnc:config-var-value-get gnc:*arg-show-usage*)
          (gnc:config-var-value-get gnc:*arg-show-help*))
      (begin
        (gnc:prefs-show-usage)
        (gnc:shutdown 0))))

(define (gnc:shutdown exit-status)
  (gnc:debug "Shutdown -- exit-status: " exit-status)

  (cond ((gnc:ui-is-running?)
	 (if (not (gnc:ui-is-terminating?))
	     (begin
	       (gnc:hook-run-danglers gnc:*ui-shutdown-hook*)
	       (gnc:ui-shutdown))))

	(else
	 (gnc:hook-run-danglers gnc:*shutdown-hook*)
	 (gnc:ui-destroy)
	 (exit exit-status))))

(define (gnc:ui-finish)
  (gnc:debug "UI Shutdown hook.")

  (gnc:file-query-save)
  (gnc:file-quit))

(define (gnc:main)

  ;; Now the fun begins.

  (gnc:startup)

  (if (not (= (gnc:lowlev-app-init) 0))
      (gnc:shutdown 0))
  
  (if (pair? gnc:*command-line-files*)
      ;; You can only open single files right now...
      (gnc:ui-open-file (car gnc:*command-line-files*)))

  (gnc:hook-add-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)

  (gnc:ui-main)

  (gnc:hook-remove-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)
  
  (gnc:shutdown 0))
