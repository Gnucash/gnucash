(define (gnc:startup)
  (gnc:debug "starting up.")
  (if (not (gnc:handle-command-line-args))
      (gnc:shutdown 1))

  ;; Load the srfis
  (gnc:load "srfi/srfi-8.guile.scm")
  (gnc:load "srfi/srfi-1.unclear.scm")
  (gnc:load "srfi/srfi-1.r5rs.scm")

  (gnc:load "depend.scm")

  ;; Now we can load a bunch of files.

  (gnc:depend "doc.scm")
  (gnc:depend "extensions.scm")
  (gnc:depend "text-export.scm")
  (gnc:depend "importqif.scm")
  (gnc:depend "report.scm")

  ;; FIXME: These do not belong here, but for now, we're putting them
  ;; here.  Later we need a generalization of gnc:load that takes a
  ;; path specifier, and then we should have a gnc:*report-path* that
  ;; determines where we look to load report files.  For now, though,
  ;; I just want to get things going...
  ;;
  ;; Just load these since we might want to redefine them on the fly
  ;; and we're going to change this mechanism anyway...
  (let 
      ((repdir 
	(opendir (string-append gnc:_share-dir-default_ "/scm/report"))))
    (while (let ((cf (readdir repdir)))
	     (if (string? cf)
		 (if (and 
		      (not (directory? cf))
		      (> (string-length cf) 4))
		     (if (string=? (substring cf (- (string-length cf) 4) 
					      (string-length cf)) ".scm")
			 (gnc:load (string-append "report/" cf)))))
             (string? cf)) ()))

  ;; Load the system configs
  (if (not (gnc:load-system-config-if-needed))
      (gnc:shutdown 1))

  ;; Load the user configs
  (gnc:load-user-config-if-needed)

  ;; Clear the change flags caused by loading the configs
  (gnc:global-options-clear-changes)

  (gnc:hook-run-danglers gnc:*startup-hook*)

  (if (gnc:config-var-value-get gnc:*arg-show-version*)
      (begin
        (gnc:prefs-show-version)
        (gnc:shutdown 0)))

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
	 (gnc:ui-destroy)
	 (gnc:hook-run-danglers gnc:*shutdown-hook*)
	 (exit exit-status))))

(define (gnc:ui-finish)
  (gnc:debug "UI Shutdown hook.")

  (gnc:ui-destroy-all-subwindows)
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

  ;; add a hook to save the user configs on shutdown
  (gnc:hook-add-dangler gnc:*shutdown-hook* gnc:save-global-options)

  (gnc:hook-add-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)

  (gnc:ui-main)

  (gnc:hook-remove-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)

  (gnc:shutdown 0))
