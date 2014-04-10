(define (add-log-replay-menu-item)
  (gnc:add-extension
   (gnc:make-menu-item(N_ "Replay GnuCash .log file")
		      (N_ "Replay a gnucash log file after a crash.  This cannot be undone.")
		      (list gnc:window-name-main "File" "_Import" "")
		      (lambda ()
			(gnc:log-replay)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-log-replay-menu-item)

