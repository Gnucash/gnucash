(define (add-ofx-menu-item)
  (gnc:add-extension
   (gnc:make-menu-item(N_ "Import OFX/QFX")
		      (N_ "Process an OFX/QFX response file")
		      (list gnc:window-name-main "File" "Import" "")
		      (lambda ()
			(gnc:ofx-import)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-ofx-menu-item)

