(define (add-mt940-menu-item)
  (gnc:add-extension
   (gnc:make-menu-item(N_ "Import MT940")
		      (N_ "Process an MT940 response file")
		      (list gnc:window-name-main "File" "_Import" "")
		      (lambda ()
			(gnc:mt940-import)))))

(gnc:hook-add-dangler gnc:*ui-startup-hook* add-mt940-menu-item)

