(define-module (gnucash gnome-utils))

(use-modules (g-wrapped gw-gnome-utils))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/app-utils" 0)

;; from gnc-menu-extensions.scm
(export gnc:extension-type)
(export gnc:extension-name)
(export gnc:extension-documentation)
(export gnc:extension-path)
(export gnc:extension-script)
(export gnc:make-menu-item)
(export gnc:make-menu)
(export gnc:make-separator)
(export gnc:new-menu-namer)
(export gnc:*add-extension-hook*)

(export gnc:kvp-option-dialog)

(load-from-path "gnc-menu-extensions.scm")

(define (gnc:kvp-option-dialog id-type slots title changed_cb)
  (let* ((options (gnc:make-kvp-options id-type))
	 (optiondb (gnc:option-db-new options))
	 (optionwin (gnc:option-dialog-new #t title)))

    (define (apply-cb)
      (gnc:options-scm->kvp options slots '("options") #t)
      (if changed_cb (changed_cb)))

    (define (close-cb)
      (gnc:option-dialog-destroy optionwin)
      (gnc:option-db-destroy optiondb))

    (gnc:options-kvp->scm options slots '("options"))
    (gnc:option-dialog-set-callbacks optionwin apply-cb close-cb)
    (gnc:option-dialog-build-contents optionwin optiondb)))
