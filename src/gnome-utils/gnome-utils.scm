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

(load-from-path "gnc-menu-extensions.scm")
