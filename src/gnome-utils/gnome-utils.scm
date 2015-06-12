(define-module (gnucash gnome-utils))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (load-extension "libgncmod-gnome-utils" "scm_init_sw_gnome_utils_module")))
  (else ))
(use-modules (sw_gnome_utils))
(gnc:module-load "gnucash/app-utils" 0)

;; from gnc-menu-extensions.scm
(export gnc:extension-type)
(export gnc:extension-name)
(export gnc:extension-guid)
(export gnc:extension-documentation)
(export gnc:extension-path)
(export gnc:extension-script)
(export gnc:make-menu-item)
(export gnc:make-menu)
(export gnc:make-separator)

(export gnc:kvp-option-dialog)

(load-from-path "gnc-menu-extensions")

