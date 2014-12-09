;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-import.scm
;;;  virtual loader for QIF import facility
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash import-export qif-import))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.

;; We do this initialization here because src/gnome isn't a real module.
;; Note: Guile 2 needs to find the symbols from the extension at compile time already
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (load-extension "libgnc-gnome" "scm_init_sw_gnome_module")))
  (else
    (load-extension "libgnc-gnome" "scm_init_sw_gnome_module")))

(use-modules (sw_gnome))

(use-modules (gnucash gnc-module))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

(debug-enable 'debug)
(debug-enable 'backtrace)

(gnc:module-load "gnucash/engine" 0)
(gnc:module-load "gnucash/app-utils" 0)
(gnc:module-load "gnucash/gnome-utils" 0)

(load-from-path "qif-import/qif-objects")      ;; class definitions
(load-from-path "qif-import/qif-parse")        ;; string-to-value
(load-from-path "qif-import/qif-utils")
(load-from-path "qif-import/qif-file")         ;; actual file reading
(load-from-path "qif-import/qif-dialog-utils") ;; build displays
(load-from-path "qif-import/qif-guess-map")    ;; build acct mappings
(load-from-path "qif-import/qif-to-gnc")       ;; conv QIF xtns to GNC
(load-from-path "qif-import/qif-merge-groups") ;; merge into user's acct

(export make-qif-file)
(export make-ticker-map)
(export qif-import:get-all-accts)
(export qif-import:fix-from-acct)
(export qif-import:any-new-accts?)
(export qif-import:update-security-hash)
(export qif-import:refresh-match-selection)
(export qif-import:save-map-prefs)
(export qif-import:load-map-prefs)
(export qif-import:qif-to-gnc)
(export qif-import:qif-to-gnc-undo)
(export qif-import:reset-cancel-pause)
(export qif-import:cancel)
(export qif-import:toggle-pause)

(export qif-map-entry:gnc-name)
(export qif-map-entry:set-gnc-name!)
(export qif-map-entry:clone)
(export qif-map-entry:qif-name)
(export qif-map-entry:new-acct?)

(export qif-file:read-file)
(export qif-file:parse-fields)
(export qif-file:parse-fields-results)
(export qif-file:check-from-acct)
(export qif-file:reparse-dates)
(export qif-file:check-from-acct)
(export qif-file:path-to-accountname)
(export qif-file:path)

(export qif-dialog:qif-file-loaded?)
(export qif-dialog:unload-qif-file)
(export qif-dialog:make-account-display)
(export qif-dialog:make-category-display)
(export qif-dialog:make-memo-display)

(export gnc:account-tree-find-duplicates)
(export gnc:account-tree-catenate-and-merge)
(export gnc:prune-matching-transactions)
