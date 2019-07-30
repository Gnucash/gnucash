;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-import.scm
;;;  virtual loader for QIF import facility
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash import-export qif-import))
(use-modules (gnucash utilities)) 
(use-modules (gnucash app-utils))

;; We do this initialization here because src/gnome isn't a real module.
;; Note: Guile 2 needs to find the symbols from the extension at compile time already
(eval-when (compile load eval expand)
  (load-extension "libgnc-gnome" "scm_init_sw_gnome_module"))

(use-modules (sw_gnome))

(use-modules (gnucash gnc-module))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

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
