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

(define-module (gnucash gnome-utils))

(eval-when (compile load eval expand)
  (load-extension "libgnc-gnome-utils" "scm_init_sw_gnome_utils_module"))

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))

(load-and-reexport (sw_gnome_utils)
                   (gnucash gnome-utils gnc-menu-extensions))

;; this function will receive 1 boolean argument, and can be used for
;; any UI init/shutdown routines. For now it will set the
;; gnc:ui-warn/error/msg tracefile routines to display dialog messages
;; in addition to tracefile logging.
(define-public gnc:set-ui-status
  (let ((save-warn gnc:gui-warn)
        (save-error gnc:gui-error)
        (save-msg gnc:gui-msg))
    (lambda (status)
      (cond
       (status
        (set! gnc:gui-warn (lambda (constr guistr)
                             (save-warn constr guistr)
                             (gnc-warning-dialog '() guistr)))
        (set! gnc:gui-error (lambda (constr guistr)
                              (save-error constr guistr)
                              (gnc-error-dialog '() guistr)))
        (set! gnc:gui-msg (lambda (constr guistr)
                            (save-msg constr guistr)
                            (gnc-info-dialog '() guistr))))
       (else
        (set! gnc:gui-warn save-warn)
        (set! gnc:gui-error save-error)
        (set! gnc:gui-msg save-msg))))))
