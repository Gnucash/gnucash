;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-utils.scm
;;;  string munging and other utility routines 
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

(define-module (gnucash qif-import qif-utils))

;; We do this initialization here because src/gnome isn't a real module.
;; Note: Guile 2 needs to find the symbols from the extension at compile time already
(eval-when (compile load eval expand)
  (load-extension "libgnc-gnome" "scm_init_sw_gnome_module"))

(use-modules (gnucash utilities))
(use-modules (sw_gnome))
(use-modules (srfi srfi-13))

(export qif-import:canceled)
(export qif-import:check-pause)
(export qif-import:log)
(export qif-import:reset-cancel-pause)
(export qif-import:cancel)
(export qif-import:canceled)
(export qif-import:toggle-pause)

(define qif-import:paused #f)
(define qif-import:canceled #f)

(define (qif-import:log progress-dialog proc str)
  (if progress-dialog
      (gnc-progress-dialog-append-log progress-dialog (string-append str "\n"))
      (gnc:warn proc ": " str)))

(define (qif-import:reset-cancel-pause)
  (set! qif-import:paused #f)
  (set! qif-import:canceled #f))

(define (qif-import:cancel)
  (set! qif-import:canceled #t))

(define (qif-import:toggle-pause progress-dialog)
  (cond
   (qif-import:paused
    (set! qif-import:paused #f)
    (when progress-dialog (gnc-progress-dialog-resume progress-dialog)))
   (else
    (set! qif-import:paused #t)
    (when progress-dialog (gnc-progress-dialog-pause progress-dialog)))))

(define (qif-import:check-pause progress-dialog)
  (while (and qif-import:paused (not qif-import:canceled))
    (gnc-progress-dialog-update progress-dialog)))
