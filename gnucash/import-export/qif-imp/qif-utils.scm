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


(use-modules (srfi srfi-13))

(define qif-import:paused #f)
(define qif-import:canceled #f)

(define (string-remove-trailing-space str)
  (issue-deprecation-warning "string-remove-trailing-space - use string-trim-right")
  (string-trim-right str))

(define (string-remove-leading-space str)
  (issue-deprecation-warning "string-remove-leading-space - use string-trim")
  (string-trim str))

(define (string-remove-char str char)
  (issue-deprecation-warning "string-remove-char - use gnc:string-delete-chars")
  (gnc:string-delete-chars s (list char)))

(define (string-replace-char! str old new)
  (issue-deprecation-warning "string-replace-char! - use gnc:string-replace-char")
  (gnc:string-replace-char str old new))

(define (string-to-canonical-symbol str)
  (issue-deprecation-warning "string-to-canonical-symbol - inline instead")
  (string->symbol 
   (string-downcase
    (string-remove-leading-space
     (string-remove-trailing-space str)))))

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

