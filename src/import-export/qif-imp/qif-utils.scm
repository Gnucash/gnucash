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


(use-modules (ice-9 regex))

(define qif-import:paused #f)
(define qif-import:canceled #f)

(define (simple-filter pred list)
  (let ((retval '()))
    (map (lambda (elt)
           (if (pred elt)
               (set! retval (cons elt retval))))
         list)
    (reverse retval)))

(define remove-trailing-space-rexp 
  (make-regexp "^(.*[^ ]+) *$"))

(define remove-leading-space-rexp 
  (make-regexp "^ *([^ ].*)$"))

(define (string-remove-trailing-space str)
  (let ((match (regexp-exec remove-trailing-space-rexp str)))
    (if match
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-leading-space str)
  (let ((match (regexp-exec remove-leading-space-rexp str)))
    (if match 
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-char str char)
  (let ((rexpstr 
         (case char  
           ((#\.) "\\.")
           ((#\^) "\\^")
           ((#\$) "\\$")
           ((#\*) "\\*")
           ((#\+) "\\+")
           ((#\\) "\\\\")
           ((#\?) "\\?")
           (else 
             (make-string 1 char)))))
    (regexp-substitute/global #f rexpstr str 'pre 'post)))


(define (string-char-count str char)
  (length (simple-filter (lambda (elt) (eq? elt char))
                         (string->list str))))


(define (string-replace-char! str old new)
  (let ((rexpstr 
         (if (not (eq? old #\.))
             (make-string 1 old)
             "\\."))
        (newstr (make-string 1 new)))
    (regexp-substitute/global #f rexpstr str 'pre newstr 'post)))

(define (string-to-canonical-symbol str)
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
  (if qif-import:paused
      (begin
        (set! qif-import:paused #f)
        (if progress-dialog
            (gnc-progress-dialog-resume progress-dialog)))
      (begin
        (set! qif-import:paused #t)
        (if progress-dialog
            (gnc-progress-dialog-pause progress-dialog)))))

(define (qif-import:check-pause progress-dialog)
  (while (and qif-import:paused (not qif-import:canceled))
    (gnc-progress-dialog-update progress-dialog)))

