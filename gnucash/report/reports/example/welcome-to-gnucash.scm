;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; welcome-to-gnucash.scm : very minimalistic sampe report
;;                          can be used as skeleton to make new reports
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports example welcome-to-gnucash))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils)) ; for gnc:version and (G_ ...)
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define (options)
  (gnc:new-options))

(define (renderer report-obj)
  (let ((doc (gnc:make-html-document)))
    (gnc:html-document-add-object!
     doc
     (gnc:make-html-text
      (gnc:html-markup-h3
       (format #f (G_ "Welcome to GnuCash ~a !")
               gnc:version))
      (gnc:html-markup-p
       (format #f (G_ "GnuCash ~a has lots of nice features. Here are a few.")
               gnc:version))))
    doc))

(gnc:define-report
 'name (N_ "Welcome to GnuCash")
 'version 1
 'report-guid "65135608f2014c6ca8412793a8cdf169"
 'menu-path (list gnc:menuname-example)
 'options-generator options
 'renderer renderer)
