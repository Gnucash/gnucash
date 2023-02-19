;; eguile-utilities.scm
;; Useful functions for eguile-based reports.
;; See also eguile-html-utilities.scm
;; Version 0.01
;;
;; (c) 2009 Chris Dennis chris@starsoftanalysis.co.uk
;;
;; $Author: chris $ $Date: 2009/07/27 15:51:02 $ $Revision: 1.28 $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

(define-module (gnucash eguile eguile-utilities))

(use-modules (ice-9 match))
; using all of these seems like overkill -- 
; not sure which are really required
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(use-modules (gnucash report))

;; Format gnc-numeric n with decimal places, or exact fraction
(define-public fmtnumeric
  (lambda (n) (xaccPrintAmount n (gnc-default-print-info #f))))

(define (find-internal ftype fname)
  ;; Find the file fname', and return its full path.
  ;; First look in the user's .config/gnucash directory.
  ;; Then look in Gnucash' gnucash/reports/'ftype' directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  (let* ((userpath (gnc-build-userdata-path fname))
         (syspath  (gnc-build-reports-path (string-append ftype "/" fname))))
    (cond
     ((access? userpath R_OK) userpath)
     ((access? syspath R_OK) syspath)
     (else fname))))

(define-public (find-stylesheet fname)
  ;; Find the stylesheet 'fname', and return its full path.
  ;; First look in the user's .config/gnucash directory.
  ;; Then look in Gnucash' gnucash/reports/stylesheets directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  (find-internal "stylesheets" fname))

(define-public (find-template fname)
  ;; Find the template 'ftype'/'fname', and return its full path.
  ;; First look in the user's .config/gnucash directory.
  ;; Then look in Gnucash' gnucash/reports/templates directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  (find-internal "templates" fname))

