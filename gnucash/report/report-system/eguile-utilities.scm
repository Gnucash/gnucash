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

(define-module (gnucash report eguile-utilities))
(use-modules (ice-9 match))

; using all of these seems like overkill -- 
; not sure which are really required
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/app-utils" 0)

(define-public (fmtnumber n)
  ;; Format a number (integer or real) into something printable
  (number->string (if (integer? n) (inexact->exact n) n)))

;; Format gnc-numeric n with as many decimal places as required
(define-public fmtnumeric fmtnumber)

(define-public (gnc-monetary-neg? monetary)
  ; return true if the monetary value is negative
  (negative? (gnc:gnc-monetary-amount monetary)))

;; 'Safe' versions of cdr and cadr that don't crash
;; if the list is empty  (is there a better way?)
(define-public safe-cdr
  (match-lambda
    ((_ . x) x)
    (_ '())))

(define-public safe-cadr
  (match-lambda
    ((_ x . y) x)
    (_ '())))

(define-public (find-file fname)
  ;; Find the file 'fname', and return its full path.
  ;; First look in the user's .gnucash directory.
  ;; Then look in Gnucash's standard report directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  ;; Note: this has been tested on Linux and Windows Vista so far...
  (let ((userpath (gnc-build-userdata-path fname))
        (syspath  (gnc-build-report-path fname)))
    ;; make sure there's a trailing delimiter
    (cond
     ((access? userpath R_OK) userpath)
     ((access? syspath R_OK) syspath)
     (else fname))))

; Define syntax for more readable for loops (the built-in for-each requires an
; explicit lambda and has the list expression all the way at the end).
(export for)
(define-syntax for
  (syntax-rules (for in do)
    ;; Multiple variables and equal number of lists (in
    ;; parenthesis). e.g.:
    ;;   (for (a b) in (lsta lstb) do (display (+ a b)))
    ;; Note that this template must be defined before the
    ;; next one, since the template are evaluated in-order.
    ((for (<var> ...) in (<list> ...) do <expr> ...)
     (for-each (lambda (<var> ...) <expr> ...) <list> ...))

    ;; Single variable and list. e.g.: (for a in lst do (display a))
    ((for <var> in <list> do <expr> ...)
     (for-each (lambda (<var>) <expr> ...) <list>))))
