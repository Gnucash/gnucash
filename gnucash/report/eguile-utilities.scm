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

(define-public (fmtnumber n)
  ;; Format a number (integer or real) into something printable
  (number->string (if (integer? n) (inexact->exact n) n)))

;; Format gnc-numeric n with as many decimal places as required
(define-public fmtnumeric
  (compose fmtnumber exact->inexact))

(define-public (gnc-monetary-neg? monetary)
  ;; return true if the monetary value is negative
  (issue-deprecation-warning "gnc-monetary-neg? is deprecated")
  (negative? (gnc:gnc-monetary-amount monetary)))

;; 'Safe' versions of cdr and cadr that don't crash
;; if the list is empty  (is there a better way?)
(define-public safe-cdr
  ;; deprecate
  (match-lambda
    ((_ . x) x)
    (_ '())))

(define-public safe-cadr
  ;; deprecate
  (match-lambda
    ((_ x . y) x)
    (_ '())))

; deprecated - use find-stylesheet or find-template instead
(define-public (find-file fname)
  ;; Find the file 'fname', and return its full path.
  ;; First look in the user's .config/gnucash directory.
  ;; Then look in Gnucash's standard report directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  (let* ((stylesheetpath (find-stylesheet fname))
         (templatepath  (find-template fname)))
    ; make sure there's a trailing delimiter
      (issue-deprecation-warning "find-file is deprecated in 4.x. Please use find-stylesheet or find-template instead.")
      (cond
       ((access? stylesheetpath R_OK) stylesheetpath)
       ((access? templatepath R_OK) templatepath)
       (else fname))))

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

;; Define syntax for more readable for loops (the built-in for-each
;; requires an explicit lambda and has the list expression all the way
;; at the end).  Note: deprecated in 4.x, removal in 5.x. this syntax
;; is pythonic rather than lispy, is not recognized by code
;; highlighters, and is not necessary to seasoned schemers.
(export for)
(define-syntax for
  (syntax-rules (for in do)
    ;; Multiple variables and equal number of lists (in
    ;; parenthesis). e.g.:
    ;;   (for (a b) in (lsta lstb) do (display (+ a b)))
    ;; Note that this template must be defined before the
    ;; next one, since the template are evaluated in-order.
    ((for (<var> ...) in (<list> ...) do <expr> ...)
     (begin
       (issue-deprecation-warning "for loops are deprecated. use for-each instead.")
       (for-each (lambda (<var> ...) <expr> ...) <list> ...)))

    ;; Single variable and list. e.g.: (for a in lst do (display a))
    ((for <var> in <list> do <expr> ...)
     (begin
       (issue-deprecation-warning "for loops are deprecated. use for-each instead.")
       (for-each (lambda (<var>) <expr> ...) <list>)))))
