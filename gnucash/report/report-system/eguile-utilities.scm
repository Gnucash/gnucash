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

; using all of these seems like overkill -- 
; not sure which are really required
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/app-utils" 0)
; Syncase is deprecated and redundant in guile 2
(cond-expand
  (guile-2 )
  (else
    (use-modules (ice-9 syncase)))) ; for define-syntax


;(use-modules (srfi srfi-13)) ; for extra string functions

(define-public (fmtnumber n)
  ;; Format a number (integer or real) into something printable
  (number->string (if (integer? n) 
                    (inexact->exact n) 
                    n)))

(define-public (fmtnumeric n)
  ;; Format gnc-numeric n with as many decimal places as required
  (fmtnumber (gnc-numeric-to-double n)))

(define-public (gnc-monetary-neg? monetary)
  ; return true if the monetary value is negative
  (gnc-numeric-negative-p (gnc:gnc-monetary-amount monetary)))

(define-public (string-repeat s n)
  ;; return a string made of n copies of string s
  ;; (there's probably a better way)
  (let ((s2 ""))
    (do ((i 1 (1+ i))) ((> i n))
      (set! s2 (string-append s2 s)))
    s2))

;; 'Safe' versions of cdr and cadr that don't crash
;; if the list is empty  (is there a better way?)
(define-public (safe-cdr l)
  (if (null? l)
    '()
    (cdr l)))
(define-public (safe-cadr l)
  (if (null? l)
    '()
    (if (null? (cdr l))
      '()
      (cadr l))))

(define-public (find-file fname)
  ;; Find the file 'fname', and return its full path.
  ;; First look in the user's .gnucash directory.
  ;; Then look in Gnucash's standard report directory.
  ;; If no file is found, returns just 'fname' for use in error messages.
  ;; Note: this has been tested on Linux and Windows Vista so far...
  (let* ((userpath (gnc-build-dotgnucash-path fname))
         (syspath  (gnc-build-report-path fname)))
    ; make sure there's a trailing delimiter
      (if (access? userpath R_OK)
        userpath
        (if (access? syspath R_OK)
          syspath
          fname))))

; Define syntax for more readable for loops (the built-in for-each requires an
; explicit lambda and has the list expression all the way at the end).
(define-syntax for
  (syntax-rules (for in => do hash)
		; Multiple variables and and equal number of lists (in
		; parenthesis). e.g.:
		;
		;   (for (a b) in (lsta lstb) do (display (+ a b)))
		;
		; Note that this template must be defined before the
		; next one, since the template are evaluated in-order.
                ((for (<var> ...) in (<list> ...) do <expr> ...)
                 (for-each (lambda (<var> ...) <expr> ...) <list> ...))
		; Single variable and list. e.g.:
		;
		; (for a in lst do (display a))
                ((for <var> in <list> do <expr> ...)
                 (for-each (lambda (<var>) <expr> ...) <list>))
		; Iterate over key & values in a hash. e.g.:
		;
		; (for key => value in hash do (display (* key value)))
                ((for <key> => <value> in <hash> do <expr> ...)
		 ; We use fold to iterate over the hash (instead of
		 ; hash-for-each, since that is not present in guile
		 ; 1.6).
                 (hash-fold (lambda (<key> <value> accum) (begin <expr> ... accum)) *unspecified* <hash>))
                ))
(export for)
