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
(use-modules (gnucash business-utils))
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-utils" 0)
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(use-modules (ice-9 slib))   ; for 'vicinity' functions
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
  ;; This is complicated because of the need to cater for
  ;; various operating systems; so it takes a fairly heuristic,
  ;; 'best guess' approach.
  ;; If no file is found, returns just 'fname' for use in error messages.
  ;; Note: this has been tested on Linux and Windows Vista so far...
  (let* ((userdir (sub-vicinity (user-vicinity) ".gnucash"))
         (sysdir  (sub-vicinity (sub-vicinity (user-vicinity) "gnucash") "report"))
         (home (or (home-vicinity)
                   (getenv "USERPROFILE")
                   (user-vicinity)
                   "")))
    ; make sure there's a trailing delimiter
    (set! home (sub-vicinity (user-vicinity) home))
    (let ((home-template (in-vicinity (in-vicinity home userdir) fname)))
      (if (access? home-template R_OK)
        home-template
        (or (%search-load-path (in-vicinity sysdir fname))
            fname)))))
  
