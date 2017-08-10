;; eguile-html-utilities.scm
;; Useful HTML-related functions for eguile-based reports.
;; See also eguile-utilities.scm
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

(define-module (gnucash report eguile-html-utilities))

; using all of these seems like overkill --
; not sure which are really required
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (gnucash app-utils))
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/app-utils" 0)

(use-modules (ice-9 regex))  ; for regular expressions
(use-modules (srfi srfi-13)) ; for extra string functions

(define-public (escape-html s1) 
  ;; Convert string s1 to escape HTML special characters < > and &
  ;; i.e. convert them to &lt; &gt; and &amp; respectively.
  ;; Maybe there's a way to do this in one go... (but order is important)
  (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
  (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
  (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))

(define-public (nl->br str)
  ;; Replace newlines with <br>
  (regexp-substitute/global #f "\n" str 'pre "<br>" 'post))

(define-public (nbsp str)
  ;; Replace spaces with &nbsp; (non-breaking spaces)
  ;; (yes, I know <nobr> is non-standard, but webkit splits e.g. "-£40.00" between
  ;; the '-' and the '£' without it.)
  (string-append 
    "<nobr>" 
    (regexp-substitute/global #f " " str 'pre "&nbsp;" 'post) 
    "</nobr>"))

(define-public (empty-cells n)
  ;; Display n empty table cells
  (display (string-repeat "<td class=\"empty\"></td>" n)))

(define-public (indent-cells n)
  ;; Display n empty table cells with width attribute for indenting
  ;; (the &nbsp;s are just there in case CSS isn't working)
  (display (string-repeat "<td min-width=\"32\" class=\"indent\">&nbsp;&nbsp;</td>" n)))

(define-public (negstyle item)
  ;; apply styling for negative amounts
  (string-append "<span class=\"negative\">" item "</span>"))

(define-public (foreignstyle item) 
  ;; apply styling for amount in foreign currency
  (if css?
    (string-append "<span class=\"foreign\">" item "</span>"))
    (string-append "<small><i>" item "</i></small>"))

;; Convert any x into something printable as HTML
(define-public (dump x) (escape-html (object->string x)))
; ddump does the display as well -- for use in eguile reports
; where anything 'display'ed becomes part of the report
(define-public (ddump x) (display (dump x)))

(define-public (fmtmoney curr amt)
  ;; Format a monetary amount in the given currency as HTML
  (nbsp (gnc:monetary->string (gnc:make-gnc-monetary curr amt)))) 

(define-public (display-comm-coll-total comm-coll negative?)
  ;; Display the total(s) of a commodity collector as HTML
  (for-each
    (lambda (pair)
      (display (nbsp (gnc:monetary->string pair))))
    (comm-coll 'format gnc:make-gnc-monetary negative?)))

(define-public (font-name-to-style-info font-name)
  ;;; Convert a font name as return by a font option to CSS format.
  ;;; e.g. "URW Bookman L Bold Italic 12" becomes
  ;;; "font-family: URW Bookman L; font-size: 12pt; font-style: italic; font-weight: bold"
  (let* ((font-family "sans") ; defaults
         (font-weight "normal")
         (font-style  "normal")
         (font-size   "medium")
         (match "")
         ; (thanks to Peter Brett for this regexp and the use of match:prefix)
         (fontre (make-regexp "([[:space:]]+(bold|semi-bold|book|regular|medium|light))?([[:space:]]+(normal|roman|italic|oblique))?([[:space:]]+(condensed))?[[:space:]]+([[:digit:]]+)" regexp/icase))
         (match (regexp-exec fontre font-name)))
    (if match
      (begin
        ; font name parsed OK -- assemble the bits for CSS
        (set! font-family (match:prefix match))
        (if (match:substring match 2)
          ; weight given -- some need translating
          (let ((weight (match:substring match 2)))
            (cond
              ((string-ci=? weight "bold")      (set! font-weight "bold"))
              ((string-ci=? weight "semi-bold") (set! font-weight "600"))
              ((string-ci=? weight "light")     (set! font-weight "200")))))
        (if (match:substring match 4)
          ; style 
          (let ((style (match:substring match 4)))
            (cond
              ((string-ci=? style "italic")  (set! font-style "italic"))
              ((string-ci=? style "oblique") (set! font-style "oblique")))))
        ; ('condensed' is ignored)
        (if (match:substring match 7)
          ; size is in points
          (set! font-size (string-append (match:substring match 7) "pt")))))
    ; construct the result (the order of these is important)
    (string-append "font: " font-weight " " font-style " " font-size " \"" font-family "\";")))

