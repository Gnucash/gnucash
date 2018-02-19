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

(define-module (gnucash utilities))

;; Turn off the scheme compiler's "possibly unbound variable" warnings.
;; In guile 2.0 we get nearly 7500 of them loading the scheme files.
;; This is the default value for auto-compilation-options without "unbound-variable".
;; See module/ice-9/boot-9.scm  */
(if (>= (string->number (major-version)) 2)
    (set! %auto-compilation-options 
          '(#:warnings (arity-mismatch format duplicate-case-datum bad-case-datum))))

(use-modules (gnucash core-utils))

;; Load the srfis (eventually, we should see where these are needed
;; and only have the use-modules statements in those files).
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))

(use-modules (gnucash gnc-module))

(load-from-path "gnucash/string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

;; from utilities.scm
(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export gnc:safe-strcmp) ;; only used by aging.scm atm...

;; Do this stuff very early -- but other than that, don't add any
;; executable code until the end of the file if you can help it.
;; These are needed for a guile 1.3.4 bug
(debug-enable 'backtrace)
(read-enable 'positions)
(debug-set! stack    200000)

;; Initalialize localization, otherwise reports may output
;; invalid characters
(setlocale LC_ALL "")

;;;; Status output functions.

(define (strify items)
  (string-join (map (lambda (x) (format #f "~A" x)) items) ""))

(define (gnc:warn . items)
  (gnc-scm-log-warn (strify items)))

(define (gnc:error . items)
  (gnc-scm-log-error (strify items )))

(define (gnc:msg . items)
  (gnc-scm-log-msg (strify items)))

(define (gnc:debug . items)
  (gnc-scm-log-debug (strify items)))
