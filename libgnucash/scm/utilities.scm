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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

;; from utilities.scm
(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export addto!)

;; Do this stuff very early -- but other than that, don't add any
;; executable code until the end of the file if you can help it.
;; These are needed for a guile 1.3.4 bug
(debug-enable 'backtrace)
(read-enable 'positions)
(debug-set! stack    200000)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following functions are initialized to log message to tracefile
;; and will be redefined in UI initialization to display dialog
;; messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (gnc:gui-warn str1 str2) (gnc:warn str1))
(define-public (gnc:gui-error str1 str2) (gnc:error str1))
(define-public (gnc:gui-msg str1 str2) (gnc:msg str1))

(define-syntax addto!
  (syntax-rules ()
    ((addto! alist element)
     (set! alist (cons element alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair of utility functions for use with guile-json which requires
;; lists converted vectors to save as json arrays. traverse list
;; converting into vectors, and vice versa.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (traverse-list->vec lst)
  (cond
   ((list? lst) (list->vector (map traverse-list->vec lst)))
   (else lst)))

(define-public (traverse-vec->list vec)
  (cond
   ((vector? vec) (map traverse-vec->list (vector->list vec)))
   (else vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general and efficent string-replace-substring function, based on
;; function designed by Mark H Weaver, core guile developer. avoids
;; string-append which will constantly build new strings. augmented
;; with start and end indices; will selective choose to replace
;; substring if start-idx <= index <= end-idx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (string-replace-substring s substr replacement #:optional
                                   (start 0)
                                   (end (string-length s))
                                   (start-idx #f)
                                   (end-idx #f))
  (let ((substr-length (string-length substr))
        (start-idx (or start-idx 0))
        (end-idx (or end-idx +inf.0)))
    (if (zero? substr-length)
        (error "string-replace-substring: empty substr")
        (let loop ((start start)
                   (i 0)
                   (pieces (list (substring s 0 start))))
          (let ((idx (string-contains s substr start end)))
            (if idx
                (loop (+ idx substr-length)
                      (1+ i)
                      (cons* (if (<= start-idx i end-idx) replacement substr)
                             (substring s start idx)
                             pieces))
                (string-concatenate-reverse (cons (substring s start)
                                                  pieces))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace
;;
;;  Search for all occurrences in string "s1" of string "s2" and
;;  replace them with string "s3".
;;
;;  Example: (gnc:substring-replace "foobarfoobar" "bar" "xyz")
;;           returns "fooxyzfooxyz".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-replace s1 s2 s3)
  (string-replace-substring s1 s2 s3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace-from-to
;;  same as gnc:substring-replace extended by:
;;  start: from which occurrence onwards the replacement shall start
;;  end-after: max. number times the replacement should executed
;;
;;  Example: (gnc:substring-replace-from-to "foobarfoobarfoobar" "bar" "xyz" 2 1)
;;           returns "foobarfooxyzfoobar".
;;
;; start=1 and end-after<=0 will call gnc:substring-replace (replace all)
;; start>1 and end-after<=0 will the replace from "start" until end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-replace-from-to s1 s2 s3 start end-after)
  (string-replace-substring
   s1 s2 s3 0 (string-length s1) (max 0 (1- start))
   (and (positive? end-after) (+ (max 0 (1- start)) (1- end-after)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avoid using strftime, still broken in guile-2.2. see explanation at
;; https://www.mail-archive.com/bug-guile@gnu.org/msg09778.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((strftime-old strftime))
  (set! strftime
    (lambda args
      (gnc:warn "strftime may be buggy. use gnc-print-time64 instead.")
      (apply strftime-old args))))
