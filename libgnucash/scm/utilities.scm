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

(define-syntax addto!
  (syntax-rules ()
    ((addto! alist element)
     (set! alist (cons element alist)))))

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
  (let ((s2len (string-length s2)))
    (let loop ((start1 0)
               (i (string-contains s1 s2)))
      (if i
          (string-append (substring s1 start1 i)
                         s3
                         (loop (+ i s2len) (string-contains s1 s2 (+ i s2len))))
          (substring s1 start1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace-from-to
;;  same as gnc:substring-replace extended by:
;;  start: from which occurrence onwards the replacement shall start
;;  end-after: max. number times the replacement should executed
;;
;;  Example: (gnc:substring-replace-from-to "foobarfoobarfoobar" "bar" "xyz" 2 2)
;;           returns "foobarfooxyzfoobar".
;;
;; start=1 and end-after<=0 will call gnc:substring-replace (replace all)
;; start>1 and end-after<=0 will the replace from "start" until end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-replace-from-to s1 s2 s3 start end-after)
  (let (
         (s2len (string-length s2))
       )

    ;; if start<=0 and end<=0 => don't do anything

    (if (and
          (<= start 0)
          (<= end-after 0)
        )
      s1
    )

    ;; else
    (begin

      ;; normalize start
      (if (= start 0)
        (set! start 1)
      )
      ;; start=1 and end<=0 => replace all
      ;; call gnc:substring-replace for that
      (if (and (= start 1) (<= end-after 0))
        (gnc:substring-replace s1 s2 s3)

        ;; else
        (begin
          (let loop (
                      (start1 0)
                      (i (string-contains s1 s2))
                    )
            (if i
              (begin
                (set! start (- start 1))
                (if (or
                        (> start 0)
                        (and (> end-after 0)
                             (<= (+ end-after start) 0)
                        )
                    )
                  (string-append
                    (substring s1 start1 i)
                    s2 ;; means: do not change anything
                    (loop (+ i s2len) (string-contains s1 s2 (+ i s2len)))
                  )
                  (string-append
                    (substring s1 start1 i)
                    s3
                    (loop (+ i s2len) (string-contains s1 s2 (+ i s2len)))
                  )
                )
              )
              ;; else
              (substring s1 start1)
            )
          )
        )
      )
    )
  )
)
