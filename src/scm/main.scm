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

(define-module (gnucash main)
  #:use-module (gnucash printf))

;; This is to silence warnings with guile-1.8:
(if (and (>= (string->number (major-version)) 1) 
         (>= (string->number (minor-version)) 8))
    (default-duplicate-binding-handler 'last))

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

;; files we can load from the top-level because they're "well behaved"
;; (these should probably be in modules eventually)
(load-from-path "string")
(load-from-path "fin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

;; from main.scm
(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export gnc:backtrace-if-exception)
(export gnc:safe-strcmp) ;; only used by aging.scm atm...

;; Get the Makefile.am/configure.in generated variables.
(load-from-path "build-config")

;; Do this stuff very early -- but other than that, don't add any
;; executable code until the end of the file if you can help it.
;; These are needed for a guile 1.3.4 bug
(debug-enable 'backtrace)
(read-enable 'positions)

;; These options should only be set for guile < 2.0
;; 'debug (deprecated and unused since guile 2)
;; maxdepth (removed since guile 2)
(cond-expand
  (guile-2 )
  (else
    (debug-enable 'debug)
    (debug-set! maxdepth 100000)))
(debug-set! stack    200000)

;; Initalialize localization, otherwise reports may output
;; invalid characters
(setlocale LC_ALL "")

;;(use-modules (ice-9 statprof))

;; various utilities

(define (gnc:safe-strcmp a b)
  (if (and a b)
      (cond
       ((string<? a b) -1)
       ((string>? a b) 1)
       (else 0))
      (cond
       (a 1)
       (b -1)
       (else 0))))

(define (gnc:backtrace-if-exception proc . args)
  (define (dumper key . args)
    (let ((stack (make-stack #t dumper)))
      ;; Send debugging output to the console.
      (display-backtrace stack (current-error-port))
      (apply display-error stack (current-error-port) args)

      ;; Send debugging output to the log.
      (if (defined? 'gnc:warn)
          (let ((string-port (open-output-string)))
            (display-backtrace stack string-port)
            (apply display-error stack string-port args)
            (gnc:warn (get-output-string string-port))
            (close-output-port string-port)))

      (throw 'ignore)))
  
  (catch 
      'ignore
    (lambda () 
      (lazy-catch #t 
                  (lambda () (apply proc args))
                  dumper))
    (lambda (key . args)
      #f)))

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

;; Set up timing functions

(define gnc:*last-time* (gettimeofday))

(define (gnc:timestamp . stuff)
  (let* ((now (gettimeofday))
         (delta (+ (- (car now) (car gnc:*last-time*))
                   (/ (- (cdr now) (cdr gnc:*last-time*)) 1000000))))
    (gnc:msg stuff "-- Elapsed time: " delta "seconds.")
    (set! gnc:*last-time* now)))

(define (gnc:shutdown exit-status)
  (gnc:debug "Shutdown -- exit-status: " exit-status)
  (exit exit-status)) ;; Temporary Stub until command-line.scm dies

(define (gnc:strip-path path)
  (let* ((parts-in (string-split path #\/))
	 (parts-out '()))

    ;; Strip out "." and ".." components
    ;; Strip out // components
    (for-each
     (lambda (part)
       (cond ((string=? part ".") #f)
	     ((string=? part "..") (set! parts-out (cdr parts-out)))
	     ((and (string-null? part) (not (= (length parts-out) 0))) #f)
	     (else (set! parts-out (cons part parts-out)))))
     parts-in)

    ;; Put it back together
    (string-join (reverse parts-out) "/")))
