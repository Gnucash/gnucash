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

(define-module (gnucash main))

(use-modules (ice-9 slib))

(use-modules (g-wrap gw-wct))
(use-modules (g-wrapped gw-core-utils))
(use-modules (g-wrapped gw-gnc))

;; Load the srfis (eventually, we should see where these are needed
;; and only have the use-modules statements in those files).
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))

(use-modules (gnucash gnc-module))
(use-modules (gnucash price-quotes))

(use-modules (ice-9 slib))
(require 'printf)

;; files we can load from the top-level because they're "well behaved"
;; (these should probably be in modules eventually)
(load-from-path "doc.scm")
(load-from-path "main-window.scm")  ;; depends on app-utils (N_, etc.)...
(load-from-path "fin.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

;; from main.scm
(export gnc:version)
(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export string-join)
(export gnc:backtrace-if-exception)
(export gnc:find-file)
(export gnc:find-localized-file)
(export gnc:main)
(export gnc:safe-strcmp) ;; only used by aging.scm atm...

(re-export hash-fold)
(re-export string-split)

;; from command-line.scm
(export gnc:*doc-path*)

;; from doc.scm
(export gnc:find-doc-file)
(export gnc:load-help-topics)

;; from main-window.scm
(export gnc:main-window-save-state)
(export gnc:main-window-properties-cb)

;; from printing/print-check.scm
(export make-print-check-format)
(export gnc:print-check)

;; Get the Makefile.am/configure.in generated variables.
(load-from-path "build-config.scm")

;; Do this stuff very early -- but other than that, don't add any
;; executable code until the end of the file if you can help it.
;; These are needed for a guile 1.3.4 bug
(debug-enable 'debug)
(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

;;(use-modules (ice-9 statprof))

(define (gnc:setup-debugging)
  (if (gnc:debugging?)
      (debug-enable 'backtrace)))

;; various utilities

;; Test for simple-format
(if (not (defined? 'simple-format))
    (begin
      (require 'format)
      (export simple-format)
      (define simple-format format)))

(define (gnc:safe-strcmp a b)
  (cond
   (if (and a b)
       (cond
        ((string<? a b) -1)
        ((string>? a b) 1)
        (else 0))
       (cond
        (a 1)
        (b -1)
        (else 0)))))

(if (not (defined? 'hash-fold))
    (define (hash-fold proc init table)
      (for-each 
       (lambda (bin)
         (for-each 
          (lambda (elt)
            (set! init (proc (car elt) (cdr elt) init)))
          bin))
       (vector->list table))))

(define (string-join lst joinstr)
  ;; This should avoid a bunch of unnecessary intermediate string-appends.
  ;; I'm presuming those are more expensive than cons...
  (if (or (not (list? lst)) (null? lst))
      ""
      (apply string-append
             (car lst)
             (let loop ((remaining-elements (cdr lst)))
               (if (null? remaining-elements)
                   '()
                   (cons joinstr (cons (car remaining-elements)
                                       (loop (cdr remaining-elements)))))))))

(define (string-split str char)
  (let ((parts '())
        (first-char #f))
    (let loop ((last-char (string-length str)))
      (set! first-char (string-rindex str char 0 last-char))
      (if first-char 
          (begin 
            (set! parts (cons (substring str (+ 1 first-char) last-char) 
                              parts))
            (loop first-char))
          (set! parts (cons (substring str 0 last-char) parts))))    
    parts))

;; only used by doc-path
(define (gnc:flatten tree)
  (let ((result '()))
    (let loop ((remaining-items tree))
      (cond
       ((null? remaining-items) #t)
       ((list? remaining-items)
        (loop (car remaining-items))
        (loop (cdr remaining-items)))
       (else
        (set! result (cons remaining-items result)))))
    (reverse! result)))

(define (gnc:backtrace-if-exception proc . args)
  (define (dumper key . args)
    (let ((stack (make-stack #t dumper)))
      (display-backtrace stack (current-error-port))
      (apply display-error stack (current-error-port) args)
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

(define (gnc:warn . items)
  (display "gnucash: [W] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:error . items)
  (display "gnucash: [E] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:msg . items)
  (display "gnucash: [M] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:debug . items)
  (if (gnc:debugging?)
      (begin
        (display "gnucash: [D] ")
        (for-each (lambda (i) (write i)) items)
        (newline))))


;; Set up timing functions

(define gnc:*last-time* (gettimeofday))

(define (gnc:timestamp . stuff)
  (let* ((now (gettimeofday))
         (delta (+ (- (car now) (car gnc:*last-time*))
                   (/ (- (cdr now) (cdr gnc:*last-time*)) 1000000))))
    (gnc:msg stuff "-- Elapsed time: " delta "seconds.")
    (set! gnc:*last-time* now)))

(define (build-path . elements)
  (string-join elements "/"))

(define (gnc:find-file file directories)
  "Find file named 'file' anywhere in 'directories'.  'file' must be a
string and 'directories' must be a list of strings."

  (gnc:debug "gnc:find-file looking for " file " in " directories)

  (do ((rest directories (cdr rest))
       (finished? #f)
       (result #f))
      ((or (null? rest) finished?) result)

    (let ((file-name (build-path (car rest) file)))
      (gnc:debug "  checking for " file-name)
      (if (access? file-name F_OK)
          (begin
            (gnc:debug "found file " file-name)
            (set! finished? #t)
            (set! result file-name))))))

(define (gnc:find-localized-file file base-directories)
  ;; Find file in path in base directories, or in any localized subdir
  ;; thereof.

  (define (locale-prefixes)
    ;; Mac OS X. 10.1 and earlier don't have LC_MESSAGES. Fall back to
    ;; LC_ALL for those systems.
    (let* ((locale (or (false-if-exception (setlocale LC_MESSAGES))
		       (setlocale LC_ALL)))
           (strings (cond ((not (string? locale)) '())
                          ((equal? locale "C") '())
                          ((<= (string-length locale) 4) (list locale))
                          (else (list (substring locale 0 2)
                                      (substring locale 0 5)
                                      locale)))))
      (reverse (cons "C" strings))))

  (let loop ((prefixes (locale-prefixes))
             (dirs base-directories))
    (if (null? dirs)
        #f
        (or (gnc:find-file file (map (lambda (prefix)
                                       (build-path (car dirs) prefix))
                                     prefixes))
            (gnc:find-file file (list (car dirs)))
            (loop prefixes (cdr dirs))))))

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

(define (gnc:main)

  ;;  (statprof-reset 0 50000) ;; 20 times/sec
  ;;  (statprof-start)

  ;; Now the fun begins.
  (gnc:debug "starting up (1).")
  (gnc:setup-debugging)

  ;; Now we can load a bunch of files.
  (load-from-path "command-line.scm") ;; depends on app-utils (N_, etc.)...

  (gnc:initialize-config-vars) ;; in command-line.scm
  ;; handle unrecognized command line args
  (if (not (gnc:handle-command-line-args))
      (gnc:shutdown 1))

  (gnc:hook-remove-dangler gnc:*book-closed-hook* 
                           gnc:main-window-book-close-handler)
  (gnc:hook-add-dangler gnc:*book-closed-hook* 
                        gnc:main-window-book-close-handler)
  
  (load-from-path "printing/print-check.scm") ;; depends on simple-obj...
  
  (gnc:update-splash-screen (_ "Checking Finance::Quote..."))
  (gnc:price-quotes-install-sources)  
  
  (gnc:report-menu-setup)
  
  ;; the Welcome to GnuCash "extravaganza" report
  (gnc:add-extension 
   (gnc:make-menu-item 
    (N_ "Welcome Sample Report")
    (N_ "Welcome-to-GnuCash report screen")
    (list gnc:menuname-reports gnc:menuname-utility "")
    (lambda (window)
      (gnc:main-window-open-report (gnc:make-welcome-report) window))))

  ;;return to C
  )
