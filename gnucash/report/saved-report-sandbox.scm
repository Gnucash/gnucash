;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saved-report-sandbox.scm : evaluate saved-report code safely
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The saved-reports file is Scheme code written by
;; gnc:report-template-serialize-internal. Previously it was run with
;; plain eval, so a tampered file (or a report option string that
;; escapes its quoting) could run arbitrary code with the user's
;; privileges.
;;
;; The code here instead:
;;  1. reads the text as data with `read' (reader-eval is refused), one
;;     top-level form at a time;
;;  2. evaluates each form in a fresh module that contains only the
;;     side-effect-free core of Guile (ice-9 sandbox's all-pure-bindings)
;;     plus an explicit allowlist of GnuCash procedures supplied by the
;;     caller, with a CPU-time and allocation limit per form.
;;
;; Anything not on the allowlist (system, open-file, eval, @@, module
;; access, ...) is simply unbound inside the sandbox.
;;
;; This module deliberately depends on nothing from GnuCash so that it
;; can be unit tested on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report saved-report-sandbox)
  #:use-module (ice-9 sandbox)
  #:use-module (system syntax)
  #:export (make-saved-report-sandbox
            safe-set!
            sandbox-eval-string
            sandbox-load-file))

;; Limits applied to each top-level form while it is *evaluated*. A
;; saved report only defines a template at load time, so these are
;; generous but finite.
(define %time-limit 30)                 ;seconds (wall clock)
(define %allocation-limit #e50e6)       ;bytes

;; Guile's sandbox leaves out `set!' because assigning to an *imported*
;; binding would change it for the whole process (e.g. `(set! car ...)').
;; The serialized embedded-report code needs `set!' on a local variable,
;; so provide a version that only accepts lexically bound variables.
;; (Guile reports `displaced-lexical' for a local `define' in the body
;; currently being expanded; globals and imports are `global'.)
(define-syntax safe-set!
  (lambda (x)
    (syntax-case x ()
      ((_ id expr)
       (identifier? #'id)
       (call-with-values (lambda () (syntax-local-binding #'id))
         (lambda (type . rest)
           (if (memq type '(lexical displaced-lexical))
               #'(set! id expr)
               (syntax-violation 'set! "may only assign to a local variable"
                                 x #'id))))))))

;; bindings: an alist of (symbol . value) that will be visible, in
;; addition to the pure Guile core, to code evaluated in the sandbox.
(define (make-saved-report-sandbox bindings)
  (let ((m (make-sandbox-module
            (cons '((gnucash report saved-report-sandbox) (safe-set! . set!))
                  all-pure-bindings))))
    (for-each (lambda (b) (module-define! m (car b) (cdr b))) bindings)
    m))

(define (exception->string key args)
  (call-with-output-string
    (lambda (port) (print-exception port #f key args))))

;; `read' never evaluates anything: Guile does not honour '#.(...)' and
;; unknown '#' syntaxes raise an error, which is handled like any other
;; unreadable input.
(define (read-datum port) (read port))

;; Evaluate every form in PORT inside MODULE. When HANDLER is #f, the
;; first error propagates to the caller. Otherwise HANDLER is called
;; with a message for each form that fails to evaluate and processing
;; continues with the next form. Returns the value of the last form,
;; or #f if there was none / it failed.
(define (eval-port port module handler)
  (define (guarded thunk)
    (if handler
        (catch #t thunk
          (lambda (key . args)
            (handler (exception->string key args))
            #f))
        (thunk)))
  (let loop ((last #f) (index 1))
    (let ((form (if handler
                    ;; An unbalanced file cannot be resynchronised, so a
                    ;; read error ends the file.
                    (catch #t
                      (lambda () (read-datum port))
                      (lambda (key . args)
                        (handler (string-append
                                  "unreadable input, ignoring the rest of the file: "
                                  (exception->string key args)))
                        the-eof-object))
                    (read-datum port))))
      (if (eof-object? form)
          last
          (loop (guarded
                 (lambda ()
                   (eval-in-sandbox form
                                    #:module module
                                    ;; the module is reused for every form
                                    #:sever-module? #f
                                    #:time-limit %time-limit
                                    #:allocation-limit %allocation-limit)))
                (1+ index))))))

(define the-eof-object (read (open-input-string "")))

;; Evaluate the forms in STR; errors propagate (as eval-string did).
(define (sandbox-eval-string str module)
  (call-with-input-string str
    (lambda (port) (eval-port port module #f))))

;; Evaluate the forms in the file FILENAME, reporting each failing form
;; via (WARN message) but loading all the others. Returns #f only if
;; the file could not be read at all.
(define (sandbox-load-file filename module warn)
  (catch #t
    (lambda ()
      (call-with-input-file filename
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (eval-port port module
                     (lambda (msg)
                       (warn (string-append filename ": " msg))))
          #t)))
    (lambda (key . args)
      (warn (string-append "Couldn't read " filename ": "
                           (exception->string key args)))
      #f)))
