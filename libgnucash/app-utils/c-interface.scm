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

(define (gnc:call-with-error-handling cmd args)
  (let ((captured-stack #f)
        (captured-error #f)
        (result #f))
    (catch #t
      (lambda ()
        ;; Execute the code in which you want to catch errors here.
        (cond
         ((procedure? cmd) (set! result (apply cmd args)))
         ((string? cmd) (set! result (eval-string cmd)))))
      (lambda (key . parameters)
        ;; Put the code which you want to handle an error after the
        ;; stack has been unwound here.
        (set! captured-error
          (call-with-output-string
            (lambda (port)
              (display-backtrace captured-stack port)
              (newline port)
              (print-exception port #f key parameters)))))
      (lambda (key . parameters)
        ;; Capture the stack here, cut the last 3 frames which are
        ;; make-stack, this one, and the throw handler.
        (set! captured-stack (make-stack #t 3))))
    (list result captured-error)))

;; gnc:eval-string-with-error-handling will evaluate the input string (cmd)
;; an captures any exception that would be generated. It returns
;; a list with 2 elements: the output of the evaluation and a backtrace.
;; The first may be set if string evaluation did generate
;; output, the latter is set when an exception was caught.
;; We'll use this to wrap guile calls in C(++), allowing
;; the C(++) code to decide how to handle the errors.
(define (gnc:eval-string-with-error-handling cmd)
  (gnc:call-with-error-handling cmd '()))

;; gnc:apply-with-error-handling will call guile's apply to run func with args
;; an captures any exception that would be generated. It returns
;; a list with 2 elements: the output of the evaluation and a backtrace.
;; The first may be set if the result of the apply did generate
;; output, the latter is set when an exception was caught.
;; We'll use this to wrap guile calls in C(++), allowing
;; the C(++) code to decide how to handle the errors.
(define (gnc:apply-with-error-handling func args)
  (gnc:call-with-error-handling func args))

(define (gnc:backtrace-if-exception proc . args)
  (let* ((apply-result (gnc:apply-with-error-handling proc args))
         (result (car apply-result))
         (captured-error (cadr apply-result)))
    (cond
     (captured-error
      (display captured-error (current-error-port))
      (set! gnc:last-captured-error (gnc:html-string-sanitize captured-error))
      (when (defined? 'gnc:warn)
        (gnc:warn captured-error))
      #f)
     (else result))))

(define-public gnc:last-captured-error "")

;; This database can be used to store and retrieve translatable
;; strings. Strings that are returned by the lookup function are
;; translated with gettext.
(define (gnc:make-string-database)
  (define string-hash (make-hash-table))
  (define (lookup key)
    (_ (hash-ref string-hash key)))
  (define (store key string)
    (hash-set! string-hash key string))
  (define (dispatch message . args)
    (let ((func (case message
                  ((lookup) lookup)
                  ((store) store)
                  (else #f))))
      (if func
          (apply func args)
          (gnc:warn "string-database: bad message" message "\n"))))
  dispatch)
