
;;; 
;;; Code to support emacs-inspired hooks.
;;;

;;;; This is not functional yet, but it should be close...

;;; Private

;; Central repository for all hooks -- so we can look them up later by name.
(define gnucash:*hooks* '())

(define (gnucash:hook-danglers-get hook)
  (vector-ref hook 2))

(define (gnucash:hook-danglers-set! hook danglers)
  (vector-set! hook 2 danglers))

;;; Developers

(define (gnucash:hook-define name description)
  (let ((hook-data (vector name description '())))
    (set! gnucash:*hooks* (assoc-set! gnucash:*hooks* name hook-data))
    hook-data))

(define (gnucash:hook-danglers->list hook)
  (gnucash:hook-danglers-get hook))

(define (gnucash:hook-replace-danglers hook function-list)
  (gnucash:hook-danglers-set! hook function-list))

(define (gnucash:hook-run-danglers hook)
  (for-each (lambda (dangler) (dangler)) (gnucash:hook-danglers-get hook)))

;;; Public

(define (gnucash:hook-lookup name)
  (assoc-ref gnucash:*hooks* name))

(define (gnucash:hook-add-dangler hook function)
  (let ((danglers (gnucash:hook-danglers-get hook)))
    (gnucash:hook-danglers-set! hook (append danglers (list function)))))

(define (gnucash:hook-remove-dangler hook function)
  (let ((danglers (gnucash:hook-danglers-get hook)))
    (gnucash:hook-danglers-set! hook (delq! function danglers))))

(define (gnucash:hook-description-get hook)
  (vector-ref hook 1))

(define (gnucash:hook-name-get hook)
  (vector-ref hook 0))

(gnucash:hook-define 'startup-hook "Startup hooks")

(let ((hook (gnucash:hook-lookup 'startup-hook)))
  (display (gnucash:hook-name-get hook))
  (newline)
  (display (gnucash:hook-description-get hook))
  (newline)
  (gnucash:hook-add-dangler hook (lambda ()
                                   (display "Running a simple startup hook")
                                   (newline)))
  (gnucash:hook-run-danglers hook))
