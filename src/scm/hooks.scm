
;;; 
;;; Code to support emacs-inspired hooks.
;;;

;;;; This is not functional yet, but it should be close...

;;; Private

;; Central repository for all hooks -- so we can look them up later by name.
(define gnc:*hooks* '())

(define (gnc:hook-danglers-get hook)
  (vector-ref hook 2))

(define (gnc:hook-danglers-set! hook danglers)
  (vector-set! hook 2 danglers))

;;; Developers

(define (gnc:hook-define name description)
  (let ((hook-data (vector name description '())))
    (set! gnc:*hooks* (assoc-set! gnc:*hooks* name hook-data))
    hook-data))

(define (gnc:hook-danglers->list hook)
  (gnc:hook-danglers-get hook))

(define (gnc:hook-replace-danglers hook function-list)
  (gnc:hook-danglers-set! hook function-list))

(define (gnc:hook-run-danglers hook)
  (gnc:debug "Running functions on hook " (gnc:hook-name-get hook))
  (for-each (lambda (dangler) (dangler)) (gnc:hook-danglers-get hook)))

;;; Public

(define (gnc:hook-lookup name)
  (assoc-ref gnc:*hooks* name))

(define (gnc:hook-add-dangler hook function)
  (let ((danglers (gnc:hook-danglers-get hook)))
    (gnc:hook-danglers-set! hook (append danglers (list function)))))

(define (gnc:hook-remove-dangler hook function)
  (let ((danglers (gnc:hook-danglers-get hook)))
    (gnc:hook-danglers-set! hook (delq! function danglers))))

(define (gnc:hook-description-get hook)
  (vector-ref hook 1))

(define (gnc:hook-name-get hook)
  (vector-ref hook 0))

(define gnc:*startup-hook*
  (gnc:hook-define 'startup-hook "Functions to run at startup."))

(define gnc:*shutdown-hook*
  (gnc:hook-define 'shutdown-hook "Functions to run at shutdown."))

;;(let ((hook (gnc:hook-lookup 'startup-hook)))
;;  (display (gnc:hook-name-get hook))
;;  (newline)
;;  (display (gnc:hook-description-get hook))
;;  (newline)
;;  (gnc:hook-add-dangler hook (lambda ()
;;                                   (display "Running a simple startup hook")
;;                                   (newline)))
;;  (gnc:hook-run-danglers hook))
