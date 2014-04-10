
;;; I'm maintaining the license that Olin put on the SRFI-1 reference
;;; code.
;;;
;;; Copyright 1999, Rob Browning <rlb@cs.utexas.edu>. You may do as
;;; you please with this code as long as you do not remove this
;;; copyright notice or hold me liable for its use.

;; This has been modified for GnuCash to use guile's built in error
;; function.

(define (srfi-1:error msg . args)
  (apply error msg args))

(define (srfi-1:check-arg pred val caller)
  (if (pred val)
      val
      (srfi-1:error "Bad argument" val "to function" caller)))
