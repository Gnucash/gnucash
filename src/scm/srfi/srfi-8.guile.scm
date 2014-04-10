;;; I'm maintaining the license that Olin put on the SRFI-1 reference
;;; code.
;;;
;;; Copyright 1999, Rob Browning <rlb@cs.utexas.edu>. You may do as
;;; you please with this code as long as you do not remove this
;;; copyright notice or hold me liable for its use.

(use-modules (ice-9 slib))
(require 'macro-by-example)
(require 'values)

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
       (lambda formals body ...)))))
