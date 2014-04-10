;;;; srfi-8.scm --- SRFI-8 procedures for Guile

;;; Copyright (C) 2000 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

(define-module (srfi srfi-8))

(cond
 ((or (string=? "1.3" (version))
      (string=? "1.3.4" (version))
      (string=? "1.4" (version)))
  
  (use-modules (ice-9 slib))
  (require 'macro-by-example)
  (require 'values)
  
  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body ...)
       (call-with-values (lambda () expression)
         (lambda formals body ...))))))
 
 (else
  (let ((msg
         (string-append
          "Loaded gnucash srfi-8.scm in unknown Guile version:" (version) ".\n"
          "If you're running a Guile newer than 1.4, then this file should\n"
          "not have been installed.  Please report the bug.")))
  (error msg))))

(export-syntax receive)
