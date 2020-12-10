;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnc-numeric.scm : rational number representation for gnucash
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash engine gnc-numeric))

(use-modules (srfi srfi-9))

(export GNC-RND-FLOOR)
(export GNC-RND-CEIL)
(export GNC-RND-TRUNC)
(export GNC-RND-PROMOTE)
(export GNC-RND-ROUND-HALF-DOWN)
(export GNC-RND-ROUND-HALF-UP)
(export GNC-RND-ROUND)
(export GNC-RND-NEVER)
(export GNC-DENOM-AUTO)
(export GNC-DENOM-REDUCE)
(export GNC-DENOM-FIXED)
(export GNC-DENOM-LCD)
(export GNC-DENOM-SIGFIG)
(export GNC-DENOM-SIGFIGS)
(export GNC-ERROR-OK)
(export GNC-ERROR-ARG)
(export GNC-ERROR-OVERFLOW)
(export GNC-ERROR-DENOM-DIFF)
(export GNC-ERROR-REMAINDER)
(export :gnc-monetary)
(export gnc:gnc-monetary?)
(export gnc:make-gnc-monetary)
(export gnc:gnc-monetary-commodity)
(export gnc:gnc-monetary-amount)
(export gnc:monetary-neg)

;; use 'logior' in guile to bit-combine RND and DENOM flags.

(define GNC-RND-FLOOR           1)
(define GNC-RND-CEIL            2)
(define GNC-RND-TRUNC           3)
(define GNC-RND-PROMOTE         4)
(define GNC-RND-ROUND-HALF-DOWN 5)
(define GNC-RND-ROUND-HALF-UP   6)
(define GNC-RND-ROUND           7)
(define GNC-RND-NEVER           8)

(define GNC-DENOM-AUTO          0)
(define GNC-DENOM-REDUCE       32)
(define GNC-DENOM-FIXED        64)
(define GNC-DENOM-LCD          48)
(define GNC-DENOM-SIGFIG       80)

(define (GNC-DENOM-SIGFIGS n)
  (logior GNC-DENOM-SIGFIG (* n 256)))

(define GNC-ERROR-OK            0)
(define GNC-ERROR-ARG          -1)
(define GNC-ERROR-OVERFLOW     -2)
(define GNC-ERROR-DENOM-DIFF   -3)
(define GNC-ERROR-REMAINDER    -4)


(define-record-type :gnc-monetary
  (make-gnc-monetary commodity amount)
  gnc-monetary?
  (commodity gnc-monetary-commodity)
  (amount gnc-monetary-amount))

(define gnc:make-gnc-monetary make-gnc-monetary)
(define gnc:gnc-monetary? gnc-monetary?)
(define gnc:gnc-monetary-commodity gnc-monetary-commodity)
(define gnc:gnc-monetary-amount gnc-monetary-amount)
(define (gnc:monetary-neg a)
  (gnc:make-gnc-monetary
   (gnc:gnc-monetary-commodity a)
   (- (gnc:gnc-monetary-amount a))))
