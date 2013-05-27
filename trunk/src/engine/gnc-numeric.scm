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

(define <gnc-numeric>
  (make-record-type "<gnc-numeric>" 
                    '(num denom)))

(define gnc:make-gnc-numeric 
  (record-constructor <gnc-numeric>))

(define gnc:gnc-numeric? 
  (record-predicate <gnc-numeric>))

(define gnc:gnc-numeric-num
  (record-accessor <gnc-numeric> 'num))

(define gnc:gnc-numeric-denom
  (record-accessor <gnc-numeric> 'denom))

(define (gnc:gnc-numeric-denom-reciprocal arg)
  (- arg))



(define <gnc-monetary> 
  (make-record-type "<gnc-monetary>" 
		    '(commodity amount)))

;; Constructor; takes one <gnc:commodity*> and one <gnc-numeric>
(define (gnc:make-gnc-monetary c a)
  ;;FIXME: we used to type-check the values, like:
  ;; (gw:wcp-is-of-type? <gnc:commodity*> c)
  (if (and #t (gnc:gnc-numeric? a))
      ((record-constructor <gnc-monetary>) c a)
      (warn "wrong arguments for gnc:make-gnc-monetary: " c a)))

(define gnc:gnc-monetary? 
  (record-predicate <gnc-monetary>))

(define gnc:gnc-monetary-commodity
  (record-accessor <gnc-monetary> 'commodity))

(define gnc:gnc-monetary-amount
  (record-accessor <gnc-monetary> 'amount))

(define (gnc:monetary-neg a)
  (if (gnc:gnc-monetary? a)
      (gnc:make-gnc-monetary 
       (gnc:gnc-monetary-commodity a)
       (gnc-numeric-neg (gnc:gnc-monetary-amount a)))
      (warn "wrong arguments for gnc:monetary-neg: " a)))
