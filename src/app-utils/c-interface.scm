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

(use-modules (ice-9 slib))
(use-modules (ice-9 syncase))

(define (gnc:error->string tag args)
  (define (write-error port)
    (if (and (list? args) (not (null? args)))
        (let ((func (car args)))
          (if func
              (begin
                (display "Function: " port)
                (display func port)
                (display ", " port)
                (display tag port)
                (display "\n\n" port)))))
    (false-if-exception
     (apply display-error (fluid-ref the-last-stack) port args))
    (display-backtrace (fluid-ref the-last-stack) port)
    (force-output port))
  
  (false-if-exception
   (call-with-output-string write-error)))


;; gettext functions
(define gnc:gettext gnc-gettext-helper)
(define gnc:_ gnc:gettext)
(define _ gnc:gettext)
(define-syntax N_
  (syntax-rules ()
    ((_ x) x)))


;; This database can be used to store and retrieve translatable
;; strings. Strings that are returned by the lookup function are
;; translated with gettext.
(define (gnc:make-string-database)

  (define string-hash (make-hash-table 23))

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
