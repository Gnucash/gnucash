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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(require 'hash-table)

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
    (display-backtrace (fluid-ref the-last-stack) port))

  (false-if-exception
   (call-with-output-string write-error)))


(define gnc:register-translatable-strings #f)
(define gnc:save-translatable-strings #f)

(let ((string-hash (make-hash-table 313)))

  (define (register . strings)
    (if (gnc:debugging?)
        (for-each
         (lambda (string)
           (if (and (string? string) (> (string-length string) 0))
               (hash-set! string-hash string #t)))
         strings)))

  (define (expand-newlines string port)
    (let loop ((chars (string->list string))
               (accum '()))
      (cond
       ((null? chars)
        (write (list->string (reverse accum)) port))
       ((char=? (car chars) #\newline)
        (write (list->string (reverse accum)) port)
        (display "\"\\n\"\n  " port)
        (loop (cdr chars) '()))
       (else
        (loop (cdr chars) (cons (car chars) accum))))))

  (define (save file)
    (let ((port (open file (logior O_WRONLY O_CREAT O_TRUNC))))
      (if port
          (begin
            (hash-for-each
             (lambda (string not-used)
               (display "_(" port)
               (expand-newlines string port)
               (display ")\n" port))
             string-hash)
            (close port)))))

  (set! gnc:register-translatable-strings register)
  (set! gnc:save-translatable-strings save))

(if (gnc:debugging?)
    (define (gnc:gettext string)
      (gnc:register-translatable-strings string)
      (gnc:gettext-helper string))
    (define gnc:gettext gnc:gettext-helper))

(define gnc:_ gnc:gettext)
(if (not (defined? '_))
    (define _ gnc:gettext))
(define (N_ x) x)


;; This database can be used to store and retrieve translatable
;; strings. If debugging is true, the stored strings are registered so
;; they can be saved to a string database. Strings that are returned
;; by the lookup function are translated with gettext.
(define (gnc:make-string-database)

  (define string-hash (make-hash-table 23))

  (define (lookup key)
    (hash-ref string-hash key))

  (define (store key string)
    (if (gnc:debugging?)
        (gnc:register-translatable-strings string))
    (hash-set! string-hash key (gnc:_ string)))

  (define (dispatch message . args)
    (let ((func (case message
                  ((lookup) lookup)
                  ((store) store)
                  (else #f))))
      (if func
          (apply func args)
          (gnc:warn "string-database: bad message" message "\n"))))

  dispatch)
