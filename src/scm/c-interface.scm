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

(define gnc:register-c-side-scheme-ptr #f)
(define gnc:unregister-c-side-scheme-ptr-id #f)

(let ((next-registration-id 0)
      ;; Not sure this has to be prime, and not sure how large it needs
      ;; to be, but on both fronts, this should be fairly safe...
      (pointer-storage (make-hash-table 313)))

  (define (register-c-side-scheme-ptr ptr)
    (let ((id next-registration-id))
      (set! next-registration-id (+ next-registration-id 1))
      (hashv-set! pointer-storage id ptr)
      id))

  (define (unregister-c-side-scheme-ptr-id id)
    (if (hashv-ref pointer-storage id)
        (hashv-remove! pointer-storage id)
        (gnc:error "unregister-c-side-scheme-ptr-id: no such id\n")))

  (set! gnc:register-c-side-scheme-ptr register-c-side-scheme-ptr)
  (set! gnc:unregister-c-side-scheme-ptr-id unregister-c-side-scheme-ptr-id))


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

  (define (save file)
    (let ((port (open file (logior O_WRONLY O_CREAT O_TRUNC))))
      (if port
          (begin
            (hash-for-each
             (lambda (string not-used)
               (display "_(" port)
               (write string port)
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
