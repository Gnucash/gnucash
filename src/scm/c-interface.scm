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
     (apply display-error #f port args))
    ;; Here we should write the stack trace.
    )

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
    (begin
      (define (gnc:gettext string)
        (gnc:register-translatable-strings string)
        (gnc:gettext-helper string)))
    (define gnc:gettext gnc:gettext-helper))

(define gnc:_ gnc:gettext)
