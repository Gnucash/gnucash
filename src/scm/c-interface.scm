(define gnc:register-c-side-scheme-ptr #f)
(define gnc:unregister-c-side-scheme-ptr-id #f)

(let ((next-registration-id 0)
      ;; Not sure this has to be prime, and not sure how large it needs
      ;; to be, but on both fronts, this should be fairly safe...
      (pointer-storage (make-vector 313)))

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
