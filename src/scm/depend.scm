
(define gnc:*_supported-files_* (make-hash-table 101))
;; Record of files that have already been loaded.  We don't do
;; anything other than record the name that the file tells us (via
;; gnc:support) that it provides.  The size of this table should
;; roughly depend on the number of .scm files in the source tree.

(define (gnc:support name)
  (hash-set! gnc:*_supported-files_* name #t))

(define (gnc:depend name)
  (let ((supported? (hash-ref gnc:*_supported-files_* name)))
    (if supported?
        #t
        (gnc:load name))))
