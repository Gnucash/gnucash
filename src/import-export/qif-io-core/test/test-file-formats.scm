;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  test-file-formats.scm
;;  test the QIF file data format checker.  
;;  read each file, check field formats, compare with truth.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(debug-enable 'backtrace)

(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(gnc:module-load "gnucash/qif-io/core" 0)

(define (run-test)
  (define (line-test filename title thunk compare)
    (let ((pass 0)
          (fail 0)
          (total 0))
      (with-input-from-file filename
        (lambda ()
          (let loop ((this-line (read)))
            (if (not (eof-object? this-line))
                (let* ((exception? #f)
                       (result 
                        (catch #t
                               (lambda ()
                                 (apply thunk (car this-line)))
                               (lambda (key . rest)
                                 (set! exception? #t)
                                 (cons key rest))))
                       (exception-expected? (cadr this-line))
                       (correct-result (caddr this-line))
                       (ok? (and (eq? exception? exception-expected?)
                                 (compare result correct-result))))
                  (set! total (+ 1 total))
                  (if ok?
                      (set! pass (+ 1 pass))
                      (begin 
                        (format #t "[fail] received ~S\n" result)
                        (format #t "       expected ~S\n" 
                                       correct-result)
                        (set! fail (+ 1 fail))))
                  (loop (read)))))))
      (format #t "test ~A: pass=~S fail=~S\n" title pass fail)
      (= pass total)))
  
  (let ((all-pass #t))
    (define (fmt-check-test filename)
      (let ((qiffile (qif-io:make-empty-file)))
        (qif-io:read-file qiffile filename #f)
        (qif-io:check-possible-formats qiffile)
        (list (qif-io:bank-xtn->record (qif-io:file-bank-xtn-format qiffile))
              (qif-io:invst-xtn->record 
               (qif-io:file-invst-xtn-format qiffile)))))
    (set! all-pass 
          (and all-pass (line-test "data/file-formats-data.txt" 
                                   "qif-io:check-possible-formats"
                                   fmt-check-test equal?)))
    all-pass))



