(debug-enable 'backtrace)

(define (line-dump filename thunk)
  (with-input-from-file filename
    (lambda ()
      (let loop ((this-line (read)))
        (if (not (eof-object? this-line))
            (begin 
              (apply thunk (car this-line))
              (loop (read))))))))

(define (read-file-thunk infile)
  (let ((qiffile (qif-io:make-file #f #f #f #f #f #f #f)))
    (format #t "======= ~A ======\n" infile)
    (qif-io:read-file qiffile infile #f)    
    (qif-io:write-file qiffile (format #f "~A.out" infile))))

(gnc:module-load "qifiocore")

(line-dump "data/reader-data.txt" read-file-thunk)
