;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  test-reader.scm
;;
;;  test the QIF reader. see test-parser.scm for info on the structure
;;  of the test data files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(gnc:module-load "gnucash/qif-io/core" 0)
  
(define (read-record-test qiffile)
  (let ((inport (open-input-file qiffile))
        (outport (open-output-file "/tmp/test-reader.tmp"))
        (record '())
        (eof? #f))
    (let loop ()
      (catch 'qif-io:parser-state
             (lambda ()
               (let ((record (qif-io:read-record inport)))
                 (set! eof? (caddr record))
                 (if (not eof?)
                     (qif-io:write-record (car record) outport))))
             (lambda (key new-state)
               (format outport "!~A\n" new-state)))
      (if (not eof?)
          (loop)))
    (close-output-port outport)
    (close-input-port inport)
    (system (format #f "diff -b -I \"\\^*\" ~A /tmp/test-reader.tmp"
                           qiffile))))


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
    ;; test record reading / writing
    (set! all-pass 
          (and all-pass (line-test "data/reader-data.txt" "read-record"
                                   read-record-test equal?)))
    all-pass))



