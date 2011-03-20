;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  test-parser.scm
;;
;;  test the QIF parser.  the data files are just scheme data; the
;;  first element is the arg to be parsed, the second indicates
;;  whether an exception is expected, and the third indicates either
;;  the return value (if no exception) or the type of exception and
;;  args.  For example, for the date file,
;;  ("02/01/2001" #f (2 1 2001)) 
;;  (#f #t (qif-io:arg-type string #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash gnc-module))
(gnc:module-system-init)

(gnc:module-load "gnucash/qif-io/core" 0)

(debug-enable 'debug)
(debug-enable 'backtrace)

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
    (define (parse-number/format num fmt)
      (let* ((gncn (qif-io:parse-number/format num fmt))
             (nstr (gnc-numeric-to-string gncn)))
        nstr))
    
    ;; test category reading 
    (set! all-pass 
          (and all-pass (line-test "data/category-data.txt" "parse-category"
                                   qif-io:parse-category equal?)))
    ;; date parsing 
    (set! all-pass 
          (and all-pass (line-test "data/date-data.txt" "parse-date/format"
                                   qif-io:parse-date/format equal?)))
    (set! all-pass 
          (and all-pass (line-test "data/date-format-data.txt" 
                                   "check-date-format"
                                   qif-io:check-date-format equal?)))

    ;; number parsing 
    (set! all-pass 
          (and all-pass (line-test "data/number-data.txt" "parse-number/format"
                                   parse-number/format equal?)))
    (set! all-pass 
          (and all-pass (line-test "data/number-format-data.txt" 
                                   "check-number-format"
                                   qif-io:check-number-format equal?)))
    all-pass))
