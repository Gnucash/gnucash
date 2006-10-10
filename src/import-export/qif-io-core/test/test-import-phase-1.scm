;; test-import-phase-1
;; import the file by direct transaction mapping (don't remove any 
;; duplicates)

(debug-enable 'debug)
(debug-enable 'backtrace)

(define (_ arg) arg)
(define (N_ arg) arg) 

(define (do-file filename)
  (use-modules (gnucash gnc-module))
  (gnc:module-system-init)
  (gnc:module-load "gnucash/qif-io/core" 0)
  ;; XXX: Need app/file to initialize (gnc:get-current-session/book)
  
  (let ((qiffile (qif-io:make-empty-file))
        (acct-table (qif-io:make-empty-acct-table))
	(session (gnc:get-current-session))
	(book (qof-session-get-book session))
	(com-table (gnc-commodity-table-new)))

    (gnc-commodity-table-add-default-data com-table book)

    ;; read the file and look at data formats. we need to do this
    ;; immediately when loading a file.
    (qif-io:read-file qiffile filename #f)

    ;; this will throw out an exception if there are no possible correct
    ;; interpretations.  we'll correct the ambiguities
    (catch 'qif-io:ambiguous-data-format
           (lambda () 
             (qif-io:setup-data-formats qiffile))
           (lambda (key field-type field-name possible-formats continue-proc)
             (simple-format #t "field format: n='~S' t='~S' v='~S' u='~S'\n"
                            field-name field-type possible-formats 
                            (car possible-formats))
             (continue-proc (car possible-formats))))
    
    ;; now we need to figure out what information is missing from this
    ;; file.
    (if (qif-io:file-xtns-need-acct? qiffile)
        (qif-io:file-set-default-src-acct! qiffile filename))

    (let ((commodity (gnc-commodity-table-lookup com-table "ISO4217" "USD")))

      ;; import the bank transactions 
      (for-each 
       (lambda (xtn)
         (qif-io:bank-xtn-import xtn qiffile acct-table commodity))
       (qif-io:file-bank-xtns qiffile))
      
      ;; and the investment transactions 
      (for-each 
       (lambda (xtn)
         (qif-io:invst-xtn-import xtn qiffile acct-table commodity))
       (qif-io:file-invst-xtns qiffile))

      ;; build a gnucash account group
      (let ((group (qif-io:acct-table-make-gnc-group 
                    acct-table qiffile commodity)))
        ;; write the file
        (let* ((name (simple-format #f "file:~A.gnc" filename)))
          (simple-format #t "using book name='~A'\n" name)
          (xaccGroupConcatGroup (xaccGetAccountGroup book) group)
	  (xaccAccountGroupDestroy group)
          (gnc:session-begin session name #t #t)
          (gnc:session-save session)
          (gnc:session-end session)
	  (gnc:file-quit)))))
  0)

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
                        (apply thunk (car this-line)))
;                        (catch #t
;                               (lambda ()
;                                 (apply thunk (car this-line)))
;                               (lambda (key . rest)
;                                 (set! exception? #t)
;                                 (cons key rest))))
                       (exception-expected? (cadr this-line))
                       (correct-result (caddr this-line))
                       (ok? (and (eq? exception? exception-expected?)
                                 (compare result correct-result))))
                  (set! total (+ 1 total))
                  (if ok?
                      (set! pass (+ 1 pass))
                      (begin 
                        (simple-format #t "[fail] test ~S\n" (car this-line))
                        (simple-format #t "       received ~S\n" result)
                        (simple-format #t "       expected ~S\n" 
                                       correct-result)
                        (set! fail (+ 1 fail))))
                  (loop (read)))))))
      (simple-format #t "test ~A: pass=~S fail=~S\n" title pass fail)
      (= pass total)))
  
  (let ((all-pass #t))
    (set! all-pass 
          (and all-pass (line-test "data/import-phase-1-data.txt"
                                   "import phase 1"
                                   do-file equal?)))
    (if all-pass
        (exit 0)
        (exit -1))))
