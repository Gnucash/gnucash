;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports))
(use-modules (srfi srfi-13))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash core-utils))

(export gnc:register-report-create)
(export gnc:register-report-hook)

(define gnc:*register-report-hash* (make-hash-table 23))

;; Keep a hash-table of records, keyed off the account type.  Each
;; record contains a function pointer for that account-type with split
;; or without split.  If no function is found, then run the 'default'
;; function

(define acct-type-info (make-record-type "AcctTypeInfo" '(split non-split)))

(define make-acct-type-private
  (record-constructor acct-type-info '(split non-split)))

(define (make-acct-type)
  (make-acct-type-private #f #f))

(define get-split
  (record-accessor acct-type-info 'split))

(define set-split
  (record-modifier acct-type-info 'split))

(define get-non-split
  (record-accessor acct-type-info 'non-split))

(define set-non-split
  (record-modifier acct-type-info 'non-split))

(define (gnc:register-report-hook acct-type split? create-fcn)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))

    (if (not type-info)
	(set! type-info (make-acct-type)))

    (if split?
	(set-split type-info create-fcn)
	(set-non-split type-info create-fcn))

    (hash-set! gnc:*register-report-hash* acct-type type-info)))

(define (lookup-register-report acct-type split)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))
    (gnc:debug "acct-type: " acct-type)
    (gnc:debug "ref: " type-info)
    (gnc:debug "hash: " gnc:*register-report-hash*)
    (gnc:debug "split: " split)
    (if type-info
	(if (and split (not (null? split)))
	    (begin (gnc:debug "get-split...") (get-split type-info))
	    (begin (gnc:debug "get-non-split...") (get-non-split type-info)))
	#f)))


;; Returns a list of files in a directory
;;
;; Param:
;;   dir - directory name
;;
;; Return value:
;;   list of files in the directory

(define (directory-files dir)
  (if (file-exists? dir)
      (let ((fname-regexp (make-regexp "\\.scm$")) ;; Regexp that matches the desired filenames
            (dir-stream (opendir dir)))

           (let loop ((fname (readdir dir-stream))
                      (acc '()))
                     (if (eof-object? fname)
                         (begin
                             (closedir dir-stream)
                             acc
                         )
                         (loop (readdir dir-stream)
                               (if (regexp-exec fname-regexp fname)
                                   (cons fname acc)
                                   acc
                               )
                         )
                     )
           ))
      (begin
          (gnc:warn "Can't access " dir ".\nEmpty list will be returned.")
          '() ;; return empty list
      )
  )
)

;; Process a list of files by removing the ".scm" suffix if it exists
;;
;; Param:
;;   l - list of files
;;
;; Return value:
;;   List of files with .scm suffix removed
(define (process-file-list l)
    (map (lambda (s) (if (string-suffix? ".scm" s) (string-drop-right s 4) s))
         l
    )
)

;; Return a list of symbols representing reports in the standard reports directory
;;
;; Return value:
;;  List of symbols for reports
(define (get-report-list)
    (map (lambda (s) (string->symbol s))
         (process-file-list (directory-files (gnc-path-get-stdreportsdir))))
)

(gnc:debug "stdrpt-dir=" (gnc-path-get-stdreportsdir))
(gnc:debug "dir-files=" (directory-files (gnc-path-get-stdreportsdir)))
(gnc:debug "processed=" (process-file-list (directory-files (gnc-path-get-stdreportsdir))))
(gnc:debug "report-list=" (get-report-list))

(for-each
    (lambda (x)
	    (module-use!
		    (current-module)
			(resolve-interface (append '(gnucash report standard-reports) (list x)))))
	(get-report-list))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

(define (gnc:register-report-create account split query journal? ledger-type?
				    double? title debit-string credit-string)
  (let* ((acct-type (xaccAccountGetType account))
	 (create-fcn (lookup-register-report acct-type split)))
    (gnc:debug "create-fcn: " create-fcn)
    (if create-fcn
	(create-fcn account split query journal? double? title
		    debit-string credit-string)
	(gnc:register-report-create-internal #f query journal? ledger-type? double?
					     title debit-string credit-string))))
