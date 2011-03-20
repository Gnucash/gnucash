;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-file.scm
;;;  read a QIF file into a <qif-file> object
;;;
;;;  Copyright (c) 2001 Linux Developers Group 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(debug-enable 'debug)
(debug-enable 'backtrace)

(define end-of-line (string #\cr #\nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:read-record
;;  this reads a "record", which is a block of tag-value lines ended
;;  by a line starting with "^".  A line starting with "!" generates 
;;  an exception. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:read-record port)
  (if (not (input-port? port))
      (throw 'qif-io:arg-type 'input-port port))
  
  (let ((byte-count 0)
        (eof? #f)
        (record '()))
    (let line-loop ((line (read-delimited end-of-line port)))
      (if (and (string? line)
               (not (string=? line "")))
          (let ((tag (string-ref line 0))
                (value (substring line 1)))
            (set! byte-count (+ (string-length line) byte-count))
            (case tag
              ((#\^) #t) 
              ((#\!)
               (throw 'qif-io:parser-state value))
              (else 
               (set! record (cons (cons tag value) record))
               (line-loop (read-delimited end-of-line port)))))
          (if (eof-object? line)
              (set! eof? #t)
              (if (not (string? line))
                  (throw 'qif-io:record-error 'qif-io:read-record line)
                  (line-loop (read-delimited end-of-line port))))))
    (list (reverse record) byte-count eof?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:write-record pairs port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:write-record record-pairs port)
  (if (not (list? record-pairs))
      (throw 'qif-io:arg-type 'list record-pairs))
  (if (not (output-port? port))
      (throw 'qif-io:arg-type 'output-port port))  
  (for-each 
   (lambda (kvp)
     (format port "~A~A\n" (car kvp) (cdr kvp)))
   record-pairs)
  (format port "^\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:read-file path  
;;  suck in all the transactions; don't do any string interpretation, 
;;  just store the fields "raw".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:read-file file-obj path progress-thunk)
  (define (string-prune arg)
    (string-remove-trailing-space arg))
  
  (if (not (string? path))
      (throw 'qif-io:arg-type 'string path))

  (let* ((port 
          (catch #t 
                 (lambda () 
                   (open-input-file path))
                 (lambda (tag . args)
                   (throw 'qif-io:file-error path))))
         (file-stats (stat path))
         (file-size (stat:size file-stats))
         (bytes-read 0)
         (exception #f)
         (record #f)
         (record-info #f)
         (record-type #f)
         (bank-xtns '())
         (invst-xtns '())
         (accounts '())
         (classes '())
         (categories '())
         (securities '())
         (autoswitch #t)
         (autoswitch-acct #f)
         (opening-bal-acct #f)
         (some-bank-need-src-acct #f)
         (some-invst-need-src-acct #f))
    (let record-loop ()
      (catch #t
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;; record processor
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (lambda ()
               ;; read the record 
               (set! record-info (qif-io:read-record port))
               (set! record (car record-info))
               (set! bytes-read (+ bytes-read (cadr record-info)))
               (if (procedure? progress-thunk)
                   (progress-thunk bytes-read file-size))
               
               ;; convert it to the relevant struct 
               (if (not (null? record))
                   (case record-type
                     ;; bank transactions 
                     ((bank-xtn)
                      (let ((xtn (qif-io:record->bank-xtn record)))
                        (if autoswitch-acct
                            (qif-io:bank-xtn-set-source-acct! 
                             xtn autoswitch-acct)                           
                            ;; the Opening Balance transaction is special. 
                            ;; if there's no autoswitch account set, the OB 
                            ;; will set it.  But beware because it doesn't 
                            ;; have to be the first xtn.  
                            (let ((obacct 
                                   (qif-io:bank-xtn-opening-bal-acct xtn)))
                              (if obacct 
                                  (begin 
                                    (qif-io:bank-xtn-set-source-acct! 
                                     xtn obacct)
                                    (set! autoswitch-acct obacct)
                                    (set! opening-bal-acct obacct))
                                  (set! some-bank-need-src-acct #t))))
                        (set! bank-xtns (cons xtn bank-xtns))))
                     
                     ;; investment transactions 
                     ((invst-xtn)
                      (let ((xtn (qif-io:record->invst-xtn record)))
                        (if autoswitch-acct
                            (qif-io:invst-xtn-set-source-acct! 
                             xtn autoswitch-acct)
                            (set! some-invst-need-src-acct #t))
                        (set! invst-xtns (cons xtn invst-xtns))))
                     
                     ;; account records
                     ((account)
                      (let ((account (qif-io:record->account record)))
                        (if autoswitch 
                            (set! autoswitch-acct 
                                  (qif-io:account-name account))
                            (set! accounts (cons account accounts)))))

                     ;; class records 
                     ((class)
                      (set! classes
                            (cons (qif-io:record->class record) 
                                  classes)))

                     ;; category records 
                     ((category)
                      (set! categories
                            (cons (qif-io:record->category record) 
                                  categories)))
                     
                     ;; anything we don't know about 
                     ((unknown) #t)
                     (else
                      (throw 'qif-io:format-error path record-type)))))
             
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;; record exception handler 
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (lambda (key . args)
               (set! exception #t)
               (case key 
                 ;; when the parser sees a ! line, it throws this
                 ;; exception
                 ((qif-io:parser-state)
                  (let ((new-state (string-prune (car args))))
                    (cond ((or (string-ci=? new-state "type:bank")
                               (string-ci=? new-state "type:cash")
                               (string-ci=? new-state "type:ccard")
                               (string-ci=? new-state "type:oth a")
                               (string-ci=? new-state "type:oth l"))
                           (set! record-type 'bank-xtn))
                          ((or (string-ci=? new-state "type:invst")
                               (string-ci=? new-state "type:port"))
                           (set! record-type 'invst-xtn))
                          ((string-ci=? new-state "account")
                           (set! record-type 'account))
                          ((string-ci=? new-state "type:class")
                           (set! record-type 'class))
                          ((string-ci=? new-state "type:cat")
                           (set! record-type 'category))
                          ((string-ci=? new-state "type:security")
                           (set! record-type 'security))
                          ((string-ci=? new-state "option:autoswitch")
                           (set! autoswitch #f))
                          ((string-ci=? new-state "clear:autoswitch")
                           (set! autoswitch #t))
                          (#t 
                           (set! record-type 'unknown)))))
                 ((qif-io:record-error)
                  (format #t "record processing error ~S\n" args))
                 (else 
                  (apply throw key args)))))
      
      ;; third element of record-info tells whether an eof was
      ;; encountered
      (if (or exception (and (list? record-info) (not (caddr record-info))))
          (begin 
            (set! exception #f)
            (record-loop))))
    
    ;; if any bank transactions don't have a source account, we need
    ;; to set it for them (if we found an Opening Balance record) or
    ;; set a flag in the file struct so that we can ask the user.

    (if some-bank-need-src-acct
        (if opening-bal-acct
            (begin
              (for-each 
               (lambda (xtn)
                 (if (not (qif-io:bank-xtn-src-acct xtn))
                     (qif-io:bank-xtn-set-src-acct! xtn opening-bal-acct)))
               bank-xtns)
              (set! some-bank-need-src-acct #f))))
    (if (or some-bank-need-src-acct some-invst-need-src-acct)
        (qif-io:file-set-xtns-need-acct?! file-obj #t))
    
    ;; done reading all the records. fill in the qif-file object.
    (qif-io:file-set-bank-xtns! file-obj (reverse bank-xtns))
    (qif-io:file-set-invst-xtns! file-obj (reverse invst-xtns))
    (qif-io:file-set-accounts! file-obj (reverse accounts))
    (qif-io:file-set-categories! file-obj (reverse categories))
    (qif-io:file-set-classes! file-obj (reverse classes))
    (qif-io:file-set-securities! file-obj (reverse securities))
    #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:write-file file-obj path  
;;  write a <qif-file> out.  all objects must have fields in 
;;  string form. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:write-file qif-obj path) 
  (if (not (string? path))
      (throw 'qif-io:arg-type 'string path))
  (if (not (qif-io:file? qif-obj))
      (throw 'qif-io:arg-type 'qif-io:file qif-obj))
  
  (let ((port (open-output-file path))
        (accts (qif-io:file-accounts qif-obj))
        (cats (qif-io:file-categories qif-obj))
        (classes (qif-io:file-classes qif-obj))
        (bank-xtns (qif-io:file-bank-xtns qif-obj))
        (invst-xtns (qif-io:file-invst-xtns qif-obj)))
    
    ;; write out the list of "classes" (user tags on transactions...
    ;; these will be dummies since Gnucash doesn't do tags the same 
    ;; way)
    (if (not (null? classes))
        (begin
          (format port "!Type:Class\n")
          (for-each 
           (lambda (class)
             (qif-io:write-record (qif-io:class->record class) port))
           classes)))

    ;; write out the list of "categories" (income and expense 
    ;; accounts)
    (if (not (null? cats))
        (begin
          (format port "!Type:Cat\n")
          (for-each 
           (lambda (cat)
             (qif-io:write-record (qif-io:category->record cat) port))
           cats)))
    
    ;; write out the list of "accounts" (asset and liability
    ;; accounts)
    (if (not (null? accts))
        (begin
          (format port "!Option:Autoswitch\n")
          (format port "!Account\n")
          (for-each 
           (lambda (acct)
             (qif-io:write-record (qif-io:account->record acct) port))
           accts)
          (format port "!Clear:Autoswitch\n")))
    
    ;; write out bank transactions.  Make sure to preface each
    ;; section with the source-account record.
    (if (not (null? bank-xtns))
        (let ((this-acct '())
              (not-this-acct '()))
          ;; first write out all the transactions that don't have 
          ;; a source-acct string 
          (for-each 
           (lambda (xtn)
             (if (not (string? (qif-io:bank-xtn-source-acct xtn)))
                 (set! this-acct (cons xtn this-acct))
                 (set! not-this-acct (cons xtn not-this-acct))))
           bank-xtns)
          (if (not (null? this-acct))
              (begin 
                (format port "!Type:Bank\n")
                (for-each 
                 (lambda (xtn)
                   (qif-io:write-record (qif-io:bank-xtn->record xtn) port))
                 this-acct)))
          (set! bank-xtns (reverse not-this-acct))
          (set! this-acct '())
          (set! not-this-acct '())

          ;; iterate over accounts, writing out all the bank xtns
          ;; that are in that account 
          (for-each 
           (lambda (acct)
             (for-each 
              (lambda (xtn)
                (if (and (string? (qif-io:bank-xtn-source-acct xtn)) 
                         (string=? (qif-io:account-name acct)
                                   (qif-io:bank-xtn-source-acct xtn)))
                    (set! this-acct (cons xtn this-acct))
                    (set! not-this-acct (cons xtn not-this-acct))))
              bank-xtns)
             (if (not (null? this-acct))
                 (begin 
                   (format port "!Account\n")
                   (qif-io:write-record (qif-io:account->record acct) port)
                   (format port "!Type:~A\n" 
                                  (qif-io:account-type acct))
                   (set! this-acct (reverse this-acct))
                   (for-each 
                    (lambda (xtn)
                      (qif-io:write-record (qif-io:bank-xtn->record xtn)
                                           port))
                    this-acct)))
             (set! bank-xtns (reverse not-this-acct))
             (set! this-acct '())
             (set! not-this-acct '()))
           accts)))
    
    ;; write out invst transactions.  Make sure to preface each
    ;; section with the source-account record.
    (if (not (null? invst-xtns))
        (let ((this-acct '())
              (not-this-acct '()))
          ;; first write out all the transactions that don't have 
          ;; a source-acct string 
          (for-each 
           (lambda (xtn)
             (if (not (string? (qif-io:invst-xtn-source-acct xtn)))
                 (set! this-acct (cons xtn this-acct))
                 (set! not-this-acct (cons xtn not-this-acct))))
           invst-xtns)
          (if (not (null? this-acct))
              (begin 
                (format port "!Type:Invst\n")
                (for-each 
                 (lambda (xtn)
                   (qif-io:write-record (qif-io:invst-xtn->record xtn) port))
                 this-acct)))
          (set! invst-xtns (reverse not-this-acct))
          (set! this-acct '())
          (set! not-this-acct '())
          
          ;; iterate over accounts, writing out all the invst xtns
          ;; that are in that account 
          (for-each 
           (lambda (acct)
             (for-each 
              (lambda (xtn)
                (if (and (string? (qif-io:invst-xtn-source-acct xtn)) 
                         (string=? (qif-io:account-name acct)
                                   (qif-io:invst-xtn-source-acct xtn)))
                    (set! this-acct (cons xtn this-acct))
                    (set! not-this-acct (cons xtn not-this-acct))))
              invst-xtns)
             (if (not (null? this-acct))
                 (begin 
                   (format port "!Account\n")
                   (qif-io:write-record (qif-io:account->record acct) port)
                   (format port "!Type:~A\n" 
                                  (qif-io:account-type acct))
                   (set! this-acct (reverse this-acct))
                   (for-each 
                    (lambda (xtn)
                      (qif-io:write-record (qif-io:invst-xtn->record xtn)
                                           port))
                    this-acct)))
             (set! invst-xtns (reverse not-this-acct))
             (set! this-acct '())
             (set! not-this-acct '()))
           accts)))
    (close-output-port port)))

