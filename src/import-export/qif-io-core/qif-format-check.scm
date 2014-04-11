;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-format-check.scm
;;;  scan a set of QIF data records to try to guess how to 
;;;  interpret number and date fields 
;;;
;;;  Copyright (c) 2001 Linux Developers Group 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:file-setup-data-formats file
;;
;; we try to find a unique data format for all the relevant fields.
;; if that fails, we throw an exception with a continuation proc that
;; allows us to resume work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:setup-data-formats file)
  ;; first: narrow down the possible field formats 
  (qif-io:check-possible-formats file)

  ;; then: make sure there's exactly one format per slot.
  (let ((invst-format-info (qif-io:file-invst-xtn-format file))
        (invst-field-info 
         (list (list qif-io:invst-xtn-date
                     qif-io:invst-xtn-set-date! "Date" 'date)
               (list qif-io:invst-xtn-t-amount 
                     qif-io:invst-xtn-set-t-amount! "Total" 'amount)
               (list qif-io:invst-xtn-u-amount 
                     qif-io:invst-xtn-set-u-amount! "UTotal" 'amount)
               (list qif-io:invst-xtn-$-amount 
                     qif-io:invst-xtn-set-$-amount! "$Total" 'amount)
               (list qif-io:invst-xtn-share-amount 
                     qif-io:invst-xtn-set-share-amount! "Num Shares" 'amount)
               (list qif-io:invst-xtn-share-price 
                     qif-io:invst-xtn-set-share-price! "Share Price" 'amount)
               (list qif-io:invst-xtn-commission
                     qif-io:invst-xtn-set-commission! "Commission" 'amount)))
        (bank-format-info (qif-io:file-bank-xtn-format file))
        (bank-field-info 
         (list (list qif-io:bank-xtn-date 
                     qif-io:bank-xtn-set-date! "Date" 'date)
               (list qif-io:bank-xtn-t-amount 
                     qif-io:bank-xtn-set-t-amount! "Total" 'amount)
               (list qif-io:bank-xtn-u-amount 
                     qif-io:bank-xtn-set-u-amount! "UTotal" 'amount)
               (list (lambda (format-xtn)
                       (let ((splits (qif-io:bank-xtn-splits format-xtn)))
                         (qif-io:split-amount (car splits))))
                     (lambda (format-xtn format-obj)
                       (let ((splits (qif-io:bank-xtn-splits format-xtn)))
                         (qif-io:split-set-amount! (car splits) format-obj)))
                     "Split total" 'amount))))
    
    ;; 'format-info' is some object. 'field-info' tells us how to get
    ;; and set its fields.  next-proc tells us what to do when we
    ;; finish.
    (define (do-xtn-format format-info field-info next-proc)
      (let loop ((fields field-info))
        (let* ((this-field (car fields))
               (getter (car this-field))
               (setter (cadr this-field))
               (field-name (caddr this-field))
               (field-type (cadddr this-field))
               (formats (getter format-info)))
          (cond 
           ((null? formats) 
            (throw 'qif-io:inconsistent-data-format field-name))
           ((not (list? formats))
            (if (not (null? (cdr fields))) (loop (cdr fields))))
           ((null? (cdr formats))
            (setter format-info (car formats))
            (if (not (null? (cdr fields))) (loop (cdr fields))))
           (#t
            ;; if there are multiple possible formats, throw an
            ;; exception.  the catcher should determine which of
            ;; 'formats' is correct and call the thunk with it as an
            ;; arg.
            (throw 'qif-io:ambiguous-data-format field-type field-name formats 
                   (lambda (correct-format)
                     (setter format-info correct-format)
                     (if (not (null? (cdr fields))) (loop (cdr fields)))
                     (next-proc)))))))
      ;; we call next-proc here if there was no exception during the 
      ;; normal loop execution. 
      (next-proc))
    
    ;; do the work.  We pass the investment format processing as a
    ;; continuation-proc so that it gets done no matter how we get out
    ;; of the loop in do-xtn-format
    (do-xtn-format 
     bank-format-info bank-field-info 
     (lambda ()
       (do-xtn-format 
        invst-format-info invst-field-info
        (lambda () #t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-field-formats 
;; this is is the engine that runs all the format tests on the qif
;; transactions.  we apply the 'checker' to the value returned by the
;; 'getter' for each object.  we successively narrow 'formats' as we
;; go along.  If there are no non-#f elements to check we return 
;; #f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-field-formats getter equiv-thunk checker formats objects)
  (let ((good-formats formats)
        (records-checked #f))
    ;; loop over objects.  If the formats list ever gets empty
    ;; we can stop right there. 
    (if (not (null? objects))
        (let loop ((current (car objects))
                   (rest (cdr objects)))
          (let ((val (getter current)))
            (if val 
                (begin
                  (set! records-checked #t)
                  (set! good-formats (checker val good-formats)))))
          (if (and (not (null? good-formats))
                   (not (null? rest)))
              (loop (car rest) (cdr rest)))))
    
    ;; we're done.  Return the formats that work for all the values.
    (if records-checked good-formats #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-possible-formats builds the file's format objects for
;; investment and bank transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:check-possible-formats file)
  (let ((bank-formats (qif-io:make-empty-bank-xtn)))
    ;; bank transactions 
    (qif-io:bank-xtn-set-date! 
     bank-formats 
     (check-field-formats 
      qif-io:bank-xtn-date equal?
      qif-io:check-date-format '(m-d-y d-m-y y-m-d y-d-m)
      (qif-io:file-bank-xtns file)))
    
    (qif-io:bank-xtn-set-t-amount! 
     bank-formats
     (check-field-formats 
      qif-io:bank-xtn-t-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-bank-xtns file)))
    
    (qif-io:bank-xtn-set-u-amount! 
     bank-formats
     (check-field-formats 
      qif-io:bank-xtn-u-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-bank-xtns file)))
    
    (let ((split (qif-io:make-empty-split)))
      (define (get-split-amounts xtn)
        (map (lambda (split)
               (qif-io:split-amount split))
             (qif-io:bank-xtn-splits xtn))) 
      (qif-io:split-set-amount! 
       split
       (check-field-formats 
        get-split-amounts gnc-numeric-equal
        qif-io:check-multi-number-format '(decimal comma)
        (qif-io:file-bank-xtns file)))
      (qif-io:bank-xtn-set-splits! bank-formats (list split)))
    
    ;; stuff the formats into the file 
    (qif-io:file-set-bank-xtn-format! file bank-formats))
  
  (let ((invst-formats (qif-io:make-empty-invst-xtn)))
    ;; invst transactions 
    (qif-io:invst-xtn-set-date! 
     invst-formats 
     (check-field-formats 
      qif-io:invst-xtn-date equal?
      qif-io:check-date-format '(m-d-y d-m-y y-m-d y-d-m)
      (qif-io:file-invst-xtns file)))
     
    (qif-io:invst-xtn-set-t-amount! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-t-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    (qif-io:invst-xtn-set-u-amount! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-u-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    (qif-io:invst-xtn-set-$-amount! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-$-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    (qif-io:invst-xtn-set-share-amount! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-share-amount gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    (qif-io:invst-xtn-set-share-price! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-share-price gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    (qif-io:invst-xtn-set-commission! 
     invst-formats
     (check-field-formats 
      qif-io:invst-xtn-commission gnc-numeric-equal
      qif-io:check-number-format '(decimal comma)
      (qif-io:file-invst-xtns file)))
    
    ;; stuff the formats into the file 
    (qif-io:file-set-invst-xtn-format! file invst-formats)))

  
     
  

