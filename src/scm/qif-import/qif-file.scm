;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-file.scm
;;;  read a QIF file into a <qif-file> object
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-file.scm")
(gnc:depend  "qif-import/qif-objects.scm")
(gnc:depend  "qif-import/qif-parse.scm")
(gnc:depend  "qif-import/qif-utils.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:read-file self path  
;;  suck in all the transactions; don't do any string interpretation, 
;;  just store the fields "raw".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:read-file self path)
  (qif-file:set-path! self path)
  (let ((qstate-type #f)
        (current-xtn #f)
        (current-split #f)
        (current-account-name #f)
        (default-split #f)
        (first-xtn #f)
        (ignore-accounts #f)
        (return-val #t)
        (line #f)
        (tag #f)
        (value #f)
        (heinous-error #f)
        (start-time #f)
        (end-time #f)
        (delimiters (string #\cr #\nl))
	(valid-acct-types 
         '(type:bank type:cash
                     type:ccard type:invst
                     #{type:oth\ a}#  #{type:oth\ l}#)))
    (set! start-time (gettimeofday))
    (with-input-from-file path
      (lambda ()
        ;; loop over lines
        (let line-loop ()
          (set! line (read-delimited delimiters))
          (if (and 
               (not (eof-object? line))
               (not (string=? line "")))
              (begin 
                ;; pick the 1-char tag off from the remainder of the line 
                (set! tag (string-ref line 0))
                (set! value (make-shared-substring line 1))
                
                ;; now do something with the line 
                (if
                 (eq? tag #\!)
                 (begin 
                   (set! qstate-type (qif-parse:parse-bang-field value))
                   (case qstate-type 
                     ((type:bank type:cash type:ccard type:invst
                                 #{type:oth\ a}#  #{type:oth\ l}#)
                      (set! current-xtn (make-qif-xtn))
                      (set! default-split (make-qif-split))
                      (qif-split:set-category! default-split "")
                      (qif-file:set-default-account-type! 
                       self (qif-parse:state-to-account-type qstate-type))
                      (set! first-xtn #t))
                     ((type:class)
                      (set! current-xtn (make-qif-class)))
                     ((type:cat)
                      (set! current-xtn (make-qif-cat)))
                     ((account)
                      (set! current-xtn (make-qif-acct)))
                     ((option:autoswitch)
                      (set! ignore-accounts #t))
                     ((clear:autoswitch)
                      (set! ignore-accounts #f))))
                 
;;;                        (#t 
;;;                         (display "qif-file:read-file can't handle ")
;;;                         (write qstate-type)
;;;                         (display " transactions yet.")
;;;                         (newline))))
                 
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;; bank-account type transactions 
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 
                 (case qstate-type 
                   ((type:bank type:cash type:ccard type:invst
                               #{type:oth\ a}#  #{type:oth\ l}#)
                    (case tag
                      ;; D : transaction date 
                      ((#\D)
                       (qif-xtn:set-date! current-xtn value))
                      
                      ;; T : total amount 
                      ((#\T)
                       (qif-split:set-amount! default-split value))
                      
                      ;; P : payee
                      ((#\P)
                       (qif-xtn:set-payee! current-xtn value))
                      
                      ;; A : address 
                      ;; multiple "A" lines are appended together with 
                      ;; newlines; some Quicken files have a lot of 
                      ;; A lines. 
                      ((#\A)
                       (qif-xtn:set-address! 
                        current-xtn
                        (let ((current (qif-xtn:address current-xtn)))
                          (if (not (string? current))
                              (set! current ""))
                          (string-append current "\n" value))))
                      
                      ;; N : check number / transaction number /xtn direction
                      ;; there's both an action and a number in gnucash,
                      ;; one for securities, one for banks. 
                      ((#\N)
                       (if (eq? qstate-type 'type:invst)
                           (qif-xtn:set-action! current-xtn value)
                           (qif-xtn:set-number! current-xtn value)))

                      ;; C : cleared flag 
                      ((#\C)
                       (qif-xtn:set-cleared! current-xtn value))
                      
                      ;; M : memo 
                      ((#\M)
                       (qif-split:set-memo! default-split value))
                      
                      ;; I : share price (stock transactions)
                      ((#\I)
                       (qif-xtn:set-share-price! current-xtn value))
                      
                      ;; Q : share price (stock transactions)
                      ((#\Q)
                       (qif-xtn:set-num-shares! current-xtn value))
                      
                      ;; Y : name of security (stock transactions)
                      ((#\Y)
                       (qif-xtn:set-security-name! current-xtn value))
                      
                      ;; O : commission (stock transactions)
                      ((#\O)
                       (qif-xtn:set-commission! current-xtn value))
                      
                      ;; L : category 
                      ((#\L)
                       (qif-split:set-category! default-split value))
                      
                      ;; S : split category 
                      ((#\S)
                       (set! current-split (make-qif-split))
                       (qif-split:set-category! current-split value)
                       (qif-xtn:set-splits! 
                        current-xtn
                        (cons current-split (qif-xtn:splits current-xtn))))
                      
                      ;; E : split memo 
                      ((#\E)
                       (if current-split 
                           (qif-split:set-memo! current-split value)))
                      
                      ;; $ : split amount (if there are splits)
                      ((#\$)
                       (if current-split
                           (qif-split:set-amount! current-split value)))
                      
                      ;; ^ : end-of-record 
                      ((#\^)
                       (if (null? (qif-xtn:splits current-xtn)) 
                           (qif-xtn:set-splits! current-xtn
                                                (list default-split)))
                       (if first-xtn 
                           (begin 
                             (qif-file:process-opening-balance-xtn 
                              self current-xtn qstate-type)
                             (set! first-xtn #f)))
                       
                       (if (and (eq? qstate-type 'type:invst)
                                (not (qif-xtn:security-name current-xtn)))
                           (qif-xtn:set-security-name! current-xtn ""))
                       
                       (if current-account-name 
                           (qif-xtn:set-from-acct! current-xtn 
                                                   current-account-name) 
                           (qif-xtn:set-from-acct! 
                            current-xtn (qif-file:default-account self)))
                       (if (qif-xtn:date current-xtn)
                           (qif-file:add-xtn! self current-xtn))
                       (set! current-xtn (make-qif-xtn))
                       (set! default-split (make-qif-split)))))
                   
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; Class transactions 
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ((type:class)
                    (case tag
                      ;; N : name 
                      ((#\N)
                       (qif-class:set-name! current-xtn value))
                      
                      ;; D : description 
                      ((#\D)
                       (qif-class:set-description! current-xtn value))
                      
                      ;; R : tax copy designator (ignored for now)
                      ((#\R)
                       #t)

                      ;; end-of-record
                      ((#\^)
                       (qif-file:add-class! self current-xtn)
                       (set! current-xtn (make-qif-class)))
                      
                      (else
                       (display "qif-file:read-file : unknown Class slot ")
                       (display tag) 
                       (display " .. continuing anyway.")
                       (newline))))
                   
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; Account definitions
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   
                   ((account)
                    (case tag
                      ((#\N)
                       (qif-acct:set-name! current-xtn value))
                      ((#\D)
                       (qif-acct:set-description! current-xtn value))
                      ((#\T)
                       (qif-acct:set-type! current-xtn value))
                      ((#\L)
                       (qif-acct:set-limit! current-xtn value))
                      ((#\B)
                       (qif-acct:set-budget! current-xtn value))
                      ((#\^)
                       (if (not ignore-accounts)
                           (set! current-account-name 
                                 (qif-acct:name current-xtn)))
                       (qif-file:add-account! self current-xtn)
                       (set! current-xtn (make-qif-acct)))))
                   
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; Category (Cat) transactions 
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   
                   ((type:cat)
                    (case tag
                      ;; N : category name 
                      ((#\N)
                       (qif-cat:set-name! current-xtn value))
                      
                      ;; D : category description 
                      ((#\D)
                       (qif-cat:set-description! current-xtn value))
                      
                      ;; E : is this a taxable category?
                      ((#\T)
                       (qif-cat:set-taxable! current-xtn #t))
                      
                      ;; E : is this an expense category?
                      ((#\E)
                       (qif-cat:set-expense-cat! current-xtn #t))
                      
                      ;; I : is this an income category? 
                      ((#\I)
                       (qif-cat:set-income-cat! current-xtn #t))
                      
                      ;; R : what is the tax rate (from some table?
                      ;; seems to be an integer)
                      ((#\R)
                       (qif-cat:set-tax-class! current-xtn value))
                      
                      ;; B : budget amount.  not really supported. 
                      ((#\B)
                       (qif-cat:set-budget-amt! current-xtn value))
                      
                      ;; end-of-record
                      ((#\^)
                       (qif-file:add-cat! self current-xtn)
                       (set! current-xtn (make-qif-cat)))
                      
                      (else
                       (display "qif-file:read-file : unknown Cat slot ")
                       (display tag) 
                       (display " .. continuing anyway") (newline))))
                   
                   ;; trying to sneak one by, eh? 
                   (else 
                    (if (not qstate-type)
                        (begin
                          (display "line = ") (display line) (newline)
                          (display "qif-file:read-file : ")
                          (display "file does not appear to be a QIF file.")
                          (newline)
                          (set! 
                           return-val 
                           (list #f "File does not appear to be a QIF file."))
                          (set! heinous-error #t))))))
                
                ;; this is if we read a normal (non-null, non-eof) line...
                (if (not heinous-error)
                    (line-loop)))
              
              ;; and this is if we read a null or eof line 
              (if (and (not heinous-error)
                       (not (eof-object? line)))
                  (line-loop))))))
    
    ;; now reverse the transaction list so xtns are in the same order that 
    ;; they were in the file.  This is important in a few cases. 
    (qif-file:set-xtns! self (reverse (qif-file:xtns self)))
    return-val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:process-opening-balance-xtn self xtn
;;
;;  this gets called for the first transaction after a !Type: tag.
;; 
;;  if the first transaction after a !Type: tag has a payee of
;;  "Opening Balance", we have to massage the transaction a little.
;;  The meaning of an OB transaction is "transfer from Equity to the
;;  account specified in the L line." idiomatically, ms-money and some
;;  others use this transaction instead of an Account record to
;;  specify "this" account (the from-account for all following
;;  transactions), so we have to allow for that.
;;
;;  even if the payee isn't "Opening Balance", we know that if there's
;;  no default from-account by this time, we need to set one.  In that
;;  case, we set the default account based on the file name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:process-opening-balance-xtn self xtn type) 
  (let ((payee (qif-xtn:payee xtn))
        (category (qif-split:category 
                   (car (qif-xtn:splits xtn))))
        (cat-is-acct? (qif-split:category-is-account? 
                       (car (qif-xtn:splits xtn))))
        (security (qif-xtn:security-name xtn)))
    (if (and payee (string? payee)              
             (not security)
             (string=? (string-remove-trailing-space payee)
                       "Opening Balance")
             cat-is-acct?)
        ;; this is an explicit "Opening Balance" transaction.  we need
        ;; to change the category to point to the equity account that
        ;; the opening balance comes from.
        (begin
          (qif-split:set-category-private! 
           (car (qif-xtn:splits xtn))
           (default-equity-account))
          (qif-split:set-category-is-account?! 
           (car (qif-xtn:splits xtn)) #t) 
          (if (eq? (qif-file:default-account self) 'unknown)
              (qif-file:set-default-account! self category)))
        
        ;; it's not an OB transaction.  Still set the default 
        ;; account if there isn't one. 
        (if (eq? (qif-file:default-account self) 'unknown)
            (begin 
              (qif-file:set-default-account!
               self (qif-file:path-to-accountname self))
              (case type
                ((type:invst)
                 (qif-file:set-default-account-type! self GNC-STOCK-TYPE))
                (else
                 (qif-file:set-default-account-type! self GNC-BANK-TYPE))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:parse-fields self 
;;  take a previously-read qif file and convert fields
;;  from strings to the appropriate type. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-fields self)
  (let* ((error #f)
         (all-ok #f)
         (start-time #f)
         (end-time #f)
         (set-error 
          (lambda (e) (set! error e)))
         (errlist-to-string 
          (lambda (lst)
            (with-output-to-string 
              (lambda ()
                (for-each 
                 (lambda (elt)
                   (display elt))
                 lst))))))
    (set! start-time (gettimeofday))
    (and 
     ;; fields of categories. 
     (check-and-parse-field 
      qif-cat:tax-class qif-cat:set-tax-class! 
      qif-parse:check-number-format '(decimal comma)
      qif-parse:parse-number/format (qif-file:cats self)
      set-error)
     
     (check-and-parse-field 
      qif-cat:budget-amt qif-cat:set-budget-amt! 
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:cats self)
      set-error)
     
     ;; fields of accounts 
     (check-and-parse-field 
      qif-acct:limit qif-acct:set-limit! 
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:accounts self)
      set-error)
     
     (check-and-parse-field 
      qif-acct:budget qif-acct:set-budget! 
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:accounts self)
      set-error)
    
     (parse-field 
      qif-acct:type qif-acct:set-type!
      qif-parse:parse-acct-type (qif-file:accounts self)
      set-error)

     ;; fields of transactions 
     (check-and-parse-field 
      qif-xtn:date qif-xtn:set-date! 
      qif-parse:check-date-format '(m-d-y d-m-y y-m-d y-d-m) 
      qif-parse:parse-date/format 
      (qif-file:xtns self)
      set-error)
     
     (parse-field 
      qif-xtn:cleared qif-xtn:set-cleared!
      qif-parse:parse-cleared-field (qif-file:xtns self) set-error)

     (parse-field 
      qif-xtn:action qif-xtn:set-action!
      qif-parse:parse-action-field (qif-file:xtns self) set-error)
     
     (check-and-parse-field 
      qif-xtn:share-price qif-xtn:set-share-price!
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:xtns self)
      set-error)
     
     (check-and-parse-field 
      qif-xtn:num-shares qif-xtn:set-num-shares!
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:xtns self)
      set-error)
     
     (check-and-parse-field 
      qif-xtn:commission qif-xtn:set-commission!
      qif-parse:check-number-format '(decimal comma) 
      qif-parse:parse-number/format (qif-file:xtns self)
      set-error)
     
     ;; this one's a little tricky... it checks and sets all the 
     ;; split amounts for the transaction together.
     (check-and-parse-field 
      qif-xtn:split-amounts qif-xtn:set-split-amounts!
      qif-parse:check-number-formats '(decimal comma) 
      qif-parse:parse-numbers/format (qif-file:xtns self)
      set-error)
     
     (begin 
       (set! all-ok #t)
       #t))
    
    (set! end-time (gettimeofday))

    (cond ((list? error)
           (list all-ok (errlist-to-string error)))
          (error 
           (list all-ok error))
          (#t #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-field 
;;  a simplified version of check-and-parse-field which just calls
;;  the parser on every instance of the field in the set of objects 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-field getter setter parser objects errormsg)
  (for-each 
   (lambda (obj)
     (let ((unparsed (getter obj)))
       (if (and unparsed (string? unparsed))
           (setter obj (parser unparsed)))))
   objects)
  #t)
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  check-and-parse-field
;;
;;  this is a semi-generic routine to apply a format check and 
;;  parsing routine to fields that can have multiple possible 
;;  formats.  In this case, any amount field cam be decimal or 
;;  comma radix and the date firled can be any of several possible
;;  types. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-and-parse-field getter setter checker 
                               formats parser objects errormsg)
  ;; first find the right format for the field
  (let ((do-parsing #f)
        (retval #t)
        (format #f))        
    ;; loop over objects.  If the formats list ever gets down 
    ;; to 1 element, we can stop right there. 
    (if (not (null? objects))
        (let loop ((current (car objects))
                   (rest (cdr objects)))
          (let ((val (getter current)))
            (if val 
                (begin 
                  (set! do-parsing #t)                  
                  (set! formats (checker val formats)))))
          (if (and (not (null? formats))
                   (not (null? (cdr formats)))
                   (not (null? rest)))
              (loop (car rest) (cdr rest)))))
    
    ;; if there's nothing left in formats, there's no format that will
    ;; fit all the values for a given field.  We have to give up at
    ;; that point.  If there are multiple items in formats, we just
    ;; take the default (first) item in the list.  This is not super
    ;; great.
    (cond 
     ((null? formats) 
      (errormsg "Data for number or date does not match a known format.")
      (set! retval #f)
      (set! do-parsing #f))
     ((and (not (null? (cdr formats))) do-parsing)
      ;; there are multiple formats that fit.  If they all produce the
      ;; same interpretation for every data point in the set, then
      ;; just ignore the format ambiguity.  Otherwise, it's really an
      ;; error.  ATM since there's no way to correct the error let's 
      ;; just leave it be.
      (all-formats-equivalent? getter parser formats objects errormsg)      
      (set! format (car formats)))
     (#t 
      (set! format (car formats))))
    
    ;; do-parsing is false if there were no objects with non-#f values
    ;; in the field.  We would have had to look at all of them once,
    ;; but at least not twice.
    (if do-parsing
        (for-each 
         (lambda (current)
           (let ((val (getter current))
                 (parsed #f))                     
             (if val
                 (begin 
                   (set! parsed (parser val format))                 
                   (if parsed 
                       (setter current parsed)
                       (begin 
                         (set! retval #f)
                         (errormsg 
                          "Data format inconsistent in QIF file.")))))))
         objects))
    retval))

(define (all-formats-equivalent? getter parser formats objects errormsg)
  (let ((all-ok #t))
    (let obj-loop ((objlist objects))
      (let* ((unparsed (getter (car objlist)))
             (parsed #f))        
        (if (string? unparsed)
            (begin 
              (set! parsed (parser unparsed (car formats)))
              (for-each 
               (lambda (fmt)
                 (let ((this-parsed (parser unparsed fmt)))
                   (if (not (equal? parsed this-parsed))
                       (begin 
                         (set! all-ok #f) 
                         (errormsg 
                          (list "Parse ambiguity : between formats "
                                formats "\nValue " unparsed " could be "
                                parsed " or " this-parsed 
                                "\nand no evidence exists to distinguish." 
                                "\nUsing " parsed ". "
                                "\nSee help for more info."))))))
               (cdr formats))))
        (if (and all-ok (not (null? (cdr objlist))))
            (obj-loop (cdr objlist)))))
    all-ok))
