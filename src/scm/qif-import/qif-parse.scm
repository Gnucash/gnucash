;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-parse.scm
;;;  routines to parse values and dates in QIF files. 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-parse.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-split:parse-category 
;;  this one just gets nastier and nastier. 
;;  ATM we return a list of 3 elements: parsed category name 
;;  (without [] if it was an account name), bool stating if it 
;;  was an account name, and string representing the class name 
;;  (or #f if no class).
;;  gosh, I love regular expressions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define qif-category-compiled-rexp 
  (make-regexp "(\\[)?([^]/]*)(]?)(/?)(.*)"))

(define (qif-split:parse-category self value)
  (let ((match (regexp-exec qif-category-compiled-rexp value)))
    (if match
        (begin 
          (list (match:substring match 2)
                (if (and (match:substring match 1)
                         (match:substring match 3))
                    #t #f)
                (if (match:substring match 4)
                    (match:substring match 5)
                    #f)))
        (begin 
          (display "qif-split:parse-category : can't parse ")
          (display value) (newline)
          (list "" #f #f)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:fix-year 
;;  this is where we handle y2k fixes etc.  input is a string
;;  containing the year ("00", "2000", and "19100" all mean the same
;;  thing). output is an integer representing the year in the C.E.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:fix-year self year-string) 
  (let ((fixed-string #f)
        (post-read-value #f)
        (y2k-fixed-value #f))    

    ;; quicken prints 2000 as "' 0" for at least some versions. 
    ;; thanks dave p for reporting this. 
    (if (eq? (string-ref year-string 0) #\')
        (begin 
          (display "qif-file:fix-year : found a weird QIF Y2K year : |")
          (display year-string)
          (display "|") (newline)
          (set! fixed-string 
                (substring year-string 2 (string-length year-string))))
        (set! fixed-string year-string))
    
    ;; now the string should just have a number in it plus some 
    ;; optional trailing space. 
    (set! post-read-value 
          (with-input-from-string fixed-string 
            (lambda () (read))))
    
    (cond 
     ;; 2-digit numbers less than the window size are interpreted to 
     ;; be post-2000.
     ((and (integer? post-read-value)
           (< post-read-value (qif-file:y2k-threshold self)))
      (set! y2k-fixed-value (+ 2000 post-read-value)))
     
     ;; there's a common bug in printing post-2000 dates that 
     ;; prints 2000 as 19100 etc.  
     ((and (integer? post-read-value)
           (> post-read-value 19000))
      (set! y2k-fixed-value (+ 1900 (- post-read-value 19000))))
     
     ;; normal dates represented in unix years (i.e. year-1900, so
     ;; 2000 => 100.)  We also want to allow full year specifications,
     ;; (i.e. 1999, 2001, etc) and there's a point at which you can't
     ;; determine which is which.  this should eventually be another
     ;; field in the qif-file struct but not yet.  mktime in scheme
     ;; doesn't deal with dates before December 14, 1901, at least for
     ;; now, so let's give ourselves until at least 3802 before this
     ;; does the wrong thing. 
     ((and (integer? post-read-value)
           (< post-read-value 1902))           
      (set! y2k-fixed-value (+ 1900 post-read-value)))
     
     ;; this is a normal, 4-digit year spec (1999, 2000, etc).
     ((integer? post-read-value)
      (set! y2k-fixed-value post-read-value))
     
     ;; No idea what the string represents.  Maybe a new bug in Quicken! 
     (#t 
      (display "qif-file:fix-year : ay caramba! What is this? |")
      (display year-string)
      (display "|") (newline)))

    y2k-fixed-value))
                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-acct-type : set the type of the account, using gnucash 
;;  conventions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-acct-type self read-value)
  (let ((mangled-string 
         (string-downcase! (string-remove-trailing-space 
                            (string-remove-leading-space read-value)))))
    (cond
     ((string=? mangled-string "bank")
      GNC-BANK-TYPE)
     ((string=? mangled-string "cash")
      GNC-CASH-TYPE)
     ((string=? mangled-string "ccard")
      GNC-CCARD-TYPE)
     ((string=? mangled-string "invst")
      GNC-STOCK-TYPE)
     ((string=? mangled-string "oth a")
      GNC-ASSET-TYPE)
     ((string=? mangled-string "oth l")
      GNC-LIABILITY-TYPE)
     (#t read-value))))

(define (qif-file:state-to-account-type self qstate)
  (cond ((eq? qstate 'type:bank)
         GNC-BANK-TYPE)
        ((eq? qstate 'type:cash)
         GNC-CASH-TYPE)
        ((eq? qstate 'type:ccard)
         GNC-CCARD-TYPE)
        ((eq? qstate 'type:invst)
         GNC-STOCK-TYPE)        
        ((eq? qstate '#{type:oth\ a}#)
         GNC-ASSET-TYPE)
        ((eq? qstate '#{type:oth\ l}#)
         GNC-LIABILITY-TYPE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-bang-field : the bang fields switch the parse context for 
;;  the qif file. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-bang-field self read-value)
  (string->symbol (string-downcase! 
                   (string-remove-trailing-space read-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-cleared-field : in a C (cleared) field in a QIF transaction,
;;  * means cleared, x or X means reconciled, and ! or ? mean some 
;;  budget related stuff I don't understand. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-cleared-field self read-value)
  
  (if (and (string? read-value) 
           (> (string-length read-value) 0))
      (let ((secondchar (string-ref read-value 0)))
        (cond ((eq? secondchar #\*)
               'cleared)
              ((or (eq? secondchar #\x)
                   (eq? secondchar #\X))
               'reconciled)
              ((or (eq? secondchar #\?)
                   (eq? secondchar #\!))
               'budgeted)
              (#t 
               #f)))
      #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:parse-date 
;;
;;  If the date format is specified, use that; otherwise, try to guess
;;  the format.  When the format is being guessed, I don't actually do
;;  any translation to a numeric format; that's saved for a second
;;  pass (calling qif-bank-xtn:reparse on every transaction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-date self date-string)
  (if (or (not (string? date-string))
          (not (> (string-length date-string) 0)))
      (begin 
        (display "qif-import: very bogus QIF date in transaction.") (newline)
        (display "qif-import: Substituting 1/1/2999 for date.") (newline)
        (set! date-string "1/1/2999")))

  (let ((date-parts '())
        (numeric-date-parts '())
        (retval date-string)
        (match 
         (string-match "([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+)"
                       date-string)))
    (if match
        (set! date-parts (list (match:substring match 1)
                               (match:substring match 2)
                               (match:substring match 3))))

    ;; get the strings into numbers (but keep the strings around)
    (set! numeric-date-parts
          (map (lambda (elt)
                 (with-input-from-string elt
					 (lambda () (read))))
               date-parts))
    
    (cond 
     ;; if the date parts list doesn't have 3 parts, we're in 
     ;; trouble 
     ((not (eq? 3 (length date-parts)))
      (begin 
        (display "qif-file:parse-date : can't interpret date ")
        (display date-string) (newline)))
     
     ;; if the format is unknown, don't try to fully interpret the 
     ;; number, just look for a good guess or an inconsistency with 
     ;; the current guess. 
     ((and (eq? (qif-file:date-format self) 'unknown)
           (not (eq? (qif-file:guessed-date-format self) 
                     'inconsistent)))
      (cond 
       ;; we currently think the date format is m/d/y
       ((eq? (qif-file:guessed-date-format self) 'm-d-y)
        (let ((m (car numeric-date-parts))
              (d (cadr numeric-date-parts)))
          (if (or (not (number? m)) (not (number? d)) (> m 12) (> d 31))
              (qif-file:set-guessed-date-format! self 'inconsistent))))
       
       ;; current guess is d/m/y
       ((eq? (qif-file:guessed-date-format self) 'd-m-y)
        (let ((d (car numeric-date-parts))
              (m (cadr numeric-date-parts)))
          (if (or (not (number? m)) (not (number? d)) (> m 12) (> d 31))
              (qif-file:set-guessed-date-format! self 'inconsistent))))
       
       ;; current guess is y/m/d 
       ((eq? (qif-file:guessed-date-format self) 'y-m-d)
        (let ((m (cadr numeric-date-parts))
              (d (caddr numeric-date-parts)))
          (if (or (not (number? m)) (not (number? d)) (> m 12) (> d 31))
              (qif-file:set-guessed-date-format! self 'inconsistent))))
       
       ;; current guess is y/d/m (is this really possible?)
       ((eq? (qif-file:guessed-date-format self) 'y-m-d)
        (let ((d (cadr numeric-date-parts))
              (m (caddr numeric-date-parts)))
          (if (or (not (number? m)) (not (number? d)) (> m 12) (> d 31))
              (qif-file:set-guessed-date-format! self 'inconsistent))))
       
       ;; no guess currently.  See if we can find a smoking gun in 
       ;; the date format.  For dates like 11-9-11 just don't try to 
       ;; guess. 
       ((eq? (qif-file:guessed-date-format self) 'unknown)
        (let ((possibilities '(m-d-y d-m-y y-m-d y-d-m))
              (n1 (car numeric-date-parts))
              (n2 (cadr numeric-date-parts))
              (n3 (caddr numeric-date-parts)))

          ;; filter the possibilities to eliminate (hopefully)
          ;; all but one
          (if (or (not (number? n1)) (> n1 12))
              (set! possibilities (delq 'm-d-y possibilities)))
          (if (or (not (number? n1)) (> n1 31))
              (set! possibilities (delq 'd-m-y possibilities)))

          (if (or (not (number? n2)) (> n2 12))
              (begin 
                (set! possibilities (delq 'd-m-y possibilities))
                (set! possibilities (delq 'y-m-d possibilities))))
          (if (or (not (number? n2)) (> n2 31))
              (begin 
                (set! possibilities (delq 'm-d-y possibilities))
                (set! possibilities (delq 'y-d-m possibilities))))

          (if (or (not (number? n3)) (> n3 12))
              (set! possibilities (delq 'y-d-m possibilities)))
          (if (or (not (number? n3)) (> n3 31))
              (set! possibilities (delq 'y-m-d possibilities)))
          
          ;; if there's exactly one possibility left, we've got a good
          ;; guess.  if there are no possibilities left, the date 
          ;; is somehow inconsistent.  More than one, do nothing.
          (cond  ((eq? (length possibilities) 1)
                  (qif-file:set-guessed-date-format! self (car possibilities)))
                 ((eq? (length possibilities) 0)
                  (display "qif-file:parse-date : can't interpret date ")
                  (display date-string)
                  (newline)
                  (qif-file:set-guessed-date-format! self 'inconsistent)))))))
     
     ;; we think we know the date format.  Make sure the data is 
     ;; consistent with that. 
     ((eq? (qif-file:date-format self) 'd-m-y)
      (let ((d (car numeric-date-parts))
            (m (cadr numeric-date-parts))
            (y (qif-file:fix-year self (caddr date-parts))))
        (if (and (integer? d) (integer? m) (integer? y)
                 (<= m 12) (<= d 31))
            (set! retval (list d m y))
            (begin 
              (display "qif-file:parse-date : format is d/m/y, but date is ")
              (display date-string) (newline)))))
     
     ((eq? (qif-file:date-format self) 'm-d-y)
      (let ((m (car numeric-date-parts))
            (d (cadr numeric-date-parts))
            (y (qif-file:fix-year self (caddr date-parts))))
        (if (and (integer? d) (integer? m) (integer? y)
                 (<= m 12) (<= d 31))
            (set! retval (list d m y))
            (begin 
              (display "qif-file:parse-date : format is m/d/y, but date is ")
              (display date-string) (newline)))))
     
     ((eq? (qif-file:date-format self) 'y-m-d)
      (let ((y (qif-file:fix-year self (car date-parts)))
            (m (cadr numeric-date-parts))
            (d (caddr numeric-date-parts))))
      (if (and (integer? d) (integer? m) (integer? y)
               (<= m 12) (<= d 31))
          (set! retval (list d m y))
          (begin 
            (display "qif-file:parse-date : format is y/m/d, but date is ")
            (display date-string) (newline))))
     
     ((eq? (qif-file:date-format self) 'y-d-m)
      (let ((y (qif-file:fix-year self (car date-parts)))
            (d (cadr numeric-date-parts))
            (m (caddr numeric-date-parts))))
      (if (and (integer? d) (integer? m) (integer? y)
               (<= m 12) (<= d 31))
          (set! retval (list d m y))
          (begin 
            (display "qif-file:parse-date : format is y/m/d, but date is ")
            (display date-string) (newline)))))
    retval))

(define (qif-file:parse-string self str)
  (if (or (not (string? str))
          (not (> (string-length str) 0)))
      (set! str "   "))

  (string-remove-leading-space (string-remove-trailing-space str)))

(define (qif-file:parse-value self value-string)
  (if (or (not (string? value-string))
          (not (> (string-length value-string) 0)))
      (set! value-string "0"))
  
  (let ((comma-index (string-rindex value-string #\,))
        (decimal-index (string-rindex value-string #\.))
        (comma-count (string-char-count value-string #\,))
        (decimal-count (string-char-count value-string #\.)))

    ;; if we don't know the radix format, it might be appropriate to
    ;; guess.  guessed radix format doesn't affect parsing at all
    ;; until you set the radix-format from the guessed-radix-format
    ;; and call reparse-values on all the values.

    (if (and (eq? (qif-file:radix-format self) 'unknown)
             (not (eq? (qif-file:guessed-radix-format self) 'inconsistent)))
        (cond 
         ;; already think it's decimal 
         ((eq? (qif-file:guessed-radix-format self) 'decimal)
          (if (or (> decimal-count 1)
                  (and decimal-index comma-index
                       (> comma-index decimal-index)))
              (begin 
                (qif-file:set-guessed-radix-format! self 'inconsistent)
                (display "this QIF file has inconsistent radix notation!")
                (newline))))
         
         ;; already think it's comma 
         ((eq? (qif-file:guessed-radix-format self) 'comma)
          (if (or (> comma-count 1)
                  (and decimal-index comma-index
                       (> decimal-index comma-index)))
              (begin 
                (qif-file:set-guessed-radix-format! self 'inconsistent)
                (display "this QIF file has inconsistent radix notation!")
                (newline))))
         
         ;; don't know : look for numbers that are giveaways. 
         ((eq? (qif-file:guessed-radix-format self) 'unknown)
          ;; case 1: there's a decimal and a comma, and the 
          ;; decimal is to the right of the comma, and there's 
          ;; only one decimal : it's a decimal number.
          (if (and decimal-index comma-index 
                   (> decimal-index comma-index)
                   (eq? decimal-count 1))
              (qif-file:set-guessed-radix-format! self 'decimal))

          ;; case 2: the opposite. 
          (if (and decimal-index comma-index 
                   (> comma-index decimal-index)
                   (eq? comma-count 1))
              (qif-file:set-guessed-radix-format! self 'comma))

          ;; case 3: there's no decimal and more than one comma:
          ;; it's a decimal number.  I wish I had more transactions
          ;; like this! 
          (if (and (eq? decimal-count 0)
                   (> comma-count 1))
              (qif-file:set-guessed-radix-format! self 'decimal))
          
          ;; case 4: the opposite (no comma, multiple decimals)
          (if (and (eq? comma-count 0)
                   (> decimal-count 1))
              (qif-file:set-guessed-radix-format! self 'comma))

          ;; case 5: one decimal, no commas, and not-3 digits
          ;; after it --> decimal.
          (if (and (eq? comma-count 0)
                   (eq? decimal-count 1)
                   (not (eq? (- (string-length value-string)
                                decimal-index)
                             4)))
              (qif-file:set-guessed-radix-format! self 'decimal))

          ;; case 6: the opposite --> comma
          (if (and (eq? comma-count 1)
                   (eq? decimal-count 0)
                   (not (eq? (- (string-length value-string)
                                comma-index)
                             4)))
              (begin 
                (display "hey!") (display comma-count) 
                (display comma-index) (display (string-length value-string))
                (newline)
                (qif-file:set-guessed-radix-format! self 'comma))))))
    
    (cond
     ;; decimal radix (US format)
     ;; number can't have more than one ., and the rightmost 
     ;; . must be to the right of the rightmost ,
     ;; , are ignored otherwise
     ((eq? 'decimal (qif-file:radix-format self))
      (if (or (and decimal-count  
                   (> decimal-count 1))
              (and decimal-index comma-index
                   (> comma-index decimal-index)))
          (error "badly-formed decimal-radix number" value-string)
          (+ 0.0 
             (with-input-from-string (string-remove-char value-string #\,)
               (lambda () (read))))))

     ;; comma radix (German format)
     ;; number can't have more than one , and the rightmost 
     ;; , must be to the right of the rightmost .
     ;; . are ignored otherwise.  Substitute . for , before 
     ;; parsing. 
     ((eq? 'comma (qif-file:radix-format self))
      (if (or (and comma-count 
                   (> comma-count 1))
              (and decimal-index comma-index
                   (> decimal-index comma-index)))
          (error "badly formed comma-radix number" value-string)
          (+ 0.0 
             (with-input-from-string (string-replace-char!
                                      (string-remove-char value-string #\.) 
                                      #\, #\.)
               (lambda () (read))))))
     
     ;; unknown radix - store the string and we can process it 
     ;; later. 
     (#t         
      value-string))))
