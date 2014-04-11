;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-parse.scm
;;;  routines to parse values and dates in QIF files. 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GNC-BANK-TYPE 0)
(define GNC-CASH-TYPE 1)
(define GNC-ASSET-TYPE 2)
(define GNC-LIABILITY-TYPE 4)
(define GNC-CCARD-TYPE 3)
(define GNC-STOCK-TYPE 5)
(define GNC-MUTUAL-TYPE 6)
(define GNC-INCOME-TYPE 8)
(define GNC-EXPENSE-TYPE 9)
(define GNC-EQUITY-TYPE 10)



(define qif-category-compiled-rexp 
  (make-regexp "^ *(\\[)?([^]/|]*)(]?)(/([^|]*))?(\\|(\\[)?([^]/]*)(]?)(/(.*))?)? *$"))

(define qif-date-compiled-rexp 
  (make-regexp "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*$"))

(define decimal-radix-regexp
  (make-regexp 
   "^ *\\$?-?\\$?[0-9]+$|^ *\\$?-?\\$?[0-9]?[0-9]?[0-9]?(,[0-9][0-9][0-9])*(\\.[0-9]*)? *$|^ *\\$?-?\\$?[0-9]+\\.[0-9]* *$"))

(define comma-radix-regexp
  (make-regexp 
   "^ *\\$?-?\\$?[0-9]+$|^ *\\$?-?\\$?[0-9]?[0-9]?[0-9]?(\\.[0-9][0-9][0-9])*(,[0-9]*) *$|^ *\\$?-?\\$?[0-9]+,[0-9]* *$"))

(define integer-regexp (make-regexp "^\\$?-?\\$?[0-9]+ *$"))

(define remove-trailing-space-rexp 
  (make-regexp "^(.*[^ ]+) *$"))

(define remove-leading-space-rexp 
  (make-regexp "^ *([^ ].*)$"))

(define (string-remove-trailing-space str)
  (let ((match (regexp-exec remove-trailing-space-rexp str)))
    (if match
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-trailing-space! str)
  (let ((match (regexp-exec remove-trailing-space-rexp str)))
    (if match
        (match:substring match 1)
        "")))

(define (string-remove-leading-space str)
  (let ((match (regexp-exec remove-leading-space-rexp str)))
    (if match 
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-leading-space! str)
  (let ((match (regexp-exec remove-leading-space-rexp str)))
    (if match 
        (match:substring match 1)
        "")))

(define (string-remove-char str char)
  (let ((rexpstr 
         (case char  
           ((#\.) "\\.")
           ((#\^) "\\^")
           ((#\$) "\\$")
           ((#\*) "\\*")
           ((#\+) "\\+")
           ((#\\) "\\\\")
           ((#\?) "\\?")
           (else 
             (make-string 1 char)))))
    (regexp-substitute/global #f rexpstr str 'pre 'post)))

(define (string-replace-char! str old new)
  (let ((rexpstr 
         (if (not (eq? old #\.))
             (make-string 1 old)
             "\\."))
        (newstr (make-string 1 new)))
    (regexp-substitute/global #f rexpstr str 'pre newstr 'post)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:parse-category 
;;  we return a list of 6 elements: 
;;    0 parsed category name (without [] if it was an account name)
;;    1 bool stating if it was an account name (a transfer)
;;    2 class of account or #f 
;;    3 string representing the "miscx category" if any 
;;    4 bool if miscx category is an account
;;    5 class of miscx cat or #f 
;;  gosh, I love regular expressions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-category value)
  (if (not (string? value))
      (throw 'qif-io:arg-type 'qif-io:parse-category 'string value))
  
  (let ((match (regexp-exec qif-category-compiled-rexp value)))
    ;; what the substrings mean:
    ;; 1 the opening [ for a transfer
    ;; 2 the category
    ;; 3 the closing ]
    ;; 4 the class /
    ;; 5 the class 
    ;; 6 the miscx expression (whole thing)
    ;; 7 the opening [ 
    ;; 8 the miscx category
    ;; 9 the closing ]
    ;; 10 the class /
    ;; 11 the class 
    (if match
        (let ((rv
               (list (match:substring match 2)
                     (if (and (match:substring match 1)
                              (match:substring match 3))
                         #t #f)
                     (if (match:substring match 4)
                         (match:substring match 5)
                         #f)
                     ;; miscx category name 
                     (if (match:substring match 6)
                         (match:substring match 8)
                         #f)
                     ;; is it an account? 
                     (if (and (match:substring match 7)
                              (match:substring match 9))
                         #t #f)
                     (if (match:substring match 10)
                         (match:substring match 11)
                         #f))))
          rv)
        (throw 'qif-io:parse-failed 'qif-io:parse-category value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:parse-year 
;;  this is where we handle y2k fixes etc.  input is a string
;;  containing the year ("00", "2000", and "19100" all mean the same
;;  thing). output is an integer representing the year in the C.E.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-year year-string y2k-threshold) 
  (if (not (string? year-string))
      (throw 'qif-io:arg-type 'qif-io:parse-year 'string year-string))
  (if (not (number? y2k-threshold))
      (throw 'qif-io:arg-type 'qif-io:parse-year 'number y2k-threshold))
  
  (let ((fixed-string #f)
        (post-read-value #f)
        (y2k-fixed-value #f))    
    
    ;; quicken prints 2000 as "' 0" for at least some versions. 
    ;; thanks dave p for reporting this. 
    (if (eq? (string-ref year-string 0) #\')
        (begin 
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
           (< post-read-value y2k-threshold))
      (set! y2k-fixed-value (+ 2000 post-read-value)))
     
     ;; there's a common bug in printing post-2000 dates that 
     ;; prints 2000 as 19100 etc.  
     ((and (integer? post-read-value)
           (> post-read-value 19000))
      (set! y2k-fixed-value (+ 1900 (- post-read-value 19000))))
     
     ;; normal dates represented in unix years (i.e. year-1900, so
     ;; 2000 => 100.)  We also want to allow full year specifications,
     ;; (i.e. 1999, 2001, etc) and there's a point at which you can't
     ;; determine which is which.  mktime in scheme doesn't deal with
     ;; dates before December 14, 1901, at least for now, so let's
     ;; give ourselves until at least 3802 before this does the wrong
     ;; thing.
     ((and (integer? post-read-value)
           (< post-read-value 1902))           
      (set! y2k-fixed-value (+ 1900 post-read-value)))
     
     ;; this is a normal, 4-digit year spec (1999, 2000, etc).
     ((integer? post-read-value)
      (set! y2k-fixed-value post-read-value))
     
     ;; No idea what the string represents.  Maybe a new bug in Quicken! 
     (#t 
      (throw 'qif-io:parse-failed 'qif-io:parse-year year-string)))
    y2k-fixed-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-bang-field : the bang fields switch the parse context for 
;;  the qif file. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-bang-field read-value)
  (if (not (string? read-value))
      (throw 'qif-io:arg-type 'qif-io:parse-bang-field 'string read-value))
  (let ((bang-field (string-downcase! 
		     (string-remove-trailing-space read-value))))
;; The QIF files output by the WWW site of Credit Lyonnais
;; begin by:   !type bank
;; instead of: !Type:bank
    (if (>= (string-length bang-field) 5)
	(if (string=? (substring bang-field 0 5) "type ")
	    (string-set! bang-field 4 #\:)))

    (string->symbol bang-field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-action-field : stock transaction actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-action-field read-value)
  (define (canonicalize string)
    (string->symbol 
     (string-downcase
      (string-remove-trailing-space! 
       (string-remove-leading-space! string)))))
  
  (if (not (string? read-value))
      (throw 'qif-io:arg-type 'qif-io:parse-action-field 'string read-value))
  
  (let ((action-symbol (canonicalize read-value)))
    (case action-symbol
      ;; buy 
      ((buy kauf)
       'buy)
      ((buyx kaufx)
       'buyx)
      ((cglong kapgew) ;; Kapitalgewinnsteuer
       'cglong)
      ((cglongx kapgewx)
       'cglongx)
      ((cgmid) ;; Kapitalgewinnsteuer
       'cgmid)
      ((cgmidx)
       'cgmidx)
      ((cgshort k.gewsp)
       'cgshort)
      ((cgshortx k.gewspx)
       'cgshortx)
      ((div)   ;; dividende
       'div) 
      ((divx)    
       'divx)
      ;; ((exercise)
      ;; 'exercise)
      ;; ((exercisx)
      ;; 'exercisx)
      ;; ((expire)
      ;; 'expire)
      ;; ((grant)
      ;; 'grant)
      ((int intinc aktzu) ;; zinsen
       'intinc)
      ((intx intincx)
       'intincx)
      ((margint)
       'margint)
      ((margintx)
       'margintx)
      ((miscexp)
       'miscexp)
      ((miscexpx)
       'miscexpx)
      ((miscinc)
       'miscinc)
      ((miscincx)
       'miscincx)
      ((reinvdiv)
       'reinvdiv)
      ((reinvint reinvzin)
       'reinvint)
      ((reinvlg reinvkur)
       'reinvlg)
      ((reinvmd)
       'reinvmd)
      ((reinvsg reinvksp)
       'reinvsg)
      ((reinvsh)
       'reinvsh)
      ((reminder erinnerg)
       'reminder)
      ((sell verkauf)  ;; verkaufen
       'sell)
      ((sellx verkaufx)
       'sellx)
      ((shrsin aktzu)
       'shrsin)
      ((shrsout aktab)
       'shrsout)
      ((stksplit aktsplit)
       'stksplit)
      ((xin) 
       'xin)
      ((xout) 
       'xout)
      ;; ((vest) 
      ;; 'vest)
      (else 
       (throw 'qif-io:parse-failed 'qif-io:parse-action-field read-value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-cleared-field : in a C (cleared) field in a QIF transaction,
;;  * means cleared, x or X means reconciled, and ! or ? mean some 
;;  budget related stuff I don't understand. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-cleared-field read-value)  
  (if (not (string? read-value))
      #\n  
      (if (> (string-length read-value) 0)
          (let ((secondchar (string-ref read-value 0)))
            (cond ((eq? secondchar #\*)
                   #\c)
                  ((or (eq? secondchar #\x)
                       (eq? secondchar #\X)
                       (eq? secondchar #\r)
                       (eq? secondchar #\R))                       
                   #\y)
                  ((or (eq? secondchar #\?)
                       (eq? secondchar #\!))
                   #\n)
                  (#t 
                   (throw 'qif-io:parse-failed 
                          'qif-io:parse-cleared-field read-value))))
          #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:check-date-format
;;  given a list of possible date formats, return a pruned list 
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:check-date-format date-string possible-formats)
  (if (not (string? date-string))
      (throw 'qif-io:arg-type 'qif-io:check-date-format 'string date-string))
  (if (not (list? possible-formats))
      (throw 'qif-io:arg-type 'qif-io:check-date-format 
             'list possible-formats))

  (let ((retval #f))
    (if (not (> (string-length date-string) 0))
        (set! retval possible-formats)
        (let ((date-parts '())
              (numeric-date-parts '())
              (match (regexp-exec qif-date-compiled-rexp date-string)))
          
          (if (not match)
              (throw 'qif-io:parse-failed 'qif-io:check-date-format 
                     date-string))

          (if (match:substring match 1)
              (set! date-parts (list (match:substring match 1)
                                     (match:substring match 2)
                                     (match:substring match 3)))
              (set! date-parts (list (match:substring match 4)
                                     (match:substring match 5)
                                     (match:substring match 6))))
                    
          ;; get the strings into numbers (but keep the strings around)
          (set! numeric-date-parts
                (map (lambda (elt)
                       (with-input-from-string elt
                         (lambda () (read))))
                     date-parts))
          
          (let ((possibilities possible-formats)
                (n1 (car numeric-date-parts))
                (n2 (cadr numeric-date-parts))
                (n3 (caddr numeric-date-parts)))
            
            ;; filter the possibilities to eliminate (hopefully)
            ;; all but one
            (if (or (not (number? n1)) (> n1 12))
                (set! possibilities (delq 'm-d-y possibilities)))
            (if (or (not (number? n1)) (> n1 31))
                (set! possibilities (delq 'd-m-y possibilities)))
            (if (or (not (number? n1)) (< n1 1))
                (set! possibilities (delq 'd-m-y possibilities)))
            (if (or (not (number? n1)) (< n1 1))
                (set! possibilities (delq 'm-d-y possibilities)))
            
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
            
            (if (or (not (number? n3)) (< n3 1))
                (set! possibilities (delq 'y-m-d possibilities)))
            (if (or (not (number? n3)) (< n3 1))
                (set! possibilities (delq 'y-d-m possibilities)))
            (set! retval possibilities))))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:parse-date/format 
;;  given a date string and a particular format spec, return a date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-date/format date-string format)
  (if (not (string? date-string))
      (throw 'qif-io:arg-type 'qif-io:parse-date/format 'string date-string))

  (let ((date-parts '())
        (numeric-date-parts '())
        (retval date-string)
        (match (regexp-exec qif-date-compiled-rexp date-string)))
    (if (not match) 
        (throw 'qif-io:parse-failed 'qif-io:parse-date/format date-string))

    (if (match:substring match 1)
        (set! date-parts (list (match:substring match 1)
                               (match:substring match 2)
                               (match:substring match 3)))
        (set! date-parts (list (match:substring match 4)
                               (match:substring match 5)
                               (match:substring match 6))))
    
    ;; get the strings into numbers (but keep the strings around)
    (set! numeric-date-parts
          (map (lambda (elt)
                 (with-input-from-string elt
                   (lambda () (read))))
               date-parts))
    
    ;; if the date parts list doesn't have 3 parts, we're in 
    ;; trouble 
    (if (not (eq? 3 (length date-parts)))
        ;; bomb out on bad parts 
        (throw 'qif-io:parse-failed 'qif-io:parse-date/format date-string)
        
        ;; otherwise try to interpret 
        (case format 
          ((d-m-y)
           (let ((d (car numeric-date-parts))
                 (m (cadr numeric-date-parts))
                 (y (qif-io:parse-year (caddr date-parts) 50)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (throw 'qif-io:parse-failed 
                        'qif-io:parse-date/format date-string))))
          ((m-d-y)
           (let ((m (car numeric-date-parts))
                 (d (cadr numeric-date-parts))
                 (y (qif-io:parse-year (caddr date-parts) 50)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (throw 'qif-io:parse-failed 
                        'qif-io:parse-date/format date-string))))
          ((y-m-d)
           (let ((y (qif-io:parse-year (car date-parts) 50))
                 (m (cadr numeric-date-parts))
                 (d (caddr numeric-date-parts)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (throw 'qif-io:parse-failed 
                        'qif-io:parse-date/format date-string))))
          ((y-d-m)
           (let ((y (qif-io:parse-year (car date-parts) 50))
                 (d (cadr numeric-date-parts))
                 (m (caddr numeric-date-parts)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (throw 'qif-io:parse-failed 
                        'qif-io:parse-date/format date-string))))
          (else 
           (throw 'qif-io:parse-failed 'qif-io:parse-date/format 
                  format))))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:check-number-format 
;;  given a list of possible number formats, return a pruned list 
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:check-number-format value-string possible-formats)
  (if (not (string? value-string))
      (throw 'qif-io:arg-type 'qif-io:check-number-format 'string value-string))
  (if (not (list? possible-formats))
      (throw 'qif-io:arg-type 'qif-io:check-number-format 
             'list possible-formats))
  
  (let ((retval '()))
    (for-each
     (lambda (format)
       (case format
         ((decimal) 
          (if (regexp-exec decimal-radix-regexp value-string)
              (set! retval (cons 'decimal retval))))
         ((comma)
          (if (regexp-exec comma-radix-regexp value-string)
              (set! retval (cons 'comma retval))))
         ((integer)
          (if (regexp-exec integer-regexp value-string)
              (set! retval (cons 'integer retval))))
         (else
          (throw 'qif-io:arg-type 'qif-io:check-number-format 
                 'number-format format))))
     possible-formats)
    (reverse! retval)))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:check-multi-number-format 
;;  apply check-number-format to a list of numbers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:check-multi-number-format value-list possible-formats)
  (let ((retval possible-formats))
    (for-each 
     (lambda (val)
       (if (string? val)
           (set! retval (qif-io:check-number-format val retval))))
     value-list)
    retval))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:parse-number/format 
;;  assuming we know what the format is, parse the string. 
;;  returns a gnc-numeric; the denominator is set so as to exactly 
;;  represent the number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-number/format value-string format) 
  (if (not (string? value-string))
      (throw 'qif-io:arg-type 'qif-io:parse-number/format 
             'string value-string))  

  (case format 
    ((decimal)
     (let* ((filtered-string
             (string-remove-char 
              (string-remove-char value-string #\,)
              #\$))
            (read-val
             (with-input-from-string filtered-string
               (lambda () (read)))))
       (if (number? read-val)
           (double-to-gnc-numeric
            (+ 0.0 read-val) GNC-DENOM-AUTO
            (logior (GNC-DENOM-SIGFIGS 
                     (string-length (string-remove-char filtered-string #\.)))
                    GNC-RND-ROUND))
           (gnc-numeric-zero))))
    ((comma)
     (let* ((filtered-string 
             (string-remove-char 
              (string-replace-char! 
               (string-remove-char value-string #\.)
               #\, #\.)
              #\$))             
            (read-val
             (with-input-from-string filtered-string
               (lambda () (read)))))
       (if (number? read-val)
           (double-to-gnc-numeric
            (+ 0.0 read-val) GNC-DENOM-AUTO
            (logior (GNC-DENOM-SIGFIGS
                     (string-length (string-remove-char filtered-string #\.)))
                    GNC-RND-ROUND))
           (gnc-numeric-zero))))
    ((integer)
     (let ((read-val
            (with-input-from-string 
                (string-remove-char value-string #\$)
              (lambda () (read)))))
       (if (number? read-val)
           (double-to-gnc-numeric
            (+ 0.0 read-val) 1 GNC-RND-ROUND)
           (gnc-numeric-zero))))
    (else 
     (throw 'qif-io:arg-type 'qif-io:parse-number/format 
            'number-format format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:parse-acct-type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:parse-acct-type read-value)
  (if (not (string? read-value))
      #f
      (let ((mangled-string 
             (string-remove-trailing-space 
              (string-remove-leading-space read-value))))
        (cond
         ((string-ci=? mangled-string "bank")
          GNC-BANK-TYPE)
         ((string-ci=? mangled-string "port")
          GNC-BANK-TYPE)
         ((string-ci=? mangled-string "cash")
          GNC-CASH-TYPE)
         ((string-ci=? mangled-string "ccard")
          GNC-CCARD-TYPE)
         ((string-ci=? mangled-string "invst") ;; these are brokerage accounts.
          GNC-BANK-TYPE)
         ((string-ci=? mangled-string "oth a")
          GNC-ASSET-TYPE)
         ((string-ci=? mangled-string "oth l")
          GNC-LIABILITY-TYPE)
         ((string-ci=? mangled-string "mutual")
          GNC-BANK-TYPE)
         (else 
          #f)))))
