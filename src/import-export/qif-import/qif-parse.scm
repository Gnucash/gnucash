;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-parse.scm
;;;  routines to parse values and dates in QIF files.
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define qif-category-compiled-rexp
  (make-regexp "^ *(\\[)?([^]/\\|]*)(]?)(/?)([^\\|]*)(\\|(\\[)?([^]/]*)(]?)(/?)(.*))? *$"))

(define qif-date-compiled-rexp
  (make-regexp "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]).*$"))

(define qif-date-mdy-compiled-rexp
  (make-regexp "([0-9][0-9])([0-9][0-9])([0-9][0-9][0-9][0-9])"))

(define qif-date-ymd-compiled-rexp
  (make-regexp "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])"))

(define decimal-radix-regexp
  (make-regexp
   "^ *\\$?[+-]?\\$?[0-9]+$|^ *\\$?[+-]?\\$?[0-9]?[0-9]?[0-9]?(,[0-9][0-9][0-9])*(\\.[0-9]*)? *$|^ *\\$?[+-]?\\$?[0-9]+\\.[0-9]* *$"))

(define comma-radix-regexp
  (make-regexp
   "^ *\\$?[+-]?\\$?[0-9]+$|^ *\\$?[+-]?\\$?[0-9]?[0-9]?[0-9]?(\\.[0-9][0-9][0-9])*(,[0-9]*)? *$|^ *\\$?[+-]?\\$?[0-9]+,[0-9]* *$"))

(define integer-regexp (make-regexp "^\\$?[+-]?\\$?[0-9]+ *$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-split:parse-category
;;  this one just gets nastier and nastier.
;;  ATM we return a list of 6 elements:
;;    parsed category name (without [] if it was an account name)
;;    bool stating if it was an account name
;;    class of account or #f
;;    string representing the "miscx category" if any
;;    bool if miscx category is an account
;;    class of miscx cat or #f
;;  gosh, I love regular expressions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-split:parse-category self value)
  (let ((match (regexp-exec qif-category-compiled-rexp value)))
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
        (begin
          (gnc:warn "qif-split:parse-category: can't parse [" value "].")
          (list "" #f #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:fix-year
;;  this is where we handle y2k fixes etc.  input is a string
;;  containing the year ("00", "2000", and "19100" all mean the same
;;  thing). output is an integer representing the year in the C.E.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:fix-year year-string y2k-threshold)
  (let ((fixed-string #f)
        (post-read-value #f)
        (y2k-fixed-value #f))

    ;; quicken prints 2000 as "' 0" for at least some versions.
    ;; thanks dave p for reporting this.
    (if (eq? (string-ref year-string 0) #\')
        (begin
          (gnc:warn "qif-file:fix-year: found weird QIF Y2K year ["
                    year-string "].")
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
      (gnc:warn "qif-file:fix-year: ay caramba! What is this? ["
                year-string "].")))

    y2k-fixed-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-acct-type : set the type of the account, using gnucash
;;  conventions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-acct-type read-value)
  (let ((mangled-string
         (string-downcase! (string-remove-trailing-space
                            (string-remove-leading-space read-value)))))
    (cond
     ((string=? mangled-string "bank")
      (list GNC-BANK-TYPE))
     ((string=? mangled-string "port")
      (list GNC-BANK-TYPE))
     ((string=? mangled-string "cash")
      (list GNC-CASH-TYPE))
     ((string=? mangled-string "ccard")
      (list GNC-CCARD-TYPE))
     ((string=? mangled-string "invst") ;; these are brokerage accounts.
      (list GNC-BANK-TYPE))
     ((string=? mangled-string "401(k)/403(b)")
      (list GNC-BANK-TYPE))
     ((string=? mangled-string "oth a")
      (list GNC-ASSET-TYPE GNC-BANK-TYPE GNC-CASH-TYPE))
     ((string=? mangled-string "oth l")
      (list GNC-LIABILITY-TYPE GNC-CCARD-TYPE))
     ((string=? mangled-string "mutual")
      (list GNC-BANK-TYPE))
     (#t
      (gnc:warn "qif-parse:parse-acct-type: unrecognized account type ["
                read-value
                "]... substituting Bank.")
      (list GNC-BANK-TYPE)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-bang-field : the bang fields switch the parse context
;;  for the qif file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-bang-field read-value)
  (let ((bang-field (string-downcase!
                     (string-remove-trailing-space read-value))))
;; The QIF files output by the WWW site of Credit Lyonnais
;; begin by:   !type bank
;; instead of: !Type:bank
    (if (>= (string-length bang-field) 5)
        (if (string=? (substring bang-field 0 5) "type ")
            (string-set! bang-field 4 #\:)))

    (string->symbol bang-field)))


(define (qif-parse:parse-action-field read-value)
  (if read-value
      (let ((action-symbol (string-to-canonical-symbol read-value)))
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
;          ((exercise)
;           'exercise)
;          ((exercisx)
;           'exercisx)
;          ((expire)
;           'expire)
;          ((grant)
;           'grant)
          ((int intinc) ;; zinsen
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
          ((miscinc cash)
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
          ((rtrncap)
           'rtrncap)
          ((rtrncapx)
           'rtrncapx)
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
          ((xin contribx)
           'xin)
          ((xout withdrwx)
           'xout)
;          ((vest)
;           'vest)
          (else
           (gnc-warning-dialog '()
            (string-append
             (sprintf #f (_ "The file contains an unknown Action '%s'.")
                      read-value)
             "\n"
             (_ "Some transactions may be discarded.")))
           #f)))
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-cleared-field : in a C (cleared) field in a QIF transaction,
;;  * means cleared, x or X means reconciled, and ! or ? mean some
;;  budget related stuff I don't understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-cleared-field read-value)
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
;;  parse-check-date-format
;;  given a match-triple (matches in spaces 1, 2, 3) and a
;;  list of possible date formats, return the list of formats
;;  that this date string could actually be.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-check-date-format match possible-formats)
  (let ((date-parts (list (match:substring match 1)
                          (match:substring match 2)
                          (match:substring match 3)))
        (numeric-date-parts '())
        (retval #f))

    ;;(define (print-list l)
    ;;  (for-each (lambda (x) (display x) (display " ")) l))

    ;;(for-each (lambda (x) (if (list? x) (print-list x) (display x)))
    ;;      (list "parsing: " date-parts " in " possible-formats "\n"))

    ;; get the strings into numbers (but keep the strings around)
    (set! numeric-date-parts
          (map (lambda (elt)
                 (with-input-from-string elt
                   (lambda () (read))))
               date-parts))
    
    (let ((possibilities possible-formats)
          (n1 (car numeric-date-parts))
          (n2 (cadr numeric-date-parts))
          (n3 (caddr numeric-date-parts))
          (s1 (car date-parts))
          (s3 (caddr date-parts)))
      
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

      ;; If we've got a 4-character year, make sure the date
      ;; is after 1930.  Don't check the high value (perhaps
      ;; we should?).
      (if (= (string-length s1) 4)
          (if (or (not (number? n1)) (< n1 1930))
              (begin
                (set! possibilities (delq 'y-m-d possibilities))
                (set! possibilities (delq 'y-d-m possibilities)))))
      (if (= (string-length s3) 4)
          (if (or (not (number? n3)) (< n3 1930))
              (begin
                (set! possibilities (delq 'm-d-y possibilities))
                (set! possibilities (delq 'd-m-y possibilities)))))

      (set! retval possibilities))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:check-date-format
;;  given a list of possible date formats, return a pruned list
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (qif-parse:check-date-format date-string possible-formats)
  (let ((retval #f))
    (if (or (not (string? date-string))
            (not (> (string-length date-string) 0)))
        (set! retval possible-formats))
    (let ((match (regexp-exec qif-date-compiled-rexp date-string)))
      (if match
          (if (match:substring match 1)
              (set! retval (parse-check-date-format match possible-formats))

              ;; Uh oh -- this is a string XXXXXXXX; we don't know which
              ;; way to test..  So test both YYYYxxxx and xxxxYYYY,
              ;; and let the parser verify the year is valid.
              (let* ((new-date-string (match:substring match 4))
                     (date-ymd (regexp-exec qif-date-ymd-compiled-rexp
                                            new-date-string))
                     (date-mdy (regexp-exec qif-date-mdy-compiled-rexp
                                               new-date-string))
                     (res1 '())
                     (res2 '()))
                (if (or (memq 'y-d-m possible-formats)
                        (memq 'y-m-d possible-formats))
                    (set! res1 (parse-check-date-format date-ymd possible-formats)))
                (if (or (memq 'd-m-y possible-formats)
                        (memq 'm-d-y possible-formats))
                    (set! res2 (parse-check-date-format date-mdy possible-formats)))

                (set! retval (append res1 res2))))))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:parse-date/format
;;  given a date-string and a format, convert the string to a
;;  date and return a list of day, month, year
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-date/format date-string format)
  (let ((date-parts '())
        (numeric-date-parts '())
        (retval #f)

        (match (regexp-exec qif-date-compiled-rexp date-string)))
    (if match
        (if (match:substring match 1)
             (set! date-parts (list (match:substring match 1)
                                    (match:substring match 2)
                                    (match:substring match 3)))
             ;; This is of the form XXXXXXXX; split the string based on
             ;; whether the format is YYYYxxxx or xxxxYYYY
             (let ((date-str (match:substring match 4)))
               (case format
                 ((d-m-y m-d-y)
                  (let ((m (regexp-exec qif-date-mdy-compiled-rexp date-str)))
                    (set! date-parts (list (match:substring m 1)
                                           (match:substring m 2)
                                           (match:substring m 3)))))
                 ((y-m-d y-d-m)
                  (let ((m (regexp-exec qif-date-ymd-compiled-rexp date-str)))
                    (set! date-parts (list (match:substring m 1)
                                           (match:substring m 2)
                                           (match:substring m 3)))))
                 ))))
        
    ;; get the strings into numbers (but keep the strings around)
    (set! numeric-date-parts
          (map (lambda (elt)
                 (with-input-from-string elt
                   (lambda () (read))))
               date-parts))
    
    ;; if the date parts list doesn't have 3 parts, we're in
    ;; trouble
    (if (not (eq? 3 (length date-parts)))
        (gnc:warn "qif-parse:parse-date/format: can't interpret date ["
                  date-string "]\nDate parts: " date-parts)
        (case format
          ((d-m-y)
           (let ((d (car numeric-date-parts))
                 (m (cadr numeric-date-parts))
                 (y (qif-parse:fix-year (caddr date-parts) 50)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (gnc:warn "qif-parse:parse-date/format: "
                           "format is d/m/y, but date is ["
                           date-string "]."))))
          
          ((m-d-y)
           (let ((m (car numeric-date-parts))
                 (d (cadr numeric-date-parts))
                 (y (qif-parse:fix-year (caddr date-parts) 50)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (gnc:warn "qif-parse:parse-date/format: "
                           "format is m/d/y, but date is ["
                           date-string "]."))))
          
          ((y-m-d)
           (let ((y (qif-parse:fix-year (car date-parts) 50))
                 (m (cadr numeric-date-parts))
                 (d (caddr numeric-date-parts)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (gnc:warn "qif-parse:parse-date/format: "
                           "format is y/m/d, but date is ["
                           date-string "]."))))
          
          ((y-d-m)
           (let ((y (qif-parse:fix-year (car date-parts) 50))
                 (d (cadr numeric-date-parts))
                 (m (caddr numeric-date-parts)))
             (if (and (integer? d) (integer? m) (integer? y)
                      (<= m 12) (<= d 31))
                 (set! retval (list d m y))
                 (gnc:warn "qif-parse:parse-date/format: "
                           "format is y/d/m, but date is ["
                           date-string "]."))))))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  number format predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value-is-decimal-radix? value)
  (if (regexp-exec decimal-radix-regexp value)
      #t #f))

(define (value-is-comma-radix? value)
  (if (regexp-exec comma-radix-regexp value)
      #t #f))

(define (value-is-integer? value)
  (if (regexp-exec integer-regexp value)
      #t #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:check-number-format
;;  given a list of possible number formats, return a pruned list
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:check-number-format value-string possible-formats)
  (let ((retval possible-formats))
    (if (not (value-is-decimal-radix? value-string))
        (set! retval (delq 'decimal retval)))
    (if (not (value-is-comma-radix? value-string))
        (set! retval (delq 'comma retval)))
    (if (not (value-is-integer? value-string))
        (set! retval (delq 'integer retval)))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:parse-number/format
;;  assuming we know what the format is, parse the string.
;;  returns a gnc-numeric; the denominator is set so as to exactly
;;  represent the number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-number/format value-string format)
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
           (gnc-numeric-zero))))))

(define (qif-parse:check-number-formats amt-strings formats)
  (let ((retval formats))
    (for-each
     (lambda (amt)
       (if amt
           (set! retval (qif-parse:check-number-format amt retval))))
     amt-strings)
    retval))

(define (qif-parse:parse-numbers/format amt-strings format)
  (let* ((all-ok #t)
         (tmp #f)
         (parsed
          (map
           (lambda (amt)
             (if amt
                 (begin
                   (set! tmp (qif-parse:parse-number/format amt format))
                   (if (not tmp)
                       (set! all-ok #f))
                   tmp)
                 (gnc-numeric-zero)))
           amt-strings)))
    (if all-ok parsed #f)))

(define (qif-parse:print-date date-list)
  (let ((tm (localtime (current-time))))
    (set-tm:mday tm (car date-list))
    (set-tm:mon tm (- (cadr date-list) 1))
    (set-tm:year tm (- (caddr date-list) 1900))
    (strftime "%a %B %d %Y" tm)))

(define (qif-parse:print-number num)
  (with-output-to-string
    (lambda ()
      (write num))))

(define (qif-parse:print-numbers num)
  (with-output-to-string
    (lambda ()
      (write num))))
