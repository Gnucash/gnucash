;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-parse.scm
;;;  routines to parse values and dates in QIF files.
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash import-export string))
(use-modules (srfi srfi-13))

(define regexp-enabled?
  (defined? 'make-regexp))
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

(define qif-category-compiled-rexp
  (and regexp-enabled?
       (make-regexp "^ *(\\[)?([^]/|]*)(]?)(/?)([^|]*)(\\|(\\[)?([^]/]*)(]?)(/?)(.*))? *$")))

(define (qif-split:parse-category self value)
  ;; example category regex matches (excluding initial 'L'):
  ;; field1
  ;; field1/field2
  ;; field1/|field3
  ;; field1/|field3/field4

  ;; where field1 is a category or [account]
  ;;   and field2 is a class
  ;;   and field3 is a miscx-category or [miscx-account]
  ;;   and field4 is a miscx-class
  (cond
   ((regexp-exec qif-category-compiled-rexp value) =>
    (lambda (rmatch)
      (list (match:substring rmatch 2)
            (and (match:substring rmatch 1)
                 (match:substring rmatch 3)
                 #t)
            (and (match:substring rmatch 4)
                 (match:substring rmatch 5))
            ;; miscx category name
            (and (match:substring rmatch 6)
                 (match:substring rmatch 8))
            ;; is it an account?
            (and (match:substring rmatch 7)
                 (match:substring rmatch 9)
                 #t)
            (and (match:substring rmatch 10)
                 (match:substring rmatch 11)))))
   (else
    ;; Parsing failed. Bug detected!
    (gnc:warn "qif-split:parse-category: can't parse [" value "].")
    (throw 'bug "qif-split:parse-category""Can't parse account or category ~A."
           (list value) #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:fix-year
;;  this is where we handle y2k fixes etc.  input is a string
;;  containing the year ("00", "2000", and "19100" all mean the same
;;  thing). output is an integer representing the year in the C.E.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:fix-year year-string y2k-threshold)
  (let* ((fixed-string
          (cond
           ((char=? (string-ref year-string 0) #\')
            (gnc:warn "qif-file:fix-year: weird QIF year [" year-string "].")
            (substring year-string 2 (string-length year-string)))
           (else year-string)))
         (post-read-value (with-input-from-string fixed-string read)))

    (cond
     ;; 2-digit numbers less than the window size are interpreted to
     ;; be post-2000.
     ((and (integer? post-read-value) (< post-read-value y2k-threshold))
      (+ 2000 post-read-value))

     ;; there's a common bug in printing post-2000 dates that prints
     ;; 2000 as 19100 etc.
     ((and (integer? post-read-value) (> post-read-value 19000))
      (+ 1900 (- post-read-value 19000)))

     ;; normal dates represented in unix years (i.e. year-1900, so
     ;; 2000 => 100.)  We also want to allow full year specifications,
     ;; (i.e. 1999, 2001, etc) and there's a point at which you can't
     ;; determine which is which.  this should eventually be another
     ;; field in the qif-file struct but not yet.
     ((and (integer? post-read-value) (< post-read-value 1902))
      (+ 1900 post-read-value))

     ;; this is a normal, 4-digit year spec (1999, 2000, etc).
     ((integer? post-read-value) post-read-value)

     ;; No idea what the string represents.  Maybe a new bug in Quicken!
     (else
      (gnc:warn "qif-file:fix-year: ay! What is this? [" year-string "].")
      #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-acct-type : set the type of the account, using gnucash
;;  conventions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-acct-type read-value errorproc errortype)
  (define string-map-alist
    (list (list "bank" GNC-BANK-TYPE)
          (list "port" GNC-BANK-TYPE)
          (list "cash" GNC-CASH-TYPE)
          (list "ccard" GNC-CCARD-TYPE)
          (list "invst" GNC-BANK-TYPE)
          (list "401(k)/403(b)" GNC-BANK-TYPE)
          (list "oth a" GNC-ASSET-TYPE GNC-BANK-TYPE GNC-CASH-TYPE)
          (list "oth l" GNC-LIABILITY-TYPE GNC-CCARD-TYPE)
          (list "oth s" GNC-ASSET-TYPE GNC-BANK-TYPE GNC-CASH-TYPE)
          (list "mutual" GNC-BANK-TYPE)))
  (or (assoc-ref string-map-alist (string-downcase! (string-trim-both read-value)))
      (let ((msg (format #f (_ "Unrecognized account type '~s'. Defaulting to Bank.")
                         read-value)))
        (errorproc errortype msg)
        (list GNC-BANK-TYPE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-bang-field : the bang fields switch the parse context
;;  for the qif file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-bang-field read-value)
  (let ((bang-field (string-downcase! (string-trim-right read-value))))
    ;; The QIF files output by the WWW site of Credit Lyonnais
    ;; begin by:   !type bank
    ;; instead of: !Type:bank
    (if (>= (string-length bang-field) 5)
        (if (string=? (substring bang-field 0 5) "type ")
            (string-set! bang-field 4 #\:)))
    (string->symbol bang-field)))

(define (qif-parse:parse-action-field read-value errorproc errortype)
  (define action-map
    '((buy cvrshrt kauf)
      (buyx cvrshrtx kaufx)
      (cglong cglong kapgew)
      (cglongx cglongx kapgewx)
      (cgmid cgmid)
      (cgmidx cgmidx)
      (cgshort cgshort k.gewsp)
      (cgshortx cgshortx k.gewspx)
      (div div)
      (divx divx)
      ;; (exercise exercise)
      ;; (exercisx exercisx)
      ;; (expire expire)
      ;; (grant grant)
      (intinc int intinc)
      (intincx intx intincx)
      (margint margint)
      (margintx margintx)
      (miscexp miscexp)
      (miscexpx miscexpx)
      (miscinc miscinc cash)
      (miscincx miscincx)
      (reinvdiv reinvdiv)
      (reinvint reinvint reinvzin)
      (reinvlg reinvlg reinvkur)
      (reinvmd reinvmd)
      (reinvsg reinvsg reinvksp)
      (reinvsh reinvsh)
      (reminder reminder erinnerg)
      (rtrncap rtrncap)
      (rtrncapx rtrncapx)
      (sell sell shtsell verkauf)
      (sellx sellx shtsellx verkaufx)
      (shrsin shrsin aktzu)
      (shrsout shrsout aktab)
      (stksplit stksplit aktsplit)
      (xin xin contribx)
      (xout xout withdrwx)))
  (and read-value
       (let ((sym (string->symbol (string-downcase (string-trim-both read-value)))))
         (or (any (lambda (lst) (and (memq sym lst) (car lst))) action-map)
             (let ((msg (format #f (_ "Unrecognized action '~a'.") read-value)))
               (errorproc errortype msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-cleared-field : In a "C" (cleared status) QIF line,
;;  * or C means cleared, X or R means reconciled, and ! or ?
;;  mean some budget related stuff I don't understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-cleared-field read-value errorproc errortype)
  (define maplist
    '((reconciled #\X #\x #\R #\r)
      (cleared #\* #\C #\c)
      (budgeted #\? #\!)))
  (and
   (string? read-value)
   (not (string-null? read-value))
   (let* ((secondchar (string-ref read-value 0)))
     (or (any (lambda (m) (and (memq secondchar (cdr m)) (car m))) maplist)
         (let ((msg (format #f (_ "Unrecognized status '~a'. Defaulting to uncleared.")
                            read-value)))
           (errorproc errortype msg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-check-date-format
;;  given a match-triple (matches in spaces 1, 2, 3) and a
;;  list of possible date formats, return the list of formats
;;  that this date string could actually be.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-check-date-format match possible-formats)
  (define (date? d m y ys)
    (and (number? d) (<= 1 d 31)
         (number? m) (<= 1 m 12)
         (number? y) (or (not (= 4 (string-length ys)))
                         (> y 1930))))
  (let* ((date-parts (list (match:substring match 1)
                           (match:substring match 2)
                           (match:substring match 3)))
         (numeric-date-parts (map (lambda (elt) (with-input-from-string elt read))
                                  date-parts))
         (n1 (car numeric-date-parts))
         (n2 (cadr numeric-date-parts))
         (n3 (caddr numeric-date-parts))
         (s1 (car date-parts))
         (s3 (caddr date-parts))
         (format-alist (list (list 'd-m-y n1 n2 n3 s3)
                             (list 'm-d-y n2 n1 n3 s3)
                             (list 'y-m-d n3 n2 n1 s1)
                             (list 'y-d-m n2 n3 n1 s1))))

    (let lp ((possible-formats possible-formats)
             (res '()))
      (cond
       ((null? possible-formats) (reverse res))
       (else
        (lp (cdr possible-formats)
            (let ((args (assq (car possible-formats) format-alist)))
              (if (apply date? (cdr args)) (cons (car args) res) res))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:check-date-format
;;  given a list of possible date formats, return a pruned list
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define qif-date-compiled-rexp
  (and regexp-enabled?
       (make-regexp "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]).*$")))

(define qif-date-mdy-compiled-rexp
  (and regexp-enabled?
       (make-regexp "([0-9][0-9])([0-9][0-9])([0-9][0-9][0-9][0-9])")))

(define qif-date-ymd-compiled-rexp
  (and regexp-enabled?
       (make-regexp "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])")))

(define (qif-parse:check-date-format date-string possible-formats)
  (and (string? date-string)
       (not (string-null? date-string))
       (let ((rmatch (regexp-exec qif-date-compiled-rexp date-string)))
         (if rmatch
             (if (match:substring rmatch 1)
                 (parse-check-date-format rmatch possible-formats)
                 ;; Uh oh -- this is a string XXXXXXXX; we don't know which
                 ;; way to test..  So test both YYYYxxxx and xxxxYYYY,
                 ;; and let the parser verify the year is valid.
                 (let* ((newstr (match:substring rmatch 4))
                        (date-ymd (regexp-exec qif-date-ymd-compiled-rexp newstr))
                        (date-mdy (regexp-exec qif-date-mdy-compiled-rexp newstr)))
                   (append
                    (if (or (memq 'y-d-m possible-formats)
                            (memq 'y-m-d possible-formats))
                        (parse-check-date-format date-ymd possible-formats))
                    (if (or (memq 'd-m-y possible-formats)
                            (memq 'm-d-y possible-formats))
                        (parse-check-date-format date-mdy possible-formats)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:parse-date/format
;;  given a date-string and a format, convert the string to a
;;  date and return a list of day, month, year
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:parse-date/format date-string dateformat)
  (define (date? d m y)
    (and (number? d) (<= 1 d 31)
         (number? m) (<= 1 m 12)))
  (let* ((rmatch (regexp-exec qif-date-compiled-rexp date-string))
         (date-parts
          (if rmatch
              (if (match:substring rmatch 1)
                  (list (match:substring rmatch 1)
                        (match:substring rmatch 2)
                        (match:substring rmatch 3))
                  ;; This is of the form XXXXXXXX; split the string based on
                  ;; whether the format is YYYYxxxx or xxxxYYYY
                  (let ((date-str (match:substring rmatch 4)))
                    (case dateformat
                      ((d-m-y m-d-y)
                       (let ((m (regexp-exec qif-date-mdy-compiled-rexp date-str)))
                         (list (match:substring m 1)
                               (match:substring m 2)
                               (match:substring m 3))))
                      ((y-m-d y-d-m)
                       (let ((m (regexp-exec qif-date-ymd-compiled-rexp date-str)))
                         (list (match:substring m 1)
                               (match:substring m 2)
                               (match:substring m 3)))))))
              '()))
         ;; get the strings into numbers (but keep the strings around)
         (numeric-date-parts (map (lambda (elt) (with-input-from-string elt read))
                                  date-parts)))

    (define (refs->list dd mm yy)
      (let ((d (list-ref numeric-date-parts dd))
            (m (list-ref numeric-date-parts mm))
            (y (qif-parse:fix-year (list-ref date-parts yy) 50)))
        (cond
         ((date? d m y) (list d m y))
         (else (gnc:warn "qif-parse:parse-date/format: format is " dateformat
                         " but date is [" date-string "].") #f))))

    ;; if the date parts list doesn't have 3 parts, we're in trouble
    (cond
     ((not (= 3 (length date-parts)))
      (gnc:warn "qif-parse:parse-date/format: can't interpret date ["
                date-string "]\nDate parts: " date-parts) #f)
     ((eq? dateformat 'd-m-y) (refs->list 0 1 2))
     ((eq? dateformat 'm-d-y) (refs->list 1 0 2))
     ((eq? dateformat 'y-m-d) (refs->list 2 1 0))
     ((eq? dateformat 'y-d-m) (refs->list 2 0 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  number format predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; eg 1000.00 or 1,500.00 or 2'000.00
(define decimal-radix-regexp
  (and regexp-enabled?
       (make-regexp "^ *[$]?[+-]?[$]?[0-9]+[+-]?$|^ *[$]?[+-]?[$]?[0-9]?[0-9]?[0-9]?([,'][0-9][0-9][0-9])*(\\.[0-9]*)?[+-]? *$|^ *[$]?[+-]?[$]?[0-9]+\\.[0-9]*[+-]? *$")))

;; eg 5.000,00 or 4'500,00
(define comma-radix-regexp
  (and regexp-enabled?
       (make-regexp "^ *[$]?[+-]?[$]?[0-9]+[+-]?$|^ *[$]?[+-]?[$]?[0-9]?[0-9]?[0-9]?([\\.'][0-9][0-9][0-9])*(,[0-9]*)?[+-]? *$|^ *[$]?[+-]?[$]?[0-9]+,[0-9]*[+-]? *$")))

;; eg 456 or 123
(define integer-regexp
  (and regexp-enabled?
       (make-regexp "^[$]?[+-]?[$]?[0-9]+[+-]? *$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:check-number-format
;;  given a list of possible number formats, return a pruned list
;;  of possibilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-parse:check-number-format value-string possible-formats)
  (define numtypes-alist
    (list (cons 'decimal decimal-radix-regexp)
          (cons 'comma comma-radix-regexp)
          (cons 'integer integer-regexp)))
  (filter (lambda (fmt) (regexp-exec (assq-ref numtypes-alist fmt) value-string))
          possible-formats))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-parse:parse-number/format
;;  assuming we know what the format is, parse the string.
;;  returns a gnc-numeric; the denominator is set so as to exactly
;;  represent the number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the following is a working refactored function
(define (qif-parse:parse-number/format value-string format)
  (let* ((filtered-string (gnc:string-delete-chars value-string "$'+"))
         (read-string (case format
                        ((decimal) (gnc:string-delete-chars filtered-string ","))
                        ((comma) (gnc:string-replace-char
                                  (gnc:string-delete-chars filtered-string ".")
                                  #\, #\.))
                        ((integer) filtered-string))))
    (or (string->number (string-append "#e" read-string)) 0)))

;; input: list of numstrings eg "10.50" "20.54"
;; input: formats to test '(decimal comma integer)
;; output: list of formats applicable eg '(decimal)
(define (qif-parse:check-number-formats amt-strings formats)
  (let lp ((amt-strings amt-strings)
           (formats formats))
    (if (null? amt-strings)
        formats
        (lp (cdr amt-strings)
            (if (car amt-strings)
                (qif-parse:check-number-format (car amt-strings) formats)
                formats)))))

;; list of number-strings and format -> list of numbers eg '("1,00"
;; "2,50" "3,99") 'comma --> '(1 5/2 399/100) this function would
;; formerly attempt to return #f if a list element couldn't be parsed;
;; but in practice always returns a list, with unparsed numbers as 0.
(define (qif-parse:parse-numbers/format amt-strings format)
  (map (lambda (amt) (if amt (qif-parse:parse-number/format amt format) 0))
       amt-strings))

(define (qif-parse:print-date date-list)
  (let ((tm (gnc-localtime (current-time))))
    (set-tm:mday tm (car date-list))
    (set-tm:mon tm (- (cadr date-list) 1))
    (set-tm:year tm (- (caddr date-list) 1900))
    (gnc-print-time64 (gnc-mktime tm) "%a %B %d %Y")))

(define (qif-parse:print-number num)
  (with-output-to-string
    (lambda ()
      (write num))))

(define (qif-parse:print-numbers num)
  (with-output-to-string
    (lambda ()
      (write num))))
