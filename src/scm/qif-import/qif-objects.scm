;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-objects.scm
;;;  representations for parts of an imported Quicken file.  
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-objects.scm")
(gnc:depend "qif-import/simple-obj.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file class 
;;  radix-format : one of 'decimal 'comma or 'unspecified 
;;  date-format  : one of 'd-m-y, 'm-d-y, 'y-m-d, 'y-d-m, 'unspecified
;;  currency     : a string representing the file's currency unit
;;  xtns         : list of <qif-xtn>  
;;  accounts     : list of <qif-acct>  
;;  cats         : list of <qif-cat>  
;;  classes      : list of <qif-class>  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-file>
  (make-simple-class 
   'qif-file 
   '(path                ;; where file was loaded 
     account             ;; guessed or specified 
     account-type        ;; either GNC-BANK-TYPE or GNC-STOCK-TYPE
     radix-format 
     guessed-radix-format 
     date-format 
     guessed-date-format 
     y2k-threshold
     currency            ;; this is a string.. no checking 
     xtns                ;; 
     accounts 
     cats
     classes)))

(define (qif-file? self)
  (eq? (simple-obj-type self) 'qif-file))

(define (qif-file:path self)
  (simple-obj-getter self <qif-file> 'path))

(define (qif-file:account self)
  (simple-obj-getter self <qif-file> 'account))

(define (qif-file:set-account! self value)
  (simple-obj-setter self <qif-file> 'account value))

(define (qif-file:account-type self)
  (simple-obj-getter self <qif-file> 'account-type))

(define (qif-file:set-account-type! self value)
  (simple-obj-setter self <qif-file> 'account-type value))

(define (qif-file:set-path! self value)
  (simple-obj-setter self <qif-file> 'path value))

(define (qif-file:radix-format self)
  (simple-obj-getter self <qif-file> 'radix-format))

(define (qif-file:set-radix-format! self value)
  (simple-obj-setter self <qif-file> 'radix-format value))

(define (qif-file:guessed-radix-format self)
  (simple-obj-getter self <qif-file> 'guessed-radix-format))

(define (qif-file:set-guessed-radix-format! self value)
  (simple-obj-setter self <qif-file> 'guessed-radix-format value))

(define (qif-file:date-format self)
  (simple-obj-getter self <qif-file> 'date-format))

(define (qif-file:set-date-format! self value)
  (simple-obj-setter self <qif-file> 'date-format value))

(define (qif-file:guessed-date-format self)
  (simple-obj-getter self <qif-file> 'guessed-date-format))

(define (qif-file:set-guessed-date-format! self value)
  (simple-obj-setter self <qif-file> 'guessed-date-format value))

(define (qif-file:y2k-threshold self)
  (simple-obj-getter self <qif-file> 'y2k-threshold))

(define (qif-file:set-y2k-threshold! self value)
  (simple-obj-setter self <qif-file> 'y2k-threshold value))

(define (qif-file:currency self)
  (simple-obj-getter self <qif-file> 'currency))

(define (qif-file:set-currency! self value)
  (simple-obj-setter self <qif-file> 'currency value))

(define (qif-file:cats self)
  (simple-obj-getter self <qif-file> 'cats))

(define (qif-file:set-cats! self value)
  (simple-obj-setter self <qif-file> 'cats value))

(define (qif-file:classes self)
  (simple-obj-getter self <qif-file> 'classes))

(define (qif-file:set-classes! self value)
  (simple-obj-setter self <qif-file> 'classes value))

(define (qif-file:xtns self)
  (simple-obj-getter self <qif-file> 'xtns))

(define (qif-file:set-xtns! self value)
  (simple-obj-setter self <qif-file> 'xtns value))

(define (qif-file:accounts self)
  (simple-obj-getter self <qif-file> 'accounts))

(define (qif-file:set-accounts! self value)
  (simple-obj-setter self <qif-file> 'accounts value))

(define (make-qif-file account radix-format date-format currency) 
  (let ((self (make-simple-obj <qif-file>)))
    (qif-file:set-account! self account)
    (qif-file:set-radix-format! self radix-format)
    (qif-file:set-guessed-radix-format! self radix-format)
    (qif-file:set-date-format! self date-format)
    (qif-file:set-guessed-date-format! self date-format)    
    (qif-file:set-currency! self currency)
    (qif-file:set-y2k-threshold! self 50)
    (qif-file:set-xtns! self '())
    (qif-file:set-accounts! self '())
    (qif-file:set-cats! self '())
    (qif-file:set-classes! self '())
    self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-split class 
;;  this is for bank/ccard accounts only. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-split>
  (make-simple-class 
   'qif-split
   '(category class memo amount category-is-account? matching-cleared mark)))

(define (qif-split:category self)
  (simple-obj-getter self <qif-split> 'category))

(define (qif-split:set-category! self value)
  (let* ((cat-info 
          (qif-split:parse-category self value))
         (cat-name (list-ref cat-info 0))
         (is-account? (list-ref cat-info 1))
         (class-name (list-ref cat-info 2)))
    (simple-obj-setter self <qif-split> 'category cat-name)
    (simple-obj-setter self <qif-split> 'class class-name)
    (simple-obj-setter self <qif-split> 'category-is-account? is-account?)))
;    (if (not is-account?)
;        (simple-obj-setter self <qif-split> 'mark #t))))

(define (qif-split:class self)
  (simple-obj-getter self <qif-split> 'class))

(define (qif-split:set-class! self value)
  (simple-obj-setter self <qif-split> 'class value))

(define (qif-split:memo self)
  (simple-obj-getter self <qif-split> 'memo))

(define (qif-split:set-memo! self value)
  (simple-obj-setter self <qif-split> 'memo value))

(define (qif-split:amount self)
  (simple-obj-getter self <qif-split> 'amount))

(define (qif-split:set-amount! self value)
  (simple-obj-setter self <qif-split> 'amount value))

(define (qif-split:mark self)
  (simple-obj-getter self <qif-split> 'mark))

(define (qif-split:set-mark! self value)
  (simple-obj-setter self <qif-split> 'mark value))

(define (qif-split:matching-cleared self)
  (simple-obj-getter self <qif-split> 'matching-cleared))

(define (qif-split:set-matching-cleared! self value)
  (simple-obj-setter self <qif-split> 'matching-cleared value))

(define (qif-split:category-is-account? self)
  (simple-obj-getter self <qif-split> 'category-is-account?))

(define (qif-split:set-category-is-account?! self value)
  (simple-obj-setter self <qif-split> 'category-is-account? value))

(define (make-qif-split)
  (let ((self (make-simple-obj <qif-split>)))
    (qif-split:set-category! self "")
    self))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-xtn class 
;;  [D] date       : parsed. 
;;  [P] payee      : string 
;;  [N] number (check number, sell, or buy)
;;  [C] cleared    : parsed (x/X/*) ;
;;  [T] amount     : parsed, units are currency from <qif-file>. 
;;  [M] memo       : string 
;;  [I] share price : parsed
;;  [Q] number of shares
;;  [Y] name of security 
;;  [O] adjustment (parsed)
;;  [L] category   : string 
;;  [S]/[E]/[$] splits : a list of <qif-split>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-xtn>
  (make-simple-class 
   'qif-xtn
   '(date payee address number cleared memo 
          share-price num-shares security-name adjustment 
          splits bank-xtn? mark)))

(define (qif-xtn? self)
  (eq? (simple-obj-type self) 'qif-xtn))

(define (qif-xtn:date self)
  (simple-obj-getter self <qif-xtn> 'date))

(define (qif-xtn:set-date! self value)
  (simple-obj-setter self <qif-xtn> 'date value))

(define (qif-xtn:payee self)
  (simple-obj-getter self <qif-xtn> 'payee))

(define (qif-xtn:set-payee! self value)
  (simple-obj-setter self <qif-xtn> 'payee value))

(define (qif-xtn:address self)
  (simple-obj-getter self <qif-xtn> 'address))

(define (qif-xtn:set-address! self value)
  (simple-obj-setter self <qif-xtn> 'address value))

(define (qif-xtn:number self)
  (simple-obj-getter self <qif-xtn> 'number))

(define (qif-xtn:set-number! self value)
  (simple-obj-setter self <qif-xtn> 'number value))

(define (qif-xtn:cleared self)
  (simple-obj-getter self <qif-xtn> 'cleared))

(define (qif-xtn:set-cleared! self value)
  (simple-obj-setter self <qif-xtn> 'cleared value))

(define (qif-xtn:share-price self)
  (simple-obj-getter self <qif-xtn> 'share-price))

(define (qif-xtn:set-share-price! self value)
  (simple-obj-setter self <qif-xtn> 'share-price value))

(define (qif-xtn:num-shares self)
  (simple-obj-getter self <qif-xtn> 'num-shares))

(define (qif-xtn:set-num-shares! self value)
  (simple-obj-setter self <qif-xtn> 'num-shares value))

(define (qif-xtn:security-name self)
  (simple-obj-getter self <qif-xtn> 'security-name))

(define (qif-xtn:set-security-name! self value)
  (simple-obj-setter self <qif-xtn> 'security-name value))

(define (qif-xtn:adjustment self)
  (simple-obj-getter self <qif-xtn> 'adjustment))

(define (qif-xtn:set-adjustment! self value)
  (simple-obj-setter self <qif-xtn> 'adjustment value))

(define (qif-xtn:splits self)
  (simple-obj-getter self <qif-xtn> 'splits))

(define (qif-xtn:set-splits! self value)
  (simple-obj-setter self <qif-xtn> 'splits value))

(define (qif-xtn:mark self)
  (simple-obj-getter self <qif-xtn> 'mark))

(define (qif-xtn:set-mark! self value)
  (simple-obj-setter self <qif-xtn> 'mark value))

(define (qif-xtn:bank-xtn? self)
  (simple-obj-getter self <qif-xtn> 'bank-xtn?))

(define (qif-xtn:set-bank-xtn?! self value)
  (simple-obj-setter self <qif-xtn> 'bank-xtn? value))

(define (make-qif-xtn)
  (let ((self (make-simple-obj <qif-xtn>)))
    (qif-xtn:set-bank-xtn?! self #t)
    (qif-xtn:set-mark! self #f)
    (qif-xtn:set-splits! self '())
    self))

(define (qif-xtn:reparse self qif-file)
  (let ((reparse-ok #t))
    ;; share price 
    (if (string? (qif-xtn:share-price self))
        (qif-xtn:set-share-price! 
         self 
         (qif-file:parse-value qif-file (qif-xtn:share-price self))))
    
    ;; number of shares 
    (if (string? (qif-xtn:num-shares self))
        (qif-xtn:set-num-shares! 
         self 
         (qif-file:parse-value qif-file (qif-xtn:num-shares self))))
    
    ;; adjustment 
    (if (string? (qif-xtn:adjustment self))
        (qif-xtn:set-adjustment! 
         self 
         (qif-file:parse-value qif-file (qif-xtn:adjustment self))))
    
    (if (or (string? (qif-xtn:share-price self))
            (string? (qif-xtn:num-shares self))
            (string? (qif-xtn:adjustment self)))
        (begin 
          (display "qif-import: failed to reparse stock info")
          (newline)
          (set! reparse-ok 
                (list #f "Could not autodetect radix format."))))
    
    ;; reparse the amount of each split 
    (for-each 
     (lambda (split)
       (if (string? (qif-split:amount split))
           (qif-split:set-amount! 
            split 
            (qif-file:parse-value qif-file (qif-split:amount split))))
       (if (string? (qif-split:amount split))
           (begin
             (display "qif-import: failed to reparse value")
             (write (qif-split:amount split)) (newline)
             (set! reparse-ok 
                   (list #f "Could not autodetect radix format.")))))
     (qif-xtn:splits self))
    
    ;; reparse the date 
    (if (string? (qif-xtn:date self))
        (qif-xtn:set-date! self 
                           (qif-file:parse-date qif-file 
                                                (qif-xtn:date self))))
    (if (string? (qif-xtn:date self))
        (begin 
          (display "qif-import: failed to reparse date")
          (write (qif-xtn:date self)) (newline)
          (set! reparse-ok 
                (list #f "Could not autodetect date format."))))
    reparse-ok))

(define (qif-xtn:print self)
  (simple-obj-print self <qif-xtn>))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-acct>
;;  [N] name         : string 
;;  [T] type         : string 
;;  [D] description  : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-acct>
  (make-simple-class 
   'qif-acct
   '(name type description limit)))

(define (qif-acct:name self)
  (simple-obj-getter self <qif-acct> 'name))

(define (qif-acct:set-name! self value)
  (simple-obj-setter self <qif-acct> 'name value))

(define (qif-acct:type self)
  (simple-obj-getter self <qif-acct> 'type))

(define (qif-acct:set-type! self value)
  (simple-obj-setter self <qif-acct> 'type value))

(define (qif-acct:description self)
  (simple-obj-getter self <qif-acct> 'description))

(define (qif-acct:set-description! self value)
  (simple-obj-setter self <qif-acct> 'description value))

(define (qif-acct:limit self)
  (simple-obj-getter self <qif-acct> 'limit))

(define (qif-acct:set-limit! self value)
  (simple-obj-setter self <qif-acct> 'limit value))

(define (make-qif-acct)
  (make-simple-obj <qif-acct>))

(define (qif-acct? self)
  (eq? (simple-obj-type self) 'qif-acct))

(define (qif-acct:print self)
  (simple-obj-print self <qif-acct>))

(define (qif-acct:reparse self file)
  (if (string? (qif-acct:limit self))
      (qif-acct:set-limit! 
       self (qif-file:parse-value file (qif-acct:limit self))))
  (if (or (string? (qif-acct:limit self))
          (string? (qif-acct:type self)))
      (list #f "Could not autodetect radix for fields in Account record")
      #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-class>
;;  [N] name         : string 
;;  [D] description  : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-class>
  (make-simple-class
   'qif-class
   '(name description)))

(define (qif-class:name self)
  (simple-obj-getter self <qif-class> 'name))

(define (qif-class:set-name! self value)
  (simple-obj-setter self <qif-class> 'name value))

(define (qif-class:description self)
  (simple-obj-getter self <qif-class> 'description))

(define (qif-class:set-description! self value)
  (simple-obj-setter self <qif-class> 'description value))

(define (qif-class:print self)
  (simple-obj-print self <qif-class>))

(define (make-qif-class)
  (make-simple-obj <qif-class>))

(define (qif-class? self)
  (eq? (simple-obj-type self) 'qif-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-cat> : a "Cat" or category transaction
;;  [N] name         : string 
;;  [D] description  : string 
;;  [T] taxable      : boolean 
;;  [E] expense?     : boolean
;;  [I] income?      : boolean 
;;  [R] tax rate     : number 
;;  [B] budget amt   : number 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <qif-cat>
  (make-simple-class 
   'qif-cat
   '(name description taxable expense-cat income-cat tax-rate budget-amt)))

(define (qif-cat:name self)
  (simple-obj-getter self <qif-cat> 'name))

(define (qif-cat:set-name! self value)
  (simple-obj-setter self <qif-cat> 'name value))

(define (qif-cat:description self)
  (simple-obj-getter self <qif-cat> 'description))

(define (qif-cat:set-description! self value)
  (simple-obj-setter self <qif-cat> 'description value))

(define (qif-cat:taxable self)
  (simple-obj-getter self <qif-cat> 'taxable))

(define (qif-cat:set-taxable! self value)
  (simple-obj-setter self <qif-cat> 'taxable value))

(define (qif-cat:expense-cat self)
  (simple-obj-getter self <qif-cat> 'expense-cat))

(define (qif-cat:set-expense-cat! self value)
  (simple-obj-setter self <qif-cat> 'expense-cat value))

(define (qif-cat:income-cat self)
  (simple-obj-getter self <qif-cat> 'income-cat))

(define (qif-cat:set-income-cat! self value)
  (simple-obj-setter self <qif-cat> 'income-cat value))

(define (qif-cat:tax-rate self)
  (simple-obj-getter self <qif-cat> 'tax-rate))

(define (qif-cat:set-tax-rate! self value)
  (simple-obj-setter self <qif-cat> 'tax-rate value))

(define (qif-cat:budget-amt self)
  (simple-obj-getter self <qif-cat> 'budget-amt))

(define (qif-cat:set-budget-amt! self value)
  (simple-obj-setter self <qif-cat> 'budget-amt value))

(define (make-qif-cat) 
  (make-simple-obj <qif-cat>))

(define (qif-cat? obj)
  (eq? (simple-obj-type obj) 'qif-cat))

(define (qif-cat:print self)
  (simple-obj-print self <qif-cat>))

(define (qif-cat:reparse self file)
  (if (string? (qif-cat:tax-rate self))
      (qif-cat:set-tax-rate! 
       self (qif-file:parse-value file (qif-cat:tax-rate self))))

  (if (string? (qif-cat:budget-amt self))
      (qif-cat:set-budget-amt! 
       self (qif-file:parse-value file (qif-cat:budget-amt self))))

  (if (or (string? (qif-cat:tax-rate self))
          (string? (qif-cat:budget-amt self)))
      (list #f "Could not autodetect radix for fields in Category record")
      #t))


(define (qif-file:add-xtn! self xtn)
  (qif-file:set-xtns! self 
                      (cons xtn (qif-file:xtns self))))

(define (qif-file:add-cat! self cat)
  (qif-file:set-cats! self 
                      (cons cat (qif-file:cats self))))

(define (qif-file:add-class! self class)
  (qif-file:set-classes! self 
                         (cons class (qif-file:classes self))))

(define (qif-file:add-account! self account)
  (qif-file:set-accounts! self 
                          (cons account (qif-file:accounts self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  munge the QIF filename to create a simple default account name 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:path-to-accountname self)
  (let ((namestring (qif-file:path self)))
    (if (and (string? namestring)
             (> (string-length namestring) 0))
        (begin 
          (set! namestring 
                (substring namestring 
                           (let ((last-slash (string-rindex namestring #\/)))
                             (if last-slash 
                                 (+ 1 last-slash) 
                                 0))
                           (let ((last-dot (string-rindex namestring #\.)))
                             (if last-dot 
                                 last-dot 
                                 (string-length namestring)))))
          (set! namestring (string-replace-char! namestring #\- #\space))
          (set! namestring (string-replace-char! namestring #\_ #\space))
          namestring)
        "QIF Import")))
