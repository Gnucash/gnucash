;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-objects.scm
;;;  representations for parts of an imported Quicken file.  
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file class 
;;  xtns         : list of <qif-xtn>  
;;  accounts     : list of <qif-acct>  
;;  cats         : list of <qif-cat>  
;;  classes      : list of <qif-class>  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-file>
  (make-simple-class 
   'qif-file 
   '(path                 ;; where file was loaded 
     y2k-threshold
     xtns                
     accounts 
     cats
     classes)))

(define qif-file?
  (record-predicate <qif-file>))

(define qif-file:path 
  (simple-obj-getter <qif-file> 'path))

(define qif-file:set-path! 
  (simple-obj-setter <qif-file> 'path))

(define qif-file:y2k-threshold 
  (simple-obj-getter <qif-file> 'y2k-threshold))

(define qif-file:set-y2k-threshold!
  (simple-obj-setter <qif-file> 'y2k-threshold))

(define qif-file:cats 
  (simple-obj-getter <qif-file> 'cats))

(define qif-file:set-cats!
  (simple-obj-setter <qif-file> 'cats))

(define qif-file:classes 
  (simple-obj-getter <qif-file> 'classes))

(define qif-file:set-classes!
  (simple-obj-setter <qif-file> 'classes))

(define qif-file:xtns 
  (simple-obj-getter <qif-file> 'xtns))

(define qif-file:set-xtns!
  (simple-obj-setter <qif-file> 'xtns))

(define qif-file:accounts 
  (simple-obj-getter <qif-file> 'accounts))

(define qif-file:set-accounts!
  (simple-obj-setter <qif-file> 'accounts))

(define (make-qif-file) 
  (let ((self (make-simple-obj <qif-file>)))
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
   '(category class memo amount category-is-account? matching-cleared mark
              miscx-category miscx-is-account? miscx-class)))

(define qif-split:category 
  (simple-obj-getter <qif-split> 'category))

(define qif-split:set-category-private!
  (simple-obj-setter <qif-split> 'category))

(define (qif-split:set-category! self value)
  (let* ((cat-info 
          (qif-split:parse-category self value))
         (cat-name (list-ref cat-info 0))
         (is-account? (list-ref cat-info 1))
         (class-name (list-ref cat-info 2))
         (miscx-name (list-ref cat-info 3))
         (miscx-is-account? (list-ref cat-info 4))
         (miscx-class (list-ref cat-info 5)))
    (qif-split:set-category-private! self cat-name)
    (qif-split:set-class! self class-name)
    (qif-split:set-category-is-account?! self is-account?)
    (qif-split:set-miscx-category! self miscx-name)
    (qif-split:set-miscx-is-account?! self miscx-is-account?)
    (qif-split:set-miscx-class! self miscx-class)))
    
(define qif-split:class 
  (simple-obj-getter <qif-split> 'class))

(define qif-split:set-class!
  (simple-obj-setter <qif-split> 'class))

(define qif-split:memo 
  (simple-obj-getter <qif-split> 'memo))

(define qif-split:set-memo! 
  (simple-obj-setter <qif-split> 'memo))

(define qif-split:amount 
  (simple-obj-getter <qif-split> 'amount))

(define qif-split:set-amount! 
  (simple-obj-setter <qif-split> 'amount))

(define qif-split:mark 
  (simple-obj-getter <qif-split> 'mark))

(define qif-split:set-mark! 
  (simple-obj-setter <qif-split> 'mark))

(define qif-split:matching-cleared 
  (simple-obj-getter <qif-split> 'matching-cleared))

(define qif-split:set-matching-cleared! 
  (simple-obj-setter <qif-split> 'matching-cleared))

(define qif-split:category-is-account? 
  (simple-obj-getter <qif-split> 'category-is-account?))

(define qif-split:set-category-is-account?! 
  (simple-obj-setter <qif-split> 'category-is-account?))

(define qif-split:miscx-is-account? 
  (simple-obj-getter <qif-split> 'miscx-is-account?))

(define qif-split:set-miscx-is-account?!
  (simple-obj-setter <qif-split> 'miscx-is-account?))

(define qif-split:miscx-category 
  (simple-obj-getter <qif-split> 'miscx-category))

(define qif-split:set-miscx-category!
  (simple-obj-setter <qif-split> 'miscx-category))

(define qif-split:miscx-class 
  (simple-obj-getter <qif-split> 'miscx-class))

(define qif-split:set-miscx-class!
  (simple-obj-setter <qif-split> 'miscx-class))

(define (make-qif-split)
  (let ((self (make-simple-obj <qif-split>)))
    (qif-split:set-category! self "")
    self))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-xtn class 
;;  [D] date       : parsed. 
;;  [P] payee      : string 
;;  [N] number (check number, sell, or buy)
;;  [C] cleared    : parsed (x/X/*) ;
;;  [T] amount     : parsed, units are currency of dest account
;;  [I] share price : parsed
;;  [Q] number of shares
;;  [Y] name of security 
;;  [O] commission (parsed)
;;  [L] category   : string 
;;  [S]/[E]/[$] splits : a list of <qif-split>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-xtn>
  (make-simple-class 
   'qif-xtn
   '(date payee address number action cleared  
          from-acct share-price num-shares security-name commission 
          default-split splits mark)))

(define qif-xtn?
  (record-predicate <qif-xtn>))

(define qif-xtn:date
  (simple-obj-getter <qif-xtn> 'date))

(define qif-xtn:set-date! 
  (simple-obj-setter <qif-xtn> 'date))

(define qif-xtn:payee
  (simple-obj-getter <qif-xtn> 'payee))

(define qif-xtn:set-payee! 
  (simple-obj-setter <qif-xtn> 'payee))

(define qif-xtn:address
  (simple-obj-getter <qif-xtn> 'address))

(define qif-xtn:set-address! 
  (simple-obj-setter <qif-xtn> 'address))

(define qif-xtn:number
  (simple-obj-getter <qif-xtn> 'number))

(define qif-xtn:set-number! 
  (simple-obj-setter <qif-xtn> 'number))

(define qif-xtn:action
  (simple-obj-getter <qif-xtn> 'action))

(define qif-xtn:set-action! 
  (simple-obj-setter <qif-xtn> 'action))

(define qif-xtn:cleared
  (simple-obj-getter <qif-xtn> 'cleared))

(define qif-xtn:set-cleared! 
  (simple-obj-setter <qif-xtn> 'cleared))

(define qif-xtn:from-acct
  (simple-obj-getter <qif-xtn> 'from-acct))

(define qif-xtn:set-from-acct! 
  (simple-obj-setter <qif-xtn> 'from-acct))

(define qif-xtn:share-price
  (simple-obj-getter <qif-xtn> 'share-price))

(define qif-xtn:set-share-price! 
  (simple-obj-setter <qif-xtn> 'share-price))

(define qif-xtn:num-shares
  (simple-obj-getter <qif-xtn> 'num-shares))

(define qif-xtn:set-num-shares! 
  (simple-obj-setter <qif-xtn> 'num-shares))

(define qif-xtn:security-name
  (simple-obj-getter <qif-xtn> 'security-name))

(define qif-xtn:set-security-name! 
  (simple-obj-setter <qif-xtn> 'security-name))

(define qif-xtn:commission
  (simple-obj-getter <qif-xtn> 'commission))

(define qif-xtn:set-commission! 
  (simple-obj-setter <qif-xtn> 'commission))

(define qif-xtn:default-split
  (simple-obj-getter <qif-xtn> 'default-split))

(define qif-xtn:set-default-split! 
  (simple-obj-setter <qif-xtn> 'default-split))

(define qif-xtn:splits
  (simple-obj-getter <qif-xtn> 'splits))

(define qif-xtn:set-splits! 
  (simple-obj-setter <qif-xtn> 'splits))

(define qif-xtn:mark
  (simple-obj-getter <qif-xtn> 'mark))

(define qif-xtn:set-mark! 
  (simple-obj-setter <qif-xtn> 'mark))

(define (make-qif-xtn)
  (let ((self (make-simple-obj <qif-xtn>)))
    (qif-xtn:set-mark! self #f)
    (qif-xtn:set-splits! self '())
    self))

(define (qif-xtn:print self)
  (simple-obj-print self))


(define (qif-xtn:split-amounts self)
  (let ((def-spl (qif-xtn:default-split self))
	(spl-lst (qif-xtn:splits self)))
    (map
     (lambda (split)
       (qif-split:amount split))
     (if def-spl (cons def-spl spl-lst) spl-lst))))

(define (qif-xtn:set-split-amounts! self amounts)
  (define (set-amounts neg? amounts)
    (map 
     (lambda (split amount)
       (qif-split:set-amount! split (if neg? (gnc-numeric-neg amount) amount)))
     (qif-xtn:splits self) amounts))

  (define (need-neg amounts)
    (let ((sum (gnc-numeric-zero)))
      (for-each
       (lambda (amt)
	 (set! sum (gnc-numeric-add sum amt 0 GNC-DENOM-LCD)))
       amounts)
      (gnc-numeric-zero-p sum)))

  (let ((def-spl (qif-xtn:default-split self)))
    (if def-spl
	(set-amounts (need-neg amounts) (cdr amounts))
	(set-amounts #f amounts))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-acct>
;;  [N] name         : string 
;;  [T] type         : string 
;;  [D] description  : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-acct>
  (make-simple-class 
   'qif-acct
   '(name type description limit budget)))

(define qif-acct:name
  (simple-obj-getter <qif-acct> 'name))

(define qif-acct:set-name! 
  (simple-obj-setter <qif-acct> 'name))

(define qif-acct:type
  (simple-obj-getter <qif-acct> 'type))

(define qif-acct:set-type! 
  (simple-obj-setter <qif-acct> 'type))

(define qif-acct:description
  (simple-obj-getter <qif-acct> 'description))

(define qif-acct:set-description! 
  (simple-obj-setter <qif-acct> 'description))

(define qif-acct:limit
  (simple-obj-getter <qif-acct> 'limit))

(define qif-acct:set-limit! 
  (simple-obj-setter <qif-acct> 'limit))

(define qif-acct:budget
  (simple-obj-getter <qif-acct> 'budget))

(define qif-acct:set-budget! 
  (simple-obj-setter <qif-acct> 'budget))

(define (make-qif-acct)
  (let ((retval (make-simple-obj <qif-acct>)))
    (qif-acct:set-type! retval "Bank")
    (qif-acct:set-name! retval "Default Account")
    retval))

(define qif-acct? 
  (record-predicate <qif-acct>))

(define (qif-acct:print self)
  (simple-obj-print self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-class>
;;  [N] name         : string 
;;  [D] description  : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-class>
  (make-simple-class
   'qif-class
   '(name description)))

(define qif-class:name
  (simple-obj-getter <qif-class> 'name))

(define qif-class:set-name! 
  (simple-obj-setter <qif-class> 'name))

(define qif-class:description
  (simple-obj-getter <qif-class> 'description))

(define qif-class:set-description! 
  (simple-obj-setter <qif-class> 'description))

(define (qif-class:print self)
  (simple-obj-print self))

(define (make-qif-class)
  (make-simple-obj <qif-class>))

(define qif-class? 
  (record-predicate <qif-class>))

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
   '(name description taxable expense-cat income-cat tax-class budget-amt)))

(define qif-cat:name
  (simple-obj-getter <qif-cat> 'name))

(define qif-cat:set-name! 
  (simple-obj-setter <qif-cat> 'name))

(define qif-cat:description
  (simple-obj-getter <qif-cat> 'description))

(define qif-cat:set-description! 
  (simple-obj-setter <qif-cat> 'description))

(define qif-cat:taxable
  (simple-obj-getter <qif-cat> 'taxable))

(define qif-cat:set-taxable! 
  (simple-obj-setter <qif-cat> 'taxable))

(define qif-cat:expense-cat
  (simple-obj-getter <qif-cat> 'expense-cat))

(define qif-cat:set-expense-cat! 
  (simple-obj-setter <qif-cat> 'expense-cat))

(define qif-cat:income-cat
  (simple-obj-getter <qif-cat> 'income-cat))

(define qif-cat:set-income-cat! 
  (simple-obj-setter <qif-cat> 'income-cat))

(define qif-cat:tax-class
  (simple-obj-getter <qif-cat> 'tax-class))

(define qif-cat:set-tax-class! 
  (simple-obj-setter <qif-cat> 'tax-class))

(define qif-cat:budget-amt
  (simple-obj-getter <qif-cat> 'budget-amt))

(define qif-cat:set-budget-amt! 
  (simple-obj-setter <qif-cat> 'budget-amt))

(define (make-qif-cat) 
  (make-simple-obj <qif-cat>))

(define qif-cat? 
  (record-predicate <qif-cat>))

(define (qif-cat:print self)
  (simple-obj-print self))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-map-entry class
;; information for mapping a QIF account/category name to a
;; gnucash name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-map-entry>
  (make-simple-class
   'qif-map-entry
   '(qif-name       ;; set while parsing file 
     allowed-types  ;; set while parsing file 
     description    ;; from QIF acct, if there is one 
     gnc-name       ;; set from guess-map 
     new-acct?      ;; set from guess-map
     display?)))    ;; set when non-zero transactions 

(define (make-qif-map-entry)
  (make-simple-obj <qif-map-entry>))

(define (qif-map-entry:clone orig)
  (let ((me (make-qif-map-entry)))
    (qif-map-entry:set-qif-name! me (qif-map-entry:qif-name orig))
    (qif-map-entry:set-allowed-types! me (qif-map-entry:allowed-types orig))
    (qif-map-entry:set-description! me (qif-map-entry:description orig))
    (qif-map-entry:set-gnc-name! me (qif-map-entry:gnc-name orig))
    (qif-map-entry:set-new-acct?! me (qif-map-entry:new-acct? orig))
    (qif-map-entry:set-display?! me (qif-map-entry:display? orig))
    me))

(define (qif-map-entry:allowed-parent-types self) 
  (let ((types-list (reverse (qif-map-entry:allowed-types self))))
    (define (add-types . rest)
      (for-each 
       (lambda (t)
         (if (not (memv t types-list))
             (set! types-list (cons t types-list))))
       rest))
    
    (for-each 
     (lambda (t)
       (cond 
        ((memv t (list GNC-BANK-TYPE GNC-CASH-TYPE GNC-CCARD-TYPE 
                       GNC-STOCK-TYPE GNC-MUTUAL-TYPE
                       GNC-ASSET-TYPE GNC-LIABILITY-TYPE
                       GNC-RECEIVABLE-TYPE GNC-PAYABLE-TYPE))
         (add-types GNC-BANK-TYPE GNC-CASH-TYPE GNC-CCARD-TYPE 
                    GNC-STOCK-TYPE GNC-MUTUAL-TYPE
                    GNC-ASSET-TYPE GNC-LIABILITY-TYPE
                    GNC-RECEIVABLE-TYPE GNC-PAYABLE-TYPE))
        ((memv t (list GNC-INCOME-TYPE GNC-EXPENSE-TYPE))
         (add-types GNC-INCOME-TYPE GNC-EXPENSE-TYPE))
        (#t
         (add-types t))))
     (qif-map-entry:allowed-types self))
    (reverse types-list)))


(define qif-map-entry:qif-name
  (simple-obj-getter <qif-map-entry> 'qif-name))

(define qif-map-entry:set-qif-name!
  (simple-obj-setter <qif-map-entry> 'qif-name))

(define qif-map-entry:allowed-types
  (simple-obj-getter <qif-map-entry> 'allowed-types))

(define qif-map-entry:set-allowed-types!
  (simple-obj-setter <qif-map-entry> 'allowed-types))

(define qif-map-entry:description
  (simple-obj-getter <qif-map-entry> 'description))

(define qif-map-entry:set-description!
  (simple-obj-setter <qif-map-entry> 'description))

(define qif-map-entry:gnc-name
  (simple-obj-getter <qif-map-entry> 'gnc-name))

(define qif-map-entry:set-gnc-name!
  (simple-obj-setter <qif-map-entry> 'gnc-name))

(define qif-map-entry:new-acct?
  (simple-obj-getter <qif-map-entry> 'new-acct?))

(define qif-map-entry:set-new-acct?!
  (simple-obj-setter <qif-map-entry> 'new-acct?))

(define qif-map-entry:display?
  (simple-obj-getter <qif-map-entry> 'display?))

(define qif-map-entry:set-display?!
  (simple-obj-setter <qif-map-entry> 'display?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-stock-symbol>
;;  [N] stock name     : string 
;;  [S] ticker symbol  : string 
;;  [T] type           : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-stock-symbol>
  (make-simple-class
   'qif-stock-symbol
   '(name symbol type)))

(define qif-stock-symbol:name
  (simple-obj-getter <qif-stock-symbol> 'name))

(define qif-stock-symbol:set-name! 
  (simple-obj-setter <qif-stock-symbol> 'name))

(define qif-stock-symbol:symbol
  (simple-obj-getter <qif-stock-symbol> 'symbol))

(define qif-stock-symbol:set-symbol! 
  (simple-obj-setter <qif-stock-symbol> 'symbol))

(define qif-stock-symbol:type
  (simple-obj-getter <qif-stock-symbol> 'type))

(define qif-stock-symbol:set-type! 
  (simple-obj-setter <qif-stock-symbol> 'type))

(define (qif-stock-symbol:print self)
  (simple-obj-print self))

(define (make-qif-stock-symbol)
  (let ((retval (make-simple-obj <qif-stock-symbol>)))
    (qif-stock-symbol:set-name! retval "")
    (qif-stock-symbol:set-symbol! retval "")
    retval))

(define <qif-ticker-map>
  (make-simple-class
   'qif-ticker-map
   '(stocks)))

(define qif-ticker-map:ticker-map
  (simple-obj-getter <qif-ticker-map> 'stocks))

(define qif-ticker-map:set-ticker-map!
  (simple-obj-setter <qif-ticker-map> 'stocks))

(define (make-ticker-map) 
  (let ((self (make-simple-obj <qif-ticker-map>)))
    (qif-ticker-map:set-ticker-map! self '())
    self))

(define (qif-ticker-map:add-ticker! ticker-map stock-symbol)
  (qif-ticker-map:set-ticker-map!
   ticker-map
   (cons stock-symbol (qif-ticker-map:ticker-map ticker-map))))

(define (qif-ticker-map:lookup-symbol ticker-map name)
  (let ((retval #f))
    (for-each 
     (lambda (symbol)
       (if (string=? name (qif-stock-symbol:name symbol))
	   (begin
	     (set! retval (qif-stock-symbol:symbol symbol))
	     (if (string=? retval "")
		 (set! retval #f)))))
     (qif-ticker-map:ticker-map ticker-map))
    retval))

(define (qif-ticker-map:lookup-type ticker-map name)
  (let ((retval #f))
    (for-each 
     (lambda (symbol)
       (if (string=? name (qif-stock-symbol:name symbol))
	   (begin
	     (set! retval (qif-stock-symbol:type symbol))
	     (if (string=? retval "")
		 (set! retval #f)))))
     (qif-ticker-map:ticker-map ticker-map))
    retval))

