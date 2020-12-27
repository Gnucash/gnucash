;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-objects.scm
;;;  representations for parts of an imported Quicken file.  
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

(define-module (gnucash qif-import qif-objects))

(eval-when (compile load eval expand)
  (load-extension "libgnc-gnome" "scm_init_sw_gnome_module"))

(use-modules (srfi srfi-9))
(use-modules (sw_gnome))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash string))

(export <qif-map-entry>)
(export make-qif-acct)
(export make-qif-cat)
(export make-qif-class)
(export make-qif-file)
(export make-qif-map-entry)
(export make-qif-split)
(export make-qif-stock-symbol)
(export make-qif-xtn)
(export make-ticker-map)
(export qif-acct:budget)
(export qif-acct:description)
(export qif-acct:limit)
(export qif-acct:name)
(export qif-acct:set-budget!)
(export qif-acct:set-description!)
(export qif-acct:set-limit!)
(export qif-acct:set-name!)
(export qif-acct:set-type!)
(export qif-acct:type)
(export qif-cat:budget-amt)
(export qif-cat:description)
(export qif-cat:expense-cat)
(export qif-cat:name)
(export qif-cat:set-budget-amt!)
(export qif-cat:set-description!)
(export qif-cat:set-expense-cat!)
(export qif-cat:set-income-cat!)
(export qif-cat:set-name!)
(export qif-cat:set-tax-class!)
(export qif-cat:set-taxable!)
(export qif-cat:tax-class)
(export qif-class:set-description!)
(export qif-class:set-name!)
(export qif-file:accounts)
(export qif-file:add-account!)
(export qif-file:add-cat!)
(export qif-file:add-class!)
(export qif-file:add-xtn!)
(export qif-file:cats)
(export qif-file:path)
(export qif-file:path-to-accountname)
(export qif-file:set-path!)
(export qif-file:set-xtns!)
(export qif-file:xtns)
(export qif-map-entry:allowed-parent-types)
(export qif-map-entry:allowed-types)
(export qif-map-entry:clone)
(export qif-map-entry:description)
(export qif-map-entry:display?)
(export qif-map-entry:gnc-name)
(export qif-map-entry:new-acct?)
(export qif-map-entry:qif-name)
(export qif-map-entry:set-allowed-types!)
(export qif-map-entry:set-description!)
(export qif-map-entry:set-display?!)
(export qif-map-entry:set-gnc-name!)
(export qif-map-entry:set-new-acct?!)
(export qif-map-entry:set-qif-name!)
(export qif-split:make)
(export qif-split:amount)
(export qif-split:category)
(export qif-split:category-is-account?)
(export qif-split:mark)
(export qif-split:matching-cleared)
(export qif-split:memo)
(export qif-split:miscx-category)
(export qif-split:set-amount!)
(export qif-split:set-category-is-account?!)
(export qif-split:set-category-private!)
(export qif-split:set-class!)
(export qif-split:set-mark!)
(export qif-split:set-matching-cleared!)
(export qif-split:set-memo!)
(export qif-split:set-miscx-category!)
(export qif-split:set-miscx-is-account?!)
(export qif-split:set-miscx-class!)
(export qif-stock-symbol:set-name!)
(export qif-stock-symbol:set-symbol!)
(export qif-stock-symbol:set-type!)
(export qif-ticker-map:add-ticker!)
(export qif-ticker-map:lookup-symbol)
(export qif-ticker-map:lookup-type)
(export qif-xtn:action)
(export qif-xtn:address)
(export qif-xtn:cleared)
(export qif-xtn:commission)
(export qif-xtn:date)
(export qif-xtn:default-split)
(export qif-xtn:from-acct)
(export qif-xtn:mark)
(export qif-xtn:num-shares)
(export qif-xtn:number)
(export qif-xtn:payee)
(export qif-xtn:security-name)
(export qif-xtn:set-action!)
(export qif-xtn:set-address!)
(export qif-xtn:set-cleared!)
(export qif-xtn:set-commission!)
(export qif-xtn:set-date!)
(export qif-xtn:set-default-split!)
(export qif-xtn:set-from-acct!)
(export qif-xtn:set-mark!)
(export qif-xtn:set-num-shares!)
(export qif-xtn:set-number!)
(export qif-xtn:set-payee!)
(export qif-xtn:set-security-name!)
(export qif-xtn:set-share-price!)
(export qif-xtn:set-split-amounts!)
(export qif-xtn:set-splits!)
(export qif-xtn:share-price)
(export qif-xtn:split-amounts)
(export qif-xtn:splits)
(export GNC-ASSET-TYPE)
(export GNC-BANK-TYPE)
(export GNC-CASH-TYPE)
(export GNC-CCARD-TYPE)
(export GNC-EQUITY-TYPE)
(export GNC-EXPENSE-TYPE)
(export GNC-INCOME-TYPE)
(export GNC-LIABILITY-TYPE)
(export GNC-MUTUAL-TYPE)
(export GNC-PAYABLE-TYPE)
(export GNC-RECEIVABLE-TYPE)
(export GNC-STOCK-TYPE)

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
(define GNC-RECEIVABLE-TYPE 11)
(define GNC-PAYABLE-TYPE 12)

(define (construct class)
  (apply (record-constructor class)
         (map (const #f) (record-type-fields class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file class 
;;  xtns         : list of <qif-xtn>  
;;  accounts     : list of <qif-acct>  
;;  cats         : list of <qif-cat>  
;;  classes      : list of <qif-class>  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-file>
  (make-record-type
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
  (record-accessor <qif-file> 'path))

(define qif-file:set-path! 
  (record-modifier <qif-file> 'path))

(define qif-file:y2k-threshold 
  (record-accessor <qif-file> 'y2k-threshold))

(define qif-file:set-y2k-threshold!
  (record-modifier <qif-file> 'y2k-threshold))

(define qif-file:cats 
  (record-accessor <qif-file> 'cats))

(define qif-file:set-cats!
  (record-modifier <qif-file> 'cats))

(define qif-file:classes 
  (record-accessor <qif-file> 'classes))

(define qif-file:set-classes!
  (record-modifier <qif-file> 'classes))

(define qif-file:xtns 
  (record-accessor <qif-file> 'xtns))

(define qif-file:set-xtns!
  (record-modifier <qif-file> 'xtns))

(define qif-file:accounts 
  (record-accessor <qif-file> 'accounts))

(define qif-file:set-accounts!
  (record-modifier <qif-file> 'accounts))

(define (make-qif-file) 
  (let ((self (construct <qif-file>)))
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
  (make-record-type
   'qif-split
   '(category class memo amount category-is-account? matching-cleared mark
              miscx-category miscx-is-account? miscx-class)))

(define qif-split:category 
  (record-accessor <qif-split> 'category))

(define qif-split:set-category-private!
  (record-modifier <qif-split> 'category))

(define qif-split:class 
  (record-accessor <qif-split> 'class))

(define qif-split:set-class!
  (record-modifier <qif-split> 'class))

(define qif-split:memo 
  (record-accessor <qif-split> 'memo))

(define qif-split:set-memo! 
  (record-modifier <qif-split> 'memo))

(define qif-split:amount 
  (record-accessor <qif-split> 'amount))

(define qif-split:set-amount! 
  (record-modifier <qif-split> 'amount))

(define qif-split:mark 
  (record-accessor <qif-split> 'mark))

(define qif-split:set-mark! 
  (record-modifier <qif-split> 'mark))

(define qif-split:matching-cleared 
  (record-accessor <qif-split> 'matching-cleared))

(define qif-split:set-matching-cleared! 
  (record-modifier <qif-split> 'matching-cleared))

(define qif-split:category-is-account? 
  (record-accessor <qif-split> 'category-is-account?))

(define qif-split:set-category-is-account?! 
  (record-modifier <qif-split> 'category-is-account?))

(define qif-split:miscx-is-account? 
  (record-accessor <qif-split> 'miscx-is-account?))

(define qif-split:set-miscx-is-account?!
  (record-modifier <qif-split> 'miscx-is-account?))

(define qif-split:miscx-category 
  (record-accessor <qif-split> 'miscx-category))

(define qif-split:set-miscx-category!
  (record-modifier <qif-split> 'miscx-category))

(define qif-split:miscx-class 
  (record-accessor <qif-split> 'miscx-class))

(define qif-split:set-miscx-class!
  (record-modifier <qif-split> 'miscx-class))

(define qif-split:make
  (record-constructor <qif-split>))

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
  (make-record-type
   'qif-xtn
   '(date payee address number action cleared  
          from-acct share-price num-shares security-name commission 
          default-split splits mark)))

(define qif-xtn?
  (record-predicate <qif-xtn>))

(define qif-xtn:date
  (record-accessor <qif-xtn> 'date))

(define qif-xtn:set-date! 
  (record-modifier <qif-xtn> 'date))

(define qif-xtn:payee
  (record-accessor <qif-xtn> 'payee))

(define qif-xtn:set-payee! 
  (record-modifier <qif-xtn> 'payee))

(define qif-xtn:address
  (record-accessor <qif-xtn> 'address))

(define qif-xtn:set-address! 
  (record-modifier <qif-xtn> 'address))

(define qif-xtn:number
  (record-accessor <qif-xtn> 'number))

(define qif-xtn:set-number! 
  (record-modifier <qif-xtn> 'number))

(define qif-xtn:action
  (record-accessor <qif-xtn> 'action))

(define qif-xtn:set-action! 
  (record-modifier <qif-xtn> 'action))

(define qif-xtn:cleared
  (record-accessor <qif-xtn> 'cleared))

(define qif-xtn:set-cleared! 
  (record-modifier <qif-xtn> 'cleared))

(define qif-xtn:from-acct
  (record-accessor <qif-xtn> 'from-acct))

(define qif-xtn:set-from-acct! 
  (record-modifier <qif-xtn> 'from-acct))

(define qif-xtn:share-price
  (record-accessor <qif-xtn> 'share-price))

(define qif-xtn:set-share-price! 
  (record-modifier <qif-xtn> 'share-price))

(define qif-xtn:num-shares
  (record-accessor <qif-xtn> 'num-shares))

(define qif-xtn:set-num-shares! 
  (record-modifier <qif-xtn> 'num-shares))

(define qif-xtn:security-name
  (record-accessor <qif-xtn> 'security-name))

(define qif-xtn:set-security-name! 
  (record-modifier <qif-xtn> 'security-name))

(define qif-xtn:commission
  (record-accessor <qif-xtn> 'commission))

(define qif-xtn:set-commission! 
  (record-modifier <qif-xtn> 'commission))

(define qif-xtn:default-split
  (record-accessor <qif-xtn> 'default-split))

(define qif-xtn:set-default-split! 
  (record-modifier <qif-xtn> 'default-split))

(define qif-xtn:splits
  (record-accessor <qif-xtn> 'splits))

(define qif-xtn:set-splits! 
  (record-modifier <qif-xtn> 'splits))

(define qif-xtn:mark
  (record-accessor <qif-xtn> 'mark))

(define qif-xtn:set-mark! 
  (record-modifier <qif-xtn> 'mark))

(define (make-qif-xtn)
  (let ((self (construct <qif-xtn>)))
    (qif-xtn:set-mark! self #f)
    (qif-xtn:set-splits! self '())
    self))

(define (qif-xtn:print self)
  (write self))


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
  (make-record-type
   'qif-acct
   '(name type description limit budget)))

(define qif-acct:name
  (record-accessor <qif-acct> 'name))

(define qif-acct:set-name! 
  (record-modifier <qif-acct> 'name))

(define qif-acct:type
  (record-accessor <qif-acct> 'type))

(define qif-acct:set-type! 
  (record-modifier <qif-acct> 'type))

(define qif-acct:description
  (record-accessor <qif-acct> 'description))

(define qif-acct:set-description! 
  (record-modifier <qif-acct> 'description))

(define qif-acct:limit
  (record-accessor <qif-acct> 'limit))

(define qif-acct:set-limit! 
  (record-modifier <qif-acct> 'limit))

(define qif-acct:budget
  (record-accessor <qif-acct> 'budget))

(define qif-acct:set-budget! 
  (record-modifier <qif-acct> 'budget))

(define (make-qif-acct)
  (let ((retval (construct <qif-acct>)))
    (qif-acct:set-type! retval "Bank")
    (qif-acct:set-name! retval "Default Account")
    retval))

(define qif-acct? 
  (record-predicate <qif-acct>))

(define (qif-acct:print self)
  (write self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-class>
;;  [N] name         : string 
;;  [D] description  : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-class>
  (make-record-type
   'qif-class
   '(name description)))

(define qif-class:name
  (record-accessor <qif-class> 'name))

(define qif-class:set-name! 
  (record-modifier <qif-class> 'name))

(define qif-class:description
  (record-accessor <qif-class> 'description))

(define qif-class:set-description! 
  (record-modifier <qif-class> 'description))

(define (qif-class:print self)
  (write self))

(define (make-qif-class)
  (construct <qif-class>))

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
  (make-record-type
   'qif-cat
   '(name description taxable expense-cat income-cat tax-class budget-amt)))

(define qif-cat:name
  (record-accessor <qif-cat> 'name))

(define qif-cat:set-name! 
  (record-modifier <qif-cat> 'name))

(define qif-cat:description
  (record-accessor <qif-cat> 'description))

(define qif-cat:set-description! 
  (record-modifier <qif-cat> 'description))

(define qif-cat:taxable
  (record-accessor <qif-cat> 'taxable))

(define qif-cat:set-taxable! 
  (record-modifier <qif-cat> 'taxable))

(define qif-cat:expense-cat
  (record-accessor <qif-cat> 'expense-cat))

(define qif-cat:set-expense-cat! 
  (record-modifier <qif-cat> 'expense-cat))

(define qif-cat:income-cat
  (record-accessor <qif-cat> 'income-cat))

(define qif-cat:set-income-cat! 
  (record-modifier <qif-cat> 'income-cat))

(define qif-cat:tax-class
  (record-accessor <qif-cat> 'tax-class))

(define qif-cat:set-tax-class! 
  (record-modifier <qif-cat> 'tax-class))

(define qif-cat:budget-amt
  (record-accessor <qif-cat> 'budget-amt))

(define qif-cat:set-budget-amt! 
  (record-modifier <qif-cat> 'budget-amt))

(define (make-qif-cat) 
  (construct <qif-cat>))

(define qif-cat? 
  (record-predicate <qif-cat>))

(define (qif-cat:print self)
  (write self))

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
          (set! namestring (gnc:string-replace-char namestring #\- #\space))
          (set! namestring (gnc:string-replace-char namestring #\_ #\space))
          namestring)
        "QIF Import")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-map-entry class
;; information for mapping a QIF account/category name to a
;; gnucash name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-map-entry>
  (make-record-type
   'qif-map-entry
   '(qif-name       ;; set while parsing file 
     allowed-types  ;; set while parsing file 
     description    ;; from QIF acct, if there is one 
     gnc-name       ;; set from guess-map 
     new-acct?      ;; set from guess-map
     display?)))    ;; set when non-zero transactions 

(define (make-qif-map-entry)
  (construct <qif-map-entry>))

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
  (record-accessor <qif-map-entry> 'qif-name))

(define qif-map-entry:set-qif-name!
  (record-modifier <qif-map-entry> 'qif-name))

(define qif-map-entry:allowed-types
  (record-accessor <qif-map-entry> 'allowed-types))

(define qif-map-entry:set-allowed-types!
  (record-modifier <qif-map-entry> 'allowed-types))

(define qif-map-entry:description
  (record-accessor <qif-map-entry> 'description))

(define qif-map-entry:set-description!
  (record-modifier <qif-map-entry> 'description))

(define qif-map-entry:gnc-name
  (record-accessor <qif-map-entry> 'gnc-name))

(define qif-map-entry:set-gnc-name!
  (record-modifier <qif-map-entry> 'gnc-name))

(define qif-map-entry:new-acct?
  (record-accessor <qif-map-entry> 'new-acct?))

(define qif-map-entry:set-new-acct?!
  (record-modifier <qif-map-entry> 'new-acct?))

(define qif-map-entry:display?
  (record-accessor <qif-map-entry> 'display?))

(define qif-map-entry:set-display?!
  (record-modifier <qif-map-entry> 'display?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <qif-stock-symbol>
;;  [N] stock name     : string 
;;  [S] ticker symbol  : string 
;;  [T] type           : string 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <qif-stock-symbol>
  (make-record-type
   'qif-stock-symbol
   '(name symbol type)))

(define qif-stock-symbol:name
  (record-accessor <qif-stock-symbol> 'name))

(define qif-stock-symbol:set-name! 
  (record-modifier <qif-stock-symbol> 'name))

(define qif-stock-symbol:symbol
  (record-accessor <qif-stock-symbol> 'symbol))

(define qif-stock-symbol:set-symbol! 
  (record-modifier <qif-stock-symbol> 'symbol))

(define qif-stock-symbol:type
  (record-accessor <qif-stock-symbol> 'type))

(define qif-stock-symbol:set-type! 
  (record-modifier <qif-stock-symbol> 'type))

(define (qif-stock-symbol:print self)
  (write self))

(define (make-qif-stock-symbol)
  (let ((retval (construct <qif-stock-symbol>)))
    (qif-stock-symbol:set-name! retval "")
    (qif-stock-symbol:set-symbol! retval "")
    (qif-stock-symbol:set-type! retval "")
    retval))

(define <qif-ticker-map>
  (make-record-type
   'qif-ticker-map
   '(stocks)))

(define qif-ticker-map:ticker-map
  (record-accessor <qif-ticker-map> 'stocks))

(define qif-ticker-map:set-ticker-map!
  (record-modifier <qif-ticker-map> 'stocks))

(define (make-ticker-map) 
  (let ((self (construct <qif-ticker-map>)))
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
	     (if (and (string? retval) (string=? retval ""))
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
	     (if (and (string? retval) (string=? retval ""))
		 (set! retval #f)))))
     (qif-ticker-map:ticker-map ticker-map))
    retval))

