;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-objects.scm
;;;  record type definitions for QIF objects.  
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this should be a GOOPS class.  later, I guess. 
(define <qif-file>  
  (make-record-type 
   "qif-file"
   '(path
     bank-xtns
     bank-xtn-format
     invst-xtns
     invst-xtn-format
     xtns-need-acct?
     default-src-acct
     accounts 
     categories
     classes
     securities)))

(define qif-io:make-file
  (record-constructor <qif-file>))
(define (qif-io:make-empty-file)
  (qif-io:make-file #f #f #f #f #f #f #f #f #f #f #f))

(define qif-io:file?
  (record-predicate <qif-file>))
(define qif-io:file-path
  (record-accessor <qif-file> 'path))
(define qif-io:file-set-path!
  (record-modifier <qif-file> 'path))
(define qif-io:file-bank-xtns
  (record-accessor <qif-file> 'bank-xtns))
(define qif-io:file-set-bank-xtns!
  (record-modifier <qif-file> 'bank-xtns))
(define qif-io:file-bank-xtn-format
  (record-accessor <qif-file> 'bank-xtn-format))
(define qif-io:file-set-bank-xtn-format!
  (record-modifier <qif-file> 'bank-xtn-format))
(define qif-io:file-invst-xtns
  (record-accessor <qif-file> 'invst-xtns))
(define qif-io:file-set-invst-xtns!
  (record-modifier <qif-file> 'invst-xtns))
(define qif-io:file-invst-xtn-format
  (record-accessor <qif-file> 'invst-xtn-format))
(define qif-io:file-set-invst-xtn-format!
  (record-modifier <qif-file> 'invst-xtn-format))
(define qif-io:file-xtns-need-acct?
  (record-accessor <qif-file> 'xtns-need-acct?))
(define qif-io:file-set-xtns-need-acct?!
  (record-modifier <qif-file> 'xtns-need-acct?))
(define qif-io:file-default-src-acct
  (record-accessor <qif-file> 'default-src-acct))
(define qif-io:file-set-default-src-acct!
  (record-modifier <qif-file> 'default-src-acct))
(define qif-io:file-accounts
  (record-accessor <qif-file> 'accounts))
(define qif-io:file-set-accounts!
  (record-modifier <qif-file> 'accounts))
(define qif-io:file-categories
  (record-accessor <qif-file> 'categories))
(define qif-io:file-set-categories!
  (record-modifier <qif-file> 'categories))
(define qif-io:file-classes
  (record-accessor <qif-file> 'classes))
(define qif-io:file-set-classes!
  (record-modifier <qif-file> 'classes))
(define qif-io:file-securities
  (record-accessor <qif-file> 'securities))
(define qif-io:file-set-securities!
  (record-modifier <qif-file> 'securities))


(define <qif-split>
  (make-record-type 
   "qif-split" '(category amount memo)))

(define qif-io:make-split 
  (record-constructor <qif-split>))
(define (qif-io:make-empty-split)
  (qif-io:make-split #f #f #f))
(define qif-io:split-category
  (record-accessor <qif-split> 'category))
(define qif-io:split-set-category!
  (record-modifier <qif-split> 'category))
(define qif-io:split-amount
  (record-accessor <qif-split> 'amount))
(define qif-io:split-set-amount!
  (record-modifier <qif-split> 'amount))
(define qif-io:split-memo
  (record-accessor <qif-split> 'memo))
(define qif-io:split-set-memo!
  (record-modifier <qif-split> 'memo))


(define <qif-bank-xtn>
  (make-record-type 
   "qif-bank-xtn"
   '(source-acct date number payee memo t-amount u-amount
                 cleared category address splits)))

(define qif-io:make-bank-xtn
  (record-constructor <qif-bank-xtn>))
(define (qif-io:make-empty-bank-xtn)
  (qif-io:make-bank-xtn #f #f #f #f #f #f #f #f #f #f #f))
(define qif-io:bank-xtn-source-acct 
  (record-accessor <qif-bank-xtn> 'source-acct))
(define qif-io:bank-xtn-set-source-acct!
  (record-modifier <qif-bank-xtn> 'source-acct))
(define qif-io:bank-xtn-date 
  (record-accessor <qif-bank-xtn> 'date))
(define qif-io:bank-xtn-set-date!
  (record-modifier <qif-bank-xtn> 'date))
(define qif-io:bank-xtn-number 
  (record-accessor <qif-bank-xtn> 'number))
(define qif-io:bank-xtn-set-number!
  (record-modifier <qif-bank-xtn> 'number))
(define qif-io:bank-xtn-payee 
  (record-accessor <qif-bank-xtn> 'payee))
(define qif-io:bank-xtn-set-payee!
  (record-modifier <qif-bank-xtn> 'payee))
(define qif-io:bank-xtn-memo 
  (record-accessor <qif-bank-xtn> 'memo))
(define qif-io:bank-xtn-set-memo!
  (record-modifier <qif-bank-xtn> 'memo))
(define qif-io:bank-xtn-t-amount 
  (record-accessor <qif-bank-xtn> 't-amount))
(define qif-io:bank-xtn-set-t-amount!
  (record-modifier <qif-bank-xtn> 't-amount))
(define qif-io:bank-xtn-u-amount 
  (record-accessor <qif-bank-xtn> 'u-amount))
(define qif-io:bank-xtn-set-u-amount!
  (record-modifier <qif-bank-xtn> 'u-amount))
(define qif-io:bank-xtn-cleared 
  (record-accessor <qif-bank-xtn> 'cleared))
(define qif-io:bank-xtn-set-cleared!
  (record-modifier <qif-bank-xtn> 'cleared))
(define qif-io:bank-xtn-category 
  (record-accessor <qif-bank-xtn> 'category))
(define qif-io:bank-xtn-set-category!
  (record-modifier <qif-bank-xtn> 'category))
(define qif-io:bank-xtn-address 
  (record-accessor <qif-bank-xtn> 'address))
(define qif-io:bank-xtn-set-address!
  (record-modifier <qif-bank-xtn> 'address))
(define qif-io:bank-xtn-splits 
  (record-accessor <qif-bank-xtn> 'splits))
(define qif-io:bank-xtn-set-splits!
  (record-modifier <qif-bank-xtn> 'splits))

(define <qif-invst-xtn>
  (make-record-type 
   "qif-invst-xtn"
   '(source-acct date action security payee memo t-amount 
                 u-amount $-amount share-price share-amount commission 
                 cleared category address)))

(define qif-io:make-invst-xtn
  (record-constructor <qif-invst-xtn>))
(define (qif-io:make-empty-invst-xtn)
  (qif-io:make-invst-xtn #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
(define qif-io:invst-xtn-source-acct 
  (record-accessor <qif-invst-xtn> 'source-acct))
(define qif-io:invst-xtn-set-source-acct!
  (record-modifier <qif-invst-xtn> 'source-acct))
(define qif-io:invst-xtn-date 
  (record-accessor <qif-invst-xtn> 'date))
(define qif-io:invst-xtn-set-date!
  (record-modifier <qif-invst-xtn> 'date))
(define qif-io:invst-xtn-action
  (record-accessor <qif-invst-xtn> 'action))
(define qif-io:invst-xtn-set-action!
  (record-modifier <qif-invst-xtn> 'action))
(define qif-io:invst-xtn-security 
  (record-accessor <qif-invst-xtn> 'security))
(define qif-io:invst-xtn-set-security!
  (record-modifier <qif-invst-xtn> 'security))
(define qif-io:invst-xtn-payee 
  (record-accessor <qif-invst-xtn> 'payee))
(define qif-io:invst-xtn-set-payee!
  (record-modifier <qif-invst-xtn> 'payee))
(define qif-io:invst-xtn-memo 
  (record-accessor <qif-invst-xtn> 'memo))
(define qif-io:invst-xtn-set-memo!
  (record-modifier <qif-invst-xtn> 'memo))
(define qif-io:invst-xtn-t-amount 
  (record-accessor <qif-invst-xtn> 't-amount))
(define qif-io:invst-xtn-set-t-amount!
  (record-modifier <qif-invst-xtn> 't-amount))
(define qif-io:invst-xtn-u-amount 
  (record-accessor <qif-invst-xtn> 'u-amount))
(define qif-io:invst-xtn-set-u-amount!
  (record-modifier <qif-invst-xtn> 'u-amount))
(define qif-io:invst-xtn-$-amount 
  (record-accessor <qif-invst-xtn> '$-amount))
(define qif-io:invst-xtn-set-$-amount!
  (record-modifier <qif-invst-xtn> '$-amount))
(define qif-io:invst-xtn-share-price 
  (record-accessor <qif-invst-xtn> 'share-price))
(define qif-io:invst-xtn-set-share-price!
  (record-modifier <qif-invst-xtn> 'share-price))
(define qif-io:invst-xtn-share-amount 
  (record-accessor <qif-invst-xtn> 'share-amount))
(define qif-io:invst-xtn-set-share-amount!
  (record-modifier <qif-invst-xtn> 'share-amount))
(define qif-io:invst-xtn-commission 
  (record-accessor <qif-invst-xtn> 'commission))
(define qif-io:invst-xtn-set-commission!
  (record-modifier <qif-invst-xtn> 'commission))
(define qif-io:invst-xtn-cleared 
  (record-accessor <qif-invst-xtn> 'cleared))
(define qif-io:invst-xtn-set-cleared!
  (record-modifier <qif-invst-xtn> 'cleared))
(define qif-io:invst-xtn-category 
  (record-accessor <qif-invst-xtn> 'category))
(define qif-io:invst-xtn-set-category!
  (record-modifier <qif-invst-xtn> 'category))
(define qif-io:invst-xtn-address 
  (record-accessor <qif-invst-xtn> 'address))
(define qif-io:invst-xtn-set-address!
  (record-modifier <qif-invst-xtn> 'address))

(define <qif-account> 
  (make-record-type 
   "qif-account"
   '(name type description limit budget)))

(define qif-io:make-account 
  (record-constructor <qif-account>))
(define qif-io:account-name 
  (record-accessor <qif-account> 'name))
(define qif-io:account-set-name!
  (record-modifier <qif-account> 'name))
(define qif-io:account-type 
  (record-accessor <qif-account> 'type))
(define qif-io:account-set-type!
  (record-modifier <qif-account> 'type))
(define qif-io:account-description 
  (record-accessor <qif-account> 'description))
(define qif-io:account-set-description!
  (record-modifier <qif-account> 'description))
(define qif-io:account-limit 
  (record-accessor <qif-account> 'limit))
(define qif-io:account-set-limit!
  (record-modifier <qif-account> 'limit))
(define qif-io:account-budget 
  (record-accessor <qif-account> 'budget))
(define qif-io:account-set-budget!
  (record-modifier <qif-account> 'budget))

(define <qif-category>
  (make-record-type
   "qif-category"
   '(name description taxable expense-cat income-cat tax-class budget-amt)))

(define qif-io:make-category 
  (record-constructor <qif-category>))
(define qif-io:category-name
  (record-accessor <qif-category> 'name))
(define qif-io:category-set-name! 
  (record-modifier <qif-category> 'name))
(define qif-io:category-description
  (record-accessor <qif-category> 'description))
(define qif-io:category-set-description! 
  (record-modifier <qif-category> 'description))
(define qif-io:category-taxable
  (record-accessor <qif-category> 'taxable))
(define qif-io:category-set-taxable! 
  (record-modifier <qif-category> 'taxable))
(define qif-io:category-expense-cat
  (record-accessor <qif-category> 'expense-cat))
(define qif-io:category-set-expense-cat! 
  (record-modifier <qif-category> 'expense-cat))
(define qif-io:category-income-cat
  (record-accessor <qif-category> 'income-cat))
(define qif-io:category-set-income-cat! 
  (record-modifier <qif-category> 'income-cat))
(define qif-io:category-tax-class
  (record-accessor <qif-category> 'tax-class))
(define qif-io:category-set-tax-class! 
  (record-modifier <qif-category> 'tax-class))
(define qif-io:category-budget-amt
  (record-accessor <qif-category> 'budget-amt))
(define qif-io:category-set-budget-amt! 
  (record-modifier <qif-category> 'budget-amt))

(define <qif-class>
  (make-record-type
   "qif-class"
   '(name description)))

(define qif-io:make-class 
  (record-constructor <qif-class>))
(define qif-io:class-name
  (record-accessor <qif-class> 'name))
(define qif-io:class-set-name! 
  (record-modifier <qif-class> 'name))
(define qif-io:class-description
  (record-accessor <qif-class> 'description))
(define qif-io:class-set-description! 
  (record-modifier <qif-class> 'description))

(define <qif-security>
  (make-record-type
   "qif-security"
   '(name symbol type)))

(define qif-io:make-security 
  (record-constructor <qif-security>))
(define qif-io:security-name
  (record-accessor <qif-security> 'name))
(define qif-io:security-set-name! 
  (record-modifier <qif-security> 'name))
(define qif-io:security-symbol
  (record-accessor <qif-security> 'symbol))
(define qif-io:security-set-symbol! 
  (record-modifier <qif-security> 'symbol))
(define qif-io:security-type
  (record-accessor <qif-security> 'type))
(define qif-io:security-set-type! 
  (record-modifier <qif-security> 'type))

(define <qif-acct-table>
  (make-record-type 
   "qif-acct-table"
   '(accounts categories securities brokerage-accts)))

(define qif-io:make-acct-table
  (record-constructor <qif-acct-table>))

(define (qif-io:make-empty-acct-table)
  (qif-io:make-acct-table 
   (make-hash-table 13)
   (make-hash-table 13)
   (make-hash-table 13)
   (make-hash-table 13)))

(define qif-io:acct-table-accounts
  (record-accessor <qif-acct-table> 'accounts))
(define qif-io:acct-table-categories
  (record-accessor <qif-acct-table> 'categories))
(define qif-io:acct-table-securities
  (record-accessor <qif-acct-table> 'securities))
(define qif-io:acct-table-brokerage-accts
  (record-accessor <qif-acct-table> 'brokerage-accts))


