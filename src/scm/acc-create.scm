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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Account creation utilities
(define gnc:account-types (initialize-hashtable 29))   ;; Need not be large...
(define (account-type->number symbol)
  (let
      ((s (hashv-ref gnc:account-types symbol)))
    (if s
	(cdr s)
	#f)))

(if (gnc:debugging?)
    (begin
      (display (account-type->number 'INCOME))
      (newline)))

(define (gnc:get-incomes-list account-group)
  (if (gnc:debugging?)
      gc-cats
      (filteroutnulls
       (flatten 
	(gnc:group-map-accounts 
	 get-names-of-incomes
	 account-group)))))

(define gnc-asset-account-types
  '(0 1 2 3 4 7))
;  (map account-type->number 
;       '(CASH CREDIT ASSET LIABILITY CURRENCY)))

(if (gnc:debugging?)
    (begin
      (display "gnc-asset-account-types:") 
      (display gnc-asset-account-types)
      (newline)))

;;; '(1 2 3 4 7))
;;;;;;;;;;;;;;;;;;;;;;; add, eventually, 11 12 13 14))
;;;                  aka CHECKING SAVINGS MONEYMRKT CREDITLINE))
;(define gnc-income-account-types '(8 9))
(define gnc-income-account-types 
  (map account-type->number '(INCOME EXPENSE)))

(if (gnc:debugging?)
    (begin
      (display "gnc-income-account-types:")
      (display gnc-income-account-types)
      (newline)))

(define gnc-invest-account-types '(5 6 10))

(define gnc-invest-account-types
  (map account-type->number '(EQUITY STOCK MUTUAL)))

(if (gnc:debugging?)
    (begin
      (display "gnc-invest-account-types:")
      (display gnc-invest-account-types)
      (newline)))

(define (get-names-of-accounts a)
  (list
   (if (member (gnc:account-get-type a) gnc-asset-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-accounts
			  (gnc:account-get-children a)))

(define (get-names-of-incomes a)
  (list
   (if (member (gnc:account-get-type a) gnc-income-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-incomes
			  (gnc:account-get-children a)))

(define (get-names-of-expenses a)
  (list
   (if (member (gnc:account-get-type a) gnc-expense-account-types)
       (gnc:account-get-name a)
       #f))
  (gnc:group-map-accounts get-names-of-expenses
			  (gnc:account-get-children a)))

(define (get-all-types)
  (set! gnc:account-types (initialize-hashtable 29))  ;; Need not be a big table
  (let loop 
      ((i 0))
    (let ((typesymbol (gnc:account-type->symbol i)))
      (hashv-set! gnc:account-types typesymbol i)
      (if (< i 14)
	  (loop (+ i 1))))))

(define (gnc:create-account AccPtr  name description notes type)
  (gnc:init-account AccPtr)
  (gnc:account-begin-edit AccPtr 0)
  (gnc:account-set-name AccPtr name)
  (gnc:account-set-description AccPtr description)
  (gnc:account-set-notes AccPtr notes)
  (gnc:account-set-type AccPtr type)
  (gnc:account-commit-edit AccPtr))

;;;;;;;;;;;  This one's REAL IMPORTANT!!! ;;;;;;;;;;;;
(if (gnc:debugging?)
    (begin
      (display (account-type->number 'CASH))
      (display (account-type->number 'INCOME))))

;;;;; And now, a "test bed"
(define (gnc:test-load-accs group)
  (let ((cash
	 (list (gnc:malloc-account)
	       "Sample Cash"
	       "Sample Cash Description"
	       "No notes - this is just a sample"
	       1))
	(inc1
	 (list (gnc:malloc-account)
	       "Misc Income"
	       "Miscellaneous Income"
	       "Just a dumb income account"
	       8))
	(exp1
	 (list (gnc:malloc-account)
	       "Misc Exp"
	       "Miscellaneous Expenses"
	       "Just a dumb expense account"
	       9)))
    (display "Samples: ") (newline)
    (display (list cash inc1 exp1)) (newline)
    (apply gnc:create-account cash)
    (apply gnc:create-account inc1)
    (apply gnc:create-account exp1)
    (display "group:") (display group) (newline)
    (gnc:group-insert-account group (car cash))
    (gnc:group-insert-account group (car inc1))
    (gnc:group-insert-account group (car exp1))
    (gnc:refresh-main-window))
  (display "Tried creation")(newline))
