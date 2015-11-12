;; engine-utilities.scm
;;
;; Convenience routines, etc. related to the engine.
;;
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

;; Copyright 2000 Rob Browning <rlb@cs.utexas.edu>
(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/engine" 0))

(use-modules (srfi srfi-1)
             (srfi srfi-13))

(define (gnc:account-map-descendants thunk account)
  (let ((descendants (or (gnc-account-get-descendants-sorted account) '())))
    (map thunk descendants)))

(define (gnc:account-map-children thunk account)
  (let ((children (or (gnc-account-get-children-sorted account) '())))
    (map thunk children)))

;; account related functions
;; is account in list of accounts?
(define (account-same? a1 a2)
  (or (eq? a1 a2)
      (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2))))

(define account-in-list?
  (lambda (account accounts)
    (cond
     ((null? accounts) #f)
     ((account-same? (car accounts) account) #t)
     (else (account-in-list? account (cdr accounts))))))

;; Optimized version of accout-in-list if we know
;; the list in advance.
(define (account-in-list-pred accounts)
  (define (my-assoc str alist)
    (find (lambda (pair) (account-same? str (car pair))) alist))
  (define (my-hash acc size)
    (remainder (string-hash (gncAccountGetGUID acc)) size))
  (let ((hash-table (make-hash-table)))
    (for-each (lambda (acc) (hashx-set! my-hash my-assoc hash-table acc #t))
	      accounts)
    (lambda (account)
      (hashx-ref my-hash my-assoc hash-table account))))

(define account-in-alist
  (lambda (account alist)
    (cond
     ((null? alist) #f)
     ((account-same? (caar alist) account) (car alist))
     (else (account-in-alist account (cdr alist))))))

;; helper for sorting of account list
(define (account-full-name<? a b)
  (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

;; return maximum depth over accounts and their children, if any
(define (accounts-get-children-depth accounts)
  (apply max
	 (map (lambda (acct)
		(let ((acct-depth (gnc-account-get-current-depth acct)))
		  (+ acct-depth (- (gnc-account-get-tree-depth acct) 1))))
	      accounts)))

;; Splits
(export split-same?)
(export split-in-list?)

(define (split-same? s1 s2)
  (or (eq? s1 s2)
      (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2))))

(define split-in-list? 
  (lambda (split splits)
    (cond 
     ((null? splits) #f)
     ((split-same? (car splits) split) #t)
     (else (split-in-list? split (cdr splits))))))

