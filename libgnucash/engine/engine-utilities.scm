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
