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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;; Copyright 2000 Rob Browning <rlb@cs.utexas.edu>

(gnc:support "engine-utilities.scm")

(define (gnc:transaction-map-splits thunk transaction)
  (let loop ((num-splits (gnc:transaction-get-split-count transaction))
             (i 0))
    (if (< i num-splits)
        (cons
         (thunk (gnc:transaction-get-split transaction i))
         (loop num-splits (+ i 1)))
        '())))

(define (gnc:group-map-accounts thunk group)
  "Call thunk for each account in group, returning the results as a
list.  Return '() for a null group."
  (let loop ((i 0)
             (num-accounts (gnc:group-get-num-accounts group)))
    (if (< i num-accounts)
        (cons (thunk (gnc:group-get-account group i))
              (loop (+ i 1) num-accounts))
        '())))

;; Pull a scheme list of accounts (including subaccounts) from group grp
(define (gnc:group-get-account-list grp)
  "Return a flat list of all the accounts in grp, or #f if there's a problem."
  (if (pointer-token-null? grp)
      #f
      (let ((account-array (gnc:get-accounts grp)))
        ;; FIXME: Need to check for account-array being null, but we can't
        ;; right now, because there's no pointer-array-null?
        (let loop ((account (gnc:account-nth-account account-array 0))
                   (index 1))
          
          (if (pointer-token-null? account)
              '()
              (cons account
                    (loop (gnc:account-nth-account account-array index)
                          (+ index 1))))))))

;; map over all accounts (including subaccounts) in a group
(define (gnc:group-map-all-accounts thunk group)
  (map thunk
       (or (gnc:group-get-account-list group)
           '())))
