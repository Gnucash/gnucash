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

(define (gnc:filename->account-group filename)
  "Returns an account group on success and #f on failure"
  (let* ((session (gnc:malloc-session)))
    (if (not session)
        #f
        (begin
          (gnc:init-session session)
          (let ((account-group (gnc:session-begin-file session filename)))
            (if (not account-group)
                (begin
                  (display (list 'serr (gnc:session-get-error session)))
                  (newline)
                  (gnc:session-destroy session)
                  #f)
                (begin
                  (gnc:session-end session)
                  (gnc:session-destroy session)
                  account-group)))))))

(define (gnc:call-with-account-data-from-file filename proc)
  ;; calls proc with one argument, the account group representing the
  ;; data in filename.  If there's an error, this procedure throws an
  ;; exception of type misc-error (for now).  Otherwise, the return
  ;; value is the result of the call to proc.
  ;;
  ;; This procedure does not currently handle any acceptions thrown by
  ;; the sub-procedures that it calls.

  (let ((account-group (gnc:filename->account-group filename)))
    (if (not account-group)
        (throw 'misc-error)
        (let ((result (proc account-group)))
          (gnc:free-account-group account-group)
          result))))

; (define (gnc:account-transactions-for-each thunk account)
;   ;; You must call gnc:group-reset-write-flags on the account group
;   ;; before using this...

;   (let loop ((num-splits (gnc:account-get-split-count account))
;              (i 0))
;     (if (< i num-splits)
;         (let* ((split (gnc:account-get-split account i))
;                (transaction (gnc:split-get-parent split)))
;           ;; We don't use the flags just like FileIO does (only 1 pass here)...
;           (if (= (gnc:transaction-get-write-flag transaction) 0)
;               (begin
;                 (thunk transaction)
;                 (gnc:transaction-set-write-flag transaction 2)))
;           (loop num-splits (+ i 1))))))


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
  (if (not grp)
      #f
      (let ((account-array (gnc:get-accounts grp)))
        ;; FIXME: Need to check for account-array being null, but we can't
        ;; right now, because there's no pointer-array-null?
        (let loop ((account (gnc:account-nth-account account-array 0))
                   (index 1))
          
          (if (not account)
              '()
              (cons account
                    (loop (gnc:account-nth-account account-array index)
                          (+ index 1))))))))

;; map over all accounts (including subaccounts) in a group
(define (gnc:group-map-all-accounts thunk group)
  (map thunk
       (or (gnc:group-get-account-list group)
           '())))
