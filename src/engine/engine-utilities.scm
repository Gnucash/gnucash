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

(define (gnc:url->loaded-session url ignore-lock? create-if-needed?)
  ;; Return a <gnc:Book*> representing the data stored at the given
  ;; url or #f on failure -- this should later be changed to returning
  ;; the symbol representing the book error...  On success, the book
  ;; will already be loaded.

  (let* ((session (gnc:session-new))
         (result (and session
                      (gnc:session-begin session url
                                         ignore-lock?
                                         create-if-needed?)
                      (gnc:session-load session)
                      session)))
    (or result
        (begin (gnc:session-destroy session) #f))))

(define (gnc:transaction-map-splits thunk transaction)
  (let ((retval '()))
    (let loop ((splits (or (gnc:transaction-get-splits transaction) '())))
      (if (not (null? splits))
          (begin 
            (set! retval (cons (thunk (car splits)) retval))
            (loop (cdr splits)))))
    (reverse retval)))

(define (gnc:group-map-all-accounts thunk group)
  (let ((accounts (or (gnc:group-get-subaccounts group) '())))
    (map thunk accounts)))

(define (gnc:group-map-accounts thunk group)
  (let ((accounts (or (gnc:group-get-account-list group) '())))
    (map thunk accounts)))
