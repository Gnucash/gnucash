;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; price-quotes.scm - manage sub-processes.
;;; Copyright 2001 Rob Browning <rlb@cs.utexas.edu>
;;; 
;;; This program is free software; you can redistribute it and/or    
;;; modify it under the terms of the GNU General Public License as   
;;; published by the Free Software Foundation; either version 2 of   
;;; the License, or (at your option) any later version.              
;;;                                                                  
;;; This program is distributed in the hope that it will be useful,  
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;;; GNU General Public License for more details.                     
;;;                                                                  
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "price-quotes.scm")
(gnc:depend "process.scm")

(define gnc:*price-quote-helper*
  "/home/rlb/opt/gnucash-working/share/gnucash/price-quote-helper")

(define (get-1-quote exchange . items)
  (let ((cmd (apply list 'fetch exchange items))
	(quoter (run-sub-process #f
				 gnc:*price-quote-helper*
				 gnc:*price-quote-helper*)))
    (and quoter
	 (write cmd (caddr quoter))
	 (newline (caddr quoter))
	 (force-output (caddr quoter))
	 (let ((result (read (cadr quoter))))
	   (close-input-port (cadr quoter))
	   (close-output-port (caddr quoter))
	   result))))


(define (gnc:book-add-quotes book)

  (define (find-quotables group)
    (define (quotable-account? a)
      (case (gnc:account-get-type a)
        ;; we no longer care what the price source was - Finance::Quote
        ;; doesn't let you specify a particular source.
        ((stock mutual-fund currency) (gnc:account-get-price-src a))
        (else #f)))
    (filter quotable-account? (gnc:group-get-subaccounts group)))

  (display (list book)) (newline)
  (display (list (gnc:book-get-group book))) (newline)
     
  (let* ((group (gnc:book-get-group book))
         (quotables (and group (find-quotables group)))
         (commodities (and quotables
			   (map gnc:account-get-commodity quotables))))
    (for-each (lambda (c)
                (display (list "Get quote for" (gnc:commodity-get-mnemonic c)))
		(newline))
	      commodities)))
