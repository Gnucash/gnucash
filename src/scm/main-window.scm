;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main-window.scm : utilities for dealing with main window
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; book open and close hooks for mdi 
;; 
;; we need to save all the active report and acct tree info during
;; book close.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (gnc:main-window-properties-cb)
  (let* ((book (gnc-get-current-book))
	 (slots (qof-book-get-slots book)))

    (define (changed_cb)
      (qof-book-kvp-changed book))
			    
    (gnc:kvp-option-dialog QOF-ID-BOOK-SCM
			   slots (_ "Book Options")
			   changed_cb)))

