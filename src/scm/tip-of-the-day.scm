;; tip-of-the-day.scm -*-scheme-*-
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

;; Scheme functions for supporting tooltips
;; Written by Robert Merkel <rgmerk@mira.net>

;; Tips should be written as a list of lists of string.  Each list of strings 
;; represents one tip

(gnc:depend "config-var.scm")
(gnc:depend "prefs.scm")
(gnc:depend "hooks.scm")

(define (non-negative-integer? value)
  (and (integer? value) (>= value 0)))

(define gnc:*current-tip-number* 
  (gnc:make-config-var
   "Which tip we're up to"
;  (lambda (x) (if (and (integer? x) (>= x 0)) '(x) #f))
   (lambda (var value) (if (non-negative-integer? value) (list value) #f))
   =
   0))

(define gnc:*number-of-tips* 
  (gnc:make-config-var
   "Total number of tips"
   (lambda(var value) (if (non-negative-integer? value) (list value) #f))
   =
   0))

(define gnc:*tip-file*
  (gnc:make-config-var
   "Tip file"
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   "tip-list.scm"))

(define gnc:*tip-list* '())

(define (gnc:read-tips)
  (let ((in-port (open-input-file 
		  (gnc:find-in-directories 
		   (gnc:config-var-value-get gnc:*tip-file*)
		   (gnc:config-var-value-get gnc:*load-path*)))))
	(set! gnc:*tip-list* (read in-port))
        (set! gnc:*tip-list*
              (map (lambda (pair) (cadr pair)) gnc:*tip-list*))
	(if (not (= (length gnc:*tip-list*)
                    (gnc:config-var-value-get gnc:*current-tip-number*)))
	    (begin 
	      (gnc:config-var-value-set! gnc:*number-of-tips* #t
                                         (length gnc:*tip-list*))
	      (if (<= (gnc:config-var-value-get gnc:*number-of-tips*)
		      (gnc:config-var-value-get gnc:*current-tip-number*))
		  (gnc:config-var-value-set! #t gnc:*current-tip-number 0))))
	(close-port in-port)
	#f))

(define (gnc:get-current-tip)
  (_ (list-ref gnc:*tip-list*
               (gnc:config-var-value-get gnc:*current-tip-number*))))

(define (gnc:increment-tip-number)
  (let ((new-value (+ (gnc:config-var-value-get gnc:*current-tip-number*) 1)))
    (if (< new-value (gnc:config-var-value-get gnc:*number-of-tips*))
	(gnc:config-var-value-set! gnc:*current-tip-number* #t new-value)
	(gnc:config-var-value-set! gnc:*current-tip-number* #t 0))))

(define (gnc:decrement-tip-number)
  (let ((new-value (- (gnc:config-var-value-get gnc:*current-tip-number*) 1)))
    (if (< new-value 0)
	(gnc:config-var-value-set! gnc:*current-tip-number* #t 
				   (- (gnc:config-var-value-get
                                       gnc:*number-of-tips*) 1))
	(gnc:config-var-value-set! gnc:*current-tip-number* #t new-value))))
				  

(gnc:read-tips)

(let ((mainopen-hook (gnc:hook-lookup 'main-window-opened-hook)))
  (gnc:hook-add-dangler 
   mainopen-hook
   (lambda (window) 
     (let ((tip-opt (gnc:lookup-global-option "General"
                                              "Display \"Tip of the Day\"")))
       (if (gnc:option-value tip-opt)
           (gnc:ui-totd-dialog-create-and-run))))))
