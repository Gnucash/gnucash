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

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__tips" "current_tip_number" 0))

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
	(if (not (= (length gnc:*tip-list*) (gnc:current-tip-number)))
	    (begin
	      (gnc:config-var-value-set! gnc:*number-of-tips* #t
                                         (length gnc:*tip-list*))
	      (if (<= (gnc:config-var-value-get gnc:*number-of-tips*)
		      (gnc:current-tip-number))
		  (gnc:reset-tip-number))))
	(close-port in-port)
	#f))

(define (gnc:current-tip-number)
  (gnc:option-value (gnc:lookup-global-option "__tips" "current_tip_number")))

(define (gnc:get-current-tip)
  (_ (list-ref gnc:*tip-list* (gnc:current-tip-number))))

(define (gnc:reset-tip-number)
  (let ((opt (gnc:lookup-global-option "__tips" "current_tip_number")))
    (gnc:option-set-value opt 0)))

(define (gnc:increment-tip-number)
  (let ((new-value (+ (gnc:current-tip-number) 1))
        (opt (gnc:lookup-global-option "__tips" "current_tip_number")))
    (if (< new-value (gnc:config-var-value-get gnc:*number-of-tips*))
	(gnc:option-set-value opt new-value)
	(gnc:option-set-value opt 0))))

(define (gnc:decrement-tip-number)
  (let ((new-value (- (gnc:current-tip-number) 1))
        (opt (gnc:lookup-global-option "__tips" "current_tip_number")))
    (if (< new-value 0)
	(gnc:option-set-value opt (- (gnc:config-var-value-get
                                      gnc:*number-of-tips*) 1))
	(gnc:option-set-value opt new-value))))

(gnc:read-tips)

(gnc:hook-add-dangler 
 gnc:*ui-startup-hook* 
 (lambda () 
   (let ((tip-opt (gnc:lookup-global-option "General"
                                            "Display \"Tip of the Day\"")))
     (if (gnc:option-value tip-opt)
         (gnc:ui-totd-dialog-create-and-run)))))



