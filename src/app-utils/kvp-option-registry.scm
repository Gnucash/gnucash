;; Code for the kvp/option registry
;;
;; Copyright (C) 2002, Derek Atkins  <derek@ihtfp.com>
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


(require 'hash)

(define gnc:*kvp-option-registry* (make-hash-table 23))

(define (get-ref id-type)
  (let ((gen-list (hash-ref gnc:*kvp-option-registry* id-type)))
    (if gen-list gen-list '())))


;;
;; the generator should be a procedure that takes one argument,
;; an options object.  The procedure should fill in the options with
;; its defined kvp options.
;;
(define (gnc:register-kvp-option-generator id-type generator)
  (let ((gen-list (get-ref id-type)))
    (hash-set! gnc:*kvp-option-registry*
	       id-type (append gen-list (list generator)))))

(define (gnc:unregister-kvp-option-generator id-type generator)
  (let ((gen-list (get-ref id-type)))
    (hash-set! gnc:*kvp-option-registry*
	       id-type (delq! generator gen-list))))

;;
;; create a new options object for the requested type
;;
(define (gnc:make-kvp-options id-type)
  (let ((gen-list (get-ref id-type))
	(options (gnc:new-options)))
    (map
     (lambda (generator)
       (generator options))
     gen-list)

    options))
