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

(gnc:support "doc.scm")

(define (gnc:find-doc-file file)
  (gnc:find-in-directories file (gnc:config-var-value-get gnc:*doc-path*)))

(define (gnc:load-help-topics fname) 
  (with-input-from-file
      (gnc:find-in-directories fname
                               (gnc:config-var-value-get gnc:*load-path*))
    (lambda () (read))))
