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

(define gnc:*_supported-files_* (make-hash-table 101))
;; Record of files that have already been loaded.  We don't do
;; anything other than record the name that the file tells us (via
;; gnc:support) that it provides.  The size of this table should
;; roughly depend on the number of .scm files in the source tree.

(define (gnc:support name)
  (hash-set! gnc:*_supported-files_* name #t))

(define (gnc:depend name)
  (let ((supported? (hash-ref gnc:*_supported-files_* name))
        (time-load? #t))
    (if supported?
        #t
        (if time-load? 
            (let* ((start-time (gettimeofday))
                   (result (gnc:load name))
                   (end-time (gettimeofday)))
              
              (simple-format #t
                             "~A elapsed load time for ~A\n"
                             (+ (- (car end-time) (car start-time))
                                (/ (- (cdr end-time) (cdr start-time))
                                   1000000))
                             name)
              result)
            (gnc:load name)))))
