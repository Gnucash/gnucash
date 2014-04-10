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

(define (gnc:find-doc-file file)
  (gnc:find-localized-file file (gnc:config-var-value-get gnc:*doc-path*)))

(define (remove-i18n-macros input)
  (cond ((null? input) input)
        ((list? input)
         (cond ((eq? (car input) 'N_) (cadr input))
               (else (cons (remove-i18n-macros (car input))
                           (remove-i18n-macros (cdr input))))))
        (else input)))

(define (fill-out-topics input)
  (define (first-non-blank-url input)
    (cond ((null? input) "")
          ((list? input)
           (cond ((and (string? (car input)) (not (eq? "" (cadr input))))
                  (cadr input))
                 (else (let ((first (first-non-blank-url (car input))))
                         (if (not (eq? "" first))
                             first
                             (first-non-blank-url (cdr input)))))))
          (else "")))

  (cond ((null? input) input)
        ((list? input)
         (cond ((and (string? (car input)) (eq? "" (cadr input)))
                (cons (car input)
                      (cons (first-non-blank-url (caddr input))
                            (fill-out-topics (cddr input)))))
               (else (cons (fill-out-topics (car input))
                           (fill-out-topics (cdr input))))))
        (else input)))

(define (gnc:load-help-topics fname) 
  ;; Should this be %load-path, or should we use doc-path, and should
  ;; topics be a .scm file, or just a file since there's no code in
  ;; there?
  (with-input-from-file (%search-load-path fname)
    (lambda ()
      (fill-out-topics (remove-i18n-macros (read))))))
