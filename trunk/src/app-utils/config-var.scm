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

;;; config-var: You can create them, set values, find out if the value
;;; is different from the default, and you can get a description.  You
;;; can also specify an action function which will be called whenever
;;; the value is changed.  The action function receives the special
;;; var and the new value as arguments and should return either #f if
;;; the modification should be rejected, or a list containing the
;;; result otherwise.

;;; Finally, a config var has two states, "officially" modified, and
;;; unofficially modified.  You control what kind of modification
;;; you're making with the second argument to
;;; gnc:config-var-value-set!  The idea is that options specified on
;;; the command line will set the value of these config vars, but that
;;; setting is considered transient.  Other settings (like from the UI
;;; preferences panel, or normal user code) should be considered
;;; permanent, and if they leave the variable value different from the
;;; default, should be saved to the auto configuration file.

(define (gnc:make-config-var description
                             set-action-func
                             equality-func
                             default)
  (let ((var (vector description set-action-func
                     equality-func #f default default)))
    (gnc:config-var-value-set! var #f default)
    var))

(define (gnc:config-var-description-get var) (vector-ref var 0))

(define (gnc:config-var-action-func-get var) (vector-ref var 1))

(define (gnc:config-var-equality-func-get var) (vector-ref var 2))

(define (gnc:config-var-modified? var) (vector-ref var 3))
(define (gnc:config-var-modified?-set! var value) (vector-set! var 3 value))

(define (gnc:config-var-default-value-get var) (vector-ref var 4))
(define (gnc:config-var-default-value-set! var value)
  (vector-set! var 4 value))

(define (gnc:config-var-value-get var) (vector-ref var 5))
(define (gnc:config-var-value-set! var is-config-mod? value)
  (let ((set-action (gnc:config-var-action-func-get var))
        (result (list value)))
    (if set-action (set! result (set-action var value)))
    (if result
        (begin
          (if is-config-mod? (gnc:config-var-modified?-set! var #t))
          (vector-set! var 5 (car result))))))

(define (gnc:config-var-value-is-default? var)
  (if (not (gnc:config-var-modified? var))
      #t
      (let ((equal-values? (gnc:config-var-equality-func-get var)))
        (equal-values? 
         (gnc:config-var-default-value-get var)
         (gnc:config-var-value-get var)))))
