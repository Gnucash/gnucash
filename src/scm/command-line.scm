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

;; also "-c"

(define gnc:*command-line-files* #f)

(define gnc:*arg-defs*
  (list
   
   (cons
    "version"
    (cons 'boolean
          (lambda (val)
            (gnc:config-var-value-set! gnc:*arg-show-version* #f val))))
   (cons
    "usage"
    (cons 'boolean
          (lambda (val)
            (gnc:config-var-value-set! gnc:*arg-show-usage* #f val))))
   (cons
    "help"
    (cons 'boolean
          (lambda (val)
            (gnc:config-var-value-set! gnc:*arg-show-help* #f val))))
   (cons
    "debug"
    (cons 'boolean
          (lambda (val)
            (gnc:config-var-value-set! gnc:*debugging?* #f val))))

   (cons
    "loglevel"
    (cons 'integer
          (lambda (val)
            (gnc:config-var-value-set! gnc:*loglevel* #f val))))

   (cons
    "nofile"
    (cons 'boolean
          (lambda (val)
            (gnc:config-var-value-set! gnc:*arg-no-file* #f val))))

   (cons
    "config-dir"
    (cons 'string
          (lambda (val)
            (gnc:config-var-value-set! gnc:*config-dir* #f val))))

   (cons
    "share-dir"
    (cons 'string
          (lambda (val)
            (gnc:config-var-value-set! gnc:*share-dir* #f val))))

   (cons
    "load-path"
    (cons 'string
          (lambda (val)
            (let ((path-list
                   (call-with-input-string val (lambda (port) (read port)))))
              (if (list? path-list)
                  (gnc:config-var-value-set! gnc:*load-path* #f path-list)
                  (begin
                    (gnc:error "non-list given for --load-path: " val)
                    (gnc:shutdown 1)))))))
   
   (cons
    "doc-path"
    (cons 'string
          (lambda (val)
            (gnc:debug "parsing --doc-path " val)
            (let ((path-list
                   (call-with-input-string val (lambda (port) (read port)))))
              (if (list? path-list)
                  (gnc:config-var-value-set! gnc:*doc-path* #f path-list)
                  (begin
                    (gnc:error "non-list given for --doc-path: " val)
                    (gnc:shutdown 1)))))))

   (cons "evaluate"
         (cons 'string
               (lambda (val)
                 (set! gnc:*batch-mode-things-to-do*
                       (cons val gnc:*batch-mode-things-to-do*)))))
   
   ;; Given a string, --load will load the indicated file, if possible.
   (cons "load"
         (cons 'string
               (lambda (val)
                 (set! gnc:*batch-mode-things-to-do*
                       (cons (lambda () (load val))
                             gnc:*batch-mode-things-to-do*)))))

   (cons "load-user-config"
         (cons 'boolean gnc:load-user-config-if-needed))

   (cons "load-system-config"
         (cons 'boolean gnc:load-system-config-if-needed))))

(define (gnc:cmd-line-get-boolean-arg args)
  ;; --arg         means #t
  ;; --arg true    means #t
  ;; --arg false   means #f

  (if (not (pair? args))
      ;; Special case of end of list
      (list #t args)
      (let ((arg (car args)))
        (if (string=? arg "false")
            (list #f (cdr args))
            (list #t 
                  (if (string=? arg "true")
                      (cdr args)
                      args))))))

(define (gnc:cmd-line-get-integer-arg args)
  (let ((arg (car args)))    
    (let ((value (string->number arg)))
      (if (not value)
          #f
          (if (not (exact? value))
              #f
              (list value (cdr args)))))))

(define (gnc:cmd-line-get-string-arg args)
  (gnc:debug "got string arg returning " (car args) " and " (cdr args))
  (list (car args) (cdr args)))

(define (gnc:prefs-show-version)
  (display "GnuCash ")
  (display gnc:version)
  (display " development version")
  (newline))

(define (gnc:prefs-show-usage)
  (display "usage: gnucash [ option ... ] [ datafile ]") (newline))


(define (gnc:handle-command-line-args)
  (gnc:debug "handling command line arguments" (program-arguments))
  
  (let ((files-to-open '())
        (result #t))
    
    (do ((rest (cdr (program-arguments))) ; initial cdr skips argv[0]
         (quit? #f)
         (item #f))
        ((or quit? (null? rest)))
      
      (set! item (car rest))      
      
      (gnc:debug "handling arg " item)
 
      (if (not (string=? "--"
			 (make-shared-substring item 0
						(min (string-length item) 2))))
          (begin
            (gnc:debug "non-option " item ", assuming file")
            (set! rest (cdr rest))
            (set! files-to-open (cons item files-to-open)))
          
          (if (string=? "--" item)
              ;; ignore --
              (set! rest (cdr rest))
              ;; Got something that looks like an option...
              (let* ((arg-string (make-shared-substring item 2))
                     (arg-def (assoc-ref gnc:*arg-defs* arg-string)))
            
                (if (not arg-def)
                    (begin
                      ;;(gnc:prefs-show-usage)
                      ;;(set! result #f)
                      ;;(set! quit? #t))
                      (display "Ignoring unused option ")(display arg-string)
                      (newline)
		      (set! rest (cdr rest)))
                    
                    (let* ((arg-type (car arg-def))
                           (arg-parse-result
                            (case arg-type
                              ((boolean) (gnc:cmd-line-get-boolean-arg (cdr rest)))
                              ((string) (gnc:cmd-line-get-string-arg (cdr rest)))
                              ((integer)
                               (gnc:cmd-line-get-integer-arg (cdr rest)))
                              (else
                               (gnc:error "bad argument type " arg-type ".")
                               (gnc:shutdown 1)))))
                      
                      (if (not arg-parse-result)
                          (begin                
                            (set result #f)
                            (set! quit? #t))
                          (let ((parsed-value (car arg-parse-result))
                                (remaining-args (cadr arg-parse-result)))
                            ((cdr arg-def) parsed-value) 
                            (set! rest remaining-args)))))))))
    (if result
        (gnc:debug "files to open: " files-to-open))
    
    (set! gnc:*command-line-files* files-to-open)
    
    result))
