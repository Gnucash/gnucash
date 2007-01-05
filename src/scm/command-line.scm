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

(use-modules (srfi srfi-2))

(define gnc:*command-line-remaining* #f)

(define (gnc:flatten tree)
  (let ((result '()))
    (let loop ((remaining-items tree))
      (cond
       ((null? remaining-items) #t)
       ((list? remaining-items)
        (loop (car remaining-items))
        (loop (cdr remaining-items)))
       (else
        (set! result (cons remaining-items result)))))
    (reverse! result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration variables

(define gnc:*doc-path* #f)

(define (gnc:expand-path new-list current-list default-generator)
  (define (expand-path-item item)
    (cond ((string? item) (list item))
          ((symbol? item)
           (case item
             ((default) (default-generator))
             ((current) current-list)
             (else
              (gnc:warn "bad symbol " item " in gnc path. Ignoring.")
              '())))
          (else 
           (gnc:warn "bad item " item " in gnc path. Ignoring.")
           '())))
  (apply append (map expand-path-item new-list)))

;; If command line args are present, then those dominate, and take
;; effect in order, left-to-right.  Otherwise, any envt var setting
;; dominates, otherwise, we use the default.  To effect this, we first
;; set it to the default, then later we process the envt vars, then
;; lastly we process the command line.

(define (gnc:make-path-config-var name default-path-func)
  (gnc:make-config-var
   name
   (lambda (var value)
     (let ((result (gnc:expand-path value
                                    (gnc:config-var-value-get var)
                                    default-path-func)))
       (if (list? result)
           (list result)
           #f)))
   equal?
   '(default)))

(define (gnc:read-from-string str)
  (call-with-input-string str (lambda (port) (read port))))

(define (gnc:initialize-config-vars)
  ;; We use a function so we don't do this at file load time.
  
  ;; Convert the temporary startup value into a config var.
  (set! gnc:*doc-path*
        (gnc:make-path-config-var
         (N_ "A list of directories (strings) indicating where to look for html and parsed-html files. \
Each element must be a string representing a directory or a symbol \
where 'default expands to the default path, and 'current expands to \
the current value of the path.")
         (let ((result (cons
                        (gnc-build-dotgnucash-path "html")
                        gnc:_install-doc-path_)))
         (lambda () result))))


  ;; Now handle any envt var overrides.

  (and-let* ((envdir (getenv "GNC_DOC_PATH"))
             (data (gnc:read-from-string envdir))
             ((list? data)))
    (gnc:config-var-value-set! gnc:*doc-path* #f (gnc:flatten data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument parsing.

;; This is a fairly complex initialization during load, but it's OK
;; for now since it doesn't depend on running any code.
(define gnc:*arg-defs*
  (list
   (list "doc-path"
         'string
         (lambda (val)
           (gnc:debug "parsing --doc-path " val)
           (let ((path-list (gnc:read-from-string val)))
             (if (list? path-list)
                 (gnc:config-var-value-set! gnc:*doc-path* #f path-list)
                 (begin
                   (gnc:error "non-list given for --doc-path: " val)
                   (gnc:shutdown 1)))))
         "DOCPATH"
         (N_ "Set the search path for documentation files"))
   

))

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
  (if (pair? args)
      (list (car args) (cdr args))
      (begin (gnc:warn "no argument given where one expected") #f)))

;;(define (gnc:handle-command-line-args)
;;  (letrec ((internal
;;            (lambda ()
;;              (getopt-long (program-arguments)
;;                           (gnc:convert-arg-defs-to-opt-args gnc:*arg-defs*))))
;;           (arg-handler
;;            (lambda (args)
;;              (if (pair? args)
;;                  (begin
;;                   (let ((one-arg (car args)))
;;                     (if (eq? (car one-arg) '())
;;                         (set! gnc:*command-line-remaining* (cdr one-arg))
;;                         (let* ((arg-name (symbol->string (car one-arg)))
;;                                (arg-stuff (assoc-ref gnc:*arg-defs* arg-name)))
;;                           (case (car arg-stuff)
;;                             ((string)
;;                              ((cdr arg-stuff) (cdr one-arg)))
;;                             ((integer)
;;                              ((cdr arg-stuff) (gnc:convert-arg-to-integer
;;                                                (cdr one-arg))))
;;                             ((boolean)
;;                              ((cdr arg-stuff) (gnc:convert-arg-to-boolean
;;                                                (cdr one-arg))))))))
;;                   (arg-handler (cdr args)))))))
;;    (display "Converted") (newline)
;;    (display (gnc:convert-arg-defs-to-opt-args gnc:*arg-defs*)) (newline)
;;    (flush-all-ports)
;;    (arg-handler (internal)))
;;  #t)


(define (gnc:handle-command-line-args)
  (gnc:debug "handling command line arguments" (program-arguments))
  
  (let ((remaining-arguments '())
        (result #t))
    
    (do ((rest (cdr (program-arguments))) ; initial cdr skips argv[0]
         (quit? #f)
         (item #f))
        ((or quit? (null? rest)))
      
      (set! item (car rest))      
      
      (gnc:debug "handling arg " item)
      
      (if (not (string=? "--"
			 (substring item 0
                                    (min (string-length item) 2))))
          (begin
            (gnc:debug "non-option " item ", assuming file")
            (set! rest (cdr rest))
            (set! remaining-arguments (cons item remaining-arguments)))

          (if (string=? "--" item)
              ;; ignore --
              (set! rest (cdr rest))
              ;; Got something that looks like an option...
              (let* ((arg-string (substring item 2))
                     (arg-def (assoc-ref gnc:*arg-defs* arg-string)))

                (if (not arg-def)
                    (begin
                      ;;(gnc:prefs-show-usage)
                      ;;(set! result #f)
                      ;;(set! quit? #t))
                      (set! remaining-arguments
                            (cons item remaining-arguments))
		      (set! rest (cdr rest)))

                    (let* ((arg-type (car arg-def))
                           (arg-parse-result
                            (case arg-type
                              ((boolean) (gnc:cmd-line-get-boolean-arg
                                          (cdr rest)))
                              ((string) (gnc:cmd-line-get-string-arg
                                         (cdr rest)))
                              ((integer)
                               (gnc:cmd-line-get-integer-arg (cdr rest)))
                              (else
                               (gnc:error "bad argument type " arg-type ".")
                               (gnc:shutdown 1)))))

                      (if (not arg-parse-result)
                          (begin                
                            (set! result #f)
                            (set! quit? #t))
                          (let ((parsed-value (car arg-parse-result))
                                (remaining-args (cadr arg-parse-result)))
                            ((cadr arg-def) parsed-value) 
                            (set! rest remaining-args)))))))))
    (if result
        (gnc:debug "files to open: " remaining-arguments))

    (set! gnc:*command-line-remaining* remaining-arguments)

    result))
