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

(use-modules (srfi srfi-2))

(define gnc:*command-line-remaining* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration variables

(define gnc:*arg-show-version* #f)
(define gnc:*arg-show-usage* #f)
(define gnc:*arg-show-help* #f)
(define gnc:*arg-no-file* #f)
(define gnc:*loglevel* #f)

(define gnc:*config-path* #f)
(define gnc:*share-path* #f)
(define gnc:*doc-path* #f)

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
  
  (set! gnc:*arg-show-version*
        (gnc:make-config-var
         (N_ "Show version.")
         (lambda (var value) (if (boolean? value) (list value) #f))
         eq?
         #f))
  
  (set! gnc:*arg-show-usage*
        (gnc:make-config-var
         (N_ "Generate an argument summary.")
         (lambda (var value) (if (boolean? value) (list value) #f))
         eq?
         #f))
  
  (set! gnc:*arg-show-help*
        (gnc:make-config-var
         (N_ "Generate an argument summary.")
         (lambda (var value) (if (boolean? value) (list value) #f))
         eq?
         #f))
  
  (set! gnc:*arg-no-file*
        (gnc:make-config-var
         (N_ "Don't load any file, including autoloading the last file.")
         (lambda (var value) (if (boolean? value) (list value) #f))
         eq?
         #f))
  
  ;; Convert the temporary startup value into a config var.
  (let ((current-value gnc:*debugging?*))
    (set! 
     gnc:*debugging?*
     (gnc:make-config-var
      (N_ "Enable debugging code.")
      (lambda (var value) (if (boolean? value) (list value) #f))
      eq?
      #f))
    (gnc:config-var-value-set! gnc:*debugging?* #f current-value))
  
  (let ((current-value gnc:*develmode*))
    (set! 
     gnc:*develmode*
     (gnc:make-config-var
      (N_ "Enable developers mode.")
      (lambda (var value) (if (boolean? value) (list value) #f))
      eq?
      #f))
    (gnc:config-var-value-set! gnc:*develmode* #f current-value))
  
  (set! gnc:*loglevel*
        (gnc:make-config-var
         (N_ "Logging level from 0 (least logging) to 5 (most logging).")
         (lambda (var value) (if (exact? value) (list value) #f))
         eq?
         #f))
  
  (set! gnc:*config-path*
        (gnc:make-path-config-var
         (N_ "List of directories to search when lookng for config files. \
Each element must be a string representing a directory or a symbol \
where 'default expands to the default path, and 'current expands to \
the current value of the path.")
         (lambda () gnc:_install-config-path_)))
  
  (set! gnc:*share-path*
        (gnc:make-path-config-var
         (N_ "List of directories to search when lookng for shared data files. \
Each element must be a string representing a directory or a symbol \
where 'default expands to the default path, and 'current expands to \
the current value of the path.")
         (lambda () gnc:_install-share-path_)))
  

  (set! gnc:*doc-path*
        (gnc:make-path-config-var
         (N_ "A list of directories (strings) indicating where to look for html and parsed-html files. \
Each element must be a string representing a directory or a symbol \
where 'default expands to the default path, and 'current expands to \
the current value of the path.")
         (let ((result (cons
                        (build-path (getenv "HOME") ".gnucash" "html")
                        gnc:_install-doc-path_)))
         (lambda () result))))


  ;; Now handle any envt var overrides.

  (and-let* ((envdir (getenv "GNC_CONFIG_PATH"))
             (data (gnc:read-from-string envdir))
             ((list? data)))
    (gnc:config-var-value-set! gnc:*config-path* #f (gnc:flatten data)))
             
  (and-let* ((envdir (getenv "GNC_SHARE_PATH"))
             (data (gnc:read-from-string envdir))
             ((list? data)))
    (gnc:config-var-value-set! gnc:*share-path* #f (gnc:flatten data)))

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
   (list "version"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*arg-show-version* #f val))
         #f
         (N_ "Show GnuCash version"))

   (list "usage"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*arg-show-usage* #f val))
         #f
         (N_ "Show GnuCash usage information"))

   (list "help"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*arg-show-help* #f val))
         #f
         (N_ "Show this help message"))

   (list "debug"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*debugging?* #f val))
         #f
         (N_ "Enable debugging mode"))

   (list "devel"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*develmode* #f val))
         #f
         (N_ "Enable developers mode"))
   
   (list "loglevel"
         'integer
         (lambda (val)
           (gnc:config-var-value-set! gnc:*loglevel* #f val))
         "LOGLEVEL"
         (N_ "Set the logging level from 0 (least) to 6 (most)"))

   (list "nofile"
         'boolean
         (lambda (val)
           (gnc:config-var-value-set! gnc:*arg-no-file* #f val))
         #f
         (N_ "Do not load the last file opened"))

   (list "config-path"
         'string
         (lambda (val)
           (gnc:debug "parsing --config-path " val)
           (let ((path-list (gnc:read-from-string val)))
             (if (list? path-list)
                 (gnc:config-var-value-set! gnc:*config-path* #f path-list)
                 (begin
                   (gnc:error "non-list given for --config-path: " val)
                   (gnc:shutdown 1)))))
         "CONFIGPATH"
         (N_ "Set configuration path"))

   (list "share-path"
         'string
         (lambda (val)
           (gnc:debug "parsing --share-path " val)
           (let ((path-list (gnc:read-from-string val)))
             (if (list? path-list)
                 (gnc:config-var-value-set! gnc:*share-path* #f path-list)
                 (begin
                   (gnc:error "non-list given for --share-path: " val)
                   (gnc:shutdown 1)))))
         "SHAREPATH"
         (N_ "Set shared data file search path"))

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
   
   (list "evaluate"
         'string
         (lambda (val)
           (set! gnc:*batch-mode-things-to-do*
                 (cons val gnc:*batch-mode-things-to-do*)))
         "COMMAND"
         (N_ "Evaluate the guile command"))

   ;; Given a string, --load will load the indicated file, if possible.
   (list "load"
         'string
         (lambda (val)
           (set! gnc:*batch-mode-things-to-do*
                 (cons (lambda () (load val))
                       gnc:*batch-mode-things-to-do*)))
         "FILE"
         (N_ "Load the given .scm file"))

   (list "add-price-quotes"
         'string
         (lambda (val)
           (set! gnc:*batch-mode-things-to-do*
                 (cons
                  (lambda ()
                    (gnc:use-guile-module-here! '(gnucash price-quotes))
                    (gnc:suspend-gui-refresh)
                    (if (not (gnc:add-quotes-to-book-at-url val))
                        (begin
                          (gnc:error "Failed to add quotes to " val)
                          (gnc:shutdown 1)))
                    (gnc:resume-gui-refresh))
                  gnc:*batch-mode-things-to-do*)))
         "FILE"
         (N_ "Add price quotes to given FILE."))

   (list "load-user-config"
         'boolean
         gnc:load-user-config-if-needed
         #f
         (N_ "Load the user configuration"))

   (list "load-system-config"
         'boolean
         gnc:load-system-config-if-needed
         #f
         (N_ "Load the system configuration"))

   (list "rpc-server"
	 'boolean
	 (lambda (val)
	   (if val (gnc:run-rpc-server)))
	 #f
	 (N_ "Run the RPC Server"))))

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

(define (gnc:prefs-show-version)
  (display "GnuCash ")
  (display gnc:version)
  (display " development version")
  (newline))

(define (gnc:prefs-show-usage)
  (display "Usage: gnucash [ option ... ] [ datafile ]")
  (newline) (newline)
  (let ((max 0))
    (map (lambda (arg-def)
           (let* ((name (car arg-def))
                  (arg (cadddr arg-def))
                  (len (+ 4 (string-length name)
                          (if arg (+ (string-length arg) 1) 0))))
             (if (> len max)
                 (set! max len))))
         gnc:*arg-defs*)
    (set! max (+ max 4))
    (map (lambda (arg-def)
           (let* ((name (car arg-def))
                  (arg (cadddr arg-def))
                  (len (+ 4 (string-length name)
                          (if arg (+ (string-length arg) 1) 0)))
                  (help (car (cddddr arg-def))))
             (display "  --")
             (display name)
             (if arg
                 (begin
                   (display " ")
                   (display arg)))
             (display (make-string (- max len) #\ ))
             (display (_ help))
             (newline)))
         gnc:*arg-defs*)))

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
			 (make-shared-substring item 0
						(min (string-length item) 2))))
          (begin
            (gnc:debug "non-option " item ", assuming file")
            (set! rest (cdr rest))
            (set! remaining-arguments (cons item remaining-arguments)))

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
