

;;;; Preferences

(define gnc:*arg-show-usage*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-show-help*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*debugging?*
  (gnc:make-config-var
   "Enable debugging code."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*startup-dir*
  (gnc:make-config-var
   "Location of initial lowest level scheme startup files."
   (lambda (var value)
     ;; You can't change the startup dir from here.  It's considered
     ;; hard-coded once known -- see startup/init.scm.
     #f)
   string=?
   gnc:_startup-dir-default_))

(define gnc:*config-dir*
  (gnc:make-config-var
   "Configuration directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_config-dir-default_))

(define gnc:*share-dir*
  (gnc:make-config-var
   "Shared files directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_share-dir-default_))

(define gnc:*load-path*
  (gnc:make-config-var
   "A list of strings indicating the load path for (gnc:load name).
Any path element enclosed in parentheses will automatically be
expanded to that directory and all its subdirectories whenever this
variable is modified.  The symbol element default will expand to the default directory.  i.e. (gnc:config-var-value-set! gnc:*load-path* '(\"/my/dir/\" default))"
   (lambda (var value)
     (if (not (list? value))
         #f
         (let ((result (gnc:_load-path-update_ var value)))
           (if (list? result)
               (list result)
               #f))))
   equal?
   (list 
    (string-append "(" (getenv "HOME") "/.gnucash/scm)")
    (string-append "(" gnc:_share-dir-default_ "/scm)"))))

(define gnc:*doc-path*
  (gnc:make-config-var
   "A list of strings indicating where to look for html and parsed-html files
Any path element enclosed in parentheses will automatically be
expanded to that directory and all its subdirectories whenever this
variable is modified.  The symbol element default will expand to the
default directory.  i.e. (gnc:config-var-value-set! gnc:*doc-path*
'(\"/my/dir/\" default))"
   (lambda (var value)
     (if (not (list? value))
         #f
         (let ((result (gnc:_doc-path-update_ var value)))
           (if (list? result)
               (list result)
               #f))))
   equal?
   (list
    (string-append "(" (getenv "HOME") "/.gnucash/doc)")    
    (string-append "(" gnc:_share-dir-default_ "/Docs)")
    (string-append "(" gnc:_share-dir-default_ "/Reports)"))))
