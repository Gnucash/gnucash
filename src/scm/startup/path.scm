
(define gnc:_load-path-directories_ #f)
(define gnc:_doc-path-directories_ #f)  

(define (gnc:_path-expand_ items default-items)
  (if
   (null? items)
   '()  
   (let ((item (car items))
         (other-items (cdr items)))
     (cond
      ((eq? item 'default)
       (append 
        (gnc:_path-expand_ default-items))
       (gnc:_path-expand_ other-items default-items))
      ((string? item)
       (if (and (char=? #\( (string-ref item 0))
                (char=? #\) (string-ref item (- (string-length item) 1))))
           
           (let ((current-dir
                  (make-shared-substring item 1 (- (string-length item) 1))))
             
             (if (directory? current-dir)
                 (let ((subdirs (gnc:directory-subdirectories current-dir))
                       (rest (gnc:_path-expand_ other-items default-items)))
                   (cons current-dir (append subdirs rest)))
                 (begin
                   (gnc:warn "Ignoring non-directory " current-dir
                             " in gnc:_path-expand_ item.")
                   (gnc:_path-expand_ other-items default-items))))
           (if (directory? item)
               (begin
                 (gnc:warn "Ignoring non-directory " item
                           " in gnc:_path-expand_ item.")
                 (gnc:_path-expand_ other-items default-items))
               (cons item (gnc:_path-expand_ other-items default-items)))))
      (else (gnc:warn "Ignoring invalid item " item " in gnc:_path-expand_.")
            (gnc:_path-expand_ other-items default-items))))))

(define (gnc:_load-path-update_ var items)
  (gnc:msg "Updating load path from " items)
  (let ((result (gnc:_path-expand_
                 items
                 (gnc:config-var-default-value-get var))))
    (if result
        (begin
          (set! gnc:_load-path-directories_ result)
          (gnc:msg "  Setting path to " result)
          result)
        (begin 
          (gnc:msg "  No path... " result)
          #f))))

(define (gnc:_doc-path-update_ var items)
  (let ((result (gnc:_path-expand_
                 items
                 (gnc:config-var-default-value-get var))))
    (if result
        (begin
          (set! gnc:_doc-path-directories_ result)
          result)
        #f)))

;; It may make sense to dump this in favor of guile's load-path later,
;; but for now this works, and having gnc things separate may be less
;; confusing and avoids shadowing problems.

(define (gnc:load name)
  "Name must be a string.  The system attempts to locate the file of
the given name and load it.  The system will attempt to locate the
file in all of the directories specified by gnc:*load-path*."
  
  (let ((file-name (gnc:find-in-directories name gnc:_load-path-directories_)))
    (if (not file-name)
        #f
        (if (false-if-exception (primitive-load file-name))
            (begin
              (gnc:debug "loaded file " file-name)
              #t)
            (begin
              (gnc:warn "failure loading " file-name)
              #f)))))

(define gnc:load-user-config-if-needed
  (let ((user-config-loaded? #f))
    (lambda ()
      (if (not user-config-loaded?)
          (begin
            (gnc:debug "loading user configuration")
            
            (let ((user-file
                   (string-append (getenv "HOME") "/.gnucash/config.user"))
                  (auto-file
                   (string-append (getenv "HOME") "/.gnucash/config.auto")))
              
              (if (access? user-file F_OK)
                  (if (false-if-exception (primitive-load user-file))
                      (set! user-config-loaded? #t)
                      (begin
                        (gnc:warn "failure loading " user-file)
                        #f))
                  (if (access? auto-file F_OK)
                      (if (false-if-exception (primitive-load auto-file))
                          (set! user-config-loaded? #t)
                          (begin
                            (gnc:warn "failure loading " auto-file)
                            #f))))))))))

;; the system config should probably be loaded from some directory
;; that wouldn't be a site wide mounted directory, like /usr/share
;; However, the code below seems to zero in on /usr/share/gnucash/config
;; ... ahh but that's OK, right ??
(define gnc:load-system-config-if-needed
  (let ((system-config-loaded? #f))
    (lambda ()
      (if (not system-config-loaded?)
          (begin
            (gnc:debug "loading system configuration")
            
            (let ((system-config (string-append
                                  (gnc:config-var-value-get gnc:*config-dir*)
                                  "/config")))
              
              (if (false-if-exception (primitive-load system-config))
                  (set! system-config-loaded? #t)        
                  (begin
                    (gnc:warn "failure loading " system-config)
                    #f))))))))
