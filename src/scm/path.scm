
(define (gnc:_expand-doc-path_ new-path)
  ;; FIXME: Bad items should probably cause this to abort with #f or
  ;; throw an exception.
  (gnc:debug "expanding doc-path value " new-path)
  (let ((path-interpret
         (lambda (item)
           (cond ((string? item) (list item))
                 ((symbol? item)
                  (case item
                    ((default)
                     (list
                      (string-append (getenv "HOME") "/.gnucash/doc")    
                      (string-append gnc:_share-dir-default_ "/Docs")
                      (string-append gnc:_share-dir-default_ "/Reports")))
                    ((current)
                     (gnc:config-var-value-get gnc:*doc-path*))
                    (else
                     (gnc:warn "bad item " item " in doc-path.  Ignoring.")
                     '())))
                 (else 
                  (gnc:warn "bad item " item " in doc-path.  Ignoring.")
                  '())))))
    (apply append (map path-interpret new-path))))

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
