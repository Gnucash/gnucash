

;;;; Warning functions...

(define (gnc:warn . items)
  (display "gnucash: [W] ")
  (for-each (lambda (i) (display i)) items)
  (newline))

(define (gnc:error . items)
  (display "gnucash: [E] ")
  (for-each (lambda (i) (display i)) items)
  (newline))

(define (gnc:msg . items)
  (display "gnucash: [M] ")
  (for-each (lambda (i) (display i)) items)
  (newline))

(define (gnc:debug . items)
  (if (gnc:config-var-value-get gnc:*debugging?*)
      (begin
        (display "gnucash: [D] ")
        (for-each (lambda (i) (display i)) items)
        (newline))))


(define (directory? path)
  ;; This follows symlinks normally.
  (let* ((status (false-if-exception (stat path)))
         (type (if status (stat:type status) #f)))
    (eq? type 'directory)))

(define (gnc:directory-subdirectories dir-name)
  ;; Return a recursive list of the subdirs of dir-name, including
  ;; dir-name.  Follow symlinks.
  
  (let ((dir-port (opendir dir-name)))
    (if (not dir-port)
        #f
        (do ((item (readdir dir-port) (readdir dir-port))
             (dirs '()))
            ((eof-object? item) (reverse dirs))

          (if (not (or (string=? item ".")
                       (string=? item "..")))
              (let* ((full-path (string-append dir-name "/" item)))
                ;; ignore symlinks, etc.
                (if (access? full-path F_OK)
                    (let* ((status (lstat full-path))
                           (type (if status (stat:type status) #f)))
                      (if (and (eq? type 'directory))
                          (set! dirs
                                (cons full-path 
                                      (append 
                                       (gnc:directory-subdirectories full-path)
                                       dirs))))))))))))

(define (gnc:find-in-directories file directories)
  "Find file named 'file' anywhere in 'directories'.  'file' must be a
string and 'directories' must be a list of strings."

  (gnc:debug "gnc:find-in-directories looking for " file " in " directories)
  
  (do ((rest directories (cdr rest))
       (finished? #f)
       (result #f))
      ((or (null? rest) finished?) result)
    
    (let ((file-name (string-append (car rest) "/" file)))
      (gnc:debug "  checking for " file-name)
      (if (access? file-name F_OK)
          (begin
            (gnc:debug "found file " file-name)
            (set! finished? #t)
            (set! result file-name))))))
