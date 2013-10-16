;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  migrate-prefs.scm
;;;  Functions used to migrated user preferences from gconf
;;;  to gsettings. Note that this module doesn't perform the
;;;  migration itself: it merely prepares the environment to
;;;  create the actual migration script.
;;;
;;;  Copyright 2013 Geert Janssens <geert@kobaltwit.be>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (migrate-prefs))

(use-modules (gnucash main))

(define gconf-dir "")
(define prefix-length 0)
(define migration-dir "")

(define (copy-one-file filename)
(let ((stats (stat filename))
      (base-name "")
      (slash-index 0)
      (dest-name ""))
     (gnc:debug "Processing file... " filename)
     (if (eq? (stat:type stats) 'regular)
       (begin
         (set! base-name (string-drop filename prefix-length))
         (set! slash-index (- (string-rindex base-name #\%) 1))
         (if (> slash-index 0)
           (begin
             (set! dest-name (string-take base-name (- (string-rindex base-name #\%) 1))) 
             (set! dest-name (string-join (string-split dest-name #\/) "-"))
             (set! dest-name (string-append migration-dir "/" dest-name ".xml"))
             (gnc:debug "Copying " base-name " -> " dest-name)
             (copy-file filename dest-name)
         ))))
     (if (eq? (stat:type stats) 'directory)
         (apply find copy-one-file (list filename))
         )))


(define (directory-files dir)
  (if (not (access? dir R_OK))
    '()
    (let* ((p (opendir dir))
           (filelist (do ((file (readdir p) (readdir p))
                          (ls '()))
                         ((eof-object? file) (closedir p) (reverse! ls))
                         (if (not (string-suffix? "." file))
                             (set! ls (cons file ls)))
                         )))
          (sort filelist string<))))

(define (find proc . dirs)
  (cond ((pair? dirs)
         (for-each proc (map (lambda (x) (string-append (car dirs) "/" x))
                             (directory-files (car dirs)))))))

(define (finddepth proc . dirs)
  (cond ((pair? dirs)
         (apply finddepth proc (cdr dirs))
         (for-each proc (map (lambda (x) (string-append (car dirs) "/" x))
                             (directory-files (car dirs)))))))


(define (migration-prepare-internal)
; cleanup first if a previous migration attempt failed to do so
  (if (access? migration-dir (logior R_OK W_OK X_OK))
      (begin
        (gnc:msg "Clear previous migration tmp dir " migration-dir)
        (migration-cleanup-internal)))
  (gnc:warn "*** GnuCash switched to a new preferences system ***")
  (gnc:warn "Attempt to migrate your preferences from the old to the new system")
  (mkdir migration-dir)
  (gnc:msg "Copy all gconf files to tmp dir " migration-dir)
  (apply find copy-one-file (list gconf-dir))
  ; Indicate successful preparation
  #t
)

(define (migration-prepare base-dir)
  (set! gconf-dir (string-append base-dir "/.gconf/apps/gnucash"))
  ; Note: calling script should already have checked whether 
  ;       gconf-dir and its parent directories exist
  (set! prefix-length (+ (string-length gconf-dir) 1))
  (set! migration-dir (string-append base-dir "/.gnc-migration-tmp"))
  (catch #t
    migration-prepare-internal
    (lambda (key . args) 
            (gnc:error "An error occurred while preparing to migrate preferences.")
            (gnc:error "The error is: "
                       (symbol->string key) " - "  (car (caddr args))  ".")
            #f))
)

(define (rmtree args)
  (define (zap f)
    (let ((rm (if (eq? (stat:type (stat f)) 'directory) rmdir delete-file)))
      (gnc:debug "deleting " f)
      (catch #t
        (lambda () (rm f))
        (lambda args (format #t "couldn't delete ~A\n" f)))))
  (apply finddepth zap args))

(define (migration-cleanup-internal)
  (rmtree (list migration-dir))
  (rmdir migration-dir)
  ; Indicate successful cleanup
  #t)

(define (migration-cleanup base-dir)
  (set! migration-dir (string-append base-dir "/.gnc-migration-tmp"))
  (if (access? migration-dir (logior R_OK W_OK X_OK))
    (begin
      (gnc:msg "Delete tmp dir " migration-dir)
      (catch #t
        migration-cleanup-internal
        (lambda (key . args) 
            (gnc:error "An error occurred while cleaning up after preferences migration.")
            (gnc:error "The error is: "
                       (symbol->string key) " - "  (car (caddr args))  ".")
            #f))))
)

(export migration-prepare migration-cleanup)