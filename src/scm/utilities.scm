;;;; $Id$
;;;; These utilities are loaded straight off
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
              (let* ((full-path (build-path dir-name item)))
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
    
    (let ((file-name (build-path (car rest) file)))
      (gnc:debug "  checking for " file-name)
      (if (access? file-name F_OK)
          (begin
            (gnc:debug "found file " file-name)
            (set! finished? #t)
            (set! result file-name))))))

(define (filteroutnulls lst)
  (cond
   ((null? lst) '())
   ((eq? (car lst) #f) (filteroutnulls (cdr lst)))
   (else
    (cons (car lst) (filteroutnulls (cdr lst))))))

(define (atom? x)
  (and
   (not (pair? x))
   (not (null? x))))

(define (flatten lst)
  (cond
   ((null? lst) '())
   ((atom? lst) (list lst))
   ((list? lst) 
    (append (flatten (car lst)) 
	    (flatten (cdr lst))))
   (else lst)))

(define (striptrailingwhitespace line)
  (let
      ((stringsize (string-length line)))
    (if
     (< stringsize 1)
     ""
     (let*
	 ((lastchar (string-ref line (- stringsize 1))))
       (if
	(char-whitespace? lastchar)
	(striptrailingwhitespace (substring line 0  (- stringsize 1)))
	line)))))

(define (string-join lst joinstr)
  (let ((len (length lst)))
    (cond 
     ((< 1 len)
      (string-append (car lst) joinstr (string-join (cdr lst) joinstr)))
     ((= 1 len)
      (car lst))
     (else
      ""))))

;;;; Simple lookup scheme; can be turned into a hash table If Need Be.
;;; Initialize lookup table
(define (initialize-hashtable . size)
  (make-vector
   (if (null? size)
       313
       (car size))
   '()))