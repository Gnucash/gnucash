;; utilities.scm
;; These utilities are loaded straight off
;;
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

(if (not (defined? 'hash-fold))
    (define (hash-fold proc init table)
      (for-each 
       (lambda (bin)
         (for-each 
          (lambda (elt)
            (set! init (proc (car elt) (cdr elt) init)))
          bin))
       (vector->list table))))

(define (item-list->hash! lst hash
			  getkey getval
			  hashref hashset 
			  list-duplicates?)
  ;; Takes a list of the form (item item item item) and returns a hash
  ;; formed by traversing the list, and getting the key and val from
  ;; each item using the supplied get-key and get-val functions, and
  ;; building a hash table from the result using the given hashref and
  ;; hashset functions.  list-duplicates? determines whether or not in
  ;; the resulting hash, the value for a given key is a list of all
  ;; the values associated with that key in the input or just the
  ;; first one encountered.

  (define (handle-item item)
    (let* ((key (getkey item))
	   (val (getval item))
	   (existing-val (hashref hash key)))

      (if (not list-duplicates?)
	  ;; ignore if not first value.
	  (if (not existing-val) (hashset hash key val))
	  ;; else result is list.
	  (if existing-val
	      (hashset hash key (cons val existing-val))
	      (hashset hash key (list val))))))
	      
  (for-each handle-item lst)
  hash)

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
  (filter values lst))

(define (flatten tree)
  ;; This is ugly, but efficient -- leaves nothing pending on the
  ;; stack, and doesn't build intermediate results that it throws
  ;; away.
  (define result '())
  (define (flatten-sub-tree tree)
    (cond
     ((null? tree) #t)
     ((list? tree)
      (flatten-sub-tree (car tree))
      (flatten-sub-tree (cdr tree)))
     (else
      (set! result (cons tree result)))))
  (flatten-sub-tree tree)
  (reverse! result))

(define (striptrailingwhitespace line)
  (substring line 0 (let loop ((pos (- (string-length line) 1)))
                      (if (negative? pos)
                          0
                          (let ((candidate (string-ref line pos)))
                            (if (char-whitespace? candidate)
                                (loop (- pos 1))
                                (+ pos 1)))))))

(define (string-join lst joinstr)
  ;; This should avoid a bunch of unnecessary intermediate string-appends.
  ;; I'm presuming those are more expensive than cons...
  (if (or (not (list? lst)) (null? lst))
      ""
      (apply string-append
             (car lst)
             (let loop ((remaining-elements (cdr lst)))
               (if (null? remaining-elements)
                   '()
                   (cons joinstr (cons (car remaining-elements)
                                       (loop (cdr remaining-elements)))))))))

(define (string-split str char)
  (let ((parts '())
        (first-char #f))
    (let loop ((last-char (string-length str)))
      (set! first-char (string-rindex str char 0 last-char))
      (if first-char 
          (begin 
            (set! parts (cons (substring str (+ 1 first-char) last-char) 
                              parts))
            (loop first-char))
          (set! parts (cons (substring str 0 last-char) parts))))    
    parts))
