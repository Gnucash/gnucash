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

(define-module (gnucash utilities))

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))

(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export gnc:gui-warn)
(export gnc:gui-error)
(export gnc:gui-msg)
(export addto!)
(export sort-and-delete-duplicates)
(export gnc:list-flatten)
(export traverse-list->vec)
(export traverse-vec->list)
(export gnc:substring-replace-from-to)
(export gnc:substring-replace)
(export gnc:html-string-sanitize)

(define (strify items)
  (string-join (map (lambda (x) (format #f "~A" x)) items) ""))

(define (gnc:warn . items)
  (gnc-scm-log-warn (strify items)))

(define (gnc:error . items)
  (gnc-scm-log-error (strify items )))

(define (gnc:msg . items)
  (gnc-scm-log-msg (strify items)))

(define (gnc:debug . items)
  (when (qof-log-check "gnc.scm" QOF-LOG-DEBUG)
    (gnc-scm-log-debug (strify items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following functions are initialized to log message to tracefile
;; and will be redefined in UI initialization to display dialog
;; messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gnc:gui-warn str1 str2) (gnc:warn str1))
(define (gnc:gui-error str1 str2) (gnc:error str1))
(define (gnc:gui-msg str1 str2) (gnc:msg str1))

(define-syntax-rule (addto! alist element)
  (set! alist (cons element alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair of utility functions for use with guile-json which requires
;; lists converted vectors to save as json arrays. traverse list
;; converting into vectors, and vice versa.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (traverse-list->vec lst)
  (issue-deprecation-warning "traverse-list->vec unused.")
  (cond
   ((list? lst) (list->vector (map traverse-list->vec lst)))
   (else lst)))

(define (traverse-vec->list vec)
  (issue-deprecation-warning "traverse-vec->list unused.")
  (cond
   ((vector? vec) (map traverse-vec->list (vector->list vec)))
   (else vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general and efficient string-replace-substring function, based on
;; function designed by Mark H Weaver, core guile developer. avoids
;; string-append which will constantly build new strings. augmented
;; with start and end indices; will selective choose to replace
;; substring if start-idx <= index <= end-idx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (string-replace-substring s substr replacement #:optional
                                   (start 0)
                                   (end (string-length s))
                                   (start-idx #f)
                                   (end-idx #f))
  (let ((substr-length (string-length substr))
        (start-idx (or start-idx 0))
        (end-idx (or end-idx +inf.0)))
    (if (zero? substr-length)
        (error "string-replace-substring: empty substr")
        (let loop ((start start)
                   (i 0)
                   (pieces (list (substring s 0 start))))
          (let ((idx (string-contains s substr start end)))
            (if idx
                (loop (+ idx substr-length)
                      (1+ i)
                      (cons* (if (<= start-idx i end-idx) replacement substr)
                             (substring s start idx)
                             pieces))
                (string-concatenate-reverse (cons (substring s start)
                                                  pieces))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace
;;
;;  Search for all occurrences in string "s1" of string "s2" and
;;  replace them with string "s3".
;;
;;  Example: (gnc:substring-replace "foobarfoobar" "bar" "xyz")
;;           returns "fooxyzfooxyz".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:substring-replace s1 s2 s3)
  (string-replace-substring s1 s2 s3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace-from-to
;;  same as gnc:substring-replace extended by:
;;  start: from which occurrence onwards the replacement shall start
;;  end-after: max. number times the replacement should executed
;;
;;  Example: (gnc:substring-replace-from-to "foobarfoobarfoobar" "bar" "xyz" 2 1)
;;           returns "foobarfooxyzfoobar".
;;
;; start=1 and end-after<=0 will call gnc:substring-replace (replace all)
;; start>1 and end-after<=0 will the replace from "start" until end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:substring-replace-from-to s1 s2 s3 start end-after)
  (issue-deprecation-warning "gnc:substring-replace-from-to is deprecated in 4.x.")
  (string-replace-substring
   s1 s2 s3 0 (string-length s1) (max 0 (1- start))
   (and (positive? end-after) (+ (max 0 (1- start)) (1- end-after)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to sanitize strings. the resulting string can be safely
;; added to html.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gnc:html-string-sanitize str)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (display
          (case c
            ((#\&) "&amp;")
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            (else c))))
       str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avoid using strftime, still broken in guile-2.2. see explanation at
;; https://lists.gnu.org/archive/html/bug-guile/2019-05/msg00003.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((strftime-old strftime))
  (set! strftime
    (lambda args
      (gnc:warn "strftime may be buggy. use gnc-print-time64 instead.")
      (apply strftime-old args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a basic sort-and-delete-duplicates. because delete-duplicates
;; usually run in O(N^2) and if the list must be sorted, it's more
;; efficient to sort first then delete adjacent elements. guile-2.0
;; uses quicksort internally.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (sort-and-delete-duplicates lst < #:optional (= =))
  (define (kons a b) (if (and (pair? b) (= a (car b))) b (cons a b)))
  (reverse (fold kons '() (sort lst <))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flattens an arbitrary deep nested list into simple list.  this is
;; probably the most efficient algorithm available. '(1 2 (3 4)) -->
;; '(1 2 3 4) thanks to manumanumanu on #guile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gnc:list-flatten . lst)
  (let loop ((lst lst) (acc '()))
    (cond
     ((null? lst) acc)
     ((pair? lst) (loop (car lst) (loop (cdr lst) acc)))
     (else (cons lst acc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compatibility hack for fixing guile-2.0 string handling. this code
;; may be removed when minimum guile is 2.2 or later. see
;; https://lists.gnu.org/archive/html/guile-user/2019-04/msg00012.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string=? (effective-version) "2.0")
  ;; When using Guile 2.0.x, use monkey patching to change the
  ;; behavior of string ports to use UTF-8 as the internal encoding.
  ;; Note that this is the default behavior in Guile 2.2 or later.
  (let* ((mod                     (resolve-module '(guile)))
         (orig-open-input-string  (module-ref mod 'open-input-string))
         (orig-open-output-string (module-ref mod 'open-output-string))
         (orig-object->string     (module-ref mod 'object->string))
         (orig-simple-format      (module-ref mod 'simple-format)))

    (define (open-input-string str)
      (with-fluids ((%default-port-encoding "UTF-8"))
        (orig-open-input-string str)))

    (define (open-output-string)
      (with-fluids ((%default-port-encoding "UTF-8"))
        (orig-open-output-string)))

    (define (object->string . args)
      (with-fluids ((%default-port-encoding "UTF-8"))
        (apply orig-object->string args)))

    (define (simple-format . args)
      (with-fluids ((%default-port-encoding "UTF-8"))
        (apply orig-simple-format args)))

    (define (call-with-input-string str proc)
      (proc (open-input-string str)))

    (define (call-with-output-string proc)
      (let ((port (open-output-string)))
        (proc port)
        (get-output-string port)))

    (module-set! mod 'open-input-string       open-input-string)
    (module-set! mod 'open-output-string      open-output-string)
    (module-set! mod 'object->string          object->string)
    (module-set! mod 'simple-format           simple-format)
    (module-set! mod 'call-with-input-string  call-with-input-string)
    (module-set! mod 'call-with-output-string call-with-output-string)

    (when (eqv? (module-ref mod 'format) orig-simple-format)
      (module-set! mod 'format simple-format))))
