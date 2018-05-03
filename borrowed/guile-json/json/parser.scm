;;; (json parser) --- Guile JSON implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-json; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (json parser)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (json->scm
            json-string->scm
            json-parser?
            json-parser-port))

;;
;; Parser record and read helpers
;;

(define-record-type json-parser
  (make-json-parser port)
  json-parser?
  (port json-parser-port))

(define (parser-peek-char parser)
  (peek-char (json-parser-port parser)))

(define (parser-read-char parser)
  (read-char (json-parser-port parser)))

(define (parser-read-delimited parser delim handle-delim)
  (let ((port (json-parser-port parser)))
    (read-delimited delim port handle-delim)))

;;
;; Number parsing helpers
;;

;; Read + or -. . If something different is found, return empty string.
(define (read-sign parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ((#\+ #\-)
       (let ((ch (parser-read-char parser)))
         (string-append s (string ch))))
      (else s))))

;; Read digits [0..9]. If something different is found, return empty
;; string.
(define (read-digits parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (parser-read-char parser)))
         (loop (parser-peek-char parser)
               (string-append s (string ch)))))
      (else s))))

(define (read-exp-part parser)
  (let ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; We might have the exponential part
      ((#\e #\E)
       (let ((ch (parser-read-char parser)) ; current char
             (sign (read-sign parser))
             (digits (read-digits parser)))
         ;; If we don't have sign or digits, we have an invalid
         ;; number.
         (if (not (and (string-null? sign)
                       (string-null? digits)))
             (string-append s (string ch) sign digits)
             #f)))
      ;; If we have a character different than e or E, we have an
      ;; invalid number.
      (else #f))))

(define (read-real-part parser)
  (let ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; If we read . we might have a real number
      ((#\.)
       (let ((ch (parser-read-char parser))
             (digits (read-digits parser)))
         ;; If we have digits, try to read the exponential part,
         ;; otherwise we have an invalid number.
         (cond
          ((not (string-null? digits))
           (let ((exp (read-exp-part parser)))
             (cond
              (exp (string-append s (string ch) digits exp))
              (else #f))))
          (else #f))))
      ;; If we have a character different than . we might continue
      ;; processing.
      (else #f))))

(define (read-number parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ((#\-)
       (let ((ch (parser-read-char parser)))
         (loop (parser-peek-char parser)
               (string-append s (string ch)))))
      ((#\0)
       (let ((ch (parser-read-char parser)))
         (string-append s
                        (string ch)
                        (or (read-real-part parser)
                            (throw 'json-invalid parser)))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (parser-read-char parser)))
         (string-append s
                        (string ch)
                        (read-digits parser)
                        (or (read-real-part parser)
                            (read-exp-part parser)
                            (throw 'json-invalid parser)))))
      (else (throw 'json-invalid parser)))))

;;
;; Object parsing helpers
;;

(define (read-pair parser)
  ;; Read string key
  (let ((key (json-read-string parser)))
    (let loop ((c (parser-peek-char parser)))
      (case c
      ;; Skip whitespaces
      ((#\ht #\vt #\lf #\cr #\sp)
       (parser-read-char parser)
       (loop (parser-peek-char parser)))
      ;; Skip colon and read value
      ((#\:)
       (parser-read-char parser)
       (cons key (json-read parser)))
      ;; invalid object
      (else (throw 'json-invalid parser))))))

(define (read-object parser)
  (let loop ((c (parser-peek-char parser))
             (pairs (make-hash-table)))
    (case c
      ;; Skip whitespaces
      ((#\ht #\vt #\lf #\cr #\sp)
       (parser-read-char parser)
       (loop (parser-peek-char parser) pairs))
      ;; end of object
      ((#\})
       (parser-read-char parser)
       pairs)
      ;; Read one pair and continue
      ((#\")
       (let ((pair (read-pair parser)))
         (hash-set! pairs (car pair) (cdr pair))
         (loop (parser-peek-char parser) pairs)))
      ;; Skip comma and read more pairs
      ((#\,)
       (parser-read-char parser)
       (loop (parser-peek-char parser) pairs))
      ;; invalid object
      (else (throw 'json-invalid parser)))))

;;
;; Array parsing helpers
;;

(define (read-array parser)
  (let loop ((c (parser-peek-char parser)) (values '()))
    (case c
      ;; Skip whitespace and comma
      ((#\ht #\vt #\lf #\cr #\sp #\,)
       (parser-read-char parser)
       (loop (parser-peek-char parser) values))
      ;; end of array
      ((#\])
       (parser-read-char parser)
       values)
      ;; this can be any json object
      (else
       (let ((value (json-read parser)))
         (loop (parser-peek-char parser)
               (append values (list value))))))))

;;
;; String parsing helpers
;;

(define (expect parser expected)
  (let ((ch (parser-read-char parser)))
    (if (not (char=? ch expected))
      (throw 'json-invalid parser)
      ch)))

(define (expect-string parser expected)
  (list->string
   (map (lambda (ch) (expect parser ch))
        (string->list expected))))

(define (read-hex-digit parser)
  (let ((c (parser-read-char parser)))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) c)
      (else (throw 'json-invalid parser)))))

(define (read-control-char parser)
  (let ((c (parser-read-char parser)))
    (case c
      ((#\" #\\ #\/) (string c))
      ((#\b) (string #\bs))
      ((#\f) (string #\ff))
      ((#\n) (string #\lf))
      ((#\r) (string #\cr))
      ((#\t) (string #\ht))
      ((#\u)
       (let* ((utf1 (string (read-hex-digit parser)
                            (read-hex-digit parser)))
              (utf2 (string (read-hex-digit parser)
                            (read-hex-digit parser)))
              (vu8 (list (string->number utf1 16)
                         (string->number utf2 16)))
              (utf (u8-list->bytevector vu8)))
         (utf16->string utf)))
      (else #f))))

(define (read-string parser)
  ;; Read characters until \ or " are found.
  (let loop ((result "")
             (current (parser-read-delimited parser "\\\"" 'split)))
    (case (cdr current)
      ((#\")
       (string-append result (car current)))
      ((#\\)
       (let ((ch (read-control-char parser)))
         (if ch
             (loop (string-append result (car current) ch)
                   (parser-read-delimited parser "\\\"" 'split))
             (throw 'json-invalid parser ))))
      (else
       (throw 'json-invalid parser)))))

;;
;; Main parser functions
;;

(define-syntax json-read-delimited
  (syntax-rules ()
    ((json-read-delimited parser delim read-func)
     (let loop ((c (parser-read-char parser)))
       (case c
         ;; skip whitespace
         ((#\ht #\vt #\lf #\cr #\sp) (loop (parser-peek-char parser)))
         ;; read contents
         ((delim) (read-func parser))
         (else (throw 'json-invalid parser)))))))

(define (json-read-true parser)
  (expect-string parser "true")
  #t)

(define (json-read-false parser)
  (expect-string parser "false")
  #f)

(define (json-read-null parser)
  (expect-string parser "null")
  #nil)

(define (json-read-object parser)
  (json-read-delimited parser #\{ read-object))

(define (json-read-array parser)
  (json-read-delimited parser #\[ read-array))

(define (json-read-string parser)
  (json-read-delimited parser #\" read-string))

(define (json-read-number parser)
  (string->number (read-number parser)))

(define (json-read parser)
  (let loop ((c (parser-peek-char parser)))
    (cond
     ;;If we reach the end we might have an incomplete document
     ((eof-object? c) (throw 'json-invalid parser))
     (else
      (case c
        ;; skip whitespaces
        ((#\ht #\vt #\lf #\cr #\sp)
         (parser-read-char parser)
         (loop (parser-peek-char parser)))
        ;; read json values
        ((#\t) (json-read-true parser))
        ((#\f) (json-read-false parser))
        ((#\n) (json-read-null parser))
        ((#\{) (json-read-object parser))
        ((#\[) (json-read-array parser))
        ((#\") (json-read-string parser))
        ;; anything else should be a number
        (else (json-read-number parser)))))))

;;
;; Public procedures
;;

(define* (json->scm #:optional (port (current-input-port)))
  "Parse a JSON document into native. Takes one optional argument,
@var{port}, which defaults to the current input port from where the JSON
document is read."
  (json-read (make-json-parser port)))

(define* (json-string->scm str)
  "Parse a JSON document into native. Takes a string argument,
@var{str}, that contains the JSON document."
  (call-with-input-string str (lambda (p) (json->scm p))))

;;; (json parser) ends here
