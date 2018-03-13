;;; (json builder) --- Guile JSON implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2015,2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (json builder)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (scm->json
            scm->json-string))

;;
;; String builder helpers
;;

(define (unicode->string unicode)
  (format #f "\\u~4,'0x" unicode))

(define (char->unicode-string c)
  (let ((unicode (char->integer c)))
    (if (< unicode 32)
        (unicode->string unicode)
        (string c))))

(define (u8v-2->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1)))
    (+ (ash (logand bv0 #b00011111) 6)
       (logand bv1 #b00111111))))

(define (u8v-3->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1))
        (bv2 (bytevector-u8-ref bv 2)))
    (+ (ash (logand bv0 #b00001111) 12)
       (ash (logand bv1 #b00111111) 6)
       (logand bv2 #b00111111))))

(define (build-char-string c)
  (let* ((bv (string->utf8 (string c)))
         (len (bytevector-length bv)))
    (cond
     ;; A single byte UTF-8
     ((eq? len 1) (char->unicode-string c))
     ;; If we have a 2 or 3 byte UTF-8 we need to output it as \uHHHH
     ((or (eq? len 2) (eq? len 3))
      (let ((unicode (if (eq? len 2)
                         (u8v-2->unicode bv)
                         (u8v-3->unicode bv))))
        (unicode->string unicode)))
     ;; Anything else should wrong, hopefully.
     (else (throw 'json-invalid)))))

;;
;; Object builder functions
;;

(define (build-object-pair p port escape pretty level)
  (display (indent-string pretty level) port)
  (json-build-string (car p) port escape)
  (display " : " port)
  (json-build (cdr p) port escape pretty level))

(define (build-newline port pretty)
  (cond (pretty (newline port))))

(define (indent-string pretty level)
  (if pretty (format #f "~v_" (* 4 level)) ""))

;;
;; Main builder functions
;;

(define (json-build-null port)
  (display "null" port))

(define (json-build-boolean scm port)
  (display (if scm "true" "false") port))

(define (json-build-number scm port)
  (if (and (rational? scm) (not (integer? scm)))
      (display (number->string (exact->inexact scm)) port)
      (display (number->string scm) port)))

(define (->string x)
  (cond ((char? x) (make-string 1 x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))

(define (atom? x)
  (or (char? x) (number? x) (string? x) (symbol? x)))

(define (json-alist? x)
  (and (pair? x)
       (let loop ((x x))
         (or (null? x)
             (null? (car x))
             (and (pair? (car x)) (atom? (caar x))
                  (loop (cdr x)))))))

(define (json-build-string scm port escape)
  (display "\"" port)
  (display
   (list->string
    (fold-right append '()
                (map
                 (lambda (c)
                   (case c
                     ((#\" #\\) `(#\\ ,c))
                     ((#\bs) '(#\\ #\b))
                     ((#\ff) '(#\\ #\f))
                     ((#\lf) '(#\\ #\n))
                     ((#\cr) '(#\\ #\r))
                     ((#\ht) '(#\\ #\t))
                     ((#\/) (if escape `(#\\ ,c) (list c)))
                     (else (string->list (build-char-string c)))))
                 (string->list (->string scm)))))
   port)
  (display "\"" port))

(define (json-build-array scm port escape pretty level)
  (display "[" port)
  (unless (null? scm)
    (json-build (car scm) port escape pretty (+ level 1))
    (for-each (lambda (v)
                (display ", " port)
                (json-build v port escape pretty (+ level 1)))
              (cdr scm)))
  (display "]" port))

(define (json-build-object scm port escape pretty level)
  (build-newline port pretty)
  (simple-format port "~A{" (indent-string pretty level))
  (build-newline port pretty)
  (let ((pairs scm))
    (unless (null? pairs)
      (build-object-pair (car pairs) port escape pretty (+ level 1))
      (for-each (lambda (p)
                  (display "," port)
                  (build-newline port pretty)
                  (build-object-pair p port escape pretty (+ level 1)))
                (cdr pairs))))
  (build-newline port pretty)
  (simple-format port "~A}" (indent-string pretty level)))

(define (json-build scm port escape pretty level)
  (cond
   ((eq? scm #nil) (json-build-null port))
   ((boolean? scm) (json-build-boolean scm port))
   ((number? scm) (json-build-number scm port))
   ((symbol? scm) (json-build-string (symbol->string scm) port escape))
   ((string? scm) (json-build-string scm port escape))
   ((json-alist? scm) (json-build-object scm port escape pretty level))
   ((list? scm) (json-build-array scm port escape pretty level))
   ((hash-table? scm)
    (json-build-object (hash-map->list cons scm) port escape pretty level))
   (else (throw 'json-invalid))))

;;
;; Public procedures
;;

(define* (scm->json scm
                    #:optional (port (current-output-port))
                    #:key (escape #f) (pretty #f))
  "Creates a JSON document from native. The argument @var{scm} contains
the native value of the JSON document. Takes one optional argument,
@var{port}, which defaults to the current output port where the JSON
document will be written."
  (json-build scm port escape pretty 0))

(define* (scm->json-string scm #:key (escape #f) (pretty #f))
  "Creates a JSON document from native into a string. The argument
@var{scm} contains the native value of the JSON document."
  (call-with-output-string
   (lambda (p)
     (scm->json scm p #:escape escape #:pretty pretty))))

;;; (json builder) ends here
