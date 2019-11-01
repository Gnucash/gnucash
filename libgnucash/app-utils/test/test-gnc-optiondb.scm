 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; test-gnc-option.scm -- unit tests for GncOption class.           ;
 ; Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               ;
 ;                                                                  ;
 ; This program is free software; you can redistribute it and/or    ;
 ; modify it under the terms of the GNU General Public License as   ;
 ; published by the Free Software Foundation; either version 2 of   ;
 ; the License, or (at your option) any later version.              ;
 ;                                                                  ;
 ; This program is distributed in the hope that it will be useful,  ;
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of   ;
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    ;
 ; GNU General Public License for more details.                     ;
 ;                                                                  ;
 ; You should have received a copy of the GNU General Public License;
 ; along with this program; if not, contact:                        ;
 ;                                                                  ;
 ; Free Software Foundation           Voice:  +1-617-542-5942       ;
 ; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       ;
 ; Boston, MA  02110-1301,  USA       gnu@gnu.org                   ;
 ;                                                                  ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash gnc-module))
(eval-when
 (compile load eval expand)
 (load-extension "libswig-gnc-optiondb" "scm_init_sw_gnc_optiondb_module"))

(gnc:module-load "gnucash/engine" 0)
(use-modules (sw_gnc_optiondb))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-gnc-optiondb-scheme")
  (test-gnc-make-text-option)
  (test-gnc-make-multichoice-option)
  (test-gnc-make-date-option)
  (test-gnc-make-number-range-option)
  (test-end "test-gnc-optiondb-scheme"))

(define (test-gnc-make-text-option)
  (test-begin "test-gnc-test-string-option")
  (let* ((option-db (gnc-option-db-new))
         (string-opt (gnc-register-string-option option-db "foo" "bar" "baz"
                                                 "Phony Option" "waldo")))
    (test-equal "waldo" (GncOptionDB-lookup-option
                         (GncOptionDBPtr-get option-db) "foo" "bar"))

    (GncOptionDB-set-option-string (GncOptionDBPtr-get option-db) "foo" "bar" "pepper")
    (test-equal "pepper" (GncOptionDB-lookup-option
                          (GncOptionDBPtr-get option-db) "foo" "bar")))
  (test-end "test-gnc-make-string-option"))

(define (test-gnc-make-multichoice-option)

  (define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))

  (define (keylist-get-info keylist key info)
  (assq-ref (assq-ref keylist key) info))

  (test-begin "test-gnc-test-multichoice-option")
  (let* ((option-db (gnc-option-db-new))
         (multilist (list
                       (list "plugh" (cons 'text "xyzzy") (cons 'tip "thud"))
                       (list "waldo" (cons 'text "pepper") (cons 'tip "salt"))
                       (list "pork" (cons 'text "sausage") (cons 'tip "links"))
                       (list "corge" (cons 'text "grault") (cons 'tip "garply"))))
         (multichoice (keylist->vectorlist multilist))
         (multi-opt (gnc-register-multichoice-option option-db "foo" "bar" "baz"
                                                     "Phony Option" multichoice)))

    (GncOptionDB-set-option-string
     (GncOptionDBPtr-get option-db) "foo" "bar" "corge")
    (test-equal "corge" (GncOptionDB-lookup-option
                         (GncOptionDBPtr-get option-db) "foo" "bar")))
    (test-end "test-gnc-test-multichoice-option"))

(define (test-gnc-make-date-option)
  (test-begin "test-gnc-test-date-option")
  (let* ((option-db (gnc-option-db-new))
         (date-opt (gnc-register-date-interval-option option-db "foo" "bar"
                                                      "baz" "Phony Option"))
         (a-time (gnc-dmy2time64 11 07 2019)))
    (test-equal (current-time) (GncOptionDB-lookup-option
                                (GncOptionDBPtr-get option-db) "foo" "bar"))
    (GncOptionDB-set-option-time64 (GncOptionDBPtr-get option-db) "foo" "bar" a-time)
    (test-equal a-time (GncOptionDB-lookup-option
                        (GncOptionDBPtr-get option-db) "foo" "bar"))
    (test-end "test-gnc-test-date-option")))

(define (test-gnc-make-number-range-option)
  (test-begin "test-gnc-number-range-option")
  (let* ((option-db (gnc-option-db-new))
         (number-opt (gnc-register-number-range-option option-db "foo" "bar"
                                                       "baz" "Phony Option"
                                                       15 5 30 1)))
    (test-equal 15 (GncOptionDB-lookup-option
                                (GncOptionDBPtr-get option-db) "foo" "bar"))
    (GncOptionDB-set-option-int (GncOptionDBPtr-get option-db) "foo" "bar" 20)
    (test-equal 20 (GncOptionDB-lookup-option
                        (GncOptionDBPtr-get option-db) "foo" "bar")))
  (test-end "test-gnc-number-range-option"))
