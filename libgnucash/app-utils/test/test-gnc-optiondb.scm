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

(eval-when
 (compile load eval expand)
 (load-extension "libswig-gnc-optiondb" "scm_init_sw_gnc_optiondb_module"))
(use-modules (sw_gnc_optiondb))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-gnc-optiondb-scheme")
  (test-gnc-make-text-option)
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
