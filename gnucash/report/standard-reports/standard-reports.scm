;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  standard-reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash report standard-reports))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-13))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))

(export gnc:register-report-create)
(export gnc:register-report-hook)

(define gnc:*register-report-hash* (make-hash-table 23))

;; Keep a hash-table of records, keyed off the account type.  Each
;; record contains a function pointer for that account-type with split
;; or without split.  If no function is found, then run the 'default'
;; function

(define-record-type :acct-type-info
  (make-acct-type-private split non-split)
  acct-type?
  (split get-split set-split)
  (non-split get-non-split set-non-split))

(define (make-acct-type)
  (make-acct-type-private #f #f))

(define (gnc:register-report-hook acct-type split? create-fcn)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type (make-acct-type))))
    (if split?
        (set-split type-info create-fcn)
        (set-non-split type-info create-fcn))
    (hash-set! gnc:*register-report-hash* acct-type type-info)))

(define (lookup-register-report acct-type split)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))
    (gnc:debug "acct-type: " acct-type)
    (gnc:debug "ref: " type-info)
    (gnc:debug "hash: " gnc:*register-report-hash*)
    (gnc:debug "split: " split)
    (and type-info
         (if (and split (not (null? split)))
             (begin (gnc:debug "get-split...") (get-split type-info))
             (begin (gnc:debug "get-non-split...") (get-non-split type-info))))))


;; Returns a list of files in a directory
;;
;; Param:
;;   dir - directory name
;;
;; Return value:
;;   list of files in the directory

(define (directory-files dir)
  (cond
   ((file-exists? dir)
    (let ((dir-stream (opendir dir)))
      (let loop ((fname (readdir dir-stream))
                 (acc '()))
        (cond
         ((eof-object? fname)
          (closedir dir-stream)
          acc)
         (else
          (loop (readdir dir-stream)
                (if (string-suffix? ".scm" fname)
                    (cons (string-drop-right fname 4) acc)
                    acc)))))))
   (else
    (gnc:warn "Can't access " dir ".\nEmpty list will be returned.")
    '())))

;; Return a list of symbols representing reports in the standard reports directory
;;
;; Return value:
;;  List of symbols for reports
(define (get-report-list)
  (map string->symbol (directory-files (gnc-path-get-stdreportsdir))))

(gnc:debug "stdrpt-dir=" (gnc-path-get-stdreportsdir))
(gnc:debug "dir-files=" (directory-files (gnc-path-get-stdreportsdir)))
(gnc:debug "report-list=" (get-report-list))

(for-each
 (lambda (x)
   (module-use!
    (current-module)
    (resolve-interface `(gnucash report standard-reports ,x))))
 (get-report-list))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

(define (gnc:register-report-create account split query journal? ledger-type?
                                    double? title debit-string credit-string)
  (let* ((acct-type (xaccAccountGetType account))
         (create-fcn (lookup-register-report acct-type split)))
    (gnc:debug "create-fcn: " create-fcn)
    (if create-fcn
        (create-fcn account split query journal? double? title
                    debit-string credit-string)
        (gnc:register-report-create-internal #f query journal? ledger-type? double?
                                             title debit-string credit-string))))
