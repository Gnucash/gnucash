;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-register-hooks.scm
;;  helper functions for loading specific reports based on context
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

(define-module (gnucash report report-register-hooks))

(use-modules (gnucash utilities))
(use-modules (srfi srfi-9))

(export gnc:register-report-hook)
(export gnc:lookup-register-report)

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

(define (gnc:lookup-register-report acct-type split)
  (let ((type-info (hash-ref gnc:*register-report-hash* acct-type)))
    (gnc:debug "acct-type: " acct-type)
    (gnc:debug "ref: " type-info)
    (gnc:debug "hash: " gnc:*register-report-hash*)
    (gnc:debug "split: " split)
    (and type-info
         (if (and split (not (null? split)))
             (begin (gnc:debug "get-split...") (get-split type-info))
             (begin (gnc:debug "get-non-split...") (get-non-split type-info))))))
