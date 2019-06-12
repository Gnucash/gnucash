;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  reports.scm
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


(define-module (gnucash report reports))
(use-modules (srfi srfi-13))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(use-modules (gnucash report report))
(use-modules (gnucash utilities))

(export gnc:register-report-create)
(export gnc:invoice-report-create)
(export gnc:payables-report-create)
(export gnc:receivables-report-create)
(export gnc:owner-report-create)

;(re-export payables-report-create-internal
;receivables-report-create-internal
;owner-report-create)

(define report-dirs (list
    "standard" ; base directory for standard reports included in gnucash
    "example"  ; base directory for example reports included in gnucash
))

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
(define (get-report-list subdir)
  (let* ((rpt-dir (gnc-build-reports-path subdir))
         (rpt-list (directory-files rpt-dir)))
        (gnc:debug "rpt-subdir=" subdir)
        (gnc:debug "rpt-dir=" rpt-dir)
        (gnc:debug "dir-files=" rpt-list)
        rpt-list))

(for-each
  (lambda (rpt-dir-str)
    (for-each
     (lambda (rpt-file-str)
       (let ((rpt-file (string->symbol rpt-file-str))
             (rpt-dir (string->symbol rpt-dir-str)))
       (module-use!
        (current-module)
        (resolve-interface `(gnucash report reports ,rpt-dir ,rpt-file)))))
     (get-report-list rpt-dir-str)))
  report-dirs)

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

(define (gnc:register-report-create account split query journal? ledger-type?
                                    double? title debit-string credit-string)
  (let* ((acct-type (xaccAccountGetType account))
         (create-fcn (gnc:lookup-register-report acct-type split)))
    (gnc:debug "create-fcn: " create-fcn)
    (if create-fcn
        (create-fcn account split query journal? double? title
                    debit-string credit-string)
        (gnc:register-report-create-internal #f query journal? ledger-type? double?
                                             title debit-string credit-string))))

;; Creates a new report instance for the given invoice. The given
;; report-template-id must refer to an existing report template, which
;; is then used to instantiate the new report instance.
(define (gnc:invoice-report-create invoice report-template-id)
    (if (gnc:find-report-template report-template-id)
        ;; We found the report template id, so instantiate a report
        ;; and set the invoice option accordingly.
        (let* ((options (gnc:make-report-options report-template-id))
               (invoice-op (gnc:lookup-option options gnc:pagename-general gnc:optname-invoice-number)))

          (gnc:option-set-value invoice-op invoice)
          (gnc:make-report report-template-id options))
        ;; Invalid report-template-id, so let's return zero as an invalid report id.
        0
        ))

(use-modules (gnucash report reports standard payables))
(define (gnc:payables-report-create account title show-zeros?)
  (payables-report-create-internal account title show-zeros?))

(use-modules (gnucash report reports standard receivables))
(define (gnc:receivables-report-create account title show-zeros?)
  (receivables-report-create-internal account title show-zeros?))

(use-modules (gnucash report reports standard owner-report))
(define (gnc:owner-report-create owner account)
  ; Figure out an account to use if nothing exists here.
  (if (null? account)
      (set! account (find-first-account-for-owner owner)))
  (owner-report-create owner account))
