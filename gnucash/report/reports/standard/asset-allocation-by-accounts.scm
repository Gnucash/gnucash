;; -*- scheme -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asset-allocation-by-accounts.scm
;; Account-grouping variant of the shared asset-allocation report engine.
;; Copyright (C) 2026 Brent McBride (mcbridebt@hotmail.com)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard asset-allocation-by-accounts))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

(use-modules (gnucash reports standard asset-allocation))

(define reportname (N_ "Asset Allocation (by Accounts)"))
(define report-guid "b965c7d638424d7da3be73eb6b32ae18")
(define pagename-categories (N_ "Categories"))
(define optname-definitions (N_ "Categories"))

;; Freeze current descendants into explicit membership when options are made.
;; Later accounts are not silently included when a saved report is reopened.
(define (default-category-definitions)
  (let ((accounts
         (filter (lambda (account)
                   (memv (xaccAccountGetType account)
                         (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH
                               ACCT-TYPE-CURRENCY ACCT-TYPE-STOCK
                               ACCT-TYPE-MUTUAL)))
                 (gnc-account-get-descendants-sorted
                  (gnc-get-current-root-account)))))
    (format #f "~s"
            (list 'explicit-targets
                  (append (list (G_ "Uncategorized") 100)
                          (sort (map gncAccountGetGUID accounts)
                                string<?))))))

(define (account-category-definitions options)
  (let ((parsed
         (allocation-read-category-data
          (gnc-optiondb-lookup-value
           options pagename-categories optname-definitions))))
    (and (list? parsed) (pair? parsed) (eq? (car parsed) 'explicit)
         (cdr parsed))))

;; Resolve GUIDs, preserving missing accounts for the shared validation error.
(define (resolve-allocation-categories definitions account-index)
  (map (lambda (category)
         (cons (car category)
               (map (lambda (guid) (hash-ref account-index guid))
                    (cdr category))))
       definitions))

(define (accounts-options-generator)
  (allocation-options-generator
   (lambda (options)
     (gnc-register-account-categories-option
      options pagename-categories optname-definitions "a"
      (N_ "Create named categories and select their accounts. Only selected \
accounts are included. Use Select Children to add descendants. Each account \
may belong to only one category.")
      (default-category-definitions)))
   #:targets? #t))

(define (accounts-renderer report-obj)
  (let* ((options (gnc:report-options report-obj))
         (index (make-hash-table)))
    (for-each
     (lambda (account)
       (hash-set! index (gncAccountGetGUID account) account))
     (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
    (let* ((definitions (account-category-definitions options))
           (valid? (allocation-valid-category-definitions?
                    definitions #:allow-empty-members? #t))
           (categories
            (if valid?
                (resolve-allocation-categories definitions index)
                '())))
      (let ((document
             (allocation-renderer
              report-obj categories
              (and (not valid?)
                   (G_ "Each category must have a unique, nonempty name."))
              #:targets? #t)))
        (when (and valid? (any pair? (map cdr categories))
                   (default-allocation-setup?
                    (gnc-optiondb-lookup-value
                     options pagename-categories optname-definitions)
                    (G_ "Uncategorized")))
          (prepend-allocation-setup! report-obj document #:accounts? #t))
        document))))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid report-guid
 'menu-path (list gnc:menuname-asset-liability)
 'menu-tip (N_ "Shows asset allocation by grouping selected account \
balances into categories, with optional targets and drift, at a specified \
date or over time.")
 'options-generator accounts-options-generator
 'renderer accounts-renderer)