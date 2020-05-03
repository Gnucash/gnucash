;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report lot-viewer))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))            ;for let-values
(use-modules (ice-9 match))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash report business-reports))
(use-modules (gnucash gettext))
(use-modules (sw_core_utils))           ;for gnc-prefs-is-extra-enabled

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Lot Viewer"))
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-account (N_ "Account"))
(define optname-desc-filter "Description Filter")

(define txn-type-alist
  (list (cons TXN-TYPE-NONE "None")
        (cons TXN-TYPE-INVOICE "Inv")
        (cons TXN-TYPE-PAYMENT "Pmt")
        (cons TXN-TYPE-LINK "Link")))

(define (options-generator)
  (let ((options (gnc:new-options)))

    (define (add-option new-option)
      (gnc:register-option options new-option))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (add-option
     (gnc:make-account-sel-option
      gnc:pagename-general optname-account "b"
      (N_ "The account to search for lots.")
      #f #f))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-desc-filter "b" "Description Filter" ""))

    options))

(define (lot-renderer report-obj)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define (get-all-lots splits)
    (let lp ((splits splits) (lots '()))
      (match splits
        (() (reverse lots))
        ((split . rest)
         (let ((lot (xaccSplitGetLot split)))
           (lp rest
               (cond
                ((null? lot) lots)
                ((member lot lots) lots) ;warning: O(N^2)!
                (else (cons lot lots)))))))))

  (let* ((to-date (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general optname-to-date))))
         (from-date (gnc:time64-start-day-time
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-from-date))))
         (account (get-option gnc:pagename-general optname-account))
         (desc-filter (get-option gnc:pagename-general optname-desc-filter))
         (desc-filter? (lambda (split)
                         (string-contains
                          (xaccTransGetDescription (xaccSplitGetParent split))
                          desc-filter)))
         (currency (xaccAccountGetCommodity account))
         (document (gnc:make-html-document))
         (splits
          (let ((query (qof-query-create-for-splits)))
            (qof-query-set-book query (gnc-get-current-book))
            (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
            (xaccQueryAddSingleAccountMatch query account QOF-QUERY-AND)
            (xaccQueryAddDateMatchTT query #t from-date #t to-date QOF-QUERY-AND)
            (filter desc-filter? (qof-query-run query))))
         (transactions
          (sort-and-delete-duplicates
           (map xaccSplitGetParent splits)
           (lambda (a b) (string<? (gncTransGetGUID a) (gncTransGetGUID b)))
           equal?))
         (lots (get-all-lots splits))
         (lots-splits (map gnc-lot-get-split-list lots)))

    (define (amount->monetary amount)
      (gnc:make-gnc-monetary currency amount))

    (define (elt->cell split)
      (gnc:html-markup-anchor
       (gnc:split-anchor-text split)
       (amount->monetary (xaccSplitGetAmount split))))

    (define (list->text lst)
      (let lp ((lst lst) (result '()))
        (match lst
          (() (if (null? result) (gnc:make-html-text ":")
                  (apply gnc:make-html-text result)))
          ((elt . rest) (lp rest (cons* (elt->cell elt) result))))))

    (define (lot->title lot)
      (let ((title (gnc-lot-get-title lot)))
        (if (string-null? title) "None" title)))

    (define (lot->guid lot)
      (string-take (gncLotReturnGUID lot) 8))

    (define (to-cell elt)
      (gnc:make-html-table-cell/markup "number-cell" elt))

    (define (lot->document lot)
      (let ((inv (gncInvoiceGetInvoiceFromLot lot)))
        (and (not (null? inv))
             (to-cell
              (gnc:make-html-text
               (gnc:html-markup-anchor
                (gnc:invoice-anchor-text inv)
                (gncInvoiceGetID inv))
               (gnc:html-markup-br)
               (string-take (gncInvoiceReturnGUID inv) 8))))))

    (define (lot->balance lot)
      (to-cell (amount->monetary (gnc-lot-get-balance lot))))

    (cond
     ((null? splits)
      (gnc:html-document-add-object!
       document (gnc:html-make-empty-data-warning
                 reportname (gnc:report-id report-obj))))

     (else
      (let ((table (gnc:make-html-table)))
        (gnc:html-table-set-multirow-col-headers!
         table `(("Date" "Desc" "Type" ,@(map lot->title lots) "Other Account")
                 (#f #f #f ,@(map lot->guid lots) #f)))

        (gnc:html-table-append-row!
         table `(#f "Document" #f ,@(map lot->document lots)))

        (for-each
         (lambda (txn)
           (gnc:html-table-append-row!
            table
            (append
             (list (qof-print-date (xaccTransGetDate txn))
                   (xaccTransGetDescription txn)
                   (assv-ref txn-type-alist (xaccTransGetTxnType txn)))
             (let lp ((lots lots)
                      (lots-splits lots-splits)
                      (splits (xaccTransGetSplitList txn))
                      (accum '()))
               (match lots
                 (() (map (compose to-cell list->text) (reverse (cons splits accum))))
                 ((this-lot . rest-lots)
                  (define (in-lot? s) (member s (car lots-splits)))
                  (let-values (((this next) (partition in-lot? splits)))
                    (lp rest-lots (cdr lots-splits) next (cons this accum)))))))))
         (sort transactions (lambda (a b) (< (xaccTransOrder a b) 0))))

        (gnc:html-table-append-row!
         table `(#f "Balance" #f ,@(map lot->balance lots)))

        (gnc:html-document-add-object! document table))))

    document))


;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "b64b8cbaa633472c93ab7d9a2424d157"
 'menu-path (list gnc:menuname-experimental)
 'options-generator options-generator
 'in-menu? (gnc-prefs-is-extra-enabled)
 'renderer lot-renderer)
