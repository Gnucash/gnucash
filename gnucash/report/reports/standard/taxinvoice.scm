;; $Author: chris $ $Date: 2009/07/29 09:31:44 $ $Revision: 1.33 $
;; Modified by Dmitry Smirnov <onlyjob@member.fsf.org>  16 Feb 2012
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

; If you want to adapt this report privately:
; - copy the report to your .gnucash directory
; - specify a different module name below (eg mytaxinvoice)
; - refer to it from .gnucash/config.user
; (see https://wiki.gnucash.org/wiki/Custom_Reports )
(define-module (gnucash reports standard taxinvoice))

(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (ice-9 match))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(use-modules (gnucash eguile))

(use-modules (srfi srfi-13)) ; for extra string functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-specific routines

(define (taxrate taxable taxtable curr)
  ;; Display the tax rate applicable to an invoice line.
  ;; This may be e.g. "15%" or "£5.00" or "15% + £5.00" or "n/a"
  ;; depending on how complicated the tax table is.
  ;; (When called from within the eguile template, anything
  ;; (display)ed becomes part of the HTML string.)
  (define (amt-type? entry)
    (eqv? (gncTaxTableEntryGetType entry) GNC-AMT-TYPE-VALUE))
  (let lp ((entries (if taxable (gncTaxTableGetEntries taxtable) '())) (acc '()))
    (match entries
      (() (display (if (null? acc)
                       (G_ "n/a")
                       (string-join (reverse acc) "&nbsp;+&nbsp;"))))
      (((and (? amt-type?) (= gncTaxTableEntryGetAmount amt)) . rest)
       (lp rest (cons (gnc:default-html-gnc-monetary-renderer
                       (gnc:make-gnc-monetary curr amt) #f) acc)))
      (((= gncTaxTableEntryGetAmount percent) . rest)
       (lp rest
           (cons (string-append (fmtnumeric percent) "%")
                 acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define all the options

; option pages
(define headingpage  (N_ "Headings 1"))
(define headingpage2 (N_ "Headings 2"))
(define notespage    (N_ "Notes"))
(define displaypage  (N_ "Display"))
(define elementspage                    (N_ "Elements"))
; option names
(define optname-col-date                (N_ "column: Date"))
(define optname-col-taxrate             (N_ "column: Tax Rate"))
(define optname-col-units               (N_ "column: Units"))
(define optname-row-address             (N_ "row: Address"))
(define optname-row-contact             (N_ "row: Contact"))
(define optname-row-invoice-number      (N_ "row: Invoice Number"))
(define optname-row-company-name        (N_ "row: Company Name"))
(define optname-invoice-number-text     (N_ "Invoice number text"))
(define optname-to-text                 (N_ "To text"))
(define optname-ref-text                (N_ "Ref text"))
(define optname-jobname-text            (N_ "Job Name text"))
(define optname-jobnumber-text          (N_ "Job Number text"))
(define optname-jobname-show            (N_ "Show Job name"))
(define optname-jobnumber-show          (N_ "Show Job number"))
(define optname-netprice                (N_ "Show net price"))
(define optname-invnum-next-to-title    (N_ "Invoice number next to title"))
(define optname-border-collapse         (N_ "table-border-collapse"))
(define optname-border-color-th         (N_ "table-header-border-color"))
(define optname-border-color-td         (N_ "table-cell-border-color"))
(define optname-extra-css               (N_ "Embedded CSS"))
(define optname-report-title            (N_ "Report Title"))
(define optname-template-file           (N_ "Template file"))
(define optname-css-file                (N_ "CSS stylesheet file"))
(define optname-heading-font            (N_ "Heading font"))
(define optname-text-font               (N_ "Text font"))
(define optname-logofile                (N_ "Logo filename"))
(define optname-logo-width              (N_ "Logo width"))
(define optname-units                   (N_ "Units"))
(define optname-qty                     (N_ "Qty"))
(define optname-unit-price              (N_ "Unit Price"))
(define optname-disc-rate               (N_ "Discount Rate"))
(define optname-disc-amount             (N_ "Discount Amount"))
(define optname-net-price               (N_ "Net Price"))
(define optname-tax-rate                (N_ "Tax Rate"))
(define optname-tax-amount              (N_ "Tax Amount"))
(define optname-total-price             (N_ "Total Price"))
(define optname-subtotal                (N_ "Sub-total"))
(define optname-amount-due              (N_ "Amount Due"))
(define optname-payment-recd            (N_ "Payment received text"))
(define optname-extra-notes             (N_ "Extra Notes"))

(define (options-generator)
  ;; Options
  (let ((options (gnc-new-optiondb)))

  (gnc-register-invoice-option options
      gnc:pagename-general gnc:optname-invoice-number
      "a" "" '())

  ;; Elements page options
  (gnc-register-simple-boolean-option options
                 elementspage   optname-col-date                "a" (N_ "Display the date?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-col-taxrate             "b" (N_ "Display the Tax Rate?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-col-units               "c" (N_ "Display the Units?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-row-contact             "d" (N_ "Display the contact?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-row-address             "e" (N_ "Display the address?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-row-invoice-number      "f" (N_ "Display the Invoice Number?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-row-company-name        "g" (N_ "Display the Company Name?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-invnum-next-to-title    "h" (N_ "Invoice Number next to title?") #f)
(gnc-register-simple-boolean-option options
               elementspage     optname-jobname-show            "i" (N_ "Display Job name?") #t)
(gnc-register-simple-boolean-option options
               elementspage     optname-jobnumber-show          "j" (N_ "Invoice Job number?") #f)
(gnc-register-simple-boolean-option options
               elementspage     optname-netprice                "k" (N_ "Show net price?") #f)

  ;; Display options
(gnc-register-string-option options
               displaypage optname-template-file "a"
    (N_ "The file name of the eguile template part of this report. This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "taxinvoice.eguile.scm")
  (gnc-register-string-option options
                 displaypage optname-css-file "b"
    (N_ "The file name of the CSS stylesheet to use with this report. This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "taxinvoice.css")
  (gnc-register-font-option  options
                displaypage optname-heading-font "c"
                (N_ "Font to use for the main heading.") "Sans Bold 18")
  (gnc-register-font-option  options
                displaypage optname-text-font "d"
                (N_ "Font to use for everything else.") "Sans 10")
  (gnc-register-pixmap-option options
                displaypage optname-logofile "e"
                (N_ "Name of a file containing a logo to be used on the report.")
                "")
  (gnc-register-string-option options
                displaypage optname-logo-width "f" (N_ "Width of the logo in CSS format, e.g. 10% or 32px. Leave blank to display the logo at its natural width. The height of the logo will be scaled accordingly.") "")
  (gnc-register-simple-boolean-option options
                 displaypage    optname-border-collapse "g" (N_ "Border-collapse?") #f)
(gnc-register-string-option options
               displaypage      optname-border-color-th "h" (N_ "CSS color.") "black")
(gnc-register-string-option options
               displaypage      optname-border-color-td "i" (N_ "CSS color.") "black")

  ;; Heading options
  (gnc-register-string-option options
                ; page / name / orderkey / tooltip / default
                headingpage optname-report-title "a" "" (G_ "Invoice"))
  (gnc-register-string-option options
                headingpage optname-units "b" "" (G_ "Units"))
  (gnc-register-string-option options
                headingpage optname-qty "c" "" (G_ "Qty"))
  (gnc-register-string-option options
                headingpage optname-unit-price "d" "" (G_ "Unit Price"))
  (gnc-register-string-option options
                headingpage optname-disc-rate "e" "" (G_ "Discount Rate"))
  (gnc-register-string-option options
                headingpage optname-disc-amount "f" "" (G_ "Discount Amount"))
  (gnc-register-string-option options
                headingpage optname-net-price "g" "" (G_ "Net Price"))
  (gnc-register-string-option options
                headingpage optname-tax-rate "h" "" (G_ "Tax Rate"))
  (gnc-register-string-option options
                headingpage optname-tax-amount "i" "" (G_ "Tax Amount"))
  (gnc-register-string-option options
                headingpage optname-total-price "j" "" (G_ "Total Price"))
  (gnc-register-string-option options
                headingpage2 optname-subtotal "a" "" (G_ "Sub-total"))
  (gnc-register-string-option options
                headingpage2 optname-amount-due "b" "" (G_ "Amount Due"))
  (gnc-register-string-option options
                headingpage2 optname-payment-recd "c" ""
                (G_ "Payment received, thank you!"))
  (gnc-register-string-option options
                 headingpage2   optname-invoice-number-text
    "d" "" (G_ "Invoice number:"))
  (gnc-register-string-option options
                 headingpage2   optname-to-text
    "e" "" (G_ "To:"))
  (gnc-register-string-option options
                 headingpage2   optname-ref-text
    "f" "" (G_ "Your ref:"))
  (gnc-register-string-option options
                 headingpage2   optname-jobnumber-text
    "g" "" (G_ "Job number:"))
  (gnc-register-string-option options
                 headingpage2   optname-jobname-text
    "h" "" (G_ "Job name:"))

  (gnc-register-text-option options
                notespage optname-extra-notes "a"
                (G_ "Notes added at end of invoice -- may contain HTML markup.")
                (G_ "Thank you for your patronage!"))

  (gnc-register-text-option options
                 notespage optname-extra-css "b"
                (N_ "Embedded CSS.")    "h1.coyname { text-align: left; }")
  (gnc:options-set-default-section options gnc:pagename-general)

  options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the report

(define (report-renderer report-obj)
  ;; Create and return the report as either an HTML string
  ;; or an <html-document>
  (define (opt-value section name)
    (gnc-optiondb-lookup-value  (gnc:report-options report-obj) section name))

  ; Get all the options
  (let* ((document                  (gnc:make-html-document))
         (opt-invoice               (opt-value gnc:pagename-general gnc:optname-invoice-number))
         (opt-template-file         (find-template
                                      (opt-value displaypage optname-template-file)))
         (opt-css-file              (find-stylesheet
                                      (opt-value displaypage optname-css-file)))
         (opt-heading-font          (font-name-to-style-info-eguile
                                      (opt-value displaypage optname-heading-font)))
         (opt-text-font             (font-name-to-style-info-eguile
                                      (opt-value displaypage optname-text-font)))
         (opt-logofile              (opt-value displaypage  optname-logofile))
         (opt-logo-width            (opt-value displaypage  optname-logo-width))
         (opt-col-date              (opt-value elementspage  optname-col-date))
         (opt-col-taxrate           (opt-value elementspage  optname-col-taxrate))
         (opt-col-units             (opt-value elementspage  optname-col-units))
         (opt-row-contact           (opt-value elementspage  optname-row-contact))
         (opt-row-address           (opt-value elementspage  optname-row-address))
         (opt-row-invoice-number    (opt-value elementspage  optname-row-invoice-number))
         (opt-row-company-name      (opt-value elementspage  optname-row-company-name))
         (opt-invnum-next-to-title  (opt-value elementspage  optname-invnum-next-to-title))
         (opt-jobname-show          (opt-value elementspage  optname-jobname-show))
         (opt-jobnumber-show        (opt-value elementspage  optname-jobnumber-show))
         (opt-netprice              (opt-value elementspage  optname-netprice))
         (opt-invoice-currency      (gncInvoiceGetCurrency opt-invoice))
         (opt-css-border-collapse   (if (opt-value displaypage optname-border-collapse) "border-collapse:collapse;"))
         (opt-css-border-color-th   (opt-value displaypage optname-border-color-th))
         (opt-css-border-color-td   (opt-value displaypage optname-border-color-td))
         (opt-report-title          (opt-value headingpage  optname-report-title))
         (opt-units-heading         (opt-value headingpage  optname-units))
         (opt-qty-heading           (opt-value headingpage  optname-qty))
         (opt-unit-price-heading    (opt-value headingpage  optname-unit-price))
         (opt-disc-rate-heading     (opt-value headingpage  optname-disc-rate))
         (opt-disc-amount-heading   (opt-value headingpage  optname-disc-amount))
         (opt-net-price-heading     (opt-value headingpage  optname-net-price))
         (opt-tax-rate-heading      (opt-value headingpage  optname-tax-rate))
         (opt-tax-amount-heading    (opt-value headingpage  optname-tax-amount))
         (opt-total-price-heading   (opt-value headingpage  optname-total-price))
         (opt-subtotal-heading      (opt-value headingpage2 optname-subtotal))
         (opt-amount-due-heading    (opt-value headingpage2 optname-amount-due))
         (opt-payment-recd-heading  (opt-value headingpage2 optname-payment-recd))
         (opt-invoice-number-text   (opt-value headingpage2 optname-invoice-number-text))
         (opt-to-text               (opt-value headingpage2 optname-to-text))
         (opt-ref-text              (opt-value headingpage2 optname-ref-text))
         (opt-jobnumber-text        (opt-value headingpage2 optname-jobnumber-text))
         (opt-jobname-text          (opt-value headingpage2 optname-jobname-text))
         (opt-extra-css             (opt-value notespage    optname-extra-css))
         (opt-extra-notes           (opt-value notespage    optname-extra-notes))
         (html (eguile-file-to-string
                 opt-template-file
                 (the-environment))))

    (gnc:debug "taxinvoice.scm - generated html:") (gnc:debug html)

    html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the report

(gnc:define-report
  'version 1
  'name (N_ "Tax Invoice")
  'report-guid "0769e242be474010b4acf264a5512e6e"
  'menu-name (N_ "Tax Invoice")
  'menu-tip (N_ "Display a customer invoice with tax columns (using eguile template)")
  'menu-path (list gnc:menuname-business-reports)
  'hook 'invoice
  'options-generator options-generator
  'renderer report-renderer)

(define (au-tax-options-generator)
  (define (set-opt options page name value)
    (gnc-set-option options page name value))

  (let ((options (options-generator)))
       (set-opt options headingpage optname-report-title (G_ "Tax Invoice"))
       (set-opt options headingpage optname-unit-price (G_ "Unit"))
       (set-opt options headingpage optname-tax-rate (G_ "GST Rate"))
       (set-opt options headingpage optname-tax-amount (G_ "GST Amount"))
       (set-opt options headingpage2 optname-amount-due (G_ "Amount Due (inc GST)"))
       (set-opt options headingpage2 optname-invoice-number-text (G_ "Invoice #:"))
       (set-opt options headingpage2 optname-ref-text (G_ "Reference:"))
       (set-opt options headingpage2 optname-jobname-text (G_ "Engagement:"))
       (set-opt options notespage optname-extra-css "h1.coyname { text-align: right; margin-bottom: 0px ; font-size: 200%; } h2.invoice { text-align: left; margin-bottom: 0px ; font-size: 500%; }")
       options))

(gnc:define-report
  'version 1
  'name (N_ "Australian Tax Invoice")
  'report-guid "3dbbc2584da64e7a8674355bc3fbfe3d"
  'menu-name (N_ "Australian Tax Invoice")
  'menu-tip (N_ "Display an Australian customer invoice with tax columns (using eguile template)")
  'menu-path (list gnc:menuname-business-reports)
  'hook 'invoice
  'options-generator au-tax-options-generator
  'renderer report-renderer)
