
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

(define-module (gnucash reports standard receipt))

(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report" 0)
(gnc:module-load "gnucash/html" 0)
(gnc:module-load "gnucash/engine" 0)

(use-modules (gnucash eguile))

(use-modules (srfi srfi-13)) ; for extra string functions
(use-modules (ice-9 format)) ; for number formatting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-specific routines

(define (taxrate taxable taxtable curr)
  ;; Display the tax rate applicable to an invoice line.
  ;; This may be e.g. "15%" or "£5.00" or "15% + £5.00" or "n/a"
  ;; depending on how complicated the tax table is.
  ;; (When called from within the eguile template, anything
  ;; (display)ed becomes part of the HTML string.)
  (if (or (not taxable) (eq? taxtable '()))
    (display "&nbsp;")
    (let* ((amttot  (gnc:make-commodity-collector))
           (pctot   (gnc:make-value-collector))
           (entries (gncTaxTableGetEntries taxtable))
           (amt?    #f)  ; becomes #t if any entries are amounts
           (pc?     #f)) ; becomes #t if any entries are percentages
      (for entry in entries do
          (let ((tttype (gncTaxTableEntryGetType   entry))
                (ttamt  (gncTaxTableEntryGetAmount entry)))
            (if (equal? tttype GNC-AMT-TYPE-VALUE)
              (begin
                (set! amt? #t)
                (amttot 'add curr ttamt))
              (begin
                (set! pc? #t)
                (pctot 'add ttamt)))))
      (if pc? (begin (display (fmtnumeric (pctot 'total #f))) (display "%")))
      (if (and amt? pc?) (display " +&nbsp;"))        ; both - this seems unlikely in practice
      (if amt?
        (display-comm-coll-total amttot #f))
      (if (and (not amt?) (not pc?)) (display (_ "n/a"))))))        ; neither

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define all the options

; option pages
(define headingpage  (N_ "Headings 1"))
(define headingpage2 (N_ "Headings 2"))
(define notespage    (N_ "Notes"))
;(define filespage    (N_ "Files"))
(define displaypage  (N_ "Display"))
(define generalpage  gnc:pagename-general)
; option names
(define optname-report-title   (N_ "Report title"))
(define optname-invoice-number (N_ "Invoice number"))
(define optname-template-file  (N_ "Template file"))
(define optname-css-file       (N_ "CSS stylesheet file"))
(define optname-heading-font   (N_ "Heading font"))
(define optname-text-font      (N_ "Text font"))
(define optname-logofile-header  (N_ "Header logo filename"))
(define optname-logo-width-header     (N_ "Header logo width"))
(define optname-logofile-footer  (N_ "Footer logo filename"))
(define optname-logo-width-footer     (N_ "Footer logo width"))
(define optname-units          (N_ "Units"))
(define optname-qty            (N_ "Qty"))
(define optname-unit-price     (N_ "Unit Price"))
(define optname-disc-rate      (N_ "Discount Rate"))
(define optname-disc-amount    (N_ "Discount Amount"))
(define optname-net-price      (N_ "Net Price"))
(define optname-tax-rate       (N_ "Tax Rate"))
(define optname-tax-amount     (N_ "Tax Amount"))
(define optname-total-price    (N_ "Total Price"))
(define optname-subtotal       (N_ "Sub-total"))
(define optname-amount-due     (N_ "Amount Due"))
(define optname-payment-recd   (N_ "Payment received text"))
(define optname-extra-notes    (N_ "Extra notes"))
(define optname-date-format    (N_ "Today date format"))

; Choose only customer invoices
; (This doesn't work very nicely -- all invoices and bills
;  are offered for selection, but if a non-customer invoice
;  is selected, the user is dumped back to viewing the
;  previous invoice (or none) with no error message)
(define (customers-only invoice)
  (let* ((owner     (gncInvoiceGetOwner  invoice))
         (endowner  (gncOwnerGetEndOwner owner))
         (ownertype (gncOwnerGetType     endowner)))
    ;(gnc:debug "ownertype is ")(gnc:debug ownertype)
    (if (eqv? ownertype GNC-OWNER-CUSTOMER)
      (list #t invoice)
      (list #f invoice))))

(define (options-generator)
  ;; Options
  (define report-options (gnc:new-options))
  (define (add-option new-option)
    (gnc:register-option report-options new-option))

  (add-option
    (gnc:make-invoice-option ; defined in gnucash/scm/business-options.scm
      generalpage optname-invoice-number
      "a" "" (lambda () '())
      #f))        ;customers-only)) ;-- see above

  ;; Display options
  (add-option (gnc:make-string-option displaypage optname-template-file "a"
    (N_ "The file name of the eguile template part of this report.  This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "receipt.eguile.scm"))
  (add-option (gnc:make-string-option displaypage optname-css-file "b"
    (N_ "The file name of the CSS stylesheet to use with this report.  This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "receipt.css"))
  (add-option (gnc:make-font-option
                displaypage optname-heading-font "c"
                (N_ "Font to use for the main heading") "Sans Bold 14"))
  (add-option (gnc:make-font-option
                displaypage optname-text-font "d"
                (N_ "Font to use for everything else") "Sans 10"))
  (add-option (gnc:make-pixmap-option
                displaypage optname-logofile-header "e"
                (N_ "Name of a file containing a logo to be used on the header of the report")
                "Receipt_header.jpg"))
  (add-option (gnc:make-string-option
                displaypage optname-logo-width-header "f" (N_ "Width of the header logo in CSS format, e.g. 10% or 32px.  Leave blank to display the logo at its natural width.  The height of the logo will be scaled accordingly.") "72mm"))
  (add-option (gnc:make-pixmap-option
                displaypage optname-logofile-footer "g"
                (N_ "Name of a file containing a logo to be used on the footer of the report")
                "Receipt_footer.jpg"))
  (add-option (gnc:make-string-option
                displaypage optname-logo-width-footer "h" (N_ "Width of the footer logo in CSS format, e.g. 10% or 32px.  Leave blank to display the logo at its natural width.  The height of the logo will be scaled accordingly.") "72mm"))

  (add-option (gnc:make-string-option
               displaypage  optname-date-format "i"
               (N_ "The format for the date->string conversion for today's date.")
               ;; Translators: Boost::date_time format string
               ;; "%l:%M %P, %e %B %Y" means " 9:56 pm, 19 June 2019"
               (_ "%l:%M %P, %e %B %Y")))

  ;; Heading options
  (add-option (gnc:make-string-option
                ; page / name / orderkey / tooltip / default
                headingpage optname-report-title "a" "" (_ "Invoice")))
  (add-option (gnc:make-string-option
                headingpage optname-units "b" "" (_ "Units")))
  (add-option (gnc:make-string-option
                headingpage optname-qty "c" "" (_ "Qty")))
  (add-option (gnc:make-string-option
                headingpage optname-unit-price "d" "" (_ "Unit Price")))
  (add-option (gnc:make-string-option
                headingpage optname-disc-rate "e" "" (_ "Discount Rate")))
  (add-option (gnc:make-string-option
                headingpage optname-disc-amount "f" "" (_ "Discount Amount")))
  (add-option (gnc:make-string-option
                headingpage optname-net-price "g" "" (_ "Net Price")))
  (add-option (gnc:make-string-option
                headingpage optname-tax-rate "h" "" (_ "Tax Rate")))
  (add-option (gnc:make-string-option
                headingpage optname-tax-amount "i" "" (_ "Tax Amount")))
  (add-option (gnc:make-string-option
                headingpage optname-total-price "j" "" (_ "Total Price")))
  (add-option (gnc:make-string-option
                headingpage2 optname-subtotal "a" "" (_ "Sub-total")))
  (add-option (gnc:make-string-option
                headingpage2 optname-amount-due "b" "" (_ "Amount Due")))
  (add-option (gnc:make-string-option
                headingpage2 optname-payment-recd "c" ""
                (_ "Payment received, thank you!")))

  (add-option (gnc:make-text-option
                notespage optname-extra-notes "a"
                (N_ "Notes added at end of invoice -- may contain HTML markup")
                ""))
                ;(N_ "(Development version -- don't rely on the numbers on this report without double-checking them.<br/>Change the 'Extra Notes' option to get rid of this message)")))

  (gnc:options-set-default-section
    report-options generalpage)

  report-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the report

(define (report-renderer report-obj)
  ;; Create and return the report as either an HTML string
  ;; or an <html-document>
  (define (opt-value section name)
    ; wrapper for option routines
    (define (get-opt section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))
    (gnc:option-value (get-opt section name)))

  ; Get all the options
  (let* ((document                  (gnc:make-html-document))
         (opt-invoice               (opt-value generalpage  optname-invoice-number))
         (opt-template-file         (find-template
                                      (opt-value displaypage optname-template-file)))
         (opt-css-file              (find-stylesheet
                                      (opt-value displaypage optname-css-file)))
         (opt-heading-font          (font-name-to-style-info
                                      (opt-value displaypage optname-heading-font)))
         (opt-text-font             (font-name-to-style-info
                                      (opt-value displaypage optname-text-font)))
         (opt-logofile-header       (opt-value displaypage  optname-logofile-header))
         (opt-logo-width-header     (opt-value displaypage  optname-logo-width-header))
         (opt-logofile-footer       (opt-value displaypage  optname-logofile-footer))
         (opt-logo-width-footer     (opt-value displaypage  optname-logo-width-footer))
         (opt-date-format           (opt-value displaypage  optname-date-format))
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
         (opt-extra-notes           (opt-value notespage    optname-extra-notes))
         (html (eguile-file-to-string
                 opt-template-file
                 (the-environment))))

    (gnc:debug "receipt.scm - generated html:") (gnc:debug html)

    html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the report

(gnc:define-report
  'version 1
  'name (N_ "Receipt")
  'report-guid "7eb3df21073d4c33920a0257da15fba5"
  'menu-name (N_ "Receipt")
  'menu-tip (N_ "Display a customer invoice as receipt, cash voucher")
  'menu-path (list gnc:menuname-business-reports)
  'options-generator options-generator
  'renderer report-renderer)


