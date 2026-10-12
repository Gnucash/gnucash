;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnc-qrencode.scm : link with gnc-qrencode.c
;; Copyright 2021 Christopher Lam
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report gnc-qrencode))

(eval-when (compile load eval expand)
  (load-extension "libgnc-report" "scm_init_sw_report_module"))

(use-modules (ice-9 match))
(use-modules (sxml simple))
(use-modules (gnucash core-utils))
(use-modules (gnucash utilities))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash options))
(use-modules (sw_report))

(export gnc:make-epc-qrcode)

(define (data->svg data width size source)
  (define (make-rect row col)
    (format #f "M~a,~ah~av~ah~aZ " (* row size) (* col size) size size (- size)))
  (let lp ((col 0) (row 0) (data data) (accum '()))
    (match data
      (()
       (call-with-output-string
         (lambda (port)
           (sxml->xml
            `(svg (@ (xmlns "http://www.w3.org/2000/svg")
                     (width ,(* width size))
                     (height ,(* row size)))
                  (title ,source) (desc "QR code")
                  (path (@ (d ,(string-concatenate accum)))))
            port))))
      ((datum . rest)
       (let ((newcol (modulo (1+ col) width)))
         (lp newcol (if (zero? newcol) (1+ row) row) rest
             (if datum (cons (make-rect row col) accum) accum)))))))

(define (string-max str maxlen)
  (cond
   ((<= (string-length str) maxlen) str)
   (else (gnc:warn "QR: truncated " str " at " maxlen " characters")
         (string-take str maxlen))))

(define (gnc:make-epc-qrcode invoice)
  (define (quit msg) (gnc:warn "QR Error:" msg) (list "qr-code-error" msg))
  (let* ((book (gncInvoiceGetBook invoice))
         (currency (gncInvoiceGetCurrency invoice))
         (amount (abs (gncInvoiceGetTotal invoice)))
         (ref (gncInvoiceGetID invoice))
         (bic (or (gnc:company-info book gnc:*company-bic*) ""))
         (name (or (gnc:company-info book gnc:*company-name*) ""))
         (iban (or (gnc:company-info book gnc:*company-iban*) ""))
         (print-info (gnc-commodity-print-info currency #f))
         (amount-str (string-append
                      (gnc-commodity-get-mnemonic currency)
                      (xaccPrintAmount amount print-info))))
    (cond
     ((not (gnc-qrcode-available)) (quit "Error: qrencode not available"))
     ((not (<= 4 (string-length iban) 34)) (quit "IBAN invalid"))
     ((not (<= (string-length bic) 11)) (quit "BIC cannot exceed 11 characters"))
     ((not (<= 1/100 amount 99999999999/100)) (quit "Amount out of bounds"))
     (else (let ((QRC (string-join
                       (list "BCD" "001" "1" "SCT" bic (string-max name 70)
                             iban amount-str "" (string-max ref 140) "") "\n")))
             (match (gnc-qrcode-encodestring QRC)
               ((version width . data)
                (list "qr-code" (data->svg data width 3 QRC)))
               (_ (quit "libqrencode error"))))))))
