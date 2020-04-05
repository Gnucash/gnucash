<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<?scm
(let ((x 42)) ; only here to allow (define)s
              ; i.e. to avoid "Bad define placement" error

;; receipt.eguile.scm
;; GnuCash report template
;;
;; This file is a mixture of HTML and Guile --
;; see eguile-gnc.scm for details.
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

  (define (display-report opt-invoice)

    (define (date<? s1 s2)
      (< (xaccTransGetDate (xaccSplitGetParent s1))
         (xaccTransGetDate (xaccSplitGetParent s2))))

    ;; Main function that creates the receipt invoice report
    (let* (; invoice and company details
           (invoiceid  (gncInvoiceGetID         opt-invoice))
           (book       (gncInvoiceGetBook       opt-invoice))
           (postdate   (gncInvoiceGetDatePosted opt-invoice))
           (duedate    (gncInvoiceGetDateDue    opt-invoice))
           (billingid  (gncInvoiceGetBillingID  opt-invoice))
           (notes      (gncInvoiceGetNotes      opt-invoice))
           (terms      (gncInvoiceGetTerms      opt-invoice))
           (termsdesc  (gncBillTermGetDescription terms))
           (lot        (gncInvoiceGetPostedLot  opt-invoice))
           (txn        (gncInvoiceGetPostedTxn  opt-invoice))
           (currency   (gncInvoiceGetCurrency   opt-invoice))
           (entries    (gncInvoiceGetEntries    opt-invoice))
           (splits     (sort (gnc-lot-get-split-list lot) date<?))
           (coyname    (gnc:company-info book gnc:*company-name*))
           (coycontact (gnc:company-info book gnc:*company-contact*))
           (coyaddr    (gnc:company-info book gnc:*company-addy*))
           (coyid      (gnc:company-info book gnc:*company-id*))
           (coyphone   (gnc:company-info book gnc:*company-phone*))
           (coyfax     (gnc:company-info book gnc:*company-fax*))
           (coyurl     (gnc:company-info book gnc:*company-url*))
           (coyemail   (gnc:company-info book gnc:*company-email*))
           (owner      (gncInvoiceGetOwner opt-invoice))
           (owneraddr  (gnc:owner-get-name-and-address-dep owner))
           (billcontact (gncAddressGetName (gnc:owner-get-address owner)))
           ; flags and counters
           (discount?  #f) ; any discounts on this invoice?
           (tax?       #f) ; any taxable entries on this invoice?
           (payments?  #f) ; have any payments been made on this invoice?
           (units?     #f) ; does any row specify units?
           (qty?       #f) ; does any row have qty <> 1?
           (maxcols    5))  ; cols of product line

      ;; pre-scan invoice entries to look for discounts and taxes
      (for-each
       (lambda (entry)
         (unless (string-null? (gncEntryGetAction entry)) (set! units? #t))
         (unless (= 1   (gncEntryGetQuantity entry))      (set! qty? #t))
         (unless (zero? (gncEntryGetInvDiscount entry))   (set! discount? #t))
         (unless (null? (gncEntryGetInvTaxTable entry))   (set! tax? #t)))
       entries)

      ;; pre-scan invoice splits to see if any payments have been made
      (let lp ((splits splits))
        (cond
         ((null? splits) #f)
         ((equal? (xaccSplitGetParent (car splits)) txn) (lp (cdr splits)))
         (else (set! payments? #t))))

?>

<!-- ====================================================================== -->
<!-- The HTML for the invoice starts here -->
<html dir='auto'>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title><?scm:d (_ "Invoice") ?> <?scm:d invoiceid ?></title>

<link rel="stylesheet" href="<?scm:d opt-css-file ?>" type="text/css">
<!-- Note that the external stylesheet file is overridden by this following: -->
<style type="text/css">
  body {
    <?scm:d opt-text-font ?>
  }
  table { /* table does not inherit font */
    <?scm:d opt-text-font ?>
  }
  div#content h1  {
    <?scm:d opt-heading-font ?>
  }
</style>

</head>
<body>

<div id="content">
  <!-- footer logo -->
  <p>
    <?scm (if (access? opt-logofile-header R_OK) (begin ?>
      <img align="left" src="<?scm:d opt-logofile-header ?>" alt="logo" class="logo"
        <?scm (if opt-logo-width-header (begin ?>
          style="width: <?scm:d opt-logo-width-header ?>"
        <?scm )) ?>
      >
    <?scm )) ?>
  </p>

  <!-- header texts -->

  <h1><?scm:d (nbsp (_ "Invoice No.")) ?> <?scm:d invoiceid ?></h1>
  <h2><?scm:d (gnc-print-time64 (gnc:get-today) opt-date-format) ?></h2>
  <p>&nbsp;</p>
  <?scm (if (not (string=? billcontact "")) (begin ?>
    <p>Attn: <?scm:d billcontact ?></p><br>
  <?scm )) ?>
  <?scm (if (not (string=? owneraddr "")) (begin ?>
    <p><?scm:d (nl->br owneraddr) ?></p>
  <?scm )) ?>

  <!-- products -->

  <div id="products">

    <!-- invoice lines table -->
    <p>
    <table border="0" width="100%" class="entries">
      <thead>
        <tr>
          <th align="left" ><?scm:d (_ "Date") ?></th>
          <th align="left" ><?scm:d (_ "Descr.") ?></th>
          <th align="right"><?scm:d opt-qty-heading ?></th>
          <th align="right"><?scm:d opt-unit-price-heading ?></th>
          <th align="right"><?scm:d opt-net-price-heading ?></th>
        </tr>
      </thead>

      <tbody> <!-- display invoice entry lines, keeping running totals -->
        <?scm
          (let* ((inv-total (gncInvoiceGetTotal opt-invoice))
                 (tax-total (gncInvoiceGetTotalTax opt-invoice))
                 (sub-total (gncInvoiceGetTotalSubtotal opt-invoice))
                 (dsc-total (- inv-total tax-total sub-total))
                 (total-col (gnc:make-commodity-collector)))
            (total-col 'add currency inv-total)
            (for-each
             (lambda (entry)
                (let ((qty       (gncEntryGetQuantity entry))
                      (each      (gncEntryGetInvPrice entry))
                      (action    (gncEntryGetAction entry))
                      (rval      (gncEntryGetDocValue entry #t #t #f))
                      (rdiscval  (gncEntryGetDocDiscountValue entry #t #t #f))
                      (rtaxval   (gncEntryGetDocTaxValue entry #t #t #f))
                      (disc      (gncEntryGetInvDiscount entry))
                      (disctype  (gncEntryGetInvDiscountType entry))
                      (acc       (gncEntryGetInvAccount entry))
                      (taxable   (gncEntryGetInvTaxable entry))
                      (taxtable  (gncEntryGetInvTaxTable entry)))
        ?>
        <tr valign="top">
          <td align="left"><?scm:d (qof-print-date (gncEntryGetDate entry)) ?></td>
          <td align="left" ><?scm:d (gncEntryGetDescription entry) ?></td>
          <td align="right"><?scm:d (fmtnumeric qty) ?></td>
          <td align="right"><?scm:d (format #f "~4,2,,,'0f" (gnc-numeric-to-double each)) ?></td>
          <td align="right" nowrap><?scm:d (format #f "~4,2,,,'0f" (gnc-numeric-to-double rval)) ?>
          <!-- <td align="right" nowrap><?scm:d (fmtnumeric rval) ?> -->
              <?scm (if tax? (begin ?>
                &nbsp;T
              <?scm ) (begin ?>
                &nbsp;&nbsp;
              <?scm )) ?>
          </td>
        </tr>

        <?scm (if (string-null? (gncEntryGetNotes entry)) (begin ?>
          <tr>
            <td align="left">&nbsp;</td>
            <td align="left" colspan="<?scm:d (- maxcols 1) ?>"><?scm:d (gncEntryGetNotes entry) ?></td>
          </tr>
        <?scm )) ?>

        <?scm ))
             entries) ?>

        <!-- display subtotals row -->
        <tr valign="top">
          <td align="center" class="total total_first" colspan="<?scm:d maxcols ?>">
              <?scm:d "Total No. Items:" ?>&nbsp;
              <?scm:d (length entries) ?>
          </td>
        </tr>

        <?scm (if tax? (begin ?>
          <tr valign="top">
            <td align="left"  class="subtotal" colspan="<?scm:d (- maxcols 2) ?>"><strong><?scm:d opt-net-price-heading ?></strong></td>
            <td align="right" class="subtotal" colspan="2>"><strong><?scm:d (fmtmoney currency sub-total) ?></strong></td>
          </tr>
          <tr valign="top">
            <td align="left"  class="subtotal" colspan="<?scm:d (- maxcols 2) ?>"><strong><?scm:d opt-tax-amount-heading ?></strong></td>
            <td align="right" class="subtotal" colspan="2>"><strong><?scm:d (fmtmoney currency tax-total) ?></strong></td>
          </tr>
        <?scm )) ?>

        <!-- payments -->

        <?scm (if payments? (begin ?>
          <tr valign="top">
            <td align="left"  class="subtotal" colspan="<?scm:d (- maxcols 2) ?>"><strong><?scm:d opt-total-price-heading ?></strong></td>
            <td align="right" class="subtotal" colspan="2>"><strong><?scm:d (fmtmoney currency inv-total) ?></strong></td>
          </tr>
        <?scm )) ?>

        <?scm
          (when payments?
            (for-each
             (lambda (split)
                (let ((t (xaccSplitGetParent split)))
                  (unless (equal? t txn) ; don't process the entry itself as a split
                    (let ((c (xaccTransGetCurrency t))
                          (a (xaccSplitGetValue    split)))
                      (total-col 'add c a)
        ?>
        <tr valign="top">
          <td align="center"><?scm:d (qof-print-date (xaccTransGetDate t)) ?></td>
          <td align="left" colspan="<?scm:d (- maxcols 3) ?>"><?scm:d opt-payment-recd-heading ?></td>
          <td align="right" colspan="2"><?scm:d (fmtmoney c a) ?></td>
        </tr>
        <?scm ))))
             splits)) ?>

        <!-- total row -->
        <tr valign="top">
          <td align="left"  class="total total_last" colspan="<?scm:d (- maxcols 2) ?>"><strong><?scm:d opt-amount-due-heading ?></strong></td>
          <td align="right" class="total total_last" colspan="2>"><strong><?scm (display-comm-coll-total total-col #f) ?></strong></td>
        </tr>

      </tbody>
      <?scm ) ?> <!-- end of (let) surrounding table body -->
    </table>

  </div>

  <p><?scm:d (nl->br notes) ?>
  <p><?scm:d (nl->br opt-extra-notes) ?>

  <!-- footer logo -->
  <p>
    <?scm (if (access? opt-logofile-footer R_OK) (begin ?>
      <img align="left" src="<?scm:d opt-logofile-footer ?>" alt="logo" class="logo"
        <?scm (if opt-logo-width-footer (begin ?>
          style="width: <?scm:d opt-logo-width-footer ?>"
        <?scm )) ?>
      >
    <?scm )) ?>
  </p>

</div>


<?scm )) ; end of display-report function

  ; 'mainline' code: check for a valid invoice, then display the report
  (cond
   ((null? opt-invoice)
    (display (string-append "<h2>" (_ "Receipt") "</h2>"))
    (display (string-append "<p>" (_ "No invoice has been selected -- please use the Options menu to select one.") "</p>")))

   ((not (eqv? GNC-OWNER-CUSTOMER
               (gncOwnerGetType
                (gncOwnerGetEndOwner
                 (gncInvoiceGetOwner opt-invoice)))))
    (display (string-append "<h2>" (_ "Receipt") "</h2>"))
    (display (string-append "<p>" (_ "This report is designed for customer (sales) invoices only. Please use the Options menu to select an <em>Invoice</em>, not a Bill or Expense Voucher.") "</p>")))

   (else
    (display-report opt-invoice)))

?>
</div>
</body>
</html>
<?scm
) ; end of enclosing let
?>
