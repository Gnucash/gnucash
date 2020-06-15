<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<?scm
(let ((x 42)) ; only here to allow (define)s
              ; i.e. to avoid "Bad define placement" error

;; taxinvoice.eguile.scm  0.03
;; GnuCash report template called from taxinvoice.scm 0.02
;; (c) 2009 Chris Dennis chris@starsoftanalysis.co.uk
;;  ©  2012 Dmitry Smirnov <onlyjob@member.fsf.org>
;;
;; $Author: chris $ $Date: 2009/07/23 10:42:08 $ $Revision: 1.33 $
;; Modified by Dmitry Smirnov <onlyjob@member.fsf.org>  16 Feb 2012
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

    ;; Main function that creates the tax invoice report
    (let* (; invoice and company details
           (invoiceid    (gncInvoiceGetID         opt-invoice))
           (credit-note? (gncInvoiceGetIsCreditNote opt-invoice))
           (book         (gncInvoiceGetBook       opt-invoice))
           (isposted     (gncInvoiceIsPosted      opt-invoice))
           (postdate     (gncInvoiceGetDatePosted opt-invoice))
           (duedate      (gncInvoiceGetDateDue    opt-invoice))
           (billingid    (gncInvoiceGetBillingID  opt-invoice))
           (notes        (gncInvoiceGetNotes      opt-invoice))
           (terms        (gncInvoiceGetTerms      opt-invoice))
           (termsdesc    (gncBillTermGetDescription terms))
           (lot          (gncInvoiceGetPostedLot  opt-invoice))
           (txn          (gncInvoiceGetPostedTxn  opt-invoice))
           (currency     (gncInvoiceGetCurrency   opt-invoice))
           (entries      (gncInvoiceGetEntries    opt-invoice))
           (splits       (sort (gnc-lot-get-split-list lot) date<?))
           (dateformat   (gnc:options-fancy-date book))
           (coyname      (gnc:company-info book gnc:*company-name*))
           (coycontact   (gnc:company-info book gnc:*company-contact*))
           (coyaddr      (gnc:company-info book gnc:*company-addy*))
           (coyid        (gnc:company-info book gnc:*company-id*))
           (coyphone     (gnc:company-info book gnc:*company-phone*))
           (coyfax       (gnc:company-info book gnc:*company-fax*))
           (coyurl       (gnc:company-info book gnc:*company-url*))
           (coyemail     (gnc:company-info book gnc:*company-email*))
           (owner        (gncInvoiceGetOwner  opt-invoice))
           (owneraddr  (gnc:owner-get-address-dep owner))
           (ownername  (gnc:owner-get-name-dep owner))
           (jobnumber  (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner  opt-invoice))))
           (jobname    (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner  opt-invoice))))
           (billcontact  (gncAddressGetName (gnc:owner-get-address owner)))
           (cust-doc? (eqv? (gncInvoiceGetType opt-invoice) GNC-INVOICE-CUST-INVOICE))
           (reverse-payments? (not (gncInvoiceAmountPositive opt-invoice)))
           ; flags and counters
           (discount?  #f) ; any discounts on this invoice?
           (tax?       #f) ; any taxable entries on this invoice?
           (payments?  #f) ; have any payments been made on this invoice?
           (units?     #f) ; does any row specify units?
           (qty?       #f) ; does any row have qty <> 1?
           (tbl_cols   0)) ; number of columns for 'colspan' attributes
      
      ;; pre-scan invoice entries to look for discounts and taxes
      (for-each
       (lambda (entry)
         (unless (string-null? (gncEntryGetAction entry)) (set! units? #t))
         (unless (= 1 (gncEntryGetDocQuantity entry credit-note?)) (set! qty? #t))
         (cond
          (cust-doc?
           (unless (zero? (gncEntryGetInvDiscount entry)) (set! discount? #t))
           (unless (null? (gncEntryGetInvTaxTable entry)) (set! tax? #t)))
          (else
           (unless (null? (gncEntryGetBillTaxTable entry)) (set! tax? #t)))))
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
<title><?scm:d (G_ "Invoice") ?> <?scm:d invoiceid ?></title>

<link rel="stylesheet" href="<?scm:d (make-file-url opt-css-file) ?>" type="text/css">
<!-- Note that the external stylesheet file is overridden by this following: -->
<style type="text/css">
  body {
    <?scm:d opt-text-font ?>
  }
  table { /* table does not inherit font */
    <?scm:d opt-text-font ?>
    <?scm:d opt-css-border-collapse ?>
  }
  table[border="1"] th {
    border-color:<?scm:d opt-css-border-color-th ?>;
  }
  table[border="1"] td {
    border-color:<?scm:d opt-css-border-color-td ?>;
  }

  h1.coyname {
    <?scm:d opt-heading-font ?>
  }
  <?scm:d opt-extra-css ?>
</style>

</head>
<body>

<div class="main">

<!-- company info -->
<table class="coytable" border="0" width="100%">
<tr valign="top" style="vertical-align: top">
  <?scm (if (access? opt-logofile R_OK) (begin ?>
    <td align="left">
      <img align="left" src="<?scm:d (make-file-url opt-logofile) ?>" alt="logo" class="logo"
        <?scm (if opt-logo-width (begin ?>
          style="width: <?scm:d opt-logo-width ?>"
        <?scm )) ?>
        >
    </td>
  <?scm )) ?>
  <td align="left">
    <h1 class="coyname"><?scm:d (or coyname (G_ "Company Name")) ?></h1>
  </td>
  <td align="right"><h2 class="invoice"><?scm:d opt-report-title ?>
    <?scm (if opt-invnum-next-to-title (begin ?><?scm:d (nbsp invoiceid) ?><?scm )) ?>
  </h2></td>
</tr>
</table>
<table border="0" width="100%">
<tr valign="top">
  <td align="left">
    <?scm (if (and opt-row-address coyaddr) (begin ?>
      <?scm:d (nl->br coyaddr) ?><br>
    <?scm )) ?>
    <?scm (if coyid (begin ?>
      <strong><?scm:d coyid ?></strong><br>
    <?scm )) ?>
  </td>
  <td align="right">
    <table border="0">
      <?scm (if (and opt-row-contact coycontact) (begin ?>
        <tr>
          <th colspan="2" align="right"><?scm:d coycontact ?></th>
        </tr>
      <?scm )) ?>
      <?scm (if coyphone (begin ?>
        <tr>
          <td align="right"><?scm:d (G_ "Phone") ?>:&nbsp;</td>
          <td align="right"><?scm:d coyphone ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyfax (begin ?>
        <tr>
          <td align="right"><?scm:d (G_ "Fax") ?>:&nbsp;</td>
          <td align="right"><?scm:d coyfax ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyemail (begin ?>
        <tr>
          <td align="right"><?scm:d (G_ "Email") ?>:&nbsp;</td>
          <td align="right"><?scm:d coyemail ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyurl (begin ?>
        <tr>
          <td align="right"><?scm:d (G_ "Website") ?>:&nbsp;</td>
          <td align="right"><?scm:d coyurl ?></td>
        </tr>
      <?scm )) ?>
    </table>
</tr>
</table>
<hr>

<table border="0" width="100%">
<tr valign="top">
  <!-- customer info -->
  <th align="right" width="1%"><?scm:d opt-to-text ?></th>
  <td align="left">
    <?scm (if (and opt-row-company-name (not (string=? ownername ""))) (begin ?>
        <?scm:d ownername ?><br>
    <?scm )) ?>
    <?scm (if (not (string=? owneraddr "")) (begin ?>
      <?scm:d (nl->br owneraddr) ?>
    <?scm )) ?>
  </td>
  <!-- invoice number etc. -->
  <td align="right">
    <table border="0">
      <?scm (if opt-row-invoice-number (begin ?>
      <tr>
        <td align="right" class="invnum"><big><strong><?scm:d (nbsp opt-invoice-number-text) ?></strong></big></td>
        <td align="right" class="invnum"><big><strong><?scm:d invoiceid ?></strong></big></td>
      </tr>
      <?scm )) ?>
      <?scm (if (not isposted) (begin ?>
        <tr>
           <td colspan="2" align="right"><?scm:d (G_ "Invoice in progress...") ?></td>
        </tr>
      <?scm ) (begin ?>
        <tr>
           <td align="right"><?scm:d (nbsp (G_ "Invoice Date")) ?>:&nbsp;</td>
           <td align="right"><?scm:d (nbsp (gnc-print-time64 postdate dateformat)) ?></td>
        </tr>
        <tr>
           <td align="right"><?scm:d (nbsp (G_ "Due Date")) ?>:&nbsp;</td>
           <td align="right"><?scm:d (nbsp (gnc-print-time64 duedate dateformat)) ?></td>
        </tr> <?scm )) ?>
        <?scm (if (not (string=? billingid "")) (begin ?>
          <tr>
            <td align="right"><?scm:d opt-ref-text ?></td>
            <td align="right"><?scm:d billingid ?></td>
          </tr>
        <?scm )) ?>
        <?scm (if (and opt-jobname-show (not (string=? jobname ""))) (begin ?>
          <tr>
            <td align="right"><?scm:d opt-jobname-text ?></td>
            <td align="right"><?scm:d jobname ?></td>
          </tr>
        <?scm )) ?>
        <?scm (if (and opt-jobnumber-show (not (string=? jobnumber ""))) (begin ?>
          <tr>
            <td align="right"><?scm:d opt-jobnumber-text ?></td>
            <td align="right"><?scm:d jobnumber ?></td>
          </tr>
      <?scm )) ?>
      <?scm (if (not (string=? termsdesc "")) (begin ?>
        <tr><td colspan="2" align="right"><?scm:d termsdesc ?></td></tr>
      <?scm )) ?>
    </table>
  </td>
</tr>
</table>

<!-- invoice lines table -->
<p>
<table border="1" width="100%" cellpadding="4" class="entries">
  <thead>
    <tr bgcolor="#ccc" valign="bottom">
      <?scm (if opt-col-date (begin ?>
      <th align="center" ><?scm:d (G_ "Date") ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
      <th align="left" width="80%"><?scm:d (G_ "Description") ?></th>
      <?scm (if (and units? opt-col-units) (begin ?>
        <th align="left"><?scm:d opt-units-heading ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
      <?scm (if (or units? qty?) (begin ?>
        <th align="right"><?scm:d opt-qty-heading ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
      <?scm (if (or units? qty? discount?) (begin ?>
        <th align="right"><?scm:d opt-unit-price-heading ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
      <?scm (if discount? (begin ?>
        <th align="right"><?scm:d opt-disc-rate-heading ?></th>
        <th align="right"><?scm:d opt-disc-amount-heading ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 2)) )) ?>
      <?scm (if tax? (begin ?>
        <th align="right"><?scm:d opt-net-price-heading ?></th>
        <?scm (set! tbl_cols (+ tbl_cols 1)) ?>
        <?scm (if opt-col-taxrate (begin ?>
        <th align="right"><?scm:d opt-tax-rate-heading ?></th>
        <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
        <th align="right"><?scm:d opt-tax-amount-heading ?></th>
      <?scm (set! tbl_cols (+ tbl_cols 1)) )) ?>
      <th align="right"><?scm:d opt-total-price-heading ?></th>
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
            (let ((qty       (gncEntryGetDocQuantity entry credit-note?))
                  (each      (gncEntryGetPrice entry cust-doc? opt-netprice))
                  (action    (gncEntryGetAction entry))
                  (rval      (gncEntryGetDocValue entry #t cust-doc? credit-note?))
                  (rdiscval  (gncEntryGetDocDiscountValue entry #t cust-doc? credit-note?))
                  (rtaxval   (gncEntryGetDocTaxValue entry #t cust-doc? credit-note?))
                  (disc      (if cust-doc? (gncEntryGetInvDiscount entry)))
                  (disctype  (gncEntryGetInvDiscountType entry))
                  (acc       (if cust-doc? (gncEntryGetInvAccount entry)(gncEntryGetBillAccount entry)))
                  (taxable   (if cust-doc? (gncEntryGetInvTaxable entry)(gncEntryGetBillTaxable entry)))
                  (taxtable  (if cust-doc? (gncEntryGetInvTaxTable entry)(gncEntryGetBillTaxTable entry))))
    ?>
    <tr valign="top">
      <?scm (if opt-col-date (begin ?>
      <td align="center" ><?scm:d (nbsp (qof-print-date (gncEntryGetDate entry))) ?></td>
      <?scm )) ?>
      <td align="left"><?scm:d (gncEntryGetDescription entry) ?></td>
      <!-- td align="left">< ?scm:d (gncEntryGetNotes entry) ?></td -->
      <?scm (if opt-col-units (begin ?>
      <?scm (if units? (begin ?>
        <td align="left"><?scm:d action ?></td>
      <?scm )) ?>
      <?scm )) ?>
      <?scm (if (or units? qty?) (begin ?>
        <td align="right"><?scm:d (fmtnumeric qty) ?></td>
      <?scm )) ?>
      <?scm (if (or units? qty? discount?) (begin ?>
        <td align="right"><?scm:d (fmtmoney currency each) ?></td>
      <?scm )) ?>
      <?scm (if discount? (begin ?>
        <?scm (if (equal? disctype GNC-AMT-TYPE-VALUE) (begin ?>
          <td align="right"><?scm:d (gnc:monetary->string (gnc:make-gnc-monetary currency disc)) ?></td>
        <?scm ) (begin ?>
          <td align="right"><?scm:d (fmtnumeric disc) ?>%</td>
        <?scm )) ?>
        <td align="right"><?scm:d (fmtmoney currency rdiscval) ?></td>
      <?scm )) ?>
      <?scm (if tax? (begin ?>
        <td align="right"><?scm:d (fmtmoney currency rval) ?></td>
        <?scm (if opt-col-taxrate (begin ?>
        <td align="right"><?scm (taxrate taxable taxtable currency) ?></td>
        <?scm )) ?>
        <td align="right"><?scm:d (fmtmoney currency rtaxval) ?></td>
      <?scm )) ?>
      <!-- TO DO: need an option about whether to display the tax-inclusive total? -->
      <td align="right"><?scm:d (fmtmoney currency (gnc-numeric-add rval rtaxval GNC-DENOM-AUTO GNC-RND-ROUND)) ?></td>
    </tr>
    <?scm ))
         entries) ?>

    <!-- subtotals row -->
    <?scm (if (or tax? discount? payments?) (begin ?>
      <tr valign="top">
        <td align="left" class="subtotal" colspan="<?scm:d
        (- tbl_cols (if (and tax? opt-col-taxrate) 1 0)
                    (if tax? 1 -1)
                    (if (and discount?) 1 0)
        ) ?>"><strong><?scm:d opt-subtotal-heading ?></strong></td>
        <?scm (if discount? (begin ?>
        <td align="right" class="subtotal"><strong><?scm:d (fmtmoney currency dsc-total) ?></strong></td>
        <?scm )) ?>
        <?scm (if tax? (begin ?>
          <td align="right" class="subtotal"><strong><?scm:d (fmtmoney currency sub-total) ?></strong></td>
          <?scm (if opt-col-taxrate (begin ?>
          <td>&nbsp;</td>
          <?scm )) ?>
          <td align="right" class="subtotal"><strong><?scm:d (fmtmoney currency tax-total) ?></strong></td>
        <?scm )) ?>
        <td align="right" class="subtotal"><strong><?scm:d (fmtmoney currency inv-total) ?></strong></td>
      </tr>
    <?scm )) ?>

    <!-- payments row -->
    <?scm
      (when payments?
        (for-each
         (lambda (split)
           (let ((t (xaccSplitGetParent split)))
             (unless (equal? t txn) ; don't process the entry itself as a split ;'
               (let ((c (xaccTransGetCurrency t))
                     (a (if reverse-payments?
                            (- (xaccSplitGetValue split))
                            (xaccSplitGetValue split))))
                 (total-col 'add c a)
    ?>
    <tr valign="top">
      <?scm (when opt-col-date ?>
      <td align="center"><?scm:d (qof-print-date (xaccTransGetDate t)) ?></td>
      <?scm ) ?>
      <td align="left" colspan="<?scm:d (+ tbl_cols (if opt-col-date 0 1)) ?>"><?scm:d opt-payment-recd-heading ?></td>
      <td align="right"><?scm:d (fmtmoney c a) ?></td>
    </tr>
    <?scm ))))
         splits)) ?>

    <!-- total row -->
    <tr valign="top">
      <td align="left" class="total" colspan="<?scm:d (+ tbl_cols 1) ?>"><strong>
        <?scm:d opt-amount-due-heading ?><?scm (if (not (string=? (gnc-commodity-get-mnemonic opt-invoice-currency) "")) (begin ?>,
        <?scm:d (gnc-commodity-get-mnemonic opt-invoice-currency) ?><?scm )) ?></strong></td>
      <td align="right" class="total"><strong><?scm (display-comm-coll-total total-col #f) ?></strong></td>
    </tr>

  </tbody>
  <?scm ) ?> <!-- end of (let) surrounding table body -->
</table>

<p><?scm:d (nl->br notes) ?>
<p><?scm:d (nl->br opt-extra-notes) ?>

<?scm )) ; end of display-report function

  ;; 'mainline' code: check for a valid invoice, then display the report


  (cond
   ((null? opt-invoice)
    (display (string-append "<h2>" (G_ "Tax Invoice") "</h2>"))
    (display (string-append "<p>" (G_ "No invoice has been selected -- please use the Options menu to select one.") "</p>")))

   (else
    (display-report opt-invoice)))

?>
</div>
</body>
</html>
<?scm
) ; end of enclosing let
?>
