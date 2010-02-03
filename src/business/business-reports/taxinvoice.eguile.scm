<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<?scm 
(let ((x 42)) ; only here to allow (define)s
              ; i.e. to avoid "Bad define placement" error

;; taxinvoice.eguile.scm  0.03
;; GnuCash report template called from taxinvoice.scm 0.02
;; (c) 2009 Chris Dennis chris@starsoftanalysis.co.uk
;;
;; $Author: chris $ $Date: 2009/07/23 10:42:08 $ $Revision: 1.33 $
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

  (define (display-report opt-invoice owner endowner ownertype)
    ;; Main function that creates the tax invoice report
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
           (splits     '()) 
           (slots      (gnc-book-get-slots book))
           (coyname    (coy-info slots gnc:*company-name*))
           (coycontact (coy-info slots gnc:*company-contact*))
           (coyaddr    (coy-info slots gnc:*company-addy*))
           (coyid      (coy-info slots gnc:*company-id*))
           (coyphone   (coy-info slots gnc:*company-phone*))
           (coyfax     (coy-info slots gnc:*company-fax*))
           (coyurl     (coy-info slots gnc:*company-url*))
           (coyemail   (coy-info slots gnc:*company-email*))
           (owneraddr  (gnc:owner-get-name-and-address-dep owner))
           (billcontact (gncAddressGetName (gnc:owner-get-address owner)))
           ; flags and counters
           (discount?  #f) ; any discounts on this invoice?
           (tax?       #f) ; any taxable entries on this invoice?
           (taxtables? #t) ; are tax tables available in this version?
           (payments?  #f) ; have any payments been made on this invoice?
           (units?     #f) ; does any row specify units?
           (qty?       #f) ; does any row have qty <> 1?
           (spancols1  2)  ; for total line
           (spancols2  2)) ; for subtotal line

      ; load splits, if any
      (if (not (null? lot))
        (set! splits
          (sort-list (gnc-lot-get-split-list lot) ; sort by date
                     (lambda (s1 s2)
                       (let ((t1 (xaccSplitGetParent s1))
                             (t2 (xaccSplitGetParent s2)))
                         (< (car (gnc-transaction-get-date-posted t1))
                            (car (gnc-transaction-get-date-posted t2))))))))

      ; pre-scan invoice entries to look for discounts and taxes
      (for-each 
        (lambda (entry) 
          (let ((action    (gncEntryGetAction entry)) 
                (qty       (gncEntryGetQuantity entry))
                (discount  (gncEntryGetInvDiscount entry))   
                (taxtable  (gncEntryGetInvTaxTable entry)))
            (if (not (string=? action "")) 
              (set! units? #t))
            (if (not (= (gnc-numeric-to-double qty) 1.0))
              (set! qty? #t))
            (if (not (gnc-numeric-zero-p discount)) (set! discount? #t))
            ;(if taxable - no, this flag is redundant
            (if (not (eq? taxtable '()))
              (begin ; presence of a tax table means it's taxed
                (set! tax? #t)
                (let ((ttentries (gncTaxTableGetEntries taxtable)))
                  (if (string-prefix? "#<swig-pointer PriceList" (object->string ttentries))
                    ; error in SWIG binding -- disable display of tax details
                    ; (see http://bugzilla.gnome.org/show_bug.cgi?id=573645)
                    (set! taxtables? #f))))))) ; hack required until Swig is fixed
        entries)

      ; pre-scan invoice splits to see if any payments have been made
      (for-each
        (lambda (split)  
          (let* ((t (xaccSplitGetParent split)))
            (if (not (equal? t txn))
              (set! payments? #t))))
        splits) 

?>

<!-- ====================================================================== -->
<!-- The HTML for the invoice starts here -->
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title><?scm:d (_ "Invoice") ?> <?scm:d invoiceid ?></title>

<?scm (if css? (begin ?>
<link rel="stylesheet" href="<?scm:d opt-css-file ?>" type="text/css">
<!-- Note that the external stylesheet file is overridden by this following: -->
<style type="text/css">
  body { 
    <?scm:d opt-text-font ?>
  }
  table { /* table does not inherit font */
    <?scm:d opt-text-font ?>
  }
  h1.coyname {
    <?scm:d opt-heading-font ?>
    /* font-size: 141%; */
    text-align: left;
  }
</style>
<?scm )) ?>

</head>
<body>

<div class="main">

<!-- company info -->
<table class="coytable" border="0" width="100%">
<tr valign="top" style="vertical-align: top">
  <?scm (if (access? opt-logofile R_OK) (begin ?>
    <td align="left">
      <img align="left" src="<?scm:d opt-logofile ?>" alt="logo" class="logo"
        <?scm (if opt-logo-width (begin ?>
          style="width: <?scm:d opt-logo-width ?>"
        <?scm )) ?>
        >
    </td>
  <?scm )) ?>
  <td align="left">
    <h1 class="coyname"><?scm:d (or coyname (_ "Company Name")) ?></h1>
  </td>
  <td align="right"><h2 class="invoice"><?scm:d opt-report-title ?></h2></td>
</tr>
</table>
<table border="0" width="100%">
<tr valign="top">
  <td align="left">
    <?scm (if coycontact (begin ?>
      <strong><?scm:d coycontact ?></strong><br>
    <?scm )) ?>
    <?scm (if coyaddr (begin ?>
      <?scm:d (nl->br coyaddr) ?><br>
    <?scm )) ?>
    <?scm (if coyid (begin ?>
      <strong><?scm:d coyid ?></strong><br>
    <?scm )) ?>
  </td>
  <td align="right">
    <table border="0">
      <?scm (if coyphone (begin ?>
        <tr>
          <td align="right"><?scm:d (_ "Phone:") ?>&nbsp;</td>
          <td align="right"><?scm:d coyphone ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyfax (begin ?>
        <tr>
          <td align="right"><?scm:d (_ "Fax:") ?>&nbsp;</td>
          <td align="right"><?scm:d coyfax ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyemail (begin ?>
        <tr>
          <td align="right"><?scm:d (_ "Email:") ?>&nbsp;</td>
          <td align="right"><?scm:d coyemail ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if coyurl (begin ?>
        <tr>
          <td align="right"><?scm:d (_ "Website:") ?>&nbsp;</td>
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
  <td align="left">
    <?scm (if (not (string=? billcontact "")) (begin ?>
      <strong>Attn: <?scm:d billcontact ?></strong><br>
    <?scm )) ?>
    <?scm (if (not (string=? owneraddr "")) (begin ?>
      <strong><?scm:d (nl->br owneraddr) ?></strong>
    <?scm )) ?>
  </td>
  <!-- invoice number etc. -->
  <td align="right">
    <table border="0">
      <tr>
        <td align="right" class="invnum"><big><strong><?scm:d (nbsp (_ "Invoice number:")) ?></strong></big>&nbsp;</td>
        <td align="right" class="invnum"><big><strong><?scm:d invoiceid ?></strong></big></td>
      </tr>
      <?scm (if (equal? postdate (cons 0 0)) (begin ?>
        <tr>
           <td colspan="2" align="right"><?scm:d (_ "Invoice in progress...") ?></td>
        </tr>
      <?scm ) (begin ?>
        <tr>
           <td align="right"><?scm:d (nbsp (_ "Invoice date:")) ?>&nbsp;</td>
           <td align="right"><?scm:d (gnc-print-date postdate) ?></td>
        </tr>
        <tr>
           <td align="right"><?scm:d (nbsp (_ "Due date:")) ?>&nbsp;</td>
           <td align="right"><?scm:d (gnc-print-date duedate) ?></td>
        </tr>
      <?scm )) ?>
      <?scm (if (not (string=? termsdesc "")) (begin ?>
        <tr><td colspan="2" align="right"><?scm:d termsdesc ?></td></tr>
      <?scm )) ?>
    </table>
  </td>
</tr>
<?scm (if (not (string=? billingid "")) (begin ?>
  <tr>
    <td>Your ref: <?scm:d billingid ?></td>
  </tr>
<?scm )) ?>
</table>

<!-- invoice lines table -->
<p>
<table border="1" width="100%" cellpadding="4" class="entries"> 
  <thead>
    <tr bgcolor="#ccc" valign="bottom">
      <th align="center" ><?scm:d (_ "Date") ?></th>
      <th align="left" width="80%"><?scm:d (_ "Description") ?></th>
      <?scm (if units? (begin ?>
        <th align="left"><?scm:d opt-units-heading ?></th>
        <?scm (set! spancols1 (+ spancols1 1)) 
              (set! spancols2 (+ spancols2 1)))) ?>
      <?scm (if (or units? qty?) (begin ?>
        <th align="right"><?scm:d opt-qty-heading ?></th>
        <?scm (set! spancols1 (+ spancols1 1)) 
              (set! spancols2 (+ spancols2 1)))) ?>
      <?scm (if (or units? qty? discount?) (begin ?>
        <th align="right"><?scm:d opt-unit-price-heading ?></th>
        <?scm (set! spancols1 (+ spancols1 1)) 
              (set! spancols2 (+ spancols2 1)))) ?>
      <?scm (if discount? (begin ?>
        <th align="right"><?scm:d opt-disc-rate-heading ?></th>
        <th align="right"><?scm:d opt-disc-amount-heading ?></th>
        <?scm (set! spancols1 (+ spancols1 2)) 
              (set! spancols2 (+ spancols2 1)))) ?>
      <?scm (if (and tax? taxtables?) (begin ?>
        <th align="right"><?scm:d opt-net-price-heading ?></th>
        <th align="right"><?scm:d opt-tax-rate-heading ?></th>
        <th align="right"><?scm:d opt-tax-amount-heading ?></th>
        <?scm (set! spancols1 (+ spancols1 3))
              (set! spancols2 (+ spancols2 0)))) ?>
      <th align="right"><?scm:d opt-total-price-heading ?></th>
    </tr>
  </thead>

  <tbody> <!-- display invoice entry lines, keeping running totals -->
    <?scm 
      (let ((tax-total (gnc:make-commodity-collector))
            (sub-total (gnc:make-commodity-collector))
            (dsc-total (gnc:make-commodity-collector))
            (inv-total (gnc:make-commodity-collector)))
        (for-each ; entry in entries
          (lambda (entry) 
            (let ((qty       (gncEntryGetQuantity entry))
                  (each      (gncEntryGetInvPrice entry)) 
                  (action    (gncEntryGetAction entry)) 
                  (rval      (gncEntryReturnValue entry #t)) 
                  (rdiscval  (gncEntryReturnDiscountValue entry #t)) 
                  (rtaxval   (gncEntryReturnTaxValue entry #t)) 
                  (disc      (gncEntryGetInvDiscount entry))
                  (disctype  (gncEntryGetInvDiscountType entry))
                  (acc       (gncEntryGetInvAccount entry))
                  (taxable   (gncEntryGetInvTaxable entry))
                  (taxtable  (gncEntryGetInvTaxTable entry)))
              (inv-total 'add currency rval)
              (inv-total 'add currency rtaxval)
              (tax-total 'add currency rtaxval)
              (sub-total 'add currency rval)
              (dsc-total 'add currency rdiscval)
    ?>
    <tr valign="top">
      <td align="center"><?scm:d (gnc-print-date (gncEntryGetDate entry)) ?></td>
      <td align="left"><?scm:d (gncEntryGetDescription entry) ?></td>
      <!-- td align="left">< ?scm:d (gncEntryGetNotes entry) ?></td -->
      <?scm (if units? (begin ?>
        <td align="left"><?scm:d action ?></td>
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
      <?scm (if (and tax? taxtables?) (begin ?>
        <td align="right"><?scm:d (fmtmoney currency rval) ?></td>
        <td align="right"><?scm (taxrate taxable taxtable currency) ?></td>  
        <td align="right"><?scm:d (fmtmoney currency rtaxval) ?></td>
      <?scm )) ?>
      <!-- TO DO: need an option about whether to display the tax-inclusive total? -->
      <td align="right"><?scm:d (fmtmoney currency (gnc-numeric-add rval rtaxval GNC-DENOM-AUTO GNC-RND-ROUND)) ?></td>
    </tr>
    <?scm )) entries) ?>

    <!-- display subtotals row -->
    <?scm (if (or tax? discount? payments?) (begin ?>
      <tr valign="top"> 
        <td align="left" class="subtotal" colspan="<?scm:d spancols2 ?>"><strong><?scm:d opt-subtotal-heading ?></strong></td>
        <?scm (if discount? (begin ?>
          <td align="right" class="subtotal"><strong><?scm (display-comm-coll-total dsc-total #f) ?></strong></td>
        <?scm )) ?>
        <?scm (if (and tax? taxtables?) (begin ?>
          <td align="right" class="subtotal"><strong><?scm (display-comm-coll-total sub-total #f) ?></strong></td>
          <td>&nbsp;</td>
          <td align="right" class="subtotal"><strong><?scm (display-comm-coll-total tax-total #f) ?></strong></td>
        <?scm )) ?>
        <td align="right" class="subtotal"><strong><?scm (display-comm-coll-total inv-total #f) ?></strong></td>
      </tr>
    <?scm )) ?>

    <!-- payments -->
    <?scm 
      (if payments? 
        (for-each
          (lambda (split)  
            (let ((t (xaccSplitGetParent split)))
              (if (not (equal? t txn)) ; don't process the entry itself as a split
                (let ((c (xaccTransGetCurrency t))
                      (a (xaccSplitGetValue    split))) 
                  (inv-total 'add c a) 
    ?>
    <tr valign="top">
      <td align="center"><?scm:d (gnc-print-date (gnc-transaction-get-date-posted t)) ?></td>
      <td align="left" colspan="<?scm:d (- spancols1 1) ?>"><?scm:d opt-payment-recd-heading ?></td> 
      <td align="right"><?scm:d (fmtmoney c a) ?></td>
    </tr>
    <?scm )))) splits)) ?>

    <!-- total row -->
    <tr valign="top">
      <td align="left" class="total" colspan="<?scm:d spancols1 ?>"><strong><?scm:d opt-amount-due-heading ?></strong></td>
      <td align="right" class="total"><strong><?scm (display-comm-coll-total inv-total #f) ?></strong></td>
    </tr>

  </tbody>
  <?scm ) ?> <!-- end of (let) surrounding table body -->
</table>
 
<p><?scm:d (nl->br notes) ?>
<p><?scm:d (nl->br opt-extra-notes) ?>

<?scm )) ; end of display-report function
    
  ; 'mainline' code: check for a valid invoice, then display the report 
  (if (null? opt-invoice)
    (begin
      (display (_ "<h2>Tax Invoice</h2>"))
      (display (_ "<p>No invoice has been selected -- please use the Options menu to select one.")))
    (let* ((owner     (gncInvoiceGetOwner  opt-invoice))
           (endowner  (gncOwnerGetEndOwner owner))
           (ownertype (gncOwnerGetType     endowner)))
      (if (not (eqv? ownertype GNC-OWNER-CUSTOMER))
        (begin
          (display (_ "<h2>Tax Invoice</h2>"))
          (display (_ "<p>This report is designed for customer (sales) invoices only.\n"))
          (display (_ "<p>Please use the Options menu to select an <em>Invoice</em>, not a Bill or Expense Voucher.")))
        (display-report opt-invoice owner endowner ownertype))))

?>
</div>
</body>
</html>
<?scm
) ; end of enclosing let
?>
