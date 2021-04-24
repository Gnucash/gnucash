<?scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balsheet-eg.eguile.scm
;; by Chris Dennis  chris@starsoftanalysis.co.uk
;;
;; This eguile template is designed to be called from
;; balsheet-eg.scm via the eguile mechanism.
;;
;; $Author: chris $ $Date: 2009/06/19 22:40:38 $ $Revision: 1.54 $
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

(let* ((version 0.01))

  ;; Display a row of the accounts table, given the account name and amount,
  ;; and several parameters for adjusting layout and styling.
  (define (display-acc-row
            maxdepth
            depth
            rshift
            name
            amount
            total?
            leftoverrule? ; put rule over cell to the left?
            )
    (let ((accname-class "accname")
          (balance-class "balance")
          (lo-adjust 0)
          (lo-cell "")
          (bold (lambda (x) x))); hack for non-CSS systems
      (if total? (begin
                   (set! accname-class "accnametotal")
                   (if (and (= depth 0) (not (string=? amount "&nbsp;")))
                     (set! balance-class "ruledtotal")
                     (set! balance-class "balancetotal"))
                   (set! bold (lambda (x) (string-append "<b>" x "</b>")))))
      (set! depth (max depth 1)); hack for depth=0
      (if leftoverrule?
        (begin
          (set! lo-adjust -1)
          (set! lo-cell "<td class=\"overruled\">&nbsp;</td>")))
?>
<tr valign="bottom">
<?scm (indent-cells (1- depth)) ?>
<td colspan="<?scm:d (1+ (- maxdepth depth)) ?>" class="<?scm:d accname-class ?>">
<?scm:d (bold name) ?></td>
<?scm (empty-cells (+ (- maxdepth depth) rshift lo-adjust)) (display lo-cell) ?>
<td class="<?scm:d balance-class ?>" align="right"><?scm:d (bold amount) ?></td>
<?scm (empty-cells (- (1- depth) rshift)) ?>
</tr>
<?scm ))

  (define (display-accounts-table-r
            tree        ; list of accrecs
            neg?
            maxdepth
            rshift
            onedepth1)
    ;; Recursively display the accounts table from the given tree
    ;; (as returned by process-acc-list)
    (for-each
     (lambda (accrec)
       (display-acc-row
        maxdepth
        (accrec-depth accrec)
        ;; has sub-accounts: shift left to put balance in same column
        ;; as sub-accounts
        (+ rshift (if (accrec-sublist accrec) -1 0))
        (accrec-namelink accrec)
        ;; Don't show zero amount for a placeholder -- the value to
        ;; test for zero depends on whether or not this is a 'summary'
        ;; value (i.e. a total of sub-accounts that are not shown
        ;; separately)
        (cond
         ((and (accrec-placeholder? accrec)
               (if (accrec-summary? accrec)
                   (not (accrec-non-zero? accrec))
                   (zero? (accrec-balance-num accrec))))
          "&nbsp;")
         ((accrec-summary? accrec) (format-comm-coll (accrec-subtotal-cc accrec)))
         (else (format-monetary (accrec-balance-mny accrec))))
        (< (accrec-depth accrec) 1); total?
        #f) ; leftoverrule?
       (when (accrec-sublist accrec)
         ;; recurse to deeper accounts...
         (display-accounts-table-r
          (accrec-sublist accrec) neg? maxdepth rshift onedepth1)
         ;; ...and then display the total
         ;; unless there is only one depth-1 account
         (unless (and onedepth1 (= 1 (accrec-depth accrec)))
           (display-acc-row
            maxdepth
            (accrec-depth accrec)
            (if (> (accrec-depth accrec) 1) rshift 0)
            (string-append (G_ "Total") " " (accrec-namelink accrec))
            (format-comm-coll-total (accrec-subtotal-cc accrec))
            (<= (accrec-depth accrec) 1)        ; total?
            (> (accrec-depth accrec) 0)))))   ; leftoverrule?

     tree))
?>

<!-- The HTML starts here... -->
<html dir='auto'>
<head>
<meta http-equiv="content-type" content="text-html; charset=utf-8">
<title><?scm:d coyname ?> <?scm:d opt-report-title ?> <?scm:d (qof-print-date opt-date) ?></title>

<link rel="stylesheet" href="<?scm:d opt-css-file ?>" type="text/css">
<!-- Note that the stylesheet file is overridden by some options, i.e.
     opt-font-family and opt-font-size                                 -->
<style type="text/css">
  body {
    <?scm (if opt-font-family (begin ?>
      font-family: <?scm:d opt-font-family ?>;
    <?scm )) ?>
    <?scm (if opt-font-size (begin ?>
      font-size: <?scm:d opt-font-size ?>;
    <?scm )) ?>
  }
  table { /* table does not inherit font sizes for some reason */
    <?scm (if opt-font-size (begin ?>
      font-size: <?scm:d opt-font-size ?>;
    <?scm )) ?>
  }
</style>

</head>
<body>
<h3><?scm:d coyname ?></h3>
<h2><?scm:d opt-report-title ?> <?scm:d (qof-print-date opt-date) ?></h2>

<?scm
  ;; This is where the work is done.
  ;; Create three accounts trees, make a few adjustments, then display them
  (let* ((accrec-as (process-acc-list asset-accounts #f))
         (accrec-li (process-acc-list liability-accounts #t))
         (accrec-eq (process-acc-list equity-accounts #t))
         (accrec-tr (process-acc-list trading-accounts #t))
         (accrec-ie (process-acc-list income-expense-accounts #t))
         (maxdepth 0)
         (rshift-as 0)
         (rshift-li 0)
         (rshift-eq 0)
         (rshift-tr 0)
         (rshift-ie 0)
         (balancing-cc (gnc:make-commodity-collector))
         (etl-cc (gnc:make-commodity-collector)))
    (accrec-set-namelink! accrec-as (G_ "Assets Accounts"))
    (accrec-set-placeholder?! accrec-as #t)
    (balancing-cc 'merge (accrec-subtotal-cc accrec-as) #f)
    (if (and (one-depth-1 accrec-as)
             (> (accrec-treedepth accrec-as) 1))
      (set! rshift-as 1))
    (accrec-set-namelink! accrec-li (G_ "Liability Accounts"))
    (accrec-set-placeholder?! accrec-li #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-li) #f)
    (if (and (one-depth-1 accrec-li)
             (> (accrec-treedepth accrec-li) 1))
      (set! rshift-li 1))
    (accrec-set-namelink! accrec-eq (G_ "Equity Accounts"))
    (accrec-set-placeholder?! accrec-eq #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-eq) #f)
    (accrec-set-namelink! accrec-tr (G_ "Trading Accounts"))
    (accrec-set-placeholder?! accrec-tr #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-tr) #f)
    (balancing-cc 'minusmerge etl-cc #f)
    (accrec-set-namelink! accrec-ie
                          (if (gnc-numeric-negative-p (accrec-balance-num accrec-ie))
                            (G_ "Retained Losses")
                            (G_ "Retained Earnings")))
    (accrec-set-placeholder?! accrec-ie #t)
    (balancing-cc 'minusmerge (accrec-subtotal-cc accrec-ie) #f)
    (if (and (one-depth-1 accrec-eq)
             (> (accrec-treedepth accrec-eq) 1))
      (set! rshift-eq 1))
    (if (and (one-depth-1 accrec-tr)
             (> (accrec-treedepth accrec-tr) 1))
      (set! rshift-tr 1))
    (if (and (one-depth-1 accrec-ie)
             (> (accrec-treedepth accrec-ie) 1))
      (set! rshift-ie 1))

?>
<table border="0" class="outer"><tr valign="top"><td valign="top"> <!-- outer table to control columns -->
<table border="0" class="accounts" align="left">
<?scm

    (set! maxdepth (max (accrec-treedepth accrec-as)
                        (accrec-treedepth accrec-li)
                        (accrec-treedepth accrec-eq)
                        (accrec-treedepth accrec-ie)
                        (accrec-treedepth accrec-tr)))

    ; Display assets section
    (display-accounts-table-r (list accrec-as) #f maxdepth rshift-as (one-depth-1 accrec-as))
    (hrule (* maxdepth 2))

    ; Split table across columns if required
    (case opt-columns
      ((autocols)
        ?>
        </table>
        <!-- <table border="0" align="left"><tr><td>&nbsp;</td></tr></table> -->
        &nbsp;&nbsp;<table border="0" align="left">
        <?scm
        )
      ((twocols)
        ?>
        </table></td><td valign="top"><table border="0">
        <?scm
        ))

    ; Display liabilities and equity sections
    (display-accounts-table-r (list accrec-li) #t maxdepth rshift-li (one-depth-1 accrec-li))
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list accrec-tr) #t maxdepth rshift-tr (one-depth-1 accrec-tr))
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list accrec-eq) #t maxdepth rshift-eq (one-depth-1 accrec-eq))
    (hrule (* maxdepth 2))
    (display-acc-row
      maxdepth 0 0
      (G_ "Total Equity, Trading, and Liabilities")
      (format-comm-coll-total etl-cc)
      #t #f)
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list accrec-ie) #t maxdepth 0 (one-depth-1 accrec-ie))
    (hrule (* maxdepth 2))
    (if (not (gnc-commodity-collector-allzero? balancing-cc))
        (display-acc-row
          maxdepth 0 0
          (G_ "Imbalance Amount")
          (format-comm-coll-total balancing-cc)
          #t #f))

?>
</table>
</table>
<?scm
    ); end of let
?>

<?scm
  ;; Display exchange rates table
  (set! xlist (assoc-remove! xlist opt-report-commodity))
  (if (not (null? xlist))
    (begin
?>
<p><?scm:d (G_ "<strong>Exchange Rates</strong> used for this report") ?>
<table border="0">
<?scm
      (for-each
       (lambda (xpair)
         (let* ((comm (car xpair))
                (one-foreign-mny (gnc:make-gnc-monetary comm 1))
                (one-local-mny (exchange-fn one-foreign-mny opt-report-commodity))
                (conv-amount (gnc:gnc-monetary-amount one-local-mny))
                (price-str (gnc:default-price-renderer
                            opt-report-commodity conv-amount)))
?>
<tr>
  <td align="right"><?scm:d (gnc:monetary->string one-foreign-mny) ?></td>
  <td>=</td>
  <td align="right"><?scm:d price-str ?></td>
</tr>
<?scm
        ))
       xlist)
?>
</table>
<?scm
  )) ; end of exchange rates table
?>

<br clear="both">
<p><?scm:d opt-extra-notes ?>

</body>
</html>

<?scm
) ; enclosing let
?>

