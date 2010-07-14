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
        (let ((rshift2 0)    ; adjust the amount column by this much
              (showamt? #t)) ; whether to show the amount (e.g. not if zero)
          (if (and (accrec-sublist accrec)
                ;   (> (accrec-depth accrec) 0))
                )
            ; has sub-accounts: shift left to put balance in same column as sub-accounts
            (set! rshift2 -1))
          ; Don't show zero amount for a placeholder -- the value to
          ; test for zero depends on whether or not this is a 'summary' value
          ; (i.e. a total of sub-accounts that are not shown separately)
          (if (and (accrec-placeholder? accrec)
                   (if (accrec-summary? accrec)
                     (not (accrec-non-zero? accrec))
                     (gnc-numeric-zero-p (accrec-balance-num accrec))))
            (set! showamt? #f))
          (display-acc-row
            maxdepth
            (accrec-depth accrec)
            (+ rshift rshift2)
            (accrec-namelink accrec)
            (if showamt?
              (if (accrec-summary? accrec)
                (format-comm-coll (accrec-subtotal-cc accrec))
                (format-monetary (accrec-balance-mny accrec)))
              "&nbsp;"
              )
            (< (accrec-depth accrec) 1); total?
            #f) ; leftoverrule?
          (if (accrec-sublist accrec)
            (begin
              ; recurse to deeper accounts...
              (display-accounts-table-r (accrec-sublist accrec) neg? maxdepth rshift onedepth1)
              ; ...and then display the total
              ; unless there is only one depth-1 account
              (if (not (and onedepth1
                            (= 1 (accrec-depth accrec))))
                (display-acc-row
                  maxdepth
                  (accrec-depth accrec)
                  (if (> (accrec-depth accrec) 1) rshift 0)
                  (string-append (_ "Total") " " (accrec-namelink accrec))
                  (format-comm-coll-total (accrec-subtotal-cc accrec))
                  (<= (accrec-depth accrec) 1)        ; total?
                  (> (accrec-depth accrec) 0)))))))   ; leftoverrule?
      tree
      ))
?>

<!-- The HTML starts here... -->
<html>
<head>
<meta http-equiv="content-type" content="text-html; charset=utf-8">
<title><?scm:d coyname ?> <?scm:d reportname ?> <?scm:d (gnc-print-date opt-date-tp) ?></title>

<?scm (if css? (begin ?>
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
<?scm )) ?>

</head>
<body>
<?scm (if (not css?) (begin ?>
  <table border="0" cellpadding="16"><tr><td> <!-- hack for GTKHTML -->
<?scm )) ?>
<h3><?scm:d coyname ?></h3>
<h2><?scm:d reportname ?> <?scm:d (gnc-print-date opt-date-tp) ?></h2>

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
    (accrec-set-namelink! accrec-as (_ "Assets Accounts"))
    (accrec-set-placeholder?! accrec-as #t)
    (balancing-cc 'merge (accrec-subtotal-cc accrec-as) #f)
    (if (and (one-depth-1 accrec-as)
             (> (accrec-treedepth accrec-as) 1))
      (set! rshift-as 1))
    (accrec-set-namelink! accrec-li (_ "Liability Accounts"))
    (accrec-set-placeholder?! accrec-li #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-li) #f)
    (if (and (one-depth-1 accrec-li)
             (> (accrec-treedepth accrec-li) 1))
      (set! rshift-li 1))
    (accrec-set-namelink! accrec-eq (_ "Equity Accounts"))
    (accrec-set-placeholder?! accrec-eq #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-eq) #f)
    (accrec-set-namelink! accrec-tr (_ "Trading Accounts"))
    (accrec-set-placeholder?! accrec-tr #t)
    (etl-cc 'merge (accrec-subtotal-cc accrec-tr) #f)
    (balancing-cc 'minusmerge etl-cc #f)
    (accrec-set-namelink! accrec-ie
                          (if (gnc-numeric-negative-p (accrec-balance-num accrec-ie))
                            (_ "Retained Losses")
                            (_ "Retained Earnings")))
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

   (if debugging?
     (begin
      (display "<p>Assets: ") (display accrec-as)
      (display "<p>Liabilities: ") (display accrec-li)
      (display "<p>Equities: ") (display accrec-eq)
      (display "<p>Trading: ") (display accrec-tr)
      (display "<p>Profit and loss: ") (display accrec-ie)))

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
      (_ "Total Equity, Trading, and Liabilities")
      (format-comm-coll-total etl-cc)
      #t #f)
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list accrec-ie) #t maxdepth 0 (one-depth-1 accrec-ie))
    (hrule (* maxdepth 2))
    (if (not (gnc-commodity-collector-allzero? balancing-cc))
        (display-acc-row
          maxdepth 0 0
          (_ "Imbalance Amount")
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
<p><?scm:d (_ "<strong>Exchange Rates</strong> used for this report") ?>
<table border="0">
<?scm
      (for-each
        (lambda (xpair)
          (let* ((comm (car xpair))
                 (one-num (gnc:make-gnc-numeric 10000 1))
                 (one-foreign-mny (gnc:make-gnc-monetary comm one-num))
                 (one-local-mny (exchange-fn one-foreign-mny opt-report-commodity)))
?>
<tr>
  <td align="right">1 <?scm:d (gnc-commodity-get-mnemonic comm) ?></td>
  <td>=</td>
  <td align="left"><?scm:d (fmtnumeric
                             (gnc-numeric-div
                               (gnc:gnc-monetary-amount one-local-mny)
                               (gnc:gnc-monetary-amount one-foreign-mny)
                               GNC-DENOM-AUTO
                               (logior (GNC-DENOM-SIGFIGS 8) GNC-RND-ROUND))) ?>
                   <?scm:d (gnc-commodity-get-mnemonic opt-report-commodity) ?></td>
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

<?scm (if (not css?) (begin ?>
  </table> <!-- hack for GTKHTML -->
<?scm )) ?>

</body>
</html>

<?scm
) ; enclosing let
?>

