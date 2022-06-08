<?scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; owner-billing.eguile.scm
;; Created by: Ralf Zerres <ralf.zerres@mail.de>
;; Copyright (c) 2022 Ralf Zerres <ralf.zerres@mail.de>
;;
;; This eguile template is designed to be called from
;; owner-billing.scm via the eguile mechanism.
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

  ;; Display a row of the owner bill table, given the account name and amount,
  ;; and several parameters for adjusting layout and styling.
  (define (display-bill-row
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

  (define (display-bill-table-r
	    tree        ; list of account records
	    neg?
	    maxdepth
	    rshift
	    onedepth1)
    ;; Recursively display the accounts table from the given tree
    ;; (as returned by process-acc-list)
    (for-each
     (lambda (billrec)
       (display-bill-row
	maxdepth
	(billrec-depth billrec)
	;; has sub-accounts: shift left to put balance in same column
	;; as sub-accounts
	(+ rshift (if (billrec-sublist billrec) -1 0))
	(billrec-namelink billrec)
	;; Don't show zero amount for a placeholder -- the value to
	;; test for zero depends on whether or not this is a 'summary'
	;; value (i.e. a total of sub-accounts that are not shown
	;; separately)
	(cond
	 ((and (billrec-placeholder? billrec)
	       (if (billrec-summary? billrec)
		   (not (billrec-non-zero? billrec))
		   (zero? (billrec-balance-num billrec))))
	  "&nbsp;")
	 ((billrec-summary? billrec) (format-comm-coll (billrec-subtotal-cc billrec)))
	 (else (format-monetary (billrec-balance-mny billrec))))
	(< (billrec-depth billrec) 1); total?
	#f) ; leftoverrule?
       (when (billrec-sublist billrec)
	 ;; recurse to deeper accounts...
	 (display-accounts-table-r
	  (billrec-sublist billrec) neg? maxdepth rshift onedepth1)
	 ;; ...and then display the total
	 ;; unless there is only one depth-1 account
	 (unless (and onedepth1 (= 1 (billrec-depth billrec)))
	   (display-acc-row
	    maxdepth
	    (billrec-depth billrec)
	    (if (> (billrec-depth billrec) 1) rshift 0)
	    (string-append (G_ "Total") " " (billrec-namelink billrec))
	    (format-comm-coll-total (billrec-subtotal-cc billrec))
	    (<= (billrec-depth billrec) 1)        ; total?
	    (> (billrec-depth billrec) 0)))))   ; leftoverrule?

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
  (let* ((billrec-as (process-acc-list asset-accounts #f))
	 (billrec-li (process-acc-list liability-accounts #t))
	 (billrec-eq (process-acc-list equity-accounts #t))
	 (billrec-tr (process-acc-list trading-accounts #t))
	 (billrec-ie (process-acc-list income-expense-accounts #t))
	 (maxdepth 0)
	 (rshift-as 0)
	 (rshift-li 0)
	 (rshift-eq 0)
	 (rshift-tr 0)
	 (rshift-ie 0)
	 (balancing-cc (gnc:make-commodity-collector))
	 (etl-cc (gnc:make-commodity-collector)))
    (billrec-set-namelink! billrec-as (G_ "Assets Accounts"))
    (billrec-set-placeholder?! billrec-as #t)
    (balancing-cc 'merge (billrec-subtotal-cc billrec-as) #f)
    (if (and (one-depth-1 billrec-as)
	     (> (billrec-treedepth billrec-as) 1))
      (set! rshift-as 1))
    (billrec-set-namelink! billrec-li (G_ "Liability Accounts"))
    (billrec-set-placeholder?! billrec-li #t)
    (etl-cc 'merge (billrec-subtotal-cc billrec-li) #f)
    (if (and (one-depth-1 billrec-li)
	     (> (billrec-treedepth billrec-li) 1))
      (set! rshift-li 1))
    (billrec-set-namelink! billrec-eq (G_ "Equity Accounts"))
    (billrec-set-placeholder?! billrec-eq #t)
    (etl-cc 'merge (billrec-subtotal-cc billrec-eq) #f)
    (billrec-set-namelink! billrec-tr (G_ "Trading Accounts"))
    (billrec-set-placeholder?! billrec-tr #t)
    (etl-cc 'merge (billrec-subtotal-cc billrec-tr) #f)
    (balancing-cc 'minusmerge etl-cc #f)
    (billrec-set-namelink! billrec-ie
			  (if (gnc-numeric-negative-p (billrec-balance-num billrec-ie))
			    (G_ "Retained Losses")
			    (G_ "Retained Earnings")))
    (billrec-set-placeholder?! billrec-ie #t)
    (balancing-cc 'minusmerge (billrec-subtotal-cc billrec-ie) #f)
    (if (and (one-depth-1 billrec-eq)
	     (> (billrec-treedepth billrec-eq) 1))
      (set! rshift-eq 1))
    (if (and (one-depth-1 billrec-tr)
	     (> (billrec-treedepth billrec-tr) 1))
      (set! rshift-tr 1))
    (if (and (one-depth-1 billrec-ie)
	     (> (billrec-treedepth billrec-ie) 1))
      (set! rshift-ie 1))

?>
<table border="0" class="outer"><tr valign="top"><td valign="top"> <!-- outer table to control columns -->
<table border="0" class="accounts" align="left">
<?scm

    (set! maxdepth (max (billrec-treedepth billrec-as)
			(billrec-treedepth billrec-li)
			(billrec-treedepth billrec-eq)
			(billrec-treedepth billrec-ie)
			(billrec-treedepth billrec-tr)))

    ; Display assets section
    (display-accounts-table-r (list billrec-as) #f maxdepth rshift-as (one-depth-1 billrec-as))
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
    (display-accounts-table-r (list billrec-li) #t maxdepth rshift-li (one-depth-1 billrec-li))
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list billrec-tr) #t maxdepth rshift-tr (one-depth-1 billrec-tr))
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list billrec-eq) #t maxdepth rshift-eq (one-depth-1 billrec-eq))
    (hrule (* maxdepth 2))
    (display-acc-row
      maxdepth 0 0
      (G_ "Total Equity, Trading, and Liabilities")
      (format-comm-coll-total etl-cc)
      #t #f)
    (hrule (* maxdepth 2))
    (display-accounts-table-r (list billrec-ie) #t maxdepth 0 (one-depth-1 billrec-ie))
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
