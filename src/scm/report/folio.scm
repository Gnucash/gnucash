
(gnc:support "report/folio.scm")

;; I haven't finished converting this yet...

;(gnc:define-report
 ;; version
; 1
 ;; Menu name
; "Folio"
 ;; Options Generator
; #f
 ;; Rendering thunk.  See report.scm for details.
; (lambda (options)
;   (list
;    "<html>"
;    "<head>"
;    "<title>Portfolio Valuation</title>"
;    "</head>"

;    "<body bgcolor=#ccccff>"
;    "This page shows the valuation of your stock/mutual fund portfolio."
;    "<br>"
;    "You can create custom reports by editing the file"
;    "<tt>Reports/report-folio.phtml</tt>"
;    "<p>"

    ;; currency symbol that is printed is a dollar sign, for now
    ;; currency amounts get printed with two decimal places
    
    ;; require ...
    ;; hack alert -- need a require here, since the folowing routine(s)
    ;; are identical to those in gnc-price script.
    ;; --------------------------------------------------
    ;; @account_list = &account_flatlist ($account_group);
    ;; This rouine accepts a pointer to a group, returns
    ;; a flat list of all of the children in the group.

;sub account_flatlist
;{
;   local ($grp) = $_[0];
;   local ($naccts) = gnucash::xaccGroupGetNumAccounts ($grp);
;   local ($n);
;   local (@acctlist, @childlist);
;   local ($children);

;   foreach $n (0..$naccts-1) {
;      $acct = gnucash::xaccGroupGetAccount ($grp, $n);
;      push (@acctlist, $acct);
;      $children = gnucash::xaccAccountGetChildren ($acct);
;      if ($children) {
;         @childlist = &account_flatlist ($children);
;         push (@acctlist, @childlist);
;      }
;   }

;   return (@acctlist);
;}

;; --------------------------------------------------
;; $split = &get_last_split ($account);
;; returns the most recent split in the account.

;sub get_last_split
;{
;   local ($acct)  = $_[0];
;   local ($query, $splitlist, $split);

;   $query = gnucash::xaccMallocQuery();
;   gnucash::xaccQueryAddAccount ($query, $acct);
;   gnucash::xaccQuerySetMaxSplits ($query, 1);
;   $splitlist = gnucash::xaccQueryGetSplits ($query);

;   $split = gnucash::IthSplit ($splitlist, 0);
;}

;; --------------------------------------------------

;; get a flat list of all the accounts ...
;@acclist = &account_flatlist ($topgroup);

;; get the most recent price date ..
;$latest = -1.0e20;
;$earliest = 1.0e20;
;foreach $acct (@acclist) 
;{
;   $accntype = &gnucash::xaccAccountGetType($acct);
;   if (($accntype == $gnucash::STOCK) || 
;       ($accntype == $gnucash::MUTUAL)) {
;      $split = &get_last_split ($acct);
;      $trans = gnucash::xaccSplitGetParent ($split);
;      $secs = gnucash::xaccTransGetDate ($trans);
;      if ($latest < $secs) { $latest = $secs; }
;      if ($earliest > $secs) { $earliest = $secs; }
;   }
;}

;$ldayte = gnucash::xaccPrintDateSecs ($latest);
;$edayte = gnucash::xaccPrintDateSecs ($earliest);


;<table cellpadding=1>
;<caption><b>Stock Portfolio Valuation</b>
;<br>Earliest Price <:= $edayte :> &nbsp; &nbsp; Latest Price <:= $ldayte :>
;</caption>
;<tr>
;<th>Name
;<th>Ticker
;<th align=center>Shares
;<th align=center>Recent Price
;<th align=center>Value
;<th align=center>Cost
;<th align=center>Profit/Loss

;$totvalue = 0;
;$totcost = 0;

;foreach $acct (@acclist) 
;{

;   $accntype = &gnucash::xaccAccountGetType($acct);
;   if (($accntype == $gnucash::STOCK) || 
;       ($accntype == $gnucash::MUTUAL)) {

;      $accname = &gnucash::xaccAccountGetName($acct);
;      $ticker = &gnucash::xaccAccountGetSecurity ($acct);
;      $accbaln = &gnucash::xaccAccountGetBalance($acct);

;      $split = &get_last_split ($acct);
;      $price = gnucash::xaccSplitGetSharePrice ($split);
;      $shares = gnucash::xaccSplitGetShareBalance ($split);
;      $value = gnucash::xaccSplitGetBalance ($split);
;      $cost = gnucash::xaccSplitGetCostBasis ($split);
;      $profit = $accbaln - $cost;

;      $totvalue += $value;
;      $totcost += $cost;

;      print "<tr><td>$accname";
;      print "<td>$ticker";
;      printf "<td align=right nowrap>%10.3f", $shares;
;      printf "<td align=right nowrap>\$%10.2f\n", $price;
;      printf "<td align=right nowrap>\$%10.2f\n", $value;
;      printf "<td align=right nowrap>\$%10.2f\n", $cost;
;      printf "<td align=right nowrap>\$%10.2f\n", $profit;
;   }
;}

;print "<tr><td>&nbsp;<td>&nbsp;<td>&nbsp;\n";   ;; blank line
;print "<td>&nbsp;<td>&nbsp;<td>&nbsp;\n";   ;; blank line

;print "<tr><td><b>Net</b><td>&nbsp;";
;print "<td>&nbsp;<td>&nbsp;";
;printf "<td align=right nowrap>&nbsp;&nbsp;<u>\$%10.2f</u> \n", $totvalue;
;printf "<td align=right nowrap>&nbsp;&nbsp;<u>\$%10.2f</u> \n", $totcost;
;printf "<td align=right nowrap>&nbsp;&nbsp;<u>\$%10.2f</u> \n", $totvalue-$totcost;

;</table>
;</body>
;</html>
