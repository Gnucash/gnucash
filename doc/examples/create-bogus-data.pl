#! /usr/bin/env perl
#
## @file
#
# create-bogus-data.pl
#
# @brief Create a lot of 'fake' transactions, handy for generating 
# large datasets for performance testing.
# 
# currently, very hacked up, uses hard-wired accounts
# from the "txnreport.xac" test file
#


$ntrans = 3000;

$fromacct="bbf5756d8cada56c1245c49a1a5627a7";
$toacct="ef2b8b63e7be48360da608038b65f5bf";

sub getguid
{
   local $guid;
   open (UID, "uuidgen |");
   $guid = <UID>;
   chop $guid;
   $guid =~ s/-//g;
   return ($guid);
}

for ($i=0; $i<$ntrans; $i++)
{
   $trnguid = &getguid;
   $spaguid = &getguid;
   $spbguid = &getguid;
   
   $val = 10001 + $i;

   $hr = $i % 24;
   $dy = (($i-$hr)/24) %26 + 1;
   $mon = ($i - ($i%625))/625 + 9;
   $mon = $mon %12 +1;
   
   $datep = join ('', "2000-", $mon, "-", $dy, " ", $hr, ":26:30 -0400");

   print "
<gnc:transaction version=\"2.0.0\">
  <trn:id type=\"guid\">$trnguid</trn:id>
  <trn:date-posted>
    <ts:date>$datep</ts:date>
  </trn:date-posted>
  <trn:date-entered>
    <ts:date>2000-09-08 15:26:30 -0400</ts:date>
    <ts:ns>598529000</ts:ns>
  </trn:date-entered>
  <trn:description>Cap. gain (short)</trn:description>
  <trn:splits>
    <trn:split>
      <split:id type=\"guid\">$spaguid</split:id>
      <split:reconciled-state>n</split:reconciled-state>
      <split:value>$val/100</split:value>
      <split:quantity>$val/100</split:quantity>
      <split:account type=\"guid\">$fromacct</split:account>
    </trn:split>
    <trn:split>
      <split:id type=\"guid\">$spbguid</split:id>
      <split:reconciled-state>n</split:reconciled-state>
      <split:value>-$val/100</split:value>
      <split:quantity>-$val/100</split:quantity>
      <split:account type=\"guid\">$toacct</split:account>
    </trn:split>
  </trn:splits>
</gnc:transaction>\n";


}
