#! /usr/bin/perl
#
# FUNCTION:
# restore gnucash transactions from a gnucash log file.
#
# Usage: cat <logfile> | gnc-restore.pl <gnucash-filename>
#
# Warning! this script probably does the wrong thing, 
# and has never been tested!!
# It will probably destroy your data!  Use at your own risk!
#

# hack alert -- fix the paths
# set the path below to where your gnucash.pm is located
use lib '/usr/local/gnucash-1.4/lib/gnucash/perl';
use lib '/usr/local/gnucash-1.4/share/gnucash/perl';
use gnucash;

  
die "Usage: cat <logfile> | $0 <gnucash-filename>" if $#ARGV < 0;
  
# open the file
print "Opening file $ARGV[0]\n";
$sess = gnucash::xaccMallocSession ();
$grp = gnucash::xaccSessionBeginFile ($sess,$ARGV[0]);

die "failed to read file $ARGV[0], maybe its locked? " if (! $grp);

$got_data = 0;
$nsplit = 0;

# --------------------------------------------------
while (<STDIN>) {

  # start of transaction
  if (/^===== START/) { 
     $nsplit = 0;
     next; 
  }

  # end of transaction
  if (/^===== END/) { 
     if ($got_data == 1) {
        gnucash::xaccTransCommitEdit ($trans);
     }
     $got_data = 0;
     next; 
  }
  
  # ignore 'begin' lines
  if (/^B/) { next; }
  if (/^D/) { 
    print "WARNING: deletes not handled, you will have to manually delete\n";
    next; 
  }

  # ignore any line that's not a 'commit'
  if (!/^C/) { next; }
  
  chop;

  # get journal entry
  ($mod, $id, $time_now, $date_entered, $date_posted,
   $account, $num, $description, $memo, $action, 
   $reconciled, $amount, $price, $date_reconciled)
    = split (/	/);

  # parse amount & price 
  # gnucash-1.4.x : float point;  gnucash-1.5.x: fractional ratio
  ($anum, $adeno) = split (/\//, $amount);
  if (0 != $adeno) {
     $amount = $anum / $adeno;
  }
    
  ($pnum, $pdeno) = split (/\//, $price);
  if (0 != $pdeno) {
     $price = $pnum / $pdeno;
     # value, not price in gnucash-1.5.x
     if (0 != $amount) {
        $price = $price/$amount;
     }
  }

  $dyear = int($date_posted/10000000000);
  $dmonth = int($date_posted/100000000) - 100*$dyear;
  $dday = int($date_posted/1000000) - 10000*$dyear - 100*$dmonth;
  
  $dpost = $dmonth . "/" . $dday . "/" . $dyear;

  # do a 'commit'
  if ($mod == C) { 
     print "restoring '$account' $dpost '$description' for $amount at price $price\n";
     
     if ($got_data == 0) {
        $trans = gnucash::xaccMallocTransaction();
        gnucash::xaccTransBeginEdit( $trans, 1);
        $got_data = 1;
     }

     gnucash::xaccTransSetDescription( $trans, $description);
     gnucash::xaccTransSetDateStr ($trans, $dpost);
     gnucash::xaccTransSetNum ($trans, $num);

     if ($nsplit == 0) {
        $split = gnucash::xaccTransGetSplit ($trans, $nsplit);
     } else {
        $split = gnucash::xaccMallocSplit();
	gnucash::xaccTransAppendSplit($trans, $split);
     }
     gnucash::xaccSplitSetAction ($split, $action);
     gnucash::xaccSplitSetMemo ($split, $memo);
     gnucash::xaccSplitSetReconcile ($split, $reconciled);

     # hack alert -- fixme: the reconcile date is not set ...
     # need to convert date_reconciled to 'seconds' ... 
     # gnucash::xaccSplitSetDateReconciled ($split, $date_reconciled);
     gnucash::xaccSplitSetSharePriceAndAmount($split, $price, $amount);

     $acct = gnucash::xaccGetAccountFromName ($grp,$account);
     gnucash::xaccAccountBeginEdit ($acct, 1);
     gnucash::xaccAccountInsertSplit ($acct, $split);
     gnucash::xaccAccountCommitEdit ($acct);
  
     $nsplit ++;
  }

}

gnucash::xaccSessionSave ($sess);
gnucash::xaccSessionEnd ($sess);


