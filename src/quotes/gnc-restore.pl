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

# --------------------------------------------------
# @account_list = &account_flatlist ($account_group);
# This routine accepts a pointer to a group, returns 
# a flat list of all of the children in the group.

sub account_flatlist 
{
  my $grp = $_[0];
  my $naccts = gnucash::xaccGroupGetNumAccounts ($grp);
  my $n;
  my (@acctlist, @childlist);
  my $children;

  foreach $n (0..$naccts-1)
  {
    $acct = gnucash::xaccGroupGetAccount ($grp, $n);
    push (@acctlist, $acct);

    $children = gnucash::xaccAccountGetChildren ($acct);
    if ($children)
    {
      @childlist = &account_flatlist ($children);
      push (@acctlist, @childlist);
    }
  }

  return (@acctlist);
}

# --------------------------------------------------
# If the gnucash engine had a 'get account by name' 
# utility function, then we wouldn't need this and the above mess.

sub get_account_by_name
{
  my $accname = $_[0];
  my $name;

  # loop over the accounts, try to match the name
  foreach $acct (@acctlist)
  {
    $name = gnucash::xaccAccountGetName ($acct);
    if ($name eq $accname) {
       $found = $acct;
       break;
    }
  }

  return ($found);
}
  
# --------------------------------------------------
  
die "Usage: cat <logfile> | $0 <gnucash-filename>" if $#ARGV < 0;
  

# open the file
print "Opening file $ARGV[0]\n";
$sess = gnucash::xaccMallocSession ();
$grp = gnucash::xaccSessionBeginFile ($sess,$ARGV[0]);

die "failed to read file $ARGV[0], maybe its locked? " if (! $grp);

# get a flat list of accounts in the file
@acctlist = &account_flatlist ($grp);


$got_data = 0;
$nsplit = 0;

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
  # gnucash-1.4 : float pt, gnucash1.5 : ratio
  ($anum, $adeno) = split (/\//, $amount);
  if (0 != $adeno) {
     $amount = $anum / $adeno;
  }
    
  ($pnum, $pdeno) = split (/\//, $price);
  if (0 != $pdeno) {
     $price = $pnum / $pdeno;
     # value, not price ... 
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
     print "restoring '$account' '$description' for $pric and '$quant'\n";
     print "date is $dpost  $date_posted\n";
     
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

     $acct = get_account_by_name ($account);
     gnucash::xaccAccountBeginEdit ($acct, 1);
     gnucash::xaccAccountInsertSplit ($acct, $split);
     gnucash::xaccAccountCommitEdit ($acct);
  
     $nsplit ++;
  }

}

gnucash::xaccSessionSave ($sess);
gnucash::xaccSessionEnd ($sess);


