#!/usr/bin/perl -w

# gnucash perl demo:
#
# This file demonstrates how to open an acount file and print 
# the names and balances of the top-level accounts in the file.
#
# use lib '../swig/perl5/';
use lib '..';
use gnucash;                                           
package gnucash;

die "Usage: $0 <filename>" if $#ARGV < 0;
print "Will load $ARGV[0]\n";

gnucash::gnc_engine_init(0, $ARGV);
$session = gnucash::gnc_book_new ();

$rc = gnucash::gnc_book_begin ($session, $ARGV[0], 1, 0);
if ($rc != 1) 
{
   $err = gnucash::gnc_book_get_error ($session);
   print "Could not find $ARGV[0], errrocode=$err\n";
}

$secs = time;

$rc = gnucash::gnc_book_load ($session);
die "Could not load $ARGV[0]\n" if $rc != 1;

($user,$sys,$cuser,$csys) = times;
$elapsed = time() - $secs;
print "time to load: user-cpu=$user sys-cpu=$sys elapsed(rounded to sec)=$elapsed\n";

$grp = gnucash::gnc_book_get_group ($session);
$numacc = gnucash::xaccGroupGetNumAccounts ($grp);
print "Loaded $numacc accounts\n\n";

for ($i=0; $i<$numacc; $i++) {
   $acct = gnucash::xaccGroupGetAccount ($grp, $i);
   $acctname = gnucash::xaccAccountGetName ($acct);
   $numeric_baln = gnucash::xaccAccountGetBalance ($acct);
   $baln = gnucash::gnc_numeric_to_double ($numeric_baln);
   print "\tAccount: $acctname \tBalance: $baln\n";
}

$fsecs = time;
($user,$sys,$cuser,$csys) = times;

gnucash::gnc_book_end ($session);

($fuser,$fsys,$cuser,$csys) = times;
$elapsed = time() - $secs;
print "time to finish: user-cpu=$fuser sys-cpu=$fsys elapsed(rounded to sec)=$elapsed\n";
$fuser -= $user;
$fsys -= $sys;
$elapsed = time() - $fsecs;
print "delta time to finish: user-cpu=$fuser sys-cpu=$fsys elapsed(rounded to sec)=$elapsed\n";

