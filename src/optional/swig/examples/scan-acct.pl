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
print "its $ARGV[0]\n";

$session = gnucash::gnc_book_new ();
gnucash::gnc_book_begin ($session, $ARGV[0], 0, 0);
gnucash::gnc_book_load ($session);

$grp = gnucash::gnc_book_get_group ($session);
$numacc = gnucash::xaccGroupGetNumAccounts ($grp);
print "Loaded $numacc accounts\n\n";

for ($i=0; $i<$numacc; $i++) {
   $acct = gnucash::xaccGroupGetAccount ($grp, $i);
   $acctname = gnucash::xaccAccountGetName ($acct);
   $baln = gnucash::xaccAccountGetBalance ($acct);
   print "\tAccount: $acctname \tBalance: $baln\n";
}

gnucash::gnc_book_end ($session);
