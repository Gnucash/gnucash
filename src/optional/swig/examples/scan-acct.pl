#!/usr/bin/perl -w

# gnucash perl demo:
#
# This file demonstrates how to open a gnucash file/url and print 
# the names and balances of the top-level accounts in the file.
# Its a pretty basic demo.
#
use lib '/usr/local/lib/gnucash/perl/';
use lib '/usr/lib/gnucash/perl/';
use lib '..';
use gnucash;                                           
package gnucash;

die "Usage: $0 <gnucash filename or url>" if $#ARGV < 0;
print "Will load $ARGV[0]\n";

gnucash::gnc_engine_init(0, $ARGV);
$session = gnucash::gnc_book_new ();

$rc = gnucash::gnc_book_begin ($session, $ARGV[0], 1, 0);
if ($rc != 1) 
{
   $err = gnucash::gnc_book_get_error ($session);
   print "Could not find $ARGV[0], errrocode=$err\n";
}

$rc = gnucash::gnc_book_load ($session);
die "Could not load $ARGV[0]\n" if $rc != 1;

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

gnucash::gnc_book_end ($session);


