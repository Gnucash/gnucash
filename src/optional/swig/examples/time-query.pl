#!/usr/bin/perl -w
#
# This utility times how long it takes to open an acount file/url.
# Handy for performance measurement.
#
# Copyright (c) 2001 Linas Vepstas
# GPL license. see COPYING.
#
use lib '/usr/local/lib/gnucash/perl/';
use lib '/usr/lib/gnucash/perl/';
use lib '..';
use gnucash;                                           
package gnucash;

die "Usage: $0 <filename>" if $#ARGV < 0;
print "Will load $ARGV[0]\n";

gnucash::gnc_engine_init(0, $ARGV);
$session = gnc_session_new();

$rc = gnucash::gnc_session_begin ($session, $ARGV[0], 1, 0);
if ($rc != 1) 
{
   $err = gnucash::gnc_session_get_error ($session);
   print "Could not find $ARGV[0], errrocode=$err\n";
}

# ------------------------------------------------------
$secs = time;

$rc = gnucash::gnc_session_load ($session);
die "Could not load $ARGV[0]\n" if $rc != 1;

($user,$sys,$cuser,$csys) = times;
$elapsed = time() - $secs;
print "delta time to load: user-cpu=$user sys-cpu=$sys elapsed(rounded to sec)=$elapsed\n";
print "\n";

# ------------------------------------------------------
$ssecs = time;
($user,$sys,$cuser,$csys) = times;

$book = gnc_session_get_book ($session);
$grp = gnucash::gnc_book_get_group ($book);

$qu = gnucash::xaccMallocQuery();
gnucash::xaccQuerySetGroup($qu,$grp);

# we know an apriori account name ...
# mutual fund three has about 3K transactions in it.
$acct = gnucash::xaccGetAccountFromName ($grp, "Mutual Fund Three");
gnucash::xaccQueryAddSingleAccountMatch($qu, $acct, $gnucash::QUERY_OR);

gnucash::xaccQueryPrint($qu);

$splits = gnucash::xaccQueryGetSplits($qu);

($fuser,$fsys,$cuser,$csys) = times;
$elapsed = time() - $ssecs;
$fuser -= $user;
$fsys -= $sys;
print "delta time to query: user-cpu=$fuser sys-cpu=$fsys elapsed(rounded to sec)=$elapsed\n";
print "\n";

# ------------------------------------------------------
$ssecs = time;
($user,$sys,$cuser,$csys) = times;

gnucash::gnc_book_destroy ($book);
gnucash::gnc_session_destroy ($session);

($fuser,$fsys,$cuser,$csys) = times;
$fuser -= $user;
$fsys -= $sys;
$elapsed = time() - $ssecs;
print "delta time to finish: user-cpu=$fuser sys-cpu=$fsys elapsed(rounded to sec)=$elapsed\n";
print "\n";

($fuser,$fsys,$cuser,$csys) = times;
$elapsed = time() - $secs;
print "total time to finish: user-cpu=$fuser sys-cpu=$fsys elapsed(rounded to sec)=$elapsed\n";
# ------------------------------------------------------
